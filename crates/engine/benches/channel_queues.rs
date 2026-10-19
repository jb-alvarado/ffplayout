//! One isolated benchmark case. Orchestrated by scripts/benchmarks/channel_queues.py.
//! Include the production queue unchanged without exposing benchmark APIs in ff-engine.
// Cargo's harness=false bench still enables cfg(test), but does not collect the
// included module's unit tests; their imports and unused queue APIs are expected.
#[allow(dead_code, unused_imports)]
#[path = "../src/utils/pipeline_queue.rs"]
mod pipeline_queue;

use std::{
    hint::black_box,
    sync::{Arc, Barrier, OnceLock, mpsc},
    thread::{self, Thread},
    time::{Duration, Instant},
};

use clap::{Parser, ValueEnum};
use pipeline_queue::{PipelineControl, PipelineQueue, QueueLimits};
use ringbuf::traits::{Consumer, Producer, Split};

#[derive(Clone, Copy, Debug, ValueEnum)]
enum Backend {
    Pipeline,
    Std,
    Crossbeam,
    Kanal,
    Flume,
    Rtrb,
    Ringbuf,
}

#[derive(Clone, Copy, Debug, ValueEnum)]
enum Scenario {
    Throughput,
    Mpsc,
    Realtime,
    Burst,
    Slow,
    Idle,
}

#[derive(Parser, Debug)]
struct Options {
    #[arg(long, value_enum)]
    backend: Backend,
    #[arg(long, value_enum)]
    scenario: Scenario,
    #[arg(long, default_value_t = 24, value_parser = clap::value_parser!(u32).range(1..=4096))]
    capacity: u32,
    #[arg(long, default_value_t = 200_000, value_parser = clap::value_parser!(u32).range(1..))]
    messages: u32,
    #[arg(long, default_value_t = 5.0, value_parser = positive_seconds)]
    seconds: f64,
}

fn positive_seconds(value: &str) -> Result<f64, String> {
    let seconds = value.parse::<f64>().map_err(|error| error.to_string())?;
    if !seconds.is_finite() || !(0.1..=3600.0).contains(&seconds) {
        return Err("seconds must be finite and between 0.1 and 3600".into());
    }
    Ok(seconds)
}

struct Message {
    producer: usize,
    sequence: usize,
    sent: Instant,
    payload: Arc<[u8]>,
}

struct SenderStats {
    send_ns: Vec<u64>,
    late_ns: Vec<u64>,
}

const WARMUP_MESSAGES: usize = 2048;

// Models FFmpeg buffer references: allocate payloads once, transfer an owned
// reference per message. The benchmark deliberately does not copy pixels.
fn payloads() -> [Arc<[u8]>; 3] {
    [1_382_400, 8192, 4096].map(|bytes| Arc::from(vec![42_u8; bytes]))
}

fn main() {
    let options = Options::parse();
    if matches!(options.scenario, Scenario::Mpsc)
        && matches!(options.backend, Backend::Rtrb | Backend::Ringbuf)
    {
        eprintln!("SPSC ring buffers do not support the MPSC scenario");
        std::process::exit(2);
    }
    let capacity = options.capacity as usize;
    let producers = if matches!(options.scenario, Scenario::Mpsc) {
        2
    } else {
        1
    };
    match options.backend {
        Backend::Pipeline => {
            let control = Arc::new(PipelineControl::default());
            let queue = PipelineQueue::new(
                QueueLimits {
                    items: capacity,
                    // Compare count-bounded transports. Real byte/time admission
                    // policies need separate validation before adopting a crate.
                    bytes: usize::MAX,
                    duration_us: i64::MAX,
                },
                control,
            );
            let senders = (0..producers)
                .map(|_| {
                    let queue = queue.clone();
                    move |message: Message| {
                        let bytes = message.payload.len();
                        let timestamp = (message.sequence as i64).saturating_mul(1000);
                        queue.send(message, bytes, Some(timestamp)).unwrap();
                    }
                })
                .collect();
            measure(&options, senders, move || queue.recv().unwrap().unwrap());
        }
        Backend::Std => {
            let (tx, rx) = mpsc::sync_channel(capacity);
            let senders = (0..producers)
                .map(|_| {
                    let tx = tx.clone();
                    move |message| tx.send(message).unwrap()
                })
                .collect();
            measure(&options, senders, move || rx.recv().unwrap());
        }
        Backend::Crossbeam => {
            let (tx, rx) = crossbeam_channel::bounded(capacity);
            let senders = (0..producers)
                .map(|_| {
                    let tx = tx.clone();
                    move |message| tx.send(message).unwrap()
                })
                .collect();
            measure(&options, senders, move || rx.recv().unwrap());
        }
        Backend::Kanal => {
            let (tx, rx) = kanal::bounded(capacity);
            let senders = (0..producers)
                .map(|_| {
                    let tx = tx.clone();
                    move |message| tx.send(message).unwrap()
                })
                .collect();
            measure(&options, senders, move || rx.recv().unwrap());
        }
        Backend::Flume => {
            let (tx, rx) = flume::bounded(capacity);
            let senders = (0..producers)
                .map(|_| {
                    let tx = tx.clone();
                    move |message| tx.send(message).unwrap()
                })
                .collect();
            measure(&options, senders, move || rx.recv().unwrap());
        }
        Backend::Rtrb => {
            let (mut tx, mut rx) = rtrb::RingBuffer::new(capacity);
            let wake = Arc::new(RingWake::new());
            let producer_wake = wake.clone();
            let send = move |mut message| {
                producer_wake.register();
                loop {
                    match tx.push(message) {
                        Ok(()) => break,
                        Err(rtrb::PushError::Full(value)) => {
                            message = value;
                            thread::park();
                        }
                    }
                }
                producer_wake.consumer.unpark();
            };
            let receive = move || loop {
                if let Ok(message) = rx.pop() {
                    wake.wake_producer();
                    break message;
                }
                thread::park();
            };
            measure(&options, vec![send], receive);
        }
        Backend::Ringbuf => {
            let (mut tx, mut rx) = ringbuf::HeapRb::new(capacity).split();
            let wake = Arc::new(RingWake::new());
            let producer_wake = wake.clone();
            let send = move |mut message| {
                producer_wake.register();
                loop {
                    match tx.try_push(message) {
                        Ok(()) => break,
                        Err(value) => {
                            message = value;
                            thread::park();
                        }
                    }
                }
                producer_wake.consumer.unpark();
            };
            let receive = move || loop {
                if let Some(message) = rx.try_pop() {
                    wake.wake_producer();
                    break message;
                }
                thread::park();
            };
            measure(&options, vec![send], receive);
        }
    }
}

// park/unpark retains a wake token across the empty/full check. No spin loop,
// periodic polling, auxiliary worker or Mutex around ring-buffer storage.
struct RingWake {
    producer: OnceLock<Thread>,
    consumer: Thread,
}

impl RingWake {
    fn new() -> Self {
        Self {
            producer: OnceLock::new(),
            consumer: thread::current(),
        }
    }

    fn register(&self) {
        self.producer.get_or_init(thread::current);
    }

    fn wake_producer(&self) {
        if let Some(producer) = self.producer.get() {
            producer.unpark();
        }
    }
}

fn schedule(options: &Options) -> Vec<Duration> {
    match options.scenario {
        Scenario::Throughput | Scenario::Mpsc | Scenario::Slow => {
            vec![Duration::ZERO; options.messages as usize]
        }
        Scenario::Realtime => {
            let mut times = Vec::new();
            for (rate, samples) in [(25_u64, 1_u64), (48_000, 1024)] {
                let mut index = 0;
                loop {
                    let offset = Duration::from_nanos(index * samples * 1_000_000_000 / rate);
                    if offset.as_secs_f64() >= options.seconds {
                        break;
                    }
                    times.push(offset);
                    index += 1;
                }
            }
            times.sort_unstable();
            times
        }
        Scenario::Burst => {
            let batches = (options.seconds / 0.2).ceil() as u64;
            (0..batches)
                .flat_map(|index| std::iter::repeat_n(Duration::from_millis(index * 200), 32))
                .collect()
        }
        Scenario::Idle => (0..(options.seconds * 100.0).ceil() as u64)
            .map(|index| Duration::from_millis(index * 10))
            .collect(),
    }
}

fn measure<S, R>(options: &Options, senders: Vec<S>, mut receive: R)
where
    S: FnMut(Message) + Send + 'static,
    R: FnMut() -> Message,
{
    let schedule = Arc::new(schedule(options));
    let per_producer = schedule.len();
    let count = per_producer * senders.len();
    let payloads = payloads();
    let mut latency = Vec::with_capacity(count);
    let mut last = vec![0; senders.len()];
    let barrier = Arc::new(Barrier::new(senders.len() + 1));
    let epoch = Arc::new(OnceLock::new());
    let mut threads = Vec::new();
    let paced = matches!(
        options.scenario,
        Scenario::Realtime | Scenario::Burst | Scenario::Idle
    );
    for (producer, mut send) in senders.into_iter().enumerate() {
        let barrier = barrier.clone();
        let epoch = epoch.clone();
        let schedule = schedule.clone();
        let payloads = payloads.clone();
        threads.push(thread::spawn(move || {
            // Reserve measurement storage before the timed section.
            let mut stats = SenderStats {
                send_ns: Vec::with_capacity(per_producer),
                late_ns: Vec::with_capacity(per_producer),
            };
            for sequence in 0..WARMUP_MESSAGES {
                send(Message {
                    producer,
                    sequence,
                    sent: Instant::now(),
                    payload: payloads[sequence % payloads.len()].clone(),
                });
            }
            barrier.wait();
            let start: Instant = *epoch.get().unwrap();
            for (sequence, offset) in schedule.iter().enumerate() {
                let due = start + *offset;
                if !offset.is_zero() {
                    thread::sleep(due.saturating_duration_since(Instant::now()));
                }
                let payload = payloads[sequence % payloads.len()].clone();
                let sent = Instant::now();
                if paced {
                    stats
                        .late_ns
                        .push(nanos(sent.saturating_duration_since(due)));
                }
                send(Message {
                    producer,
                    sequence,
                    sent,
                    payload,
                });
                stats.send_ns.push(nanos(sent.elapsed()));
            }
            stats
        }));
    }
    for _ in 0..WARMUP_MESSAGES * last.len() {
        black_box(receive());
    }
    let before = usage();
    let start = Instant::now();
    epoch.set(start).unwrap();
    barrier.wait();
    for index in 0..count {
        let message = receive();
        latency.push(nanos(message.sent.elapsed()));
        assert_eq!(message.sequence, last[message.producer], "FIFO/data loss");
        last[message.producer] += 1;
        black_box(message.payload[message.sequence % message.payload.len()]);
        drop(message);
        if matches!(options.scenario, Scenario::Slow) && (index + 1) % 128 == 0 {
            thread::sleep(Duration::from_millis(2));
        }
    }
    let statistics: Vec<_> = threads
        .into_iter()
        .map(|handle| handle.join().unwrap())
        .collect();
    let elapsed = start.elapsed().as_secs_f64();
    let after = usage();
    assert!(last.iter().all(|value| *value == per_producer));
    let cpu = after.cpu_s - before.cpu_s;
    // Sorting and JSON serialization are deliberately outside CPU/wall measurements.
    let mut sends = Vec::with_capacity(count);
    let mut late = Vec::with_capacity(count);
    for stats in statistics {
        sends.extend(stats.send_ns);
        late.extend(stats.late_ns);
    }
    latency.sort_unstable();
    sends.sort_unstable();
    late.sort_unstable();
    println!(
        "{}",
        serde_json::json!({
            "backend": format!("{:?}", options.backend).to_lowercase(),
            "scenario": format!("{:?}", options.scenario).to_lowercase(),
            "capacity": options.capacity, "messages": count,
            "wall_s": elapsed, "messages_s": count as f64 / elapsed,
            "cpu_s": cpu, "cpu_percent": cpu / elapsed * 100.0,
            "handoff_p50_us": percentile(&latency, 50) as f64 / 1000.0,
            "handoff_p99_us": percentile(&latency, 99) as f64 / 1000.0,
            "handoff_max_us": latency.last().copied().unwrap_or(0) as f64 / 1000.0,
            "send_p99_us": percentile(&sends, 99) as f64 / 1000.0,
            "schedule_late_p99_us": percentile(&late, 99) as f64 / 1000.0,
            "peak_rss_bytes": after.peak_rss_bytes,
            "voluntary_switches": after.voluntary - before.voluntary,
            "involuntary_switches": after.involuntary - before.involuntary,
        })
    );
}

fn nanos(duration: Duration) -> u64 {
    duration.as_nanos().min(u128::from(u64::MAX)) as u64
}

fn percentile(sorted: &[u64], percentile: usize) -> u64 {
    if sorted.is_empty() {
        return 0;
    }
    sorted[(sorted.len() * percentile)
        .div_ceil(100)
        .saturating_sub(1)
        .min(sorted.len() - 1)]
}

struct Usage {
    cpu_s: f64,
    peak_rss_bytes: u64,
    voluntary: libc::c_long,
    involuntary: libc::c_long,
}

#[cfg(unix)]
fn usage() -> Usage {
    let mut usage = std::mem::MaybeUninit::<libc::rusage>::uninit();
    // SAFETY: getrusage initializes this valid output pointer on success.
    assert_eq!(
        unsafe { libc::getrusage(libc::RUSAGE_SELF, usage.as_mut_ptr()) },
        0
    );
    let usage = unsafe { usage.assume_init() };
    let cpu_s = (usage.ru_utime.tv_sec + usage.ru_stime.tv_sec) as f64
        + (usage.ru_utime.tv_usec + usage.ru_stime.tv_usec) as f64 / 1_000_000.0;
    Usage {
        cpu_s,
        peak_rss_bytes: usage.ru_maxrss as u64 * if cfg!(target_os = "macos") { 1 } else { 1024 },
        voluntary: usage.ru_nvcsw,
        involuntary: usage.ru_nivcsw,
    }
}

#[cfg(not(unix))]
fn usage() -> Usage {
    panic!("CPU/RSS measurements currently require Linux or macOS");
}
