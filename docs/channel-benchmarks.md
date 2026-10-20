# Channel and queue benchmarks

Compare channel implementations without starting ffplayout, accessing a database,
connecting destinations or changing the production pipeline. Requires Python 3.9+
and the normal Rust/FFmpeg development environment. CPU, context-switch and RSS
measurements currently target Linux and macOS.

```sh
# Build the optimized benchmark and smoke-test every supported combination.
python3 scripts/benchmarks/channel_queues.py --quick

# Default: 3 fresh processes per combination, capacity 24, 5-second paced cases,
# 100,000 messages per producer for throughput/slow cases.
python3 scripts/benchmarks/channel_queues.py

# Compare the small raw-frame queues and larger compressed-packet queues.
python3 scripts/benchmarks/channel_queues.py --capacities 4,24,256 \
    --seconds 10 --repetitions 5

# A focused comparison. --binary can reuse the executable reported by Cargo.
python3 scripts/benchmarks/channel_queues.py \
    --backends pipeline,crossbeam,kanal,flume --scenarios realtime,burst,slow
```

The runner uses `cargo bench -p ff-engine --no-default-features --features tokio
--bench channel_queues --no-run`. New channel crates are **dev-dependencies only**.
No replacement is enabled in production. Individual cases can also be invoked
through Cargo, for example:

```sh
cargo bench -p ff-engine --no-default-features --features tokio \
    --bench channel_queues -- --backend kanal --scenario realtime \
    --capacity 24 --seconds 5
```

## Implementations

| Name | Implementation |
| --- | --- |
| `pipeline` | The actual `PipelineQueue` source, included unchanged in the benchmark |
| `std` | `std::sync::mpsc::sync_channel` |
| `crossbeam` | `crossbeam-channel::bounded` |
| `kanal` | `kanal::bounded`, default locking implementation |
| `flume` | `flume::bounded`, default features disabled; no optional spin feature |
| `rtrb` | `rtrb::RingBuffer` with a blocking `park`/`unpark` adapter |
| `ringbuf` | `ringbuf::HeapRb` with the same blocking adapter |

Ring buffers support one sender and one receiver, so they are excluded from the
two-sender case. Empty/full rings park the waiting thread; they do not busy-spin
or start a helper thread. Wake tokens avoid losing notifications between testing
the queue and parking. The adapter's costs are part of the result: this is a
comparison of usable blocking transports, not raw ring operations or an audio
callback benchmark. Native channels retain their own internal waiting strategy.

## Workloads

| Scenario | Traffic | Purpose |
| --- | --- | --- |
| `throughput` | One producer sends without pacing | Instrumented throughput ceiling and contention |
| `mpsc` | Two producers send concurrently | The two-encoder-to-packet-queue topology |
| `realtime` | Merged 25 fps and 48 kHz / 1024-sample deadlines | Frequent sleeping and waking at program rates |
| `burst` | 32 messages every 200 ms | Short encoder/demux bursts and temporary queue filling |
| `slow` | Unpaced producer; receiver pauses 2 ms every 128 messages | Bounded backpressure and recovery after write stalls |
| `idle` | One message every 10 ms | Wakeup latency with an otherwise empty queue |

Queues have the same **message count** capacity. The production queue's byte and
time-span bounds are configured not to constrain this test; its existing
timestamp scans still run. This isolates transport costs. It does **not** establish
that another crate can replace our byte/time admission policy, selective packet
discard, cross-queue scheduling, cancellation or reconnect control semantics.
Those need a separate integration comparison before adopting any replacement.

Messages contain producer/sequence identifiers, a monotonic send timestamp and
an owned `Arc<[u8]>` reference. Three payloads are allocated before measurement:
1,382,400 bytes (720p YUV420), 8,192 bytes (1024 stereo f32 samples), and 4,096
bytes (a compressed packet). Payload types cycle independently of deadlines.
The receiver reads one byte and drops the reference. Pixel/sample copying and
codec work are deliberately absent, approximating FFmpeg reference handoffs.
The fixed shared payload pool also adds reference-count contention; this is
identical for all candidates. It cannot model frame allocation churn or a
long-running memory leak.

Every process warms up with 2,048 messages per producer, drains them, then starts
the measured section behind a barrier. Per-thread measurement vectors are
reserved in advance. All measured messages must arrive in order within their
producer, with no omissions or duplication; any violation fails the case.
Sender/receiver closures are statically dispatched, not boxed trait objects.

## Measurements and interpretation

- **CPU seconds / CPU percent:** process user + system CPU from `getrusage`,
  measured after warmup until all messages are consumed and sender threads join.
  100% means one CPU core. Includes instrumentation, backpressure and native
  channel spinning; excludes building, warmup, percentile sorting and reporting.
  The summary also reports CPU nanoseconds per delivered message, so different
  throughput rates are not mistaken for higher/lower efficiency from CPU percent
  alone.
- **Handoff p50/p99/max:** from immediately before calling send until receipt.
  Includes time blocked on a full queue, and therefore represents end-to-end
  handoff, not pure residence time inside the channel.
- **Send p99:** duration of the send operation, including backpressure. Producer
  instrumentation adds a small common overhead to this number.
- **Schedule lateness p99:** delay before starting a scheduled send, relative to
  its absolute deadline. Unpaced scenarios report zero. OS timer accuracy is
  included; all channels process the same schedule without dropping late items.
- **Context switches:** process voluntary/involuntary switch deltas where the OS
  reports them. Compare within one OS, not between Linux and macOS.
- **Peak RSS:** process high-water mark, in bytes. Every case runs in a fresh
  process. Includes startup, warmup, payloads, channel storage and timing vectors;
  it is not the channel's isolated allocation size. Recording vectors can
  dominate throughput-case RSS. Sorting/reporting allocations happen after the
  RSS sample. Expect allocator/page-size noise in small differences.

The runner randomizes candidate order reproducibly (`--seed`) for each repetition.
It writes a unique directory under `target/channel-benchmarks/` containing:

- `runs.jsonl` / `runs.csv`: individual measurements, retained incrementally as
  JSONL even if a later test is interrupted.
- `summary.csv`: medians across repetitions, worst observed handoff and peak RSS.
  A median of run p99 values is explicitly named as such; it is not a pooled p99.
- `metadata.json`: revision, dirty state, platform, CPU count, Rust version,
  CPU model/affinity where available, build overrides, arguments and benchmark
  binary SHA-256.
- `Cargo.lock`: exact dependency versions; `failures.json`: crashes/timeouts.
- Copies of the benchmark, production queue and runner sources, including
  uncommitted changes at recording time.

Run on an idle machine without other compiles or profiling tools. Keep power
mode and workload identical and repeat on the actual Linux test server. Prioritize
CPU and tail latency in paced/burst/slow workloads over maximum throughput. These
benchmarks do not predict total playout CPU savings or prove 24/7 stability.

## Full pipeline: polling versus activity notifications

```sh
python3 scripts/benchmarks/pipeline_wakeups.py
```

This Linux/macOS comparison uses the actual parallel output pipeline, not a
queue adapter. It builds two isolated source snapshots before timing starts.
Both keep the current composition, encoder settings, queue limits and early
capacity check. The polling snapshot restores the scheduler, demux, encoder
worker and delivery paths from `0452dbcb` (including the scheduler's original
queue inspections); it preserves the current
shared-logo interface. The current activity API remains in that snapshot but
has no production subscribers. `recv_timeout` is enabled for its original
delivery loop. The activity snapshot uses the current checkout unchanged.
Source hashes and complete snapshots are retained so this comparison can be
inspected independently of the branch's commit history.

The default workload repeats `filler_0.mp4` into one 720p25 HLS output, x264
CRF 23 / preset faster, two-second segments, no logo or MoQ. Each fresh process
plays one clip for warmup and measures two further clips (about 30 seconds).
Three pairs run in A/B, B/A, A/B order. Startup and finalization are outside the
CPU/time measurement; peak RSS includes startup/warmup. CPU percentages count
100% per core. HLS duration, resolution and video packet count are checked after
each process. No compilation or probing runs concurrently with the measured
workload. Run on an otherwise idle machine with the same power settings.

Reports under `tmp/bench/pipeline-wakeups/` retain `runs.jsonl`, `runs.csv`,
`summary.json`, build/run logs, executable hashes and source snapshots. The
script uses the existing Cargo target cache and does not access the application
database or configured outputs. For a longer comparison:

```sh
python3 scripts/benchmarks/pipeline_wakeups.py \
    --warmup-clips 2 --measured-clips 8 --repetitions 5
```

These results apply to this HLS workload and machine. They do not measure MoQ,
RTMP or live ingest, prove long-term memory stability, or establish the effect
of logo sharing. Pairwise CPU differences should agree across repetitions
before attributing small differences to the waiting strategy.
