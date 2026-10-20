//! Real-time HLS workload for scripts/benchmarks/pipeline_wakeups.py.
#[cfg(unix)]
mod unix {
    use std::{path::PathBuf, time::Instant};

    use anyhow::{Result, ensure};
    use clap::Parser;
    use ff_engine::{ClipResult, LogLevel, OutputConfig, OutputDestination, OutputTarget, Playout};

    #[derive(Parser)]
    struct Args {
        #[arg(long)]
        input: PathBuf,
        #[arg(long)]
        output: PathBuf,
        #[arg(long, default_value_t = 1, value_parser = clap::value_parser!(u32).range(1..))]
        warmup_clips: u32,
        #[arg(long, default_value_t = 2, value_parser = clap::value_parser!(u32).range(1..))]
        measured_clips: u32,
    }

    struct Usage {
        cpu: f64,
        voluntary: libc::c_long,
        involuntary: libc::c_long,
        peak_rss_bytes: u64,
    }

    fn usage() -> Usage {
        let mut value = std::mem::MaybeUninit::<libc::rusage>::uninit();
        // SAFETY: getrusage initializes the valid output pointer on success.
        assert_eq!(
            unsafe { libc::getrusage(libc::RUSAGE_SELF, value.as_mut_ptr()) },
            0
        );
        let value = unsafe { value.assume_init() };

        Usage {
            cpu: (value.ru_utime.tv_sec + value.ru_stime.tv_sec) as f64
                + (value.ru_utime.tv_usec + value.ru_stime.tv_usec) as f64 / 1_000_000.0,
            voluntary: value.ru_nvcsw,
            involuntary: value.ru_nivcsw,
            peak_rss_bytes: value.ru_maxrss as u64
                * if cfg!(target_os = "macos") { 1 } else { 1024 },
        }
    }

    fn play(playout: &mut Playout, input: &str, count: u32) -> Result<()> {
        for _ in 0..count {
            ensure!(
                playout.play(input)? == ClipResult::Played,
                "input was not played normally"
            );
        }

        Ok(())
    }

    pub(super) fn run() -> Result<()> {
        let args = Args::parse();
        std::fs::create_dir_all(&args.output)?;
        let mut config = OutputConfig::new(1280, 720, 25, 48_000);
        config.ffmpeg_log_level = LogLevel::Error;
        for (key, value) in [
            ("preset", "faster"),
            ("quality", "23"),
            ("rate_control", "crf"),
        ] {
            config.video_options.insert(key.into(), value.into());
        }
        config.destinations.push(OutputDestination {
            id: 1,
            config: config.clone(),
            target: OutputTarget::Hls {
                playlist: args
                    .output
                    .join("index.m3u8")
                    .to_string_lossy()
                    .into_owned(),
                variants: Vec::new(),
                subtitle: None,
                segment_seconds: 2,
                list_size: 1000,
            },
        });
        let status = config.output_status.clone();
        let mut playout = Playout::open_parallel(config, 1.0)?;
        let input = args
            .input
            .to_str()
            .ok_or_else(|| anyhow::anyhow!("non-UTF8 input path"))?;
        play(&mut playout, input, args.warmup_clips)?;
        let before = usage();
        let started = Instant::now();
        play(&mut playout, input, args.measured_clips)?;
        let elapsed = started.elapsed().as_secs_f64();
        let after = usage();
        let finishing = Instant::now();
        playout.finish()?;
        let finish_s = finishing.elapsed().as_secs_f64();
        let states = status.snapshot();
        let state = states
            .get(&1)
            .ok_or_else(|| anyhow::anyhow!("missing output state"))?;
        ensure!(
            state.error.is_none() && state.phase == "stopped",
            "HLS output failed: {state:?}"
        );
        let cpu = after.cpu - before.cpu;
        println!(
            "{}",
            serde_json::json!({
                "cpu_s": cpu, "wall_s": elapsed, "cpu_percent": cpu / elapsed * 100.0,
                "voluntary_switches": after.voluntary - before.voluntary,
                "involuntary_switches": after.involuntary - before.involuntary,
                "peak_rss_bytes": after.peak_rss_bytes, "finish_s": finish_s,
                "warmup_clips": args.warmup_clips, "measured_clips": args.measured_clips,
            })
        );

        Ok(())
    }
}

fn main() -> anyhow::Result<()> {
    #[cfg(unix)]
    {
        unix::run()
    }
    #[cfg(not(unix))]
    {
        anyhow::bail!("pipeline wakeup measurements require Linux or macOS")
    }
}
