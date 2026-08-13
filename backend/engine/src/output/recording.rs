use std::{
    ffi::CString,
    fs,
    path::{Path, PathBuf},
    ptr,
    sync::atomic::{AtomicU64, Ordering},
    time::{Duration, Instant, SystemTime, UNIX_EPOCH},
};

use anyhow::{Context, Result, anyhow};
use ffmpeg::{codec, format, util::rational::Rational};
use ffmpeg_next as ffmpeg;
use sysinfo::{Disk, Disks};

use crate::RecordingConfig;

static RECORDING_SEQUENCE: AtomicU64 = AtomicU64::new(0);

pub(super) struct RecordingMuxer {
    octx: format::context::Output,
    video_stream_index: usize,
    audio_stream_index: usize,
    monitor: RecordingMonitor,
}

pub(super) struct RecordingMonitor {
    directory: PathBuf,
    minimum_free_space_gb: u32,
    check_interval: Duration,
    next_check: Instant,
}

impl RecordingMuxer {
    pub(super) fn open(
        config: &RecordingConfig,
        video_encoder: &codec::encoder::video::Encoder,
        audio_encoder: &codec::encoder::audio::Encoder,
    ) -> Result<Self> {
        if config.path.trim().is_empty() {
            return Err(anyhow!("recording path must not be empty"));
        }
        if config.segment_duration == 0 {
            return Err(anyhow!(
                "recording segment duration must be greater than zero"
            ));
        }

        let (pattern, monitor) = prepare_recording(config)?;
        let mut octx = segment_output_context(&pattern)?;

        let mut video_stream = octx.add_stream_with(video_encoder)?;
        video_stream.set_time_base(video_encoder.time_base());
        let video_stream_index = video_stream.index();
        let mut audio_stream = octx.add_stream_with(audio_encoder)?;
        audio_stream.set_time_base(audio_encoder.time_base());
        let audio_stream_index = audio_stream.index();

        let mut options = ffmpeg::Dictionary::new();
        options.set("segment_time", &config.segment_duration.to_string());
        options.set("segment_format", "matroska");
        options.set("reset_timestamps", "1");
        options.set("strftime", "1");
        reject_unused_options(octx.write_header_with(options)?)?;

        Ok(Self {
            octx,
            video_stream_index,
            audio_stream_index,
            monitor,
        })
    }

    pub(super) fn write_video(
        &mut self,
        packet: &ffmpeg::Packet,
        time_base: Rational,
    ) -> Result<()> {
        self.write_packet(packet, self.video_stream_index, time_base)
    }

    pub(super) fn write_audio(
        &mut self,
        packet: &ffmpeg::Packet,
        time_base: Rational,
    ) -> Result<()> {
        self.write_packet(packet, self.audio_stream_index, time_base)
    }

    fn write_packet(
        &mut self,
        packet: &ffmpeg::Packet,
        stream_index: usize,
        encoder_time_base: Rational,
    ) -> Result<()> {
        self.monitor.check()?;
        let stream_time_base = self
            .octx
            .stream(stream_index)
            .context("recording output stream is missing")?
            .time_base();
        let mut packet = packet.clone();
        packet.set_stream(stream_index);
        packet.rescale_ts(encoder_time_base, stream_time_base);
        packet.write_interleaved(&mut self.octx)?;
        Ok(())
    }

    pub(super) fn finish(mut self) -> Result<()> {
        self.octx.write_trailer()?;
        Ok(())
    }
}

impl RecordingMonitor {
    pub(super) fn check(&mut self) -> Result<()> {
        if self.minimum_free_space_gb == 0 || Instant::now() < self.next_check {
            return Ok(());
        }
        self.next_check = Instant::now() + self.check_interval;
        ensure_minimum_free_space(&self.directory, self.minimum_free_space_gb)
    }
}

pub(super) fn prepare_recording(config: &RecordingConfig) -> Result<(PathBuf, RecordingMonitor)> {
    let directory = Path::new(&config.path);
    fs::create_dir_all(directory).with_context(|| {
        format!(
            "failed to create recording directory {}",
            directory.display()
        )
    })?;
    remove_expired_segments(directory, config.retention_days)?;
    ensure_minimum_free_space(directory, config.minimum_free_space_gb)?;
    let channel_id = config.channel_id.unwrap_or_default();
    let sequence = RECORDING_SEQUENCE.fetch_add(1, Ordering::Relaxed);
    let pattern = directory.join(format!(
        "recording-ch{channel_id}-%Y-%m-%d_%H-%M-%S-{}-{sequence}.mkv",
        std::process::id()
    ));
    let check_interval = Duration::from_secs(u64::from(config.segment_duration));
    Ok((
        pattern,
        RecordingMonitor {
            directory: directory.to_path_buf(),
            minimum_free_space_gb: config.minimum_free_space_gb,
            check_interval,
            next_check: Instant::now() + check_interval,
        },
    ))
}

/// The segment muxer owns its child files. `ffmpeg::format::output_as` opens
/// the pattern itself as AVIO and would leave an additional literal `%06d`
/// file behind.
pub(super) fn segment_output_context(path: &Path) -> Result<format::context::Output> {
    let path = CString::new(path.to_string_lossy().as_bytes())
        .context("recording output path contains a null byte")?;
    let muxer = CString::new("segment").expect("static segment muxer name is valid");

    unsafe {
        let mut context = ptr::null_mut();
        let result = ffmpeg::ffi::avformat_alloc_output_context2(
            &mut context,
            ptr::null_mut(),
            muxer.as_ptr(),
            path.as_ptr(),
        );
        if result < 0 {
            if !context.is_null() {
                ffmpeg::ffi::avformat_free_context(context);
            }
            return Err(ffmpeg::Error::from(result).into());
        }
        if context.is_null() {
            return Err(ffmpeg::Error::Unknown.into());
        }
        Ok(format::context::Output::wrap(context))
    }
}

fn ensure_minimum_free_space(directory: &Path, minimum_free_space_gb: u32) -> Result<()> {
    if minimum_free_space_gb == 0 {
        return Ok(());
    }

    let directory = fs::canonicalize(directory).unwrap_or_else(|_| directory.to_path_buf());
    let required = u64::from(minimum_free_space_gb) * 1024 * 1024 * 1024;
    let available = Disks::new_with_refreshed_list()
        .list()
        .iter()
        .filter(|disk| directory.starts_with(disk.mount_point()))
        .max_by_key(|disk| disk.mount_point().as_os_str().len())
        .map(Disk::available_space)
        .context("could not determine free space for recording path")?;

    if available < required {
        return Err(anyhow!(
            "recording path has {:.2} GB free; at least {} GB is required",
            available as f64 / 1_073_741_824.0,
            minimum_free_space_gb
        ));
    }
    Ok(())
}

fn remove_expired_segments(directory: &Path, retention_days: u32) -> Result<()> {
    if retention_days == 0 {
        return Ok(());
    }
    let cutoff = SystemTime::now()
        .checked_sub(Duration::from_secs(u64::from(retention_days) * 86_400))
        .unwrap_or(UNIX_EPOCH);
    for entry in fs::read_dir(directory)? {
        let entry = entry?;
        let path = entry.path();
        if is_managed_segment(&path)
            && entry
                .metadata()?
                .modified()
                .is_ok_and(|modified| modified < cutoff)
        {
            fs::remove_file(&path).with_context(|| {
                format!("failed to remove expired recording {}", path.display())
            })?;
        }
    }
    Ok(())
}

fn is_managed_segment(path: &Path) -> bool {
    let Some(name) = path.file_name().and_then(|name| name.to_str()) else {
        return false;
    };
    let Some(stem) = name
        .strip_prefix("recording-ch")
        .and_then(|name| name.strip_suffix(".mkv"))
    else {
        return false;
    };
    is_current_managed_segment(stem) || is_legacy_managed_segment(stem)
}

fn is_current_managed_segment(stem: &str) -> bool {
    let mut suffixes = stem.rsplitn(3, '-');
    let Some(sequence) = suffixes.next() else {
        return false;
    };
    let Some(process_id) = suffixes.next() else {
        return false;
    };
    let Some(prefix) = suffixes.next() else {
        return false;
    };
    let Some((channel, started_at)) = prefix.split_once('-') else {
        return false;
    };

    channel.parse::<i32>().is_ok()
        && valid_recording_timestamp(started_at)
        && process_id.chars().all(|ch| ch.is_ascii_digit())
        && !process_id.is_empty()
        && sequence.chars().all(|ch| ch.is_ascii_digit())
        && !sequence.is_empty()
}

fn valid_recording_timestamp(value: &str) -> bool {
    let Some((date, time)) = value.split_once('_') else {
        return false;
    };
    let date = date
        .split('-')
        .map(str::parse::<u32>)
        .collect::<Result<Vec<_>, _>>();
    let time = time
        .split('-')
        .map(str::parse::<u32>)
        .collect::<Result<Vec<_>, _>>();
    matches!(date.as_deref(), Ok([year, month, day]) if *year >= 1970 && (1..=12).contains(month) && (1..=31).contains(day))
        && matches!(time.as_deref(), Ok([hour, minute, second]) if *hour < 24 && *minute < 60 && *second < 60)
}

fn is_legacy_managed_segment(stem: &str) -> bool {
    let mut parts = stem.split('-');
    let values = [parts.next(), parts.next(), parts.next(), parts.next()];
    parts.next().is_none()
        && values.iter().all(|value| {
            value.is_some_and(|value| {
                !value.is_empty() && value.chars().all(|ch| ch.is_ascii_digit())
            })
        })
        && values[0].is_some_and(|channel| channel.parse::<i32>().is_ok())
        && values[3].is_some_and(|segment| segment.len() == 6)
}

fn reject_unused_options(options: ffmpeg::Dictionary<'_>) -> Result<()> {
    let unused = options
        .iter()
        .map(|(key, value)| format!("{key}={value}"))
        .collect::<Vec<_>>();
    if unused.is_empty() {
        Ok(())
    } else {
        Err(anyhow!(
            "unused FFmpeg recording option(s): {}",
            unused.join(", ")
        ))
    }
}

#[cfg(test)]
mod tests {
    use std::{fs, time::SystemTime};

    use crate::RecordingConfig;

    use super::{
        ensure_minimum_free_space, is_managed_segment, prepare_recording, remove_expired_segments,
    };

    #[test]
    fn accepts_disabled_minimum_free_space_limit() {
        assert!(ensure_minimum_free_space(std::path::Path::new("."), 0).is_ok());
    }

    #[test]
    fn rejects_an_unattainable_minimum_free_space_limit() {
        assert!(ensure_minimum_free_space(std::path::Path::new("."), u32::MAX).is_err());
    }

    #[test]
    fn recognizes_only_managed_recording_segments() {
        assert!(is_managed_segment(std::path::Path::new(
            "recording-ch2-2026-08-13_15-42-09-42-0.mkv"
        )));
        assert!(is_managed_segment(std::path::Path::new(
            "recording-ch2-1786617288287263738-42-000001.mkv"
        )));
        assert!(!is_managed_segment(std::path::Path::new("archive.mkv")));
        assert!(!is_managed_segment(std::path::Path::new(
            "recording-ch2-2026-08-13_15-42-09-42-x.mkv"
        )));
    }

    #[test]
    fn retention_does_not_delete_unmanaged_mkv_files() {
        let directory = std::env::temp_dir().join(format!(
            "ffplayout-recording-retention-{}-{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        fs::create_dir_all(&directory).unwrap();
        let unmanaged = directory.join("archive.mkv");
        fs::write(&unmanaged, []).unwrap();

        remove_expired_segments(&directory, u32::MAX).unwrap();

        assert!(unmanaged.exists());
        fs::remove_dir_all(directory).unwrap();
    }

    #[test]
    fn recording_patterns_are_unique_and_include_the_channel() {
        let directory = std::env::temp_dir().join(format!(
            "ffplayout-recording-pattern-{}",
            std::process::id()
        ));
        let config =
            RecordingConfig::new(directory.to_string_lossy(), 300).with_channel_id(Some(7));

        let (first, _) = prepare_recording(&config).unwrap();
        let (second, _) = prepare_recording(&config).unwrap();

        assert_ne!(first, second);
        assert!(
            first
                .file_name()
                .unwrap()
                .to_string_lossy()
                .starts_with("recording-ch7-")
        );
        fs::remove_dir_all(directory).unwrap();
    }
}
