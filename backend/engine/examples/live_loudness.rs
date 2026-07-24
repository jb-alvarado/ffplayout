//! Offline reference runner for the engine's live loudness processor.
//!
//! ```text
//! cargo run -p ff-engine --example live_loudness -- INPUT_FILE --target-lufs -17 --max-gain-db 16
//! ```
//!
//! When present, the video stream is copied into an MP4. The first audio stream
//! is decoded, normalized by [`LiveLoudnessProcessor`], and encoded as 128
//! kbit/s Opus. Audio-only inputs produce an `.opus` file. No external `ffmpeg`
//! executable is used.

use std::{
    collections::VecDeque,
    io::{self, Write},
    path::{Path, PathBuf},
};

use anyhow::{Context, Result, bail};
use clap::{Parser, ValueEnum};
use ff_engine::{
    LiveLoudnessConfig, LiveLoudnessMeasurement, LiveLoudnessMetrics, LiveLoudnessProcessor,
};
use ffmpeg::Rescale;
use ffmpeg::{
    Stream, codec, encoder, format, frame, media,
    rescale::TIME_BASE,
    software::resampling,
    util::{
        channel_layout::ChannelLayout,
        format::sample::{Sample, Type},
    },
};
use ffmpeg_next as ffmpeg;

const SAMPLE_RATE: u32 = 48_000;
const CHANNELS: usize = 2;

#[derive(Clone, Copy, Debug, Default, ValueEnum)]
enum AnalysisMode {
    /// Analyze and correct each frame immediately; no added delay.
    #[default]
    ImmediateShortTerm,
    /// Buffer 500 ms and drive the rider from EBU R128 momentary loudness.
    Momentary500ms,
    /// Buffer 3 seconds and drive the rider from EBU R128 short-term loudness.
    ShortTerm3s,
}

impl AnalysisMode {
    fn file_suffix(self) -> &'static str {
        match self {
            Self::ImmediateShortTerm => "immediate-short-term",
            Self::Momentary500ms => "momentary-500ms",
            Self::ShortTerm3s => "short-term-3s",
        }
    }
}

#[derive(Debug, Parser)]
#[command(
    about = "Normalize an audio stream with the engine's live loudness processor",
    after_help = "The immediate mode follows short-term loudness rather than forcing programme-integrated loudness. The lookahead modes use either 400 ms momentary or 3-second short-term loudness.\n\nFor unusually quiet source material, a target of -17 LUFS may require more than the conservative default of --max-gain-db 8; for example use --max-gain-db 16."
)]
struct Arguments {
    /// Input audio or video file. Video inputs retain their original video stream.
    input: PathBuf,

    /// Overwrite an existing output file.
    #[arg(short = 'y', long)]
    overwrite: bool,

    /// Loudness analysis/correction mode. Lookahead modes delay audio by the selected window. Options are: immediate-short-term, momentary500ms, short-term3s.
    #[arg(long, value_enum, default_value_t = AnalysisMode::ImmediateShortTerm)]
    analysis_mode: AnalysisMode,

    /// Target loudness in LUFS. Higher values are louder; e.g. -17 is louder than -23.
    #[arg(short = 't', long, default_value_t = -23.0, allow_hyphen_values = true)]
    target_lufs: f64,

    /// No gain change inside this distance from the target, in LU. A wider band reduces gain movement.
    #[arg(long, default_value_t = 1.0)]
    dead_band_lu: f64,

    /// Largest upward gain the live rider may apply, in dB. Increase for very quiet sources; higher values also raise background noise.
    #[arg(long, default_value_t = 8.0)]
    max_gain_db: f64,

    /// Largest attenuation the live rider may apply, in dB. This must be zero or negative; e.g. -12 allows reducing loud input by up to 12 dB.
    #[arg(long, default_value_t = -12.0, allow_hyphen_values = true)]
    max_attenuation_db: f64,

    /// Maximum upward gain change per second, in dB/s. Smaller values avoid pumping but take longer to lift quiet material.
    #[arg(long, default_value_t = 0.5)]
    gain_up_db_per_second: f64,

    /// Maximum downward gain change per second, in dB/s. A higher value reacts more quickly to unexpectedly loud input.
    #[arg(long, default_value_t = 2.0)]
    gain_down_db_per_second: f64,

    /// Signals below the selected measurement's loudness are not amplified, in LUFS. This keeps silence and low ambient noise from being raised.
    #[arg(long, default_value_t = -60.0, allow_hyphen_values = true)]
    silence_gate_lufs: f64,

    /// Final sample ceiling in dBTP. This is a safety ceiling after the gain rider; it does not replace a look-ahead limiter.
    #[arg(long, default_value_t = -1.0, allow_hyphen_values = true)]
    true_peak_ceiling_dbtp: f64,
}

impl Arguments {
    fn loudness_config(&self) -> Result<LiveLoudnessConfig> {
        let values = [
            self.target_lufs,
            self.dead_band_lu,
            self.max_gain_db,
            self.max_attenuation_db,
            self.gain_up_db_per_second,
            self.gain_down_db_per_second,
            self.silence_gate_lufs,
            self.true_peak_ceiling_dbtp,
        ];
        if values.iter().any(|value| !value.is_finite()) {
            bail!("all loudness parameters must be finite numbers");
        }
        if self.dead_band_lu < 0.0
            || self.max_gain_db < 0.0
            || self.max_attenuation_db > 0.0
            || self.gain_up_db_per_second < 0.0
            || self.gain_down_db_per_second < 0.0
        {
            bail!(
                "gain limits and rates must be positive; max attenuation must be zero or negative"
            );
        }
        Ok(LiveLoudnessConfig {
            target_lufs: self.target_lufs,
            dead_band_lu: self.dead_band_lu,
            max_gain_db: self.max_gain_db,
            max_attenuation_db: self.max_attenuation_db,
            gain_up_db_per_second: self.gain_up_db_per_second,
            gain_down_db_per_second: self.gain_down_db_per_second,
            silence_gate_lufs: self.silence_gate_lufs,
            true_peak_ceiling_dbtp: self.true_peak_ceiling_dbtp,
        })
    }
}

fn main() -> Result<()> {
    let arguments = Arguments::parse();
    let input = arguments.input.clone();
    let loudness_config = arguments.loudness_config()?;
    ffmpeg::init().context("initializing FFmpeg libraries")?;
    let mut input_context = format::input(&input).context("opening input")?;
    let video_input = input_context.streams().best(media::Type::Video);
    let audio_input = input_context
        .streams()
        .best(media::Type::Audio)
        .context("input has no audio stream")?;
    let video_index = video_input.as_ref().map(Stream::index);
    let audio_index = audio_input.index();
    let audio_time_base = audio_input.time_base();
    let output = output_path(&input, video_input.is_some(), arguments.analysis_mode)?;
    if output.exists() && !arguments.overwrite {
        bail!("output file already exists: {}", output.display());
    }
    let mut progress = Progress::new(input_context.duration());

    let audio_decoder_context = codec::context::Context::from_parameters(audio_input.parameters())?;
    let mut audio_decoder = audio_decoder_context.decoder().audio()?;
    let input_layout = channel_layout(&audio_decoder);
    let mut decode_resampler = resampling::Context::get(
        audio_decoder.format(),
        input_layout,
        audio_decoder.rate(),
        Sample::F32(Type::Planar),
        ChannelLayout::STEREO,
        SAMPLE_RATE,
    )?;

    let mut output_context = format::output(&output).context("creating output")?;
    let video_output_index = video_input
        .map(|video_input| {
            let mut stream = output_context.add_stream(encoder::find(codec::Id::None))?;
            stream.set_parameters(video_input.parameters());
            // The input tag can be invalid in an MP4 stream-copy output.
            unsafe {
                (*stream.parameters().as_mut_ptr()).codec_tag = 0;
            }
            stream.set_time_base(video_input.time_base());
            Ok::<_, anyhow::Error>(stream.index())
        })
        .transpose()?;

    let opus = codec::encoder::find_by_name("libopus").context("libopus encoder is unavailable")?;
    let global_header = output_context
        .format()
        .flags()
        .contains(format::flag::Flags::GLOBAL_HEADER);
    let encoder_format = preferred_audio_format(opus)?;
    let (audio_output_index, mut audio_encoder) = {
        let mut stream = output_context.add_stream(opus)?;
        let mut context = codec::context::Context::new_with_codec(opus)
            .encoder()
            .audio()?;
        context.set_rate(SAMPLE_RATE as i32);
        context.set_channel_layout(ChannelLayout::STEREO);
        context.set_format(encoder_format);
        context.set_time_base((1, SAMPLE_RATE as i32));
        context.set_bit_rate(128_000);
        if global_header {
            context.set_flags(codec::flag::Flags::GLOBAL_HEADER);
        }
        let encoder = context.open_as(opus)?;
        stream.set_parameters(&encoder);
        stream.set_time_base((1, SAMPLE_RATE as i32));
        (stream.index(), encoder)
    };
    let mut encode_resampler = (encoder_format != Sample::F32(Type::Planar))
        .then(|| {
            resampling::Context::get(
                Sample::F32(Type::Planar),
                ChannelLayout::STEREO,
                SAMPLE_RATE,
                encoder_format,
                ChannelLayout::STEREO,
                SAMPLE_RATE,
            )
        })
        .transpose()?;

    output_context.set_metadata(input_context.metadata().to_owned());
    output_context.write_header()?;

    let mut loudness = LoudnessPipeline::new(loudness_config, arguments.analysis_mode)?;
    let frame_size = audio_encoder.frame_size() as usize;
    if frame_size == 0 {
        bail!("libopus reported a zero audio frame size");
    }
    let mut samples = [Vec::new(), Vec::new()];
    let mut next_audio_pts = 0_i64;
    progress.print();

    for (stream, mut packet) in input_context.packets() {
        match stream.index() {
            index if video_index == Some(index) => {
                let video_output_index =
                    video_output_index.context("video output stream missing")?;
                progress.report(packet.pts().or_else(|| packet.dts()), stream.time_base());
                let time_base = output_context
                    .stream(video_output_index)
                    .context("video output stream missing")?
                    .time_base();
                packet.rescale_ts(stream.time_base(), time_base);
                packet.set_position(-1);
                packet.set_stream(video_output_index);
                packet.write_interleaved(&mut output_context)?;
            }
            index if index == audio_index => {
                if video_index.is_none() {
                    progress.report(packet.pts().or_else(|| packet.dts()), audio_time_base);
                }
                audio_decoder.send_packet(&packet)?;
                decode_and_normalize(
                    &mut audio_decoder,
                    &mut decode_resampler,
                    &mut loudness,
                    &mut samples,
                )?;
                write_ready_audio(
                    &mut samples,
                    frame_size,
                    &mut next_audio_pts,
                    &mut audio_encoder,
                    &mut encode_resampler,
                    &mut output_context,
                    audio_output_index,
                )?;
            }
            _ => {}
        }
    }

    audio_decoder.send_eof()?;
    decode_and_normalize(
        &mut audio_decoder,
        &mut decode_resampler,
        &mut loudness,
        &mut samples,
    )?;
    flush_decode_resampler(&mut decode_resampler, &mut loudness, &mut samples)?;
    loudness.flush(&mut samples);
    write_ready_audio(
        &mut samples,
        frame_size,
        &mut next_audio_pts,
        &mut audio_encoder,
        &mut encode_resampler,
        &mut output_context,
        audio_output_index,
    )?;
    if !samples[0].is_empty() {
        for channel in &mut samples {
            channel.resize(frame_size, 0.0);
        }
        write_ready_audio(
            &mut samples,
            frame_size,
            &mut next_audio_pts,
            &mut audio_encoder,
            &mut encode_resampler,
            &mut output_context,
            audio_output_index,
        )?;
    }
    flush_encode_resampler(
        &mut encode_resampler,
        &mut audio_encoder,
        &mut output_context,
        audio_output_index,
    )?;
    audio_encoder.send_eof()?;
    write_encoded_packets(&mut audio_encoder, &mut output_context, audio_output_index)?;
    output_context.write_trailer()?;

    progress.finish();
    println!("created: {}", output.display());
    println!("final metrics: {:#?}", loudness.metrics());
    Ok(())
}

struct Progress {
    duration_us: Option<i64>,
    latest_us: i64,
    last_reported_us: i64,
}

impl Progress {
    fn new(duration_us: i64) -> Self {
        Self {
            duration_us: (duration_us > 0).then_some(duration_us),
            latest_us: 0,
            last_reported_us: 0,
        }
    }

    fn report(&mut self, timestamp: Option<i64>, time_base: ffmpeg::Rational) {
        let Some(timestamp) = timestamp else {
            return;
        };
        self.latest_us = self.latest_us.max(timestamp.rescale(time_base, TIME_BASE));
        if self.latest_us - self.last_reported_us >= 1_000_000 {
            self.print();
            self.last_reported_us = self.latest_us;
        }
    }

    fn finish(&mut self) {
        if let Some(duration_us) = self.duration_us {
            self.latest_us = duration_us;
        }
        self.print();
        println!();
    }

    fn print(&self) {
        let seconds = self.latest_us as f64 / 1_000_000.0;
        if let Some(duration_us) = self.duration_us {
            let percent = (self.latest_us as f64 / duration_us as f64 * 100.0).min(100.0);
            print!("\rprocessing: {seconds:.1} s ({percent:.0}%)");
        } else {
            print!("\rprocessing: {seconds:.1} s");
        }
        let _ = io::stdout().flush();
    }
}

struct LoudnessPipeline {
    processor: LiveLoudnessProcessor,
    lookahead_samples: usize,
    buffered_samples: usize,
    pending: VecDeque<frame::Audio>,
}

impl LoudnessPipeline {
    fn new(config: LiveLoudnessConfig, mode: AnalysisMode) -> Result<Self> {
        let mut processor = LiveLoudnessProcessor::new(SAMPLE_RATE, config)?;
        let lookahead_samples = match mode {
            AnalysisMode::ImmediateShortTerm => 0,
            AnalysisMode::Momentary500ms => {
                processor.set_measurement(LiveLoudnessMeasurement::Momentary);
                SAMPLE_RATE as usize / 2
            }
            AnalysisMode::ShortTerm3s => SAMPLE_RATE as usize * 3,
        };
        Ok(Self {
            processor,
            lookahead_samples,
            buffered_samples: 0,
            pending: VecDeque::new(),
        })
    }

    fn process(&mut self, mut frame: frame::Audio, samples: &mut [Vec<f32>; CHANNELS]) {
        if self.lookahead_samples == 0 {
            self.processor.process(&mut frame);
            append_frame(samples, &frame);
            return;
        }

        self.processor.analyze(&mut frame);
        self.buffered_samples += frame.samples();
        self.pending.push_back(frame);
        while self.buffered_samples >= self.lookahead_samples {
            let mut buffered = self
                .pending
                .pop_front()
                .expect("lookahead buffer is not empty");
            self.buffered_samples -= buffered.samples();
            self.processor.apply_gain(&mut buffered);
            append_frame(samples, &buffered);
        }
    }

    fn flush(&mut self, samples: &mut [Vec<f32>; CHANNELS]) {
        while let Some(mut frame) = self.pending.pop_front() {
            self.processor.apply_gain(&mut frame);
            append_frame(samples, &frame);
        }
        self.buffered_samples = 0;
    }

    fn metrics(&self) -> LiveLoudnessMetrics {
        self.processor.metrics()
    }
}

fn append_frame(samples: &mut [Vec<f32>; CHANNELS], frame: &frame::Audio) {
    for (channel, buffer) in samples.iter_mut().enumerate() {
        buffer.extend_from_slice(frame.plane::<f32>(channel));
    }
}

fn decode_and_normalize(
    decoder: &mut codec::decoder::Audio,
    resampler: &mut resampling::Context,
    loudness: &mut LoudnessPipeline,
    samples: &mut [Vec<f32>; CHANNELS],
) -> Result<()> {
    let mut decoded = frame::Audio::empty();
    while decoder.receive_frame(&mut decoded).is_ok() {
        if decoded.channel_layout().is_empty() {
            decoded.set_channel_layout(channel_layout(decoder));
        }
        let converted = resample_frame(resampler, &decoded)?;
        loudness.process(converted, samples);
    }
    Ok(())
}

fn flush_decode_resampler(
    resampler: &mut resampling::Context,
    loudness: &mut LoudnessPipeline,
    samples: &mut [Vec<f32>; CHANNELS],
) -> Result<()> {
    loop {
        let mut converted =
            frame::Audio::new(Sample::F32(Type::Planar), 4096, ChannelLayout::STEREO);
        converted.set_rate(SAMPLE_RATE);
        if resampler.flush(&mut converted)?.is_none() || converted.samples() == 0 {
            return Ok(());
        }
        loudness.process(converted, samples);
    }
}

fn write_ready_audio(
    samples: &mut [Vec<f32>; CHANNELS],
    frame_size: usize,
    next_pts: &mut i64,
    encoder: &mut codec::encoder::Audio,
    resampler: &mut Option<resampling::Context>,
    output: &mut format::context::Output,
    output_index: usize,
) -> Result<()> {
    while samples.iter().all(|channel| channel.len() >= frame_size) {
        let mut audio =
            frame::Audio::new(Sample::F32(Type::Planar), frame_size, ChannelLayout::STEREO);
        audio.set_rate(SAMPLE_RATE);
        audio.set_pts(Some(*next_pts));
        for (channel, buffer) in samples.iter_mut().enumerate() {
            audio
                .plane_mut::<f32>(channel)
                .copy_from_slice(&buffer[..frame_size]);
            buffer.drain(..frame_size);
        }
        *next_pts += frame_size as i64;
        if let Some(resampler) = resampler {
            let mut converted = frame::Audio::empty();
            resampler.run(&audio, &mut converted)?;
            converted.set_pts(audio.pts());
            encoder.send_frame(&converted)?;
        } else {
            encoder.send_frame(&audio)?;
        }
        write_encoded_packets(encoder, output, output_index)?;
    }
    Ok(())
}

fn flush_encode_resampler(
    resampler: &mut Option<resampling::Context>,
    encoder: &mut codec::encoder::Audio,
    output: &mut format::context::Output,
    output_index: usize,
) -> Result<()> {
    let Some(resampler) = resampler else {
        return Ok(());
    };
    loop {
        let definition = *resampler.output();
        let mut converted = frame::Audio::new(definition.format, 4096, definition.channel_layout);
        converted.set_rate(definition.rate);
        if resampler.flush(&mut converted)?.is_none() || converted.samples() == 0 {
            return Ok(());
        }
        encoder.send_frame(&converted)?;
        write_encoded_packets(encoder, output, output_index)?;
    }
}

fn write_encoded_packets(
    encoder: &mut codec::encoder::Audio,
    output: &mut format::context::Output,
    output_index: usize,
) -> Result<()> {
    let time_base = output
        .stream(output_index)
        .context("audio output stream missing")?
        .time_base();
    let mut packet = ffmpeg::Packet::empty();
    while encoder.receive_packet(&mut packet).is_ok() {
        packet.set_stream(output_index);
        packet.rescale_ts(encoder.time_base(), time_base);
        packet.write_interleaved(output)?;
    }
    Ok(())
}

fn resample_frame(
    resampler: &mut resampling::Context,
    input: &frame::Audio,
) -> Result<frame::Audio> {
    let capacity =
        unsafe { ffmpeg::ffi::swr_get_out_samples(resampler.as_mut_ptr(), input.samples() as i32) };
    if capacity < 0 {
        return Err(ffmpeg::Error::from(capacity)).context("calculating resampler capacity");
    }
    let definition = *resampler.output();
    let mut output = frame::Audio::new(
        definition.format,
        capacity.max(1) as usize,
        definition.channel_layout,
    );
    output.set_rate(definition.rate);
    resampler.run(input, &mut output)?;
    Ok(output)
}

fn channel_layout(decoder: &codec::decoder::Audio) -> ChannelLayout {
    let layout = decoder.channel_layout();
    if layout.is_empty() {
        ChannelLayout::default(i32::from(decoder.channels()).max(1))
    } else {
        layout
    }
}

fn preferred_audio_format(codec: codec::codec::Codec) -> Result<Sample> {
    let preferred = Sample::F32(Type::Planar);
    let formats: Vec<_> = codec
        .audio()?
        .formats()
        .context("libopus exposes no sample formats")?
        .collect();
    formats
        .iter()
        .copied()
        .find(|format| *format == preferred)
        .or_else(|| formats.first().copied())
        .context("libopus exposes an empty sample format list")
}

fn output_path(input: &Path, has_video: bool, analysis_mode: AnalysisMode) -> Result<PathBuf> {
    if !input.is_file() {
        bail!("input file not found: {}", input.display());
    }
    let parent = input.parent().unwrap_or_else(|| Path::new("."));
    let stem = input
        .file_stem()
        .context("input file has no name")?
        .to_string_lossy();
    let extension = if has_video { "mp4" } else { "opus" };
    Ok(parent.join(format!(
        "{stem} # live_loudness # {}.{extension}",
        analysis_mode.file_suffix()
    )))
}
