//! Offline reference runner for the engine's live loudness processor.
//!
//! ```text
//! cargo run -p ff-engine --example live_loudness -- INPUT_FILE [-17]
//! ```
//!
//! The video stream is copied; the first audio stream is decoded, normalized by
//! [`LiveLoudnessProcessor`], and encoded as 128 kbit/s Opus.  No external
//! `ffmpeg` executable is used.

use std::{
    env,
    io::{self, Write},
    path::{Path, PathBuf},
};

use anyhow::{Context, Result, bail};
use ff_engine::{LiveLoudnessConfig, LiveLoudnessProcessor};
use ffmpeg::Rescale;
use ffmpeg::{
    codec, encoder, format, frame, media,
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

fn main() -> Result<()> {
    let (input, target_lufs) = arguments()?;
    let output = output_path(&input)?;
    if output.exists() {
        bail!("output file already exists: {}", output.display());
    }

    ffmpeg::init().context("initializing FFmpeg libraries")?;
    let mut input_context = format::input(&input).context("opening input")?;
    let video_input = input_context
        .streams()
        .best(media::Type::Video)
        .context("input has no video stream")?;
    let audio_input = input_context
        .streams()
        .best(media::Type::Audio)
        .context("input has no audio stream")?;
    let video_index = video_input.index();
    let audio_index = audio_input.index();
    let video_time_base = video_input.time_base();
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

    let mut output_context = format::output(&output).context("creating MP4 output")?;
    let video_output_index = {
        let mut stream = output_context.add_stream(encoder::find(codec::Id::None))?;
        stream.set_parameters(video_input.parameters());
        // The input tag can be invalid in an MP4 stream-copy output.
        unsafe {
            (*stream.parameters().as_mut_ptr()).codec_tag = 0;
        }
        stream.set_time_base(video_time_base);
        stream.index()
    };

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

    let mut processor = LiveLoudnessProcessor::new(
        SAMPLE_RATE,
        LiveLoudnessConfig {
            target_lufs,
            ..Default::default()
        },
    )?;
    let frame_size = audio_encoder.frame_size() as usize;
    if frame_size == 0 {
        bail!("libopus reported a zero audio frame size");
    }
    let mut samples = [Vec::new(), Vec::new()];
    let mut next_audio_pts = 0_i64;
    progress.print();

    for (stream, mut packet) in input_context.packets() {
        match stream.index() {
            index if index == video_index => {
                progress.report(packet.pts().or_else(|| packet.dts()), video_time_base);
                let time_base = output_context
                    .stream(video_output_index)
                    .context("video output stream missing")?
                    .time_base();
                packet.rescale_ts(video_time_base, time_base);
                packet.set_position(-1);
                packet.set_stream(video_output_index);
                packet.write_interleaved(&mut output_context)?;
            }
            index if index == audio_index => {
                audio_decoder.send_packet(&packet)?;
                decode_and_normalize(
                    &mut audio_decoder,
                    &mut decode_resampler,
                    &mut processor,
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
        &mut processor,
        &mut samples,
    )?;
    flush_decode_resampler(&mut decode_resampler, &mut processor, &mut samples)?;
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
    println!("final metrics: {:?}", processor.metrics());
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

fn decode_and_normalize(
    decoder: &mut codec::decoder::Audio,
    resampler: &mut resampling::Context,
    processor: &mut LiveLoudnessProcessor,
    samples: &mut [Vec<f32>; CHANNELS],
) -> Result<()> {
    let mut decoded = frame::Audio::empty();
    while decoder.receive_frame(&mut decoded).is_ok() {
        if decoded.channel_layout().is_empty() {
            decoded.set_channel_layout(channel_layout(decoder));
        }
        let mut converted = resample_frame(resampler, &decoded)?;
        processor.process(&mut converted);
        for (channel, buffer) in samples.iter_mut().enumerate() {
            buffer.extend_from_slice(converted.plane::<f32>(channel));
        }
    }
    Ok(())
}

fn flush_decode_resampler(
    resampler: &mut resampling::Context,
    processor: &mut LiveLoudnessProcessor,
    samples: &mut [Vec<f32>; CHANNELS],
) -> Result<()> {
    loop {
        let mut converted =
            frame::Audio::new(Sample::F32(Type::Planar), 4096, ChannelLayout::STEREO);
        converted.set_rate(SAMPLE_RATE);
        if resampler.flush(&mut converted)?.is_none() || converted.samples() == 0 {
            return Ok(());
        }
        processor.process(&mut converted);
        for (channel, buffer) in samples.iter_mut().enumerate() {
            buffer.extend_from_slice(converted.plane::<f32>(channel));
        }
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

fn arguments() -> Result<(PathBuf, f64)> {
    let mut arguments = env::args_os().skip(1);
    let input = arguments
        .next()
        .context("usage: live_loudness INPUT_FILE [-17]")?;
    let target_lufs = arguments
        .next()
        .map(|value| {
            value
                .into_string()
                .map_err(|_| anyhow::anyhow!("LUFS target must be UTF-8"))
        })
        .transpose()?
        .map(|value| value.parse::<f64>().context("parsing LUFS target"))
        .transpose()?
        .unwrap_or(-23.0);
    if arguments.next().is_some() || !target_lufs.is_finite() {
        bail!("usage: live_loudness INPUT_FILE [-17]");
    }
    Ok((PathBuf::from(input), target_lufs))
}

fn output_path(input: &Path) -> Result<PathBuf> {
    if !input.is_file() {
        bail!("input file not found: {}", input.display());
    }
    let parent = input.parent().unwrap_or_else(|| Path::new("."));
    let stem = input
        .file_stem()
        .context("input file has no name")?
        .to_string_lossy();
    Ok(parent.join(format!("{stem} # live_loudness.mp4")))
}
