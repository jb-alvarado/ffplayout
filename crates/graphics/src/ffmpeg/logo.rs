use anyhow::{Result, anyhow};
use ffmpeg_next::{codec, frame, media, software::scaling, util::format::pixel::Pixel};

use super::{
    overlay::{
        OverlayBounds, OverlayRef, alpha_bounds, blend_overlay, chroma_alpha, prepare_execution,
    },
    reference_video_frame,
};
use crate::{
    even,
    layout::{logo_dimensions, logo_position},
    style::LogoStyle,
};

#[derive(Clone)]
pub struct LogoOverlay {
    pub frame: frame::Video, // YUVA420P
    chroma_alpha: Vec<u8>,
    bounds: OverlayBounds,
    pub x: u32,
    pub y: u32,
    pub width: u32,
    pub height: u32,
    pub opacity: u8, // 0..255
}

impl LogoOverlay {
    /// Reuse the decoded logo pixels; only placement depends on the destination.
    pub fn positioned(&self, position: Option<&str>, width: u32, height: u32) -> Result<Self> {
        let (x, y) = if let Some(position) = position {
            logo_position(position, width, height, self.width, self.height)?
        } else {
            (self.x, self.y)
        };

        Ok(Self {
            frame: reference_video_frame(&self.frame)?,
            chroma_alpha: self.chroma_alpha.clone(),
            bounds: self.bounds,
            x: even(x),
            y: even(y),
            width: self.width,
            height: self.height,
            opacity: self.opacity,
        })
    }

    pub fn from_input(
        config: &LogoStyle,
        mut ictx: ffmpeg_next::format::context::Input,
        source_label: &str,
        output_width: u32,
        output_height: u32,
    ) -> Result<Self> {
        if !(0.0..=1.0).contains(&config.opacity) || !config.opacity.is_finite() {
            return Err(anyhow!("logo opacity must be between 0.0 and 1.0"));
        }

        let stream = ictx
            .streams()
            .best(media::Type::Video)
            .ok_or_else(|| anyhow!("logo {} contains no video/image stream", source_label))?;

        let stream_index = stream.index();
        let ctx = codec::context::Context::from_parameters(stream.parameters())?;
        let mut decoder = ctx.decoder().video()?;

        let input_width = decoder.width();
        let input_height = decoder.height();

        let (width, height) = logo_dimensions(
            config.scale.as_deref(),
            input_width,
            input_height,
            output_width,
            output_height,
        )?;

        // Convert the logo directly to YUVA420P.
        // Plane 0 = Y
        // Plane 1 = U
        // Plane 2 = V
        // Plane 3 = Alpha
        let mut scaler = scaling::Context::get(
            decoder.format(),
            input_width,
            input_height,
            Pixel::YUVA420P,
            width,
            height,
            scaling::flag::Flags::BILINEAR,
        )?;

        let mut decoded = frame::Video::empty();
        let mut yuva = None;

        for (packet_stream, packet) in ictx.packets() {
            if packet_stream.index() != stream_index {
                continue;
            }

            decoder.send_packet(&packet)?;

            if decoder.receive_frame(&mut decoded).is_ok() {
                let mut scaled = frame::Video::empty();
                scaler.run(&decoded, &mut scaled)?;
                yuva = Some(scaled);
                break;
            }
        }

        if yuva.is_none() {
            decoder.send_eof()?;

            if decoder.receive_frame(&mut decoded).is_ok() {
                let mut scaled = frame::Video::empty();
                scaler.run(&decoded, &mut scaled)?;
                yuva = Some(scaled);
            }
        }

        let frame = yuva.ok_or_else(|| anyhow!("logo {} produced no frame", source_label))?;
        let (x, y) = logo_position(&config.position, output_width, output_height, width, height)?;
        let bounds = alpha_bounds(&frame);
        if config.opacity > 0.0 {
            prepare_execution(bounds);
        }

        Ok(Self {
            chroma_alpha: chroma_alpha(&frame, width, height),
            bounds,
            frame,
            x: even(x),
            y: even(y),
            width,
            height,
            opacity: (config.opacity * 255.0).round().clamp(0.0, 255.0) as u8,
        })
    }

    pub fn as_overlay(&self) -> OverlayRef<'_> {
        OverlayRef {
            frame: &self.frame,
            bounds: self.bounds,
            chroma_alpha: &self.chroma_alpha,
            chroma_alpha_stride: self.width as usize / 2,
            x: self.x as i32,
            y: self.y as i32,
            width: self.width,
            height: self.height,
            opacity: self.opacity,
        }
    }
}

pub fn blend_logo(target: &mut frame::Video, logo: &LogoOverlay, opacity_factor: f64) {
    blend_overlay(target, logo.as_overlay(), opacity_factor);
}
