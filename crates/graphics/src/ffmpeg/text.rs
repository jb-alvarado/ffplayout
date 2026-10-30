//! Prepare a straight-alpha text bitmap for the YUV420 compositor.
use std::sync::Arc;

use anyhow::{Context, Result};
use ffmpeg_next::{frame, util::format::pixel::Pixel};

use crate::{
    animation::{TextPlayback, TextPreparation},
    bitmap::StraightRgbaBitmap,
    style::TextStyle,
    text::rasterize_overlay_text,
};

use super::overlay::{OverlayFrame, alpha_bounds, blend_overlay, chroma_alpha, prepare_execution};

pub struct TextOverlay {
    overlay: Arc<OverlayFrame>,
    playback: TextPlayback,
}

impl TextOverlay {
    pub fn prepare(
        style: &TextStyle,
        text: &str,
        context: TextPreparation,
    ) -> Result<Option<Self>> {
        if text.trim().is_empty() {
            return Ok(None);
        }
        let bitmap =
            rasterize_overlay_text(style, text, context.canvas_width, context.canvas_height)?;
        let playback = TextPlayback::new(style, context, (bitmap.width, bitmap.height));
        let prepared = rgba_to_yuva420p(&bitmap).context("failed to create text overlay frame")?;
        let alpha = chroma_alpha(&prepared, bitmap.width, bitmap.height);
        let bounds = alpha_bounds(&prepared);
        if style.opacity > 0.0 {
            prepare_execution(bounds);
        }

        Ok(Some(Self {
            overlay: Arc::new(OverlayFrame {
                bounds,
                frame: prepared,
                chroma_alpha: alpha,
                x: 0,
                y: 0,
                width: bitmap.width,
                height: bitmap.height,
                opacity: (style.opacity.clamp(0.0, 1.0) * 255.0).round() as u8,
            }),
            playback,
        }))
    }

    pub fn snapshot(&self, program_frame: i64, scroll_frame: i64) -> Option<TextFrame> {
        let placement = self.playback.evaluate(program_frame, scroll_frame);
        (placement.opacity > 0.0).then(|| TextFrame {
            surface: self.overlay.clone(),
            x: even_signed(placement.x),
            y: even_signed(placement.y),
            opacity: placement.opacity,
        })
    }

    pub fn blend(&mut self, target: &mut frame::Video, program_frame: i64, scroll_frame: i64) {
        if let Some(snapshot) = self.snapshot(program_frame, scroll_frame) {
            snapshot.blend(target);
        }
    }

    pub fn dimensions(&self) -> (u32, u32) {
        (self.overlay.width, self.overlay.height)
    }
}

/// Immutable pixels and per-program-frame placement; safe to retain in bounded queues.
#[derive(Clone)]
pub struct TextFrame {
    surface: Arc<OverlayFrame>,
    x: i32,
    y: i32,
    opacity: f64,
}

pub type TextOverlays = [Option<TextFrame>; 2];

#[derive(Default)]
pub struct TextScaleCache {
    geometry: Option<((u32, u32), (u32, u32))>,
    source: Option<Arc<OverlayFrame>>,
    scaled: Option<Arc<OverlayFrame>>,
}

impl TextFrame {
    pub fn dimensions(&self) -> (u32, u32) {
        (self.surface.width, self.surface.height)
    }

    pub fn blend(&self, target: &mut frame::Video) {
        let mut overlay = self.surface.as_ref().as_ref();
        overlay.x = self.x;
        overlay.y = self.y;
        blend_overlay(target, overlay, self.opacity);
    }

    /// Scale a prepared bitmap only when its resource changes. Scrolling and
    /// fading update placement without rerasterizing or rescaling the pixels.
    pub fn blend_scaled(
        &self,
        target: &mut frame::Video,
        canvas: (u32, u32),
        cache: &mut TextScaleCache,
    ) -> Result<()> {
        let destination = (target.width(), target.height());
        if canvas == destination {
            self.blend(target);
            return Ok(());
        }
        if cache.geometry != Some((canvas, destination))
            || !cache
                .source
                .as_ref()
                .is_some_and(|source| Arc::ptr_eq(source, &self.surface))
        {
            let width = ((u64::from(self.surface.width) * u64::from(destination.0)
                / u64::from(canvas.0)) as u32)
                .max(2)
                & !1;
            let height = ((u64::from(self.surface.height) * u64::from(destination.1)
                / u64::from(canvas.1)) as u32)
                .max(2)
                & !1;
            let mut scaler = ffmpeg_next::software::scaling::Context::get(
                Pixel::YUVA420P,
                self.surface.width,
                self.surface.height,
                Pixel::YUVA420P,
                width,
                height,
                ffmpeg_next::software::scaling::flag::Flags::BILINEAR,
            )?;
            let mut prepared = frame::Video::empty();
            scaler.run(&self.surface.frame, &mut prepared)?;
            cache.scaled = Some(Arc::new(OverlayFrame {
                bounds: alpha_bounds(&prepared),
                chroma_alpha: chroma_alpha(&prepared, width, height),
                frame: prepared,
                width,
                height,
                x: 0,
                y: 0,
                opacity: self.surface.opacity,
            }));
            cache.source = Some(self.surface.clone());
            cache.geometry = Some((canvas, destination));
        }
        if let Some(surface) = &cache.scaled {
            let snapshot = Self {
                surface: surface.clone(),
                x: even_signed(
                    (i64::from(self.x) * i64::from(destination.0) / i64::from(canvas.0)) as i32,
                ),
                y: even_signed(
                    (i64::from(self.y) * i64::from(destination.1) / i64::from(canvas.1)) as i32,
                ),
                opacity: self.opacity,
            };
            snapshot.blend(target);
        }

        Ok(())
    }
}

fn rgba_to_yuva420p(bitmap: &StraightRgbaBitmap) -> Result<frame::Video> {
    let rgba = &bitmap.pixels;
    let (width, height) = (bitmap.width, bitmap.height);
    let mut frame = frame::Video::new(Pixel::YUVA420P, width, height);
    let width = width as usize;
    let height = height as usize;

    let y_stride = frame.stride(0);
    let u_stride = frame.stride(1);
    let v_stride = frame.stride(2);
    let a_stride = frame.stride(3);

    for y in 0..height {
        for x in 0..width {
            let src_idx = (y * width + x) * 4;
            let (yy, _, _) = rgb_to_yuv(rgba[src_idx], rgba[src_idx + 1], rgba[src_idx + 2]);
            frame.data_mut(0)[y * y_stride + x] = yy;
            frame.data_mut(3)[y * a_stride + x] = rgba[src_idx + 3];
        }
    }

    for y in 0..height / 2 {
        for x in 0..width / 2 {
            let mut r = 0_u32;
            let mut g = 0_u32;
            let mut b = 0_u32;
            let mut coverage = 0_u32;

            for dy in 0..2 {
                for dx in 0..2 {
                    let px = x * 2 + dx;
                    let py = y * 2 + dy;
                    let src_idx = (py * width + px) * 4;
                    let alpha = u32::from(rgba[src_idx + 3]);
                    r += u32::from(rgba[src_idx]) * alpha;
                    g += u32::from(rgba[src_idx + 1]) * alpha;
                    b += u32::from(rgba[src_idx + 2]) * alpha;
                    coverage += alpha;
                }
            }

            // Reduce color by coverage before subsampling alpha separately.
            // Transparent RGB must not dilute visible edge colors.
            let rgb = if coverage == 0 {
                [0; 3]
            } else {
                [r, g, b].map(|value| ((value + coverage / 2) / coverage) as u8)
            };
            let (_, u, v) = rgb_to_yuv(rgb[0], rgb[1], rgb[2]);
            frame.data_mut(1)[y * u_stride + x] = u;
            frame.data_mut(2)[y * v_stride + x] = v;
        }
    }

    Ok(frame)
}

fn rgb_to_yuv(r: u8, g: u8, b: u8) -> (u8, u8, u8) {
    let r = i32::from(r);
    let g = i32::from(g);
    let b = i32::from(b);
    let y = ((66 * r + 129 * g + 25 * b + 128) >> 8) + 16;
    let u = ((-38 * r - 74 * g + 112 * b + 128) >> 8) + 128;
    let v = ((112 * r - 94 * g - 18 * b + 128) >> 8) + 128;
    (clamp_u8(y), clamp_u8(u), clamp_u8(v))
}

fn clamp_u8(value: i32) -> u8 {
    value.clamp(0, 255) as u8
}

fn even_signed(value: i32) -> i32 {
    value & !1
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        animation::TextTiming,
        style::{RgbaColor, TextPosition},
    };

    #[test]
    fn text_preparation_applies_white_coverage_only_once() {
        let style = TextStyle {
            text_color: RgbaColor {
                r: 255,
                g: 255,
                b: 255,
                a: 128,
            },
            position_x: TextPosition::Pixels(0),
            position_y: TextPosition::Pixels(0),
            ..TextStyle::default()
        };
        let context = TextPreparation {
            canvas_width: 320,
            canvas_height: 100,
            frames_per_second: 25,
            timing: TextTiming::default(),
        };
        let mut overlay = TextOverlay::prepare(&style, "MMMM", context)
            .unwrap()
            .unwrap();
        let mut target = frame::Video::new(Pixel::YUV420P, 320, 100);
        for plane in 0..3 {
            target
                .data_mut(plane)
                .fill(if plane == 0 { 16 } else { 128 });
        }
        overlay.blend(&mut target, 0, 0);
        // Limited-range white at 50% coverage over black is Y=126, not Y=71.
        assert_eq!(target.data(0).iter().copied().max(), Some(126));
    }

    #[test]
    fn transparent_neighbors_do_not_dilute_chroma() {
        let bitmap = StraightRgbaBitmap {
            width: 2,
            height: 2,
            pixels: vec![255, 0, 0, 255, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0],
        };
        let prepared = rgba_to_yuva420p(&bitmap).unwrap();
        let (_, u, v) = rgb_to_yuv(255, 0, 0);
        assert_eq!(prepared.data(1)[0], u);
        assert_eq!(prepared.data(2)[0], v);
        assert_eq!(chroma_alpha(&prepared, 2, 2), vec![63]);
    }
}
