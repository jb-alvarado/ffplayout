//! Native text layout/rasterization. Produces CPU RGBA bitmaps, not video frames.
use std::{
    collections::BTreeSet,
    mem,
    sync::{Mutex, OnceLock, PoisonError},
};

use anyhow::{Result, anyhow};
use cosmic_text::{
    Align, Attrs, Buffer, Color, Family, FontSystem, Metrics, Shaping, SwashCache, Weight, Wrap,
};

use crate::{
    bitmap::{StraightRgbaBitmap, source_over_straight},
    even,
    style::{RgbaColor, TextScroll, TextStyle, TextWeight},
};

static TEXT_RENDERER: OnceLock<Mutex<TextRenderer>> = OnceLock::new();

const MAX_GLYPH_CACHE_ENTRIES: usize = 4096;
const MAX_GLYPH_CACHE_BYTES: usize = 32 * 1024 * 1024;

// Swash caches are append-only while drawing. Unchanged entry counts need no
// repeated byte scan. Evict the largest entries first with 25% headroom, keeping
// small reusable glyphs instead of forcing a complete cache rebuild.
fn trim_glyph_cache(cache: &mut SwashCache, checked: &mut Option<(usize, usize)>) {
    let counts = (cache.image_cache.len(), cache.outline_command_cache.len());

    if *checked == Some(counts) {
        return;
    }

    let mut entries = Vec::with_capacity(counts.0 + counts.1);

    for (key, image) in &cache.image_cache {
        entries.push((
            *key,
            true,
            image.as_ref().map_or(0, |image| image.data.capacity()),
        ));
    }

    for (key, commands) in &cache.outline_command_cache {
        entries.push((
            *key,
            false,
            commands
                .as_ref()
                .map_or(0, |commands| mem::size_of_val(commands.as_ref())),
        ));
    }

    let mut bytes = entries
        .iter()
        .fold(0usize, |sum, entry| sum.saturating_add(entry.2));
    let mut count = entries.len();

    if count > MAX_GLYPH_CACHE_ENTRIES || bytes > MAX_GLYPH_CACHE_BYTES {
        entries.sort_unstable_by_key(|entry| std::cmp::Reverse(entry.2));

        for (key, image, size) in entries {
            if count <= MAX_GLYPH_CACHE_ENTRIES * 3 / 4 && bytes <= MAX_GLYPH_CACHE_BYTES * 3 / 4 {
                break;
            }

            if image {
                cache.image_cache.remove(&key);
            } else {
                cache.outline_command_cache.remove(&key);
            }

            count -= 1;
            bytes = bytes.saturating_sub(size);
        }
        cache.image_cache.shrink_to_fit();
        cache.outline_command_cache.shrink_to_fit();
    }
    *checked = Some((cache.image_cache.len(), cache.outline_command_cache.len()));
}

struct TextRenderer {
    font_system: FontSystem,
    swash_cache: SwashCache,
    cache_checked: Option<(usize, usize)>,
}

impl TextRenderer {
    fn new() -> Self {
        Self {
            font_system: FontSystem::new(),
            swash_cache: SwashCache::new(),
            cache_checked: None,
        }
    }
}

pub fn init() {
    TEXT_RENDERER.get_or_init(|| Mutex::new(TextRenderer::new()));
}

fn renderer() -> &'static Mutex<TextRenderer> {
    TEXT_RENDERER.get_or_init(|| Mutex::new(TextRenderer::new()))
}

pub fn available_font_families() -> Vec<String> {
    let renderer = renderer().lock().unwrap_or_else(PoisonError::into_inner);
    renderer
        .font_system
        .db()
        .faces()
        .filter_map(|face| face.families.first().map(|(family, _)| family.clone()))
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect()
}

pub fn render_wrapped_text_bitmap(
    text: &str,
    font_size: f32,
    font_weight: TextWeight,
    color: RgbaColor,
    max_width: u32,
) -> Result<StraightRgbaBitmap> {
    render_text_bitmap(
        text,
        font_size,
        font_weight,
        color,
        Some(max_width.max(2)),
        Some(Wrap::WordOrGlyph),
        Some(Align::Center),
    )
}

pub fn render_left_aligned_text_bitmap(
    text: &str,
    font_size: f32,
    font_weight: TextWeight,
    color: RgbaColor,
    max_width: u32,
) -> Result<StraightRgbaBitmap> {
    render_text_bitmap(
        text,
        font_size,
        font_weight,
        color,
        Some(max_width.max(2)),
        Some(Wrap::WordOrGlyph),
        Some(Align::Left),
    )
}

fn render_text_bitmap(
    text: &str,
    font_size: f32,
    font_weight: TextWeight,
    color: RgbaColor,
    max_width: Option<u32>,
    wrap: Option<Wrap>,
    align: Option<Align>,
) -> Result<StraightRgbaBitmap> {
    if text.trim().is_empty() {
        return Err(anyhow!("text bitmap input is empty"));
    }

    if !font_size.is_finite() || font_size <= 0.0 {
        return Err(anyhow!("text font size must be a positive number"));
    }

    let estimated_width = (text.chars().count() as f32 * font_size * 0.75).ceil() as u32 + 8;
    let width = max_width
        .unwrap_or(estimated_width)
        .min(estimated_width.max(2))
        .max(2);
    let line_height = font_size * 1.25;
    let estimated_lines =
        ((estimated_width as f32 / width as f32).ceil() as u32).max(text.lines().count() as u32);
    let height = ((line_height * estimated_lines as f32).ceil() as u32 + 4).max(2);
    let mut rgba = vec![0_u8; width as usize * height as usize * 4];
    let mut renderer = renderer().lock().unwrap_or_else(PoisonError::into_inner);
    let TextRenderer {
        font_system,
        swash_cache,
        cache_checked,
    } = &mut *renderer;
    let mut buffer = Buffer::new(font_system, Metrics::new(font_size, line_height));
    buffer.set_size(Some(width as f32), Some(height as f32));

    if let Some(wrap) = wrap {
        buffer.set_wrap(wrap);
    }
    buffer.set_text(
        text,
        &Attrs::new().weight(cosmic_weight(font_weight)),
        Shaping::Advanced,
        align,
    );
    buffer.shape_until_scroll(font_system, false);

    let mut bounds = Bounds::default();
    let text_opacity = color.a;
    buffer.draw(
        font_system,
        swash_cache,
        Color::rgba(color.r, color.g, color.b, color.a),
        |x, y, pixel_width, pixel_height, color| {
            let color = glyph_color(color, text_opacity);
            let Some(dest) = PixelRect::new(x, y, pixel_width, pixel_height, width, height) else {
                return;
            };

            bounds.include(dest.x, dest.y, dest.width, dest.height);

            for py in 0..dest.height {
                for px in 0..dest.width {
                    let idx = ((dest.y + py) * width as usize + dest.x + px) * 4;
                    alpha_composite(&mut rgba[idx..idx + 4], color);
                }
            }
        },
    );

    trim_glyph_cache(swash_cache, cache_checked);
    let Some(bounds) = bounds.finish() else {
        return Err(anyhow!("text bitmap produced no visible pixels"));
    };
    let mut cropped = vec![0_u8; bounds.width * bounds.height * 4];

    for y in 0..bounds.height {
        let src_start = ((bounds.y + y) * width as usize + bounds.x) * 4;
        let src_end = src_start + bounds.width * 4;
        let dst_start = y * bounds.width * 4;
        cropped[dst_start..dst_start + bounds.width * 4].copy_from_slice(&rgba[src_start..src_end]);
    }

    Ok(StraightRgbaBitmap {
        pixels: cropped,
        width: bounds.width as u32,
        height: bounds.height as u32,
    })
}

/// Rasterize text/background into straight-alpha RGBA for backend preparation.
/// This is a CPU bitmap, independent of the final video frame format.
pub fn rasterize_overlay_text(
    config: &TextStyle,
    text: &str,
    output_width: u32,
    output_height: u32,
) -> Result<StraightRgbaBitmap> {
    if !config.font_size.is_finite() || config.font_size <= 0.0 {
        return Err(anyhow!("text font size must be a positive number"));
    }

    let padding = config
        .background
        .map(|background| background.padding)
        .unwrap_or(0);
    let render_width = render_width(config, text, output_width, padding);
    let text_width = render_width
        .saturating_sub(padding.saturating_mul(2))
        .max(2);
    let text_height = output_height
        .saturating_sub(padding.saturating_mul(2))
        .max(2);
    let line_height = (config.font_size + config.line_spacing.max(0.0))
        .ceil()
        .max(config.font_size);

    let mut rgba = vec![0_u8; (render_width as usize) * (output_height as usize) * 4];
    let mut renderer = renderer().lock().unwrap_or_else(PoisonError::into_inner);
    let TextRenderer {
        font_system,
        swash_cache,
        cache_checked,
    } = &mut *renderer;
    let metrics = Metrics::new(config.font_size, line_height);
    let mut buffer = Buffer::new(font_system, metrics);
    buffer.set_size(Some(text_width as f32), Some(text_height as f32));

    let mut attrs = Attrs::new();

    if let Some(family) = &config.font_family {
        attrs = attrs.family(Family::Name(family));
    }

    attrs = attrs.weight(cosmic_weight(config.font_weight));
    buffer.set_text(text, &attrs, Shaping::Advanced, None);
    buffer.shape_until_scroll(font_system, false);

    let text_color = Color::rgba(
        config.text_color.r,
        config.text_color.g,
        config.text_color.b,
        config.text_color.a,
    );
    let mut bounds = Bounds::default();
    buffer.draw(
        font_system,
        swash_cache,
        text_color,
        |x, y, width, height, color| {
            let color = glyph_color(color, config.text_color.a);
            let Some(dest) = PixelRect::new(
                x + i32::try_from(padding).unwrap_or(0),
                y + i32::try_from(padding).unwrap_or(0),
                width,
                height,
                render_width,
                output_height,
            ) else {
                return;
            };
            bounds.include(dest.x, dest.y, dest.width, dest.height);

            for py in 0..dest.height {
                for px in 0..dest.width {
                    let idx = ((dest.y + py) * render_width as usize + dest.x + px) * 4;
                    alpha_composite(&mut rgba[idx..idx + 4], color);
                }
            }
        },
    );

    trim_glyph_cache(swash_cache, cache_checked);
    let Some(mut bounds) = bounds.finish() else {
        return Err(anyhow!("text overlay produced no visible pixels"));
    };
    bounds.expand(
        padding as usize,
        render_width as usize,
        output_height as usize,
    );

    if let Some(background) = config.background {
        let mut boxed = vec![0_u8; rgba.len()];
        draw_box(&mut boxed, render_width, &bounds, background.color);

        for y in bounds.y..bounds.y + bounds.height {
            for x in bounds.x..bounds.x + bounds.width {
                let idx = (y * render_width as usize + x) * 4;
                let text = RgbaColor {
                    r: rgba[idx],
                    g: rgba[idx + 1],
                    b: rgba[idx + 2],
                    a: rgba[idx + 3],
                };
                alpha_composite(&mut boxed[idx..idx + 4], text);
            }
        }

        rgba = boxed;
    }

    let width = even(bounds.width as u32).max(2);
    let height = even(bounds.height as u32).max(2);
    let mut cropped = vec![0_u8; width as usize * height as usize * 4];

    for y in 0..height as usize {
        for x in 0..width as usize {
            let src_idx = ((bounds.y + y) * render_width as usize + bounds.x + x) * 4;
            let dst_idx = (y * width as usize + x) * 4;
            cropped[dst_idx..dst_idx + 4].copy_from_slice(&rgba[src_idx..src_idx + 4]);
        }
    }

    Ok(StraightRgbaBitmap {
        pixels: cropped,
        width,
        height,
    })
}

fn render_width(config: &TextStyle, text: &str, output_width: u32, padding: u32) -> u32 {
    if matches!(config.scroll, TextScroll::None) {
        return output_width.max(2);
    }

    let estimated_text_width = (text.chars().count() as f32 * config.font_size * 0.75).ceil();
    let estimated = estimated_text_width as u32 + padding.saturating_mul(2);
    let max_width = output_width.saturating_mul(8).max(output_width);
    even(estimated.max(output_width).min(max_width)).max(2)
}

fn cosmic_weight(weight: TextWeight) -> Weight {
    match weight {
        TextWeight::Normal => Weight::NORMAL,
        TextWeight::Semibold => Weight::SEMIBOLD,
        TextWeight::Bold => Weight::BOLD,
    }
}

fn glyph_color(color: Color, text_opacity: u8) -> RgbaColor {
    let [r, g, b, coverage] = color.as_rgba();
    // cosmic-text 0.19 / SwashCache::with_pixels returns glyph coverage and
    // ignores the base color's alpha for mask/color glyphs. Apply it once here.
    let a = ((u16::from(coverage) * u16::from(text_opacity) + 127) / 255) as u8;

    RgbaColor { r, g, b, a }
}

fn alpha_composite(dst: &mut [u8], src: RgbaColor) {
    let pixel = dst.try_into().expect("RGBA pixel has four components");
    source_over_straight(pixel, [src.r, src.g, src.b, src.a]);
}

fn draw_box(rgba: &mut [u8], width: u32, bounds: &ResolvedBounds, color: RgbaColor) {
    let width = width as usize;

    for y in bounds.y..bounds.y + bounds.height {
        for x in bounds.x..bounds.x + bounds.width {
            let idx = (y * width + x) * 4;
            alpha_composite(&mut rgba[idx..idx + 4], color);
        }
    }
}

struct PixelRect {
    x: usize,
    y: usize,
    width: usize,
    height: usize,
}

impl PixelRect {
    fn new(
        x: i32,
        y: i32,
        width: u32,
        height: u32,
        max_width: u32,
        max_height: u32,
    ) -> Option<Self> {
        let left = x.max(0);
        let top = y.max(0);
        let right = (x + width as i32).min(max_width as i32);
        let bottom = (y + height as i32).min(max_height as i32);

        if left >= right || top >= bottom {
            return None;
        }

        Some(Self {
            x: left as usize,
            y: top as usize,
            width: (right - left) as usize,
            height: (bottom - top) as usize,
        })
    }
}

#[derive(Default)]
struct Bounds {
    x_min: Option<usize>,
    y_min: Option<usize>,
    x_max: usize,
    y_max: usize,
}

impl Bounds {
    fn include(&mut self, x: usize, y: usize, width: usize, height: usize) {
        self.x_min = Some(self.x_min.map_or(x, |min| min.min(x)));
        self.y_min = Some(self.y_min.map_or(y, |min| min.min(y)));
        self.x_max = self.x_max.max(x + width);
        self.y_max = self.y_max.max(y + height);
    }

    fn finish(self) -> Option<ResolvedBounds> {
        let x = self.x_min?;
        let y = self.y_min?;
        Some(ResolvedBounds {
            x,
            y,
            width: self.x_max.saturating_sub(x).max(1),
            height: self.y_max.saturating_sub(y).max(1),
        })
    }
}

struct ResolvedBounds {
    x: usize,
    y: usize,
    width: usize,
    height: usize,
}

impl ResolvedBounds {
    fn expand(&mut self, padding: usize, max_width: usize, max_height: usize) {
        let x = self.x.saturating_sub(padding);
        let y = self.y.saturating_sub(padding);
        let right = (self.x + self.width + padding).min(max_width);
        let bottom = (self.y + self.height + padding).min(max_height);
        self.x = x;
        self.y = y;
        self.width = right.saturating_sub(x).max(1);
        self.height = bottom.saturating_sub(y).max(1);
    }
}

#[cfg(test)]
mod cache_tests {
    use super::*;

    fn key(glyph: u16) -> cosmic_text::CacheKey {
        cosmic_text::CacheKey::new(
            Default::default(),
            glyph,
            24.0,
            (0.0, 0.0),
            Weight::NORMAL,
            cosmic_text::CacheKeyFlags::empty(),
        )
        .0
    }

    #[test]
    fn retains_small_cache_but_releases_excess_entries() {
        let mut cache = SwashCache::new();
        cache.image_cache.insert(key(0), None);
        trim_glyph_cache(&mut cache, &mut None);
        assert_eq!(cache.image_cache.len(), 1);

        for glyph in 1..=MAX_GLYPH_CACHE_ENTRIES {
            cache.image_cache.insert(key(glyph as u16), None);
        }
        trim_glyph_cache(&mut cache, &mut None);
        assert_eq!(cache.image_cache.len(), MAX_GLYPH_CACHE_ENTRIES * 3 / 4);
        assert!(cache.outline_command_cache.is_empty());
    }

    #[test]
    fn releases_large_raster_even_with_few_entries() {
        let mut cache = SwashCache::new();
        cache.image_cache.insert(key(1), None);
        let mut image = cosmic_text::SwashImage::new();
        image.data.resize(MAX_GLYPH_CACHE_BYTES + 1, 0);
        cache.image_cache.insert(key(0), Some(image));
        trim_glyph_cache(&mut cache, &mut None);
        assert!(!cache.image_cache.contains_key(&key(0)));
        assert!(cache.image_cache.contains_key(&key(1)));
    }

    #[test]
    fn repeated_growth_retains_headroom_and_reuses_unchanged_cache() {
        let mut cache = SwashCache::new();
        let mut checked = None;

        for cycle in 0..20 {
            for glyph in 0..5000 {
                cache.image_cache.insert(key(glyph), None);
            }
            trim_glyph_cache(&mut cache, &mut checked);
            assert_eq!(
                cache.image_cache.len(),
                MAX_GLYPH_CACHE_ENTRIES * 3 / 4,
                "cycle {cycle}"
            );
            let keys: BTreeSet<_> = cache.image_cache.keys().copied().collect();
            trim_glyph_cache(&mut cache, &mut checked);
            assert_eq!(keys, cache.image_cache.keys().copied().collect());
        }
    }

    #[test]
    #[ignore = "manual render-time and cache-growth stress test"]
    fn changing_text_render_stress() {
        let started = std::time::Instant::now();
        let mut worst = std::time::Duration::ZERO;

        for index in 0..1000 {
            let config = TextStyle {
                font_size: (20 + index % 80) as f32,
                ..TextStyle::default()
            };
            let text = format!("Subtitle {index}: changing sizes — ÄÖÜ é 中文 العربية");
            let draw = std::time::Instant::now();
            rasterize_overlay_text(&config, &text, 1920, 1080).unwrap();
            worst = worst.max(draw.elapsed());
        }
        eprintln!(
            "1000 changing overlays: {:?}, slowest render: {:?}",
            started.elapsed(),
            worst
        );
    }
}

#[cfg(test)]
mod raster_tests {
    use super::*;

    #[test]
    fn translucent_white_glyphs_are_straight_alpha() {
        let bitmap = rasterize_overlay_text(
            &TextStyle {
                text_color: RgbaColor {
                    r: 255,
                    g: 255,
                    b: 255,
                    a: 128,
                },
                ..TextStyle::default()
            },
            "MMMM",
            320,
            100,
        )
        .unwrap();
        let visible: Vec<_> = bitmap
            .pixels
            .as_chunks::<4>()
            .0
            .iter()
            .filter(|pixel| pixel[3] > 0)
            .collect();
        assert!(!visible.is_empty());
        assert!(visible.iter().all(|pixel| pixel[..3] == [255, 255, 255]));
        assert!(visible.iter().any(|pixel| pixel[3] == 128));
        assert!(visible.iter().all(|pixel| pixel[3] <= 128));
    }

    #[test]
    fn bitmap_rendering_rejects_invalid_font_metrics() {
        for size in [0.0, -1.0, f32::NAN, f32::INFINITY] {
            assert!(
                render_wrapped_text_bitmap(
                    "Text",
                    size,
                    TextWeight::Normal,
                    RgbaColor::opaque(255, 255, 255),
                    320
                )
                .is_err()
            );
        }
    }
}
