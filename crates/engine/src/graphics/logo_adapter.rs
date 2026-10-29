//! Engine adapter: retain media I/O policy while graphics owns logo preparation/layout.
use anyhow::{Context, Result, ensure};
use ffmpeg_next::frame;

use ff_graphics::{ffmpeg::logo::LogoOverlay as PreparedLogoOverlay, style::LogoStyle};

use crate::utils::{config::LogoConfig, helper::open_media_input};

#[derive(Clone)]
pub struct LogoOverlay(PreparedLogoOverlay);

impl LogoOverlay {
    pub fn load(config: &LogoConfig, width: u32, height: u32) -> Result<Self> {
        let style = LogoStyle {
            scale: config.scale.clone(),
            opacity: config.opacity,
            position: config.position.clone(),
        };
        ensure!(
            (0.0..=1.0).contains(&config.opacity) && config.opacity.is_finite(),
            "logo opacity must be between 0.0 and 1.0"
        );
        let input = open_media_input(&config.path)
            .with_context(|| format!("failed to open logo {}", config.path))?;
        PreparedLogoOverlay::from_input(&style, input, &config.path, width, height).map(Self)
    }

    pub fn width(&self) -> u32 {
        self.0.width
    }

    pub fn height(&self) -> u32 {
        self.0.height
    }

    #[cfg(test)]
    pub(crate) fn position(&self) -> (u32, u32) {
        (self.0.x, self.0.y)
    }

    pub fn opacity(&self) -> u8 {
        self.0.opacity
    }

    pub(crate) fn positioned(
        &self,
        position: Option<&str>,
        width: u32,
        height: u32,
    ) -> Result<Self> {
        self.0.positioned(position, width, height).map(Self)
    }
}

pub fn blend_logo(target: &mut frame::Video, logo: &LogoOverlay, opacity: f64) {
    ff_graphics::ffmpeg::logo::blend_logo(target, &logo.0, opacity);
}
