//! Live-only loudness processing for decoded planar `f32` audio.
//!
//! The gain rider intentionally follows short-term loudness instead of the
//! programme-integrated value.  The latter is a reporting metric and must not
//! steer a live programme because it never forgets earlier material.

use ebur128_stream::{Analyzer, AnalyzerBuilder, Channel, Mode};
use ffmpeg_next::frame;
use std::sync::{Arc, RwLock};

const TARGET_LUFS: f64 = -23.0;
const DEAD_BAND_LU: f64 = 1.0;
const MAX_GAIN_DB: f64 = 8.0;
const MAX_ATTENUATION_DB: f64 = -12.0;
const SILENCE_GATE_LUFS: f64 = -60.0;
const GAIN_UP_DB_PER_SECOND: f64 = 0.5;
const GAIN_DOWN_DB_PER_SECOND: f64 = 2.0;
const TRUE_PEAK_CEILING_DBTP: f64 = -1.0;

/// Parameters for the live gain rider and the final safety limiter.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LiveLoudnessConfig {
    pub target_lufs: f64,
    pub dead_band_lu: f64,
    pub max_gain_db: f64,
    pub max_attenuation_db: f64,
    pub gain_up_db_per_second: f64,
    pub gain_down_db_per_second: f64,
    pub silence_gate_lufs: f64,
    pub true_peak_ceiling_dbtp: f64,
}

impl Default for LiveLoudnessConfig {
    fn default() -> Self {
        Self {
            target_lufs: TARGET_LUFS,
            dead_band_lu: DEAD_BAND_LU,
            max_gain_db: MAX_GAIN_DB,
            max_attenuation_db: MAX_ATTENUATION_DB,
            gain_up_db_per_second: GAIN_UP_DB_PER_SECOND,
            gain_down_db_per_second: GAIN_DOWN_DB_PER_SECOND,
            silence_gate_lufs: SILENCE_GATE_LUFS,
            true_peak_ceiling_dbtp: TRUE_PEAK_CEILING_DBTP,
        }
    }
}

/// Shared runtime settings for the live processor. Updates take effect on the
/// next audio frame and do not require recreating the playout.
#[derive(Debug, Clone)]
pub struct LiveLoudnessControl(Arc<RwLock<LiveLoudnessSettings>>);

#[derive(Debug, Clone, Copy)]
pub struct LiveLoudnessSettings {
    pub enabled: bool,
    pub config: LiveLoudnessConfig,
}

impl LiveLoudnessControl {
    pub fn new(enabled: bool, config: LiveLoudnessConfig) -> Self {
        Self(Arc::new(RwLock::new(LiveLoudnessSettings {
            enabled,
            config,
        })))
    }

    pub fn settings(&self) -> LiveLoudnessSettings {
        *self
            .0
            .read()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
    }

    pub fn update(&self, enabled: bool, config: LiveLoudnessConfig) {
        *self
            .0
            .write()
            .unwrap_or_else(std::sync::PoisonError::into_inner) =
            LiveLoudnessSettings { enabled, config };
    }
}

/// Metrics from the source signal and the gain stage, suitable for API/UI
/// publishing without exposing the analyzer itself.
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub struct LiveLoudnessMetrics {
    pub momentary_lufs: Option<f64>,
    pub short_term_lufs: Option<f64>,
    pub integrated_lufs: Option<f64>,
    pub true_peak_dbtp: Option<f64>,
    pub rider_gain_db: f64,
    pub limiter_gain_reduction_db: f64,
}

/// Stateful EBU R128 analyzer, slow gain rider and ceiling limiter.
///
/// `ebur128-stream` performs the 4x true-peak measurement. The limiter is a
/// deliberately conservative final sample-domain safety stage. A future
/// lookahead limiter can replace this stage at the live A/V delay boundary.
pub struct LiveLoudnessProcessor {
    analyzer: Analyzer,
    config: LiveLoudnessConfig,
    sample_rate: u32,
    rider_gain_db: f64,
    metrics: LiveLoudnessMetrics,
}

impl LiveLoudnessProcessor {
    pub fn new(
        sample_rate: u32,
        config: LiveLoudnessConfig,
    ) -> Result<Self, ebur128_stream::Error> {
        let analyzer = AnalyzerBuilder::new()
            .sample_rate(sample_rate)
            .channels(&[Channel::Left, Channel::Right])
            .modes(Mode::Momentary | Mode::ShortTerm | Mode::Integrated | Mode::TruePeak)
            .build()?;
        Ok(Self {
            analyzer,
            config,
            sample_rate,
            rider_gain_db: 0.0,
            metrics: LiveLoudnessMetrics::default(),
        })
    }

    pub fn metrics(&self) -> LiveLoudnessMetrics {
        self.metrics
    }

    pub fn config(&self) -> LiveLoudnessConfig {
        self.config
    }

    /// Processes one normalized stereo frame in place. Non-finite samples are
    /// sanitized before analysis so malformed live input cannot poison the
    /// analyzer or encoder.
    pub fn process(&mut self, frame: &mut frame::Audio) {
        if frame.planes() != 2 || frame.samples() == 0 {
            return;
        }
        for plane in 0..2 {
            for sample in frame.plane_mut::<f32>(plane) {
                if !sample.is_finite() {
                    *sample = 0.0;
                }
            }
        }

        let left = frame.plane::<f32>(0);
        let right = frame.plane::<f32>(1);
        if self.analyzer.push_planar::<f32>(&[left, right]).is_err() {
            return;
        }
        let snapshot = self.analyzer.snapshot();
        self.metrics.momentary_lufs = snapshot.momentary_lufs();
        self.metrics.short_term_lufs = snapshot.short_term_lufs();
        self.metrics.integrated_lufs = snapshot.integrated_lufs();
        self.metrics.true_peak_dbtp = snapshot.true_peak_dbtp();

        self.update_rider(frame.samples());
        self.apply_gain_and_ceiling(frame);
    }

    fn update_rider(&mut self, samples: usize) {
        let Some(short_term) = self.metrics.short_term_lufs else {
            return;
        };
        if short_term < self.config.silence_gate_lufs {
            return;
        }
        let error = self.config.target_lufs - short_term;
        let desired = if error.abs() <= self.config.dead_band_lu {
            0.0
        } else {
            error.clamp(self.config.max_attenuation_db, self.config.max_gain_db)
        };
        let seconds = samples as f64 / f64::from(self.sample_rate);
        let rate = if desired < self.rider_gain_db {
            self.config.gain_down_db_per_second
        } else {
            self.config.gain_up_db_per_second
        };
        let delta = (desired - self.rider_gain_db).clamp(-rate * seconds, rate * seconds);
        self.rider_gain_db += delta;
        self.metrics.rider_gain_db = self.rider_gain_db;
    }

    fn apply_gain_and_ceiling(&mut self, frame: &mut frame::Audio) {
        let gain = db_to_gain(self.rider_gain_db) as f32;
        let ceiling = db_to_gain(self.config.true_peak_ceiling_dbtp) as f32;
        let mut most_reduction = 0.0_f64;
        for plane in 0..2 {
            for sample in frame.plane_mut::<f32>(plane) {
                let scaled = *sample * gain;
                let limited = scaled.clamp(-ceiling, ceiling);
                if scaled != 0.0 && limited != scaled {
                    most_reduction =
                        most_reduction.max(-20.0 * f64::from((limited / scaled).abs()).log10());
                }
                *sample = limited;
            }
        }
        self.metrics.limiter_gain_reduction_db = most_reduction;
    }
}

fn db_to_gain(db: f64) -> f64 {
    10.0_f64.powf(db / 20.0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use ffmpeg_next::{
        format::sample::{Sample, Type},
        util::channel_layout::ChannelLayout,
    };

    #[test]
    fn ceiling_limiter_never_allows_a_sample_over_the_ceiling() {
        let mut processor =
            LiveLoudnessProcessor::new(48_000, LiveLoudnessConfig::default()).unwrap();
        let mut frame = frame::Audio::new(Sample::F32(Type::Planar), 48_000, ChannelLayout::STEREO);
        for plane in 0..2 {
            frame.plane_mut::<f32>(plane).fill(1.0);
        }
        processor.process(&mut frame);
        let ceiling = db_to_gain(-1.0) as f32;
        assert!(
            frame
                .plane::<f32>(0)
                .iter()
                .all(|sample| sample.abs() <= ceiling)
        );
        assert!(processor.metrics().limiter_gain_reduction_db > 0.0);
    }

    #[test]
    fn silence_is_not_amplified() {
        let mut processor =
            LiveLoudnessProcessor::new(48_000, LiveLoudnessConfig::default()).unwrap();
        let mut frame = frame::Audio::new(Sample::F32(Type::Planar), 48_000, ChannelLayout::STEREO);
        processor.process(&mut frame);
        assert_eq!(processor.metrics().rider_gain_db, 0.0);
    }
}
