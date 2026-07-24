use std::sync::{
    Arc, RwLock,
    atomic::{AtomicUsize, Ordering},
};

use ebur128_stream::{Analyzer, AnalyzerBuilder, Channel, Mode};
use ffmpeg_next::frame;

#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub struct LoudnessMetrics {
    pub momentary_lufs: Option<f64>,
    pub short_term_lufs: Option<f64>,
    pub integrated_lufs: Option<f64>,
    pub true_peak_dbtp: Option<f64>,
}

pub struct LoudnessAnalyzer {
    analyzer: Analyzer,
    metrics: LoudnessMetrics,
}

impl LoudnessAnalyzer {
    pub fn new(sample_rate: u32) -> Result<Self, ebur128_stream::Error> {
        Ok(Self {
            analyzer: AnalyzerBuilder::new()
                .sample_rate(sample_rate)
                .channels(&[Channel::Left, Channel::Right])
                .modes(Mode::Momentary | Mode::ShortTerm | Mode::Integrated | Mode::TruePeak)
                .build()?,
            metrics: LoudnessMetrics::default(),
        })
    }

    pub fn process_frame(&mut self, frame: &frame::Audio) -> LoudnessMetrics {
        if frame.planes() != 2 || frame.samples() == 0 {
            return self.metrics;
        }
        if self
            .analyzer
            .push_planar::<f32>(&[frame.plane::<f32>(0), frame.plane::<f32>(1)])
            .is_err()
        {
            return self.metrics;
        }
        let snapshot = self.analyzer.snapshot();
        self.metrics = LoudnessMetrics {
            momentary_lufs: snapshot.momentary_lufs(),
            short_term_lufs: snapshot.short_term_lufs(),
            integrated_lufs: snapshot.integrated_lufs(),
            true_peak_dbtp: snapshot.true_peak_dbtp(),
        };
        self.metrics
    }
}

#[derive(Debug, Clone, Default)]
pub struct LoudnessMeterControl {
    subscribers: Arc<AtomicUsize>,
    metrics: Arc<RwLock<Option<LoudnessMetrics>>>,
}

impl LoudnessMeterControl {
    pub fn subscribe(&self) {
        self.subscribers.fetch_add(1, Ordering::Relaxed);
    }
    pub fn unsubscribe(&self) {
        let _ = self
            .subscribers
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |n| n.checked_sub(1));
    }
    pub fn active(&self) -> bool {
        self.subscribers.load(Ordering::Relaxed) > 0
    }
    pub fn metrics(&self) -> Option<LoudnessMetrics> {
        self.active()
            .then(|| {
                *self
                    .metrics
                    .read()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
            })
            .flatten()
    }
    fn set_metrics(&self, metrics: LoudnessMetrics) {
        *self
            .metrics
            .write()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(metrics);
    }
}

pub(crate) struct LoudnessMeter {
    control: LoudnessMeterControl,
    analyzer: Option<LoudnessAnalyzer>,
    sample_rate: u32,
}
impl LoudnessMeter {
    pub(crate) fn new(sample_rate: u32, control: LoudnessMeterControl) -> Self {
        Self {
            control,
            analyzer: None,
            sample_rate,
        }
    }
    pub(crate) fn process_frame(&mut self, frame: &frame::Audio) {
        if !self.control.active() {
            self.analyzer = None;
            return;
        }
        if self.analyzer.is_none() {
            self.analyzer = LoudnessAnalyzer::new(self.sample_rate).ok();
        }
        if let Some(analyzer) = &mut self.analyzer {
            self.control.set_metrics(analyzer.process_frame(frame));
        }
    }
}
