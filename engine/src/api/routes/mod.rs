use std::path::PathBuf;

use chrono::{Datelike, NaiveDateTime, TimeZone, Utc};
use chrono_tz::Tz;
use serde::{Deserialize, Serialize};

use crate::utils::{config::Template, naive_date_time_from_str};

mod channel;
mod control;
mod file;
mod log;
mod playlist;
mod playout_advanced;
mod playout_config;
mod presets;
mod program;
mod public;
mod system;
mod user;

pub use channel::*;
pub use control::*;
pub use file::*;
pub use log::*;
pub use playlist::*;
pub use playout_advanced::*;
pub use playout_config::*;
pub use presets::*;
pub use program::*;
pub use public::*;
pub use system::*;
pub use user::*;

#[derive(Debug, Deserialize, Serialize)]
pub struct DateObj {
    #[serde(default)]
    date: String,
}

#[derive(Debug, Default, Deserialize, Serialize)]
pub struct PathsObj {
    #[serde(default)]
    paths: Option<Vec<String>>,
    template: Option<Template>,
}

#[derive(Debug, Deserialize, Serialize)]
struct FileObj {
    #[serde(default)]
    path: PathBuf,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct LogReq {
    #[serde(default)]
    date: String,
    #[serde(default)]
    timezone: Tz,
    #[serde(default)]
    download: bool,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct ImportObj {
    #[serde(default)]
    file: PathBuf,
    #[serde(default)]
    date: String,
}

#[derive(Debug, Deserialize, Clone)]
pub struct ProgramObj {
    #[serde(default = "time_after", deserialize_with = "naive_date_time_from_str")]
    start_after: NaiveDateTime,
    #[serde(default = "time_before", deserialize_with = "naive_date_time_from_str")]
    start_before: NaiveDateTime,
}

fn time_after() -> NaiveDateTime {
    let today = Utc::now();

    chrono::Local
        .with_ymd_and_hms(today.year(), today.month(), today.day(), 0, 0, 0)
        .unwrap()
        .naive_local()
}

fn time_before() -> NaiveDateTime {
    let today = Utc::now();

    chrono::Local
        .with_ymd_and_hms(today.year(), today.month(), today.day(), 23, 59, 59)
        .unwrap()
        .naive_local()
}

#[derive(Debug, Serialize)]
struct ProgramItem {
    source: String,
    start: String,
    title: Option<String>,
    r#in: f64,
    out: f64,
    duration: f64,
    category: String,
}
