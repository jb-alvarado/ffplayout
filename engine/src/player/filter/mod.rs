use std::{fmt, path::Path, sync::Arc};

use log::*;
use regex::Regex;
use shlex::split;
use tokio::sync::Mutex;

mod custom;
pub mod v_drawtext;

use crate::player::{
    controller::ProcessUnit::*,
    utils::{calc_aspect, custom_format, fps_calc, fraction, is_close, Media},
};
use crate::utils::{
    config::{OutputMode::*, PlayoutConfig},
    logging::Target,
    advanced_config::FilterValue,
};
use crate::vec_strings;

#[derive(Clone, Debug, Copy, Eq, PartialEq)]
pub enum FilterType {
    Audio,
    Video,
}

impl fmt::Display for FilterType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::Audio => write!(f, "a"),
            Self::Video => write!(f, "v"),
        }
    }
}

use FilterType::*;

const HW_FILTER_POSTFIX: &[&str; 6] = &["_cuda", "_npp", "_opencl", "_vaapi", "_vulkan", "_qsv"];

// NO_HW_TRANSFER_FILTERS contains filters that don't require hardware acceleration transfer
// These filters operate on metadata, timing, or perform stream operations that can be
// efficiently handled in software without the overhead of hardware memory transfers
const NO_HW_TRANSFER_FILTERS: &[&str] = &[
    // Metadata and timing filters
    "settb",        // Sets timebase for the video frames
    "setpts",       // Modifies presentation timestamps
    "metadata",     // Manipulates stream metadata
    "setdar",       // Sets display aspect ratio
    "setsar",       // Sets sample aspect ratio
    "setrange",     // Sets color range metadata
    
    // Stream operation filters
    "streamselect", // Selects specific streams from input
    "select",       // Selects frames based on conditions
    "trim",         // Cuts the input stream
    "segment",      // Splits the input stream into segments
    
    // Buffer and memory operation filters
    "loop",         // Loops the input stream
    "fifo",         // Temporally buffers frames
    "shuffleframes",// Reorders frames in the stream
    "reverse",      // Reverses the input stream
    
    // Timing and control filters
    "fps",          // Adjusts frames per second
    "sendcmd",      // Schedules commands at specific times
    "setparams",    // Sets filter parameters dynamically
    "framestep",    // Selects one frame every N frames
    "dejudder",     // Fixes judder issues in video
    "realtime",     // Synchronizes to real-time
    
    // Analysis and debugging filters
    "bench",        // Benchmarks filter performance
    "perms"         // Controls read/write permissions
];

#[derive(Debug, Clone)]
pub struct Filters {
    category: String,
    hw_context: bool,
    hw_context_cuda: bool,
    a_chain: Vec<String>,
    v_chain: Vec<String>,
    pub audio_chain: String,
    pub video_chain: String,
    pub output_chain: Vec<String>,
    pub audio_map: Vec<String>,
    pub video_map: Vec<String>,
    pub audio_out_link: Vec<String>,
    pub video_out_link: Vec<String>,
    pub output_map: Vec<String>,
    config: PlayoutConfig,
    audio_position: i32,
    video_position: i32,
    audio_last: i32,
    video_last: i32,
}

impl Filters {
    pub fn new(config: PlayoutConfig, audio_position: i32) -> Self {
        let hw = config
            .advanced
            .decoder
            .input_param
            .as_ref()
            .is_some_and(|i| i.contains("-hw"));

        let hw_cuda = config
            .advanced
            .decoder
            .input_param
            .as_ref()
            .is_some_and(|p| p.contains("cuda") || p.contains("cuvid"));

        Self {
            category: String::new(),
            hw_context: hw,
            hw_context_cuda: hw_cuda,
            a_chain: vec![],
            v_chain: vec![],
            audio_chain: String::new(),
            video_chain: String::new(),
            output_chain: vec![],
            audio_map: vec![],
            video_map: vec![],
            audio_out_link: vec![],
            video_out_link: vec![],
            output_map: vec![],
            config,
            audio_position,
            video_position: 0,
            audio_last: -1,
            video_last: -1,
        }
    }

    pub fn add(&mut self, filter: Option<&str>, track_nr: i32, filter_type: FilterType) {
        if let Some(filter) = filter {
            let (map, chain, position, last) = match filter_type {
                Audio => (
                    &mut self.audio_map,
                    &mut self.a_chain,
                    self.audio_position,
                    &mut self.audio_last,
                ),
                Video => (
                    &mut self.video_map,
                    &mut self.v_chain,
                    self.video_position,
                    &mut self.video_last,
                ),
            };
    
            if *last == track_nr {
                chain.push(filter.to_string());
            } else {
                // start new filter chain
                let (selector, sep) = if chain.is_empty() {
                    (String::new(), String::new())
                } else {
                    (format!("[{filter_type}out{last}]"), ";".to_string())
                };
                let mut chain_start = String::new();
    
                chain_start.push_str(&selector);
    
                if filter.starts_with("aevalsrc") || filter.starts_with("movie") {
                    if filter_type == Video && chain.is_empty() {
                        chain.push(format!("[{position}:{filter_type}:{track_nr}]null"));
                    }
                    chain_start.push_str(&sep);
                } else {
                    // build audio/video selector like [0:a:0]
                    chain_start.push_str(&format!("{sep}[{position}:{filter_type}:{track_nr}]"));
                }
    
                if self.hw_context && filter_type == Video {
                    let chain_str = chain.join(",");
                    let hw_dl = hw_download(self.hw_context_cuda, &chain_str, filter);
                    if !hw_dl.is_empty() {
                        chain_start.push_str(&hw_dl);
                        chain_start.push_str(",");
                    }
                }
    
                chain_start.push_str(filter);
    
                chain.push(chain_start);
    
                *last = track_nr;
                let m = format!("[{filter_type}out{track_nr}]");
                map.push(m.clone());
                self.output_map.append(&mut vec_strings!["-map", m]);
            }
        }
    }

    fn build(&mut self) {
        for (i, filter) in self.a_chain.iter().enumerate() {
            if i > 0 && i < self.a_chain.len() && !filter.starts_with("[") {
                self.audio_chain.push(',');
            }

            self.audio_chain.push_str(filter);

            if filter.ends_with(']') && i < self.a_chain.len() - 1 {
                self.audio_chain.push(';');
            }
        }

        for (i, filter) in self.v_chain.iter().enumerate() {
            if filter.starts_with("movie=") && !self.video_chain.ends_with(';') {
                if self.hw_context
                    && !last_is_hw(self.hw_context_cuda, &self.video_chain)
                    && !self.config.advanced.is_empty_filter()
                {
                    let hw_up = hw_upload_str(&self.config);

                    self.video_chain.push(',');
                    self.video_chain.push_str(&hw_up);
                }
                if !filter.ends_with(']') {
                    self.video_chain.push_str("[v];");
                }
            } else if filter.starts_with("overlay") && !self.video_chain.ends_with(';') {
                let hw_up = hw_upload(&self.config, self.hw_context_cuda, &self.video_chain, filter);

                if !hw_up.is_empty() {
                    self.video_chain.push(',');
                    self.video_chain.push_str(&hw_up);
                }
                if !filter.ends_with(']') {
                    self.video_chain.push_str("[l];[v][l]");
                }
            } else if i > 0 && i < self.v_chain.len() && !self.video_chain.ends_with(';') && !filter.starts_with("[") {
                self.video_chain.push(',');

                let hw_dl = hw_download(self.hw_context_cuda, &self.video_chain, filter);
                let hw_ul = hw_upload(&self.config, self.hw_context_cuda, &self.video_chain, filter);

                if (!hw_dl.is_empty() || !hw_ul.is_empty())
                    && !filter.starts_with("movie=")
                    && !filter.starts_with("overlay")
                {
                    self.video_chain.push_str(&hw_dl);
                    self.video_chain.push(',');
                    self.video_chain.push_str(&hw_ul);
                    self.video_chain.push(',');
                }
            }

            self.video_chain.push_str(filter);
            if filter.ends_with(']') && i < self.v_chain.len() - 1 {
                self.video_chain.push(';');
            }
        }
    }

    pub fn cmd(&mut self) -> Vec<String> {
        if !self.output_chain.is_empty() {
            return self.output_chain.clone();
        }
     
        let mut cmd = vec![];
     
        if self.audio_last >= 0 && !self.audio_chain.ends_with(']') {
            self.audio_chain.push_str(&format!("[aout{}]", self.audio_last));
        }
     
        if self.video_last >= 0 && !self.video_chain.is_empty() {
            if self.category == "advertisement" {
                if let Some(last_bracket_start) = self.video_chain.rfind('[') {
                    if let Some(last_bracket_end) = self.video_chain[last_bracket_start..].find(']') {
                        let final_bracket_end = last_bracket_start + last_bracket_end + 1;
                        if final_bracket_end == self.video_chain.len() {
                            self.video_chain = self.video_chain[..last_bracket_start].to_string();
                        }
                    }
                }
            }

            if !self.video_chain.ends_with(']') {
                if self.hw_context && !last_is_hw(self.hw_context_cuda, &self.video_chain) {
                    let hw_up = hw_upload_str(&self.config);
                    self.video_chain.push_str(",format=nv12,");
                    self.video_chain.push_str(&hw_up);
                }
                
                self.video_chain.push_str(&format!("[vout{}]", self.video_last));
            }
        }
     
        let mut f_chain = self.video_chain.clone();
     
        if !self.audio_chain.is_empty() {
            if !f_chain.ends_with(';') {
                f_chain.push(';');
            }
            f_chain.push_str(&self.audio_chain);
        }
     
        if !f_chain.is_empty() {
            cmd.push("-filter_complex".to_string());
            cmd.push(f_chain);
        }
     
        cmd
     }

    pub fn map(&mut self) -> Vec<String> {
        if !self.output_chain.is_empty() && self.config.processing.override_filter {
            return vec![];
        }

        let mut o_map = self.output_map.clone();

        if self.video_last == -1 && !self.config.processing.audio_only {
            let v_map = "0:v".to_string();

            if !o_map.contains(&v_map) {
                o_map.append(&mut vec_strings!["-map", v_map]);
            };
        }

        if self.audio_last == -1 {
            for i in 0..self.config.processing.audio_tracks {
                let a_map = format!("{}:a:{i}", self.audio_position);

                if !o_map.contains(&a_map) {
                    o_map.append(&mut vec_strings!["-map", a_map]);
                };
            }
        }

        o_map
    }
}

impl Default for Filters {
    fn default() -> Self {
        Self::new(PlayoutConfig::default(), 0)
    }
}

fn get_filter_name(filter: &str) -> &str {
    let trimmed = filter.trim();

    if trimmed.starts_with('[') && trimmed.ends_with(']') {
        let content = &trimmed[1..trimmed.len()-1];
        if !content.contains('=') && !content.contains('[') {
            return "";
        }
    }
    
    let mut content = trimmed;
    
    while let Some(right_bracket) = content.find(']') {
        let after_bracket = &content[right_bracket + 1..].trim();
        if after_bracket.is_empty() {
            break;
        }
        content = after_bracket;
        if !content.starts_with('[') {
            break;
        }
    }
    
    if let Some(left_bracket) = content.rfind('[') {
        content = &content[..left_bracket].trim();
    }
    
    if content.is_empty() {
        return "";
    }
    
    content
        .split([':', '=', '@'])
        .next()
        .map(|s| s.trim().split('\\').next().unwrap_or(s).trim())
        .unwrap_or("")
}

fn is_no_hw_transfer_filter(filter: &str) -> bool {
    let filter_name = get_filter_name(filter);
    NO_HW_TRANSFER_FILTERS.contains(&filter_name)
}

fn is_hw(filter: &str) -> bool {
    HW_FILTER_POSTFIX.iter().any(|p| filter.contains(p))
}

fn split_filter_string(s: &str) -> Vec<&str> {
    let mut result = Vec::new();
    let mut start = 0;
    let mut in_parentheses = false;
    let mut is_escaped = false;
    
    for (i, c) in s.char_indices() {
        match c {
            '\\' => {
                is_escaped = !is_escaped;
            },
            '(' => {
                if !is_escaped {
                    in_parentheses = true;
                }
                is_escaped = false;
            },
            ')' => {
                if !is_escaped {
                    in_parentheses = false;
                }
                is_escaped = false;
            },
            ',' | ';' => {
                if !is_escaped && !in_parentheses {
                    let part = &s[start..i];
                    if !part.trim().is_empty() {
                        result.push(part.trim());
                    }
                    if c == ';' {
                        result.push(";");
                    }
                    start = i + 1;
                }
                is_escaped = false;
            },
            _ => {
                is_escaped = false;
            }
        }
    }
    
    let final_part = &s[start..];
    if !final_part.trim().is_empty() {
        result.push(final_part.trim());
    }
    
    result
}

fn find_last_hw_filter(chain: &str) -> Option<(usize, &str)> {
    let parts: Vec<&str> = split_filter_string(chain);

    if parts.is_empty() {
        return None;
    }

    let last_filter = get_filter_name(parts.last().unwrap());
    if last_filter.contains("hwdownload") ||
       last_filter == "format" ||
       last_filter.contains("hwupload") {
        return Some((parts.len() - 1, ""));
    }

    for (idx, part) in parts.iter().enumerate().rev() {
        let filter_name = get_filter_name(part);

        if !is_no_hw_transfer_filter(filter_name) &&
           HW_FILTER_POSTFIX.iter().any(|p| filter_name.contains(p)) &&
           !filter_name.contains("hwdownload") &&
           !filter_name.contains("hwupload")
        {
            for check_part in &parts[idx+1..] {
                let check_name = get_filter_name(check_part);
                
                if check_name.contains("hwdownload") || 
                   check_name == "format" || 
                   check_name.contains("hwupload") ||
                   *check_part == ";" {
                    return Some((parts.iter().position(|&x| x == *check_part).unwrap(), ""));
                }
                
                if !is_hw(check_name) && 
                   !is_no_hw_transfer_filter(check_name) && 
                   !check_name.is_empty() {
                    return None;
                }
            }
            return Some((idx, *part));
        }
    }

    None
}

fn find_first_sw_filter(f: &str) -> Option<(usize, &str)> {
    let parts: Vec<&str> = split_filter_string(f);

    for (idx, part) in parts.iter().enumerate() {
        let filter_name = get_filter_name(part);
        
        if filter_name.contains("hwdownload") || 
           filter_name == "format" || 
           filter_name.contains("hwupload") ||
           *part == ";" {
            return Some((idx, ""));
        }

        if !is_no_hw_transfer_filter(filter_name) &&
           HW_FILTER_POSTFIX.iter().any(|p| filter_name.contains(p)) {
            return None;
        }

        if !is_no_hw_transfer_filter(filter_name) &&
           !HW_FILTER_POSTFIX.iter().any(|p| filter_name.contains(p)) &&
           !filter_name.contains("hwdownload") &&
           !filter_name.contains("hwupload") {
            return Some((idx, part));
        }
    }

    None
}

fn find_last_yuv420p_hw_filter(chain: &str) -> Option<(usize, &str)> {
    let parts: Vec<&str> = split_filter_string(chain);
 
    if parts.is_empty() {
        return None;
    }
 
    for (idx, part) in parts.iter().enumerate().rev() {
        let filter_name = get_filter_name(part);
 
        if filter_name.contains("hwdownload") || filter_name.contains("hwupload") {
            return Some((idx, ""));
        }
 
        let is_hw_filter = !is_no_hw_transfer_filter(filter_name) && 
                          HW_FILTER_POSTFIX.iter().any(|p| filter_name.contains(p));
 
        if is_hw_filter {
            let params_start = part.find(filter_name)
                .map(|i| i + filter_name.len())
                .unwrap_or(0);
            let params_str = &part[params_start..];
 
            let has_yuv420p = params_str
                .split(|c| c == ':' || c == '=')
                .skip(1)
                .collect::<Vec<_>>()
                .windows(2)
                .any(|w| w[0].trim() == "format" && w[1].trim() == "yuv420p");
 
            if has_yuv420p {
                let mut valid = true;
                for check_part in &parts[idx+1..] {
                    let check_name = get_filter_name(check_part);
                    
                    if check_name.contains("hwdownload") || 
                       check_name.contains("hwupload") {
                        return Some((
                            parts.iter().position(|&x| x == *check_part).unwrap(),
                            ""
                        ));
                    }
                    
                    if !is_hw(check_name) && 
                       !is_no_hw_transfer_filter(check_name) && 
                       check_name != "pad" &&
                       !check_name.is_empty() {
                        valid = false;
                        break;
                    }
                }
 
                if valid {
                    return Some((idx, *part));
                }
            } else {

                if !filter_name.contains("hwdownload") && !filter_name.contains("hwupload") {
                    let mut standard_valid = true;
                    
                    for check_part in &parts[idx+1..] {
                        let check_name = get_filter_name(check_part);
                        
                        if check_name.contains("hwdownload") || 
                           check_name == "format" || 
                           check_name.contains("hwupload") ||
                           *check_part == ";" {
                            return Some((
                                parts.iter().position(|&x| x == *check_part).unwrap(),
                                ""
                            ));
                        }
                        
                        if !is_hw(check_name) && 
                           !is_no_hw_transfer_filter(check_name) && 
                           !check_name.is_empty() {
                            standard_valid = false;
                            break;
                        }
                    }
                    
                    if standard_valid {
                        return Some((idx, *part));
                    }
                }
            }
        }
    }
    
    let last_filter = get_filter_name(parts.last().unwrap());
    if last_filter.contains("hwdownload") ||
       last_filter == "format" ||
       last_filter.contains("hwupload") {
        return Some((parts.len() - 1, ""));
    }
    
    None
}

fn should_use_nv12_format<'a>(last_yuv420p_hw: Option<(usize, &str)>, f: &'a str) -> Option<(usize, &'a str)> {
    if let Some((_, hw_filter)) = last_yuv420p_hw {
        if !hw_filter.is_empty() {
            let has_yuv420p = hw_filter.contains("format=yuv420p");
                
            if has_yuv420p {
                let f_parts: Vec<&str> = split_filter_string(f);
                let has_pad = f_parts.iter().any(|&x| get_filter_name(x) == "pad");
                
                if has_pad {
                    let pad_pos = f_parts.iter()
                        .position(|&x| get_filter_name(x) == "pad")
                        .unwrap_or(f_parts.len());
                    
                    for (idx, part) in f_parts[..pad_pos].iter().enumerate() {
                        let name = get_filter_name(part);
                        let is_sw = !NO_HW_TRANSFER_FILTERS.contains(&name) && name != "pad";
                        if is_sw {
                            return Some((idx, "nv12"));
                        }
                    }
                    None
                } else {
                    for (idx, part) in f_parts.iter().enumerate() {
                        let name = get_filter_name(part);
                        if !NO_HW_TRANSFER_FILTERS.contains(&name) {
                            return Some((idx, "nv12"));
                        }
                    }
                    None
                }
            } else {

                let parts: Vec<&str> = split_filter_string(f);
                
                for (idx, part) in parts.iter().enumerate() {
                    let filter_name = get_filter_name(part);
                    
                    if filter_name.contains("hwdownload") || 
                       filter_name == "format" || 
                       filter_name.contains("hwupload") ||
                       *part == ";" {
                        return Some((idx, ""));
                    }
                    
                    if !is_no_hw_transfer_filter(filter_name) &&
                       HW_FILTER_POSTFIX.iter().any(|p| filter_name.contains(p)) {
                        return None;
                    }
                    
                    if !is_no_hw_transfer_filter(filter_name) &&
                       !HW_FILTER_POSTFIX.iter().any(|p| filter_name.contains(p)) &&
                       !filter_name.contains("hwdownload") &&
                       !filter_name.contains("hwupload") {
                        return Some((idx, *part));
                    }
                }
                
                None
            }
        } else {
            None
        }
    } else {
        None
    }
}

fn last_is_hw(hw_context_cuda: bool, chain: &str) -> bool {
    if hw_context_cuda {
        if let Some((_, _last_yuv420p_hw)) = find_last_yuv420p_hw_filter(chain) {
            return true;
        }
    } else if let Some((_, _last_hw)) = find_last_hw_filter(chain) {
        return true;
    }
    false
}

fn hw_download(hw_context_cuda: bool, chain: &str, f: &str) -> String {
    let mut filter = String::new();
    let chain_trimmed = chain.trim();
    let f_trimmed = f.trim();
    
    if chain_trimmed.ends_with(']') || f_trimmed.starts_with('[') || f_trimmed.starts_with(";") {
        return filter;
    }

    if hw_context_cuda {
        if let Some((idx, last_yuv420p_hw)) = find_last_yuv420p_hw_filter(chain) {
            if let Some((_, first_sw)) = should_use_nv12_format(Some((idx, last_yuv420p_hw)), f) {
                if !last_yuv420p_hw.is_empty() {
                    if !first_sw.is_empty(){
                        filter = "hwdownload".to_string();
                        if first_sw == "nv12" {
                            filter.push_str(",format=nv12");
                        }
                    }
                }
            }
        }
    } else {
        if let Some((_, last_hw)) = find_last_hw_filter(chain) {
            if let Some((_, first_sw)) = find_first_sw_filter(f) {
                if !last_hw.is_empty() && !first_sw.is_empty(){
                    filter = "hwdownload,format=nv12".to_string();
                }
            }
        }
    }

    filter
}

fn hw_upload_str(config: &PlayoutConfig) -> String {
    if config
        .advanced
        .decoder
        .input_param
        .as_ref()
        .is_some_and(|p| p.contains("cuda") || p.contains("cuvid"))
    {
        return "hwupload_cuda".to_string();
    }

    "hwupload".to_string()
}

fn hw_upload(config: &PlayoutConfig, hw_context_cuda: bool, chain: &str, f: &str) -> String {
    let mut filter = String::new();

    if !last_is_hw(hw_context_cuda, chain) && is_hw(f) && !f.contains("hwdownload") && !f.contains("hwupload") {
        filter = hw_upload_str(config);
    }

    filter
}

fn deinterlace(config: &PlayoutConfig, chain: &mut Filters, field_order: &Option<String>) {
    if let Some(order) = field_order {
        if order != "progressive" {
            let deinterlace = match config.advanced.filter.deinterlace.clone() {
                FilterValue::Some(deinterlace) => Some(deinterlace.clone()),
                FilterValue::None => Some("yadif=0:-1:0".to_string()),
                FilterValue::Null => None,
            };

            chain.add(deinterlace.as_deref(), 0, Video);
        }
    }
}

fn pad(config: &PlayoutConfig, chain: &mut Filters, aspect: f64) {
    let pad = match &config.advanced.filter.pad_video {
        FilterValue::Some(pad_video) => {
            if pad_video.contains("{}:{}") {
                let pair_count = pad_video.matches("{}:{}").count();
                
                let mut values = Vec::with_capacity(pair_count * 2);
                for _ in 0..pair_count {
                    values.push(config.processing.width.to_string());
                    values.push(config.processing.height.to_string());
                }
                
                Some(custom_format(pad_video, &values))
            } else {
                let (numerator, denominator) = fraction(config.processing.aspect, 100);
                let placeholder_count = pad_video.matches("{}").count();

                let mut values = Vec::with_capacity(placeholder_count);
                for i in 0..placeholder_count {
                    if i % 2 == 0 {
                        values.push(numerator.to_string());
                    } else {
                        values.push(denominator.to_string());
                    }
                }
                Some(custom_format(pad_video, &values))
            }
        },
        FilterValue::None => {
            if !is_close(aspect, config.processing.aspect, 0.03) {
                let (numerator, denominator) = fraction(config.processing.aspect, 100);
                Some(format!("pad='ih*{numerator}/{denominator}:ih:(ow-iw)/2:(oh-ih)/2'"))
            } else {
                None
            }
        },
        FilterValue::Null => None,
    };

    chain.add(pad.as_deref(), 0, Video);
}

fn fps(config: &PlayoutConfig, chain: &mut Filters, fps: f64) {
    if fps != config.processing.fps {
        let fps_filter = match config.advanced.filter.fps.clone() {
            FilterValue::Some(fps) => Some(custom_format(&fps, &[&config.processing.fps.to_string()])),
            FilterValue::None => Some(format!("fps={}", config.processing.fps)),
            FilterValue::Null => None,
        };

        chain.add(fps_filter.as_deref(), 0, Video);
    }
}

fn scale(config: &PlayoutConfig, chain: &mut Filters, width: Option<i64>, height: Option<i64>) {
    match config.advanced.filter.scale.clone() {
        FilterValue::Some(scale) => {
            let formatted = custom_format(
                &scale,
                &[&config.processing.width.to_string(), &config.processing.height.to_string()],
            );
            chain.add(Some(&formatted), 0, Video);
            Some(())
        },
        FilterValue::None => {
            if width.is_some_and(|w| w != config.processing.width)
                || height.is_some_and(|w| w != config.processing.height)
            {
                let formatted = format!(
                    "scale={}:{}",
                    config.processing.width, config.processing.height
                );
                chain.add(Some(&formatted), 0, Video);
            }
            Some(())
        },
        FilterValue::Null => None,
    };
}

fn setdar(config: &PlayoutConfig, chain: &mut Filters, aspect: f64) {
    if !is_close(aspect, config.processing.aspect, 0.03) {
        let dar = match config.advanced.filter.set_dar.clone() {
            FilterValue::Some(set_dar) => Some(custom_format(&set_dar, &[&config.processing.aspect.to_string()])),
            FilterValue::None => Some(format!("setdar=dar={}", config.processing.aspect)),
            FilterValue::Null => None,
        };

        chain.add(dar.as_deref(), 0, Video);
    }
}

fn fade(
    config: &PlayoutConfig,
    chain: &mut Filters,
    node: &mut Media,
    nr: i32,
    filter_type: FilterType,
) {
    let mut t = "";
    let mut fade_audio = false;

    if filter_type == Audio {
        t = "a";

        if node.duration_audio > 0.0 && node.duration_audio != node.duration {
            fade_audio = true;
        }
    }

    if node.seek > 0.0 || node.unit == Ingest {
        let fade_in = if t == "a" {
            match &config.advanced.filter.afade_in {
                FilterValue::Some(fade) => Some(custom_format(fade, &[t])),
                FilterValue::None => Some(format!("{t}fade=in:st=0:d=0.5")),
                FilterValue::Null => None,
            }
        } else {
            match &config.advanced.filter.fade_in {
                FilterValue::Some(fade) => Some(custom_format(fade, &[t])),
                FilterValue::None => Some(format!("{t}fade=in:st=0:d=0.5")),
                FilterValue::Null => None,
            }
        };

        chain.add(fade_in.as_deref(), nr, filter_type);
    }

    if (node.out != node.duration && node.out - node.seek > 1.0) || fade_audio {
        let out_time = node.out - node.seek - 1.0;
        let fade_out = if t == "a" {
            match &config.advanced.filter.afade_out {
                FilterValue::Some(fade) => Some(custom_format(fade, &[&out_time.to_string()])),
                FilterValue::None => Some(format!("{t}fade=out:st={out_time}:d=1.0")),
                FilterValue::Null => None,
            }
        } else {
            match &config.advanced.filter.fade_out {
                FilterValue::Some(fade) => Some(custom_format(fade, &[&out_time.to_string()])),
                FilterValue::None => Some(format!("{t}fade=out:st={out_time}:d=1.0")),
                FilterValue::Null => None,
            }
        };

        chain.add(fade_out.as_deref(), nr, filter_type);
    }
}

fn overlay(config: &PlayoutConfig, chain: &mut Filters, node: &mut Media) {
    if config.processing.add_logo
        && Path::new(&config.processing.logo_path).is_file()
        && &node.category != "advertisement"
    {
        let logo_path = config
            .processing
            .logo_path
            .replace('\\', "/")
            .replace(':', "\\\\:");

        // Movie filter
        let movie = match &config.advanced.filter.logo {
            FilterValue::Some(logo) => {
                Some(custom_format(logo, &[logo_path.clone(), config.processing.logo_opacity.to_string()]))
            },
            FilterValue::None => Some(format!(
                "movie={logo_path}:loop=0,setpts=N/(FRAME_RATE*TB),format=rgba,colorchannelmixer=aa={}",
                config.processing.logo_opacity),
            ),
            FilterValue::Null => None,
        };

        chain.add(movie.as_deref(), 0, Video);

        if node.last_ad {
            let fade_in = match config.advanced.filter.overlay_logo_fade_in.clone() {
                FilterValue::Some(fade_in) => Some(fade_in.clone()),
                FilterValue::None => Some("fade=in:st=0:d=1.0:alpha=1".to_string()),
                FilterValue::Null => None,
            };

            chain.add(fade_in.as_deref(), 0, Video);
        }

        if node.next_ad {
            let length = node.out - node.seek - 1.0;

            let fade_out = match &config.advanced.filter.overlay_logo_fade_out {
                FilterValue::Some(fade_out) => Some(custom_format(fade_out, &[&length.to_string()])),
                FilterValue::None => Some(format!("fade=out:st={length}:d=1.0:alpha=1")),
                FilterValue::Null => None,
            };

            chain.add(fade_out.as_deref(), 0, Video);
        }

        if !config.processing.logo_scale.is_empty() {
            let scale = match &config.advanced.filter.overlay_logo_scale {
                FilterValue::Some(logo_scale) => Some(custom_format(logo_scale, &[&config.processing.logo_scale])),
                FilterValue::None => Some(format!("scale={}", config.processing.logo_scale)),
                FilterValue::Null => None,
            };

            chain.add(scale.as_deref(), 0, Video);
        }

        let overlay = match &config.advanced.filter.overlay_logo {
            FilterValue::Some(ov) => Some(custom_format(ov, &[&config.processing.logo_position])),
            FilterValue::None => Some(format!("overlay={}:shortest=1", config.processing.logo_position)),
            FilterValue::Null => None,
        };

        chain.add(overlay.as_deref(), 0, Video);
    }
}

fn extend_video(config: &PlayoutConfig, chain: &mut Filters, node: &mut Media) {
    if let Some(video_duration) = node
        .probe
        .as_ref()
        .and_then(|p| p.video.first())
        .and_then(|v| v.duration.as_ref())
    {
        if node.out - node.seek > video_duration - node.seek + 0.1 && node.duration >= node.out {
            let duration = (node.out - node.seek) - (video_duration - node.seek);

            let tpad = match config.advanced.filter.tpad.clone() {
                FilterValue::Some(pad) => Some(custom_format(&pad, &[&duration.to_string()])),
                FilterValue::None => Some(format!("tpad=stop_mode=add:stop_duration={duration}")),
                FilterValue::Null => None,
            };

            chain.add(tpad.as_deref(), 0, Video);
        }
    }
}

/// add drawtext filter for lower thirds messages
async fn add_text(
    config: &PlayoutConfig,
    chain: &mut Filters,
    node: &mut Media,
    filter_chain: &Option<Arc<Mutex<Vec<String>>>>,
) {
    if config.text.add_text
        && (config.text.text_from_filename || config.output.mode == HLS || node.unit == Encoder)
    {
        let filter = v_drawtext::filter_node(config, Some(node), filter_chain).await;
        
        if !filter.is_empty() {
            chain.add(Some(&filter), 0, Video);
        }
    }
}

fn add_audio(config: &PlayoutConfig, chain: &mut Filters, node: &Media, nr: i32) {
    let audio = match config.advanced.filter.aevalsrc.clone() {
        FilterValue::Some(aevalsrc) => Some(custom_format(&aevalsrc, &[node.out - node.seek])),
        FilterValue::None => Some(format!(
            "aevalsrc=0:channel_layout=stereo:duration={}:sample_rate=48000",
            node.out - node.seek
        )),
        FilterValue::Null => None,
    };

    chain.add(audio.as_deref(), nr, Audio);
}

fn extend_audio(config: &PlayoutConfig, chain: &mut Filters, node: &mut Media, nr: i32) {
    if !Path::new(&node.audio).is_file() {
        if let Some(audio_duration) = node
            .probe
            .as_ref()
            .and_then(|p| p.audio.first())
            .and_then(|a| a.duration)
        {
            if node.out - node.seek > audio_duration - node.seek + 0.1 && node.duration >= node.out
            {
                let apad = match config.advanced.filter.apad.clone() {
                    FilterValue::Some(apad) => Some(custom_format(&apad, &[node.out - node.seek])),
                    FilterValue::None => Some(format!("apad=whole_dur={}", node.out - node.seek)),
                    FilterValue::Null => None,
                };

                chain.add(apad.as_deref(), nr, Audio);
            }
        }
    }
}

fn audio_volume(config: &PlayoutConfig, chain: &mut Filters, nr: i32) {
    if config.processing.volume != 1.0 {
        let volume = match config.advanced.filter.volume.clone() {
            FilterValue::Some(volume) => Some(custom_format(&volume, &[config.processing.volume])),
            FilterValue::None => Some(format!("volume={}", config.processing.volume)),
            FilterValue::Null => None,
        };

        chain.add(volume.as_deref(), nr, Audio);
    }
}

pub fn split_filter(config: &PlayoutConfig, chain: &mut Filters, nr: i32, filter_type: FilterType) {
    let count = config.output.output_count;

    if count > 1 {
        let out_link = match filter_type {
            Audio => &mut chain.audio_out_link,
            Video => &mut chain.video_out_link,
        };

        for i in 0..count {
            let link = format!("[{filter_type}out_{nr}_{i}]");
            if !out_link.contains(&link) {
                out_link.push(link);
            }
        }

        let split = match config.advanced.filter.split.clone() {
            FilterValue::Some(split) => Some(custom_format(&split, &[count.to_string(), out_link.join("")])),
            FilterValue::None => Some(format!("split={count}{}", out_link.join(""))),
            FilterValue::Null => None,
        };

        chain.add(split.as_deref(), nr, filter_type);
    }
}

/// Process output filter chain and add new filters to existing ones.
fn process_output_filters(config: &PlayoutConfig, chain: &mut Filters, output_filter: &str) {
    let filter =
        if (config.text.add_text && !config.text.text_from_filename) || config.output.mode == HLS {
            let re_v = Regex::new(r"\[[0:]+[v^\[]+([:0]+)?\]").unwrap(); // match video filter input link
            let _re_a = Regex::new(r"\[[0:]+[a^\[]+([:0]+)?\]").unwrap(); // match audio filter input link
            let re_a_out = Regex::new(r"\[aout[0-9]+\];?").unwrap(); // match audio output link
            let mut o_filter = output_filter.to_string();

            if let Some(first) = chain.v_chain.first() {
                o_filter = re_v.replace(&o_filter, &format!("{},", first)).to_string();
            }

            if !chain.a_chain.is_empty() {
                let audio_split = chain
                    .a_chain
                    .iter()
                    .filter(|f| f.contains("0:a") || f.contains("1:a"))
                    .map(|p| re_a_out.replace(p, "").to_string())
                    .collect::<Vec<String>>();

                for i in 0..config.processing.audio_tracks {
                    o_filter = o_filter.replace(
                        &format!("[0:a:{i}]"),
                        &format!("{},", &audio_split[i as usize]),
                    );
                }
            }

            o_filter
        } else {
            output_filter.to_string()
        };

    chain.output_chain = vec_strings!["-filter_complex", filter];
}

fn custom(filter: &str, chain: &mut Filters, nr: i32, filter_type: FilterType) {
    if !filter.is_empty() {
        chain.add(Some(filter), nr, filter_type);
    }
}

pub async fn filter_chains(
    config: &PlayoutConfig,
    node: &mut Media,
    filter_chain: &Option<Arc<Mutex<Vec<String>>>>,
) -> Filters {
    let mut filters = Filters::new(config.clone(), 0);
    filters.category = node.category.clone();

    if config.processing.override_filter {
        //override hole filtering
        if node.unit == Ingest && !config.ingest.custom_filter.is_empty() {
            filters.output_chain = split(&config.ingest.custom_filter).unwrap_or_default();
        } else {
            filters.output_chain = split(&config.processing.custom_filter).unwrap_or_default();
        }

        return filters;
    }

    if node.source.contains("color=c=") {
        filters.audio_position = 1;
    }

    if node.unit == Encoder {
        if !config.processing.audio_only {
            add_text(config, &mut filters, node, filter_chain).await;
        }

        if let Some(f) = config.output.output_filter.clone() {
            process_output_filters(config, &mut filters, &f);
        } else if config.output.output_count > 1 && !config.processing.audio_only {
            split_filter(config, &mut filters, 0, Video);
        }

        filters.build();

        return filters;
    }

    if !config.processing.audio_only && !config.processing.copy_video {
        if let Some(probe) = node.probe.as_ref() {
            if Path::new(&node.audio).is_file() {
                filters.audio_position = 1;
            }

            if let Some(v_stream) = &probe.video.first() {
                let aspect = calc_aspect(config, &v_stream.aspect_ratio);
                let frame_per_sec = fps_calc(&v_stream.frame_rate, 1.0);

                deinterlace(config, &mut filters, &v_stream.field_order);
                pad(config, &mut filters, aspect);
                fps(config, &mut filters, frame_per_sec);
                scale(config, &mut filters, v_stream.width, v_stream.height);
                setdar(config, &mut filters, aspect);
            }

            extend_video(config, &mut filters, node);
        } else {
            fps(config, &mut filters, 0.0);
            scale(config, &mut filters, None, None);
        }

        add_text(config, &mut filters, node, filter_chain).await;
        fade(config, &mut filters, node, 0, Video);
        overlay(config, &mut filters, node);
    }

    let (proc_vf, proc_af) = if node.unit == Ingest {
        custom::filter_node(config.general.channel_id, &config.ingest.custom_filter)
    } else {
        custom::filter_node(config.general.channel_id, &config.processing.custom_filter)
    };

    let (list_vf, list_af) = custom::filter_node(config.general.channel_id, &node.custom_filter);

    if !config.processing.copy_video {
        custom(&proc_vf, &mut filters, 0, Video);
        custom(&list_vf, &mut filters, 0, Video);
    }

    let mut audio_indexes = vec![];

    if config.processing.audio_track_index == -1 {
        for i in 0..config.processing.audio_tracks {
            audio_indexes.push(i);
        }
    } else {
        audio_indexes.push(config.processing.audio_track_index);
    }

    if !config.processing.copy_audio {
        for i in audio_indexes {
            if node
                .probe
                .as_ref()
                .and_then(|p| p.audio.get(i as usize))
                .is_some()
                || Path::new(&node.audio).is_file()
            {
                extend_audio(config, &mut filters, node, i);
            } else if node.unit == Decoder && !node.source.contains("color=c=") {
                warn!(target: Target::file_mail(), channel = config.general.channel_id;
                    "Missing audio track (id {i}) from <b><magenta>{}</></b>",
                    node.source
                );

                add_audio(config, &mut filters, node, i);
            }

            // add at least anull filter, for correct filter construction,
            // is important for split filter in HLS mode
            filters.add(Some("anull"), i, Audio);

            fade(config, &mut filters, node, i, Audio);
            audio_volume(config, &mut filters, i);

            custom(&proc_af, &mut filters, i, Audio);
            custom(&list_af, &mut filters, i, Audio);
        }
    } else if config.processing.audio_track_index > -1 {
        error!(target: Target::file_mail(), channel = config.general.channel_id; "Setting 'audio_track_index' other than '-1' is not allowed in audio copy mode!");
    }

    if config.output.mode == HLS {
        if let Some(f) = config.output.output_filter.clone() {
            process_output_filters(config, &mut filters, &f);
        }
    }

    filters.build();

    filters
}
