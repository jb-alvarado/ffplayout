// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.

export type General = { stop_threshold: number, };

export type Ingest = { enable: boolean, input_param: string, custom_filter: string, };

export type Logging = { ffmpeg_level: string, ingest_level: string, detect_silence: boolean, ignore_lines: Array<string>, };

export type Mail = { show: boolean, subject: string, recipient: string, mail_level: string, interval: bigint, };

export type Output = { id: number, mode: OutputMode, output_param: string, };

export type OutputMode = "desktop" | "hls" | "null" | "stream";

export type Playlist = { day_start: string, length: string, infinit: boolean, };

/**
 * Channel Config
 *
 * This we init ones, when ffplayout is starting and use them globally in the hole program.
 */
export type PlayoutConfig = { general: General, mail: Mail, logging: Logging, processing: Processing, ingest: Ingest, playlist: Playlist, storage: Storage, text: Text, task: Task, output: Output, };

export type ProcessMode = "folder" | "playlist";

export type Processing = { mode: ProcessMode, audio_only: boolean, copy_audio: boolean, copy_video: boolean, width: bigint, height: bigint, aspect: number, fps: number, add_logo: boolean, logo: string, logo_scale: string, logo_opacity: number, logo_position: string, audio_tracks: number, audio_track_index: number, audio_channels: number, volume: number, custom_filter: string, override_filter: boolean, vtt_enable: boolean, vtt_dummy: string | null, };

export type Storage = { filler: string, extensions: Array<string>, shuffle: boolean, shared_storage: boolean, };

export type Task = { enable: boolean, path: string, };

export type Text = { add_text: boolean, font: string, text_from_filename: boolean, style: string, regex: string, };
