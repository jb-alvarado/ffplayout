use sqlx::sqlite::{SqlitePool, SqliteQueryResult};

use crate::{
    db::models::Configuration,
    utils::{config::PlayoutConfig, errors::ProcessError},
};

pub async fn select_configuration(
    pool: &SqlitePool,
    channel: i32,
) -> Result<Configuration, ProcessError> {
    const QUERY: &str = "SELECT * FROM configurations WHERE channel_id = $1";

    let result = sqlx::query_as(QUERY).bind(channel).fetch_one(pool).await?;

    Ok(result)
}

pub async fn insert_configuration(
    pool: &SqlitePool,
    channel_id: i32,
    output_id: i32,
) -> Result<SqliteQueryResult, ProcessError> {
    const QUERY: &str = "INSERT INTO configurations (channel_id, output_id) VALUES($1, $2)";

    let result = sqlx::query(QUERY)
        .bind(channel_id)
        .bind(output_id)
        .execute(pool)
        .await?;

    Ok(result)
}

pub async fn update_configuration(
    pool: &SqlitePool,
    id: i32,
    config: PlayoutConfig,
) -> Result<SqliteQueryResult, ProcessError> {
    const QUERY: &str = "UPDATE configurations SET general_stop_threshold = $2, general_dev_metrics = $3, mail_subject = $4, mail_recipient = $5, mail_level = $6, mail_interval = $7, logging_ffmpeg_level = $8, logging_ingest_level = $9, logging_detect_silence = $10, logging_ignore = $11, processing_mode = $12, processing_audio_only = $13, processing_copy_audio = $14, processing_copy_video = $15, processing_width = $16, processing_height = $17, processing_aspect = $18, processing_fps = $19, processing_add_logo = $20, processing_logo = $21, processing_logo_scale = $22, processing_logo_opacity = $23, processing_logo_position = $24, processing_audio_tracks = $25, processing_audio_track_index = $26, processing_audio_channels = $27, processing_volume = $28, processing_filter = $29, processing_override_filter = $30, processing_vtt_enable = $31, processing_vtt_dummy = $32, ingest_enable = $33, ingest_param = $34, ingest_filter = $35, playlist_day_start = $36, playlist_length = $37, playlist_infinit = $38, storage_filler = $39, storage_extensions = $40, storage_shuffle = $41, text_add = $42, text_from_filename = $43, text_font = $44, text_style = $45, text_regex = $46, task_enable = $47, task_path = $48, output_id = $49 WHERE id = $1";

    let result = sqlx::query(QUERY)
        .bind(id)
        .bind(config.general.stop_threshold)
        .bind(config.general.dev_metrics)
        .bind(config.mail.subject)
        .bind(config.mail.recipient)
        .bind(config.mail.mail_level.as_str())
        .bind(config.mail.interval)
        .bind(config.logging.ffmpeg_level)
        .bind(config.logging.ingest_level)
        .bind(config.logging.detect_silence)
        .bind(config.logging.ignore_lines.join(";"))
        .bind(config.processing.mode.to_string())
        .bind(config.processing.audio_only)
        .bind(config.processing.copy_audio)
        .bind(config.processing.copy_video)
        .bind(config.processing.width)
        .bind(config.processing.height)
        .bind(config.processing.aspect)
        .bind(config.processing.fps)
        .bind(config.processing.add_logo)
        .bind(config.processing.logo)
        .bind(config.processing.logo_scale)
        .bind(config.processing.logo_opacity)
        .bind(config.processing.logo_position)
        .bind(config.processing.audio_tracks)
        .bind(config.processing.audio_track_index)
        .bind(config.processing.audio_channels)
        .bind(config.processing.volume)
        .bind(config.processing.custom_filter)
        .bind(config.processing.override_filter)
        .bind(config.processing.vtt_enable)
        .bind(config.processing.vtt_dummy)
        .bind(config.ingest.enable)
        .bind(config.ingest.input_param)
        .bind(config.ingest.custom_filter)
        .bind(config.playlist.day_start)
        .bind(config.playlist.length)
        .bind(config.playlist.infinit)
        .bind(config.storage.filler)
        .bind(config.storage.extensions.join(";"))
        .bind(config.storage.shuffle)
        .bind(config.text.add_text)
        .bind(config.text.text_from_filename)
        .bind(config.text.font)
        .bind(config.text.style)
        .bind(config.text.regex)
        .bind(config.task.enable)
        .bind(config.task.path.to_string_lossy().to_string())
        .bind(config.output.id)
        .execute(pool)
        .await?;

    Ok(result)
}
