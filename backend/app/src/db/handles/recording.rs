use sqlx::{
    Sqlite, SqliteConnection,
    sqlite::{SqlitePool, SqliteQueryResult},
};

use crate::{
    db::models::Recording,
    utils::{
        config::{Recording as RecordingConfig, RecordingSource},
        errors::ProcessError,
    },
};

pub async fn select_recording(
    pool: &SqlitePool,
    channel_id: i32,
) -> Result<Recording, ProcessError> {
    Ok(
        sqlx::query_as("SELECT * FROM recordings WHERE channel_id = $1")
            .bind(channel_id)
            .fetch_one(pool)
            .await?,
    )
}

pub async fn insert_recording<'e, E>(
    executor: E,
    channel_id: i32,
) -> Result<SqliteQueryResult, ProcessError>
where
    E: sqlx::Executor<'e, Database = Sqlite>,
{
    // Each channel gets its own recording directory by default so that
    // multiple channels never write their segments into the same folder.
    let default_path = format!("/var/lib/ffplayout/recordings/{channel_id}");
    Ok(
        sqlx::query("INSERT INTO recordings (channel_id, path) VALUES ($1, $2)")
            .bind(channel_id)
            .bind(default_path)
            .execute(executor)
            .await?,
    )
}

pub async fn update_recording(
    pool: &SqlitePool,
    channel_id: i32,
    recording: &RecordingConfig,
) -> Result<SqliteQueryResult, ProcessError> {
    let mut connection = pool.acquire().await?;
    update_recording_on(&mut connection, channel_id, recording).await
}

pub async fn update_recording_on(
    connection: &mut SqliteConnection,
    channel_id: i32,
    recording: &RecordingConfig,
) -> Result<SqliteQueryResult, ProcessError> {
    let source = match recording.source {
        RecordingSource::HlsVariant => "hls_variant",
        RecordingSource::Stream => "stream",
        RecordingSource::Encode => "encode",
    };
    let video_options = serde_json::to_string(&recording.video_options)?;
    Ok(sqlx::query("UPDATE recordings SET enabled = $2, source = $3, source_output_id = $4, hls_variant = $5, path = $6, segment_duration = $7, retention_days = $8, minimum_free_space_gb = $9, width = $10, height = $11, video_codec = $12, video_options = $13, audio_codec = $14, audio_bitrate = $15 WHERE channel_id = $1")
        .bind(channel_id).bind(recording.enable).bind(source).bind(recording.source_output_id).bind(&recording.variant).bind(&recording.path)
        .bind(i64::from(recording.segment_duration)).bind(i64::from(recording.retention_days)).bind(i64::from(recording.minimum_free_space_gb))
        .bind(i64::from(recording.width)).bind(i64::from(recording.height)).bind(&recording.video_codec).bind(video_options).bind(&recording.audio_codec).bind(i64::from(recording.audio_bitrate))
        .execute(connection).await?)
}
