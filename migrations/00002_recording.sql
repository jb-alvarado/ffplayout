CREATE TABLE IF NOT EXISTS recordings (
    id INTEGER PRIMARY KEY,
    channel_id INTEGER NOT NULL UNIQUE,
    enabled INTEGER NOT NULL DEFAULT 0,
    source TEXT NOT NULL DEFAULT 'stream',
    source_output_id INTEGER,
    hls_variant TEXT NOT NULL DEFAULT '',
    path TEXT NOT NULL DEFAULT '/var/lib/ffplayout/recordings',
    segment_duration INTEGER NOT NULL DEFAULT 300,
    retention_days INTEGER NOT NULL DEFAULT 62,
    minimum_free_space_gb INTEGER NOT NULL DEFAULT 0,
    width INTEGER NOT NULL DEFAULT 0,
    height INTEGER NOT NULL DEFAULT 0,
    video_codec TEXT NOT NULL DEFAULT 'libx264',
    video_options TEXT NOT NULL DEFAULT '{"preset":"faster","rate_control":"crf","quality":"23","maxrate":"2400"}',
    audio_codec TEXT NOT NULL DEFAULT 'aac',
    audio_bitrate INTEGER NOT NULL DEFAULT 128,
    FOREIGN KEY (channel_id) REFERENCES channels(id) ON UPDATE CASCADE ON DELETE CASCADE,
    FOREIGN KEY (source_output_id) REFERENCES outputs(id) ON UPDATE CASCADE ON DELETE SET NULL
);

INSERT OR IGNORE INTO recordings (channel_id, path) VALUES (1, '/var/lib/ffplayout/recordings/1');
