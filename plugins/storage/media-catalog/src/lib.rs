//! A small runtime-backed storage example for the ffplayout component ABI.
//!
//! The host grants the test-media directory through the manifest. Listing is
//! therefore performed at runtime, rather than being embedded during build.

mod bindings {
    wit_bindgen::generate!({
        path: "../../wit/ffplayout-storage",
        world: "storage-plugin",
    });
}

use bindings::{
    exports::ffplayout::storage::storage::{BrowserResult, Guest, PlaybackSource, StorageError},
    ffplayout::storage::{filesystem_catalog, types::Capabilities},
};

struct MediaCatalog;

impl Guest for MediaCatalog {
    fn capabilities() -> Capabilities {
        Capabilities::BROWSE
            | Capabilities::DIRECT_PLAYBACK_URL
            | Capabilities::SUPPORTS_RANGE_REQUESTS
    }

    fn browse(
        prefix: String,
        folders_only: bool,
        extensions: Vec<String>,
    ) -> Result<BrowserResult, StorageError> {
        filesystem_catalog::browse(&prefix, folders_only, &extensions).map_err(StorageError::Failed)
    }

    fn playback(key: String) -> Result<PlaybackSource, StorageError> {
        if key.is_empty() || key.split('/').any(|part| part == "..") {
            return Err(StorageError::NotFound);
        }

        Ok(PlaybackSource::Url(format!("http://127.0.0.1:8090/{key}")))
    }
}

bindings::export!(MediaCatalog with_types_in bindings);
