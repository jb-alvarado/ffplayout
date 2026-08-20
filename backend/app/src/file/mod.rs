use std::{
    path::{Path, PathBuf},
    sync::{Arc, atomic::AtomicBool},
};

use async_trait::async_trait;
use axum::extract::Multipart;
use path_clean::PathClean;
use relative_path::RelativePath;
use serde::{Deserialize, Serialize};
use tokio::sync::Mutex;

pub mod local;
mod upload;
mod watcher;

use crate::utils::errors::ServiceError;
use crate::{player::utils::Media, utils::config::PlayoutConfig};
use local::LocalStorage;
pub(crate) use upload::MAX_UPLOAD_REQUEST_SIZE;
pub use upload::{UploadStatus, UploadStatusQuery};

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct PathObject {
    pub source: String,
    pub parent: Option<String>,
    pub parent_folders: Option<Vec<String>>,
    pub folders: Option<Vec<String>>,
    pub files: Option<Vec<StorageEntry>>,
    #[serde(default)]
    pub folders_only: bool,
    #[serde(default)]
    pub recursive: bool,
}

impl PathObject {
    fn new(source: String, parent: Option<String>) -> Self {
        Self {
            source,
            parent,
            parent_folders: Some(vec![]),
            folders: Some(vec![]),
            files: Some(vec![]),
            folders_only: false,
            recursive: false,
        }
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct MoveObject {
    pub source: String,
    pub target: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct StorageEntry {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub duration: Option<f64>,
}

/// A media source that can be handed to the playback engine.
///
/// Remote storage implementations can return a short-lived URL here, while
/// local and mounted filesystems return a path.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PlaybackSource {
    LocalPath(PathBuf),
    Url(String),
}

/// Features a storage backend can provide.
///
/// Consumers must use these flags to decide which operations to expose rather
/// than assuming that every backend behaves like a writable local filesystem.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct StorageCapabilities {
    pub browse: bool,
    pub write: bool,
    pub move_entry: bool,
    pub recursive_delete: bool,
    pub direct_playback_url: bool,
    pub supports_range_requests: bool,
    pub watch_changes: bool,
}

/// Storage operations used by the application.
///
/// Implementations may back these operations with a local filesystem, a remote
/// object store, or a future plugin. Paths passed to this interface are storage
/// relative and must be validated by the implementation; callers must not rely
/// on them being host filesystem paths.
#[async_trait]
pub trait Storage: Send + Sync + std::fmt::Debug {
    fn capabilities(&self) -> StorageCapabilities;
    async fn browser(&self, path_obj: &PathObject) -> Result<PathObject, ServiceError>;
    async fn mkdir(&self, path_obj: &PathObject) -> Result<(), ServiceError>;
    async fn rename(&self, move_object: &MoveObject) -> Result<MoveObject, ServiceError>;
    async fn remove(&self, source_path: &str, recursive: bool) -> Result<(), ServiceError>;
    async fn resolve_playback(&self, key: &str) -> Result<PlaybackSource, ServiceError>;
    async fn upload_status(
        &self,
        query: &UploadStatusQuery,
        user_id: i32,
    ) -> Result<UploadStatus, ServiceError>;
    async fn upload(&self, data: Multipart, path: &Path, user_id: i32) -> Result<(), ServiceError>;

    /// Keep the folder-mode source list up to date where the backend supports
    /// change notifications or polling.
    async fn watchman(
        &self,
        config: PlayoutConfig,
        is_alive: Arc<AtomicBool>,
        sources: Arc<Mutex<Vec<Media>>>,
    );
    async fn stop_watch(&self);
    async fn fill_filler_list(
        &self,
        config: &PlayoutConfig,
        fillers: Option<Arc<Mutex<Vec<Media>>>>,
    ) -> Vec<Media>;
    /// Update the media extensions configured for this channel.
    async fn set_extensions(&self, extensions: Vec<String>);

    /// Initialise the built-in assets when a backend needs them. Remote
    /// backends can intentionally implement this as a no-op.
    async fn copy_assets(&self) -> Result<(), ServiceError>;
}

pub async fn init_storage(
    root: PathBuf,
    extensions: Vec<String>,
) -> Result<Arc<dyn Storage>, ServiceError> {
    if let Ok(plugin_id) = std::env::var("FFPLAYOUT_STORAGE_PLUGIN") {
        return crate::plugins::storage::load(&plugin_id, root, extensions)
            .map(|storage| Arc::new(storage) as Arc<dyn Storage>);
    }
    Ok(Arc::new(LocalStorage::new(root, extensions).await?))
}

/// Normalize absolut path
///
/// This function takes care, that it is not possible to break out from root_path.
pub fn norm_abs_path(
    root_path: &Path,
    input_path: &str,
) -> Result<(PathBuf, String, String), ServiceError> {
    let path_relative = strip_parent_segments(
        RelativePath::new(&root_path.to_string_lossy())
            .normalize()
            .as_str(),
    );
    let path_suffix = root_path
        .file_name()
        .unwrap_or_default()
        .to_string_lossy()
        .to_string();
    let mut source_relative =
        strip_parent_segments(RelativePath::new(input_path).normalize().as_str());

    if input_path.starts_with(&*root_path.to_string_lossy())
        || source_relative.starts_with(&path_relative)
    {
        source_relative = source_relative
            .strip_prefix(&path_relative)
            .and_then(|s| s.strip_prefix('/'))
            .unwrap_or_default()
            .to_string();
    } else {
        source_relative = source_relative
            .strip_prefix(&path_suffix)
            .and_then(|s| s.strip_prefix('/'))
            .unwrap_or(&source_relative)
            .to_string();
    }

    let path = root_path.join(&source_relative);

    // Defensive containment check: the cleaned absolute path must never leave
    // the storage root, regardless of the normalization above.
    let cleaned = path.clean();
    let cleaned_root = root_path.clean();
    if !cleaned.starts_with(&cleaned_root) {
        return Err(ServiceError::Forbidden("Access denied".to_string()));
    }

    Ok((path, path_suffix, source_relative))
}

/// Removes every `../` traversal segment, repeating until the string is stable
/// so a single non-recursive pass cannot leave a reconstructed `../` behind.
fn strip_parent_segments(value: &str) -> String {
    let mut result = value.to_string();
    loop {
        let stripped = result.replace("../", "");
        if stripped == result {
            return stripped;
        }
        result = stripped;
    }
}
