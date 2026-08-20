use super::{Plugin, Runtime};
use crate::{
    file::{
        MoveObject, PathObject, PlaybackSource, Storage, StorageCapabilities, StorageEntry,
        UploadStatus, UploadStatusQuery,
    },
    player::utils::Media,
    utils::{config::PlayoutConfig, errors::ServiceError},
};
use async_trait::async_trait;
use axum::extract::Multipart;
use std::{
    fmt,
    path::{Path, PathBuf},
    sync::{Arc, atomic::AtomicBool},
};
use tokio::sync::Mutex;
use wasmtime::{
    Store,
    component::{HasSelf, Linker},
};
use wasmtime_wasi::{ResourceTable, WasiCtx, WasiCtxBuilder, WasiCtxView, WasiView};

mod bindings {
    wasmtime::component::bindgen!({
        path: "../../plugins/wit/ffplayout-storage",
        world: "storage-plugin",
    });
}

#[derive(Clone)]
pub struct WasmStorage {
    plugin: Plugin,
    runtime: Runtime,
    root: PathBuf,
    extensions: Arc<Mutex<Vec<String>>>,
}

impl fmt::Debug for WasmStorage {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("WasmStorage")
            .finish_non_exhaustive()
    }
}

struct HostState {
    filesystem_read_root: Option<PathBuf>,
    table: ResourceTable,
    wasi: WasiCtx,
}

impl WasiView for HostState {
    fn ctx(&mut self) -> WasiCtxView<'_> {
        WasiCtxView {
            ctx: &mut self.wasi,
            table: &mut self.table,
        }
    }
}

pub fn load(id: &str, root: PathBuf, extensions: Vec<String>) -> Result<WasmStorage, ServiceError> {
    let runtime = Runtime::new().map_err(ServiceError::BadRequest)?;
    let plugin = runtime.storage(id).map_err(ServiceError::BadRequest)?;
    Ok(WasmStorage {
        plugin,
        runtime,
        root,
        extensions: Arc::new(Mutex::new(extensions)),
    })
}

impl bindings::ffplayout::storage::filesystem_catalog::Host for HostState {
    fn browse(
        &mut self,
        prefix: String,
        folders_only: bool,
        extensions: Vec<String>,
    ) -> Result<bindings::ffplayout::storage::types::BrowserResult, String> {
        let root = self
            .filesystem_read_root
            .as_deref()
            .ok_or_else(|| "plugin has no filesystem read permission".to_string())?;
        catalog_browser(root, &prefix, folders_only, &extensions)
    }
}

impl bindings::ffplayout::storage::types::Host for HostState {}

fn catalog_browser(
    root: &Path,
    prefix: &str,
    folders_only: bool,
    extensions: &[String],
) -> Result<bindings::ffplayout::storage::types::BrowserResult, String> {
    let relative = prefix.trim_matches('/');
    if Path::new(relative)
        .components()
        .any(|component| matches!(component, std::path::Component::ParentDir))
    {
        return Err("storage path must not contain parent segments".into());
    }

    let path = std::fs::canonicalize(root.join(relative)).map_err(|error| error.to_string())?;
    if !path.starts_with(root) || !path.is_dir() {
        return Err("storage path is not a readable directory".into());
    }

    let source = path
        .strip_prefix(root)
        .map_err(|error| error.to_string())?
        .to_string_lossy()
        .replace('\\', "/");
    let parent_path = if source.is_empty() {
        root
    } else {
        path.parent().unwrap_or(root)
    };
    let mut parent_folders = vec![];
    if path != parent_path && !folders_only {
        parent_folders = catalog_directories(parent_path)?;
    }

    let mut folders = vec![];
    let mut files = vec![];
    for entry in std::fs::read_dir(&path).map_err(|error| error.to_string())? {
        let entry = entry.map_err(|error| error.to_string())?;
        let child = entry.path();
        let name = entry.file_name().to_string_lossy().to_string();
        if name.starts_with('.') {
            continue;
        }
        if child.is_dir() {
            folders.push(name);
        } else if !folders_only
            && child.is_file()
            && child.extension().is_some_and(|extension| {
                extensions.iter().any(|allowed| {
                    extension
                        .to_string_lossy()
                        .eq_ignore_ascii_case(allowed.as_str())
                })
            })
        {
            files.push(bindings::ffplayout::storage::types::Entry {
                name,
                duration: ff_engine::probe_media(child.to_string_lossy().as_ref())
                    .ok()
                    .and_then(|probe| probe.format.duration),
            });
        }
    }

    folders.sort();
    parent_folders.sort();
    files.sort_by(|left, right| left.name.cmp(&right.name));
    Ok(bindings::ffplayout::storage::types::BrowserResult {
        source,
        parent: root
            .file_name()
            .map(|name| name.to_string_lossy().to_string()),
        parent_folders,
        folders,
        files,
    })
}

fn catalog_directories(path: &Path) -> Result<Vec<String>, String> {
    let mut folders = vec![];
    for entry in std::fs::read_dir(path).map_err(|error| error.to_string())? {
        let entry = entry.map_err(|error| error.to_string())?;
        let name = entry.file_name().to_string_lossy().to_string();
        if entry.path().is_dir() && !name.starts_with('.') {
            folders.push(name);
        }
    }
    Ok(folders)
}

impl WasmStorage {
    fn storage_key(&self, input: &str) -> Result<String, ServiceError> {
        // The existing frontend identifies a file as
        // `<storage-root>/<relative-key>`. Plugins, on the other hand, receive
        // storage-relative keys by contract. Remove exactly that presentation
        // prefix before applying the same containment normalization as local
        // storage. This applies equally to root files and nested files.
        let without_leading_slashes = input.trim_start_matches('/');
        let normalized_input = std::iter::once(&self.root)
            .chain(self.plugin.filesystem_read_roots.iter())
            .filter_map(|root| root.file_name().and_then(|name| name.to_str()))
            .find_map(|name| {
                (without_leading_slashes == name).then_some("").or_else(|| {
                    without_leading_slashes
                        .strip_prefix(name)?
                        .strip_prefix('/')
                })
            })
            .unwrap_or(input);

        crate::file::norm_abs_path(&self.root, normalized_input).map(|(_, _, relative)| relative)
    }

    fn instance(&self) -> Result<(Store<HostState>, bindings::StoragePlugin), ServiceError> {
        let filesystem_read_root = self.plugin.filesystem_read_roots.first().cloned();
        let mut linker = Linker::new(&self.runtime.engine);
        wasmtime_wasi::p2::add_to_linker_sync(&mut linker)
            .map_err(|error| ServiceError::BadRequest(error.to_string()))?;
        bindings::StoragePlugin::add_to_linker::<_, HasSelf<_>>(&mut linker, |state| state)
            .map_err(|error| ServiceError::BadRequest(error.to_string()))?;
        let mut store = Store::new(
            &self.runtime.engine,
            HostState {
                filesystem_read_root,
                table: ResourceTable::new(),
                wasi: WasiCtxBuilder::new().build(),
            },
        );
        store
            .set_fuel(100_000)
            .map_err(|error| ServiceError::BadRequest(error.to_string()))?;
        let instance =
            bindings::StoragePlugin::instantiate(&mut store, &self.plugin.component, &linker)
                .map_err(|error| ServiceError::BadRequest(error.to_string()))?;
        Ok((store, instance))
    }
}

#[async_trait]
impl Storage for WasmStorage {
    fn capabilities(&self) -> StorageCapabilities {
        // Capabilities are an exported component call and may fail only for an
        // invalid plugin. Discovery has already validated the component, so a
        // failed call safely exposes no optional capability.
        let Ok((mut store, instance)) = self.instance() else {
            return StorageCapabilities::default();
        };
        let Ok(flags) = instance
            .ffplayout_storage_storage()
            .call_capabilities(&mut store)
        else {
            return StorageCapabilities::default();
        };
        StorageCapabilities {
            browse: flags.contains(bindings::ffplayout::storage::types::Capabilities::BROWSE),
            write: flags.contains(bindings::ffplayout::storage::types::Capabilities::WRITE),
            move_entry: flags
                .contains(bindings::ffplayout::storage::types::Capabilities::MOVE_ENTRY),
            recursive_delete: flags
                .contains(bindings::ffplayout::storage::types::Capabilities::RECURSIVE_DELETE),
            direct_playback_url: flags
                .contains(bindings::ffplayout::storage::types::Capabilities::DIRECT_PLAYBACK_URL),
            supports_range_requests: flags.contains(
                bindings::ffplayout::storage::types::Capabilities::SUPPORTS_RANGE_REQUESTS,
            ),
            watch_changes: flags
                .contains(bindings::ffplayout::storage::types::Capabilities::WATCH_CHANGES),
        }
    }

    async fn browser(&self, p: &PathObject) -> Result<PathObject, ServiceError> {
        let extensions = self.extensions.lock().await.clone();
        let source = self.storage_key(&p.source)?;
        let (mut store, instance) = self.instance()?;
        let result = instance
            .ffplayout_storage_storage()
            .call_browse(&mut store, &source, p.folders_only, &extensions)
            .map_err(|error| ServiceError::BadRequest(error.to_string()))?
            .map_err(|error| {
                ServiceError::BadRequest(format!("Storage browse failed: {error:?}"))
            })?;
        Ok(PathObject {
            source: result.source,
            parent: result.parent,
            parent_folders: Some(result.parent_folders),
            folders: Some(result.folders),
            files: Some(
                result
                    .files
                    .into_iter()
                    .map(|entry| StorageEntry {
                        name: entry.name,
                        duration: entry.duration,
                    })
                    .collect(),
            ),
            folders_only: p.folders_only,
            recursive: p.recursive,
        })
    }

    async fn mkdir(&self, _: &PathObject) -> Result<(), ServiceError> {
        Err(ServiceError::BadRequest(
            "Storage plugin is read-only".into(),
        ))
    }
    async fn rename(&self, _: &MoveObject) -> Result<MoveObject, ServiceError> {
        Err(ServiceError::BadRequest(
            "Storage plugin is read-only".into(),
        ))
    }
    async fn remove(&self, _: &str, _: bool) -> Result<(), ServiceError> {
        Err(ServiceError::BadRequest(
            "Storage plugin is read-only".into(),
        ))
    }
    async fn resolve_playback(&self, key: &str) -> Result<PlaybackSource, ServiceError> {
        let key = self.storage_key(key)?;
        let (mut store, instance) = self.instance()?;
        let source = instance
            .ffplayout_storage_storage()
            .call_playback(&mut store, &key)
            .map_err(|error| ServiceError::BadRequest(error.to_string()))?
            .map_err(|error| {
                ServiceError::BadRequest(format!("Storage playback failed: {error:?}"))
            })?;
        match source {
            bindings::ffplayout::storage::types::PlaybackSource::LocalPath(path) => {
                let path = std::fs::canonicalize(path)
                    .map_err(|error| ServiceError::BadRequest(error.to_string()))?;
                if self
                    .plugin
                    .filesystem_read_roots
                    .iter()
                    .any(|root| path.starts_with(root))
                {
                    Ok(PlaybackSource::LocalPath(path))
                } else {
                    Err(ServiceError::BadRequest(
                        "Plugin returned a local path outside its filesystem permission".into(),
                    ))
                }
            }
            bindings::ffplayout::storage::types::PlaybackSource::Url(url) => {
                Ok(PlaybackSource::Url(url))
            }
        }
    }
    async fn upload_status(
        &self,
        _: &UploadStatusQuery,
        _: i32,
    ) -> Result<UploadStatus, ServiceError> {
        Err(ServiceError::BadRequest(
            "Storage plugin is read-only".into(),
        ))
    }
    async fn upload(&self, _: Multipart, _: &Path, _: i32) -> Result<(), ServiceError> {
        Err(ServiceError::BadRequest(
            "Storage plugin is read-only".into(),
        ))
    }
    async fn watchman(&self, _: PlayoutConfig, _: Arc<AtomicBool>, _: Arc<Mutex<Vec<Media>>>) {}
    async fn stop_watch(&self) {}
    async fn fill_filler_list(
        &self,
        _: &PlayoutConfig,
        _: Option<Arc<Mutex<Vec<Media>>>>,
    ) -> Vec<Media> {
        vec![]
    }
    async fn set_extensions(&self, e: Vec<String>) {
        *self.extensions.lock().await = e;
    }
    async fn copy_assets(&self) -> Result<(), ServiceError> {
        Ok(())
    }
}
