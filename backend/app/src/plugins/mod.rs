//! Shared plugin discovery and Wasmtime runtime.
//!
//! Plugin kinds (storage, A/V filters, and others) share this module. A kind
//! specific adapter must validate and use only the exports it understands.
use std::{collections::HashSet, env, fs, path::PathBuf, sync::Arc};

use serde::Deserialize;
use wasmtime::component::Component;
use wasmtime::{Config, Engine};

pub mod storage;

pub const API_VERSION: i64 = 1;

#[derive(Clone, Debug, Deserialize)]
pub struct ManifestPlugin {
    pub id: String,
    pub kind: String,
    pub api_version: i64,
    pub module: String,
}

#[derive(Clone, Debug, Deserialize)]
pub struct Manifest {
    pub plugin: ManifestPlugin,
    pub permissions: Option<ManifestPermissions>,
}

#[derive(Clone, Debug, Deserialize)]
pub struct ManifestPermissions {
    pub filesystem: Option<FilesystemPermissions>,
}

#[derive(Clone, Debug, Deserialize)]
pub struct FilesystemPermissions {
    #[serde(default)]
    pub read: Vec<String>,
}

#[derive(Clone)]
pub struct Plugin {
    pub manifest: ManifestPlugin,
    pub component: Component,
    pub filesystem_read_roots: Vec<PathBuf>,
}

#[derive(Clone, Debug)]
pub struct Runtime {
    pub engine: Arc<Engine>,
}

impl Runtime {
    pub fn new() -> Result<Self, String> {
        let mut config = Config::new();
        config.consume_fuel(true);
        config.wasm_component_model(true);
        Ok(Self {
            engine: Arc::new(Engine::new(&config).map_err(|e| e.to_string())?),
        })
    }

    pub fn storage(&self, id: &str) -> Result<Plugin, String> {
        let mut found = None;
        for root in plugin_roots() {
            let path = root.join("storage").join(id);
            let manifest_path = path.join("plugin.toml");
            if !manifest_path.is_file() {
                continue;
            }
            if found.is_some() {
                return Err(format!(
                    "Storage plugin '{id}' occurs in more than one plugin root"
                ));
            }
            let manifest: Manifest = toml_edit::de::from_str(
                &fs::read_to_string(&manifest_path).map_err(|e| e.to_string())?,
            )
            .map_err(|e| format!("{}: {e}", manifest_path.display()))?;
            if manifest.plugin.id != id
                || manifest.plugin.kind != "storage"
                || manifest.plugin.api_version != API_VERSION
            {
                return Err(format!(
                    "Invalid storage plugin manifest: {}",
                    manifest_path.display()
                ));
            }
            let filesystem_read_roots = manifest
                .permissions
                .and_then(|p| p.filesystem)
                .map(|p| p.read)
                .unwrap_or_default()
                .into_iter()
                .map(|value| {
                    let root = PathBuf::from(value);
                    std::fs::canonicalize(if root.is_absolute() {
                        root
                    } else {
                        path.join(root)
                    })
                    .map_err(|error| format!("Invalid filesystem read permission: {error}"))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let module_path = path.join(&manifest.plugin.module);
            let component = Component::from_file(&self.engine, &module_path)
                .map_err(|e| format!("{}: {e}", module_path.display()))?;
            found = Some(Plugin {
                manifest: manifest.plugin,
                component,
                filesystem_read_roots,
            });
        }
        found.ok_or_else(|| format!("Storage plugin '{id}' was not found"))
    }
}

fn plugin_roots() -> Vec<PathBuf> {
    let mut roots: Vec<PathBuf> = env::var_os("FFPLAYOUT_PLUGIN_DIR")
        .map(|v| env::split_paths(&v).collect())
        .unwrap_or_default();
    if cfg!(debug_assertions) {
        roots.push(PathBuf::from("plugins"));
    }
    #[cfg(target_os = "linux")]
    roots.extend([
        PathBuf::from("/usr/share/ffplayout/plugins"),
        PathBuf::from("/var/lib/ffplayout/plugins"),
    ]);
    #[cfg(target_os = "macos")]
    roots.push(PathBuf::from(
        "/Library/Application Support/ffplayout/plugins",
    ));
    #[cfg(target_os = "windows")]
    if let Some(base) = env::var_os("PROGRAMDATA") {
        roots.push(PathBuf::from(base).join("ffplayout/plugins"));
    }
    let mut seen = HashSet::new();
    roots.retain(|root| {
        let key = fs::canonicalize(root).unwrap_or_else(|_| root.clone());
        seen.insert(key)
    });
    roots
}
