# Media Catalog storage plugin

This is a deliberately small Rust WebAssembly component. It is read-only and
lists the permitted `tests/assets/storage` catalog at runtime, so it
demonstrates plugin discovery, manifest permissions, Wasmtime's Component Model
execution and URL playback without credentials or an external object store.

## Build

From the repository root:

```sh
rustup target add wasm32-wasip2
cargo build --manifest-path plugins/storage/media-catalog/Cargo.toml \
  --target wasm32-wasip2 --release
```

The generated component is already the path referenced by `plugin.toml`.
The catalog is read again whenever ffplayout asks the plugin to browse it, so
adding or removing test assets does not require rebuilding the component.
This uses Rust's native Component Model target and `wit-bindgen`; no
`cargo-component` installation is required.

## Run

Start the test-media server from the repository root:

```sh
python3 plugins/storage/media-catalog/scripts/range_server.py \
  --directory tests/assets/storage
```

Enable the plugin explicitly for a development run:

```sh
FFPLAYOUT_PLUGIN_DIR="$PWD/plugins" \
FFPLAYOUT_STORAGE_PLUGIN=media-catalog \
cargo run -p ffplayout -- --channel 1 -l 1270.0.0.1:8787
```

The included server supports HTTP byte ranges, which are required for reliable
MP4 playback and seeking. It is development-only; a production storage plugin
must return playback URLs from its own range-capable service. Accordingly, the
example advertises `supports_range_requests` when used with this server.
