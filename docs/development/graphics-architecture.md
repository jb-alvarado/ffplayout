# Graphics architecture

## Current boundary

`ff-graphics` is an internal workspace crate. The dependency direction is:

```text
ffplayout application → ff-engine → ff-graphics
```

The application continues to own API, persistence and playlist adapters. The
engine owns media selection, input/network policy, program timing and integration
with the video/output pipeline. Graphics code does not depend on the engine,
application, channels, output protocols, or encoders.

The initial extraction retained the current rendering and blending algorithms.
Follow-up fixes make native text alpha handling explicit and correct, while the
per-frame YUV blend kernel and production compositing policy remain unchanged.
The subsequent animated-logo milestone adds bounded image/video source playback.
It does not introduce a scene controller, a new command queue, a GPU path, Servo,
or linear-light production compositing. See [Animated logos](../animated_logos.md).

## Modules and contracts

| Module | Responsibility |
| --- | --- |
| `ff_graphics::bitmap` | Straight-alpha RGBA CPU bitmaps and source-over preparation of translucent text/background/subtitle layers |
| `ff_graphics::source` | Backend-independent frame intervals and exact rational loop timing |
| `ff_graphics::ffmpeg::source` | Incremental image/video decoder and reusable YUVA preparation on an owned worker |
| `ff_graphics::animation` | Backend-independent fade/scroll evaluation and named canvas/program-frame preparation context |
| `ff_graphics::style` | Plain logo/text layout, colors, styling and existing transition parameters; no FFmpeg, font-library or GPU handles |
| `ff_graphics::layout` | Backend-independent logo sizing and placement compatibility rules |
| `ff_graphics::text` | Native cosmic-text/Swash shaping and rasterization, cached fonts/glyphs, CPU RGBA bitmap results |
| `ff_graphics::blend` | Existing borrowed-plane scalar/SIMD alpha blend kernels, independent of FFmpeg |
| `ff_graphics::ffmpeg::overlay` | Existing YUVA420 overlays and direct blending into borrowed YUV420 video frames |
| `ff_graphics::ffmpeg::logo` | Decode/scale an already-open input, position/reuse logo frames and composite them |
| `ff_graphics::ffmpeg::text` | Prepare straight-alpha text bitmaps as YUVA420 and apply placement/opacity returned by the common playback evaluator |
| `ff_graphics::ffmpeg::reference_video_frame` | Refcount existing video storage when the pipeline needs another frame handle |

FFmpeg support is an explicit optional `ffmpeg` feature. The crate has no default
features. Native text and plane blending can be built/tested without FFmpeg;
`ff-engine` explicitly enables the FFmpeg backend. Backend frame handles are
confined to that module. Public native text calls use graphics-owned style types,
not cosmic-text types.

`StraightRgbaBitmap` owns tightly packed RGBA8 with unassociated alpha. RGB
remains in the encoded color domain; it does not imply linear-light compositing.
Source-over preparation normalizes RGB by resulting coverage, including when a
translucent text layer is placed over a translucent background. Desktop subtitle
preparation uses the same contract. Glyph coverage is explicitly multiplied by
the requested text-color alpha once because cosmic-text 0.19's Swash pixel path
ignores the base alpha.

The FFmpeg text adapter converts straight RGB to YUV and preserves per-pixel
coverage. Chroma colors are reduced using coverage-weighted RGB so transparent
neighbors cannot dilute edge colors. The video blend kernel applies coverage once.
The signal-domain YUV blend kernels are unchanged. Logo decoding now prepares
immutable timed surfaces rather than reopening and reading a first image per clip.

`TextOverlay::prepare(style, text, TextPreparation { ... })` receives named canvas
size, frame rate and `TextTiming` values. All timing values are canonical program
frame indices, not seconds or raw source PTS ticks. `TextPlayback::evaluate`
accepts separate program/scroll frame indices and returns position and opacity
without any FFmpeg dependency. Backends apply their own pixel alignment rules.

## Compatibility adapters in the engine

`crates/engine/src/graphics/` contains `logo_adapter.rs`, `text_adapter.rs`,
`source.rs` (owned preparation/cache lifecycle) and `program.rs` (selection before
output fan-out); rendering and blending live in `ff-graphics`.

- `LogoConfig` retains its source path and existing public fields. The adapter
  passes local file inputs and a `LogoStyle` to the graphics backend on its
  preparation worker. The programme graphics source survives clip/live boundaries.
  Selection happens after live timestamp mapping and audio lookahead; fades travel
  with queued video frames. The graphics crate never
  imports the engine's media/network helpers.
- `TextConfig` retains explicit text, filename selection and regex behavior. The
  engine resolves content and passes the resulting string with a `TextStyle`.
  Immutable text snapshots retain placement and opacity with each video frame
  through file/live queues and loudness lookahead. The final compositor blends
  video, logo, configured text, then runtime text; scaled text bitmaps are cached
  per composition resolution.
- Existing exported color/position/weight types are re-exported through the
  engine, preserving callers' import paths.
- Runtime text control, revision handling, live/file transitions and program
  frame indices remain integrated by the existing engine code. Both engine and
  graphics preparation entry points accept the named context.
- The logo adapter exposes only explicit size and opacity accessors; it
  does not dereference into the complete backend API.
- The desktop renderer consumes the same composited program frames as recording
  and encoded outputs. Window-only subtitles/help remain prepared bitmaps; their
  text weight inputs use the graphics-owned enum rather than a cosmic-text enum.

Fan-out retains refcounted video storage rather than copying pixels for each
consumer. A visible logo requires one writable image per composition resolution;
matching consumers share that image. Blending borrows that destination and the
existing prepared overlay pixels. No additional channels are introduced.
Logo repositioning retains refcounted pixel reuse. Configuration-to-style copying
occurs during preparation, not each blend call.

### Cached regions and adaptive CPU execution

Each prepared YUVA surface caches `OverlayBounds`, the nontransparent alpha
extent rounded outwards to whole 2×2 chroma blocks. Pixel storage and original
layout dimensions remain unchanged. Common clipping intersects that extent with
the visible frame region, so logo placement and rendition scaling remain stable.
The scan runs during preparation; replacing/modifying alpha requires new bounds.
Fully transparent or offscreen graphics do no blending work.

The default compositor uses serial SIMD below 3,500,000 visible luma pixels.
Larger regions may use a process-wide Rayon pool capped at four workers (also
limited by available parallelism). Large visible-opacity prepared resources warm
the executor outside frame blending. Small/transparent graphics do not initialize
it. A nonblocking permit prevents channels from queueing behind one another:
when another adaptive blend owns the pool, the caller runs serially. Pool setup
failure is logged once and falls back to serial execution. No pool is created
per channel or frame, and there are no new unsafe pixel views.

The threshold is a conservative heuristic derived from local ARM64 measurements,
not a hardware-independent CPU guarantee. At about 3.7M/8.3M pixels, four workers
reduced wall latency by about 3.2×/3.5× at roughly 18%/13% more CPU time. Smaller
regions had proportionally higher scheduling overhead. In the actual 4K scene,
cropping alone reduced both wall and CPU time by about 82%; the adaptive policy
therefore selects serial work for that scene's largest 1.37M-pixel layer.

Explicit `BlendExecution::Serial` and `Parallel` support controlled benchmarks;
both share clipping, opacity rounding and SIMD kernels with the default path.

Composition relative to output scaling remains unchanged. Existing tests cover
shared frames, multiple resolutions, concurrent composition and retained encoder
frames. New layer/layout scopes must preserve these boundaries explicitly.

### Desktop, recording and output fan-out

`ProgramVideo` retains the unmodified program frame and prepares a composited
image per required resolution. Desktop submits through the same `VideoCompositor`
as the parallel encoding groups. The window and its recording retain references
to the same prepared pixels; an encoder consuming that resolution reuses them as
well. A full desktop queue is retried before dispatching to other outputs, so
retrying cannot duplicate their frames. Logo fade opacity belongs to each video
frame, including frames waiting in the desktop queue.

The window no longer loads/converts/uploads a separate program logo or blends
it over the window. Resizing/letterboxing now scales the logo with its video.
Window controls, help and selectable desktop subtitles remain window-only.

HLS keeps its existing behavior: composite once at the largest rendition's
resolution, then scale that image for the smaller variants. Its variant encoder
path is unchanged. Independent outputs at different composition resolutions
still require their own layout; they start from the unmodified program image,
preventing previously composited logos from appearing a second time.

## Following stages

Before implementing the scene controller, define backend-independent scene
revisions, source/layer identifiers, layout contexts and canonical time semantics.
Do not store backend frame handles or glyph-cache types inside scene nodes.

A future general surface descriptor must separate pixel layout, color metadata,
alpha convention/premultiplication domain, geometry and storage ownership.
`StraightRgbaBitmap` is one concrete CPU result, not a requirement that every future
renderer expose readable CPU bytes. Renderer/compositor capabilities determine
explicit preparation/conversion paths.

GPU work can replace rasterization and final compositing behind backend adapters.
Do not impose a CPU readback step on every backend or claim zero-copy interop
before measuring the complete video path. Device synchronization and readiness
must have a defined late-frame policy.

A future Servo adapter must receive resolved content/data, layout and scene
revisions and deliver ready surfaces. Its timing capabilities, cancellation,
resource bounds and failure isolation need a separate proposal. Native PTS-based
animation guarantees do not automatically apply to HTML/CSS/JavaScript.

Animated logos belong to time-varying sources, independent of layer transforms
and the compositor. Decode them outside the video path with bounded look-ahead;
select frames by program time and retain the last ready frame when late. Static
logos need no continuously running decoder. See the staged plan in
[graphics TODO](../../.dev/graphics_todo.md).

## Validation

```sh
cargo test -p ff-graphics --all-features
cargo test -p ff-graphics --no-default-features
cargo test -p ff-engine --lib graphics::
cargo test -p ff-engine --lib output::composition
cargo clippy --all-features --all-targets -- --deny warnings
```

Existing SIMD/scalar, alpha and logo layout/cache tests moved with their code.
Additional tests cover translucent straight-alpha composition, coverage-weighted
chroma, actual 50% white text over black, frame-rate-independent animation,
separate fade/scroll clocks and finite/repeating scrolling.
Engine adapter tests check filename/regex selection and pixel-equivalent
rendering against the graphics backend with nondefault styles and program times.
The existing overlay benchmark now calls the graphics crate's actual exported
kernels/backend instead of including engine implementation files:

```sh
CARGO_PROFILE_BENCH_LTO=thin cargo bench -p ff-graphics --features ffmpeg --bench overlay_blend
CARGO_PROFILE_BENCH_LTO=thin cargo bench -p ff-graphics --features ffmpeg --bench yuv_execution
```

The experimental linear-light paths live in the graphics crate's example/benchmark
laboratory; their measurements do not select production rendering policy. Audio
and playout examples/benchmarks remain in `ff-engine`.
