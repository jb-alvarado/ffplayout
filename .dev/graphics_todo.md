# Agent TODO: Native Graphics Engine and High-Quality Compositing for ffplayout

## Objective

Design and incrementally implement a native, CPU-first graphics subsystem for ffplayout that supports layered broadcast graphics, deterministic animations, reusable templates, and high-quality alpha compositing.

The existing YUV-based video pipeline must remain the primary processing path. Avoid unnecessary full-frame RGB conversions.

The architecture must support both an optimized YUV compositing path and an optional high-quality linear RGB compositing path. Linear-light blending is a quality policy, not an assumed CPU optimization. Renderer selection, compositing policy, and storage location are separate decisions.

**Do not introduce Chromium, CEF, Electron, Node.js, or another browser runtime in the current implementation.** Define an adapter contract for a potential future Servo integration, but do not implement or require Servo. GPU rendering and GPU compositing must also remain optional future backends.

## General Instructions

- Inspect the current repository before making architectural decisions.
- Identify the existing YUV/alpha compositing implementation, logo renderer, text overlay renderer, frame representation, and processing pipeline.
- Reuse existing functionality wherever possible.
- Preserve current APIs, configuration behavior, and output compatibility.
- Avoid additional frame copies, allocations, or color conversions in the hot path.
- Keep the graphics engine independent of encoders, output destinations, and playlist scheduling.
- Follow existing Rust conventions and project architecture.
- Implement the work incrementally, with tests and benchmarks.
- Do not perform a large rewrite unless clearly justified.
- Keep backend-specific FFmpeg, cosmic-text/Swash, and future GPU/Servo types out of the scene and control models.
- Keep API, persistence, and playlist adapters in their existing architectural layers; they translate into graphics commands rather than becoming engine dependencies.

---

## Implemented Groundwork — Workspace Crate Extraction

- [x] Introduce an internal `ff-graphics` workspace crate with one-way engine → graphics dependencies.
- [x] Move native text rendering and existing blend/layout code while preserving algorithms and public engine configuration behavior.
- [x] Keep native text bitmap/style APIs independent of FFmpeg and font-library types; isolate FFmpeg surfaces and compositing in an optional backend module.
- [x] Keep media path/regex selection, network-opening policy and pipeline/program-time integration in engine adapters.
- [x] Reuse refcounted logo pixels without additional per-frame channels, workers or frame copies.
- [x] Define straight-alpha CPU bitmap results, correct text/background/desktop alpha preparation, and apply requested glyph opacity once.
- [x] Extract existing text fade/scroll evaluation into a backend-independent component with explicit program-frame timing and named preparation context.
- [x] Keep logo backend access behind explicit engine-adapter methods rather than `Deref`.
- [x] Route desktop program graphics through the shared video compositor; reuse prepared pixels for desktop, recording and matching encoder resolutions. Preserve largest-rendition composition and subsequent downscaling for HLS variants.
- [x] Cache outward-aligned alpha bounds during preparation and skip transparent borders without changing original layout dimensions or copying frames.
- [x] Unify serial/parallel clipping and SIMD execution; use a shared, capped Rayon pool only for large visible regions and serial fallback when busy.
- [x] Measure both wall and aggregate CPU time, including a self-contained area sweep and byte-identical full-scene comparison. Keep small cropped overlays serial.
- [x] Update the overlay benchmark to use graphics-crate kernels/backend and document the extraction in `docs/development/graphics-architecture.md`.

This is the extraction step, not completion of the minimal scene engine. General
scene/layer state, surface descriptors/capability negotiation, bounded command
handoff and resource lifecycle extensions remain open below. Do not treat
GPU, Servo, templates or new APIs as already implemented. The animated-logo
source milestone below is implemented separately; general scene/layer state remains open.

---

## Phase 1 — Investigate the Existing Pipeline

**Priority: Critical**

- [ ] Locate all existing graphics-related code.
- [ ] Identify current logo and text-overlay rendering paths.
- [ ] Identify the exact pixel formats used during decoding, processing, compositing, and encoding.
- [ ] Determine whether the current YUV+alpha implementation uses straight or premultiplied alpha.
- [ ] Verify how alpha blending is calculated for Y, U, and V planes.
- [ ] Check how chroma subsampling and chroma location are handled.
- [ ] Identify whether current compositing operates directly on video frame planes or intermediate buffers.
- [ ] Review frame ownership, synchronization, memory reuse, and thread boundaries.
- [ ] Verify whether graphics are applied before output rendition scaling.
- [ ] Identify existing SIMD optimizations and available CPU features.
- [ ] Document bottlenecks and correctness concerns.

### Deliverable

Create `docs/development/graphics-architecture.md` documenting the current implementation, proposed architecture, integration points, and migration strategy.

Do not replace existing functionality during this phase.

### Existing Investigation and Benchmark Inputs

Reuse and review the work already present rather than starting from scratch:

- `crates/graphics/src/`: native text rendering, plain style types, shared plane kernels, and the optional FFmpeg CPU compositor backend.
- `crates/engine/src/graphics/`: compatibility adapters for media-derived text and source-opening policy.
- `crates/graphics/examples/frame_blend.rs` and its supporting modules: color models, independent references, linear-light SIMD and text-rendering experiments.
- `crates/graphics/benches/overlay_blend.rs`: single-plane microbenchmarks and complete-overlay comparisons, including direct SIMD calls and one-/four-worker pools.
- `docs/frame-blend.md`: assumptions, timing boundaries, measurements, and limitations.

These experiments are evidence and reusable test material, not production architecture. Existing measurements favor the current YUV compositor for speed. Do not infer aggregate CPU savings from lower wall time with multiple workers; caller/worker scheduling and CPU core placement also affect comparisons.

---

## Phase 2 — Define the Graphics Architecture

**Priority: Critical**

Introduce a graphics subsystem with clearly separated responsibilities.

Suggested responsibilities (not mandatory public types or separate crates):

```text
API / configuration / playlist adapters
                 ↓ commands
Graphics state + resource preparation
                 ↓ immutable scene revision
Canonical program time → scene evaluation + layout context
                 ↓ evaluated layers / render requests
Native renderer / future Servo adapter
                 ↓ prepared surface references
CPU compositor / future GPU compositor
                 ↓ program frame
```

- The scene contains backend-independent content descriptions, resource identifiers, transforms, stacking order, and animation definitions.
- Scene evaluation determines presentation properties for a program timestamp; it does not perform file I/O or rasterization.
- Renderers prepare graphics surfaces; compositors combine ready surfaces with the video frame.
- The resource manager owns caches, preparation jobs, budgets, cancellation, and retirement of unused surfaces.
- Backend-specific adapters own frame/texture handles and synchronization. Never require a scene node to contain an FFmpeg frame, glyph cache, or GPU texture.

### Tasks

- [ ] Define graphics layer identifiers and deterministic z-ordering.
- [ ] Support layer visibility, position, opacity, and transforms.
- [ ] Separate graphics state management from rendering.
- [ ] Separate graphics rendering from final video compositing.
- [ ] Introduce a bounded, nonblocking command interface with explicit acknowledgements/rejections and overload behavior.
- [ ] Define which updates may be coalesced and preserve ordering of activation, stop, and clear commands.
- [ ] Publish immutable scene revisions at a defined frame boundary.
- [ ] Prepare resources before activation; distinguish accepted/loading/ready/active/failed states.
- [ ] Tag preparation/render jobs with resource and scene revisions so stale completions cannot reactivate replaced or cleared graphics.
- [ ] Define cancellation and explicit shutdown/join behavior for preparation workers.
- [ ] Ensure the video processing thread never waits for HTTP, database access, template parsing, or file I/O.
- [ ] Support cached static graphics and animated graphics.
- [ ] Define ownership and lifetime of graphical resources.
- [ ] Ensure errors in individual graphics do not terminate playout.
- [ ] Define per-layer behavior for late or failed rendering: retain the last valid surface or hide the layer according to an explicit policy.
- [ ] Never wait for an unfinished render job or backend synchronization on the video thread; consume ready results and apply the late-frame policy.
- [ ] Define bounded resource handoff and retirement without an unbounded queue of old surfaces.

Suggested commands:

```rust
enum GraphicsCommand {
    Load { /* ... */ },
    Play { /* ... */ },
    Update { /* ... */ },
    Stop { /* ... */ },
    Clear { /* ... */ },
    ClearAll,
}
```

Preserve compatibility with existing logo and text APIs by mapping them to the new internal model.

### Layout and Rendition Contract

- [ ] Define canvas/reference resolution, coordinate units, anchors, clipping, and fit/aspect rules.
- [ ] Distinguish reference-canvas pixels, normalized coordinates, and explicit destination-pixel sizing; do not mix these implicitly.
- [ ] Default shared program graphics to composition before rendition scaling so HLS variants inherit the same proportional appearance.
- [ ] Treat intentionally destination-specific graphics as a separate composition scope with a concrete layout context.
- [ ] Preserve existing logo/text sizing and placement semantics through compatibility adapters; make any behavior change explicit and tested.
- [ ] Keep layout contexts independent of encoders and output protocols. Pipeline integration selects the scope and supplies its dimensions.
- [ ] Test mixed resolutions/aspect ratios and verify that graphics are neither duplicated nor unexpectedly resized across rendition branches.

---

## Phase 3 — Pixel Formats and Color Management

**Priority: Critical**

Do not force all graphics into RGBA8 or YUVA420.

Create an abstraction that supports different graphics surfaces and compositing formats.

### Tasks

- [ ] Preserve YUV as the primary program video format.
- [ ] Evaluate YUVA420 and YUVA444 as graphics representations.
- [ ] Support graphics with full-resolution alpha.
- [ ] Avoid irreversible chroma subsampling during initial graphics rendering whenever practical.
- [ ] Define explicit pixel-format and plane-layout metadata.
- [ ] Support correct chroma location and subsampling behavior.
- [ ] Preserve color primaries, transfer characteristics, matrix coefficients, and signal range.
- [ ] Design for future 10-bit and HDR support without requiring immediate implementation.
- [ ] Establish explicit handling of straight versus premultiplied alpha.
- [ ] Ensure alpha is not incorrectly applied to offset-encoded YUV components.
- [ ] Describe surfaces with independent format, color, alpha, geometry, and storage metadata.
- [ ] Specify the domain of premultiplication (linear or encoded RGB) explicitly; pixel format alone does not determine alpha semantics.
- [ ] Include full-resolution alpha, visible bounds, plane dimensions/strides, and chroma siting where applicable.
- [ ] Implement owned/shared CPU surfaces first, without making CPU byte access a requirement of every surface.
- [ ] Leave storage extensible to opaque backend resource references with explicit lifetime and readiness contracts. Do not implement placeholder GPU resource types yet.

Suggested abstraction:

```rust
enum GraphicsPixelFormat {
    Yuva420,
    Yuva444,
    Rgba8,
    Rgba16F,
}
```

Treat these as candidate formats, not mandatory implementations. Adapt them to the actual frame types already used by ffplayout. This enum describes pixel layout only; storage residency, color information, and alpha conventions are separate descriptors.

### Backend Capabilities and Conversion Policy

- [ ] Define renderer output capabilities and compositor input capabilities without requiring every backend to implement every format.
- [ ] Select explicit, cached preparation/conversion paths outside the video hot path where possible; account separately for unavoidable per-frame conversions.
- [ ] Reject unsupported combinations or use a documented fallback; do not silently change color or alpha semantics.
- [ ] Keep fast signal-domain YUV and linear-light RGB as explicit compositing policies with independently measured costs.
- [ ] Allow a future GPU backend to own final compositing as well as graphic rasterization.
- [ ] Do not prescribe mandatory CPU readback, GPU upload, or full-frame RGB conversion in the backend-neutral contract. Actual interop and transfer costs must be evaluated before implementing a GPU path.

### Acceptance Criteria

- No mandatory RGB conversion of the full program frame.
- No avoidable quality degradation during graphics preparation.
- Pixel-format and color metadata are explicit.
- Future renderers can provide graphics without changing the engine's control API.

---

## Phase 4 — Correct Alpha Compositing

**Priority: Critical**

Implement and validate a mathematically consistent alpha-compositing model.

### Fast Path — YUV Compositor

Continue using the existing YUV compositing approach where appropriate.

- [ ] Verify correct source-over alpha blending.
- [ ] Support partial transparency.
- [ ] Handle fully transparent and fully opaque pixels efficiently.
- [ ] Correctly account for limited-range versus full-range video.
- [ ] Handle chroma subsampling without introducing unnecessary color bleeding.
- [ ] Correctly handle odd coordinates and nonaligned graphic regions.
- [ ] Avoid blindly applying full-resolution alpha to subsampled U/V planes.
- [ ] Preserve existing performance optimizations.

Pay special attention to chroma blending at partially transparent edges.

### Correctness Reference — Signal-Domain YUV

- [ ] Reuse or extend an independent high-precision reference with the same signal-domain blending, color assumptions, and chroma reduction as the fast path.
- [ ] Compare optimized output against that reference with explicit quantization tolerances.
- [ ] Test transparent-pixel invariance, opaque endpoints, coverage-weighted chroma edges, and overlapping layers.

### Quality Reference — Linear RGB Compositor

Create an independent reference implementation for validating compositing quality.

- [ ] Convert test inputs into a defined linear-light RGB working space.
- [ ] Support mathematically correct source-over operations with premultiplied alpha.
- [ ] Convert the resulting image back to the required YUV format.
- [ ] Use sufficient numerical precision for reference calculations.
- [ ] Compare YUV compositing with this reference for documented visual differences; do not require pixel equality between different compositing policies.
- [ ] Document intentional differences between signal-domain and linear-light blending.

The reference implementation does not need to run in the production video hot path initially.

### Acceptance Criteria

- No unexpected halos around transparent edges.
- No incorrect darkening from mismatched alpha conventions.
- No obvious chroma artifacts caused by incorrect alpha sampling.
- Correct behavior when multiple semitransparent layers overlap.
- Documented and reproducible differences between fast and reference compositing modes.

---

## Phase 5 — Native Graphics Rendering

**Priority: High**

Implement native rendering without browser dependencies.

Initially support:

- [ ] PNG graphics with transparency.
- [ ] SVG graphics.
- [ ] Text graphics.
- [ ] Basic shapes and colored rectangles.
- [ ] Layer opacity.
- [ ] Positioning and scaling.

Evaluate lightweight Rust rendering libraries such as `tiny-skia`, `resvg`, and the existing ffplayout text-rendering implementation.

Do not replace existing dependencies without benchmarking or establishing a concrete functional need.

### Animated Logos and Time-Varying Graphics Sources

Animated logos are an explicit native graphics requirement. Keep the existing
logo configuration/control path as the compatible user-facing entry point, but
implement animated content as a reusable graphics source within a layer, not as
animation-specific logic inside the logo compositor.

```text
Logo configuration adapter → Graphics layer
                              ├─ static or animated graphics source
                              └─ layer transforms / opacity / transitions
                                           ↓ ready surface
                                      Compositor
```

- [x] Separate source-content animation (changing image frames) from layer animation (position, scale, opacity, and transitions). Both must be usable together.
- [ ] Define a reusable source interface that supplies prepared surfaces for a canonical program timestamp. Keep decoder, container, and pixel-storage details behind source adapters.
- [ ] Evaluate initial formats such as image sequences and alpha-capable animated images/video; document supported formats and their alpha/color behavior before implementation. Do not promise all formats initially.
- [x] Preserve source frame durations, including variable durations, and map them to program presentation time rather than advancing one animation frame per video frame.
- [ ] Define source start time, repeat behavior, finite-loop completion (hold final frame or hide), explicit restart, and stop behavior.
- [x] Define behavior across pause, seek, clip boundaries, live takeovers, and timestamp resets using the canonical timeline. Decoder completion time must not become the animation clock.
- [x] Decode and prepare animated content outside the video processing thread with bounded look-ahead in time, frame count, and decoded bytes.
- [x] Avoid fully preloading arbitrarily long animations; evaluate bounded reuse for short loops separately.
- [x] When a frame is late, retain the last valid surface without blocking playout; before the first valid surface, keep the layer hidden. Select the frame appropriate to current program time when rendering catches up rather than replaying an obsolete backlog.
- [x] Distinguish delayed decoding from malformed/unsupported assets; apply the layer failure policy and expose useful status without repeated per-frame error logs.
- [x] Cancel obsolete jobs and release decoded buffers/decoder resources on replacement, stop, clear, and shutdown. Guard results with source revisions.
- [x] Static logos must not require a continuously running decoder or an additional worker. Share bounded preparation resources where practical rather than creating a thread per layer.
- [x] Keep the compositor unaware of whether a surface came from a static logo, animated source, native renderer, or future backend.

Implemented initial scope: existing logo configuration maps to a persistent
program source with local PNG/GIF/video decoding, exact rational loop periods,
64 MiB/256-frame/5-second caching, bounded streaming lookahead, immutable per-frame
selection before fan-out, error containment and cancellation/join. See
`docs/animated_logos.md` for supported test formats and limitations. The current
single-logo implementation uses one owned worker per channel source. The
backend-independent timing model is implemented, while a general surface/source
contract, shared multi-layer scheduler, finite repeats and graphics commands
remain open. Static assets use a one-time preparation worker that exits after
caching; they retain no decoder or running worker during playback.

This source abstraction should also support future animated lower thirds and
other graphics. It must not depend on Servo or GPU rendering, and it must not
force future sources to return CPU-readable byte buffers.

### Rendering and Caching

- [ ] Rasterize static graphics only when their content changes.
- [ ] Cache parsed SVGs and decoded images.
- [ ] Cache text layouts and glyphs where applicable.
- [ ] Reuse graphics surfaces across frames.
- [ ] Avoid full-scene rerendering when only one layer changes.
- [ ] Minimize unnecessary buffer clears and memory copies.
- [ ] Support dirty-region tracking where beneficial.
- [ ] Distinguish rasterization invalidation from video compositing: a cached static surface must still be composited onto every fresh video frame.
- [ ] Include content revision, layout size, font/style dependencies, color/alpha representation, and backend/device identity where relevant in cache keys.
- [ ] Set per-channel/shared cache budgets and limits for asset dimensions, layer counts, and outstanding jobs.
- [ ] Retire unused resources and prevent in-flight jobs from keeping obsolete scenes alive indefinitely.
- [ ] Verify that eviction, rapid replacement, and shutdown release CPU resources and can support future backend-specific retirement rules.

---

## Phase 6 — Deterministic Animation System

**Priority: High**

Animations must use program presentation timestamps rather than wall-clock timing.

Implement:

- [ ] Fade-in and fade-out.
- [ ] Translation and slide animations.
- [ ] Scale animation.
- [ ] Configurable durations.
- [ ] Basic easing functions.
- [ ] Automatic animation completion.
- [ ] Layer replacement and stopping transitions.

Example principle:

```rust
animation_time = current_program_pts - animation_start_pts;
```

### Requirements

- Animations must behave consistently at different frame rates.
- Pauses, seeking, clip transitions, and live-source changes must have documented behavior.
- Do not allow cumulative animation timing drift.
- Ensure consistent results across HLS, SRT, and other output paths.
- Define behavior for discontinuous or reset timestamps.

Use a canonical program timeline rather than assuming that source PTS values are continuous.

Native animations must be evaluated from that time, independent of rasterization completion or worker scheduling. A future external/HTML renderer must declare its timing capabilities separately; do not promise deterministic CSS/JavaScript animation merely because the native timeline is deterministic.

---

## Phase 7 — Graphics Templates

**Priority: Medium**

Introduce a declarative JSON-based graphics template format.

Templates should support:

- [ ] Versioned template schema and explicit supported features.
- [ ] Canvas dimensions, coordinate units, anchors, and scaling policy.
- [ ] Multiple layers.
- [ ] Images, text, and shapes.
- [ ] Dynamic text properties.
- [ ] Positioning and opacity.
- [ ] Basic animation definitions.
- [ ] Runtime data updates.
- [ ] Animated source references and explicit playback/repeat settings, separate from layer animation definitions.

Example:

```json
{
  "name": "lower-third",
  "canvas": {
    "width": 1920,
    "height": 1080
  },
  "layers": [
    {
      "type": "rectangle",
      "x": 80,
      "y": 840,
      "width": 900,
      "height": 160
    },
    {
      "type": "text",
      "value": "{{name}}",
      "x": 120,
      "y": 875
    }
  ]
}
```

Template parsing and resource loading must occur outside the real-time video processing path.

Validate template data and resource paths before activation.

---

## Phase 8 — Control API and Integration

**Priority: Medium**

Expose the graphics subsystem through the existing ffplayout API architecture.

Potential operations:

```text
GET    /api/graphics/{channel}
POST   /api/graphics/{channel}/take
PUT    /api/graphics/{channel}/{layer}
DELETE /api/graphics/{channel}/{layer}
POST   /api/graphics/{channel}/clear
```

Adapt paths and naming to existing project conventions.

### Tasks

- [ ] Map existing logo control to graphics layers.
- [ ] Map existing text overlays to graphics layers.
- [ ] Preserve existing API endpoints.
- [ ] Add graphics state inspection.
- [ ] Allow layer updates without restarting the playout pipeline.
- [ ] Provide deterministic ordering for concurrent control commands.
- [ ] Define behavior during restart, recovery, and configuration reload.
- [ ] Ensure channel isolation.

Do not introduce breaking changes to existing clients.

---

## Phase 9 — Playlist-Based Graphics Events

**Priority: Medium**

Design optional timeline events that allow graphics to be activated or stopped during playlist playback.

Example:

```json
{
  "graphics": [
    {
      "at": 2.0,
      "action": "take",
      "layer": 100,
      "template": "lower-third"
    },
    {
      "at": 10.0,
      "action": "stop",
      "layer": 100
    }
  ]
}
```

### Requirements

- [ ] Execute graphics events against program timeline semantics.
- [ ] Define events relative to a playlist item's effective playback start.
- [ ] Handle skipped, shortened, or interrupted items.
- [ ] Prevent duplicate event execution.
- [ ] Define behavior during live takeovers and schedule jumps.
- [ ] Keep existing playlists valid without modification.

---

## Phase 10 — Testing and Performance

**Priority: Critical**

### Visual Correctness

Create reproducible reference test cases for:

- [ ] White antialiased text over black video.
- [ ] Saturated red text over blue video.
- [ ] Semitransparent gradients.
- [ ] Soft shadows.
- [ ] Overlapping transparent graphics.
- [ ] Transparent PNG edges.
- [ ] Odd-positioned graphics on YUV420.
- [ ] Rec.709 limited-range content.
- [ ] Multiple graphics layers with animated opacity.
- [ ] Color-conversion round trips.

Validate each optimized compositor against an independent reference with the same mathematical policy. Separately compare signal-domain YUV and linear-light RGB visually and numerically, without treating their intentional differences as implementation errors.

Use numerical comparisons where appropriate and retain visual regression test images.

### Performance

Benchmark:

- [ ] Existing compositor versus new implementation.
- [ ] Static graphics compositing.
- [ ] Animated lower thirds.
- [ ] Multiple overlapping layers.
- [ ] YUVA420 versus YUVA444 graphics preparation.
- [ ] Full-frame versus region-based compositing.
- [ ] 1080p25, 1080p50, and higher resolutions where supported.
- [ ] Allocation counts per frame.
- [ ] Memory bandwidth and CPU utilization.
- [ ] Long-running playback stability.
- [ ] Direct caller-thread SIMD versus persistent worker pools, with wall latency and aggregate CPU measured separately.
- [ ] Idle channels, multiple active channels, rapid updates/clear/reload, cache eviction, failed and cancelled preparation, and bounded memory use.
- [ ] Late/out-of-order renderer results and backend failures: verify uninterrupted video and no resurrection of stale layers.
- [ ] Shared-canvas scaling versus destination-specific composition, including multiple HLS variants.
- [ ] Animated logos at differing source/program frame rates, variable frame durations, loop boundaries, and combined source/layer animation.
- [ ] Late animation frames, malformed assets, rapid source replacement, and stop/clear during decoding; verify no video-thread waits or obsolete-frame backlog.
- [ ] Long-running animated logo loops: bounded buffers/cache memory, decoder resource release, and no persistent decoding work for static/inactive sources.

### Performance Goals

- No per-frame allocations in the steady-state hot path where avoidable.
- No unnecessary full-frame conversions.
- No blocking operations on the video processing thread.
- No regression for channels without active graphics.
- Predictable resource usage during continuous 24/7 playout.

Do not claim performance improvements without measurements. Establish a baseline before implementation and repeat focused benchmarks at each relevant stage, rather than postponing performance validation to the end.

---

## Phase 11 — Future Extensions (Do Not Implement Yet)

Keep the architecture extensible for:

- Servo-based HTML/CSS graphics rendering.
- Optional GPU acceleration.
- HDR graphics and wide-gamut color.
- Higher bit-depth compositing.
- Animated vector graphics.
- Advanced transitions and graphical effects.
- External fill/key outputs.
- Preview/program graphics workflows.

### Future Adapter Contracts to Document Now

- [ ] Document how a renderer receives content/data updates, layout changes, scene revisions, and render timestamps or deadlines.
- [ ] Document delivery of ready surfaces, alpha/color descriptors, storage ownership, and last-good-surface behavior.
- [ ] Document capability reporting, unsupported combinations, and timing guarantees per backend.
- [ ] Document cancellation, resource budgets, shutdown, and failure containment requirements for a future Servo adapter.
- [ ] Require explicit evaluation of stronger failure isolation before adding an external/HTML renderer; a scene abstraction alone cannot contain a process crash.
- [ ] Keep any future Servo-specific HTML/CSS/JavaScript behavior behind its adapter rather than adding browser concepts to native scene nodes or control commands.
- [ ] For GPU integration, evaluate renderer-to-compositor resource sharing, device identity, synchronization, and transfers across the video pipeline before choosing an implementation.

Do not implement Servo, GPU backends, or speculative plugin-loading infrastructure now. Document these contracts and implement only the CPU interfaces needed by existing logo/text functionality. Do not add unnecessary abstractions solely for hypothetical future features.

---

## Implementation Order

The phases above describe work areas, not one large delivery. Use these independently reviewable milestones:

1. **Audit and baseline:** document existing behavior, reuse current experiments, and establish matched-policy correctness tests and performance baselines. Fix demonstrated YUV issues separately.
2. **Minimal modular core:** define scene revisions, surface descriptors, layout scopes, bounded commands, and resource ownership. Adapt existing logo/text paths and CPU YUV compositing while preserving behavior. No template system, new public API, or new renderer is required to complete this milestone.
3. **Resource lifecycle:** add asynchronous preparation/activation where needed, stale-result rejection, bounded caching, cancellation, and failure policies. Validate hot-path allocation behavior and 24/7 resource stability.
4. **Native rendering extensions:** add animated logo sources as a separate, reviewable feature using the shared source/surface contract and bounded decoding. Add shapes/SVG and richer text independently as needed. Benchmark preparation/decoding separately from per-frame compositing.
5. **Animation:** extend the canonical program timeline and layout rules with deterministic native transitions.
6. **Templates:** introduce the versioned declarative schema and validation as an independent feature.
7. **Control integration:** expose additional operations through existing application API/configuration architecture while preserving current clients.
8. **Playlist events:** add optional scheduling adapters with explicit seek, skip, interruption, and live-takeover semantics.
9. **Optional quality/backend work:** decide on production linear-light compositing, Servo, or GPU implementations in separate proposals backed by measurements and capability requirements. These are not prerequisites for the CPU graphics subsystem.

**Do not implement all phases in one large commit.** Stop at the agreed milestone; later features are not implicit requirements for an earlier delivery. Run relevant correctness, compatibility, lifecycle, and performance checks throughout implementation.

## Final Acceptance Criteria

The implementation is successful when:

- ffplayout can composite multiple independent graphics layers.
- Existing logo and text features remain compatible.
- The normal YUV video pipeline is preserved.
- Alpha blending is verified against an independent high-precision reference with matching compositing semantics.
- Graphics can be activated, updated, and removed dynamically.
- Animations are deterministic and synchronized to program frames.
- Static graphics are cached efficiently.
- Animated logos use program-time frame selection and bounded decoding without blocking playout; existing static-logo behavior remains compatible.
- Playout remains operational when graphics rendering fails.
- The subsystem does not require a browser runtime or GPU.
- The architecture can support future rendering and compositing backends without redesigning its scene/control model or requiring CPU readback in every backend.
- Resource budgets, cancellation, stale-result rejection, and shutdown behavior are explicit and tested.
- Shared program graphics scale consistently across renditions, and destination-specific graphics use an explicit composition scope.

Apply these criteria to the completed milestones: animated sources, templates, new API operations, playlist events, and future backends must not block acceptance of the minimal modular core.

## Important Agent Constraints

**Prioritize correctness, maintainability, and broadcast reliability over feature count.**

Do not replace the current working compositor merely because another pixel format or rendering approach appears more conventional.

Investigate first, measure second, and modify only where a clear technical benefit exists.

Preserve ffplayout's CPU-first architecture and existing YUV processing model while establishing a clean path toward higher-quality compositing.
