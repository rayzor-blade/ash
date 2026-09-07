# hlwgpu -- WebGPU for HashLink

A native library giving HashLink programs a modern, explicit GPU API: command
encoders, bind groups, render and compute pipelines, WGSL shaders. The
primitives are Rust; the externs and the classes a program writes against are
Haxe.

It is not a renderer and not an engine. It is the layer an engine would be
written on. It is meant to be consumed by people who are not us, so
self-containment and a documented contract come before anything convenient for
ash.

## Why WebGPU is the API

Modern GPU programming means one of Vulkan, D3D12, Metal, or the API that
abstracts all three. Only one of those has an implementation in a browser, and
ash is going to 100% on wasm. WebGPU is the only choice that lets one Haxe
program run on a desktop and in a page without a second API and a second set of
shaders.

`wgpu` is the Rust implementation. It already resolves to Vulkan, Metal and
D3D12 underneath, so this is a translation layer over one crate rather than
three backends to maintain.

## Where the implementation lives

**Natively, `wgpu.hdll` is the whole library.** It contains wgpu, links against
libhl and answers the `DEFINE_PRIM` protocol, so it loads in upstream HashLink
as readily as in ash. Nothing in this repo is needed to run it. That is the
baseline for a library others consume, and it is the common case.

**On wasm the split is forced by the platform, not chosen.** A `wasm32-wasip1`
module has no GPU and no JavaScript interop; no version of this library reaches
a device from inside one. So on wasm, and only on wasm, the primitives call out
-- and hlwgpu ships what answers them:

| context | guest | what answers it | implementation |
|---|---|---|---|
| native, any HashLink | -- | -- | `wgpu.hdll`, self-contained |
| wasm in a page | `wgpu.wasm` calls out | `hlwgpu.js`, linked by the page | `navigator.gpu` |
| wasm under wasmtime | `wgpu.wasm` calls out | `hlwgpu_host`, a Rust crate | the wgpu crate |

**The browser half is an emitted `hlwgpu.js`**, exactly as wasm-bindgen emits
glue beside its `_bg.wasm`. A page adds a script tag and merges the object it
exports into the imports it already builds. Nothing is compiled and nothing is
rebuilt, so an embedder that has never heard of ash has one line to write.

That also removes what was this plan's largest risk. An earlier draft made the
browser half Rust over `web-sys`, which required wgpu's WebGPU backend to build
for `wasm32-unknown-unknown` against bindings historically gated behind
`web_sys_unstable_apis`. A JS shim calls `navigator.gpu` directly and none of
that arises.

The import names live in `env`, prefixed `hlwgpu_`, so there is no new
namespace and no change to import resolution: `native/dylink.rs` already asks
the host for any `env` name the program does not export, which is the mechanism
landed in 1a8d874. **Exactly one generic change to ash is needed** -- a page
must be able to merge its own entries into `env`, which `browser/imports.rs`
builds internally today. That hook belongs to ash, not to hlwgpu, and every
library shipping a JS half uses the same one.

`hlwgpu_host` is the same contract for a host with no JavaScript: the
conformance lane, and headless compute under wasmtime. It shares `hlwgpu_impl`
with the native library, so it is nearly free, and it is the lowest priority of
the three.

An earlier draft had the guest importing `ash_host_wgpu_*` with the host half
inside `ash_wasm_runtime`, generalising from `crates/tinysdl`. That was
backwards. tinysdl is a demo -- sixty-seven primitives answered by a recorder,
built to prove a mechanism -- and what is worth keeping from it is the
mechanism, not its architecture. A library whose contract is defined by one
embedder's host is not portable, and this one has to be.

`wasi:webgpu` (wasi-gfx) is this interface on the standards track, and is the
eventual answer for hosts that want one. Milestone 1 should look at whether it
is usable yet; the declaration is shaped so the namespace can move.

## The interface is declared once, and hlwgpu owns every side

`scripts/generate_sdl_shim.py` does a version of this for one library, emitting
a guest shim and host bindings from the signatures a `.hdll` reports about
itself. The idea is right; being tied to one library, and to a binary that has
to exist first, is not. `crates/hl_native_gen` is the general form, called from
`build.rs`, so building a library needs cargo and nothing else.

One declaration, one tool, every side emitted -- **all of it inside hlwgpu**:

    crates/hlwgpu/wgpu.api                      the declaration

    crates/hlwgpu/src/kinds.rs                  the handle kind numbering
    crates/hlwgpu/src/native.rs                 prims -> imp
    crates/hlwgpu/src/wasm.rs                   prims -> the hlwgpu imports
    crates/hlwgpu/host/src/bindings.rs          imports -> imp
    crates/hlwgpu/js/hlwgpu.js                  imports -> navigator.gpu
    crates/hlwgpu/haxe/wgpu/_Native.hx          externs, with signatures
    crates/hlwgpu/IMPORTS.md                    the contract, for other hosts

Six artifacts, one source. Two pieces are hand-written, each once: `hlwgpu_impl`
in Rust, and a small runtime prelude in `hlwgpu.js` -- the handle table, the
memory views, the ticket bookkeeping. Everything per-primitive is generated on
both sides, which is the split `generate_sdl_shim.py --host` already makes
between generated arithmetic and hand-written behaviour.

Derived from a declaration rather than from annotations on the Rust, as
wasm-bindgen does, because it also has to emit Haxe, JavaScript and a documented
contract, and because a library whose implementation is not Rust still needs
every side. Drift is a compile error rather than a silent mismatch: a generated
wrapper calls `hlwgpu_impl::device_create` with generated argument types, so a
declaration disagreeing with the implementation does not link.
`crates/ash/tests/native_signatures.rs` stays the runtime check that what ships
reports what was declared.

The tool is general, and `sdl` can be retrofitted onto it later. A convenience,
not a goal; nothing here waits on it.

## Naming

Project, crate and haxelib: `hlwgpu`. The native library it ships is
`wgpu.hdll` / `wgpu.wasm`, so a primitive is `@:hlNative("wgpu", "device_create")`
answered by `hlp_device_create` -- the relationship `hlsdl` has to `sdl.hdll`.
Haxe package `wgpu`.

## Three layers

    crates/hl_abi/            the HashLink C ABI, shared by every library here
    crates/hl_native_gen/     the generator, as a build-dependency
    crates/hlwgpu/            the declaration, the prims, imp.rs
    crates/hlwgpu/host/       the host half for wasm embedders without JS
    crates/hlwgpu/js/         the host half for a page, emitted
    crates/hlwgpu/haxe/       the externs, generated, and the classes

The externs are a flat one-to-one account of the primitives. The classes above
them are where the API becomes pleasant: `wgpu.Device`, `wgpu.Buffer`,
`wgpu.RenderPass`, typed enums, descriptors as Haxe structures. A program
should never see an extern.

`crates/hl_abi` is the C ABI, shared by `hlwgpu`, `ash_hdll_sqlite` and
`tinysdl`, which each had a copy. It holds only `#[repr(C)]` layouts and
`extern "C"` declarations, so it defines no symbol and a library built on it
links nothing of the runtime into itself -- which is the property that made
writing the file out necessary rather than depending on `ash_std`, whose
`#[no_mangle]` exports would arrive as a second definition of every name.

## Handles are integers

Every GPU object -- adapter, device, queue, buffer, texture, view, sampler,
shader module, bind group, layout, pipeline, encoder, pass, query set --
crosses as an `i32`.

Not `hl.Abstract`, which is a raw pointer:

1. **A pointer cannot cross to a host.** In the two wasm rows the object lives
   in the host's address space, not the guest's. An integer means the same on
   both sides, so one signature serves whether the implementation is local or
   across a boundary.
2. **Use-after-destroy becomes an exception instead of undefined behaviour.**
   The integer packs an index and a generation -- 20 bits and 12, so a million
   live objects and 4096 reuses of a slot before wraparound. A stale handle
   fails its generation check and raises. `ash_hdll_sqlite` stamps magic
   numbers into its structs for a weaker version of this.
3. **It costs nothing in Haxe.** `abstract Buffer(Int)` gives back every bit of
   the type safety `hl.Abstract` would have and compiles to a bare `Int`.

The slab lives behind a plain `RwLock`. Not an HL mutex: a library's own
threads are foreign to the VM (`foreign-thread-identity` -- `current_id()` is 0
for every fiber-less thread, and HL mutexes gave HDLL threads no exclusion
until b90e749).

## Resources must be destroyed explicitly

**ash runs no finalizers.** `hl_gc_alloc_finalizer` allocates and leaves the
finalizer slot null, but nothing in `std/src/gc.rs` ever calls one --
`std/src/file.rs:11` says so about file handles. A `wgpu.Buffer` that goes out
of scope in Haxe leaks its VRAM until the process exits. Upstream HashLink
*does* run finalizers, so a library relying on them would behave differently on
the two VMs, which is its own reason not to.

That is the lifetime model, not a defect to route around:

- Every resource class has `destroy()`, idempotent.
- Destroying frees the slab slot and bumps its generation, so a stale handle
  raises rather than reaching a reused object.
- A `wgpu.Scope` helper destroys everything registered with it on exit.
- Debug builds keep an allocation site per live handle and can dump what is
  still alive; "the buffer leaked" is otherwise unattributable.

## No GC pointer may live in Rust

`rust-heap-has-no-gc-root` is the most-repeated bug in this codebase -- four
instances, one shape. The collector does not scan the malloc heap, so a
`*mut vdynamic` whose only holder is a Rust `Vec`, `Box` or `HashMap` is
unrooted and the next collection takes it.

A GPU library walks into this, because WebGPU's asynchronous operations are
where one wants to stash a callback: `buffer.mapAsync()`,
`queue.onSubmittedWorkDone()`, `device.popErrorScope()`, `device.lost`.

**Rust stores tickets, Haxe stores closures.** A `vclosure*` never crosses into
the library. Haxe keeps the callback where the collector traces it and passes
an `i32` index; on completion the library hands the ticket back and Haxe
dispatches. Nothing to root, because nothing is held.

The gate is `ASH_GC_STRESS=1` output bit-identical to a normal run, over the
whole corpus, comparing stdout only.

## Every primitive is non-blocking; Haxe does the waiting

WebGPU is asynchronous at adapter and device request, buffer mapping,
submitted-work completion, error scopes, and presentation. Natively `wgpu`
blocks on `device.poll(Wait)`. In a page nothing can block: a promise settles
only when the JS task returns, the mechanism that stopped a HashLink worker
from ever painting an OffscreenCanvas earlier in this project.

The obvious answer is to suspend inside the primitive, and it does not work.
`native/sdl.rs` gives the reason for its frame boundary and it generalises:
**suspension has to come from the engine -- `func_wrap_async` under wasmtime,
JSPI in a page -- and not from the link-time fiber transform.** The transform
instruments the *program's* module; a suspend point inside `wgpu.wasm` is in a
module it never saw, and an unwind travels exactly as far as the instrumentation
does. Depending on JSPI would also pin the browser support matrix to whichever
engines have shipped it.

So invert it. **No hlwgpu primitive ever blocks or suspends.** Work is started
and polled:

    buffer_map_begin(buf, ticket)     starts it, returns immediately
    ticket_ready(ticket) -> Bool      never blocks

and the waiting happens in Haxe, in the program's own module, where the fiber
transform *is* in force:

```haxe
var data = buffer.read(0, 1024);   // loops on ticket_ready, yielding
```

That line blocks the calling Haxe thread on both targets -- natively by
spinning `device.poll` behind the ticket, in a page by yielding the fiber so
the task returns, the promise settles and the host resumes it. The library
stays free of suspension machinery, the API reads synchronously, and no host is
required to support stack switching to run it.

## Wrap every wait in `hl_blocking`

A thread parked waiting is not at a safepoint. Leave it unannounced and the
collector produces exactly the failure seen earlier in this project: *gave up
stopping the world after 2021ms; 1 of 5 mutators never reached a safepoint*.

`hl_blocking(true)` before a wait and `hl_blocking(false)` after, in the Haxe
polling helper. Cheap, easy to forget, and the symptom points nowhere near the
cause.

## Descriptors are built by typed calls, never a blob

WebGPU descriptors are deep: a render pipeline carries vertex buffer layouts
carrying attribute arrays, plus blend state per colour target, depth, stencil
and multisampling. Two ways to move one across:

- **One packed `hl.Bytes` with a declared layout**, decoded on the far side.
- **A sequence of calls, each taking typed scalars**, that accumulate into the
  descriptor before one call builds the object.

Take the calls. A packed buffer is an *untyped* blob: nothing checks it at
either end except the generator that wrote both sides, so a layout mistake is a
silent misread rather than a compile error, and it needs a Rust decoder and a
JavaScript decoder that must agree. Typed calls have none of that -- every
argument is an `i32`, `f64` or `bytes` the compiler checks on both sides, and
there is no encoding to get wrong.

    pipeline_begin(device) -> builder
    pipeline_vertex(builder, shader, entry, stride, step_mode)
    pipeline_attribute(builder, format, offset, location)
    pipeline_target(builder, format, blend_src, blend_dst, blend_op, write_mask)
    pipeline_depth(builder, format, write_enabled, compare)
    render_pipeline_build(builder) -> pipeline

What it costs is boundary crossings, and that cost lands where it does not
matter: pipelines, layouts and bind group layouts are built once at load, not
per frame. The earlier plan reached for the packed buffer to save crossings in
a page, which was optimising the wrong thing at the price of the only static
checking the boundary has.

**And the calls make the Haxe side a fluent builder whose type states carry
the rules.** A run of calls has an order the primitives cannot check for
themselves -- an attribute belongs to the vertex buffer opened last, a blend to
the target opened last -- so each stage is a separate abstract over the same
handle, and the compiler refuses a chain that does not make sense:

```haxe
device.pipeline()
    .shader(shader, "vs", "fs")
    .vertexBuffer().attributes(Float32x2, Float32x4)
    .target(Rgba8Unorm).blend(One, One)
    .build();
```

`attributes` is variadic, and packs: the stride and every offset follow from
the formats, at consecutive shader locations. Those are numbers a caller would
otherwise compute by hand and occasionally get wrong -- `attribute(format,
offset, location)` is still there for a layout that is not packed.

`attribute` exists only after a `vertexBuffer`, `blend` only after a `target`,
and `build` only once there is something to draw into. Every state is an
`abstract X(Int)` with `inline` methods, so none of it costs anything at
runtime. `test/constraints.sh` checks it the only way a thing that must not
compile can be checked: by trying.

**Data still crosses as bytes**, because it is data: buffer contents, texture
pixels, and a homogeneous array of handles. The rule is that *structure* never
crosses as bytes -- if the far side has to know what field lives at what
offset, it should be an argument instead.

## Every layer is typed

There is no `Dynamic` and no `untyped` in this library, and there should not
be. Handles are `abstract X(Int)`, so a buffer cannot be passed where a texture
belongs even though both are integers underneath. Enumerations are `enum abstract`, so a format is
not an arbitrary number -- and they are **generated from the vendored IDL**, so
their values and order are the spec's and nothing is typed out. Descriptors a program
writes are typedefs with concrete field types, which means a structure literal
is checked field by field:

```haxe
device.createRenderPipeline({
    vertex: { module: shader, entryPoint: "vs", buffers: [...] },
    fragment: { module: shader, entryPoint: "fs", targets: [...] },
    depthStencil: { format: Depth24Plus, depthWriteEnabled: true, depthCompare: Less }
});
```

That reads the way WebGPU reads, which is the point -- someone who has written
WebGPU should recognise it -- and an unknown field or a wrong type is a
compile error rather than something that shows up as a blank window.

## Shaders: WGSL, and nothing else

WGSL is the only shading language a browser accepts. Natively `wgpu` will also
take SPIR-V and GLSL, and it is tempting to expose that.

Don't. **No primitive may exist on one target and not another**; that is how
cross-target code breaks in the target tested least. The library takes WGSL
text and returns a shader module.

hxsl translation, and a Heaps driver over this library, come later and are
separate work. Nothing here is shaped around them.

## Surfaces

The library never creates a window. Natively that would mean owning an event
loop; in a page a canvas belongs to the embedder. A surface is requested
against a target the embedder already has, and that target is the one place
where the platform genuinely shows through:

    wgpu.Surface.fromCanvas("main")     a page
    wgpu.Surface.fromWindow(handle)     native

Both go through one primitive taking one packed descriptor whose first field is
a tag, so the *symbol* exists everywhere and only the tag is refused by the
wrong host -- a raised error naming the target kind, not a missing primitive.
That is the narrowest reading of the rule above: no capability silently absent,
nothing that links on one target and not another.

**In a page the target is an OffscreenCanvas, and the name is a registry key.**
HashLink runs on a Worker, which cannot touch the DOM, so
`document.getElementById` is unavailable and would return null if it were. The
page transfers a canvas with `transferControlToOffscreen()` and hands it to
`hlwgpu.registerCanvas("main", offscreen)`; `fromCanvas("main")` looks that up.
Registration is a JavaScript call because the canvas is a JavaScript object,
which is one more thing the JS shim makes ordinary. Both flavours are supported,
and both give a context through `getContext("webgpu")`:

- a canvas **transferred** from the page, which is the presentation path, and
- a standalone `new OffscreenCanvas(w, h)` made in the worker, which is
  offscreen rendering in a page with no displayed canvas at all.

WebGPU renders straight into the transferred canvas's swap chain, so the RGBA
framebuffer copy `browser/canvas.rs` performs today is not on this path.

**Presenting is the frame boundary, and it does not block either.** A
transferred OffscreenCanvas only reaches the page at the end of a task, so a
program that renders in a loop and never returns paints nothing -- measured
earlier in this project, where a whole run composited as one frame after every
thread had finished. `surface_present()` returns immediately and the Haxe frame
loop yields, which is what lets the host resume on `requestAnimationFrame`. A
frame ends where the *program* suspends.

**The window comes from somewhere else.** `crates/hlwindow` is that somewhere
on native: a small `winit` companion, its own `window.hdll`, which a program
that renders offscreen or into a page's canvas never loads.

The two libraries share no Rust type and no Haxe type. `hlwindow` reports its
raw handle as a platform code and four integers; `hlwgpu` puts them back
together. On the Haxe side `wgpu.WindowSource` is a structural type, so a
`window.Window` satisfies it by shape and neither library names the other. That
is what keeps hlwgpu depending on nothing while still being able to draw into
a window.

**v1 is headless-first** for that reason and a better one: offscreen textures
and readback need no display, are exactly reproducible, and are the only way to
assert a rendered image in CI.

## Milestones

Native first. The page half of each is designed for but not built, because a
desktop library that works is worth more than two halves that do not.

1. **The tool and the seam.** DONE natively. `wgpu.api`, the generator, and
   `Instance.request()` reporting an adapter's name, backend and limits.
2. **Compute.** DONE natively. Buffer upload, a WGSL kernel, dispatch, and a
   polled readback checked value by value.
3. **Offscreen render.** DONE natively. A vertex buffer, a render pipeline, a
   pass into a texture, and a readback whose pixels are checked exactly.
   Colours are 0 or 1 per channel so the unorm conversion is exact on any GPU;
   PNG output is a convenience that has not been needed.
4. **Textured, indexed drawing.** DONE natively. Samplers, texture upload,
   index buffers, and bind groups holding buffers, views and samplers
   together. A 2x2 texture on a 64x64 quad puts one texel in each quadrant,
   so the four expected colours are exact.
5. **Presentation.** DONE natively. A surface on a native window, configured,
   acquired, drawn and presented every frame. `crates/hlwindow` is the small
   `winit` companion that supplies the window; depth and blending are still
   ahead.
6. **Capability.** Close the gap the IDL measures: the remaining enums, the
   descriptor members the builder now has somewhere to put, and the 30
   operations. Blending and depth landed with the builder; instancing,
   multisampling, and multiple colour targets are the next descriptor members,
   and `setViewport`, `setScissorRect` and the texture-to-texture copies the
   next operations.
7. **The page.** Milestones 1 to 4 unchanged against `navigator.gpu`, which
   needs ash's one generic import hook and a browser to verify in. If any of
   it needs a Haxe-side `#if`, something above went wrong.
8. **Later, separately.** A `winit` companion for native windows. hxsl, a
   Heaps driver, SPIR-V ingestion, ray queries.

## Testing

Compute tests assert exact values. Render tests compare against golden images
with a tolerance, because rasterisation differs between vendors.

`test/Conventions.hx` pins what a symmetric test cannot see: that clip space
has +y upward while a texture's first row is the top, and that depth runs 0 to
1 rather than -1 to 1. Both are places someone arriving from OpenGL expects
the opposite, and a flip in either would have been caught only incidentally,
by the one textured test whose four quadrants differ.

CI runs on the NUC against **lavapipe** (Mesa's software Vulkan): no GPU
needed, deterministic, so a golden image means something. A real-GPU run is a
separate, non-gating job.

Three gates beyond the tests, all of which have caught this class of bug here
before:

- `ASH_GC_STRESS=1` bit-identical over the corpus -- the rooting gate.
- Every handle created in a test is destroyed, asserted by the live-handle dump.
- Conformance still at 100%, since the library changes what a program exports.

One more, because of what this library claims to be: `wgpu.hdll` loads and runs
its compute test **under upstream HashLink**, not only ash. If that ever fails,
the library has grown a dependency on us.

## What is deliberately not here

- **SDL, in any form.** Not a dependency, not a surface source, not a fallback.
- **Hand-written JavaScript.** `hlwgpu.js` is emitted from the declaration, and
  its only hand-written part is a prelude that knows nothing about primitives.
- **Any dependency on ash.** The native library must load in stock HashLink,
  and the browser half must work in a page that has never heard of us.
- **A Heaps driver.** Downstream, and it needs hxsl work that is not this.
- **A renderer, scene graph, or material system.** This is the layer under those.
- **GL or WebGL.** The point of WebGPU is not writing that translation.

## What building this found in ash

A library is a good way to walk into the parts of a VM nobody has needed yet.

**The interpreter dispatches a native through a hand-written table** keyed by
arity, return kind and a bitmask of which arguments are floats
(`ash_interp/src/interpreter/natives.rs`). A shape not in the table is a
runtime error, not a miscompile, but it is still a wall: `encoder_render_begin`
is `(i32, i32, f64, f64, f64, f64)` and no arm matched, and its depth variant
needed a seven-argument one, of which the table had none at all. Two arms so
far, one per new pass shape. A float-heavy library will keep finding these, and
the real answer eventually is a signature-directed dispatcher rather than a
table.

## A miss must not look like an answer

`surface_preferred_format` mapped wgpu's format to ours and returned 0 for
anything unrecognised -- and 0 is `Rgba8Unorm`, a real format. The first
surface it met preferred `Bgra8UnormSrgb`, which the list did not have, so it
answered "Rgba8Unorm" with confidence and `configure` panicked two calls later
with a validation error that named neither the cause nor the caller. On screen
it was a blank white window.

It now answers -1, which is not a format. The same shape as the conformance
harness reporting a compiler warning as the reason a suite failed: a default
that is indistinguishable from a real result turns a clear failure into a
puzzle. Any lookup added here should fail loudly rather than plausibly.

## Queued

- **The declaration parser should be `nom` or `pest`.** `hl_native_gen` splits
  strings by hand, which is fine for `prim`, `js` and `kinds` lines and will
  not stay fine as the declaration grows descriptor layouts and enums. A real
  grammar before that happens, not after.

## Open questions

- Whether `wasi:webgpu` is mature enough to be the import namespace. If it is,
  the wasm half is portable by standard rather than by documentation.
- The shape of ash's one generic hook: whether a page passes extra `env`
  entries to `run()`, or registers them on a global the loader reads. The
  second needs no signature change and matches how `ashPresent` is already
  found.
- Handle packing: 20/12 index/generation in one `i32`. If a million live
  objects is not enough headroom, `i64`, at the cost of clumsier Haxe in hot
  paths.
- Whether `wgpu.Scope` suffices for lifetimes or the slab needs reference counts.
