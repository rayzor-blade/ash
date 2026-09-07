# hlwgpu

WebGPU for HashLink: buffers, textures, WGSL shaders, render and compute
pipelines. It is the layer you would build a renderer on, not a renderer.

```haxe
var device = wgpu.Instance.create().adapter().device();
var queue = device.queue;

var pipeline = device.pipeline()
    .shader(device.shader(WGSL), "vs", "fs")
    .vertexBuffer().attributes(Float32x2, Float32x4)
    .target(Rgba8Unorm).blend(One, One)
    .build();
```

Builders are validated as you write them. An attribute with no vertex buffer
open, or a build with nowhere to draw, will not compile.

`vertexBuffer().attributes(...)` works out the stride, the byte offsets and the
shader locations from the formats you list. If you need a layout that is not
packed, `attribute(format, offset, location)` places one by hand.

## Coordinates

WebGPU's coordinates are not OpenGL's. These are the two differences that catch
people out:

- Clip space has +y pointing up, but the first row of a texture is the top row.
  So geometry at +y comes back near the start of a readback buffer, and texture
  coordinates start at the top left corner.
- Depth runs from 0 to 1, not from -1 to 1. A vertex at z = -0.5 is behind the
  near plane, so it is clipped away rather than drawn in front of everything.

`test/Conventions.hx` checks both of these, because a symmetric test cannot.

## Destroying things

Call `destroy()` on anything that has it. Buffers, textures and pipelines are
not freed when they go out of scope, so anything you do not destroy holds its
GPU memory until the process exits. Destroying something twice is safe.

## Waiting

Nothing in this library blocks. Anything that takes time gives you a `Request`
instead: check `ready` whenever you like, or call `await()` to wait for the
result. `await()` yields between checks rather than spinning, so your other
threads keep running while the GPU works.

## Native target and Browser via wasm

On desktop, hlwgpu is one native library, `wgpu.hdll`. HashLink loads it and
calls straight into it, and there is nothing else to install.

For the browser we recommend [ash](https://ash.rayzor.tech), which builds a
WebAssembly binary from the same HashLink bytecode you already have, and hosts
it in a page.

hlwgpu's wasm tooling is designed around ash's, but it is not tied to it. A
WebAssembly module cannot reach a GPU or call JavaScript on its own, so
`wgpu.wasm` contains no GPU code at all and forwards every call out to whatever
is hosting it. In a page that is `js/hlwgpu.js`, which does the real work
against `navigator.gpu`: include it with a script tag and pass what it exports
to the module as imports. Nothing needs compiling.

`IMPORTS.md` lists every function a host has to provide, so you can write your
own instead.

## Getting a window to draw into

hlwgpu does not create windows. Implement `wgpu.WindowSource` on whatever
provides yours, then pass it to `Surface.fromWindow`.

```haxe
typedef WindowSource = {
    function surfacePlatform() : Int;
    function surfaceHandle(which : Int) : haxe.Int64;
}
```

`surfacePlatform` says which kind of native handle you are reporting, and
`surfaceHandle` returns its fields: 0 and 1 for the window, 2 and 3 for the
display.
