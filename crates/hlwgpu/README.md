# hlwgpu

WebGPU for HashLink: command encoders, bind groups, render and compute
pipelines, WGSL shaders. Not a renderer -- the layer one is written on.

```haxe
var device = wgpu.Instance.create().adapter().device();
var queue = device.queue;

var pipeline = device.pipeline()
    .shader(device.shader(WGSL), "vs", "fs")
    .vertexBuffer().attributes(Float32x2, Float32x4)
    .target(Rgba8Unorm).blend(One, One)
    .build();
```

The builder's shape is checked as you write it: an attribute with no vertex
buffer open, or a build with nowhere to draw, will not compile.

## Coordinates

These are WebGPU's, and they are not OpenGL's. `test/Conventions.hx` is what
keeps them that way.

- **Clip space has +y upward**, and the first row of a texture is the **top**.
  So +y lands in the early rows of a readback, and texture coordinates start
  at the top-left.
- **Depth runs 0 to 1**, not -1 to 1. A vertex at z = -0.5 is behind the near
  plane and is clipped away rather than drawn in front of everything.

## Destroying things

Everything with a `destroy()` needs one. Nothing frees a buffer, texture or
pipeline when it goes out of scope, and GPU memory is not the sort to leave
to chance. Destroying twice is harmless.

## Waiting

Nothing in this library blocks. Work that takes time hands back a `Request`,
which you can ask `ready` at any moment or `await` -- and awaiting yields
between checks rather than spinning, so other threads keep running.

## Where it runs

Natively it is `wgpu.hdll`, which contains the implementation and loads in any
HashLink. On wasm the primitives call out, because a wasm module has neither a
GPU nor JavaScript, and `js/hlwgpu.js` is what a page answers them with.
`IMPORTS.md` is the contract for any other host.

Drawing into a window needs a window from somewhere: anything that fits
`wgpu.WindowSource` will do.
