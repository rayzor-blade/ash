# hlwgpu

WebGPU for HashLink: command encoders, bind groups, render and compute
pipelines, WGSL shaders.

```haxe
var device = wgpu.Instance.create().adapter().device();
var queue = device.queue;

var pipeline = device.pipeline()
    .shader(device.shader(WGSL), "vs", "fs")
    .vertexBuffer().attributes(Float32x2, Float32x4)
    .target(Rgba8Unorm).blend(One, One)
    .build();
```

Builders are validated as you write them: an attribute with no vertex buffer
open, or a build with nowhere to draw, will not compile.

## Coordinates

These are WebGPU's, and they are not OpenGL's. See `test/Conventions.hx`.

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

## Native target and Browser via wasm

Natively this is `wgpu.hdll`. It holds the implementation, answers the VM
directly, and needs nothing else -- any HashLink can load it.

The browser route is **ash's**: ash is what compiles a HashLink program into a
wasm module, runs it in a page, and loads native libraries alongside it. This
library supplies its own half of that. A wasm module has neither a GPU nor
JavaScript, so `wgpu.wasm` holds no implementation at all -- every primitive
calls out, and `js/hlwgpu.js` is what answers. A page adds that with a script
tag and merges what it exports into the imports the module is given. Nothing
is compiled and nothing is rebuilt.

`IMPORTS.md` is the same contract written out, for a host that is not a page.

The native half is what the tests cover. The browser half is written, and
generated from the same declaration, but has not been run yet.

## Drawing into a window

Somewhere to draw has to come from somewhere. Anything that fits
`wgpu.WindowSource` will do -- it reports where a native window is, and this
library asks it nothing else.
