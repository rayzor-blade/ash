# The WebGPU IDL

`webgpu.idl` as published at <https://gpuweb.github.io/gpuweb/webgpu.idl>,
fetched 2026-09-07. Auto-generated from the specification; not edited here.

It is vendored because it is the checklist. hlwgpu does not have to look like
this IDL -- its API is its own -- but everything a browser's WebGPU can do
has to be reachable through it, and this is the list of what that is.

Measured against it on the day it was vendored:

| | in the spec | reachable |
|---|---|---|
| operations | 66 | 41 |
| dictionary members | 121 | few |
| enum values | 278 | 57 |

The enums are where most of it lives: `GPUTextureFormat` alone has 101 values
and hlwgpu names five. They need no new primitives -- they are the contents of
descriptors -- and seven of them are now generated from this file by
`hl_native_gen`, named in `wgpu.api`. The rest should follow the same way.
