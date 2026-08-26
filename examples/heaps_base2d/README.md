# Heaps Base2D on Ash

This example uses HashLink's SDL3 hlsdl sources rather than Ash's experimental
Rust SDL shim. `bin/sdl.hdll` is a macOS arm64 build from HashLink hlsdl 1.17
at commit `fca7164dab6235ad4e4281c9d32540e7d84d0fdd`, statically linked with SDL
3.4.10.

The HDLL carries the relocatable `LC_RPATH @loader_path` change proposed in
[HaxeFoundation/hashlink#974](https://github.com/HaxeFoundation/hashlink/pull/974).
Without it, a raw CMake build embeds its absolute `build/bin` path and can load
the builder's HashLink `libhl` alongside Ash's compatibility runtime.

SHA-256:

```text
b58e23e4f631f3edcc0041fd4c8ecb89df43afe5db6f0f4846d5b544ce33cdca  bin/sdl.hdll
```

Compile with matching hlsdl 1.17 and Heaps 2.1 sources, then run:

```bash
haxe compile.hxml
../../target/release/ash --mode hybrid bin/game.hl
```

The full setup and troubleshooting guide is published at
<https://rayzor-blade.github.io/ash/heaps.html>.
