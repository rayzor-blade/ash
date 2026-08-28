# MBHaxe on Ash

MBHaxe must be tested with RandomityGuy's matching dependency family. Ash's
decommissioned Rust `ash_sdl` crate is not ABI or behaviorally interchangeable
with that game's SDL2 HSDL bindings and must never be copied into the fixture.

Use the repository workflow rather than assembling the directory by hand:

```bash
cargo build --release --bin ash
scripts/prepare_mbhaxe.sh --native-dir /path/to/mbhaxe/native-hdlls
cd target/mbhaxe/run
./ash --mode interp marblegame.hl
```

The native directory supplies the game's non-SDL extensions, such as `fmt`,
`ui`, `uv`, `openal`, and `datachannel`. Its `sdl.hdll` is deliberately ignored.
The script always builds SDL itself from the pinned
`RandomityGuy/hashlink:libs/sdl` C sources and their Haxe externs, linked to
SDL2 and the staged Ash compatibility runtime.

Every run recreates `target/mbhaxe/run` and validates that:

- `sdl.hdll` contains no `ash_sdl` or Cargo `target/*/deps/libsdl` provenance;
- it links SDL2, never SDL3;
- it resolves `@rpath/libhl.dylib` beside the staged `ash` executable;
- the key window and shader exports expected by MBHaxe exist; and
- all source commits and artifact hashes are recorded in `PROVENANCE.txt`.

The pinned source revisions live at the top of
[`scripts/prepare_mbhaxe.sh`](../scripts/prepare_mbhaxe.sh). Update them as one
reviewed dependency set; do not update the Haxe HSDL externs independently of
the native HashLink SDL sources.
