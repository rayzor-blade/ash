# MBHaxe on ash

Build the fixture with the script, never by hand:

```bash
cargo build --release --bin ash
scripts/prepare_mbhaxe.sh --native-dir /path/to/mbhaxe/native-hdlls
cd target/mbhaxe/run
./ash --mode interp marblegame.hl
```

The script recreates `target/mbhaxe/run` every run, records source commits and
artifact hashes in `PROVENANCE.txt`, and refuses to continue unless:

- `sdl.hdll` has no `ash_sdl` or `target/*/deps/libsdl` provenance;
- it links SDL2, not SDL3;
- it resolves `@rpath/libhl.dylib` beside the staged `ash`;
- MBHaxe's expected window and shader exports are present;
- `ash`, `libhl.dylib`, `sdl.hdll` and SDL2 are all the host architecture.

## SDL comes from the pinned sources

The script always builds `sdl.hdll` itself, from
`RandomityGuy/hashlink:libs/sdl` and its Haxe externs. The decommissioned
`ash_sdl` crate is not ABI-compatible with those bindings; do not substitute
it. Any `sdl.hdll` in `--native-dir` is ignored.

Pinned revisions are at the top of
[`scripts/prepare_mbhaxe.sh`](../scripts/prepare_mbhaxe.sh). Update them as one
reviewed set — the Haxe HSDL externs and the native SDL sources move together.

## Architecture is checked, not assumed

A Mac can carry both Homebrew prefixes: `/usr/local` (Intel) and
`/opt/homebrew` (arm64), each with its own `pkg-config` and SDL2. Whichever
leads `PATH` wins, so `pkg-config sdl2` on an arm64 host can report the x86_64
install, and the build then dies in `SDL_cpuinfo.h` pulling x86 intrinsics into
an arm64 translation unit. The script picks SDL2 with `lipo` instead of
trusting `PATH`, and refuses when no matching build exists.

It checks the staged binaries the same way, because dyld reports an
architecture mismatch as "not found" — which reads as a missing HDLL and sends
you looking in the wrong place.

## Native HDLLs

`--native-dir` supplies the non-SDL extensions: `fmt`, `ui`, `uv`, `openal`,
`datachannel`. The pinned build registers `hlsdl`, `datachannel` and
`hlopenal`. Missing ones are named up front rather than surfacing as a load
failure at launch.

Take that directory from a shipped macOS build, not from the component
repositories: `RandomityGuy/hashlink`'s darwin release is an x86_64 nightly
from 2022, and the `hxDatachannel` archive carries a Windows PE DLL. Neither
loads on an arm64 Mac. A release `.dmg` carries universal binaries:

```bash
gh release download 1.3.0-mbu --repo RandomityGuy/MBHaxe --pattern '*Mac.dmg'
hdiutil attach -nobrowse -readonly MBHaxe-Ultra-Mac.dmg
cp "/Volumes/Marble Blast Ultra/MarbleBlast Ultra.app/Contents/Frameworks"/* native/
rm native/sdl.hdll native/libhl.1.dylib     # the fixture supplies both
```

Stage `.dylib` files alongside the `.hdll` files — the HDLLs bring their own
dependencies (`libopenal`, `libpng`, `libuv`, vorbis). Any `libhl` there is skipped,
since the fixture is meant to run ash's runtime rather than upstream's.

Those binaries are staged **byte for byte** and verified, since the fixture is
only evidence if it runs what the game ships. Nothing is re-signed except what
the script builds, because re-signing changes the hash.

ash's runtime is also staged as `libhl.1.dylib`, HashLink's versioned install
name, so upstream's unmodified load commands resolve it without patching.
