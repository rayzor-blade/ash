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
- the key window and shader exports expected by MBHaxe exist;
- `ash`, `libhl.dylib`, `sdl.hdll` and the linked SDL2 are all the host's
  architecture; and
- all source commits and artifact hashes are recorded in `PROVENANCE.txt`.

## Architecture

A Mac can carry both Homebrew prefixes at once — `/usr/local` from the Intel
era and `/opt/homebrew` for arm64 — each with its own `pkg-config` and its own
SDL2. Whichever leads `PATH` wins, so an unqualified `pkg-config sdl2` on an
arm64 host happily reports the x86_64 install, and the build then dies inside
`SDL_cpuinfo.h` dragging x86 intrinsics into an arm64 translation unit. The
script chooses SDL2 by inspecting the library with `lipo` rather than trusting
the search order, and refuses when no matching build exists.

The staged binaries are checked the same way. An architecture mismatch does
not fail at load with a useful message: dyld reports the library as simply
"not found", which reads as a missing HDLL and sends you looking somewhere
else entirely.

## Native HDLLs

The pinned build registers `hlsdl`, `datachannel` and `hlopenal`, so the
bytecode can reference natives from all three. `sdl.hdll` is always built
here; the rest come from `--native-dir`, and without them the script names
exactly which are missing rather than leaving them to surface as a load
failure at launch.

Take that directory from a shipped macOS build, not from the component
repositories. `RandomityGuy/hashlink`'s darwin release is an x86_64 nightly
from 2022, and the `hxDatachannel` release archive carries a Windows PE DLL —
neither loads on an arm64 Mac. A release `.dmg` carries universal
(`x86_64 arm64`) binaries:

```bash
gh release download 1.3.0-mbu --repo RandomityGuy/MBHaxe --pattern '*Mac.dmg'
hdiutil attach -nobrowse -readonly MBHaxe-Ultra-Mac.dmg
cp "/Volumes/Marble Blast Ultra/MarbleBlast Ultra.app/Contents/Frameworks"/* native/
rm native/sdl.hdll native/libhl.1.dylib     # this fixture supplies both
```

The HDLLs bring their own third-party dependencies (`libopenal`, `libpng`,
`libuv`, the vorbis family), so `--native-dir` stages `.dylib` files as well
as `.hdll` files. Any `libhl` there is skipped: supplying Ash's runtime
instead of upstream's is the whole point.

Those binaries are staged **byte for byte** and the script verifies it — the
fixture is only evidence if it runs what the game ships, unmodified. Nothing
is re-signed except what this script builds, since re-signing rewrites the
signature and changes the hash.

Upstream links its HDLLs against `@rpath/libhl.1.dylib`, HashLink's versioned
install name. Rather than patch them, Ash's runtime is staged under that name
too, so the shipped binaries resolve Ash by their own unmodified load
commands.

The pinned source revisions live at the top of
[`scripts/prepare_mbhaxe.sh`](../scripts/prepare_mbhaxe.sh). Update them as one
reviewed dependency set; do not update the Haxe HSDL externs independently of
the native HashLink SDL sources.
