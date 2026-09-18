# MBHaxe on ash

[MBHaxe](https://github.com/RandomityGuy/MBHaxe) (Marble Blast) is an
SDL2-based game with its own forks of HashLink's SDL bindings. It runs on
ash through a staged fixture that the script builds; do not assemble it by
hand.

```bash
cargo build --release --bin ash
scripts/prepare_mbhaxe.sh --native-dir /path/to/mbhaxe/native-hdlls
cd target/mbhaxe/run
./ash --mode interp marblegame.hl
```

The script recreates `target/mbhaxe/run` on every run, records source commits
and artifact hashes in `PROVENANCE.txt`, and stops unless:

- `sdl.hdll` was built from the pinned sources (no `ash_sdl` or
  `target/*/deps/libsdl` provenance);
- it links SDL2, not SDL3;
- it resolves `@rpath/libhl.dylib` beside the staged `ash`;
- MBHaxe's window and shader exports are present;
- `ash`, `libhl.dylib`, `sdl.hdll` and SDL2 are all the host architecture.

## SDL

The script builds `sdl.hdll` itself from `RandomityGuy/hashlink:libs/sdl`
and the matching Haxe externs. Any `sdl.hdll` in `--native-dir` is ignored.
The decommissioned `ash_sdl` crate is not ABI-compatible with those
bindings. Pinned revisions are at the top of
[`scripts/prepare_mbhaxe.sh`](../scripts/prepare_mbhaxe.sh); the externs and
the native sources move together, so update them as one set.

On a Mac with both Homebrew prefixes (`/usr/local` for Intel,
`/opt/homebrew` for arm64), `pkg-config sdl2` reports whichever leads
`PATH`, and an x86_64 SDL2 breaks the arm64 build inside `SDL_cpuinfo.h`.
The script selects SDL2 with `lipo` rather than trusting `PATH`, and checks
the staged binaries the same way: dyld reports an architecture mismatch as
"not found", which looks like a missing HDLL.

## Other HDLLs

`--native-dir` supplies `fmt`, `ui`, `uv`, `openal` and `datachannel`. The
pinned build registers `hlsdl`, `datachannel` and `hlopenal`; missing ones
are named before anything is staged.

Take the directory from a shipped macOS build, not from the component
repositories — `RandomityGuy/hashlink`'s darwin release is a 2022 x86_64
nightly and the `hxDatachannel` archive is a Windows DLL. A release `.dmg`
carries universal binaries:

```bash
gh release download 1.3.0-mbu --repo RandomityGuy/MBHaxe --pattern '*Mac.dmg'
hdiutil attach -nobrowse -readonly MBHaxe-Ultra-Mac.dmg
cp "/Volumes/Marble Blast Ultra/MarbleBlast Ultra.app/Contents/Frameworks"/* native/
rm native/sdl.hdll native/libhl.1.dylib     # the fixture supplies both
```

Stage the `.dylib` dependencies (`libopenal`, `libpng`, `libuv`, vorbis)
alongside the `.hdll` files. Any `libhl` there is skipped: the fixture runs
ash's runtime, staged as `libhl.1.dylib` so upstream's load commands resolve
it unmodified. Shipped binaries are staged byte for byte and verified; only
what the script builds is re-signed, because re-signing changes the hash.
