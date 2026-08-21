#!/usr/bin/env bash
# Package the release `ash` binary into dist/ash-<target>.tar.gz.
#
# The binary embeds ash_std and links LLVM statically, but on macOS a few
# Homebrew dylibs (zstd, libxml2, ...) remain dynamic and live at paths only
# a machine with Homebrew has. Those get bundled next to the binary with
# their load commands rewritten to @executable_path, then the binary is
# ad-hoc re-signed (rewriting load commands invalidates the signature).
# On Linux the equivalents are distro packages present on any desktop
# install; they stay dynamic and the installer prints the apt line if one
# is missing.
#
# Usage: scripts/package_release.sh <target-name>   e.g. macos-aarch64
set -euo pipefail

TARGET="${1:?usage: package_release.sh <target-name>}"
BIN="target/release/ash"
test -x "$BIN" || { echo "error: $BIN not built" >&2; exit 1; }

DIST="dist/ash-${TARGET}"
rm -rf "$DIST"
mkdir -p "$DIST"
cp "$BIN" "$DIST/ash"
cp LICENSE "$DIST/" 2>/dev/null || true

# Ship the runtime beside the binary even though the binary also embeds it.
#
# ash resolves ash_std in three steps: a system libhl, then a sibling next to
# the executable, then — as a last resort — writing the embedded copy to a
# fresh temp file, spawning codesign over it, and dlopening that. A release
# tarball that carries only `ash` skips straight to the last resort, so every
# run of an installed ash paid a temp write plus a codesign process before it
# executed a single opcode. Installing the sibling costs a few MB and turns
# that into a plain dlopen of an already-signed, stable path.
if [[ "$(uname -s)" == "Darwin" ]]; then
  STD_LIB="libash_std.dylib"
else
  STD_LIB="libash_std.so"
fi
STD_SRC=""
for c in "target/release/$STD_LIB" target/*/release/"$STD_LIB"; do
  if [[ -f "$c" ]]; then STD_SRC="$c"; break; fi
done
if [[ -n "$STD_SRC" ]]; then
  cp "$STD_SRC" "$DIST/$STD_LIB"
  chmod u+w "$DIST/$STD_LIB"
  echo "bundled runtime: $STD_SRC"
else
  # Not fatal — the binary still has the embedded copy and will fall back to
  # extracting it. But that is the slow path this bundling exists to avoid,
  # so it must not pass unremarked.
  echo "warning: $STD_LIB not found; installed ash will extract its runtime on every run" >&2
fi

if [[ "$(uname -s)" == "Darwin" ]]; then
  # Bundle every non-system dylib the binary references. The runtime dylib is
  # walked too: it is a separate Mach-O with its own dependency list, and
  # rewriting only the executable's would leave it pointing at Homebrew paths
  # that exist on no user's machine.
  for macho in "$DIST/ash" ${STD_SRC:+"$DIST/$STD_LIB"}; do
  otool -L "$macho" | awk 'NR>1 {print $1}' | while read -r dep; do
    case "$dep" in
      /usr/lib/*|/System/*|@*) continue ;;
    esac
    name="$(basename "$dep")"
    cp "$dep" "$DIST/$name"
    chmod u+w "$DIST/$name"
    install_name_tool -change "$dep" "@executable_path/$name" "$macho"
    install_name_tool -id "@executable_path/$name" "$DIST/$name"
    # A bundled dylib can itself reference other Homebrew dylibs.
    otool -L "$DIST/$name" | awk 'NR>1 {print $1}' | while read -r sub; do
      case "$sub" in
        /usr/lib/*|/System/*|@*) continue ;;
      esac
      subname="$(basename "$sub")"
      [[ -f "$DIST/$subname" ]] || cp "$sub" "$DIST/$subname"
      install_name_tool -change "$sub" "@executable_path/$subname" "$DIST/$name"
    done
    codesign --force -s - "$DIST/$name"
  done
  done
  # Signing comes last: rewriting load commands invalidates any signature, and
  # an unsigned dylib makes dlopen register a fresh signature in the kernel on
  # first open, which is the stall this whole path exists to avoid.
  if [[ -n "$STD_SRC" ]]; then codesign --force -s - "$DIST/$STD_LIB"; fi
  codesign --force -s - "$DIST/ash"
  echo "bundled dylibs:"
  otool -L "$DIST/ash" | sed -n '2,20p'
else
  echo "dynamic dependencies (expected to come from distro packages):"
  ldd "$DIST/ash" | grep -v "linux-vdso\|ld-linux\|libc\.\|libm\.\|libgcc\|libpthread\|libdl" || true
fi

mkdir -p dist
tar czf "dist/ash-${TARGET}.tar.gz" -C "$DIST" .
echo "wrote dist/ash-${TARGET}.tar.gz"
