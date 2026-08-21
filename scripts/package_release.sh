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

if [[ "$(uname -s)" == "Darwin" ]]; then
  # Bundle every non-system dylib the binary references.
  otool -L "$DIST/ash" | awk 'NR>1 {print $1}' | while read -r dep; do
    case "$dep" in
      /usr/lib/*|/System/*|@*) continue ;;
    esac
    name="$(basename "$dep")"
    cp "$dep" "$DIST/$name"
    chmod u+w "$DIST/$name"
    install_name_tool -change "$dep" "@executable_path/$name" "$DIST/ash"
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
