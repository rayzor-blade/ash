#!/bin/bash
# Make the Rust-internal symbols of a phone runtime archive local.
#
# libash_std.a carries its own copy of Rust's standard library. Left global,
# that copy's `rust_eh_personality` and statics collide with the copy inside any
# other Rust static library an app links (libhlwindow.a), and Apple's linker
# has no way to accept a duplicate definition. Only the C interface stays global.
#
#   localize_runtime.sh <libash_std.a> ios
#   localize_runtime.sh <libash_std.a> android <NDK llvm bin directory>
#
# Idempotent: a symbol that is already local is not listed again.
set -euo pipefail

archive=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
os=${2:?usage: localize_runtime.sh <archive> ios|android [ndk-bin]}

case "$os" in
  ios)
    nm="xcrun nm"; ar="xcrun ar"; ranlib="xcrun ranlib"; prefix=_ ;;
  android)
    bin=${3:?the NDK llvm bin directory is required for android}
    nm="$bin/llvm-nm"; ar="$bin/llvm-ar"; ranlib="$bin/llvm-ranlib"
    objcopy="$bin/llvm-objcopy"; prefix= ;;
  *) echo "unknown platform '$os'" >&2; exit 2 ;;
esac

# Mangled Rust names (legacy and v0) and the runtime shims rustc emits. Mach-O
# adds one underscore to every name.
internal="^${prefix}(_R|_ZN|rust_eh_personality\$|__rust|__rdl|__rg)"

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cd "$work"
$ar x "$archive"

members=$(ls | grep -E '^ash_std-.*\.rcgu\.o$' || true)
[ -n "$members" ] || { echo "no ash_std object in $archive" >&2; exit 1; }

for member in $members; do
  $nm -g --defined-only "$member" | awk 'NF == 3 { print $3 }' | sort -u > all.syms
  grep -E "$internal" all.syms > local.syms || true
  if [ "$os" = ios ]; then
    # nmedit keeps the listed globals and makes the rest local.
    grep -vE "$internal" all.syms > keep.syms || true
    xcrun nmedit -s keep.syms "$member"
  elif [ -s local.syms ]; then
    "$objcopy" --localize-symbols=local.syms "$member"
  fi
  echo "$member: $(wc -l < local.syms | tr -d ' ') symbols made local"
  $ar r "$archive" "$member"
done
$ranlib "$archive" 2>/dev/null
