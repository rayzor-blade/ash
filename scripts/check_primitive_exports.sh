#!/usr/bin/env bash
# Every `DEFINE_PRIM(_RET, name, ...)` in HashLink is a primitive some Haxe
# program can reference as `std@name`, and ash resolves it by dlsym'ing
# `hlp_<name>`. So the set of DEFINE_PRIM names is exactly the set of symbols
# we owe, and `nm` over libash_std is exactly what we provide.
#
# This is the one check that can see the failure class it exists for. A unit
# test links the rlib directly and never consults the export table, so a
# missing or misspelled `#[no_mangle]` is invisible to it -- and shows up
# instead as `natives resolved, N missing` in a user's program. That is how all
# 47 of the ones fixed in 061cd8d were found: one user, one program.
#
# Usage: check_primitive_exports.sh <path-to-hashlink-checkout>
set -euo pipefail
HL="${1:?usage: check_primitive_exports.sh <hashlink-checkout>}"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

[ -d "$HL/src" ] || { echo "no $HL/src -- pass a HashLink checkout, not its src dir" >&2; exit 2; }

# The dylib first: its symbol table is the one the runtime actually dlsyms
# against, so it is the artifact a missing #[no_mangle] shows up in. The static
# lib is only a fallback for a build that produced no cdylib.
# One `ls -t` over every pattern would re-sort them together and hand back
# whichever happens to be newest, so ask for one kind at a time and stop at the
# first that exists.
lib=""
for ext in dylib so a; do
  lib="$(ls -t "$ROOT"/target/*/libash_std.$ext "$ROOT"/target/*/*/libash_std.$ext 2>/dev/null | head -1 || true)"
  [ -n "$lib" ] && break
done
[ -n "$lib" ] || { echo "no ash_std artifact; run: cargo build -p ash_std" >&2; exit 2; }

# Apple's nm cannot decode objects from a newer LLVM than Xcode ships and exits
# nonzero, which pipefail turns into an unexplained failure. Any llvm-nm reads
# every vintage, so prefer one wherever it is found.
NM="$(command -v llvm-nm || ls /opt/homebrew/opt/llvm/bin/llvm-nm /usr/lib/llvm-*/bin/llvm-nm 2>/dev/null | head -1 || true)"
[ -n "$NM" ] || NM="$(find "$HOME/.rustup/toolchains" -name llvm-nm -type f 2>/dev/null | head -1 || true)"
[ -n "$NM" ] || NM=nm

# LC_ALL=C on every sort: `comm` silently reports garbage when its inputs were
# ordered under a different collation, and it says so only as a warning.
# -R, not -r: a checkout may reach its sources through a symlink, and the
# difference is silent -- grep just finds nothing and pipefail turns that into
# a bare exit code with no explanation.
grep -RhoE 'DEFINE_PRIM[A-Z_]*\(\s*[_A-Z0-9]+\s*,\s*[a-zA-Z0-9_]+' "$HL/src" \
  | sed -E 's/.*,[[:space:]]*//' | LC_ALL=C sort -u > /tmp/hl_prims.txt

# An empty side means the comparison is meaningless, and a gate that passes
# because it measured nothing is worse than no gate.
[ -s /tmp/hl_prims.txt ] || { echo "found no DEFINE_PRIM in $HL/src -- wrong path?" >&2; exit 2; }

# The leading underscore is Mach-O's; ELF has none. Strip either.
# Both tables, unioned. A shared object publishes its exports in the DYNAMIC
# table (-D) while an archive has only the static one (-g), and reading just
# one of the two silently reports an empty export list for the other kind.
# `|| true`: a member object with no symbols at all is normal and exits
# nonzero, which must not be mistaken for a broken artifact.
{ "$NM" -g --defined-only "$lib" 2>/dev/null || true
  "$NM" -D --defined-only "$lib" 2>/dev/null || true; } \
  | grep -oE ' T _?hlp_[a-zA-Z0-9_]+' | sed -E 's/ T _?hlp_//' \
  | LC_ALL=C sort -u > /tmp/ash_prims.txt

[ -s /tmp/ash_prims.txt ] || { echo "no hlp_* symbols in $lib (via $NM) -- stale or wrong artifact?" >&2; exit 2; }


missing="$(LC_ALL=C comm -23 /tmp/hl_prims.txt /tmp/ash_prims.txt)"
n_hl=$(wc -l < /tmp/hl_prims.txt)
n_ash=$(wc -l < /tmp/ash_prims.txt)
echo "HashLink declares $n_hl primitives; ash exports $n_ash hlp_* symbols."

if [ -n "$missing" ]; then
  echo
  echo "MISSING $(echo "$missing" | grep -c .) primitive(s) -- a program using any of these"
  echo "will fail native resolution at startup:"
  echo "$missing" | sed 's/^/  std@/'
  exit 1
fi
echo "No gap: every primitive HashLink declares is exported."
