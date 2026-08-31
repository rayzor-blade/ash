#!/usr/bin/env bash
# Link an object emitted by ash into a standalone native binary.
#
# There is no C compiler in this path and no C source. `clang` is used purely
# as the driver that knows where the platform's crt startup files and libc
# live; `cc`, `gcc` or a bare `ld` invocation with the same inputs would do.
# What it links is one object from ash's own LLVM backend plus `libash_std.a`,
# the runtime compiled as a Rust staticlib.
#
# The object defines exactly two global symbols -- `main` and
# `ash_module_init` -- and imports the `hlp_*`/`hl_*` runtime entry points plus
# `_setjmp`. Every bytecode body is internal: a Haxe method named `write` would
# otherwise take libc's symbol, and the runtime's own print would jump into
# Haxe code.
set -euo pipefail
obj="${1:?usage: link.sh <in.o> [out] [libash_std.a]}"
out="${2:-${obj%.o}}"
runtime="${3:-}"

# Newest wins rather than a fixed profile order: the object and the runtime
# have to agree about the runtime's own symbols, and a stale release build
# sitting beside a fresh debug one fails the link on whatever was added last.
if [ -z "$runtime" ]; then
  root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
  runtime="$(ls -t "$root"/target/*/libash_std.a "$root"/target/*/*/libash_std.a \
             2>/dev/null | head -1 || true)"
fi
[ -n "$runtime" ] || { echo "no libash_std.a; run: cargo build -p ash_std" >&2; exit 1; }

case "$(uname -s)" in
  Darwin) libs=(-framework CoreFoundation -framework Security -liconv -lm) ;;
  *)      libs=(-lpthread -ldl -lm) ;;
esac

clang "$obj" "$runtime" -o "$out" "${libs[@]}"
echo "linked $out ($(stat -f%z "$out" 2>/dev/null || stat -c%s "$out") bytes) against $runtime"
