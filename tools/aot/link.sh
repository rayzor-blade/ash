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

# An HDLL imports the runtime by its HashLink name -- @rpath/libhl.dylib on
# Darwin, libhl.so elsewhere -- so a binary that loads one needs an rpath
# pointing at its own directory, exactly as the `ash` executable does. Without
# it dlopen refuses the HDLL with "no LC_RPATH's found", which reads as the
# HDLL being missing when it is sitting right there. Programs with no HDLL are
# unaffected: an rpath nobody consults costs nothing.
case "$(uname -s)" in
  Darwin) libs=(-framework CoreFoundation -framework Security -liconv -lm
                -Wl,-rpath,@executable_path -Wl,-rpath,@loader_path) ;;
  *)      libs=(-lpthread -ldl -lm
                -Wl,-rpath,'$ORIGIN' -Wl,--export-dynamic) ;;
esac

# Any C driver will do -- it is here for the crt files and the libc search
# path, not to compile anything. $CC first so a cross build can say so.
for candidate in "${CC:-}" cc clang gcc; do
  [ -n "$candidate" ] && command -v "$candidate" >/dev/null 2>&1 && driver="$candidate" && break
done
[ -n "${driver:-}" ] || { echo "no C driver found (tried \$CC, cc, clang, gcc)" >&2; exit 1; }

"$driver" "$obj" "$runtime" -o "$out" "${libs[@]}"

# When the runtime is a shared library, the binary must name it the way an
# HDLL does. A staged copy keeps the install name of whatever it was copied
# from -- an absolute path into the build tree -- so the binary would load THAT
# image while the HDLL loads @rpath/libhl.dylib, and the process ends up with
# two runtimes and two collectors. That crashes the moment one meets the
# other's objects.
case "$runtime" in
  *.dylib)
    if [ "$(uname -s)" = "Darwin" ]; then
      recorded="$(otool -L "$out" | awk 'NR==2{print $1}')"
      case "$recorded" in
        @rpath/libhl.dylib) ;;
        *) install_name_tool -change "$recorded" "@rpath/libhl.dylib" "$out" ;;
      esac
    fi
    ;;
esac
echo "linked $out ($(stat -f%z "$out" 2>/dev/null || stat -c%s "$out") bytes) against $runtime"
