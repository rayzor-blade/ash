#!/bin/bash
# Build a layer for the Haxe standard library (HL target) that a program compiled with
# the same defines can be exported on.
#
#   std_layer.sh OUT.tast [-D name[=value] ...] [--debug]
#
# Pass the defines the programs use that shape how the standard library types: hl-ver,
# analyzer flags, hl-check, debug. A program with other defines that the library's sources
# do not test can use the layer as well.
#
# Not every module types under every set of defines (`haxe.atomic` needs HL 1.13), so the
# script excludes, one at a time, the modules the compiler refuses, then builds the layer
# from the rest. It runs in an empty directory because `include('')` takes every module
# under the working directory too.

set -e
HAXE=${HAXE:-haxe}
HERE=$(cd "$(dirname "$0")/.." && pwd)
OUT=$1
[ -n "$OUT" ] || { echo "usage: $0 OUT.tast [-D name[=value] ...]" >&2; exit 2; }
shift
case "$OUT" in /*) ;; *) OUT="$PWD/$OUT";; esac

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
cd "$WORK"

# Packages of other targets, the HL overrides (which are used when the modules they replace
# are typed) and this exporter's own classes.
IGN=(cpp cs flash java jvm js lua neko php python eval hl._std ash.tast)
for _ in $(seq 1 60); do
  LIST=$(printf "'%s'," "${IGN[@]}"); LIST="[${LIST%,}]"
  OUTPUT=$("$HAXE" -cp "$HERE/haxe" "$@" --macro "include('', true, $LIST)" -dce no --hl unused.hl --no-output 2>&1 | sed 's/\x1b\[[0-9;]*m//g') || true
  if [ -z "$(echo "$OUTPUT" | grep -v '^[[:space:]]*$')" ]; then
    "$HAXE" -cp "$HERE/haxe" "$@" --macro "include('', true, $LIST)" \
      --macro "ash.tast.Export.init(\"$OUT\", [], [\"<std>\"])" -D ash_tast_verbose \
      -dce no --hl unused.hl --no-output
    exit 0
  fi
  # The first module the compiler names inside the standard library, as `pack.Module`.
  FILE=$(echo "$OUTPUT" | grep -oE '[^ :]*/std/[A-Za-z0-9_/]+\.hx' | head -1)
  [ -n "$FILE" ] || { echo "$OUTPUT" | head -5 >&2; exit 1; }
  MOD=$(echo "$FILE" | sed -E 's|^.*/std/||; s|^hl/_std/||; s|\.hx$||; s|/|.|g')
  # Refused again after it was excluded: the module that pulls it in is elsewhere, so
  # exclude the package.
  case " ${IGN[*]} " in *" $MOD "*) MOD="${MOD%.*}";; esac
  case " ${IGN[*]} " in *" $MOD "*) echo "cannot find a set of modules that types: $OUTPUT" >&2; exit 1;; esac
  echo "excluding $MOD" >&2
  IGN+=("$MOD")
done
echo "gave up after 60 exclusions" >&2
exit 1
