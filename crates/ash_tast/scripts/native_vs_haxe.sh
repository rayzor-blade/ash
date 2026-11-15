#!/bin/bash
# Export a build with the Haxe exporter and with the OCaml plugin, and check they are the
# same program (every function has the same text).
#
#   native_vs_haxe.sh [hxml args...]      run it in the project's directory
#
# HAXE must name a haxe built with the ash_tast plugin (`make plugin PLUGIN=ash_tast` in a
# Haxe 4.3.6 tree) and HAXE_STD_PATH its std. TAST_DUMP is the tast_dump binary and
# ALLOW lists functions allowed to differ, such as a build-date constant.

set -e
HAXE=${HAXE:?set HAXE to a haxe with the plugin built}
HERE=$(cd "$(dirname "$0")/.." && pwd)
DUMP=${TAST_DUMP:-tast_dump}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

"$HAXE" -cp "$HERE/haxe" --macro "ash.tast.Export.init(\"$TMP/haxe.tast\")" "$@" --no-output >"$TMP/haxe.log" 2>&1 \
  || { cat "$TMP/haxe.log"; exit 1; }
"$HAXE" -cp "$HERE/ocaml/ash_tast/hx" --macro "AshTastNative.export(\"$TMP/native.tast\")" -D ash_tast_verbose "$@" --no-output >"$TMP/native.log" 2>&1 \
  || { cat "$TMP/native.log"; exit 1; }
grep 'ash_tast\] native' "$TMP/native.log" || true

ALLOWS=()
for f in $ALLOW; do ALLOWS+=("--allow=$f"); done
"$DUMP" --against="$TMP/haxe.tast" "${ALLOWS[@]}" "$TMP/native.tast" | grep -v -E '^(valid|layer |entry )'
