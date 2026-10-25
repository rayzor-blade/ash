#!/bin/sh
# Build the ring-echo example into examples/ring-echo/out:
#
#     examples/ring-echo/build.sh [path to ash]
#
# Then `ash serve examples/ring-echo/out` and open http://localhost:8731/, or
# run `node examples/ring-echo/run-chrome.mjs` for the headless Chrome check.
set -e
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
ash=${1:-"$root/target/release/ash"}
haxe=${HAXE:-haxe}
out="$here/out"

sh "$root/scripts/build_wasm_channel.sh" "$root/target/wasm-channel"
rm -rf "$out"
mkdir -p "$out"
"$haxe" -cp "$here" -cp "$root/haxelib/ash-channel" -cp "$root/haxelib/ash-future" \
  -main "${MAIN:-Echo}" -hl "$out/echo.hl"
# A side module beside the output is found and listed by --build.
cp "$root/target/wasm-channel/ash_channel.wasm" "$out/"
"$ash" --build "$out/echo.wasm" --target wasm32-wasip1-threads "$out/echo.hl"
cp "$root/target/wasm-channel/channel-agent.mjs" "$here/ring_echo.mjs" "$out/"
echo "built $out"
