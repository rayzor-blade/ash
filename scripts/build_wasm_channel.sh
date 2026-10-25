#!/bin/sh
# Build ash_channel.wasm, the side module behind haxelib/ash-channel.
#
#     scripts/build_wasm_channel.sh [output directory]
#
# Writes ash_channel.wasm and the page agent (channel-agent.mjs) to the output
# directory, target/wasm-channel by default.
#
# This is a threaded, position-independent side module, which is not what
# scripts/build_wasm_hdll.py builds: the channel lives in shared memory, so the
# module has to be built for wasm32-wasip1-threads with atomics, and linked
# --shared-memory. The standard library is rebuilt position-independent
# (nightly, rust-src), and every primitive is named as an export because
# --gc-sections would otherwise remove all of them: nothing inside the module
# calls them, the program does, by name, after loading it.
set -e

here=$(cd "$(dirname "$0")/.." && pwd)
nightly=${RUST_NIGHTLY:-nightly}
cd "$here"
out=${1:-"$here/target/wasm-channel"}
mkdir -p "$out"
rm -f "$out/ash_channel.wasm"

CARGO_PROFILE_RELEASE_LTO=false \
RUSTFLAGS="-C relocation-model=pic -C target-feature=+mutable-globals -C panic=abort" \
  cargo +"$nightly" rustc -p ash_hdll_channel --lib --crate-type staticlib \
    --target wasm32-wasip1-threads --release \
    -Z build-std=std,panic_abort

lld=$(find "$(rustc +"$nightly" --print sysroot)/lib/rustlib" -name rust-lld -type f | head -1)
[ -n "$lld" ] || { echo "no rust-lld in the $nightly sysroot" >&2; exit 1; }

# One resolver export per primitive the Haxe side binds.
exports=$(sed -n 's/.*@:hlNative("ash_channel", "\([^"]*\)").*/--export=hlp_\1/p' \
  "$here"/haxelib/ash-channel/ash/channel/Channel.hx | sort -u)
[ -n "$exports" ] || { echo "no ash_channel primitives found in Channel.hx" >&2; exit 1; }

# shellcheck disable=SC2086
"$lld" -flavor wasm \
  --experimental-pic -shared --no-entry \
  --unresolved-symbols=import-dynamic --gc-sections --no-export-dynamic \
  --shared-memory --max-memory=1073741824 \
  $exports \
  --whole-archive "$here/target/wasm32-wasip1-threads/release/libash_hdll_channel.a" --no-whole-archive \
  -o "$out/ash_channel.wasm"

cp "$here/haxelib/ash-channel/js/channel-agent.mjs" "$out/channel-agent.mjs"
echo "wrote $out/ash_channel.wasm ($(wc -c < "$out/ash_channel.wasm" | tr -d ' ') bytes) and channel-agent.mjs"
