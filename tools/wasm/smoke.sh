#!/usr/bin/env bash
# End-to-end check that the wasm toolchain works: emit -> link -> run under
# BOTH engines, and require the same answer from each. A module that behaves
# differently under wasmtime and node is the bug this catches.
set -euo pipefail
cd "$(dirname "$0")/../.."
hl="${1:-crates/ash/test/tests/bench_fib.hl}"
tmp=/tmp/ash-wasm-smoke
mkdir -p "$tmp"

./target/release/examples/wasm_spike "$hl" "$tmp/spike.o" >/dev/null
./tools/wasm/link.sh "$tmp/spike.o" "$tmp/spike.wasm"

wt=$(wasmtime --invoke add "$tmp/spike.wasm" 2 3 2>/dev/null | tail -1)
nd=$(node tools/wasm/run.mjs "$tmp/spike.wasm" add 2 3 2>/dev/null | tail -1)

echo "wasmtime add(2,3) = $wt"
echo "node     add(2,3) = $nd"
[ "$wt" = "5" ] && [ "$nd" = "5" ] || { echo "SMOKE FAILED"; exit 1; }
echo "smoke OK — both engines agree"
