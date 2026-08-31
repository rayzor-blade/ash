#!/usr/bin/env bash
# Link a wasm object emitted by ash into a runnable module.
#
# LLVM's WebAssembly backend emits a relocatable object that imports
# `env.__linear_memory`; wasm-ld turns that into a module owning its own
# memory. `--no-entry` because an HL module's entrypoint is a findex we call
# explicitly, not a C `_start`. `--allow-undefined` lets the host ABI arrive
# as imports rather than link errors — see tools/wasm/host.mjs.
set -euo pipefail
obj="${1:?usage: link.sh <in.o> [out.wasm]}"
out="${2:-${obj%.o}.wasm}"
# Homebrew's wasm-ld is lld 20 resolving against LLVM 21's libLLVM and aborts
# with a missing ELFAttributeParser symbol. The Rust toolchain ships its own
# self-contained lld, which is version-matched to itself; prefer it.
lld="$(find "$HOME/.rustup/toolchains" -name rust-lld -type f 2>/dev/null | head -1)"
if [ -n "$lld" ]; then
  "$lld" -flavor wasm --no-entry --export-dynamic --allow-undefined \
         --initial-memory=$((16 * 1024 * 1024)) "$obj" -o "$out"
else
  wasm-ld --no-entry --export-dynamic --allow-undefined \
          --initial-memory=$((16 * 1024 * 1024)) "$obj" -o "$out"
fi
echo "linked $out ($(stat -f%z "$out" 2>/dev/null || stat -c%s "$out") bytes)"
