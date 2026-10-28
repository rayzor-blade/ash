# Ashui WASM build times and AIR reuse

Local macOS arm64 measurements on 2026-10-28. Ashui WindowPage was compiled
with its WASM flags and blinc_abi, xgpu and xwindow side modules staged.
Target: `wasm32-wasip1-threads`; fibers enabled; five LLVM shards; default
AIR and LLVM optimization levels; 512 MiB disk cache budget. Builds ran
serially after Rust compilation finished. These are individual build samples.

The source edit changes `elapsed >= QUIT_AFTER` to `elapsed > QUIT_AFTER` in
one callback. Both bytecode files are 2,149,277 bytes; three bytes differ.

## Build stages

| Run | Wall | Decode / preparation¹ | LLVM IR² | LLVM backend³ | Link | Peak RSS | Optimized hits |
|---|---:|---:|---:|---:|---:|---:|---:|
| disabled | 20.175 s | 0.019 s | 5.932 s | 12.638 s | 1.044 s | 1.50 GiB | 0 / 3,552 |
| cold | 20.526 s | 0.030 s | 8.415 s | 10.692 s | 1.028 s | 2.21 GiB | 0 / 3,552 |
| warm | 20.906 s | 0.029 s | 6.038 s | 13.486 s | 1.030 s | 1.83 GiB | 3,552 / 3,552 |
| edit | 25.326 s | 0.029 s | 8.523 s | 15.326 s | 1.148 s | 1.50 GiB | 3,551 / 3,552 |

¹ Decode/preparation is part of module setup. ² LLVM IR time includes AIR
work and its disk I/O. ³ Backend wall time includes sharding, LLVM middle-end
and code generation across workers. These nested and parallel timings do
not add to wall time. RSS covers the whole compiler, including LLVM.

| Run | AIR lowering | AIR passes | Cache reads | Cache writes | Misses | Entries written |
|---|---:|---:|---:|---:|---:|---:|
| disabled | 137.665 ms | 859.755 ms | 0.000 ms | 0.000 ms | 0 | 0 |
| cold | 98.204 ms | 1269.538 ms | 250.395 ms | 2772.753 ms | 7,104 | 7,104 |
| warm | 0.000 ms | 0.000 ms | 1241.524 ms | 0.000 ms | 0 | 0 |
| edit | 0.031 ms | 0.375 ms | 1774.570 ms | 0.773 ms | 2 | 2 |

The warm build reuses every optimized body and runs no AIR lowering or passes.
The edit reuses 3,551 bodies and writes the edited lowered/optimized pair.
Disk persistence has a cold write cost. The small per-function AIR cost in
WindowPage also makes cache I/O a material part of warm preparation.
These samples show no consistent overall wall-time speedup from disk reuse;
LLVM backend variance and work dominate the result. Reuse counts and skipped
AIR work are deterministic; an end-to-end speedup needs workload-specific
measurement.

## WASM linker

Warm build, measured independently of LLVM:

| Linker stage | Time |
|---|---:|
| Read/parse inputs | 39.657 ms |
| Resolve symbols | 50.909 ms |
| Layout/reachability | 275.180 ms |
| Relocations | 66.649 ms |
| Emit module | 44.591 ms |
| Fiber rewrite | 542.473 ms |
| Whole link call | 1029.900 ms |

LLVM is the largest remaining cost. WindowPage emits 8,922 LLVM bodies.
Each shard parses the complete 23 MiB LLVM stream; the last shard owns
5,882 bodies. The function and trampoline registries retain addresses.
AOT currently visits all bytecode functions before later LLVM/shard/linker
sweeps. The diagnostic reachability analyzer excludes reflection roots, so
its current live set cannot safely be used as a deletion list.

Before sharing AOT lookup tables, frameless answers and bounded lowered
bodies, measured LLVM IR time was 6.429 s with disk reuse disabled, 8.827 s
cold and 6.691 s warm. Sharing this state retains a 2 MiB lowered-body budget
and releases it at build exit. The separate optimized-body budget is 8 MiB.

## Validation

- Disabled, cold and warm outputs have identical SHA-256:
  `b80dbcd62ddb24253563b8b2e91a6011f38b39dde341222a243e5a99484b5793`.
- The edited output has SHA-256 `ebcccb7620ae176752a2863dee3b7a32ffafdb7b025aa70b078b4a4779c07ce7`.
- Cached and edited applications passed Ashui’s headless Chrome smoke:
  more than 230 rendered frames each and exit 0.
- Cache tests cover callee/transitive-proof edits, unrelated-function reuse,
  target/ABI/profile/options/type/global/native changes, observed integer
  values, native/WASM text-only edits, schema mismatch,
  corruption, concurrent writes, eviction, interrupted accounting recovery,
  bounded workspace ownership and coalesced LRU timestamps.
- Native and WASM CLI process tests cover cold/warm/edit output, disabling,
  clearing, corruption recovery and separate target caches. Two concurrent
  native builds also completed with correct output and zero cache errors.

Checks passed: 9 cache unit tests, 3 CLI process tests, 226 AIR tests and
40 WASM linker/fiber tests. The final disk cache contains 7,106 entries,
5,271,419 serialized bytes, including the edited pair.

## Reproduce

Compile original and edited bytecode at the same source paths and with the
same Haxe options, then stage the application’s WASM side modules in the
output directory. Run:

```sh
python3 scripts/bench_air_cache.py original.hl edited.hl \
  --ash target/release/ash --out-dir /tmp/ash-air-cache-results \
  --target wasm32-wasip1-threads \
  --runtime target/release/wasm32-wasip1-threads/ash_runtime.o \
  --wasm-fibers --keep-outputs
```

The script creates a fresh cache and preserves stage logs, output hashes
and `results.json`. It checks that disabled/cold/warm output hashes agree.
`--runtime` in this script selects `ASH_RUNTIME`, using the compiler’s
corresponding staged runtime.

Issue: `c78c3b39b7af7f1f0c20b18a6640d766a6f4522a487790bd8d11663b3b2cd050`.
