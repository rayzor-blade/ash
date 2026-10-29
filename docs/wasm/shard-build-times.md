# WASM shard balancing on the NUC

Measured on 2026-10-29 using WindowPage, its unchanged bytecode and native WASM
side modules. The baseline is `a8f4140a`; the candidate adds a fixed cost of 32
instruction equivalents per WASM body to the partition weights. This accounts
for tiny generated wrappers without changing instructions or the inline-import
threshold. Contiguous function ordering is preserved.

The final policy applies that cost at machine-codegen levels 1–3. Codegen 0 and
native targets retain instruction-only weights. The codegen-0 experiment below
did not support changing its default partition.

## Setup

- Intel Core i5-1250P NUC, 12 cores / 16 threads, 6.8 GiB RAM, Linux x86-64.
- LLVM 21.1.8; Rust 1.101.0-nightly (`c36f14571`, 2026-10-01).
- Matched compiler builds: host release O3, ThinLTO, 16 codegen units. These
  memory-conscious host settings differ from Ash's normal fat-LTO release
  profile. Compare the paired builds; their absolute wall times are not stock
  release or CI results.
- WASM target `wasm32-wasip1-threads`, fibers on, **five shards** in both builds,
  AIR O3 and LLVM IR O3. Five is fixed for this comparison, rather than the
  NUC's automatic shard count of six.
- The same runtime object and blinc_abi, xgpu and xwindow side modules were used
  throughout. The input has 3,552 bytecode functions and emits 8,922 LLVM bodies.
- Both compilers and both codegen settings were warmed first. Then three
  measurements per compiler/setting ran serially, reversing order on alternate
  repetitions. No Rust builds ran during the measurements.
- Every measured run reused all 3,552 optimized AIR bodies. Lowering and AIR
  passes took zero cache-reported time. LLVM IR was still regenerated.

LLVM's system package lacked Polly archives. The matching `libpolly-21-dev`
package was extracted into the task directory, and a private llvm-config
library-directory overlay exposed it alongside the unchanged system libraries.
No system package or existing source checkout was modified.

## Results

Medians of three measured runs, excluding warm-ups:

| Machine codegen | Policy | Whole build | LLVM backend | Process CPU | WASM bytes |
|---|---|---:|---:|---:|---:|
| 3 | instruction-only | 32.010 s | 8.428 s | 55.600 s | 16,202,426 |
| 3 | plus body cost | 30.106 s | 7.662 s | 55.315 s | 16,221,893 |
| 0 | instruction-only | 29.301 s | 5.464 s | 46.526 s | 19,742,401 |
| 0 | plus body cost, rejected | 29.507 s | 5.587 s | 46.589 s | 19,763,856 |

At codegen 3, backend time falls **9.1%**, and the longest worker's median
preparation/optimization/codegen sum falls from 7.732 to 7.010 seconds. Total CPU
work is similar; the improvement comes from reducing the parallel critical path.
The final module grows **0.12%**.

Observed whole-build wall time falls 5.9%. LLVM IR time also varies between
runs, although partitioning happens afterwards: its medians are 22.360 and
21.228 seconds. Do not attribute the whole wall-time difference to balancing.
The backend ranges are narrower: 8.361–8.530 seconds for the baseline and
7.657–7.679 seconds for the candidate.

At codegen 0, the body-cost model increases median backend time 2.2%, with
overlapping sample ranges. It offers no reliable improvement, so the final
implementation keeps the original partition there.

| Shard | Baseline bodies | Weighted bodies |
|---|---:|---:|
| 0 | 900 | 1,045 |
| 1 | 1,081 | 1,135 |
| 2 | 638 | 576 |
| 3 | 421 | 1,491 |
| 4 | 5,882 | 4,675 |

Peak RSS medians at codegen 3 are 2.108 and 2.090 GiB. System-wide swap counters
show small amounts of activity in some runs; they do not identify which process
caused it. The raw data retains the counters, load averages and individual
worker timings.

## Validation

The final NUC compiler reproduces the measured weighted codegen-3 module
byte-for-byte, SHA-256
`d2fc6d2d9d5e55278bd8c42417aced1cf76abd7f6d6c1691749c6ba67ffd96bc`.
At codegen 0 it reproduces the original baseline module byte-for-byte, SHA-256
`87e171f8cdebc2f4a5ffa3b48a47eba4204640fbb92dcaf319ca054f55f3c205`.

Both NUC-generated final modules passed the Ashui Chrome smoke on the Mac:
233 frames at codegen 3 and 224 at codegen 0, both exiting 0. This checks
application behavior with fibers and side modules; the refresh-capped frame
counts are not a throughput comparison.

Non-fiber WASM checks on the NUC cover real recursive calls plus a numeric loop,
and DeltaBlue. All outputs match: `calls=39088169 numeric=9250034` and
`Checksum: 14065400`. Four measured executions per compiler/setting, after one
warm-up, show no runtime regression in these samples. At codegen 3 the recursive
and numeric kernel's median is 0.812 seconds for the baseline and 0.799 seconds
for the candidate; at codegen 0 both are 0.773 seconds. These are diagnostic
samples, rather than a general runtime-performance claim.

The final NUC compiler also passes all six `-O` modes and two independent
optimization overrides, including exception catches and worker-option
precedence. Local partition/exception/attribute unit tests, WASM object boundary
validation and CLI clippy checks pass.

[Raw measurements and validation](shard-build-time-results.json) retain the
initial experiment patch, compiler hashes, final artifact comparisons and
individual execution samples.

## Reproduce

Build baseline and candidate compilers with the same host configuration, stage
the application's native side modules in the output directory, then run:

```sh
python3 scripts/bench_wasm_compile.py window-original.hl \
  --baseline /path/to/ash-baseline --candidate /path/to/ash-candidate \
  --runtime /path/to/ash_runtime.o --out-dir /path/to/staged-app \
  --report-dir /path/to/results --shards 5 --repeat 3 --fibers
```

The script records warm-ups separately, uses independent AIR caches, preserves
logs and writes results after each successful build. The retained experiment
uses the body cost at both codegen settings; the final implementation deliberately
retains the baseline at codegen 0.

Issues: `1cf21d8` (this checkpoint), `3b7fa54` (selective body loading),
`8b12bc8` (LLVM artifact reuse), `94d819f` (early elimination),
`81d0887` (linker/fiber overhead).
