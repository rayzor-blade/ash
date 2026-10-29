# Pocket Studio WASM build investigation

Measured on the NUC on 2026-10-29 using `RayDE/site/demo`. Removing an AOT/JIT
policy mix-up reduces the median WASM build from **20.53 s to 6.98 s**: **66%
less time**, or **2.9× faster**. The application and optimization settings are
the same in both builds.

## Workload and method

The current demo uses published Ashui, ashui-components and ashui-icons 0.2.2,
with `-dce full`. Its old `bin/web` output names 0.1.5, so it was not used.
The source was copied into a temporary directory and rebuilt with the project's
settings plus `-D wasm` and RayDE's `preview/haxe` filesystem adapter, matching
`RunPlan.compilerArgs`. No demo source or existing output was modified.

The original bytecode is 2,138,419 bytes, SHA-256
`3f7903172336a0e49ceae291c4104982dec9f7050ac60bbd26ebde4014322e72`.
It contains 2,699 bytecode functions. The edit case changes `BoardStore.add`'s
initial `id` from 1 to 2 in the temporary copy. The two bytecode files differ
by one byte and have the same length; the primary before/after comparison
always uses the original input.

The NUC has an Intel i5-1250P, 12 cores / 16 threads and 6.8 GiB RAM. Both
compilers use LLVM 21.1.8 and the same host release O3/ThinLTO/16-codegen-unit
configuration. These absolute times are specific to that host build profile,
rather than stock fat-LTO releases or CI runners. The baseline is `7e105d9e`;
the candidate bypasses the JIT promotion budget in AOT.

WASM settings match RayDE's preview defaults: AIR O3, LLVM IR O3, machine
codegen 0, `wasm32-wasip1-threads`, fibers enabled and six shards. The same runtime
object and published blinc_abi, xgpu, xwindow and ash_channel side modules are
staged throughout. Haxe compilation, RayDE input fingerprinting and bundle-cache
publication are outside the measured Ash invocation.

Each compiler is warmed first, then three measurements run serially in reversed
order on alternate repetitions. No Rust builds run on the NUC during them.
All measured builds hit all 2,699 optimized AIR bodies, with zero AIR lowering,
passes, misses or writes. Separate phase-profile runs provide attribution.

## The cause

`promote_function_strict` was applying the JIT's shared-module cap to AOT.
The default cap is 256 bodies. Each promotion counted the growing module's
function bodies, then limited pending compilation and replaced pending
declarations with private `$via_table` trampolines. AOT later compiled the real
bodies anyway. This combines repeated whole-module scans with unnecessary
indirection and extra definitions.

The baseline phase profile attributes **13.98 s** to `llvm promote`, of which
**13.08 s** is self time. Actual `AIR v2 -> LLVM` scopes total only **0.90 s**.
It records **2,747 table trampolines** and **7,160 LLVM bodies**.

As a diagnostic, `ASH_PROMOTE_MODULE_CAP=off` reduces IR time to 0.95 s and
the whole build to 6.68 s. The fix makes AOT use an uncapped pending queue while
preserving the JIT's existing budget. Its module has **4,413 bodies**, exactly
2,747 fewer, with no table-trampoline counter. Its output matches the cap-off
diagnostic byte-for-byte. All bytecode bodies are still compiled.

## Repeated comparison

Medians of three warm measurements per compiler:

| Stage / metric | Before | Fixed |
|---|---:|---:|
| Whole build | 20.525 s | 6.984 s |
| LLVM IR stage | 14.125 s | 0.933 s |
| LLVM backend | 5.067 s | 4.922 s |
| Link | 0.933 s | 0.899 s |
| Process CPU | 41.235 s | 26.813 s |
| Peak RSS | 2.244 GiB | 2.201 GiB |
| WASM bytes | 18,621,509 | 18,580,879 |

The backend includes bitcode preparation, parallel middle-end optimization,
machine code generation and the data object. Nested/parallel stage timings
are not additive. Whole-build samples are 20.176–21.331 s before and
6.834–7.186 s after the fix. The change saves CPU work as well as wall time.

## Cold, warm and edited builds

These are individual samples, separate from the repeated comparison:

| AIR cache case | Before wall | Fixed wall | Fixed IR | Fixed backend | Optimized hits, fixed |
|---|---:|---:|---:|---:|---:|
| Disabled | 20.118 s | 7.283 s | 1.446 s | 4.859 s | 0 |
| Cold | 21.425 s | 7.685 s | 1.935 s | 4.613 s | 0 |
| Warm | 19.978 s | 7.187 s | 0.938 s | 5.143 s | 2,699 |
| One-function edit | 19.876 s | 7.235 s | 0.940 s | 5.175 s | 2,697 |

Disabled/cold/warm outputs agree within each compiler. The edited build writes
three cache entries: one lowered body and two optimized bodies. Persistent AIR
reuse works; the original delay was outside the work that cache skips.

## What remains

The fixed build now spends about **70% in the LLVM backend**, **13% constructing
IR**, and **13% linking**. On the representative middle measurement, the slowest
worker spends 1.253 s preparing its module, 2.702 s in the O3 middle end and
0.382 s in machine codegen. At codegen 0, further machine-codegen tuning is a
smaller target than the middle end and eager whole-stream parsing.

Linking spends roughly 0.61 s in the fiber rewrite, 0.09 s in layout and
reachability, and tens of milliseconds in each remaining stage. The existing
selective-loading and LLVM-artifact-reuse issues remain relevant, with this
corrected baseline. Earlier large IR-stage measurements included the JIT policy
overhead; they should not be treated as unavoidable LLVM IR emission cost.

## Validation

- Native and WASM regression tests vary JIT caps between 1, 256 and `off`,
  exercise virtual-method closures, a getter and exception handling, and print
  the expected 50. WASM artifacts are identical across caps. Both NUC and Mac pass.
- All 13 native AOT smoke programs match their JIT output on the Mac.
- The original, fixed and edited NUC modules run in Chrome with the preview
  filesystem channel connected to the demo's unmodified task-data fixture.
  All three remain running, show the eight-task board and produce identical
  screenshots, SHA-256
  `32162fded7eaa567ac4741f2b579e07822c3c7baad7363806b185b25642e6bfb`.
- CLI clippy checks pass. The Mac release binary is rebuilt with the fix.

The fixed original module is SHA-256
`ed67b601aa40208952a41dd39a236a1cd79d99cb36110f295477a3a37daa3ef4`;
the edited module is
`365de14f658196f5c991b54de3b4bbb0b3a7b4278cd4644d163c89f1ae3f4f7b`.
[Raw measurements](rayde-demo-build-time-results.json) retain the input/build
configuration, compiler identities, individual samples and phase profiles.

## Reproduce

Generate original/edited bytecode at the same source paths, preserving the
demo's Haxelib versions and full DCE. Add the WASM define and RayDE preview
classpath as described above. Stage the four native WASM side modules beside
the output, then use:

```sh
python3 scripts/bench_wasm_compile.py original.hl \
  --baseline /path/to/baseline-ash --candidate /path/to/fixed-ash \
  --runtime /path/to/ash_runtime.o --out-dir /path/to/staged-app \
  --report-dir /path/to/comparison --shards 6 --repeat 3 \
  --codegen-level 0 --fibers

python3 scripts/bench_air_cache.py original.hl edited.hl \
  --ash /path/to/fixed-ash --out-dir /path/to/staged-app \
  --target wasm32-wasip1-threads --runtime /path/to/ash_runtime.o \
  --wasm-fibers --shards 6 --opt-level 3 --codegen-opt-level 0 --keep-outputs
```

For attribution, repeat a warm build with `ASH_PROFILE=phases`. On the baseline,
`ASH_PROMOTE_MODULE_CAP=off` isolates the AOT/JIT policy error. Profiling runs
are excluded from the comparison medians.

Tracked in git-bug `ada9c02`; related follow-ups are `8b12bc8` and `3b7fa54`.
