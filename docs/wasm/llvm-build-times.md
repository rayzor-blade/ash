# LLVM WASM build speed

LLVM 21.1.2 has a useful iteration setting: keep AIR and LLVM IR optimization
at level 3, and lower machine-codegen effort to 0. Ash exposes this as:

```sh
ash -O3 --codegen-opt-level 0 --build app.wasm \
  --target wasm32-wasip1-threads app.hl
```

The default remains level 3. Lower codegen effort trades artifact size and
potential runtime performance for compilation speed. The browser checks below
establish correctness, rather than equal CPU throughput.

## What the LLVM sources explain

LLVM's IR optimizer and machine code generator have separate optimization
settings. Ash previously selected aggressive machine codegen even when
`ASH_LLVM_PASSES` selected a weaker IR pipeline.

In LLVM 21, WebAssembly's register-stackification pass enables its optimizing
path at every codegen level except 0. That path requests dominator and live
interval analyses. Level 0 uses the simpler path. This explains why changing
machine codegen from 3 to 1 helped little in the isolated samples, while 0
made a substantial difference. [LLVM 21 register stackification](https://github.com/llvm/llvm-project/blob/llvmorg-21.1.2/llvm/lib/Target/WebAssembly/WebAssemblyRegStackify.cpp).

The target still runs the required control-flow, explicit-local and exception
preparation passes at level 0. LLVM 21's WebAssembly target installs the
SelectionDAG instruction selector; enabling a generic FastISel switch is not
a demonstrated alternative for this backend. [LLVM 21 WASM pipeline](https://github.com/llvm/llvm-project/blob/llvmorg-21.1.2/llvm/lib/Target/WebAssembly/WebAssemblyTargetMachine.cpp).

Emscripten also recommends lower optimization levels during development.
Its link-time guidance includes Binaryen and JavaScript processing. Ash uses
its own linker, measured around one second here; the useful change for Ash
is in LLVM codegen. [Emscripten optimization guidance](https://emscripten.org/docs/optimizing/Optimizing-Code.html).

## Isolated codegen experiment

The same optimized final shard was replayed through the installed LLVM 21.1.2
`llc`, retaining Ash's SIMD, exception, setjmp and relocation settings. The IR
bodies were identical between runs. The original dump contained personalities
on stripped declarations; those were removed for LLVM verification. Ash now
clears them when stripping a body, with a verifier regression test.

Uninstrumented runs, in order 3, 0, 0, 3; medians of two runs per setting:

| Codegen | Process wall | Process CPU | Object bytes |
|---|---:|---:|---:|
| 3 | 2.782 s | 2.635 s | 2,211,275 |
| 0 | 1.420 s | 0.997 s | 2,427,185 |

The isolated CPU reduction is about 62%; the object grows about 10%.
Separate `-time-passes` profiling attributed 1.074 seconds to optimizing
register stackification, versus 0.028 seconds on the level-0 path. Pass timers
add overhead, so their process times are not the uninstrumented comparison.

For a fresh dump, use `ASH_AOT_DUMP_IR=/tmp/llvm-profile/module.ll` on a
normal `ash -O3 --build ...` invocation. A shard can then be replayed with:

```sh
llc /tmp/llvm-profile/module.shard4.ll -O=0 -filetype=obj \
  -relocation-model=pic -mcpu=generic -mattr=+exception-handling,+simd128 \
  -wasm-enable-sjlj -wasm-enable-eh -wasm-use-legacy-eh=false \
  -exception-model=wasm -o /tmp/llvm-profile/shard4.o
```

Compare `-O=3` on the same input. Add `-time-passes` only for a separate
profiling run.

## Whole Ashui application

Local macOS arm64, commit `ff20945c`, WindowPage, five shards,
`wasm32-wasip1-threads`, fibers enabled, its native WASM side modules staged.
Both configurations use AIR O3 and LLVM IR O3, with a warm AIR cache.
After an excluded warm-up, the order was codegen 3, 0, 0, 3.

| Codegen | Build wall median | LLVM backend wall median | Final WASM bytes |
|---|---:|---:|---:|
| 3 | 34.918 s | 22.045 s | 16,202,426 |
| 0 | 23.776 s | 13.337 s | 19,742,401 |

The final module grows 21.8%. Unrelated compiler and system activity was
observed on this machine; these two-sample wall-time medians are indicative.
The excluded cold warm-up was 22.415 seconds, illustrating the variability.
The isolated CPU comparison is stronger evidence of the codegen improvement.
[Raw measurements](llvm-build-time-results.json) retain every sample.

Both final modules passed Ashui's headless Chrome smoke: codegen 3 rendered
232 frames, codegen 0 rendered 228, and both exited 0. That application is
refresh-rate bounded; these counts are not a runtime-throughput benchmark.

## CLI controls

| Option | Effect |
|---|---|
| `-O0`, `-O1`, `-O2`, `-O3` | Select LLVM IR optimization and matching codegen effort |
| `-Os`, `-Oz` | Select LLVM size pipelines and function attributes, with codegen 2 |
| `--air-opt-level 0..3` | Override AIR independently; otherwise its existing default/environment applies |
| `--codegen-opt-level 0..3` | Override the codegen part independently of `-O` |

Explicit CLI choices override the corresponding environment settings.
`ASH_AOT_CODEGEN_LEVEL` remains available for library callers and scripts.
`AotRequest` and `emit_aot` remain compatible; `emit_aot_with_options` accepts
scoped overrides. Workers inherit the resolved settings, so concurrent shards
use the same configuration. AIR cache keys continue to distinguish AIR levels.

Validation covers all six `-O` modes and two independent overrides, including
exception catches and worker configuration. The stripped-declaration and
size-versus-exception-shield verifier tests pass. LLVM and no-LLVM CLI checks,
the WASM object boundary test and `cargo clippy -p ash --all-targets -- -D warnings`
also passed.

## Further work

The instruction-only baseline gives the last shard 5,882 of 8,922 LLVM bodies.
Optimizing WASM codegen now includes a per-body cost in its partition weights;
[the NUC comparison](shard-build-times.md) measures a 9.1% backend improvement
at codegen 3. Codegen 0 retains instruction-only weights after its experiment
showed no reliable gain. Each shard still parses the entire 23 MiB input stream
before pruning its view.

Earlier elimination needs a sound root policy for reflection, function tables,
trampolines and host entry points. The existing diagnostic reachability set
omits reflection roots and cannot safely become an executable deletion list.
