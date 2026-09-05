<p align="center">
<img style="display: block;" src="ash.png" alt="ASH Logo" width="250"/>
</p>

<h1 align="center">ASH</h1>

<p align="center">A fast HashLink virtual machine written in Rust.</p>

<p align="center">
  <a href="https://github.com/rayzor-blade/ash/actions/workflows/lint.yml"><img src="https://github.com/rayzor-blade/ash/actions/workflows/lint.yml/badge.svg" alt="lint"></a>
  <a href="https://github.com/rayzor-blade/ash/actions/workflows/parity_gate.yml"><img src="https://github.com/rayzor-blade/ash/actions/workflows/parity_gate.yml/badge.svg" alt="parity gate"></a>
  <a href="https://github.com/rayzor-blade/ash/actions/workflows/conformance.yml"><img src="https://github.com/rayzor-blade/ash/actions/workflows/conformance.yml/badge.svg" alt="haxe conformance"></a>
  <a href="https://github.com/rayzor-blade/ash/actions/workflows/bench.yml"><img src="https://github.com/rayzor-blade/ash/actions/workflows/bench.yml/badge.svg" alt="benchmarks"></a>
  <br>
  <a href="https://rayzor-blade.github.io/ash/#conformance"><img src="https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Frayzor-blade.github.io%2Fash%2Fbench%2Fconformance.json&query=%24.summary.engines.interp.case_pct&suffix=%25&label=Haxe%20suite%20%C2%B7%20interpreter&color=2ea44f" alt="Haxe conformance, interpreter"></a>
  <a href="https://rayzor-blade.github.io/ash/#conformance"><img src="https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Frayzor-blade.github.io%2Fash%2Fbench%2Fconformance.json&query=%24.summary.engines.aot.case_pct&suffix=%25&label=Haxe%20suite%20%C2%B7%20AOT&color=2ea44f" alt="Haxe conformance, native AOT"></a>
  <a href="https://rayzor-blade.github.io/ash/#conformance"><img src="https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Frayzor-blade.github.io%2Fash%2Fbench%2Fconformance.json&query=%24.summary.engines.wasm.case_pct&suffix=%25&label=Haxe%20suite%20%C2%B7%20wasm32&color=2ea44f" alt="Haxe conformance, wasm32"></a>
  <a href="https://rayzor-blade.github.io/ash/#benchmarks"><img src="https://img.shields.io/badge/benchmarks-published-a8703c" alt="benchmarks"></a>
  <a href="https://discord.gg/NYdr8eWxF4"><img src="https://img.shields.io/badge/Discord-join-5865F2?logo=discord&logoColor=white" alt="Discord"></a>
</p>

ASH executes [HashLink](https://hashlink.haxe.org/) bytecode (`.hl` files) compiled from [Haxe](https://haxe.org/). Execution is tiered: a bytecode interpreter runs everything, hot functions are promoted to Cranelift-compiled code, and the hottest are recompiled by LLVM.

Against the Haxe 4.3.6 compiler test suite the interpreter passes **1069 of 1069 attemptable cases** — 4/4 suites, 1372/1372 tests, 12199/12199 assertions. The same suite is measured under the two other ways ash runs a program: as a native ahead-of-time binary (`ash --build`) and as a wasm32 module (`ash --build --target wasm32-wasip1`, under `ash-wasm-run`); the badges above are those three figures, live from CI. The suite holds 1195 cases in total; the remaining 126 have nothing to run on the `hl` target (other targets' project directories, and suites that test the compiler rather than a program — null-safety diagnostics, source maps, the compilation server). Live numbers, including the per-engine breakdown, are on the [conformance page](https://rayzor-blade.github.io/ash/#conformance).

## Execution tiers

Promotion is brokered by [beadie](https://github.com/darmie/beadie), which counts invocations, compiles on background threads, and publishes code pointers atomically.

| Tier | Engine | Role |
|------|--------|------|
| 0 | Bytecode interpreter | Runs everything; NaN-boxed values |
| 1 | Cranelift (`opt_level=speed`) | Fast compilation (~0.04 ms/function) for warm functions |
| 2 | LLVM 21 (MCJIT) | Full codegen for the hottest functions |

A function that Cranelift cannot lower — anything containing `Trap`/`EndTrap`, or the object-model opcodes — falls through to LLVM rather than being excluded from compilation. `--jit-tier` pins a single rung for testing.

## Features

- **Tiered execution** — interpreter → Cranelift → LLVM, with per-tier promotion counts reported at exit
- **AIR** — typed phi-SSA intermediate representation over HashLink bytecode, with first-class trap regions, an effect lattice, and an alias model
- **Optimization passes** — null-check elimination, GVN/CSE, LICM, FMA fusion, dead-code elimination, tail-recursion elimination, inlining, and scalar replacement of aggregates
- **Garbage collector** — conservative stack-scanning Immix with a demand-committed heap, adaptive collection triggers, external-pressure accounting, and optional statistics
- **Fibers** — `sys.thread` threads run as cooperative stackful fibers, so Haxe code that blocks on locks, deques or `Sys.sleep` makes progress on a single OS thread
- **Shared symbol table** — one canonical `lib@symbol` → address map built at startup and consumed by the interpreter and both compiled tiers
- **HDLL support** — external HashLink dynamic libraries via the standard `DEFINE_PRIM` resolver protocol
- **Embedded standard library** — the HashLink standard library implemented in Rust, built as a cdylib and embedded in the binary

## Platforms

| Architecture | Status |
|--------------|--------|
| `aarch64` | Complete; primary development and test target |
| `x86_64` | Complete, including the SysV and Windows fiber ABIs |
| others | Needs the two assembly components below |

ASH requires a 64-bit target: `HL_WSIZE` is 8, and NaN-boxed values pack a 48-bit payload into a `u64`.

Code generation itself is architecture-independent — AIR, the lowerings, and both backends work on any target LLVM and Cranelift support. What does not follow automatically is two pieces of assembly at the runtime boundary:

- **The reflection call bridge** (`ash_static_call`) marshals arguments into registers to invoke a function pointer whose signature is only known at runtime, for `Type.createInstance` and dynamic dispatch. There is no portable fallback, so an unported architecture fails to link.
- **The fiber context switch**, which backs `sys.thread`, lives in [krio](https://github.com/darmie/krio). Unported architectures compile against a stub that panics when a thread is created.

Native crash recovery already has a portable fallback; only the register dump in its report is specific to macOS on `aarch64`.

macOS is where ASH is developed and tested. Linux and Windows have code paths throughout, and `make all` builds every target `rustup` has installed, but neither is exercised regularly yet.

## Prerequisites

- **Rust nightly** — `ash_std` uses unstable features
- **LLVM 21** — required by the Inkwell bindings
- **Haxe** (optional) — only to recompile `.hx` sources to `.hl` bytecode

`llvm-sys` finds LLVM through `LLVM_SYS_211_PREFIX`, or `llvm-config` on `PATH`:

```bash
brew install llvm                                   # macOS
export LLVM_SYS_211_PREFIX=/opt/homebrew/opt/llvm

apt install llvm-21-dev                             # Debian/Ubuntu
export LLVM_SYS_211_PREFIX=/usr/lib/llvm-21
```

## Building

```bash
cargo build -p ash        # the `ash` binary: interpreter, JIT and AOT compiler
```

Release builds use `make` (host target with LTO) or `make all` (every installed target).

### Rebuilding after `std/` changes

`ash_std` is a cdylib embedded into `ash_core` via `include_bytes!`, and
nothing in cargo's dependency graph records that — the embedding crate does not
link it. So it must be built first, by hand, and `ash` must be cleaned to make
its build script run again:

```bash
cargo build -p ash_std
cargo clean -p ash_core
cargo build -p ash
```

**Release builds need the same two steps**, and the ordering matters more:

```bash
cargo build --release -p ash_std
cargo build --release -p ash
```

`build.rs` prefers a cdylib matching the profile being built and falls back to
the debug one, because `ash_std` may only have been built in debug. That
fallback is load-bearing but easy to get wrong: the embedded runtime is
everything compiled code calls into — the collector, field access, strings — so
a release binary that falls back runs its entire runtime at the dev profile's
`opt-level` while looking optimized. The build script prints a `cargo:warning`
when it happens.

## CLI

### `ash` — the one binary

```
ash [OPTIONS] [<file.hl>] [PROGRAM_ARGS]...
```

| Option | Values | Description |
|--------|--------|-------------|
| `--mode` | `interp`, `hybrid`, `jit` | Execution mode (default: `interp`) |
| `--jit-tier` | `auto`, `cranelift`, `llvm`, `off` | Which rungs of the ladder to use (default: `auto`) |
| `--jit-threshold` | integer | Calls before a function is promoted (default: 100) |
| `--jit-min-ops` | integer | Minimum opcode count before promotion (0 disables the gate) |
| `--jit-max-args` | integer | Maximum argument count for promoted calls (default: 8) |
| `--jit-log` | flag | Log every promotion, decline and tier crossing |
| `--hot-reload` | flag | Route direct calls through indirect dispatch so code can be swapped |
| `--quiet` | flag | Suppress non-program output |
| `--build` | path | Compile the program to a native binary: emit and link in one step. See [docs/aot.md](docs/aot.md) |
| `--emit-aot` | path | The same compile, stopping at the object file |
| `--runtime` | path | Runtime to link against, instead of searching (with `--build`) |
| `--target` | triple | Target to compile for; defaults to this machine |
| `--allow-refused` | flag | Emit even when a function could not be lowered; each becomes a throw |
| `--pgo[=<profile>]` | path | Devirtualise from a call-site profile produced by `ASH_AOT_PROFILE_OUT` |
| `--emit-optimized` | path | Run the AIR pipeline and write ordinary bytecode, then exit |

```bash
cargo run -p ash -- --mode hybrid path/to/program.hl
cargo run -p ash -- --mode hybrid --jit-tier cranelift --jit-log program.hl
```

### Compiling ahead of time

`--build` compiles the whole program and links it into a native binary, which
needs no bytecode, no interpreter and no JIT at run time.

```bash
ash --build myprogram myprogram.hl
./myprogram
```

`--emit-aot` stops at the object file, and `--target` compiles for another
machine. Executables are host-only, because linking one needs that platform's
linker; a cross build asks for the object. [docs/aot.md](docs/aot.md) covers
the runtime, the shard dial and the failure modes.

## Environment variables

| Variable | Effect |
|----------|--------|
| `ASH_TIER` | Same values as `--jit-tier`; used when the flag is absent |
| `ASH_TIER_LOG` | Log promotions, declines and tier crossings |
| `ASH_TIERED_TIMING` | Break down tiered JIT startup cost by phase |
| `ASH_PROFILE` | `phases`, `sample` or `all` — see [Profiling](#profiling) |
| `ASH_PROFILE_HZ` | Sampling rate, default 997 |
| `ASH_PROFILE_OUT` | Write the profile to a file instead of stderr |
| `ASH_GC_STATS` | Print collection count, reclaimed blocks, live bytes and pause times |
| `ASH_GC_HEAP_MB` | Heap reservation size (demand-committed, so this is a ceiling) |
| `ASH_GC_TRIGGER_MB` | Floor for the adaptive collection threshold |
| `ASH_GC_STRESS` | Collect every N allocations — torture mode for root correctness |
| `ASH_CRASH_BACKTRACE` | Capture a backtrace in the crash handler (best-effort; allocates in a signal handler) |
| `ASH_JIT_NATIVE_TRAPS` | Compile unresolved natives to call-time traps instead of failing the function |
| `ASH_LIBHL` | `system` or `embedded` — override stdlib selection |

## Profiling

`ASH_PROFILE` turns on a built-in profiler that works in every mode and on both
binaries. It answers two separate questions:

```bash
ASH_PROFILE=phases ash --mode hybrid program.hl   # where startup and compilation go
ASH_PROFILE=sample ash --mode hybrid program.hl   # where the running program goes
ASH_PROFILE=all    ash --mode hybrid program.hl   # both
```

**Phases** are nested named regions — decode, native resolution, each tier's
lowering and codegen, execution — reported as a tree with total and self time,
so a phase that is slow because of one child reads differently from one that is
slow on its own. Background compilation appears under its own thread rather
than nested inside whatever the main thread was doing.

**Samples** come from interrupting the running thread and recording the program
counter. Each one is classified, which is the point: it separates time spent in
*generated code* from time the generated code hands back to the runtime.

| Bucket | Meaning |
|--------|---------|
| `llvm` / `cranelift` | Inside code a JIT tier emitted |
| `interp` | Inside the bytecode interpreter |
| `runtime` | An `hlp_*` helper |
| `gc` | Allocation and collection |
| `native` | Any other resolved symbol |
| `unknown` | Anonymous memory with no registered code range |

Compiled functions register their entry points with the profiler as they are
installed, so JIT frames resolve to Haxe function names — an external profiler
sees only anonymous `mmap` memory there.

Samples are attributed to a bucket by their own address, and a bare libc leaf
is re-attributed to its caller, so the collector's `madvise` and lock traffic
is charged to `gc` rather than to `native`. Because standard signals do not
queue, a thread that is starved of CPU loses ticks instead of banking them; the
report compares CPU against wall time and says so when that happens, since
timings taken on a busy machine otherwise look like ordinary results.

## Crates

| Crate | Description |
|-------|-------------|
| **ash** | Core VM — bytecode decoder, LLVM and Cranelift backends, native library loading, symbol table |
| **ash** (`crates/ash_cli`) | The binary: runner, tier selection and AOT compiler |
| **ash_interp** | Bytecode interpreter with NaN-boxed values, and the promotion hot path |
| **air** | Intermediate representation — CFG, dominators, loops, SSA, and the optimization passes |
| **ash_std** | HashLink standard library in Rust (cdylib, embedded into the binary) |
| **ash_macro** | Procedural macros for FFI symbol loading |

## Tests

```bash
# One program through the interpreter
cargo run -p ash -- --mode interp crates/ash/test/tests/test_basic.hl

# The same program with promotion enabled
cargo run -p ash -- --mode hybrid --jit-threshold 1 crates/ash/test/tests/test_basic.hl

# Every program through the whole-program JIT
for f in crates/ash/test/tests/*.hl; do cargo run -q -p ash -- "$f"; done

# IR and pass unit tests
cargo test -p air

# Interpreter/hybrid parity against a Haxe oracle
cargo test -p ash --test stdlib_matrix
```

| Test | Covers |
|------|--------|
| `test_basic` | Arithmetic, comparisons, loops, print |
| `test_closures` | Closure allocation and callbacks |
| `test_dynamic` | Dynamic typing, `DynGet`/`DynSet` |
| `test_exceptions` | `Trap`/`EndTrap`/`Throw`/`Rethrow` via setjmp/longjmp |
| `test_gc` | Allocation pressure |
| `test_array_push` | Array creation and element access |
| `test_divtest` | Floating-point division edge cases |
| `test_mandelbrot` | Full Mandelbrot set |
| `test_mandelbrot_small` | Smaller Mandelbrot variant |
| `test_tiered_hotloop` | Hot loop that exercises promotion |
| `test_stdlib` | Strings, arrays, maps, enums, closures, exceptions, math, JSON |
| `test_std_reflect_type` | Reflection and the `Type` API |
| `test_hdll` | External HDLL loading via `DEFINE_PRIM` |

### Floating-point checksums

The Mandelbrot tests are sensitive to floating-point contraction, which makes their checksum a useful signal about *which* engine ran the hot function:

| | 298² | 875×500 |
|---|---|---|
| Unfused (separate multiply and add) | 22816350 | 112790102 |
| Fused (single-rounding `fma`) | 22825041 | 112798515 |

The interpreter rounds every opcode separately and so produces the unfused values; the fused values match `clang -ffp-contract=on`, hxcpp and hxjava. `crates/ash/test/tests/Mandelbrot_reference.c` is the C program those numbers come from.

## Writing an HDLL

External native libraries use the standard `DEFINE_PRIM` protocol:

```c
#define HL_NAME(n) mylib_##n
#include "hl.h"

HL_PRIM int HL_NAME(add)(int a, int b) {
    return a + b;
}
DEFINE_PRIM(_I32, add, _I32 _I32);
```

```bash
cc -shared -o mylib.hdll mylib.c -I/path/to/ash/std
```

```haxe
@:hlNative("mylib", "add")
static function nativeAdd(a:Int, b:Int):Int { return 0; }
```

ASH discovers `.hdll` files in the same directory as the `.hl` file.

## Heaps.io

`examples/heaps_base2d/` runs a [Heaps](https://heaps.io/) Base2D application — window creation, GL context, shader compilation, the render loop and input events — through a relocatable macOS arm64 build of HashLink's SDL3 `sdl.hdll`:

```bash
cargo run -p ash -- --mode hybrid examples/heaps_base2d/bin/game.hl
```

See the [Heaps on Ash guide](https://rayzor-blade.github.io/ash/heaps.html) for matching haxelib versions, HDLL placement, Apple Silicon setup, and troubleshooting.

For RandomityGuy's SDL2-based MarbleGame, use the isolated
[MBHaxe workflow](docs/mbhaxe.md). It pins the game's dependency forks and
rejects any `sdl.hdll` produced by Ash's decommissioned Rust SDL shim.

## Status

Known gaps, open defects and planned work are tracked in [BACKLOG.md](BACKLOG.md).
