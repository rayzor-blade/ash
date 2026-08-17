<p align="center">
<img style="display: block;" src="ash.png" alt="ASH Logo" width="250"/>
</p>

<h1 align="center">ASH</h1>

<p align="center">A fast HashLink virtual machine written in Rust.</p>

ASH executes [HashLink](https://hashlink.haxe.org/) bytecode (`.hl` files) compiled from [Haxe](https://haxe.org/). Execution is tiered: a bytecode interpreter runs everything, hot functions are promoted to Cranelift-compiled code, and the hottest are recompiled by LLVM.

## Execution tiers

Promotion is brokered by [beadie](https://github.com/darmie/beadie), which counts invocations, compiles on background threads, and publishes code pointers atomically.

| Tier | Engine | Role |
|------|--------|------|
| 0 | Bytecode interpreter | Runs everything; NaN-boxed values |
| 1 | Cranelift (`opt_level=speed`) | Fast compilation (~0.04 ms/function) for warm functions |
| 2 | LLVM 21 (MCJIT) | Full codegen for the hottest functions |

A function that Cranelift cannot lower — anything containing `Trap`/`EndTrap`, or the object-model opcodes — falls through to LLVM rather than being excluded from compilation. `ash_cli --jit-tier` pins a single rung for testing.

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
cargo build -p ash        # JIT binary
cargo build -p ash_cli    # interpreter / hybrid runner
```

Release builds use `make` (host target with LTO) or `make all` (every installed target).

### Rebuilding after `std/` changes

`ash_std` is a cdylib embedded via `include_bytes!` in `build.rs`, so a stale copy will be linked unless it is rebuilt and the embedding crate is cleaned:

```bash
cargo build -p ash_std
cargo clean -p ash
cargo build -p ash -p ash_cli
```

`build.rs` reads the cdylib from `target/<triple>/debug/`. If the toolchain writes it to `target/debug/` instead — which happens when no explicit target is configured — copy it across before rebuilding `ash`.

## CLI

### `ash_cli` — interpreter and tiered runner

```
ash_cli [OPTIONS] [<file.hl>]
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

```bash
cargo run -p ash_cli -- --mode hybrid path/to/program.hl
cargo run -p ash_cli -- --mode hybrid --jit-tier cranelift --jit-log program.hl
```

### `ash` — whole-program JIT

Compiles every function through LLVM before execution.

```bash
cargo run -p ash -- path/to/program.hl
```

## Environment variables

| Variable | Effect |
|----------|--------|
| `ASH_TIER` | Same values as `--jit-tier`; used when the flag is absent |
| `ASH_TIER_LOG` | Log promotions, declines and tier crossings |
| `ASH_TIERED_TIMING` | Break down tiered JIT startup cost by phase |
| `ASH_GC_STATS` | Print collection count, reclaimed blocks, live bytes and pause times |
| `ASH_GC_HEAP_MB` | Heap reservation size (demand-committed, so this is a ceiling) |
| `ASH_GC_TRIGGER_MB` | Floor for the adaptive collection threshold |
| `ASH_GC_STRESS` | Collect every N allocations — torture mode for root correctness |
| `ASH_CRASH_BACKTRACE` | Capture a backtrace in the crash handler (best-effort; allocates in a signal handler) |
| `ASH_JIT_NATIVE_TRAPS` | Compile unresolved natives to call-time traps instead of failing the function |
| `ASH_LIBHL` | `system` or `embedded` — override stdlib selection |

## Crates

| Crate | Description |
|-------|-------------|
| **ash** | Core VM — bytecode decoder, LLVM and Cranelift backends, native library loading, symbol table |
| **ash_cli** | Runner with mode and tier selection |
| **ash_interp** | Bytecode interpreter with NaN-boxed values, and the promotion hot path |
| **air** | Intermediate representation — CFG, dominators, loops, SSA, and the optimization passes |
| **ash_std** | HashLink standard library in Rust (cdylib, embedded into the binary) |
| **ash_sdl** | SDL/GL bindings exposed as an HDLL, for Heaps.io and other windowed programs |
| **ash_macro** | Procedural macros for FFI symbol loading |

## Tests

```bash
# One program through the interpreter
cargo run -p ash_cli -- --mode interp crates/ash/test/tests/test_basic.hl

# The same program with promotion enabled
cargo run -p ash_cli -- --mode hybrid --jit-threshold 1 crates/ash/test/tests/test_basic.hl

# Every program through the whole-program JIT
for f in crates/ash/test/tests/*.hl; do cargo run -q -p ash -- "$f"; done

# IR and pass unit tests
cargo test -p air

# Interpreter/hybrid parity against a Haxe oracle
cargo test -p ash_cli --test stdlib_matrix
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

`examples/heaps_base2d/` runs a [Heaps](https://heaps.io/) Base2D application — window creation, GL context, shader compilation, the render loop and input events — through `ash_sdl` in place of the C `sdl.hdll`:

```bash
cargo run -p ash_cli -- --mode hybrid examples/heaps_base2d/bin/game.hl
```

## Status

Known gaps, open defects and planned work are tracked in [BACKLOG.md](BACKLOG.md).
