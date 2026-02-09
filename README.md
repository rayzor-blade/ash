# ASH

A HashLink Virtual Machine rewritten in Rust, targeting ARM64/Apple M-series.

ASH executes [HashLink](https://hashlink.haxe.org/) bytecode (`.hl` files) compiled from [Haxe](https://haxe.org/). It provides both a JIT compiler (via LLVM) and a bytecode interpreter, along with an optimization framework operating on an SSA-based intermediate representation.

## Features

- **JIT Compiler** &mdash; LLVM 18-based JIT compilation for ARM64 with O3 optimizations and FMA contraction
- **Bytecode Interpreter** &mdash; NaN-boxed value interpreter for development and fallback
- **AIR Optimizer** &mdash; SSA-based intermediate representation with null-check elimination, copy propagation, and dead code elimination
- **Garbage Collector** &mdash; Conservative stack-scanning GC with block-level collection
- **HDLL Support** &mdash; Load external HashLink dynamic libraries using the standard DEFINE_PRIM resolver protocol
- **Embedded Standard Library** &mdash; Rust implementation of the HashLink standard library, compiled as a cdylib and embedded into the binary

## Prerequisites

- **Rust** (stable toolchain)
- **LLVM 18** &mdash; required by the Inkwell LLVM bindings
- **Haxe** (optional) &mdash; only needed to recompile `.hx` test sources to `.hl` bytecode

### Installing LLVM 18

macOS (Homebrew):
```bash
brew install llvm@18
```

Linux (apt):
```bash
apt install llvm-18-dev
```

## Building

The default build target is `aarch64-apple-darwin`. LLVM is discovered via the `LLVM_SYS_180_PREFIX` environment variable, set in `.cargo/config.toml`:

```bash
cargo build -p ash       # JIT binary
cargo build -p ash_cli   # Interpreter/hybrid binary
```

For release builds:
```bash
make                     # Build for host target with LTO
make all                 # Build for all installed targets
```

### Build gotcha: ash_std changes

`ash_std` is a cdylib embedded via `include_bytes!` in `build.rs`. After modifying any file under `std/src/`, you must rebuild it explicitly:

```bash
cargo build -p ash_std && cargo clean -p ash && cargo build -p ash
```

## CLI

### `ash` &mdash; JIT Compiler

Compiles and executes a `.hl` file through the LLVM JIT pipeline.

```
ash [<file.hl>]
```

| Argument | Description |
|----------|-------------|
| `<file.hl>` | Path to a HashLink bytecode file. Defaults to the bundled `test.hl`. |

```bash
cargo run -p ash -- path/to/program.hl
```

### `ash_cli` &mdash; Interpreter / Hybrid Runner

Executes a `.hl` file with a choice of execution backend.

```
ash_cli [--interp | --jit] [<file.hl>]
```

| Flag | Description |
|------|-------------|
| `--interp` | Run using the bytecode interpreter |
| `--jit` | Run using the JIT compiler (currently falls back to interpreter) |
| *(none)* | Hybrid mode (defaults to interpreter) |

```bash
cargo run -p ash_cli -- --interp path/to/program.hl
```

## Crates

| Crate | Description |
|-------|-------------|
| **ash** | Core VM &mdash; bytecode decoder, LLVM JIT compiler, native library loader |
| **ash_cli** | CLI runner with interpreter/JIT mode selection |
| **ash_interp** | Bytecode interpreter with NaN-boxed values |
| **air** | ASH Intermediate Representation &mdash; CFG, dominator trees, SSA, optimization passes |
| **ash_std** | HashLink standard library implemented in Rust (cdylib, embedded into binary) |
| **ash_macro** | Procedural macros for FFI symbol loading |

## Tests

The test suite includes 10 HashLink programs covering core VM functionality:

```bash
# Run all tests through the JIT
for f in crates/ash/test/tests/*.hl; do
  cargo run -q -p ash -- "$f"
done

# Run the HDLL extern test
cargo run -p ash -- crates/ash/test/hdll/test_hdll.hl

# Run interpreter tests
cargo run -p ash_cli -- --interp crates/ash/test/tests/test_basic.hl

# Run AIR optimizer unit tests
cargo test -p air
```

| Test | Covers |
|------|--------|
| `test_basic` | Arithmetic, comparisons, loops, print |
| `test_closures` | Closure allocation and callbacks |
| `test_dynamic` | Dynamic typing and DynGet/DynSet |
| `test_exceptions` | Trap/EndTrap/Throw/Rethrow via setjmp/longjmp |
| `test_gc` | GC pressure test with 1000 allocations |
| `test_array_push` | Array creation and element access |
| `test_divtest` | Floating-point division edge cases |
| `test_mandelbrot` | Full Mandelbrot set (checksum: 112798587) |
| `test_mandelbrot_small` | Smaller Mandelbrot variant (checksum: 22825041) |
| `test_hdll` | External HDLL loading with DEFINE_PRIM |

## Writing an HDLL

External native libraries follow the standard HashLink DEFINE_PRIM protocol. Create a C file that includes `hl.h`:

```c
#define HL_NAME(n) mylib_##n
#include "hl.h"

HL_PRIM int HL_NAME(add)(int a, int b) {
    return a + b;
}
DEFINE_PRIM(_I32, add, _I32 _I32);
```

Compile as a shared library and place the `.hdll` next to your `.hl` file:

```bash
cc -shared -o mylib.hdll mylib.c -I/path/to/ash/std
```

In Haxe, declare the native binding:

```haxe
@:hlNative("mylib", "add")
static function nativeAdd(a:Int, b:Int):Int { return 0; }
```

ASH discovers `.hdll` files automatically from the same directory as the `.hl` bytecode file.

## Optimization Levels

The AIR pass manager supports three levels, selectable at compile time:

| Level | Passes |
|-------|--------|
| **O0** | No optimization |
| **O1** | Null-check elimination, copy propagation, dead register elimination |
| **O2** | O1 passes + SSA construction, global copy propagation, SSA dead code elimination |

The JIT compiler currently uses **O2** by default.
