<p align="center">
<img style="display: block;" src="ash.png" alt="ASH Logo" width="250"/>
</p>

<h1 align="center">ASH</h1>

<p align="center">A fast HashLink virtual machine.</p>

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

ASH runs HashLink bytecode. It is a drop-in replacement for `hl`: the same
`.hl` file produced by `haxe -hl`, the same standard library semantics, the
same `@:hlNative` HDLLs. It differs in how the code executes.

- **Tiered JIT.** Hot functions are compiled while the program runs, first by
  Cranelift (fast to compile) and then by LLVM (fast to run). A loop that is
  already executing is transferred into the compiled version without
  returning from it.
- **Native AOT.** `ash --build` produces a standalone executable: no bytecode,
  no interpreter, no warm-up.
- **WebAssembly.** The same compiler targets `wasm32-wasip1`, with a browser
  host and a native host for testing. Exceptions, threads and sockets work.
- **SIMD.** The `ash-simd` haxelib exposes 128-bit vector types that compile
  to vector instructions on ASH and run through an HDLL on stock HashLink.

ASH passes the executable part of the Haxe 4.3.6 test suite under the
interpreter, as a native executable and as a wasm module. The badges are live
from CI; the [conformance page](https://rayzor-blade.github.io/ash/#conformance)
has the per-engine breakdown and the
[benchmark page](https://rayzor-blade.github.io/ash/#benchmarks) compares
ASH with HashLink's JIT, HashLink/C and the JVM on the same programs.

## Install

```sh
curl -fsSL https://raw.githubusercontent.com/rayzor-blade/ash/main/install.sh | sh
```

```powershell
irm https://raw.githubusercontent.com/rayzor-blade/ash/main/install.ps1 | iex
```

Installs `ash` into `~/.ash/bin` and adds it to `PATH`. Prebuilt binaries:
macOS arm64 and x86_64, Linux x86_64 and aarch64, Windows x86_64. The standard library
and the wasm linker are inside the binary; nothing else is installed.
ASH requires a 64-bit target. Other platforms build from source — see
[CONTRIBUTING.md](CONTRIBUTING.md).

`ASH_DEV=1 ... | sh` installs the `-dev` build instead (Linux x86_64 and
macOS arm64): the same ash with every LLVM backend registered, so
`--target` can emit objects for architectures outside the supported set.
See [docs/aot.md](docs/aot.md#cross-compiling).

## Run

```bash
ash main.hl                     # hybrid: interpret, compile hot functions in the background
ash --mode interp main.hl       # interpreter only
ash --mode jit main.hl          # compile every function at its first call
```

Arguments after the `.hl` file go to the program, as with `hl`.

The default is `hybrid`. Functions start interpreted; call counts decide
promotion; Cranelift compiles at 100 calls and LLVM recompiles at 1000
(`--preset` picks thresholds for a script, a game or a server). Compilation
runs on background threads and the new code is installed atomically at the
next call, or mid-loop for a function that never returns. `interp` is the
reference every other mode is checked against, and the right choice for a
script that finishes before compilation would pay for itself.

[docs/cli.md](docs/cli.md) lists every option.

## Build a native executable

```bash
ash --build mygame main.hl
./mygame
```

The runtime is linked in statically. A program that loads HDLLs gets the
runtime as a shared library instead, staged beside the executable under the
names HDLLs import (`libhl.dylib`, `libhl.1.dylib`), so program and
extensions share one garbage collector. The `.hdll` files are yours to place
next to the executable.

Build time, memory, `haxe.CallStack` in compiled code, cross-compilation and
the troubleshooting table are in [docs/aot.md](docs/aot.md).

## Build for wasm

```bash
ash --build mygame.wasm --target wasm32-wasip1 main.hl
```

No external toolchain: the linker is part of `ash`. The output is a WASI
preview-1 module that exports `main` and imports the few things a sandbox
cannot do for itself — suspending a fiber, sockets. ASH ships a browser host
and a wasmtime-based one; [docs/wasm/README.md](docs/wasm/README.md) covers
embedding, threads via Workers, and native libraries as wasm side modules.

Native `.hdll` files do not load in a sandbox. A library that needs one
guards it with `#if wasm` or ships a `.wasm` side module
([docs/wasm/hdlls.md](docs/wasm/hdlls.md)).

## SIMD

```haxe
import ash.simd.Float32x4;

var acc = Float32x4.splat(0);
var i = 0;
while (i < n) {
    acc = acc + Float32x4.load(a, i << 2) * Float32x4.load(b, i << 2);
    i += 4;
}
var dot = acc.sum();
```

`-lib ash-simd` (or `-cp haxelib/ash-simd`). `Float32x4` and `Int32x4` are
abstracts over a 16-byte `hl.Bytes`; `ash.simd.Vec` is the underlying set of
memory-to-memory primitives for f32x4, f64x2, i32x4, i16x8, i8x16 and u8x16.

On stock HashLink the primitives come from `simd.hdll` and every operator
allocates its result. On ASH they are part of the runtime: the compiled tiers
emit each one as a vector instruction, and a value that does not escape the
function is kept in a register — the loop above compiles to a load, a load,
`fmul`, `fadd` with the accumulator in a phi. [docs/simd.md](docs/simd.md)
documents the API and the lane semantics.

## HDLLs

Any HDLL built for HashLink loads unchanged from the directory of the `.hl`
file (or of the executable, for an AOT build). Writing one:
[docs/hdll.md](docs/hdll.md).

## Heaps

`examples/heaps_base2d/` runs a [Heaps](https://heaps.io/) Base2D application
through HashLink's SDL3 `sdl.hdll`:

```bash
ash --mode hybrid examples/heaps_base2d/bin/game.hl
```

The [Heaps on Ash guide](https://rayzor-blade.github.io/ash/heaps.html) has
the haxelib versions, HDLL placement and Apple Silicon notes. MarbleGame
(SDL2) has its own pinned workflow in [docs/mbhaxe.md](docs/mbhaxe.md).

## Diagnostics

A result that differs between `--mode interp` and a compiled mode is a bug in
ASH. `--jit-tier cranelift|llvm|off` pins one tier so the report can name
it; `ASH_PROFILE=sample` is a built-in sampling profiler that attributes time
to Haxe functions, including JIT-compiled ones. Both are in
[docs/debugging.md](docs/debugging.md).

Bug reports and questions: [Discord](https://discord.gg/NYdr8eWxF4), or an
issue via git-bug ([CONTRIBUTING.md](CONTRIBUTING.md#issues)).

## Documentation

| | |
|---|---|
| [docs/cli.md](docs/cli.md) | command-line reference |
| [docs/aot.md](docs/aot.md) | native and wasm builds |
| [docs/debugging.md](docs/debugging.md) | tier tuning, profiling, bisecting a wrong answer |
| [docs/simd.md](docs/simd.md) | ash-simd |
| [docs/hdll.md](docs/hdll.md) | writing an HDLL |
| [docs/wasm/](docs/wasm/README.md) | hosting a wasm build |
| [docs/mbhaxe.md](docs/mbhaxe.md) | MarbleGame workflow |
| [CONTRIBUTING.md](CONTRIBUTING.md) | building from source, architecture, tests, internals |
