# Command line

```
ash [OPTIONS] <file.hl> [PROGRAM_ARGS]...
ash run [--dir PATH]... <module.wasm> [PROGRAM_ARGS]...
ash serve [DIR] [--port N]
ash wasm [--validate] <module.wasm>
```

Everything after the `.hl` file is the program's `Sys.args()`, as with `hl`.

```bash
ash main.hl                                  # hybrid: interpret, compile hot functions
ash --mode interp main.hl                    # interpreter only
ash --build mygame main.hl                   # native executable
ash --build mygame.wasm --target wasm32-wasip1 main.hl
```

## Execution

| Option | Values | Default | |
|--------|--------|---------|---|
| `--mode` | `interp`, `hybrid`, `jit` | `hybrid` | see below |
| `--preset` | `script`, `application`, `game`, `server`, `benchmark`, `development`, `interpreter` | `application` | promotion thresholds for the program's shape |
| `--jit-threshold` | integer | set by the preset (`application`: 10) | calls before Cranelift compiles a function |
| `--opt-threshold` | integer | 10,000 | calls before LLVM recompiles it; counted on interpreted calls only |
| `--jit-tier` | `auto`, `cranelift`, `llvm`, `off` | `auto` | restrict the ladder to one compiler, or disable promotion |
| `--jit-log` | flag | | log every promotion, decline and tier transfer to stderr |
| `--quiet` | flag | | suppress everything ash prints that the program did not |
| `--no-tree-fusion` | flag | | disable confined recursive tree allocation elimination |
| `--tree-fusion-log` | flag | | log fusion candidates, refusals, helpers and budgets to stderr |
| `--no-fma` | flag | | round every float operation on its own; see below |

**`interp`** executes bytecode and compiles nothing. It is the reference
semantics: every compiled mode is checked against it, and a difference is a
bug in ASH.

**`hybrid`** starts in the interpreter and promotes functions as they get
hot. Cranelift compiles a function after `--jit-threshold` calls; LLVM
recompiles it after `--opt-threshold` interpreted calls. Compilation runs on
background threads. The new code is installed at the function's next call,
or — for a loop that never returns — at the loop header. A short program
finishes before any of this pays off; use `interp` or `--preset script`.

**`jit`** never interprets: each function is compiled by Cranelift the first
time it is reached, and the LLVM tier still takes over the hottest ones. Cold
code pays its compile time up front, so this is for benchmarks and for
isolating the interpreter from a problem, not for ordinary use.

`--jit-tier cranelift` and `--jit-tier llvm` pin one compiler; `off` keeps
the tiering machinery and disables promotion. Explicit thresholds override
the preset. `ASH_TIER` supplies `--jit-tier` when the flag is absent.

The default `application` preset compiles warm functions in Cranelift at
100 calls and reserves LLVM for sustained work at 10,000 interpreted calls
or loop back-edges. This keeps ordinary GUI interactions in the middle tier
and avoids LLVM compilation and retained metadata for short bursts of work.
A compiled caller dispatches directly without increasing its callees' counters,
so many warm functions remain in Cranelift. LLVM remains available for long
running loops; `--jit-tier cranelift` disables that tier entirely.

**Floating point.** The optimiser fuses `a * b + c` into one rounding, and
every engine — interpreter, both JIT tiers, AOT — computes the same number
for it, so promoting a function never changes a result. HashLink's JIT
rounds the multiply and the add separately; a program whose floats must
match it bit for bit (lockstep simulation against HashLink clients, a
checksum recorded from `hl`) runs with `--no-fma`, in every mode and in
`--build`. Which pairs fuse is decided per expression from the bytecode, so
the fused value is stable across ash versions but is not the one `hxcpp` or
HL/C produce; those fuse different pairs.

## Compiling

| Option | | |
|--------|---|---|
| `--build <OUT>` | path | compile and link an executable (or a `.wasm` for a wasm target) |
| `--emit-aot <OUT.o>` | path | the same compile, stopping at the object file |
| `--target <TRIPLE>` | triple | target; defaults to the host, built for this machine's CPU. Naming a target, the host's included, builds for a generic CPU (`ASH_TARGET_FEATURES` replaces its default features) |
| `--runtime <PATH>` | path | the runtime library or object to link, instead of searching |
| `--abi-version <N>` | 1 or 2 | which `libhl.N.dylib` name the program's HDLLs import (default 1) |
| `--allow-refused` | flag | emit even when a function could not be lowered; each becomes a throw |
| `--pgo[=<PROFILE>]` | path | devirtualise from a call-site profile |

Executables link on the host only; a cross build stops at `--emit-aot` and
you link. `--pgo` reads a profile written by running the program once with
`ASH_AOT_PROFILE_OUT` set; without a value it reads `<file>.prof` beside the
bytecode. Every devirtualised call is guarded, so a stale profile costs a
compare, never a wrong answer. The value must be attached with `=`.

[aot.md](aot.md) covers what a build costs, the dials, and the failure modes.

## Tools

| Option | |
|--------|---|
| `--emit-optimized <PATH>` | run the AIR optimiser over every function and write a plain `.hl` that stock `hl` or HL/C runs; ash as an optimiser rather than a runtime |
| `--hot-reload` | route direct calls through indirect dispatch so a function can be replaced at run time |
| `ash wasm <module>` | list a wasm module's functions, tables, exports and imports, grouped by who supplies them |
| `ash wasm --validate <module>` | exit non-zero and name what a host would still have to provide |

## WebAssembly

`ash --build app.wasm --target wasm32-wasip1 main.hl` links against the wasm
runtime installed beside `ash` (`wasm32-wasip1/ash_runtime.o`; the threads
target has its own directory) and writes a page beside the module:
`index.html`, `worker.js`, `thread.js`, `display.js` and the browser host
(`ash_browser.js`, `ash_browser_bg.wasm`), all built into `ash` by
`crates/ash_page`, which other tools can use as a library. An `index.html`
without ash's first-line comment is left alone.

| Command | |
|--------|---|
| `ash run <module> [args]` | run the module under wasmtime; the exit status is the program's, and a `wasm32-wasip1-threads` module runs its threads in parallel. `--dir PATH` makes a directory visible beyond the working one |
| `ash serve [dir]` | serve a directory on `127.0.0.1` (port 8731, `--port N`), every response carrying the COOP/COEP headers a threaded module needs |

`ASH_RUNTIME` names a different runtime object. [wasm/README.md](wasm/README.md)
covers threads, fibers and native libraries.

Tuning flags beyond these, environment variables and the profiler are in
[debugging.md](debugging.md).

### Native cross builds

`--target` accepts LLVM triples, Rust's `riscv64gc`/`riscv32gc` spellings, and
release names such as `linux-x86_64`, `linux-aarch64`, `macos-x86_64`,
`macos-aarch64`, `windows-x86_64` and `windows-aarch64`. An explicit target
uses a generic CPU even when it names the build machine. With no target, AOT
uses the host CPU. This policy ends when the build finishes.

A foreign executable requires an Ash runtime and system link libraries built
for its target. Install `libash_std.a` (or the target shared/import library)
in `<ash-directory>/<triple>/`, `../lib/<triple>/`, or
`$ASH_RUNTIME_DIR/<triple>/`; alternatively pass `--runtime PATH` or set
`ASH_RUNTIME_<triple_with_underscores>`. Cross builds never extract the host's
embedded runtime. Build a target runtime with Cargo and that target's C SDK:

```sh
cargo build --release -p ash_std --target x86_64-unknown-linux-gnu
ASH_LINKER_x86_64_unknown_linux_gnu=clang \
ASH_SYSROOT_x86_64_unknown_linux_gnu=/opt/sysroots/linux-x86_64 \
ash --build app-linux --target linux-x86_64 \
  --runtime /opt/ash-runtimes/x86_64-unknown-linux-gnu/libash_std.a app.hl
```

`ASH_LINKER_<triple>` takes a driver command with optional quoted arguments;
`CARGO_TARGET_<UPPERCASE_TRIPLE>_LINKER` and `CC_<triple>` also work. Global
`ASH_LINKER` and `CC` apply when no target-specific setting exists. Otherwise
Ash looks for a target GCC driver, then Clang (Clang-CL for Windows MSVC).
Clang receives `--target` and uses LLD for a cross link; Zig receives its
`cc -target` arguments. Install the matching linker alongside the driver.
`ASH_SYSROOT_<triple>` selects the target libc/SDK (`/winsysroot` for Clang-CL).
For Windows MSVC, provide the Windows SDK/UCRT and MSVC libraries through that
SDK and `LIB`, or include their search paths in `ASH_LINK_ARGS_<triple>`.
`ASH_LINK_ARGS_<triple>` appends shell-quoted arguments without invoking a
shell. The global forms of these settings also work. Missing target resources
are reported explicitly; `--emit-aot` needs neither a runtime nor a linker.

Shared-runtime builds stage the target library under its platform's HashLink
names beside the output. A Windows HDLL program must link an import library
for `libhl.dll`; the corresponding DLL must be beside that import library.
Native executable linking covers Linux, macOS and Windows targets with an
installed toolchain; object emission covers the additional architectures in
the cross-target gate. Apple targets from a non-Apple host need an Apple SDK.
