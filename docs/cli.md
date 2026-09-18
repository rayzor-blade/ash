# Command line

```
ash [OPTIONS] <file.hl> [PROGRAM_ARGS]...
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
| `--jit-threshold` | integer | 100 | calls before Cranelift compiles a function |
| `--opt-threshold` | integer | 1000 | calls before LLVM recompiles it; counted on interpreted calls only |
| `--jit-tier` | `auto`, `cranelift`, `llvm`, `off` | `auto` | restrict the ladder to one compiler, or disable promotion |
| `--jit-log` | flag | | log every promotion, decline and tier transfer to stderr |
| `--quiet` | flag | | suppress everything ash prints that the program did not |

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

## Compiling

| Option | | |
|--------|---|---|
| `--build <OUT>` | path | compile and link an executable (or a `.wasm` for a wasm target) |
| `--emit-aot <OUT.o>` | path | the same compile, stopping at the object file |
| `--target <TRIPLE>` | triple | target; defaults to the host. A non-host triple is compiled for a generic CPU |
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

Tuning flags beyond these, environment variables and the profiler are in
[debugging.md](debugging.md).
