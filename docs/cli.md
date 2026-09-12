# CLI

One binary runs programs and compiles them.

```
ash [OPTIONS] [<file.hl>] [PROGRAM_ARGS]...
```

```bash
ash program.hl                              # interpret
ash --mode hybrid program.hl                # interpret, promote hot functions
ash --build myprogram program.hl            # compile to a native binary
ash --build game.wasm --target wasm32-wasip1 game.hl
```

Anything after the `.hl` file is passed to the program.

## Running

| Option | Values | Description |
|--------|--------|-------------|
| `--mode` | `interp`, `hybrid`, `jit` | Execution mode (default: `interp`) |
| `--quiet` | flag | Suppress non-program output |

`interp` runs everything in the bytecode interpreter. `hybrid` adds tiered
promotion, which is what a long-running program wants. `jit` is the same
ladder with no interpreter: every function is compiled at its first call.

## Compiling

`--build` produces a native binary that needs no bytecode, no interpreter and
no JIT at run time.

```bash
ash --build myprogram myprogram.hl
./myprogram
```

| Option | Values | Description |
|--------|--------|-------------|
| `--build` | path | Compile and link in one step |
| `--emit-aot` | path | The same compile, stopping at the object file |
| `--target` | triple | Target to compile for; defaults to this machine |
| `--runtime` | path | Runtime to link against instead of searching |
| `--pgo[=<profile>]` | path | Devirtualise from a call-site profile |

Executables are host-only, because linking one needs that platform's linker —
a cross build asks for the object with `--emit-aot`.

`--pgo` takes a profile produced by running the program once with
`ASH_AOT_PROFILE_OUT` set. Every guard it emits re-checks its target at run
time, so a stale profile costs a compare and never a wrong answer. The `=` is
required.

[aot.md](aot.md) covers the runtime, the shard dial and the failure modes.

## Reading a wasm module

```bash
ash wasm prog.wasm             # functions, tables, exports, imports
ash wasm --validate prog.wasm  # exit non-zero and name what is missing
```

---

Flags for tuning promotion, tracing what the JIT did, and profiling a run are
in [debugging.md](debugging.md).
