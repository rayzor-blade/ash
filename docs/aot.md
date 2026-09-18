# Ahead-of-time compilation

`ash --build` compiles a HashLink program to a native executable or a wasm
module. The result contains no bytecode and no interpreter; nothing compiles
at run time.

The cost moves to the build: a large program is a real optimising compile,
not a JIT promotion. Most of this page is about that cost and its dials.

## Native

```
ash --build prog prog.hl
./prog
```

`ash` emits the code, links it and finds the runtime on its own: beside the
`ash` binary first, then the usual library directories. `--runtime <path>` or
`ASH_RUNTIME` names one explicitly. `--emit-aot prog.o` stops at the object
file for a caller that links itself.

### Programs that load HDLLs

An HDLL and the program must share one runtime. Two copies in one process
means two garbage collectors, and they crash as soon as one meets the other's
objects. `ash` detects that the program loads HDLLs, links the runtime as a
shared library, and stages it beside the executable under both names an HDLL
may import: `libhl.1.dylib` (HashLink 1.x builds) and the bare
`libhl.dylib`. `--abi-version 2` stages `libhl.2.dylib` for HDLLs built
against HashLink 2.

You supply the `.hdll` files. An AOT binary looks for them in its own
directory, then the working directory, and nowhere else.

If no runtime is installed anywhere, `ash` writes out the copy it carries
inside itself, so a machine that has never built the workspace still works.

The reverse case fails deliberately: linking a program that loads no HDLL
against the shared runtime stops with `fixup error ... does not have
address`. Such a program is emitted in static mode and references the
runtime directly.

### Build time and memory

The emitter shards the program across threads. Each shard keeps a slice of
the functions, reduces the rest to declarations, and runs LLVM's optimiser
and code generator on its slice; the emitter writes the object holding the
program's data itself. `--build` hands every object to the linker at once;
`--emit-aot` joins them into the single object it promised.

An 8,577-function game on a ten-core Apple M-series machine:

| shards | wall clock | peak memory |
|---|---|---|
| 1 | 350 s | 2.5 GB |
| 2 | 57 s | 3.0 GB |
| 4 | 42 s | 4.3 GB |
| 8 | 45 s | 4.9 GB |

Two effects decide the setting. One shard is the single-module path, where
the optimiser sees all 9,021 bodies at once and its cost grows faster than
linearly in module size; splitting in two cuts the work by six, not two.
Beyond two shards the gain is ordinary parallelism, and it flattens because
every shard reads the whole program before discarding what it does not own.
Memory scales with shard count for the same reason.

The default is half the machine's cores, clamped to [2, 6]. `ASH_AOT_SHARDS=2`
on a memory-constrained machine; `1` for the smallest footprint.

### Dials

| variable | effect |
|---|---|
| `ASH_AOT_SHARDS=N` | shard count. `1` is the single-module path, and the first thing to try when a sharded build misbehaves |
| `ASH_AOT_NO_OPT=1` | skip the optimiser: fast builds, readable stacks, much slower code |
| `ASH_AOT_DUMP_IR=<path>` | write the LLVM IR. A directory takes `module.ll`; anything else is the file. Each shard's IR lands beside it |
| `ASH_LLVM_PASSES=<spec>` | replace the optimiser pipeline (`default<O2>`, …); `off` skips it |
| `ASH_AIR_LEVEL=0..3` | how hard the AIR optimiser works before LLVM sees the code |
| `ASH_AIR_NO_WIDEN=1` | turn off loop widening, the one AIR pass that rewrites arithmetic |
| `ASH_MIDDLE_END_LOG=1` | report how many functions were shielded from the optimiser |
| `ASH_AOT_NO_STATIC_DEVIRT=1` | do not guess method-call targets from the classes the program instantiates; only a `--pgo` profile devirtualises. Safe; slower on monomorphic dispatch |

Flags:

- `--target <triple>` cross-compiles. A non-host target is built for a
  generic CPU and always with one shard, because shards are joined by the
  host's `ld`, which reads one object format.
- `--allow-refused` emits even when some functions could not be lowered; each
  becomes a throw at the point it is reached. Without it a refusal stops the
  build.
- `--pgo=<profile>` devirtualises from a call-site profile recorded by running
  the program once with `ASH_AOT_PROFILE_OUT` set. Every guard re-checks its
  target, so a stale profile costs a compare. The `=` is required.

### Reading the emitter's output

```
[aot] lowering 17279ms total; slowest:
[aot] sharding: 9021 bodies, 4 shards, shielded 51, stream 61 MB, prepared in 2388ms
[aot] shard 0: kept 2828, carried 3031, stripped 3162, swept 58406, folded 30169, declared 57996; prepare 2982ms, middle end 6656ms, codegen 7639ms
[aot] data object: 9021 bodies stripped; prepare 547ms, codegen 369ms
```

- **lowering** — bytecode to LLVM IR, ash's own work, single-threaded. The
  floor on build time.
- **shielded** — functions kept out of the optimiser because they catch
  exceptions. A trap is a `setjmp`, and a local promoted out of memory has an
  indeterminate value after the jump.
- **kept** — bodies this shard emits; **carried** — small bodies it may
  inline but will not emit; **stripped** — reduced to declarations;
  **swept** — deleted as unreachable; **folded** / **declared** — data it can
  read versus data it can only reference.
- **prepare** — dominated by reading the stream, which every shard does in
  full; the reason more shards stop helping.

### Exception stacks

`haxe.CallStack` works in an AOT binary. Every function keeps its frame
pointer, the object registers a table of body addresses and Haxe names at
startup, and the runtime walks the frame chain through that table.

Do not strip the binary. The table is the primary source, so stacks survive
stripping, but `dladdr` is consulted to keep runtime frames out of the trace
and without symbols that filter goes blind. Optimised builds show fewer
frames than unoptimised ones; inlining removed them.

### Troubleshooting

| symptom | cause | fix |
|---|---|---|
| `fixup error ... does not have address` at link | a static-mode object linked against the shared runtime | link the static `libash_std.a`, or emit a program that actually loads HDLLs |
| `Native library 'ssl' not loaded` at startup | the `.hdll` is not beside the binary or in the working directory | copy the HDLLs next to the binary |
| `no LC_RPATH's found` when an HDLL loads | the binary has no rpath for its own directory | build with `--build`, which adds it |
| GC crash shortly after an HDLL loads | two runtimes in one process | link the shared runtime and stage `libhl` beside the binary |
| `ld -r failed` | shards emitted for a foreign object format | guarded now; if it appears, set `ASH_AOT_SHARDS=1` and report it |
| build killed, machine swapping | too many shards for the memory | lower `ASH_AOT_SHARDS` |
| a wrong answer only in the AOT binary | an optimiser or lowering bug | bisect: `ASH_AOT_SHARDS=1`, then `ASH_AIR_NO_WIDEN=1`, then `ASH_LLVM_PASSES=off`, then `ASH_AOT_NO_OPT=1` |

### What an AOT binary does not do

No tiers, no on-stack replacement, no hot reload. The call-site profile is
advisory. There is no bytecode in the binary, so nothing can be reloaded
into it.

`cargo test -p ash --test aot_smoke` compiles a corpus with `--build` and
compares each binary's output with the JIT's, byte for byte.

## WebAssembly

```sh
ash --build game.wasm --target wasm32-wasip1 game.hl
```

No external toolchain. The linker is `crates/ash_wasm_link`, and the libc it
links against was joined into the runtime object when ash was built; the
command above works with `PATH` unset. The one thing it looks for is that
runtime object, `ash_runtime.o`, in a directory named for the target beside
`ash`, then the usual library directories; `--runtime` or `ASH_RUNTIME` names
it.

**The result is a library, not a command.** It exports `main` and
`ash_module_init` and imports what only a host can answer: WASI, fiber
suspension, sockets. A wasm module cannot switch its own stacks, so an ash
program is suspended by whoever embeds it. [wasm/README.md](wasm/README.md)
has the hosts and the import contract.

**Only reachable code is emitted.** Functions nothing calls are dropped —
about a third of a module. Reachability is generous: all data is kept, so any
function whose address is written anywhere survives, because compiled code
reaches most of the runtime through tables built in data. Debug sections are
dropped.

Building the runtime object for the target, when working from source:

```sh
cargo build --release -p ash_std --target wasm32-wasip1
scripts/build_wasm_runtime.py
```
