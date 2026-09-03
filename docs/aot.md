# Ahead-of-time compilation

`ash --build` compiles a whole HashLink program to a native binary: no
bytecode file, no interpreter, no JIT, no warmup. It starts at full speed and
stays there.

The cost is on the build side. Compiling a large program is a real compile, not
a JIT promotion, and this page is mostly about what that costs and how to
control it.

## Quick start

```
ash --build prog prog.hl
./prog
```

That is the whole build. `ash` emits the code, links it, and finds the runtime
itself: beside the `ash` binary first, then the usual library directories, then
`--runtime <path>` or `ASH_RUNTIME` if you want to name one.

`--emit-aot prog.o` stops at the object file instead, for a caller that does
its own linking.

## Programs that load HDLLs

A program that loads an HDLL — SDL, fmt, ssl, openal — must share one runtime
with it. Two copies of the runtime in one process means two garbage collectors,
and they crash as soon as one meets the other's objects, so such a binary takes
the runtime as a shared library rather than linking it in.

The same command handles it. `ash` notices the program loads HDLLs, links the
shared runtime, and stages it beside the binary under **both** names an HDLL
may import: upstream HDLLs link the versioned `libhl.1.dylib`, ash's own
`sdl.hdll` links the bare `libhl.dylib`.

```
ash --build game game.hl
```

The one thing left to you is the `.hdll` files. An AOT binary looks in its own
directory first, then the working directory, and nowhere else, so an HDLL one
directory up is an HDLL that does not exist.

If no runtime is installed anywhere, `ash` writes out the copy it carries
inside itself, so this works on a machine that has never built the workspace.

Conversely, linking a program that loads no HDLL against the shared runtime
fails at link time with `fixup error ... does not have address`. That is
expected, not a regression: such a program is emitted in static mode and
references the runtime directly.

## How long it takes, and how much memory

The emitter splits the work across threads. Each shard keeps a slice of the
program's functions, reduces the rest to declarations, and runs the optimizer
and code generator on what it kept; the emitter itself writes the object that
holds the program's data. Building a binary hands all of those objects to the
linker at once; `--emit-aot` joins them into the single object it promised.

Measured on a game of 8,577 functions, on a ten-core Apple M-series machine:

| shards | wall clock | peak memory |
|---|---|---|
| 1 | 350 s | 2.5 GB |
| 2 | 57 s | 3.0 GB |
| 4 | 42 s | 4.3 GB |
| 8 | 45 s | 4.9 GB |

Two things in that table are worth understanding, because they decide how you
should set it.

**The jump from one shard to two is not parallelism.** One shard is the
single-module path, where the optimizer sees all 9,021 bodies at once, and its
cost grows faster than linearly with module size. Splitting the module in two
does not halve the work, it cuts it by six, because each half is a much easier
problem. Everything past two shards is ordinary parallelism, and it flattens
quickly: every shard pays a fixed cost to read the program before it can
discard the parts it does not own.

**Memory scales with shard count**, for the same reason: each shard holds its
own copy of the program while it decides what to keep. Peak is reached while
those copies exist, so freeing memory afterwards does not lower it.

So the dial is simple, and the default already sits at the knee: half the
machine's cores, never fewer than two or more than six. Set `ASH_AOT_SHARDS=2`
on a memory-constrained machine, `ASH_AOT_SHARDS=1` for the smallest possible
footprint if you can wait, and leave it alone otherwise.

## Knobs

| variable | effect |
|---|---|
| `ASH_AOT_SHARDS=N` | how many shards. `1` is the single-module path, and the first thing to try if a sharded build misbehaves. |
| `ASH_AOT_NO_OPT=1` | skip the optimizer. Fast builds and readable stack traces, much slower code. |
| `ASH_AOT_DUMP_IR=<path>` | write the LLVM IR. A directory takes `module.ll`; anything else is the file. Each shard's own IR lands beside it. |
| `ASH_LLVM_PASSES=<spec>` | replace the optimizer pipeline, e.g. `default<O2>`. `off` skips it. |
| `ASH_AIR_LEVEL=0..3` | how hard the AIR optimizer works before LLVM sees anything. |
| `ASH_AIR_NO_WIDEN=1` | turn off loop widening, the one AIR pass that rewrites arithmetic. |
| `ASH_MIDDLE_END_LOG=1` | report how many functions were shielded from the optimizer. |

Command-line flags that matter here:

- `--runtime <path>` names the runtime to link against, instead of searching.
- `--target <triple>` cross-compiles. A non-native target is built for a
  generic CPU, because the host's instruction set cannot be assumed of a
  machine we cannot ask, and it always uses one shard: the shards are joined by
  the host's `ld`, which reads one object format only.
- `--allow-refused` emits even when some functions could not be lowered. Each
  refused function becomes a throw. Without it, a refusal stops the build
  rather than writing a binary that dies when it reaches one.
- `--pgo=<profile>` devirtualizes from a call-site profile, which you produce
  by running the program once with `ASH_AOT_PROFILE_OUT` set. Every guard it
  emits re-checks its target at run time, so a stale profile costs a compare
  and never a wrong answer. The `=` is required.

## WebAssembly

```sh
ash --build game.wasm --target wasm32-wasip1 game.hl
```

Same command, three different things to find, and each reports itself by
name when it is missing.

| what | where ash looks | how to say it yourself |
|---|---|---|
| the linker | beside `ash`, then `wasm-ld` on PATH, then the LLD in a Rust toolchain if there is one | `ASH_WASM_LD` |
| a libc | `share/wasi-sysroot` under every prefix on your PATH, then the wasi-sdk's default | `ASH_WASM_SYSROOT`, or the wasi-sdk's own `WASI_SDK_PATH` |
| the ash runtime | `libash_std.a` in a directory named for the triple beside `ash`, then the usual library directories | `--runtime`, or `ASH_RUNTIME` |

Two notes on how that list is built, because both were mistakes first.

**Nothing is hardcoded to a machine.** The sysroot is found by reading the
prefixes off your own PATH -- whoever installed a wasi libc installed it under
some prefix, and that prefix is on PATH because that is what installing a
toolchain means. Naming a package manager's directories instead would be right
on one machine and wrong on the rest, and a path baked in when `ash` was
*compiled* says nothing about where `ash` is *running*.

**A linker that exists is not a linker that works.** Each candidate is run
before it is used. This is not defensive habit: a standalone `wasm-ld` from
one LLVM release, resolving at load time against another release's `libLLVM`,
aborts in dyld with a missing symbol. Ash reports that by name and moves to
the next candidate.

Two things are worth knowing before the first build.

**The sysroot must have `libsetjmp.a`.** An ash program's exception handling
is `setjmp`, so a libc without it links until the first `try`. Ash says which
libc it found and what it lacks, rather than using it silently.

**The result is a library, not a command.** It exports `main` and
`ash_module_init` and imports what only a host can answer: WASI, plus fiber
suspension, plus sockets. A wasm module cannot switch its own stacks -- the
call frames are the engine's, not the program's -- so an ash program running
there is suspended by whoever embeds it. Whatever embeds the module supplies
those; the import contract is written down in
[`wasm-target.md`](wasm-target.md).

Build the runtime for the target the same way as any other:

```sh
cargo build --release -p ash_std --target wasm32-wasip1
```

## Reading the emitter's output

```
[aot] lowering 17279ms total; slowest:
[aot] sharding: 9021 bodies, 4 shards, shielded 51, stream 61 MB, prepared in 2388ms
[aot] shard 0: kept 2828, carried 3031, stripped 3162, swept 58406, folded 30169, declared 57996; prepare 2982ms, middle end 6656ms, codegen 7639ms
[aot] data object: 9021 bodies stripped; prepare 547ms, codegen 369ms
```

- **lowering** is ash's own work, turning bytecode into LLVM IR. It is
  single-threaded and it is the floor on build time.
- **shielded** counts functions excluded from the optimizer because they catch
  exceptions. A trap is a `setjmp`, and a local promoted out of memory has an
  indeterminate value after the jump.
- **kept** is the bodies this shard emits; **carried** are small bodies it may
  inline but will not emit; **stripped** are the ones it reduced to
  declarations; **swept** counts everything it then deleted as unreachable;
  **folded** and **declared** are the data it can still read and the data it
  can only reference.
- **prepare** is dominated by reading the stream, which every shard does in
  full whatever it keeps, and is why more shards stop helping.
- **data object** is written by the emitter itself, from the module it already
  has, so no shard pays to read the program's data a second time.

## Exception stacks, and why not to strip

`haxe.CallStack` works in an AOT binary. Every function keeps its frame
pointer, the object registers a table of body addresses and Haxe names at
startup, and the runtime walks the frame chain and resolves each address
through that table.

Do not strip the binary's symbols. The table is the primary source, so a
stripped binary still produces stacks, but `dladdr` is consulted to keep
runtime frames out of the trace, and without symbols that filter goes blind.

Optimized builds produce shorter stacks than unoptimized ones, because inlining
really does remove the frames. That is what a native toolchain does too.

## Troubleshooting

| symptom | cause | fix |
|---|---|---|
| `fixup error ... does not have address` at link | a static-mode object linked against the shared runtime | link the static `libash_std.a`, or emit a program that actually loads HDLLs |
| `Native library 'ssl' not loaded` at startup | the `.hdll` is not beside the binary or in the working directory | copy the HDLLs next to the binary |
| `no LC_RPATH's found` when an HDLL loads | the binary has no rpath for its own directory | build with `--build`, which adds it |
| garbage-collector crash soon after an HDLL loads | two runtimes in one process | link the shared runtime and stage `libhl` beside the binary |
| `ld -r failed` | shards emitted for a foreign object format | this is guarded now; if you see it, set `ASH_AOT_SHARDS=1` and report it |
| the build is killed, or the machine swaps | too many shards for the available memory | lower `ASH_AOT_SHARDS` |
| a wrong answer only in the AOT binary | an optimizer or lowering bug | bisect with `ASH_AOT_SHARDS=1`, then `ASH_AIR_NO_WIDEN=1`, then `ASH_LLVM_PASSES=off`, then `ASH_AOT_NO_OPT=1` |

## What an AOT binary does not do

It has no tiers, no on-stack replacement and no hot reload: everything is
compiled once, ahead of time, and nothing recompiles at run time. The
call-site profile is advisory rather than a specialization contract. There is
no bytecode in the binary, so nothing can be reloaded into it.

`cargo test -p ash --test aot_smoke` compiles a corpus of test programs with
`--build` and compares each binary's output against the JIT's, byte for
byte. Run it after any change to the emitter.
