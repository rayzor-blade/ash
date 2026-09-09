# Diagnostics and debugging

Everything here is for finding out what ash did, or making it do something
different so you can tell what went wrong. Everyday flags are in
[cli.md](cli.md).

## Tuning promotion

| Option | Values | Description |
|--------|--------|-------------|
| `--jit-tier` | `auto`, `cranelift`, `llvm`, `off` | Pin the ladder to one rung (default: `auto`) |
| `--jit-threshold` | integer | Calls before promotion (default: 100) |
| `--jit-min-ops` | integer | Minimum opcode count before promotion (0 disables) |
| `--jit-max-args` | integer | Maximum argument count for promoted calls (default: 8) |
| `--jit-log` | flag | Log every promotion, decline and tier crossing |
| `--hot-reload` | flag | Route direct calls through indirect dispatch so code can be swapped |

Pinning a tier tests execution without a cross-tier hand-off:

```bash
ash --mode hybrid --jit-tier cranelift --jit-log program.hl
```

If both pinned tiers are correct but the ladder is not, that implicates the
transition, not either backend in isolation. `ASH_OSR=0` separates mid-frame
transfers from promotion at the next call.

`--jit-threshold 1` promotes everything immediately, which is the fastest way
to reach compiled code from a small test program.

## Build escape hatches

| Option | Description |
|--------|-------------|
| `--allow-refused` | Emit even when a function could not be lowered; each becomes a throw |
| `--emit-optimized` | Run the AIR pipeline, write ordinary bytecode, exit |

Without `--allow-refused` a refusal stops the build rather than writing a binary
that dies when it reaches one. `--emit-optimized` writes bytecode stock
HashLink can run, which is how an AIR pass gets checked against another VM.

## Environment variables

| Variable | Effect |
|----------|--------|
| `ASH_TIER` | Same values as `--jit-tier`; used when the flag is absent |
| `ASH_TIER_LOG` | Log promotions, declines and tier crossings |
| `ASH_TIERED_TIMING` | Break down tiered JIT startup cost by phase |
| `ASH_PROFILE` | `phases`, `sample` or `all` — see below |
| `ASH_PROFILE_HZ` | Sampling rate, default 997 |
| `ASH_PROFILE_OUT` | Write the profile to a file instead of stderr |
| `ASH_GC_STATS` | Print collection count, reclaimed blocks, live bytes, pause times |
| `ASH_GC_HEAP_MB` | Heap reservation (demand-committed, so a ceiling) |
| `ASH_GC_TRIGGER_MB` | Floor for the adaptive collection threshold |
| `ASH_GC_STRESS` | Collect every N allocations — torture mode for root correctness |
| `ASH_CRASH_BACKTRACE` | Capture a backtrace in the crash handler (best-effort; allocates in a signal handler) |
| `ASH_JIT_NATIVE_TRAPS` | Compile unresolved natives to call-time traps instead of failing the function |
| `ASH_LIBHL` | `system` or `embedded` — override stdlib selection |
| `ASH_JIT_REGION_MB` | Windows x86-64: JIT section region size (default 512) |

Build-side knobs — shard count, AIR level, IR dumps — are in
[aot.md](aot.md#knobs).

**`ASH_GC_STRESS=1` does not scale collections with the workload.** For many
collections over a long run, use `ASH_GC_TRIGGER_MB=1` instead; stress disables
the TLAB and is the tool for root correctness, not collection frequency.

## Profiling

`ASH_PROFILE` works in every mode and on both binaries, and answers two
separate questions.

```bash
ASH_PROFILE=phases ash --mode hybrid program.hl   # where startup and compilation go
ASH_PROFILE=sample ash --mode hybrid program.hl   # where the running program goes
ASH_PROFILE=all    ash --mode hybrid program.hl   # both
```

**Phases** are nested named regions — decode, native resolution, each tier's
lowering and codegen, execution — reported as a tree with total and self time,
so a phase that is slow because of one child reads differently from one slow on
its own. Background compilation appears under its own thread rather than nested
inside whatever the main thread was doing.

**Samples** interrupt the running thread and record the program counter. Each
sample is classified, which separates time spent in generated code from time
that code hands back to the runtime.

| Bucket | Meaning |
|--------|---------|
| `llvm` / `cranelift` | Inside code a JIT tier emitted |
| `interp` | Inside the bytecode interpreter |
| `runtime` | An `hlp_*` helper |
| `gc` | Allocation and collection |
| `native` | Any other resolved symbol |
| `unknown` | Anonymous memory with no registered code range |

Compiled functions register their entry points as they are installed, so JIT
frames resolve to Haxe function names. An external profiler sees only anonymous
`mmap` memory there.

Two details that change how a report reads. Samples are attributed by their own
address, and a bare libc leaf is re-attributed to its caller, so the collector's
`madvise` and lock traffic is charged to `gc` rather than `native`. And because
standard signals do not queue, a CPU-starved thread loses ticks instead of
banking them — the report compares CPU against wall time and says so, since
timings from a busy machine otherwise look like ordinary results.

The sampler is `#[cfg(unix)]`. On Windows `ASH_PROFILE=sample` errors once and
the phase tree keeps working.

## Bisecting a wrong answer

In order, cheapest first:

1. `ASH_AIR_NO_WIDEN=1` — the widener is the only O3 pass that rewrites
   arithmetic, so an unchanged result rules it out.
2. `--jit-tier cranelift` / `--jit-tier llvm` / `--jit-tier off` — compare the
   engines without a cross-tier hand-off. If all pass, investigate the ladder.
3. `ASH_AOT_SHARDS=1` for an AOT binary — the single-module path.
4. `ASH_LLVM_PASSES=off`, then `ASH_AOT_NO_OPT=1`.

For a suspected GC rooting bug, `ASH_GC_STRESS=1` is the detector, and
`ASH_GC_NO_RECLAIM=1` separates a rooting fault from a logic one.

The safepoints a collection can stop at: `ReentrantGcLock::acquire`, so every
allocation slow path; `fiber::park`'s loops and `hlp_fiber_poll`;
`gc_set_blocking(true)`; `worker_main`'s loop head. The TLAB bump path polls
nothing, and AOT wasm emits no safepoint of its own — so a thread in a tight
compiled loop on wasm cannot be stopped.

## Compiled-to-compiled re-tier snapshots

A running Cranelift loop transfers to LLVM when it gets hot, which is **on by
default**. `ASH_CL_RETIER=0` refuses the hand-off, so "is this a re-tier bug"
is answerable without a rebuild.

It was off by default for a day, as the mitigation for the hand-off dropping
the loop's live registers. Transferring typed SSA snapshots fixed that at the
root; keeping the mitigation cost closure_call 249ms against 145ms and
method_call 189ms against 138ms.

The former exit inferred a de-SSA register image from header phis and
materialized dominating definitions. The LLVM entry restored values by its
own register numbering. The sharp failure was an exit from a Cranelift OSR
entry: values loaded by its synthetic prologue were not represented by that
walk. A throwing callee exposed the publication window, rather than causing
the corrupted counters itself.

Re-tiering now has a separate ABI from interpreter OSR. A shared layout owns
one immutable AIR version and names an exact header, after its phis but
before its instructions. Its eight-byte slots identify SSA values and cells,
not originating registers. Cranelift spills those inputs from current block
parameters, dominating values, OSR-prologue bindings and current cell storage;
LLVM restores only those declared inputs. Every word is initialized, floats
use raw bits, and the buffer belongs to the activation. Missing source values
decline the exit. Vector inputs and oversized snapshots are refused.

Publication slots belong to a program/AIR version/header layout, and the
publisher must supply that same layout. Equal register counts or serialized
pcs are not compatibility checks; even empty AIR blocks can share a pc.
Target publication uses release/acquire synchronization. The interpreter's
existing register-image entry remains separate and unchanged.

Use `ASH_OSR_LOG=1` to distinguish a compiled entry being published from a
transfer actually being taken:

```text
[retier] published layout=2 findex=257 pc=11 inputs=11
[retier] taken layout=2 source=osr
```

`source=ordinary` is an exit from a normal Cranelift call; `source=osr` is an
exit from its OSR entry. The regression's `ASH_TEST_RETIER_AFTER=N` hook holds
off the transfer until the Nth poll in an activation, then waits up to 30
seconds for publication. It aborts on timeout rather than passing a test that
never transferred. This is a test hook, not a tuning option: it requires a
loop whose promotion has been requested. No counter/helper is emitted unless
the hook is set.

```sh
cargo test -p ash_core --lib retier::tests
cargo test --release -p ash --test osr_retier -- --nocapture
```

The integration tests require an actual hand-off from each entry path, compare
three million iterations against the interpreter, and cover both throwing
and non-throwing callees, pointer and float state, swapping header phis, and
a narrow scalar return through the compiled-entry ABI.
They also retain the default-off mitigation regression and reject missing
result lines or unsuccessful processes. Haxe rebuilds the fixtures when
available; otherwise the committed bytecode is used.
