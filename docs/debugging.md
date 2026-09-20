# Diagnostics

Finding out what ash did, and making it do something else so the difference
can be seen. Everyday options are in [cli.md](cli.md).

## Pinning a tier

| Option | Values | |
|--------|--------|---|
| `--jit-tier` | `auto`, `cranelift`, `llvm`, `off` | restrict the ladder to one compiler (default `auto`) |
| `--jit-threshold` | integer | calls before Cranelift promotion (default 100) |
| `--opt-threshold` | integer | interpreted calls before LLVM promotion (default 1000) |
| `--jit-min-ops` | integer | minimum opcode count before a function is promoted (0 disables) |
| `--jit-max-args` | integer | widest signature that is promoted (default 8) |
| `--jit-log` | flag | log every promotion, decline and tier transfer |
| `--hot-reload` | flag | route direct calls through indirect dispatch so code can be swapped |

The first question about a wrong answer is which engine produced it:

```bash
ash --mode interp program.hl
ash --mode hybrid --jit-tier cranelift --jit-threshold 1 program.hl
ash --mode hybrid --jit-tier llvm --jit-threshold 1 program.hl
```

`--jit-threshold 1` promotes everything at the first call, so a small program
reaches compiled code without a warm-up loop. If both pinned tiers agree with
the interpreter and `--jit-tier auto` does not, the fault is in the transfer
between tiers, not in either compiler; `ASH_OSR=0` then separates mid-loop
transfers from promotion at the next call, and `ASH_CL_RETIER=0` refuses the
Cranelift → LLVM hand-off specifically.

`ASH_OSR_LOG=1` distinguishes a compiled entry being published from a transfer
being taken:

```text
[retier] published layout=2 findex=257 pc=11 inputs=11
[retier] taken layout=2 source=osr
```

## Environment variables

| Variable | Effect |
|----------|--------|
| `ASH_TIER` | same values as `--jit-tier`; used when the flag is absent |
| `ASH_TIER0_WORKERS` | threads draining the tier-0 compile queue; default half the cores, at most four |
| `ASH_TIER_LOG` | log promotions, declines and tier crossings |
| `ASH_TIERED_TIMING` | break down JIT startup cost by phase |
| `ASH_OSR` | `0` disables mid-loop transfers into compiled code |
| `ASH_CL_RETIER` | `0` refuses the Cranelift → LLVM hand-off of a running loop |
| `ASH_OSR_LOG` | log published and taken transfers |
| `ASH_CL_VERIFIER` | `1` runs Cranelift's IR verifier on every tier-0 compile (a debug build's default), `0` skips it (a release build's default) |
| `ASH_STUB_COMPILE` | `1` makes compiled code that reaches an uncompiled callee compile it on the mutator instead of running it in the interpreter until the ladder promotes it; safe, for measuring |
| `ASH_OSR_ENTRY_SYNC` | `1` builds a late Cranelift OSR entry on the interpreter thread instead of the `ash-osr-entry` worker; safe, for measuring the stall |
| `ASH_TRACE_LINES` | `0` drops the source-position markers compiled code carries; a compiled frame then reports its function's entry line and inlined code is not named. On by default |
| `ASH_PROFILE` | `phases`, `sample` or `all` — see below |
| `ASH_PROFILE_HZ` | sampling rate (default 997) |
| `ASH_PROFILE_OUT` | write the profile to a file instead of stderr |
| `ASH_INLINE_ALLOC` | `0` makes compiled code call the runtime for every allocation instead of bumping the thread's region inline. Safe, slower |
| `ASH_SROA_WHY` | `1` reports why each allocation the optimiser looked at was kept in memory, including ash-simd slots |
| `ASH_AIR_LEVEL` | `0..3`: how hard the AIR optimiser works |
| `ASH_AIR_NO_WIDEN` | `1` disables loop widening |
| `ASH_CL_CODE_DUMP` | a directory: the Cranelift tier writes each body's machine code there, one file per address, for `objdump -D -b binary -mi386:x86-64` |
| `ASH_STRIP_MINE` | `0` makes compiled loops poll on every iteration instead of once per strip of 256 |
| `ASH_AIR` | `v2` runs the interpreter over AIR instead of opcodes |
| `ASH_GC_STATS` | print collections, reclaimed blocks, live bytes, pause times at exit |
| `ASH_GC_HEAP_MB` | heap reservation; committed on demand, so a ceiling |
| `ASH_GC_TRIGGER_MB` | floor for the adaptive collection threshold |
| `ASH_GC_SHARE_PCT` | target share of run time spent collecting (default 5); the trigger adapts toward it |
| `ASH_GC_STRESS` | collect every N allocations |
| `ASH_GC_GEN` | `0` makes every collection a major. By default (unix) most are minor: the blocks a collection kept are write-protected, the first store into one faults and marks it dirty, and the next collection traces only young blocks and dirty old ones. A rooting bug in a long-lived object only shows under a major, so hunt those with `ASH_GC_GEN=0` |
| `ASH_GC_MAJOR_EVERY` | collections between majors (default 8); exhaustion and `Gc.major` force one |
| `ASH_GC_PROTECT` | `1` keeps the write-protection and its counts (`ASH_GC_STATS`) with `ASH_GC_GEN=0` |
| `ASH_GC_COLLECT_AT_REFILLS` | `a,b,c`, `a-b` or `every:N`: collect at exactly those bump-region refills, numbered from 1 as `ASH_GC_STATS` reports them, to replay a timing-dependent collection |
| `ASH_GC_SWEEP_AUDIT` | report roots that still point into a block being freed or a line run being recycled |
| `ASH_GC_POISON` | `1` fills freed blocks and recycled line runs with `0xA5`, so a stale reference faults at its first use |
| `ASH_GC_STALE` | `1` makes the interpreter check every pointer it reads from or stores into a field against the allocation table, and abort with the frames' registers (each with the collection it was written at) when one no longer starts an allocation or its line was recycled since |
| `ASH_GC_CHECK_FRESH` | `1` reports memory handed out twice: every allocation claims its quanta in a shadow table that only the sweep clears, and the block lists are checked against each other |
| `ASH_GC_VERIFY_HEAP` | `1` checks, between mark and sweep, that every class-typed field of a marked object points at the start of a marked object |
| `ASH_GC_STACK_ONLY` | `1` lists, per collection, the marked objects that precise roots do not reach (only the native stack does), and the ones a later collection reaches precisely: a root the interpreter did not publish |
| `ASH_INTERP_CHECK_FRAMES` | `1` verifies on every interpreter step that the top frame belongs to the function being stepped and is the last published root range |
| `ASH_NATIVE_RECOVERY` | `0` stops the interpreter turning a SIGSEGV inside a native call into a recoverable error, so the crash banner names the real fault site |
| `ASH_CRASH_BACKTRACE` | capture a backtrace in the crash handler (best effort; allocates in a signal handler) |
| `ASH_JIT_NATIVE_TRAPS` | compile unresolved natives to call-time traps instead of declining the function |
| `ASH_LIBHL` | `system` or `embedded`: which runtime library to load |
| `ASH_JIT_REGION_MB` | Windows x86-64: JIT code region size (default 512) |

Build-time dials — shards, IR dumps, the optimiser pipeline — are in
[aot.md](aot.md#dials).

`ASH_GC_STRESS=1` does not scale with the workload and it disables the TLAB,
so it changes the allocation path as well as the collection frequency. For
many collections over a long run use `ASH_GC_TRIGGER_MB=1`. A clean stress
run does not rule out a rooting bug exposed by TLAB line reuse; run a second
time with TLAB and recycling enabled. `ASH_GC_NO_RECLAIM=1 ASH_GC_RECYCLE=0`
disables block reclamation and line reuse for comparison.

## Profiling

`ASH_PROFILE` works in every mode, in `ash` and in AOT binaries.

```bash
ASH_PROFILE=phases ash program.hl   # where startup and compilation go
ASH_PROFILE=sample ash program.hl   # where the running program goes
ASH_PROFILE=all    ash program.hl
```

**Phases** are nested named regions — decode, native resolution, each
tier's lowering and codegen, execution — printed as a tree with total and
self time. Background compilation is reported under its own thread.

**Samples** interrupt the running thread at `ASH_PROFILE_HZ` and record the
program counter. Each sample is classified:

| Bucket | |
|--------|---|
| `llvm` / `cranelift` | inside code that tier emitted |
| `interp` | inside the bytecode interpreter |
| `runtime` | an `hlp_*` primitive |
| `gc` | allocation and collection |
| `native` | any other resolved symbol |
| `unknown` | anonymous memory with no registered code range |

Compiled functions register their entry points when installed, so JIT frames
resolve to Haxe function names; an external profiler sees anonymous `mmap`
memory there. A bare libc leaf is charged to its caller, so the collector's
`madvise` and lock traffic lands in `gc`. Signals do not queue, so a
CPU-starved thread loses ticks; the report compares CPU time with wall time
and says so.

The sampler is Unix-only. On Windows `ASH_PROFILE=sample` prints one error
and the phase tree still works.

## Bisecting a wrong answer

Cheapest first:

1. `ASH_AIR_NO_WIDEN=1` — the widener is the only O3 pass that rewrites
   arithmetic; an unchanged result rules it out.
2. `--jit-tier cranelift`, `--jit-tier llvm`, `--jit-tier off` — compare the
   engines without a cross-tier hand-off. All correct means the ladder.
3. `ASH_AOT_SHARDS=1` for an AOT binary.
4. `ASH_LLVM_PASSES=off`, then `ASH_AOT_NO_OPT=1`.

For a suspected GC rooting bug: `ASH_GC_STRESS=1`, then a second run with the
TLAB enabled as above. Collections stop the world only at safepoints —
`ReentrantGcLock::acquire` (every allocation slow path), `fiber::park`,
`hlp_fiber_poll` at loop back edges, `gc_set_blocking(true)`, the worker
loop head. The TLAB bump path polls nothing, and wasm AOT emits no safepoint
of its own, so a thread in a tight compiled loop on wasm cannot be stopped.

## Escape hatches

| Option | |
|--------|---|
| `--allow-refused` | build even when a function could not be lowered; each becomes a throw |
| `--emit-optimized <path>` | run the AIR pipeline and write plain bytecode, then exit |

`--emit-optimized` produces a `.hl` that stock HashLink runs, which is how an
AIR pass is checked against another VM.

## Re-tier tests

The Cranelift → LLVM hand-off of a running loop carries typed SSA snapshots
of the live values; a snapshot that cannot be built declines the exit.
`ASH_TEST_RETIER_AFTER=N` is a test hook that holds the transfer until the Nth
poll and then waits up to 30 seconds for publication, aborting on timeout.

```sh
cargo test -p ash_core --lib retier::tests
cargo test --release -p ash --test osr_retier -- --nocapture
```
