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
| `--jit-max-args` | integer | widest signature that is promoted (default 32) |
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
| `ASH_OSR_LOG` | log published and taken transfers, and for each hot loop header whether an entry can be built (`ELIGIBLE`), what refused it, or why an entry compile declined |
| `ASH_VERIFY_OSR` | `only` reports without running the program how many functions have a loop an entry can be built for, and what refused the rest |
| `ASH_CL_VERIFIER` | `1` runs Cranelift's IR verifier on every tier-0 compile (a debug build's default), `0` skips it (a release build's default) |
| `ASH_STUB_COMPILE` | `1` makes compiled code that reaches an uncompiled callee compile it on the mutator instead of running it in the interpreter until the ladder promotes it; safe, for measuring |
| `ASH_OSR_ENTRY_SYNC` | `1` builds a late Cranelift OSR entry on the interpreter thread instead of the `ash-osr-entry` worker; safe, for measuring the stall |
| `ASH_RELOAD_LOG` | `1` names, at each hot reload, the functions it recompiled and the ones it added |
| `ASH_TRACE_LINES` | `0` drops the source-position markers compiled code carries; a compiled frame then reports its function's entry line and inlined code is not named. On by default |
| `ASH_PROFILE` | `phases`, `sample` or `all` — see below |
| `ASH_PROFILE_HZ` | sampling rate (default 997) |
| `ASH_PROFILE_OUT` | write the profile to a file instead of stderr |
| `ASH_INLINE_ALLOC` | `0` makes compiled code call the runtime for every allocation instead of bumping the thread's region inline. Safe, slower |
| `ASH_SROA_WHY` | `1` reports why each allocation the optimiser looked at was kept in memory, including ash-simd slots |
| `ASH_TREE_FUSION_SURVEY` | `only` reports recursive tree producer/consumer candidates and blockers, then exits without executing the program |
| `ASH_AIR_CACHE_STATS` | report live canonical AIR storage, bounded strong-cache residency, hits, misses and evictions at exit (conservative owned-storage bytes, not RSS) |
| `ASH_DECODE_STATS` | `1` reports at exit how many function bodies the run decoded. The decoder checks every body at load, then leaves it encoded until something reads it: most functions a `.hl` carries never run |
| `ASH_DECODE_EAGER` | `1` keeps every function body decoded from load instead. Safe to run with; for comparing memory |
| `ASH_DECODE_CHUNKED_OPS` | bodies with at least this many ops (default 2048) are decoded a chunk of 256 ops at a time by the interpreter after their first call, so a call that returns early decodes only the chunks it reaches. Safe to run with any value; `1` puts every body on that path |
| `ASH_AIR_LEVEL` | `0..3`: how hard the AIR optimiser works |
| `ASH_AIR_NO_WIDEN` | `1` disables loop widening |
| `ASH_CL_CODE_DUMP` | a directory: the Cranelift tier writes each body's machine code there, one file per address, for `objdump -D -b binary -mi386:x86-64` |
| `ASH_STRIP_MINE` | `0` makes compiled loops poll on every iteration instead of once per strip of 256. A loop that calls a native polls on every iteration either way |
| `ASH_AIR` | `v2` runs the interpreter over AIR instead of opcodes |
| `ASH_GC_STATS` | print collections, reclaimed blocks, live bytes, pause times and finalizer registrations / queued / completed / pending counts at exit |
| `ASH_GC_HEAP_MB` | heap reservation; committed on demand, so a ceiling |
| `ASH_GC_TRIGGER_MB` | floor for the adaptive collection threshold |
| `ASH_GC_SHARE_PCT` | target share of run time spent collecting (default 5); the trigger adapts toward it |
| `ASH_GC_STRESS` | collect every N allocations |
| `ASH_GC_GEN` | `0` makes every collection a major. By default (unix) most are minor: the write barrier marks the 512-byte card of every pointer store into the heap, and the next collection traces only young objects and the old ones in marked cards. A rooting bug in a long-lived object only shows under a major, so hunt those with `ASH_GC_GEN=0` |
| `ASH_GC_MAJOR_EVERY` | collections between majors (default 8); exhaustion and `Gc.major` force one |
| `ASH_GC_PROTECT` | `1` keeps the write-protection and its counts (`ASH_GC_STATS`) with `ASH_GC_GEN=0` |
| `ASH_GC_CARDS` | `0`: minors find written old objects by write-protecting the blocks a collection kept, as a native library that does not export `ash_hdll_barrier_aware` also makes them, instead of from the write barrier's cards. Compiled code and AOT binaries built with it carry no barrier. Safe to run with. `force` stays in card mode whatever libraries load, for running `ASH_GC_CARD_VERIFY` over a program that loads one; unsafe otherwise |
| `ASH_GC_CARD_VERIFY` | in card mode: before each minor, report any old object holding a young pointer in a clean card -- a store the barrier missed; `abort` also aborts, `dump` also prints the objects. Slow |
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

`ASH_GC_STATS=1` counts finalizer registrations, queued callbacks and
completed callbacks cumulatively. `pending` counts callbacks waiting for a
mutator to run them; callbacks currently executing are outside that queue.
These counters describe GC-owned handles, not Rust compiler caches or GPU
textures. The external allocation total includes a 4 KiB pressure charge per
finalizable handle, so it is not a census of the native bytes those handles own.

During allocation bursts, sweep keeps freed blocks resident to avoid repeated
page faults. It remembers those blocks across collections and returns their
pages once the mutators are idle, keeping 512 KiB warm. Reusing a block cancels
its pending return; on macOS, reusable pages are reclaimed before writing live
data. `hl.Gc.dumpMemory()` reports `blocks-pending-handback` and
`blocks-reusable` separately from live blocks. Reserved heap capacity is not
resident memory.

### AIR storage

AIR caches belong to Rust, outside the GC. Optimized bodies share an 8 MiB
strong reuse budget; lowered callees share a 2 MiB budget per module across
its views. An oversized body is served without retaining a strong cache
reference. Eviction releases unused bodies, while weak entries find the same
canonical body if an interpreter or compiler still owns it. Flat opcodes are
materialized only for flat consumers; canonical bodies keep only the register
and PC layout alongside SSA. The SSA walker
shares that body and its pc maps, skips source markers through a compact
instruction index, and builds OSR liveness only when a transfer needs it.
Invalidation releases retired walker bodies after their last active call.

`ASH_AIR_CACHE_STATS=1` separates live body storage from cache residency. Live
storage can exceed the reuse budget because executable interpreter bodies
must remain available while the program uses them. Counts include vector
capacity and nested operands with conservative allocation overhead; they
exclude executable code, GC pages, GPU resources and allocator fragmentation.
Use an allocation profiler or physical-footprint measurement for those.

LLVM reads an immutable view of the same decoded type table. Reverse native
type conversion detaches that view only when it needs to fill missing metadata.
Both interpreter walkers execute loop-free functions directly from decoded
bytecode until the JIT handles hot entries. A body with a backward jump or
switch target still prepares SSA, allowing a single long call to transfer by
OSR. `ASH_AIR_ALL=1` opts into preparation of every interpreted body for
comparison. This avoids retaining prepared IR for startup-only helpers.

An entry-only function's prepared interpreter body is retired after compiled
code lands; active calls keep their own owner and loop bodies retain their OSR
coordinates. `ASH_AIR_CACHE_STATS` reports the retained layout bytes separately
from the IR, rather than counting discarded flat opcode arrays.

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

### A hybrid run much slower than `--mode jit`

Run the program under `--mode jit` first. If it is several times faster, hybrid
is leaving a hot loop in the interpreter: `ASH_PROFILE=sample` names the
function ("by interpreted function"), `ASH_OSR_LOG=1` says for each of its
loop headers whether the interpreter could hand the running frame to compiled
code and, if not, what refused it or declined the entry, and
`ASH_VERIFY_OSR=only` surveys a whole program's loops the same way without
running it.

### Recursive tree allocation candidates

```sh
ASH_TREE_FUSION_SURVEY=only target/debug/ash --quiet \
  crates/ash/test/tests/bench_binary_trees.hl
```

This is the analysis checkpoint of git-bug `b002f3f` / `05ec58f`; it does not
enable fusion or reduce allocations. It inspects pre-inlining AIR so the
producer and consumer call boundaries are still visible. The unchanged
binary-trees fixture reports three structural candidates in function 25
(stretch tree and both inner-loop trees), producer 24 / consumer 23. The
long-lived tree is refused because its use is delayed.

The first supported pattern has a signed i32 depth guard, self recursion with
`depth - 1` only when depth is positive, a fresh root initialized exactly once,
unique recursive children, and a scalar fold visiting every child once.
Integer add/subtract/multiply and bitwise operations retain wrapping semantics;
division, shifts, casts, floats, unknown calls, mutation, identity observations,
traps, and mutual recursion are conservatively refused. Constructors must be
straight-line initialization helpers. Unused loop phi cycles do not escape;
observed phis, additional receiver uses, and intervening effects do.

These restrictions address evaluation order: construction originally finishes
before the fold, while a fused helper would interleave them. The accepted
scalar operations cannot throw, the depth proof establishes termination, and
the field/child proof rules out null reads. A future rewrite must preserve the
scalar computations and their wrapping semantics; allocation, stack exhaustion
and GC observability still require an explicit runtime contract.

`structural-candidate` is conditional evidence, not rewrite authorization.
Ash deliberately reports two unresolved contracts: allocation elision and
code/layout lifetime. A bytecode class declaration cannot certify the absence
of host tails/drop hooks or custom allocator effects for the lifetime of
generated code. Future integration must establish those facts or guard them
and invalidate helpers on code, layout, or host-policy changes. It must also
define permitted allocation/GC/OOM observability and preserve safepoints and
rooting for any retained objects. The survey does not cache executable proofs.

The survey bounds body size, symbolic steps, candidate sites and module size.
Skipped bodies and exhausted budgets are reported explicitly; a partial
survey is not evidence that the remaining module has no candidates. No normal
compilation path runs this analysis yet.

### Generated helper preparation

Git-bug `8599508` adds `ash_core::generated::prepare`. It prepares a fresh
`DecodedBytecode` before an interpreter, compiler, function table or AIR cache
can reference it. This infrastructure does not enable tree fusion.

The lifecycle is: decode and register host modules; declare helpers; reserve
all helper IDs; build and verify their AIR; serialize and append their bodies;
publish the module to the runtime. Original function IDs remain unchanged.
Keys sort by pass name/version, source function IDs and specialization values.
Identical declarations share one helper; conflicting signatures fail. Every
builder receives the complete reservation map, so recursive references resolve
before bodies are installed. Registration failure leaves the input unchanged.

The initial supported bodies have i32 arguments, results and locals, scalar
arithmetic, branches and direct calls with checked signatures. Reference
values, natives, exception regions, pinned cells, SIMD scratch and inlined
debug frames are refused. Limits default to 32 helpers, 2,048 instructions per
body, 16,384 serialized instructions in total and 16 MiB of AIR. Builders are
compiler code and must also bound their own construction work. Private integer
constants are rebased into one shared module pool. Source positions become
ordinary per-op debug entries; helper names identify the generating pass.

Helpers use ordinary function dispatch, reachability, serialization and the
shared AIR pipeline. The interpreter and native tiers optimize the same
registered body under their existing configurations; OSR continues to use the
interpreter configuration and its canonical PC/register mapping. There is no
per-tier helper generation. A fresh preparation has a unique generation in
the AIR cache key, including when a module's memory address is reused.
Deduplication never crosses module snapshots. After any code, type or host
metadata change, rebuild from original input. Host registration on a prepared
module and in-place hot reload are refused; dependency-aware invalidation is
not implemented. Public module fields must remain immutable after preparation,
as they must after runtime tables are created.

For development, `--dev-generated-helper INDEX` clones one scalar function
into a registered helper, redirects its self calls, and leaves a forwarding
body at its original ID. It tests registration and dispatch; it changes debug
stack shape and is not an optimization. It is hidden from normal CLI help and
conflicts with `--hot-reload`, `--build` and `--emit-aot`. To test serialization
and AOT, first emit the prepared module:

```sh
ash --dev-generated-helper INDEX --emit-optimized prepared.hl input.hl
ash --build prepared prepared.hl
ash --target wasm32-wasip1 --emit-aot prepared.o prepared.hl
```

AOT decodes separately for its target ABI, so it consumes the emitted helpers
as ordinary functions. The `.hl` encoder already drops debug information;
helper IDs, signatures, calls and bodies survive. Registration metadata and
generation numbers are process-local, and do not enter the file. These helpers
carry no runtime-dependent specialization assumptions. Future allocation
elision must establish its policy separately and refuse serialization if that
policy cannot travel with the program. A structural tree candidate alone is
still insufficient authorization.

### Recursive tree fusion prototype

Git-bug `bfd0a53` adds an explicit development option:

```sh
ASH_GC_STATS=1 ASH_GC_TLAB=0 target/debug/ash --dev-tree-fusion --quiet \
  crates/ash/test/tests/bench_binary_trees.hl
```

Normal runs do not enable it. Before creating runtime tables, preparation
surveys raw AIR, reserves a helper for each proven producer/fold pair, then
rechecks and rewrites the callers. Unsupported sites retain their original
calls. Failed helper construction, serialization, verification or growth
limits abandon the entire transaction and retain the original module.

The helper is derived from the two proven depth cases. It keeps the signed
positive-depth guard and producer scalar calculations, replaces each recursive
child construction with a recursive call returning that child's folded value,
and substitutes initialized fields into the consumer. The consumer's null
branches resolve from the proven shape. Existing DCE removes unused values;
AIR is verified before registration and after caller rewriting. Each scalar
temporary gets a private register. Neither names nor checksum formulas select
the transformation. The ordinary tier pipelines perform further optimization.

The runtime contract is deliberately limited to a fresh standalone Ash CLI
process using the linked runtime. Hosted modules, external native libraries,
host native overrides, active object tails/drop hooks/layout room, and native
registration hooks are refused. In-place hot reload is refused. As with
allocation elimination elsewhere in the compiler, allocation counts, GC and
unrelated finalizer scheduling, resource exhaustion (including stack/OOM), and
profiling timing are not barriers under this opt-in contract. Observable value
results, memory effects and ordinary exceptions must be preserved. The proof
excludes object identity, escaping references, weak observations, effectful
constructors and unknown calls. Total wrapping integer operations and the
well-founded recursion proof justify interleaving construction and consumption.

Helpers use normal calls and the backend's normal safepoint policy. Retained
objects continue through ordinary liveness/rooting; the prototype introduces
no arena, hidden allocation counter or alternate collector. The long-lived
benchmark tree still materializes. Source positions are retained, but helper
frames can change diagnostic stack shape within the elided computation.

This development flag conflicts with `--hot-reload`, `--dev-generated-helper`,
`--emit-optimized`, `--build` and `--emit-aot`. The bytecode encoder also refuses
a fused module: ordinary `.hl` cannot carry this runtime contract. Native AOT
and wasm activation need explicit policy plumbing in a later checkpoint.

The unchanged depth-16 benchmark produces checksum `-104864`. A local hybrid
comparison with TLAB disabled on both runs reported **900.0 MiB → 4.0 MiB**
cumulative allocation: three immediate sites share one 19-op scalar helper,
while the depth-16 long-lived tree remains. The runtime labels these counters
`MB` but divides by 1,048,576. This is prototype allocation evidence, not a CI
timing result or a default rollout. Diagnostics report transformed sites,
refusals, helper size and skipped/truncated surveys. Inspect the helper with
`ASH_VERIFY_AIR=dump:INDEX` alongside `--dev-tree-fusion`.

#### Fusion validation (checkpoint 5)

Git-bug `3189601` exercises the transformation beyond the benchmark checksum:

- `TestTreeFusionMatrix`: unary and ternary recursion, depth in different
  argument positions, reordered fields and child consumption, constructor
  arithmetic, a Haxe-inline wrapper, and 128 deterministic wrapping-integer
  cases. Retained trees provide the unfused reference result. Four sites share
  two helpers; the normal result is `tree-matrix=-663667236`.
- `TestTreeFusionRefusals`: shared/escaping references, identity, mutation,
  dynamic calls, skipped children, constructor effects and exceptions, and
  null access. No site transforms. A separate bounded timeout checks that a
  divergent producer remains divergent even when its consumer skips children.
- `TestTreeFusionSuspended`: a retained local tree survives worker suspension
  and explicit collections around a fused call, with zero and two native
  workers. It reports `tree-suspended=57`.

The CLI tests compare original and fused execution in the interpreter,
Cranelift, LLVM (when enabled), and hybrid. GC stress runs also verify the heap
and require more collections than the fixture explicitly requests. The hot
matrix case retains a local reference and scalar accumulator through an
asserted Cranelift OSR entry. Another run pins promotion to the generated
unary helper and asserts each enabled tier's installation. Merely selecting
hybrid mode or getting the right checksum does not satisfy those assertions.

Fusion now requests unoptimized proof bodies independently of inliner policy.
Previously `ASH_AIR_NO_INLINE=1` silently prevented fusion by withholding its
callees. It now keeps fusion active while disabling later inlining, so the
helper dispatch/promotion test actually exercises the intended code.

Core tests reject host metadata, external natives and registration hooks;
fresh child processes also install actual object tails, drop policy or layout
room and verify refusal. Existing tests cover atomic fallback, serialization
and reload refusal, and portable helper AOT/wasm object registration.

```sh
cargo test --offline -p ash_core --lib tree_fusion
cargo test --offline -p ash --test tree_fusion --test tree_fusion_validation \
  --test tree_fusion_survey --test generated_helpers
cargo test --offline -p ash --no-default-features --test tree_fusion \
  --test tree_fusion_validation --test tree_fusion_survey
```

These are local macOS correctness checks, including a build without LLVM.
Fusion remains unavailable for serialized bytecode, native AOT and wasm until
their runtime contract is carried explicitly; portable-helper AOT tests do
not establish support for fused programs. The benchmark source/bytecode and
checksum remain unchanged. Same-host CI measurements and default rollout are
the subsequent gates.

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

## Hot reload

`ash --mode hybrid --hot-reload program.hl` watches `program.hl`. When the file
is replaced, the program's next `hl.Api.checkReload()` returns true and the
interpreter takes the new code. A frame that is running keeps its body until it
returns, every call after the swap runs the new one, and what objects and
statics hold is kept.

A reload can change function bodies and literals, add, remove, move and edit
closures, add and remove the methods of a class that is running, add fields to
a class that is running, and add string constants, function types and classes.
A closure
that is in both programs keeps its place, so one made before the reload runs
the new body; one the new program drops stays callable for the closures that
still name it, with its old body and the constants it reads. A class the
new program adds is set up
once, after the reload, by the part of the new program's entry function that
registers it and gives its statics their first values, as at start. If that
throws, a warning says so and the next reload sets the classes up again. A
class the new program drops stays in the running one, so a later reload that
adds it back finds the class that was running, with the values of its statics.

A method keeps the vtable slot it has, and one that gains a slot (the first
override of a method gives it one) or is new and virtual takes a slot above the
class's and its subclasses'. A call that began before the reload therefore
reaches the same method, and an override the reload adds or removes is seen by
every call that goes through the vtable. A call the compiler made direct,
because nothing overrode the method, stays direct in a frame that began before
the reload, so such a frame does not see an override the reload adds. A removed
override leaves its slot to the method it overrode; a removed method that
introduced a slot stays in the class, with its old body, for the calls that
name the slot.

Under `--hot-reload` every object is allocated with 64 spare bytes at its end
(`ASH_RELOAD_ROOM=<bytes>` sets how many; with 0 a class can only grow into the
padding at its end), and a field the new program adds to a class goes after the
fields the class has, in that room. An object made before the reload holds zero
in it: the constructor's initialiser has not run for that object. A field
declared between the others in the new program is still put last in the running
class, and the bodies that name the fields are renumbered to match. A field the
new program no longer has stays in the class, unused: objects keep their value,
the bodies that began before the reload can still read it, and a reload that
declares it again finds it as it was. It also stays in the class's reflection
(`Reflect.fields`, `Type.getInstanceFields`) and keeps its bytes of the spare
room, so a program that renames a field over and over runs out of room, and a
class cannot gain a subclass while it has such a field. Only a class that nothing extends can change its fields, since the
fields of a subclass follow its parent's, and a class cannot grow past the room
its first objects were given, however many reloads add to it.

A constructor that sets the new field to a constant or null is accepted. One that
sets it to anything else is refused, naming the field, because the object made
before the reload would never get that value. A field that comes back after a
reload dropped it is held to the same rule, since the objects built while it was
gone hold null in it. Only the constructor's own body is read: a field that a
method it calls sets is not seen. The exception is the backing
field of an ashui `@:state`, named `<field>__state`, in a program built with
`-D ashui_hot_reload`: its accessors make the signal when the field is null,
and the program says so by having `ashui.ui.HotReload.stateOnFirstUse`.

What live objects and compiled code are laid out by cannot change. The reload
is refused, on stderr, and the program goes on as it was:

| Message | Why |
|---------|-----|
| `Class.method is new and Sub already declares a method of that name` | the new method and the subclass's would have to share a slot |
| `Class.method moves from slot a to b, the slot of the method it now overrides` | frames that began before the reload call the old slot |
| `Class binds different statics; a static function was added or removed` | statics are fields of the `$Class` object, which is laid out like any |
| `the statics of Name changed` | a class's statics are fields of a `$Name` object, so a static added to a class that was running, which includes the first use of a function of the standard library, adds a field to that object, and nothing sets it up |
| `field f of Name changed its type` | live objects have the field where it was, with the old size |
| `Name gained f, and Sub extends it, so its fields would move` (or `lost f`) | the fields of a subclass follow its parent's |
| `Name gained f, and a CArray of it lays its objects out one after another` | the size of the class is the stride of the array |
| `Name gained f, and an object made before the reload has N bytes where the class now needs M` | the spare room is used up; restart the program |
| `Name.f is set by a constructor with something that is not a constant` | an object made before the reload would hold null in it |
| `Name.f__state is a state field the program does not make on first use` | build with `-D ashui_hot_reload` |
| `the superclass of Name changed` | live objects are laid out by the old one |
| `the constructors of enum Name changed` | enum values are laid out by their constructors |
| `the setup of the new classes has control flow in it` | what a new class needs from the entry function is not a straight run of ops: a static whose first value is picked by a test written into the entry function, such as `static var on = env != null` |
| `needs N globals`, `needs N function slots` | the program reserved room for as many added globals and functions as it started with, at least 65536 and 8192; restart it |

A reload is applied when the program calls `hl.Api.checkReload()` and gets
`true`, whether the loop that calls it is interpreted or has been compiled, so
the next call it makes is to the new code. `--mode jit` applies a reload too,
but code compiled before it keeps the functions it inlined; reload in hybrid
mode. `ASH_RELOAD_LOG=1` lists, by name, the functions each reload recompiled
and added.

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
