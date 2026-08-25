# ash — Development Backlog

Known gaps, deferred refinements, and open defects. Code docs describe what the
code guarantees today; anything that should change lives here.

**Last updated**: 2026-08-22

**Status**: Heaps Base2D renders through the real init path under
`--mode interp`, reaching `Main.init()` in ~1.5s.

One more sighting for the rate ledger (2026-08-21 evening):
parity_matrix_hybrid_default failed once during a full three-leg run while
two agent workflows were compiling at peak load, then passed solo and in
three consecutive full runs. 1-in-5 under heavy load, 0-in-4 after; the
failing case name was lost to output truncation. Same intermittency family;
the parity corpus reproduces it far more weakly than the Heaps sample.

`--mode hybrid` on the same program **crashes about four runs in five**
(SIGSEGV, `fault_addr=0x0`, after roughly four Cranelift installs). This
entry previously read "sustains 19 promoted functions with no crashes";
that was measured on a single successful run, and 5-run samples of both a
2026-08-17-era binary and today's put it at 1/5. It is intermittent, which
points at a race between the broker thread and the mutator rather than at
any one lowering. Not a regression — both binaries fail at the same rate.

`--mode jit` does not reach `Main.init()` within 90s on this program; the
whole-module LLVM compile of a 1.4MB module is the suspect, and it has not
been separated from the crash above.

The tiered pre-warm is not the startup cost it was assumed to be: measured
at 0.04s here, so "hybrid is slow to start" is the crash, not the warm-up.

---

## DeltaBlue: hybrid dies on an uncaught Null access (compiled tiers)

Narrowed 2026-08-21. This entry began as "two defects, one of them
GC-shaped"; the investigation proved the two interp SIGSEGV modes were ONE
defect, and not the GC's. The interp half is fixed; only the hybrid failure
remains open.

CLOSED — interp SIGSEGV, page-aligned and 0x6 modes alike. The
interpreter's `dynamic_type_name` decoded the UTF-16 uchar* name returned
by `hlp_type_name` with `CStr::from_ptr`, truncating every type name to its
first ASCII character ("String", "Strength", "StayConstraint" all read
"S"), and the string-equality fast paths gated on `Some("String") |
Some("S")` — the "S" arm existed precisely because real Strings truncated
the same way. Every `==` between S-named non-String objects therefore
content-compared them as String {bytes, length}: field 0 read as the bytes
pointer (Strength.WEAKEST.value = 6 → the 0x6 fault), field 1 as the
length (low 32 bits of a heap pointer → `utf16_len_eq` walked the mapped
reservation and faulted at exactly heap_base + 512MB — the fault addresses
were page-aligned because they were heap_end, not because a page had been
reclaimed). Fix: decode the name as UTF-16, drop the "S" arms at the three
gates, and short-circuit identical pointers in the HOBJ Eq/NotEq path.
Verified on this tree: 20/20 interp runs at Checksum: 14065400, plus
ASH_GC_STRESS=10/2000 runs with freed-block poisoning and the sweep audit —
13k+ collections, 3000 blocks freed, 0 audit hits, checksum bit-correct.

The GC was exonerated three independent ways: the crash reproduced with
zero collections ever having run; the heap is a single PROT_READ|WRITE
mapping, so no in-heap address can fault regardless of MADV_FREE_REUSABLE
state (page handback is additionally gated on the 30s quiet HEARTBEAT the
sub-second crash never reached); and ASH_GC_NO_RECLAIM / quarantine /
poison runs left the crash identical. The hunt's env-gated diagnosis knobs
are kept in std/src/gc.rs: ASH_GC_TRACE_MAP (reservation/HANDBACK/REUSE
tracing), ASH_GC_NO_RECLAIM, ASH_GC_POISON, ASH_GC_QUARANTINE,
[gc-collect] origin+seq tracing, [gc-reuse], and a NaN-box-aware sweep
audit with a second pass over live retained lines.

CLOSED 2026-08-22 (54312ea) — and the bisect above was reading the right
signal: not the interpreter, not one backend, the shared promotion path.
`CallMethod` resolves its target from the receiver's vobj_proto; when a
promotion has patched that row with real code the findex has to be
re-derived, and it was re-derived from the register's DECLARED type. For an
overridden method that is the base class, so every call through a patched
row ran the base implementation. Which rows happened to be patched decided
what broke, hence "sometimes a wrong answer, sometimes a Null access".
Resolution now walks the RUNTIME type's proto chain child-first, the order
vobj_proto is itself built in. DeltaBlue is 5/5 at 14065400 in hybrid with
either tier; benchmarks.toml is back to all four modes.

Pinning it needed a new gate: ASH_TIERED_ONLY_FINDEXES promotes only the
listed findexes. Skip-lists cannot bisect a promotion defect — each
exclusion just lets the next-hottest function promote instead, so the
tested set never converges.

---

## ASH_GC_STRESS runs out of memory on binary trees

Found the day the benchmark was added, by the benchmark. `bench_binary_trees`
(and even its lighter n=14 variant) aborts under `ASH_GC_STRESS=1` with
"Out of memory" from `hlp_alloc_obj` (std/src/obj.rs:134), while completing
fine — checksum-correct in every engine — without stress mode.

The corpus being "bit-identical at ASH_GC_STRESS=1" predates any test that
allocates like this: millions of short-lived 3-field objects around one
long-lived tree. That is the worst case for block-level reclamation with
conservative scanning — a block survives if ANY stale stack word points into
it, and this workload maximizes both the number of blocks and the number of
plausible-pointer integers (tree items) on the stack. Suspect list, in order:
false retention pinning nearly every block until the 512MB heap fills;
stress-mode collecting so often that the proactive trigger never sees a
genuinely empty block; or a leak in the stress path itself.

Not blocking the bench (normal-mode runs are what CI measures), but it means
the GC stress gate currently proves less than it claims.

---

## The published numbers are Mac numbers, and the Mac compiles LLVM ~3x slower

Measured 2026-08-22. Same function (fib's hot findex 22), same release
build, tier log's own compile timing, three-plus samples each:

    M1 Pro (this box)   544 / 525 / 530 ms
    NUC i5-1250P        193 / 217 / 143 / 142 ms

~3.1x slower on Apple Silicon. Our benchmarks all finish in 0.1-3.7s and
are compile-dominated, and website/bench/results.json records
`"cpu_model": "Apple M1 Pro"` — so every number we publish is paying that
multiplier. "Our numbers are terrible" is substantially this.

The engine itself is not slow. On the NUC, where `hl` actually runs (it
does not run on the Mac at all), fib interleaved, identical checksum
102334155 both sides:

    HashLink 1.15   1722 / 1759 / 1739 ms
    ash hybrid        99 /   97 /  100 ms

ash is ~17.5x faster than HashLink on fib. Any claim that ash is slow needs
to name the machine before it means anything.

## The ladder can be much worse than pinning the top tier

Harness, release, 5 iterations, hybrid modes on the Mac:

    benchmark     hybrid-auto   hybrid-cranelift   hybrid-llvm
    fib             875.1 ms        1163.3 ms        129.8 ms   <- 6.74x
    mandelbrot      630.4 ms          22.4 s        1111.4 ms
    method_call     294.1 ms          17.0 s         309.3 ms

For fib the default ladder is 6.7x slower than going straight to LLVM. The
sequence is: promote to Cranelift (0.75ms compile, but much slower code),
which stops call-count accrual, then chase LLVM whose 530ms compile lands
too late to be used and is waited on at exit. Pinning LLVM skips all of it.

This is NOT a general "pin LLVM" result — mandelbrot is 1.8x WORSE pinned
to LLVM than on auto, and method_call is a wash. And hybrid-cranelift's
22.4s/17.0s are functions refusing that tier and staying interpreted (tier
counts 1/0), not Cranelift emitting slow code for them. What the fib row
does show is that when Cranelift wins the race for a program's one hot
function, the result can be several times worse than not promoting at all.

Worth checking against e9cbe35 ("Cranelift covers 99.4% of the corpus, up
from 57%"): raising coverage means Cranelift now wins that race for
functions that previously went straight to LLVM. Coverage was measured;
the effect of coverage on wall time was not.

## Hybrid blocks process exit on LLVM chases whose results are never used

Measured 2026-08-22, RELEASE binary, interleaved A/B/C/D over three rounds
(consecutive blocks would put drift entirely on whichever mode ran last).
deltablue, checksum verified 14065400 on all twelve runs:

    mode          wall (3 runs, ms)        execute/run    peak RSS
    interp        733.8 / 711.9 / 735.2    ~724 ms        35 MB
    hybrid-auto   3726 / 3448 / 3752       ~57 ms         87 MB
    hybrid-llvm   801.8 / 808.7 / 1147.8   ~813 ms        80 MB
    full-jit      1484 / 1541 / 1868       ~3.6 ms       108 MB

Hybrid is the SLOWEST mode on wall and the FASTEST to the answer:

    hybrid --jit-tier auto   checksum at  228 ms   exit at 5331 ms
    hybrid --jit-tier off    checksum at  896 ms   exit at  927 ms

228ms to a correct answer — 3x better than the interpreter — then 5.1
seconds of nothing. The phase tree accounts for ~100ms of that wall
(`run` 83ms), so the time is outside every instrumented phase: it is
`retier_chase_join()` at shutdown.

Confirmed by the gate:

    ASH_LLVM_CHASE=0   exit at 1110 / 866 ms
    default            exit at 4994 / 4780 ms

Each Cranelift install spawns a thread chasing the LLVM tier (38 Cranelift
+ 14 LLVM installs on this program). Those threads cannot be detached — one
still compiling while the interpreter tears down segfaults, roughly one run
in ten — so shutdown joins them. For a program that ends in 228ms, every
one of those compiles is dead work the process still waits for.

**The chase does not pay for itself on any benchmark we have.** An earlier
draft of this entry said it was "worth keeping", citing the code comment at
its spawn site (fib(40): 362ms Cranelift vs 87ms LLVM). That was repeating
a comment, not a measurement. A/B over the whole bench set — same release
binary, the two arms differing ONLY by ASH_LLVM_CHASE, alternated within
each round so drift hits both equally, three rounds, medians:

    benchmark          chase ON      chase OFF      delta
    array_access        426 ms        326 ms       +30.7%
    binary_trees        643 ms        605 ms        +6.3%  (within noise)
    closure_call        299 ms        276 ms        +8.3%  (within noise)
    deltablue          3705 ms        759 ms      +388.1%
    fib                 559 ms        559 ms        +0.0%  (within noise)
    field_access        189 ms        192 ms        -1.6%  (within noise)
    free_call           158 ms        157 ms        +0.6%  (within noise)
    inlined_call        142 ms        140 ms        +1.4%  (within noise)
    method_call         238 ms        210 ms       +13.3%
    mandelbrot          625 ms        405 ms       +54.3%
    nbody               696 ms        583 ms       +19.4%

It costs 13-388% on five benchmarks and is indistinguishable on six. It
wins on none — field_access is the only nominal win and it is inside the
noise floor. Notably it does not pay on **fib**, the benchmark its own
justification names: 559ms both arms, and the chase-on arm carried 36%
spread against 3% for chase-off, so it adds variance as well as time.

The honest scope of that result: every benchmark we own finishes in
0.14-3.7s. The chase's theoretical case is a long-running program where
better code amortises over many iterations, and we have no such benchmark.
So this refutes "the chase pays on our suite", not "the chase can never
pay" — but the burden is now on producing a workload where it does.

Fix direction, in order:
  1. Make the chase CANCELLABLE rather than joinable-only: a shutdown flag
     checked before each chase starts its compile retires the queue
     instantly, leaving only an in-flight compile to wait on. Do NOT simply
     detach — the segfault above is why the join exists. This alone should
     take deltablue from 3705ms to near its 228ms answer time.
  2. Then re-run this A/B. If the chase still cannot win once shutdown
     stops charging for it, default it off and keep ASH_LLVM_CHASE=1 as the
     opt-in for the long-running case it was designed for.

Consequences worth stating:
  * The published deltablue row should NOT switch to hybrid on the strength
    of "hybrid works now". It is correct, and it is 2.4x SLOWER on wall
    than full-jit. An earlier note in this file predicted the opposite; it
    was wrong and this measurement replaces it.
  * If exit stopped waiting, hybrid on deltablue lands near its 228ms
    answer time — the best of the four modes by a wide margin, and roughly
    12x better than the row we publish today.
  * hybrid-llvm's `run` (~813ms) is no better than the interpreter's: pinned
    to the top tier, promotion arrives too late to pay for itself on a
    program this short. Only the Cranelift rung (hybrid-auto, ~57ms) does.

---

## full-JIT startup dominates any short program

Measured 2026-08-22, debug binary, three samples on a loaded box:

    deltablue --mode jit    wall 1827 / 1640 / 1634 ms
      compile               95.0% / 96.5% / 96.5% of wall
      execute               4.42 / 3.57 / 4.10 ms
      jit init              73 / 48 / 48 ms

Compile: mcjit codegen 644ms, llvm middle-end 451ms, llvm lower 367ms over
499 functions, compile pending 59ms, verify 5.5ms.

What this does establish: the wall time is compile, not execution — the
whole-module LLVM compile of the program plus the stdlib it never calls,
standing in front of a run of a few milliseconds.

What it does NOT establish is any comparison against HashLink. The site's
12ms (HashLink/C) and 17ms (HashLink JIT) are release builds on CI
hardware; the numbers above are a debug build on a loaded M1 Pro, and `hl`
does not run on this machine at all, so no same-box comparison is possible
here. "ash executes deltablue faster than HashLink/C" is NOT supported by
this data and should not be repeated until both are measured on one box —
CI or the NUC.

Note also that execute-ms is the LEAST stable figure here: its spread over
the three samples was 21.1%, worse than wall's 11.3%, because absolute
noise is a larger fraction of a small number. Two modes whose execute
times differ by less than ~20% are indistinguishable on this box today. The published row
was full-jit only because hybrid crashed (closed above, 54312ea); with the
mode restriction lifted the site should publish hybrid-auto, which compiles
lazily on background threads — `fib` publishes `compile_ms: 0.64` for the
same reason.

Quote the compile/execute RATIO, not wall. Across those three samples wall
swung 12% while the ratio held within 1.5 points: a ratio of two things
measured inside one run cancels machine noise that wall time absorbs whole.

Open, in order:
  * Nothing lazy about full-JIT: every function in the module is lowered
    before main runs, including the whole stdlib the program never calls.
    HashLink JIT compiles AND runs deltablue in 17ms total.
  * `heaps_game` is still pinned to full-jit for unrelated reasons (its SDL
    loop never returns under an interpreter), so it carries the same shape.
  * Not yet measured in RELEASE. llvm middle-end and llvm lower are our
    Rust code and should shrink a lot; mcjit codegen is LLVM's own and
    should not. The compile share stays dominant either way, but the
    absolute numbers above are debug and must not be quoted against the
    site's release figures.

---

## Conformance: measured per case, and what is left

Superseded 2026-08-22. "The whole main suite is behind this one opcode" was
true of every entry this section ever had, and that was the real problem:
one process for 1195 cases means one crash reports as total failure, so the
number could not move until the LAST crash was fixed. `--isolate` (864e27b)
runs each case in its own process.

First real measurement (interp, 4.3.6, macOS): **937/1195 cases, 78.4%**,
233 failed, 25 crashed. `Assert` is implemented in all four engines
(cd60b89); the other eight blockers found behind it are in the same commit.

Next, in order of value:

  * The crash column is mostly ONE structural gap: "hlp_throw called
    without active trap; aborting". The interpreter installs no trap around
    native calls, so any `hl_error` raised inside a native takes the process
    down instead of propagating as a catchable exception the suite would
    record as an ordinary failure. `call_compiled_function` already does
    this correctly and is the model.
  * 233 failures, 191 of them `unit.issues.*`, holding 2634 assertions of
    which only 397 are bad — most cases are close, not broken.
  * threads — std/src/obj.rs "Failed to allocate memory" in hlp_get_obj_rt
    on a spawned thread; the EventLoop/fiber area MEMORY.md already flags.
  * sys — SIGSEGV at fault_addr=0x30; also fails under stock HashLink here
    (helper-spawning suite), so attribute before fixing.
  * Isolation is unit-only so far. sys and threads still report whole-suite.

## `--mode jit` is not checked against anything

`AshMode` in the parity matrix is `Interp | Hybrid`. The standalone
whole-module JIT is never compared to an oracle, which is how an
always-false `String ==` survived in it (fixed in e7f7fec — all three
compiled tiers gated `hlp_dyn_compare` on HDYN/HNULL and let HOBJ fall
through to pointer identity).

Measured across the 45-case corpus, `--mode jit` vs the interpreter:

    agrees                        38
    differs, expected (FP fusion)  2   Mandelbrot, MandelbrotSmall
    differs, unexplained           5
    crashes                        0

The two FP cases are intended: compiled code fuses multiply-add, the
interpreter rounds every opcode. The remaining five are worth a lane in the
parity matrix once they are down.

### Map Int-values iteration is wrong under --mode jit — settled and cornered

`for (v in map)` over an Int-VALUED map returns heap addresses under the
whole-module JIT. Settled against stock HashLink 1.15 (agrees with ash's
interpreter to the digit), so this is a jit defect, not interpreter
permissiveness — the earlier open question here is closed.

The discriminator table, from TestMapIterAll (the committed fixture):

    interp                          correct
    hybrid                          correct
    hybrid --jit-threshold 1        correct   <- EVERY function through the
                                                 same LLVM lowering
    hybrid --jit-tier cranelift     correct
    --mode jit                      WRONG, Int values only

String-valued maps, keys(), and keyValueIterator().value are correct even
under jit. Since forcing every function through the identical lowering is
correct while whole-module compilation is not, the defect is MODULE-LEVEL
state only --mode jit sets up (its own constants/type-table init in
jit/module.rs), not the shared per-function lowering — start there, not in
the SafeCast/GetArray emission.

---

## LLVM is queued on a counter, not on demand — 8 of 10 compiles are wasted

Measured at 323cf15. What ash promotes to LLVM and where the demand actually
is are different sets of functions:

    bench          LLVM promoted (counter)      hot loops ELIGIBLE      OSR entered
    deltablue      29, 308, 320, 74             242,245,248,251,252,     324
                                                30, 324
    binary_trees   22, 23, 24, 25               25                       25
    nbody          28, 29                       28                       28

deltablue's two sets are DISJOINT. All four of its LLVM compiles went to
functions with no hot loop, while the one function whose frame actually
entered OSR was never promoted. Ten compiles across the three benchmarks,
two of them for a function with any demonstrated demand.

That is what the profile costs: `beadie-promoter` is **35.7%** of deltablue's
CPU (main thread 49.9%, broker 13.7%) for a program that finishes in 73ms.

ash already computes the demand signal -- it logs loops ELIGIBLE, stages
entries, enters frames. Nothing lets it DECIDE anything; `opt_threshold`
decides, independently, on invocation count.

**Signals that do not separate** (checked, so nobody re-checks):

  - Invocation RATE at the tier-1 request. deltablue's wasteful promotion
    runs at 585 calls/ms; binary_trees' useful one at 387/ms.
  - Elapsed time at the request. fib asks at 1.7ms and needs it.

**The signal that does** is a hot loop in the function, plus an escape hatch
for recursion, which has no loop and still needs the top tier. Counts at the
tier-1 request separate cleanly:

    needs LLVM:  fib 7924, binary_trees 9793/8586/3180, closure_call 2449,
                 method_call 2021
    wasteful:    deltablue 234/503/711, binary_trees 446, nbody 242/682

So: promote when the function has an eligible hot loop, OR when the count is
high enough that recursion-shaped work repays. `opt_threshold` at 250 is why
a 234-call function qualifies today. Note the tension already measured: a
flat threshold at 500+ costs closure_call and method_call 12-18%, because
their SECOND promotion has a low count -- which is the argument for the OR
rather than simply raising the number.

### What demand gating measured, and the two things that constrain it

Implemented and measured. Demand is read off the frame a call is about to
return into: its own hot loop, a self-call, or a caller already 64 back-edges
into a loop of its own. A tier-1 request without demand is refused.

Corpus result over all 61 cases: the baseline compiles 18-22 functions
depending on the sweep, the gate compiles 16 every time. Over three sweeps of
the whole corpus the ONLY promotions it removes anywhere are deltablue's
findexes 74 and 308; nothing else in the corpus loses or gains one. Wall time
is unchanged everywhere within noise (worst +1.0%, and deltablue is no slower
for losing them -- which is the claim in this section, measured from the other
side). Under 7-way CPU contention deltablue is 2.9% faster.

**Demand is only observable while a function is interpreted.** `note_hot_loop`
fires from the interpreter's dispatch loop and `on_invoke` from its call path,
so both go silent the moment a function -- or its CALLER -- runs as compiled
code. `step` in bench_method_call is called 100M times and its bead sees only
the ~4ms before `main` is OSR-compiled. Gating on the function's own loop
alone therefore starved every leaf: method_call +23.2%, closure_call +18.2%,
nbody +15.6%. The caller's back-edge count is what makes a leaf visible, and
it is the load-bearing signal, not a refinement.

**A refused promotion re-proposes with doubling backoff** (beadie aeb9600).
The queued flag now means exactly "in flight or installed": a null compile
lowers it and sets a re-proposal horizon of twice the count it declined at, a
failed submission lowers it, and a swap that finds the bead reloaded lowers
it; jobs carry an epoch so one from before a reset touches nothing. A function
that never earns demand is asked O(log calls) times; one whose demand arrives
while it still ticks is compiled at the first horizon after. Reproduced with a
leaf called 40x per iteration under a slow outer loop (BenchLateDemand): the
latching gate deferred once and capped it at Cranelift for the run; now the
log reads defer -> demand -> install tier=llvm 5ms later, same checksum. ash
memoizes findexes whose LLVM compile HARD-fails (`llvm_failed`), so
re-proposals of those cost a null return, not a recompile behind the global
llvm mutex. `defer` lines can now repeat per findex, and `attempted` counts
each re-ask. What the retry cannot do is act after the function stops ticking
-- all interpreted callers compiled -- which is observability, not policy.

### Demand reaches one frame, not a chain

`under_loop` reads `self.stack.last()` only, and `InterpreterFrame::backedges`
is per-invocation, so a leaf below a non-looping wrapper inherits nothing from
the loop two frames up and can be refused before it ever earns a signal. The
window is narrower than it sounds -- compiled frames are not on the interpreter
stack, so once the wrapper reaches Cranelift the leaf is once again called from
whatever interpreted frame is beneath, and that frame IS the looping one. Two
attempts to build a program that loses measurably this way (mutual fib(38), a
leaf behind an 8-iteration inner loop) came out level with the baseline: the
first because a live-frame mark lands as soon as either partner compiles, the
second because Haxe inlines the leaf. Left as is until a program shows the
loss; the fix if one does is to inherit a flag at frame push rather than walk
the stack, and the risk is that inheriting it grants demand to everything under
a hot loop, which is where this started. Since beadie re-proposes (aeb9600),
a refusal during the blind window no longer caps anything -- the leaf promotes
at the first backoff horizon after its demand becomes visible.

### Hybrid ships three float arithmetics, and the Mandelbrot checksum was the race between them

Found by the first parity-nightly-slow after promotion started firing properly:
hybrid full Mandelbrot printed 112798500 or 112798779 run to run against the
oracle's 112790102. Isolated per engine (all measured, one variable at a time):

    interp                              112790102   rounds every op
    cranelift (AIR serializes Fma back to mul+add)  112790102   bit-exact with interp
    LLVM via AIR pipeline (fma peephole, auto tier) fused, FMF-independent
    LLVM legacy path (--jit-tier llvm, contract FMF) 112798779 on / 112790102 off

`ASH_OSR=0` snapped auto back to 112790102: the fused values enter through the
promoted bodies, and WHICH mixed value a run prints depends on how many pixels
compute before promotion lands -- a checksum that varies with compile latency
can never satisfy an exact expectation. The gate fix: `jit_tier = "cranelift"`
per case in parity_cases.toml pins the two Mandelbrots onto the tiers that
round the way the oracle does, still promoting, still exercising tier-0
codegen and the tiering machinery. The engine fix stays on the roadmap: fusion
by construction in EVERY engine (interpreter included, `Fma` executed as
f64::mul_add), at which point the pins and the oracle policy move together --
and the legacy llvm path's contract FMF should go, since the AIR peephole is
the mechanism that keeps engines agreeing about WHICH pairs fuse.

### beadie reads generation as tier, and OSR attach bumps generation

`swap_compiled_with_osr` bumps the bead's generation, and `maybe_promote`
computes the next tier as generation+1. So an OSR-entry attach at tier 0 makes
the two-tier ladder read "already at tier 1" and never propose LLVM for that
bead -- the promotion machinery, re-proposals included, is unreachable behind
it. Pre-existing (unrelated to the queued flag) and empirically the hot-loop
benches still reach tier=llvm, because their tier-1 queues before any attach;
the exposure is a header that turns hot late at tier 0. The fix belongs in
beadie: promotion state and OSR-table versioning want separate counters.

### The gate abstains under ASH_AIR=v2, because it cannot see there

`ssa_loop` counts no back-edges and probes no hot loops, so under the opt-in
v2 interpreter none of the three signals exists. Gating on absent evidence cost
deltablue 94ms -> 400ms. Counting back-edges in the SSA dispatcher was tried
and is not the answer either: the fix that mattered was ticking the ladder from
loop progress, which took deltablue's tier-1 compiles from 3 to 11 and the run
to 412ms, because each promotion drags an AIR prepare onto the mutator. So the
gate returns early when `ssa::enabled()`, leaving v2 exactly as it was. Give v2
back-edges before giving it the gate.

### CI cannot compare milliseconds across sweeps, and said otherwise

Every benchmark is its own matrix job on its own runner (`bench.yml:186-205`),
so a sweep is nine machines, not one -- measured across 91 push sweeps, a mean
of 3.4 distinct CPU models each, drawn from six models in the pool. The merge
then published ONE cpu_model for the whole page: `merge_bench_site.py` took
`ash_docs[0]`, and the sorted glob plus `merge-multiple: true` makes that
always `ash-binary_trees.json`. Over 63 sweeps, a given row actually ran on the
advertised CPU 26-41% of the time; 322 of 448 non-binary_trees legs did not.

What that cost: closure_call read 263 -> 144 -> 162ms over three commits and
looked like a win then a regression. Per-leg, those ran on an EPYC 7763, a
Xeon 8573C and an EPYC 9V74; hashlink-c, whose code never changed, read
158 -> 121 -> 178ms on the same three legs. Against it: 1.67 -> 1.18 -> 0.91,
monotonic, and ash is now faster than AOT C on that benchmark.

**No ratio rescues cross-sweep comparison in general.** Take a ratio where BOTH
engines are frozen (hashlink-hl2 / hashlink-c): it still moves up to 2.03x
across runs, because the JIT engines and the AOT build respond to a host by
different factors (that ratio is 1.03 on an EPYC 9V45 and 1.90 on a Xeon
8573C). Best denominator measured is geo(jit, hl2, c) at 1.066 mean spread and
1.171 worst; raw ms across sweeps is meaningless below ~45%, and 54% of the
5%+ movements it reports have the wrong sign. WITHIN a row the ratios are
sound -- ash and all four reference engines are timed in the same job on the
same VM -- which is why the ash/hashlink-c column is the one to read.

Fixed: the merge carries each leg's machine onto its own benchmark entry, the
sweep-level `cpu_model` is published only when every leg agrees (otherwise
`runners` lists the models), the site labels each number with the CPU that
produced it, and the copy no longer claims "same machine" across rows.

**Still open, and the thing that would actually answer "did this commit
help?":** an in-sweep A/B -- build HEAD^ (or a named ref) alongside HEAD in the
existing build job, ship it as `ash-base` in the toolchain artifact, and time
both interleaved in the same leg on the same VM. Roughly +2 minutes of sweep
wall time, no new jobs. Until that exists, a local interleaved A/B is the only
trustworthy verdict on a performance change.

### closure_call's 19.7% SD is the same coin, and the private-module cure is measured worse

CI: median 316.9 / min 196.4. Reproduced under 9-way local load: llvm=2 runs
sit at ~175ms, the occasional llvm=1 run at ~257-305ms -- the modes are which
of the TWO tier-1 compiles landed before 100M iterations ran out. Timeline of
a clean run: the leaf (findex 22, 10 ops) is requested at 0.5ms and compiles
in ~7ms in its own module; the loop owner (23) crosses its threshold at 7.6ms
and takes ~23-40ms in the SHARED module -- middle-end over the 7-function
callee cluster (~17ms) plus MCJIT object emission (~10ms), serialized behind
the leaf on the one promoter and the global llvm mutex. Ladder complete at
~48ms of a 165ms run locally; on 4 contended vCPUs the compile balloons
superlinearly and usually misses, which is the 317ms median.

**Falsified fix, do not retry:** compiling indirect-call loops in a private
module (CallMethod/CallClosure no longer forcing the shared path) cuts the
compile but costs +29.2% closure_call / +32.1% method_call STEADY STATE --
the shared module is where the middle-end sees the callee bodies, and that
is worth more than the latency. The doc on `promotion_wants_full_module`
said 10-18%; it is worse than documented.

**Root cause found and fixed.** The CI bench JSON showed the truth the local
latency theory missed: the leaf's LLVM compiled in 8ms on CI -- main's tier-1
was never PROPOSED at all (attempted=3: two tier-0, one tier-1). The ladder's
fast door is the mechanism: cranelift installs at ~1ms, the OSR entry attaches,
the frame transfers, and from that moment the interpreted ticks that counter-
based proposal runs on stop. Whether tier-1 was proposed is a race between the
count reaching threshold (200 ticks = 12.8k iterations) and the transfer --
won on a fast interpreter, lost on CI's, hence the machine-split modes. The
attach's generation bump seals it. Fix: try_osr_transfer force-promotes tier 1
at the moment the frame steps through the fast door -- the transfer IS the
demand, and it is the last signal the interpreter will ever emit for that
findex. Verified by disabling the counter path outright (ASH_TIER1=100000):
before, the transfer strands the run at tier 0 exactly like CI; after, the
log reads osr-transfer proposes -> install tier=llvm. Under 9-way load the
median drops 235 -> 193ms; steady state unchanged across the suite.

Residual tail, separate cause: ~1 run in 30 both compiles slow together
(f22 6->84ms, f23 37->70ms in the same run) -- the broker thread itself
starved, likely E-core/QoS scheduling of beadie's promoter thread. A QoS/
priority hint on the promoter spawn is the lever if it matters on CI.

Levers not taken (latency, not correctness): a second LLVM context to overlap
compiles; one cluster promote that installs the leaf from the shared module
it was already lowered into (the leaf is compiled twice today); a cheaper
pipeline than default<O2> for promote-sized modules.

### method_call reports two different numbers, and which one is a coin flip

Noticed while A/B-ing the gate, present at HEAD and unchanged by it. Over 20
runs of bench_method_call the LLVM set comes out one of two ways, because
`main` is called once and reaches tier 1 only on back-edge ticks -- so whether
its compile lands before a 100M-iteration loop ends is a race:

    llvm={23,26}   14/20 runs   min 151ms  median 157ms
    llvm={23}       6/20 runs   min 196ms  median 204ms

The two modes are 30% apart, so report the mode split rather than a single
number. Note what this does NOT explain: with a 14/20 split a min-of-N lands
in the fast mode almost surely, so the same binary reading +0.7% on one sweep
and +7.9% on the next is ordinary between-sweep drift of the fast mode's own
minimum (base alone came out 136.4, 141.0, 143.7 and 151ms), not a mode
mixture. Either way a sub-10% delta on method_call means nothing without
several sweeps, and this is the first thing to check when it moves in CI.

## The scoreboard moved: HL/C and the JVM are the targets now

Latest CI medians, ash against each engine (within-row, so machine-independent):

    bench          ash      HL/C     hl2      hxjvm    vs HL/C       vs JVM
    fib            23.8     169      467      348      7.10x faster  14.63x
    binary_trees   560.5    3452     3554     210      6.16x faster  2.67x SLOWER
    nbody          641.1    964      1315     984      1.50x faster  1.53x faster
    mandelbrot     390.6    -        42749    788      -             2.02x faster
    method_call    121.0    117      199      151      1.03x slower  1.25x faster
    closure_call   136.9    134      248      154      1.02x slower  1.13x faster
    free_call      119.1    96       200      146      1.24x slower  1.22x faster
    inlined_call   119.6    96       203      150      1.25x slower  1.25x faster
    deltablue      84.2     14       23       87       5.84x SLOWER  1.03x faster

ash now leads the JVM on 8 of 9 (binary_trees is the exception) and has passed
hl2 everywhere. Against HL/C it trails on five rows, but four are within 1.25x
and one is not: deltablue carries essentially the whole remaining gap.

**deltablue is not a "JITs cannot win short programs" problem.** hl2-ir is also
a JIT, on the same bytecode, and finishes in 23ms; ash is 3.66x that. The JVM
is slow here too (87ms), which is interesting but is not an excuse -- HL/C and
hl2 both demonstrate the workload is winnable. Local recon (54.5ms here
against CI's 84.2): `air prepare` on the MUTATOR is 12.27ms over 55 calls
(22.5% of wall), the interpreter is ~26% of samples (execute_opcode 13.0%,
execute_hl_function 7.4%, op_field_set 5.6%), and only 1.9% of samples land in
compiled code at all despite 38 Cranelift and 5 LLVM compiles, with tier-0
still firing 16-18ms into the run. Sized separately: dropping the
per-native-call `sigprocmask` (sigsetjmp savemask 1 -> 0) is worth -3.9% here
and ~0 elsewhere, and needs the signal unblocked on the recovery path first.

## binary_trees: the JVM's 2.84x, measured rather than assumed

ash ~597ms against Haxe/JVM's ~210ms in CI, a ratio that holds across three
sweeps on three CPUs (and ash beats every HashLink variant on it by 5.6x, so
this is an ash-vs-JVM gap specifically). The benchmark allocates 902MB in
short-lived trees against a 12-18MB live set -- 97% of every trigger is
garbage.

**Fixed: the marker allocated a Vec per marked line.**
`mark_allocation_at_line` returned `Vec<(usize, usize)>` and every caller
immediately `extend`ed it away -- 1.97M malloc/free pairs per run for a median
of ONE element each. Threading the caller's accumulator through the five call
sites: GC pause 106.4 -> 48.9ms, max pause 7.45 -> 3.68ms, wall -12.7%
(436.5 -> 381.1ms). Collections, blocks reclaimed and live set are identical
run to run across both binaries (19 / 28143 / 586), so marking behaviour is
provably unchanged. No other benchmark moves.

**Measured dead ends, so nobody spends a week on them:**

  - *Escape analysis / scalar replacement: worth exactly zero here.* The trees
    genuinely escape; the JVM gains nothing from it on this benchmark either.
  - *Tier latency: ~0ms.* Every hot function reaches LLVM inside the first
    29ms of a 442ms run; nothing is stuck in the interpreter or on Cranelift.
  - *Mutator codegen quality: at most 10-15ms.* ash's LLVM tier is already
    good enough here; this is not a codegen problem.
  - *Inlining the TLAB bump into JIT-emitted code the way C2 does.* Two
    independent lenses put this between 0 and 13ms, not the 31% the profile's
    allocation bucket suggests: ~87% of what the sampler charges to
    `gc_alloc`/`hlp_alloc_obj` is work that survives inlining and merely moves
    into the mutator. The allocation path is not instruction-throughput-bound.
  - *Per-object zeroing instead of the TLAB's bulk `__bzero`: COSTS 28-39ms.*
    The refill-time zero is already the cheapest form and is not deferrable
    without a write barrier.

**The growth-factor knob was mostly an artifact of the Vec bug.** The earlier
sensitivity (x4 -> x32 taking pause 109 -> 23ms) was largely measuring malloc
churn proportional to marking work. Re-measured after the fix, min wall / GC
pause: x4 379.0ms / 48.8ms, x8 368.0 / 30.6, x16 371.6 / 24.2, x32 377.0 /
13.4. So pause keeps falling all the way to x32 while WALL TIME turns around
after x8 -- the pause it removes is replaced by page-reclaim cost. x8 is worth
about 3% here and costs heap headroom; x16 and beyond are a loss. Whatever is
chosen, choose it on wall time and RSS, never on the pause number alone.

**Fixed: an LLVM OSR entry for a late header stalled the mutator.**
`late_osr_entry` compiled a promote-sized LLVM entry synchronously on the main
thread -- the profiler charged one call 42ms of a 442ms run. It is an UPGRADE,
not a rescue: the Cranelift door (~1ms) has already taken the frame out of the
interpreter, and since entries for headers probed before the promote are built
ahead of it, nothing on the corpus depends on the late LLVM one. Dropping it
is worth 10.3% on binary_trees with every other benchmark inside 0.7%.
`ASH_LATE_LLVM_OSR=1` restores it. Measured bound on OSR overall, for
perspective: with `ASH_OSR=0` method_call is 127x slower and closure_call
103x, while binary_trees is 10% FASTER -- OSR is load-bearing everywhere
except a loop whose body is entirely calls into already-promoted functions.

**The macOS win did not transfer, and the reason generalises.** Local
(M1 Pro) said -21.7%; CI (Linux, EPYC 7763, same CPU model both sweeps) said
-7.0%. Decomposed on real Linux (NUC i5-1250P, ABBA-interleaved n=18, both
binaries built there):

    neither fix        451.3ms min
    GC Vec fix only    439.4ms   -2.6%   (macOS: -12.7%)
    both fixes         413.4ms   -8.4%   (macOS: -21.7%)

CI's -7.0% agrees with Linux's -8.4%. The Vec churn was ~5x more expensive on
macOS than on Linux: `_xzm_xzone_malloc_tiny` charges far more for a tiny
alloc/free pair than glibc's tcache does. GC pause still halves on Linux
(72.7 -> 52.8ms), so the fix is real there -- it is the SIZE that did not
transfer, because the thing being removed was an allocator cost and the two
allocators are not comparable.

**Rule this establishes: never predict CI from a macOS measurement of an
allocator- or syscall-bound change.** Measure it on the NUC first
(`~/ash_ab`, build script `~/gc_ab.sh`). Compute-bound changes still transfer.

**CI now publishes collector numbers.** `bench.yml` passes `--gc-stats`, the
merge carries `{collections, pause_total_ms, pause_max_ms, bytes_allocated_mb,
live_blocks}` onto the ash row, and the site shows them in each number's
tooltip. They ride `run_instrumented`'s separate run, so the timed numbers are
untouched -- but that run also carries `--jit-log`, so its pause reads higher
than a clean one (75ms vs 49ms on binary_trees). Compare them across sweeps,
not against a local clean run.

**Still open, in order of measured value:** the pause is 83% transitive
tracing (per cycle: rootscan 0.03ms, trace 5.06ms, sweep 0.90ms), so tracing
is where any further collector work belongs; `sweep()` calls
`std::env::var("ASH_GC_SWEEP_AUDIT")` once per freed block, ~1,550 getenv
calls per cycle each taking the macOS process-wide environ lock (~7ms);
roughly 10 of the 59 instructions per allocation re-decide facts that are
constant for the whole run and are removable in Rust alone; and ash's objects
are 33% larger than the JVM's, a bandwidth tax on both allocation and zeroing.
A young-only collector fits the 99.7%-young-death shape but is unsound in ash
today (conservative roots, no write barrier) and is worth less once the Vec
churn is gone.

## Dispatch: the JVM leads closure_call and method_call, and inlining is why

Haxe's own JVM target is a benchmark lane as of `fc5c0bd`, and it beats ash
on exactly two call shapes (CI, EPYC 7763, Temurin 21, 100M iterations):

    closure_call   ash 317.7ms   jvm 150.4ms    3.18ns vs 1.50ns per call
    method_call    ash 239.6ms   jvm 150.9ms    2.40ns vs 1.51ns per call

ash wins fib 14.4x, mandelbrot 2.1x, nbody 1.5x, inlined_call and free_call.
The losses are indirect dispatch and nothing else.

**Target, falsifiable:** closure_call and method_call converge on
inlined_call (locally 171.5 / 140.5 against 119.1). If speculating does not
move them toward that number the model is wrong and this is abandoned, not
patched.

**What is already ruled out.** The `vclosure` field loads are not the cost:
marking all three `!invariant.load` hoists them out of the loop (confirmed in
IR) and buys +0.1%. Nor is it a stale closure pointer -- the tier-attributed
profile shows `Fun_22 [llvm] 19.2%` against `[cranelift] 11.5%`, so the
closure does reach LLVM code. What costs 3.18ns is the indirect call itself,
which the JVM does not make: a monomorphic site gets an inline cache and the
callee inlined.

**Mechanism — IMPLEMENTED.** Profile-guided guarded devirtualisation, LLVM
tier. The interpreter records what every CallClosure/CallMethod site actually
called (`ash_core::callsite_profile`, keyed by (caller findex, opcode index)
into the shared optimized ops array both sides use); the lowering emits a
guarded fast arm for monomorphic sites, with the whole existing indirect path
as the miss arm, so a wrong profile costs one compare:

  - CallClosure guards on the fun field against the CONSTANT `target + 1` --
    the sentinel form is never patched, which makes the closure's identity a
    compile-time immediate -- plus a hasValue match.
  - CallMethod guards on the receiver's type header against the recorded
    `hl_type*` -- the header never moves, unlike the vtable SLOT, which
    promotion patches.

Both fast arms are direct calls the inliner takes (verified in the OSR body's
IR: `cm_devirt_hit` is the callee's mul/srem/add, no call). Measured:
closure_call 161.8 -> 137.9 (-14.6%), checksums identical, corpus clean in
default, v2 and promotion-heavy modes. method_call stayed ~131: its indirect
path was already branch-predicted cheap, and its wall is dominated by the
CRANELIFT phase -- the frame runs the tier-0 OSR body until the promote and
its OSR entry land (~35ms warm; the first run after a build pays a ~50ms
cold-start that repeatedly masqueraded as a structural asymmetry). The
`!invariant.load` on the guard loads is in place; it buys nothing while the
cranelift phase dominates.

**The entry is built BEFORE the function it belongs to** (superseding the
ride-along attempt, which was cost-neutral and is gone). Timeline that
settled it, method_call: cranelift installs at ~1ms, the frame transfers at
~1.5ms, and the LLVM promote is requested at 6.8ms but takes ~36ms -- so the
frame ran the middle tier until ~43ms of a ~131ms run. A frame that has
already transferred can only leave through an OSR entry; the promoted body is
for FUTURE calls, and a loop owner like `main` is called once, so for the
frame running right now the body is worth nothing and the entry is
everything. Building the entry into its own small module first publishes the
re-tier slot in a fraction of the promote's time: closure_call -5.1%,
method_call -3.7%, everything else within noise. The promote itself is
unchanged in cost -- what changed is that nobody waits for it.

Promote composition, warm, for anyone attacking the remaining latency
(2 promotes, method_call): mcjit codegen 17.2ms, OSR entry build 10.7ms,
middle-end 10.0ms, lower 1.0ms.

**Falsified while chasing method_call, do not retry as-is:** a "cluster"
promote -- private module carrying the profile-named hot callees, so the
inliner still sees them while MCJIT stops re-emitting the shared module. The
theory was the documented "35-71ms of codegen per promotion" for the shared
module; the measurement was neutral everywhere (+-2%), INCLUDING deltablue's
11 promotes. At this scale there is nothing accumulated to re-emit, and the
promote's cost is its own three phases above, not the module's other
tenants. Revisit only with a program whose shared module is genuinely large,
and measure the re-emission directly before building anything.

**Next lever for method_call**, still open: the cranelift-phase dispatch.
The frame now leaves the middle tier much sooner, so the window is smaller
than the ~40% measured before -- re-measure the split before spending on it.

**The AIR says most of this needs no profiling at all.** Dumping the two
losing benchmarks (`ASH_AIR_DUMP`) shows the target is already recoverable
by reaching definitions, in both cases:

    closure_call, findex 23          method_call, findex 26
      3 StaticClosure fun=RefFun(25)   3 New dst=Reg(3)        (type A)
      5 StaticClosure fun=RefFun(22)   7 New dst=Reg(0)        (type B)
     12 CallClosure   fun=Reg(0)      15 CallMethod field=0 args=[Reg(0)..]

`Reg(0)` is a phi of two definitions whose targets are compile-time
constants -- two `StaticClosure` with their findex in the instruction, two
`New` whose register type names the class, so the vtable slot resolves. The
optimized dump still emits `CallClosure { fun: Reg(0) }`, so no pass looks.

That makes the FIRST mechanism a static AIR pass, not an inline cache, and
it is better on four counts: no feedback plumbing, no `(findex, pc)` keying,
no cross-thread publish, and it fixes **Cranelift as well as LLVM** because
it rewrites the IR both tiers consume. Where a definition is a single
`StaticClosure` it needs no guard at all -- it is a proved fact, not a
speculation.

Everything AIR needs is in place: `Instr::StaticClosure { dst, fun: usize }`
carries the findex, v2 is SSA with `Phi`, and `passes/inline.rs` already
exists to reuse.

Runtime feedback stays in the plan, demoted to the fallback for sites whose
definitions genuinely are not recoverable -- a closure out of a field, an
argument, a collection.

**Stages, each with an exit criterion.**

0. DONE. Forcing a direct call at the site (unguarded probe, since removed)
   gives closure_call 167.3ms against 138.4ms, **-17.3%**, checksum
   unchanged, with `inlined_call` at 117.9ms for reference. The IR settles
   what happened: the loop body becomes

       %mul.i = mul i32 %reg_18, 31
       %smod.i36 = and i32 %reg_5, 7
       %add.i = add i32 %smod.i36, %mul.i

   -- `step` fully inlined, no call left. So the mechanism works end to end
   and the ceiling is real. Two things it also settles: the residual 20.5ms
   against inlined_call is NOT dispatch (both loops are pure arithmetic by
   then, 0.2ns/iteration apart), and -17.3% is the honest expectation for a
   guarded version, not the 2x the JVM comparison might suggest. CI's
   closure_call is 2.5x its own inlined_call where the Mac's is 1.4x, so
   whatever else is slow there is a separate question.
1. DONE, as a survey rather than a rewrite. `ASH_DEVIRT_SURVEY=1` counts,
   per function, how resolvable each dispatch is. The answer redirects the
   work:

       bench            closure single/phi/unres   CallMethod sites (distinct)
       closure_call            0 / 1 / 0                  0
       method_call             0 / 0 / 0                  1  (1)
       deltablue               0 / 0 / 2                 27 (25)
       test_stdlib             1 / 0 / 6                  7  (7)

   The whole corpus holds TWO resolvable closure sites, and one of them is
   inside the benchmark written to measure closures. A CallClosure pass
   would optimise its own benchmark. Do not write it.

   CallMethod is where the volume is: deltablue -- the row that is 73.7%
   interpreter and the worst against rayzor -- has 27 sites across 25
   distinct (receiver type, slot) pairs.

2. IN PROGRESS. `ModuleInfo::method_target(ty, slot)` resolves a proto slot
   against the receiver's static type (walking `super_` for an inherited
   implementation, matching on `pindex` the way the JIT reads the slot).
   With it the survey answers how much the pass could reach:

       deltablue      27 CallMethod sites, 27 resolved (100%)
       method_call     1 site,              1 resolved
       test_stdlib     7 sites,             3 resolved (43%)

   All of deltablue's dispatch is statically nameable. What remains is the
   rewrite: `GetType` the receiver, `TypeConst` the static type, `CondJump
   Eq` to a direct `Call` on the hit and today's `CallMethod` on the miss,
   with a phi for the result. Every one of those exists in AIR v2 already
   and all of them serialize back to HL opcodes, which matters because the
   interpreter consumes the same IR.

   WRITTEN AND REVERTED. The pass was built exactly as described --
   `GetType` + `TypeConst` + `CondJump Eq` into a direct `Call`, with the
   original `CallMethod` as the miss arm and a phi for the result -- and it
   is correct: every checksum matched in both interp and hybrid, once it
   learned to refuse two things it first got wrong (a result in a PINNED
   register, which goes through a cell that a phi bypasses, and a site
   inside a trap region, whose handler edge the split does not fix up).

   It is also a large regression:

       deltablue    auto        73.3ms -> 1015.7ms   (+1286%)
       method_call  auto       140.4ms ->  209.6ms   (+49%)
       method_call  cranelift        -13.6%

   The mechanism is the miss arm. It leaves a `CallMethod` behind by
   construction, Cranelift's opcode gate refuses functions containing one,
   so the tier-0 attempt declines and the ladder falls back to LLVM: six
   LLVM compiles appeared on a 73ms program. Speculating cost more than the
   dispatch it removed.

   Two things chasing that turned up, both worth knowing before anyone
   tries again:

   - Cranelift's gate (`is_cranelift_lowerable`) is an ALLOWLIST that
     excludes several things its AIR codegen implements -- `CallClosure`,
     `StaticClosure`, `InstanceClosure`, the `Ref`/cell family -- the same
     staleness its own doc records for `Field`/`SetField`. Admitting
     `CallMethod`, `CallThis`, `CallClosure`, `StaticClosure` and
     `InstanceClosure` is CORRECT (11 programs, every checksum) and changes
     NOTHING measurable: identical Cranelift coverage on deltablue (38
     installs, 1 decline), closure_call and method_call. Not the blocker.
   - deltablue's method-calling functions are already Cranelift-compiled:
     findexes 245, 247, 248 and 249 all carry `CallMethod` sites and all
     install. So its 73.7% "native" is NOT uncompiled code waiting for a
     tier -- it is time in ash's own runtime helpers, which is a different
     problem from dispatch and wants its own measurement.

   A future attempt has to eliminate the miss arm rather than add one --
   which needs the receiver's type proved, not guarded, and that is the
   whole-program analysis this design was chosen to avoid. Every AIR value carries its
   HL type (`ValueData { ty }`), so the receiver's static type plus the
   field slot names a target; guard on the receiver's RUNTIME type pointer
   against that static type, direct-call on the hit, vtable dispatch on the
   miss. Sound whatever the hierarchy does, monomorphic sites always hit,
   and the direct arm is what LLVM can inline.
   Exit: method_call and deltablue move, and they move under
   `--jit-tier cranelift` too -- that is the proof the rewrite landed in the
   IR rather than in one backend.
3. Runtime feedback per `(findex, pc)` at the interpreter's `CallClosure`
   (interpreter.rs:4855) and `CallMethod` (:4844) for the sites step 1 and
   2 cannot resolve statically: observed target, or MEGAMORPHIC once a
   second appears.
4. Publish it through `TieredSharedCtx`, which already carries
   `pending_osr` across that boundary, and speculate in the LLVM lowering
   (function.rs ~3588).

**Risks.** (a) The guard constant may never match: a closure captures
`functions_ptrs[T]` at creation and `install_function_address` patches no
existing `vclosure`, so a closure older than its target's promotion holds a
stale address. Stage 0 measures the hit rate; if it bites, the answer is a
self-updating cache that patches `vclosure.fun` on a miss -- which is also
why `!invariant.load` was reverted rather than kept for free. (b) LLVM must
have the callee as a body, not a declaration; a closure call is an
un-inlined hot-path call so `promotion_wants_full_module` keeps the shared
module, but assert it rather than assume it. (c) Polymorphic sites fall back
to today's path: no gain, no regression. (d) Correctness is cheap -- exact
pointer equality, miss takes the existing path, no deopt and no new
invariant.

This is NOT the binary_trees fix. That one is GC, the JVM leads it 2.83x,
and it needs its own plan.

## Where the time actually goes

Measured with the built-in profiler (`ASH_PROFILE=sample`, see the README), so
these are shares of observed execution rather than estimates. Both rows are the
whole-program JIT, where every function is LLVM-compiled at `-O3` and nothing
is left interpreted — that is, the best code ash currently produces. Debug and
release builds are given side by side because the obvious objection to the
first column is that it was taken at `opt-level = 1`.

| | nbody (dbg) | nbody (rel) | mandelbrot (dbg) | mandelbrot (rel) |
|---|---|---|---|---|
| Generated code (`llvm`) | 45.6% | 48.5% | 15.4% | 16.5% |
| `hlp_get_obj_rt` | **44.8%** | **43.3%** | 13.7% | 14.0% |
| GC (mark, sweep, allocate, lock, `madvise`) | ~0% | ~0% | **53.6%** | **51.8%** |

**A correct release build does not move any of this** — the end-to-end times
land between 0.98× and 1.04×, and the shares above shift by at most three
points. That is the expected result once the costs are named: optimization
level cannot delete a call, a mutex, or a heap line. `hlp_get_obj_rt` is about
five instructions behind a cached pointer, so compiling it better is worth
nothing against 1.76 billion invocations of it; the allocator takes a reentrant
mutex per allocation; and a 24-byte `Complex` occupies a whole 128-byte GC
line either way.

The two benchmarks isolate different costs, which is why both are worth
keeping. nbody's hot loop only reads and writes fields of long-lived objects,
so it measures field access alone: nearly half of it is one runtime call per
field access. mandelbrot allocates two `Complex` values per inner iteration, so
it measures allocation, and the collector costs more than three times what the
generated code does.

The consequence for prioritization is that **codegen quality is not currently
the limiting factor for either**. If ash emitted perfect machine code and
changed nothing else, mandelbrot would still spend 84% of its time exactly
where it does now. The three items that would move these numbers —
[static field-offset GEP lowering](#jit--tiering), scalar replacement so the
`Complex` allocations never happen, and the [GC](#gc) work below — are already
listed separately; this section exists to rank them.

Getting the release column required fixing the release build first, which had
been producing an optimized compiler around an unoptimized runtime. Three
defects, all now closed: `[profile.release]` lived in `crates/ash/Cargo.toml`
rather than the workspace root, so cargo discarded it with only a warning;
`ash_std` was not built for the release profile, so the embedded runtime came
from the debug tree (the build script now says so loudly instead of falling
back in silence); and once fat LTO actually applied, every release binary died
at startup with `JIT has not been linked in.` because nothing referenced
LLVM's MCJIT registration object and the linker collected it — fixed by
calling `link_in_mc_jit` explicitly.

---

## AIR v2

The typed phi-SSA IR (`crates/air/src/v2`) is in place with lowering,
verification, a serializer back to standard HL bytecode, a native-import
declaration table, loop and alias-class analyses, and a pass manager running
null-check elimination, GVN/CSE, LICM, the FMA peephole and DCE at O2, plus
tail-recursion elimination, inlining and scalar replacement of aggregates at
O3. v1 remains the production path until the backends switch over.

**Round-trip status: 19623 of 19623 functions**, over all 43
`crates/ash/test/tests/*.hl` plus `examples/heaps_base2d/bin/game.hl` (5094
functions, 1129 object types), at O2 via `ASH_VERIFY_AIR=only`. Every function
lowers, verifies, optimizes, re-verifies and serializes; no function needs
refusing. The identity round trip (lower → serialize, no passes) changes
opcode counts on 19 of those 5094 functions, and every delta is an accounted
normalization: the `GetThis`/`SetThis`/`CallThis` rewrites (matched
loss/gain pairs), zero-offset `JAlways` elision, unreachable `EndTrap`s after
a `Throw`, and one genuine self-move (`Mov r131, r131`, findex 5470).

Three defects were found by that sweep and fixed; each is worth knowing about
because two of them changed what the IR *means*, not just what it accepts:

- **GVN leaked value numbers across dominator-tree siblings** (883 of the 931
  failures). The dominator-scoped table only ever removed the keys a block
  added; when a block *shadowed* an existing key — which happens whenever a
  reuse is refused, e.g. a load clobbered on the way in — the shadowing
  binding survived the block's `Undo` and a sibling subtree then rewrote uses
  to a value that does not dominate them. The table now records what each
  binding displaced and restores it, unwinding in reverse.
- **`OEndTrap`'s operand is a bool, not a register.** It is Haxe's
  `OEndTrap of bool`; hashlink's `jit.c` never reads it and simply pops
  `trap_current` to its `prev`. Across the corpus it only ever holds 0 or 1.
  Lowering had been reading it as the exception register, which failed
  outright when it named an unpinned register (41 functions) and silently
  paired the wrong regions when it named a pinned one (51 functions). The
  region an `EndTrap` closes now comes from the trap stack, and the flag is
  carried through `Instr::EndTrap::flag` so serialization stays byte-exact.
- **`OMov` between differently-typed registers is an unsafe cast.** HL's
  `OMov` is an untyped register move — `jit.c` handles it in the same switch
  arm as `OUnsafeCast`, and both ash engines implement the two identically.
  Haxe emits it across reference types; every mismatched `Mov` in the corpus
  is `HOBJ`→`HOBJ`, `HOBJ`/`HVIRTUAL`/`HDYNOBJ`→`HDYN`, never a width or
  scalar change. Lowering it to `Instr::Copy` claimed src and dst share a
  type, which the verifier rightly rejected (41 functions) and which would
  have let copy propagation hand every use of `dst` a value of the wrong
  type. It now lowers to `Cast { UnsafeCast }`.

- **The serializer normalizes a type-changing `Mov` to `UnsafeCast`.** One
  more entry in the documented normalization list, and the only one that is
  not opcode-count-neutral in kind. Safe because all three engines already
  implement the two identically, but it does mean the identity round trip is
  not byte-exact for those 41 functions.
- **`ash_interp` treats `OEndTrap`'s operand as a register and nulls it**
  (`interpreter.rs`, `Opcode::EndTrap` → `registers.set(exc.0, null)`). Since
  the operand is a bool flag, this clears r1 (or r0 when the flag is 0) after
  every `try` block. It has not bitten the test corpus — Haxe evidently keeps
  nothing live in r0/r1 across an `EndTrap` in these programs — but it is a
  live miscompile waiting for a program that does, and it is why the AIR
  serializer preserves the flag verbatim rather than inventing a register.
  The JIT (`jit/function.rs`) correctly ignores the operand.
- **`air` cannot tell a pointer type from a scalar one.** `ModuleInfo` offers
  `is_float` and nothing else, so v2 cannot itself distinguish the benign
  reference-upcast `Mov` above from a hypothetical width-changing one; it
  models both as `UnsafeCast`, which is what HL does in either case. A
  `ModuleInfo::kind_of` would let the verifier hold a real rule here, and
  would also serve the unbox/box-forwarding passes below.
- **Remaining optimization passes over the typed IR**, in payoff order
  established by the mandelbrot trace: static field-offset resolution,
  bounds-check elimination, box/unbox forwarding. (Field-load GVN with the HL
  alias classes, dominance-based null-check elimination, LICM, tail-recursion
  elimination, inlining and scalar replacement have landed.)
- **`Function::float_types` needs module info.** A `TypeRef` is an index into
  the module's type table, so `air` cannot tell floats apart on its own: the
  embedder answers `ModuleInfo::is_float`. Lowering through the bare `lower()`
  wrapper records no float types, which makes the FMA peephole inert — the
  JIT must lower through `lower_with`/`ModuleBuilder` to get fusion.
- **The FMA peephole is same-block only.** Sinking a multiply into a
  dominated block is legal for a pure operation but is refused today, so a
  product computed in a loop preheader and consumed in the body does not
  fuse. It also refuses both negating forms when an operand register happens
  to be the product's register, rather than allocating around it.
- **Live-range extension uses per-value private registers.** Any pass that
  lengthens a value's live range appends a register for that value instead of
  running a real allocator, and refuses outright when the value is a `Param`.
  Correct and cheap, but it grows the register table and turns elided copies
  into real `Mov`s on the serialize path.
- **LICM skips loops whose entry edges cross a trap-region boundary** (and
  loops whose header is a trap handler). A preheader has to share the
  header's handler, so a loop opened by a `Trap` terminator never gets one.
- **GVN's clobber-free check is quadratic.** Each `ReadMem` candidate walks
  the whole reachable-and-reaching block region. Fine at fixture scale;
  a per-class memory-SSA numbering would replace it if it shows up in
  compile-time profiles.
- **Scalar replacement covers `New` and enum payloads, not boxes.**
  `vvirtual` boxes and `ToDyn` boxing still allocate on every iteration of a
  de-abstracted loop; both would need their own accessor model, since neither
  addresses storage by `(object type, field slot)`.
- **SROA is all-or-nothing.** An allocation whose pointer escapes anywhere is
  left completely alone, even when most of its fields are only ever read and
  written locally. Partial scalarization would have to keep the object's
  memory and the promoted values coherent at every point the pointer is
  visible, which is a different (and much larger) transform.
- **SROA cannot name an object's initial state**, so a field read on a path
  where nothing wrote it refuses the whole allocation. HL zero-fills a fresh
  object; expressing that needs a typed default the IR has no way to mint —
  the same gap that makes `EnumIndex` an escape (folding a construct tag
  would need an integer constant-pool index).
- **An allocation in a loop body is scalarized a round late.** It is written
  to a register the loop header merges, so the header carries a phi over the
  pointer — an escape — until DCE removes that phi as dead. Pruned SSA
  construction at lowering time, or a phi-transparency rule in the escape
  analysis, would collapse that to one round.
- **Inlining refuses anything involving a trap region or a cell.** A call site
  covered by a handler is refused because the inlined blocks would become new
  exceptional predecessors of a handler that may carry phis; a callee with
  cells is refused because a cell is a frame slot, and materializing it as a
  caller register would share one slot across activations and lose the
  frame's initial value. That last rule also excludes every callee using
  `Incr`/`Decr` or `Ref`, which is broader than it needs to be — only
  `Ref`-taken cells can actually escape an activation. Lifting the call-site
  rule needs the landing-pad design below.
- **Inlining gives every callee register a fresh caller register**, so the
  register table grows with the total size of everything inlined. Same
  trade-off as the private-register rule above, at a larger scale.
- **Inlining's depth cap is per-run.** `PassOptions::inline_max_depth` bounds
  nesting within one pass invocation; across manager rounds the count starts
  over, and only `inline_max_function` bounds total growth. A depth mark
  carried on the IR would make the cap exact.
- **Tail-recursion elimination refuses cell parameters** rather than writing
  through the cell: a pinned argument register is a memory slot whose address
  may have escaped, and each real activation gets a fresh one. Narrowing this
  to `Ref`-taken parameters only would let `Incr`/`Decr` counters through.
  It also refuses when a non-argument `Param` is still read, for the same
  reason SROA refuses a read-before-write: the frame's initial value cannot
  be named. Mutual recursion remains out of scope.
- **JIT lowers from v2** instead of raw opcodes — the point at which
  malformed-IR crashes and verifier rejections become structurally impossible
  rather than blacklisted.
- **Liveness-refined trap-region pinning** — every register written inside a
  trap region is currently pinned to a cell. Liveness would narrow this to
  registers actually live into the handler.
- **Landing-pad design for exceptional-edge copies** — would lift the
  handler-block non-trivial-phi restriction.
- **Bounds-check elimination.** HashLink puts the check in Haxe source, not in
  generated code: `ArrayObj.getDyn` is `var pos:UInt = pos; if (pos >= length)
  return null;`, and ash's JIT emits no check of its own on
  `GetArray`/`SetArray`. So every check is a bytecode-level `Field(length)` +
  unsigned compare + branch that AIR can see and remove outright — no backend
  cooperation needed, and no safety consequence, since eliminating a
  *provably* redundant check cannot change behaviour. Like SROA it depends on
  inlining running first: until `ArrayObj.getDyn` is inlined there is no check
  in the loop, only a call. What it then needs is a range/interval analysis
  relating an induction variable to the array length — for the mandelbrot
  benchmark, `palette[iteration]` with `iteration` in `[0, MaxIterations]`
  against a palette of `MaxIterations + 1` entries, and `image[outPixel++]`
  bounded by the loop trip count. The `ArrayLen` alias class already exists,
  so GVN can CSE repeated length loads even before full elimination lands.
- **Loop vectorization.** Earlier analysis deferred this to LLVM's loop/SLP
  vectorizers, but that assumed LLVM was the only compiled backend. Cranelift
  has SIMD types and instructions (`i8x16`…`f64x2`, including a vector `fma`)
  and *no* vectorizing pass, so whatever the middle tier should vectorize,
  AIR must vectorize. Prerequisites are mostly in place or planned: the alias
  classes give memory-dependence information, loop analysis gives the trip
  structure; still needed are unit-stride access detection over varray/Bytes
  data and a cost model. Best targets are `Bytes`/varray loops (clean stride,
  no cross-class aliasing) and pixel loops once inlining and scalar
  replacement have de-abstracted them.

## JIT & tiering

- **`FunctionLookupError` is the last blacklist class** — 47 occurrences on
  `game.hl`. Functions reachable only indirectly are never code-generated
  before MCJIT finalization, so `get_function_address` cannot find them.
  Fixing it needs either eager declaration of indirectly-reachable callees
  before finalization, or an execution engine that compiles on demand.
- **27 std natives remain unimplemented** (file, process, and rnd families).
  Each compiles to an `hlp_error` call-time trap, so they only fail if
  actually called; implement as workloads demand them.
- **Full-JIT `game.hl` crashes** in LLVM `SimplifyCFG` during MCJIT
  finalization after compiling ~5000 functions (malformed-IR family). Tiered
  promotion sidesteps it; AIR v2 lowering should remove the cause.
- **Hybrid `test_mandelbrot_small` post-main `SIGSEGV`** (exit 139) after
  printing the correct checksum. Pre-existing, reproduces on baseline.
- **Full-JIT `Map` iteration yields garbage; `Map.get` is correct.** Found by
  `scripts/ash_bench.py`, which is why the benchmark corpus checks answers
  rather than only clocks — `target/debug/ash` completes `test_map_simple.hl`
  and `test_mapiter.hl` in ~1 s and prints a wrong number, so without the
  correctness gate it reads as a fast run. `m.get("x")` returns `42`, but
  `for (v in m)` prints a *different* value on every invocation (247019776,
  239646976, 205322496 across three runs) — the magnitude and the run-to-run
  variance both say the low 32 bits of a heap pointer are being read where an
  `i32` is expected, i.e. the iterator's `next()` return is not being unboxed.
  Only the full-JIT binary is affected; `interp` and all three hybrid tiers
  are correct. Repro:
  `target/debug/ash crates/ash/test/tests/test_map_simple.hl`.
- **MCJIT is legacy** — isolate the execution-engine surface
  (`create_jit_execution_engine`, `get_function_address`, `run_function`,
  `add_global_mapping`) behind a trait so an ORC backend can replace it
  without touching opcode lowering.

## Cranelift middle tier

Validated on aarch64-apple-darwin: 0.04 ms per small function, no MAP_JIT
hazard, bit-exact AAPCS64 through transmuted pointers, safe under the beadie
broker shape (backend on one thread, compile+finalize on another, execute on
a third). Pins: cranelift 0.130.2, `wasmtime-internal-jit-icache-coherence`
43.0.2.

### A failed compiled invoke re-runs the function — side effects double

Found while enabling field access below; independent of it, and the more
serious of the two. `call_function` dispatches to compiled code and, when the
invoke returns `Err`, records a fallback and calls `execute_hl_function`
anyway:

```rust
match self.call_compiled_function(findex, &entry, args) {
    Ok(v)  => return Ok(v),
    Err(e) => self.record_tiered_fallback(findex, ...),   // falls through
}
self.execute_hl_function(...)                             // runs it AGAIN
```

That is only sound if a failed invoke is guaranteed to have done nothing. It is
not: the compiled function can run to completion and fail afterwards (return
marshaling), or fail partway through. Anything it did already — printing,
allocating, mutating a field — happens a second time. The observable symptom is
output duplicated in place:

```
alloc: 100alloc: 100
palette done: 1001palette done: 1001
```

Reproduced with `[tiered] fallback findex=29 reason=compiled invoke failed: HL
exception: Null access`, where the function had already printed before failing.

The fix is to make the fallback honest about what it can retry. A compiled
invoke that failed *before entering* the function (bad signature, no entry
point) is safe to retry; one that failed *inside* it is not, and must propagate
rather than re-execute. Until the two are distinguishable, any tier that
declines mid-call can corrupt program output.

### The gate rejects essentially all real code — highest priority here

The opcode gate excludes the object model, and Haxe code touches the object
model constantly, so in practice the middle tier absorbs nothing. Measured on
`test_mandelbrot_small` (`--jit-tier auto --jit-log`):

```
[tier] decline findex=65 tier=cranelift reason=unsupported_opcode GetThis
[tier] decline findex=34 tier=cranelift reason=unsupported_opcode New
[tier] decline findex=27 tier=cranelift reason=unsupported_opcode SetThis
[tier] decline findex=33 tier=cranelift reason=unsupported_opcode New
[tier] decline findex=28 tier=cranelift reason=unsupported_opcode SetThis
[tier] decline findex=30 tier=cranelift reason=unsupported_opcode Field
attempted=9 succeeded=3 failed=2 fallbacks=2 cranelift=0 llvm=6
```

Six candidates, six declines, `cranelift=0`. Every promotion therefore lands on
LLVM, and LLVM's cost is paid *during the run* — the first install above took
**152.57 ms**, against **1.36 ms** for the one function Cranelift does accept in
`test_tiered_hotloop` (13 opcodes, no objects), where it delivers a 72×
speedup. That is roughly a 110× difference in compile cost per function, and
the tier that exists to absorb it currently absorbs none.

Two of those six were then uncallable — `compiled invoke failed: Float native
dispatch: 3 args, float_mask=0b110, ret_float=false not yet supported` — so
their compile time bought nothing at all. The dispatch gap is tracked under
[JIT & tiering](#jit--tiering); it is listed here too because it converts paid
compile time into pure loss.

The shared layout oracle now exists (`crates/ash/src/layout.rs`, verified
against `hlp_get_obj_rt` across 44 programs), and
`Field`/`SetField`/`GetThis`/`SetThis` are lowered against it in
`cranelift/lower.rs`. They are **not** admitted by `is_cranelift_lowerable`
yet, because turning them on diverges from the interpreter in a
threshold-dependent way:

| `--jit-threshold` | programs differing from interp |
|---|---|
| 1 | 6 of 41 |
| 10 | 3 of 41 |
| 100 (shipping default) | 0 of 41 |

A defect that disappears as the threshold rises is not in the offsets — those
are checked independently — it is in some function that only becomes eligible
for this tier once field access is allowed, and that runs during early
initialization. Part of the symptom is the double-execution bug above; the
underlying `Null access` needs running down first. `ASH_CL_DUMP=<findex>`
prints the lowered CLIF, which is how the null-check-then-load shape was read.

Enabling this is worth real speed — Cranelift went from **0 installs to 3** on
`mandelbrot_small` with the gate open — but not at the price of correctness
that holds only at one threshold setting. `New` remains unlowered regardless.

- **Build the tier**: `beadie` `TieredAdapter` ladder (interpreter → Cranelift
  `opt_level=speed` → LLVM), AIR v2 → CLIF lowering, `enable_probestack=false`
  and `preserve_frame_pointers=true`.
- **Shared layout oracle first** — extract the compile-time decisions
  (vtable slot and findex resolution, field offsets per `HOBJ`/`HVIRTUAL`/
  `HDYNOBJ`, enum construct offsets, unbox decisions) out of the LLVM
  lowering so both backends consume one implementation. Every one of these
  opcodes has produced a corruption bug at least once.
- **One `hl_kind → AbiClass` mapping** shared by the CLIF signature builder,
  the LLVM function type builder, and the interpreter's `arg_kinds` /
  `float_mask`, with a differential test calling the same findex through all
  three engines and comparing bit-exactly.
- **Exclude trap-containing functions** — Cranelift has no `returns_twice`,
  so a resumed longjmp into a Cranelift frame is unsound. Measured at 1.0% of
  functions (4.0% of opcodes) in `game.hl`; throw-only functions stay
  eligible because the interpreter's per-call setjmp wrapper guarantees the
  jump exits the frame. Three ways to lift the exclusion, cheapest first:
  *(a)* **outline the trap region** so the setjmp lives in an interpreter or
  LLVM stub and the protected body is a separate Cranelift function —
  longjmp then always exits Cranelift frames, and AIR v2's first-class trap
  regions already provide the structure for the transform; *(b)* adopt
  Cranelift's **`try_call` / exception tables** (present since 0.134), whose
  landing pads are real CFG edges the register allocator understands — this
  requires moving the compiled tiers from longjmp to unwinding-based throws,
  and would also let the LLVM tier drop `returns_twice`, which today
  pessimizes optimization around every try/catch; *(c)* **fiber-based
  unwinding**, sound because a fiber switch restores callee-saved registers
  cooperatively (unlike longjmp), but grain-mismatched — it needs a
  discardable fiber per active trap region, nested for nested traps, paying
  setup cost even when nothing throws.
- **Cross-tier FMA policy — emit explicit `fma`, do not rely on contraction.**
  Measured against a C port of `Mandelbrot.hx` (`clang -O2`, checksums for
  298² / 875×500): unfused `-ffp-contract=off` gives 22816350 / 112790102 and
  matches ash's interpreter bit-for-bit; fused `-ffp-contract=on` (clang's
  default, 4 `fmadd` in the binary) gives 22825041 / 112798515 and matches
  ash's LLVM tier exactly on 298² — and 112798515 is also the hxcpp/hxjava
  value. So **fusion is what every reference implementation does**, and the
  unfused number appears nowhere but a strict interpreter. Cranelift has an
  `fma` instruction (spike-verified bit-exact with `f64::mul_add`) but does
  not auto-contract, so the tier must emit `fma` explicitly — best as an AIR
  peephole (`Mul` feeding a single-use `Add`/`Sub` → `Fma`) that both
  backends lower literally, which also removes ash's dependence on LLVM
  DAGCombine heuristics (the residual 72-unit gap from hxcpp at 875×500 is a
  fusion-*pattern* difference, not a fusion-presence one). Repro:
  `scratchpad/fma_repro/mandel.c`.
- **`TieredAdapter` has no batched-flush path** — tier ≥ 1 promotions pay one
  `finalize_definitions` per function. Acceptable at hot-tier volume; the
  null-sentinel trick (backend performs `swap_compiled_with_osr` itself)
  applies if it shows up in profiles.

## Native symbols & libraries

- **Lazy HDLL loading via forward declarations** — build the symbol table
  from bytecode native declarations as `{lib, name, Resolved(addr) |
  Unresolved}`, and `dlopen` each hdll on first lookup against it, resolving
  all of its `DEFINE_PRIM` exports in one pass. Startup then loads zero
  hdlls.
- **Stale `/usr/local/lib/libhl.dylib`** — a root-owned March build is
  preferred by C hdlls even though ash now detects it (canary symbol) and
  falls back to the embedded stdlib. Refresh it with
  `sudo cp target/debug/libash_std.dylib /usr/local/lib/libhl.dylib`.

## GC

Heap is demand-committed with adaptive triggers, external pressure
accounting, and `ASH_GC_STATS` / `ASH_GC_STRESS` observability
(`ASH_GC_HEAP_MB`, `ASH_GC_TRIGGER_MB` configure it).

- **Interpreter root snapshots go stale** — constant buffers and globals are
  registered once, so `ASH_GC_STRESS=1` under the interpreter frees live
  data. Roots need re-registration per snapshot, or per allocating native
  call. Real-frequency collections are unaffected.
- **Worker-triggered collections scan only the triggering thread's stack** —
  block-level reclamation mitigates it. Keeping GC-allocating work on the
  main thread is the current discipline.
- **128-byte line rounding** wastes up to 8× on small objects (a 16–24 byte
  `vdynamic` occupies a full line).
- **Block-level reclamation only** — line-level reclamation needs marking
  precise enough to prove a line dead, which conservative scanning cannot
  give today.
- **Per-fiber scan ranges** — the fiber runtime swaps trap/exception state
  per fiber, but interpreter scan ranges are still global.

## Runtime & Heaps

- **`win_create_ex` argument-swap heuristic** (`crates/ash_sdl/src/sdl.rs`)
  papers over caller-side marshaling: it guesses based on value magnitude.
- **~40 no-op stubs in `ash_sdl`** — controllers, joystick, haptics, cursors,
  clipboard, displays, surfaces.
- **`hlp_thread_create` fibers**: `hlp_tls_*` is still pthread-keyed rather
  than per-fiber, and blocking primitives yield only when fibers exist.
- **Debug output sweep** — `ash_sdl` first-N-call counters, the
  `ASH_DUMP_TYPES` block with hardcoded Heaps indices in `ash_cli`, and the
  post-main `hlp_sys_get_loop` print (the last two also break
  `parity_matrix`, which compares stderr).
- **Committed build artifacts** in `examples/heaps_base2d/bin/` (`game.hl`
  plus eight `.hdll` files).

## Portability

- **Generate the reflection call bridge with Cranelift instead of writing it by
  hand.** `ash_static_call` is per-ABI assembly (currently `aarch64` and
  `x86_64`, with no fallback, so an unported architecture fails to link) that
  marshals arguments into registers for a signature known only at runtime. It
  is reached as a function pointer installed through `hl_setup_callbacks2`, so
  it can just as well be a trampoline emitted per distinct signature by the
  Cranelift backend already in the tree and cached by signature — roughly
  0.04 ms to compile, once per signature. Architecture support would then
  follow the backends rather than needing new assembly. The fiber context
  switch cannot be handled this way: swapping the stack pointer and restoring
  callee-saved registers is inexpressible in both LLVM IR and CLIF, which
  assume call/return discipline.
- **64-bit only.** `HL_WSIZE` is 8 and NaN-boxed values pack a 48-bit payload
  into a `u64`, so a 32-bit target needs a different value representation
  regardless of which backend compiles it.

## Dead code

- **Delete the pre-JIT module path and drop the `hlbc` dependency.**
  `crates/ash/src/{module.rs, functions.rs, values.rs}` (916 + 574 + 209
  lines) reference only each other and `types.rs`'s `FunPtr` enum, and
  `AshModule` is constructed in exactly one place — its own unit test at
  `module.rs:888`. It is the pre-`jit/` prototype: `translate_opcode` covers
  about twelve opcodes, bails with "not yet implemented" on the rest, and
  never implemented jump targets at all. The live entry point is
  `main.rs` → `ash::jit::module::JITModule`, and `jit/`, `bytecode.rs` and
  `c_types.rs` contain no `hlbc` at all (they use ash's own decoder and
  AIR's vendored opcode definitions). Removing the three files, the `FunPtr`
  enum with its `use hlbc::types::{Function, Native}`, the three `pub mod`
  lines in `lib.rs`, and the `hlbc` entry in `crates/ash/Cargo.toml`
  eliminates the last dependency on an upstream that has been unmaintained
  for two years — without forking or vendoring anything.

## Build & tooling

- **`build.rs` dylib path** — nightly cargo emits `libash_std.dylib` to
  `target/debug/`, while `crates/ash/build.rs` reads
  `target/aarch64-apple-darwin/debug/` and panics if it is missing. It should
  fall back.
- **No test runner** — there is no `run_tests.sh`; the parity matrix
  (`crates/ash/test/tests/run_stdlib_matrix.sh` + `parity_cases.toml`) is the
  closest thing. For performance, `scripts/ash_bench.py` is now canonical
  (corpus in `bench/benchmarks.toml`, docs in `bench/README.md`);
  `scripts/run_perf_matrix.py` is a deprecated shim that forwards to it.
- **README is stale** — it documents O0/O1/O2 AIR levels and describes MCJIT
  codegen as "O3 optimizations"; no LLVM middle-end pipeline runs at all
  (adding one is the cheapest large win available).

## Cranelift tier — follow-ups

- **Own the backend; implement `beadie::JitBackend` directly.** beadie-cranelift
  is a reference implementation, not a dependency — neither rayzor
  (`BeadieJit`) nor zyntax (`ZyntaxCraneliftBackend`) uses its concrete
  backend. `AshCraneliftBackend` should own its own
  `Arc<Mutex<cranelift_jit::JITModule>>` and `FunctionDef`, depending only on
  the cranelift-agnostic `beadie-core` / `beadie-backend` traits. This also
  frees ash to pick its own cranelift version: while ash consumes
  beadie-cranelift's `CraneliftFunctionDef`, both crates must resolve to the
  same cranelift or the `ir::Function` types are distinct.
- **Move to cranelift from wasmtime upstream** (git rev, 0.136.0-dev — the
  crates.io maximum is 0.134.3). Verify the flag names the tier sets survive
  the bump: `opt_level`, `enable_probestack`, `preserve_frame_pointers`,
  `is_pic`.
