# ash — Development Backlog

Known gaps, deferred refinements, and open defects. Code docs describe what the
code guarantees today; anything that should change lives here.

**Last updated**: 2026-08-21

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

## DeltaBlue crashes interp and hybrid — two defects, one of them GC-shaped

Added 2026-08-21 from rayzor's suite; `--mode jit` is correct (checksum
14065400, agreeing with stock HashLink 1.15 on x86_64), and the bench is
registered full-jit-only until this closes.

On the release binary, interp crashes 5/5: four of five at a PAGE-ALIGNED
heap address (0x14ae04000, 0x13fe04000, ... ~350-420ms in), one of five at
the small-int fault (0x6, ~27s in) that is the OSafeCast/__cast defect.
The SafeCast fix has landed and peeled off the small-int mode; the
page-aligned fault is what remains, and it is the GC investigation's
quarry.

hybrid does not SIGSEGV at all: it dies on an uncaught "Null access",
consistent with compiled code loading from a page the GC reclaimed and
faulting the *pointer it read* rather than the page itself.

The crash-handler frame walk (landed the same day) names the fault site
directly: `utf16_len_eq <- compare_regs_in <- execute_opcode` — the
interpreter comparing a String whose UTF-16 payload page is gone. So the
reclaimed object is one held live by an interp register at the moment of a
string comparison, which is precisely the scan-root-liveness shape.

A page-aligned fault address in a workload of small object graphs points at
page handback — the macOS MADV_FREE_REUSABLE path and the
reclaim_block_pages bookkeeping — or block reclamation freeing a block that
conservative marking should have kept. Note binary_trees independently
showed ASH_GC_STRESS running out of memory the same day: the allocator's
story under object-graph pressure has two open holes, and this one
reproduces in 0.4s, headless, ~100% of the time — a far better instrument
than the 1-in-5 Heaps repro.

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

## Conformance: the next three blockers, one per suite

The OSafeCast fix removed the shared fault_addr=0x9 wall; each suite now
fails on its own defect, in order of value:

  * unit — "Assert hit at pc 3": the `Assert` opcode is unimplemented in
    the interpreter. The whole main suite is behind this one opcode.
  * threads — std/src/obj.rs:979 "Failed to allocate memory" in
    hlp_get_obj_rt on a spawned thread; the EventLoop/fiber area MEMORY.md
    already flags.
  * sys — a new SIGSEGV at fault_addr=0x30; also fails under stock
    HashLink here (helper-spawning suite), so attribute before fixing.

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
