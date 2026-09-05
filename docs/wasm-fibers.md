# Suspending a fiber inside a wasm module

*Design note. Intended home: `docs/wasm-fibers.md`, companion to `docs/wasm-target.md` and `docs/wasm-threads.md`.*

## 1. What we are building

We are building the ability to stop a Haxe thread part-way through a call inside a single-threaded wasm module and start it again later, with no engine feature and no host driver — the third row of the table in `docs/wasm-target.md:749-760`, the one whose "needs" column says *nothing*. `std/src/fiber.rs` already routes every fiber operation through four calls that `ash_wasm_runtime::guest` implements (`Fiber::with_stack_size`, `Fiber::resume`, `Fiber::state`, `yield_now`), and on wasm three of those are currently degenerate: `resume` runs the body to completion (`guest.rs:147-159`), `with_stack_size` accepts a stack size and ignores it (`guest.rs:130-133`), and `yield_now` calls the `env.ash_host_fiber_yield` import and returns immediately when nothing is bound. After this work the same four calls keep their signatures and gain real guarantees: **`guest::yield_now()`**, called on a fiber, does not return until the scheduler resumes that fiber — it unwinds the wasm call stack out to `Fiber::resume` and returns *there*; **`Fiber::resume()`** runs a `Ready` fiber from its entry or a `Suspended` fiber from its suspension point, and returns when the body finishes (`Done`/`Errored`) or when it suspends (`Suspended`, a state the wasm backend can currently never reach); **`Fiber::with_stack_size`** starts honouring its argument, because each fiber now owns a shadow-stack region; and **`Fiber::stack_range()`/`saved_sp()`** stop returning null, because a suspended fiber's live values are spilled into a buffer in linear memory, which the collector can scan exactly the way it scans a native fiber's stack. That last one is not a side effect worth burying — it is the one place this design is *better* than JSPI rather than merely cheaper, since `guest.rs:163-180` currently has to say roots on this target must be explicit precisely because the engine holds the stack where the collector cannot see it. What the primitive does **not** promise: no parallelism (this is suspension, like the JSPI row, not the worker row); no suspension from inside a host callback; no reentrancy — `Fiber::resume` may not be called from a frame that is itself suspendable.

## 2. The design, borrowed and diverged

Asyncify is right about more than it is wrong about, and the parts it is right about are the parts that are cheap. Roughly 600 lines of Binaryen's 2,027-line `Asyncify.cpp` are the rewrite; the rest is analysis and option plumbing. We take the rewrite nearly verbatim and rebuild the analysis, because the analysis is where ash knows things Binaryen structurally cannot.

**Taken verbatim, because it is simply correct.** The three-state machine (`Normal=0`, `Unwinding=1`, `Rewinding=2`) in one mutable i32 global, checked with a two-instruction `global.get; i32.eq`. The side-stack layout: a two-word `{ current_pos, end }` header in linear memory addressed by a second global, and a stack that grows *up*, holding per frame an i32 call index followed by the live locals, each padded to four bytes. The five-function API with Binaryen's exact names and exact semantics — `asyncify_start_unwind(ptr)`, `asyncify_stop_unwind()`, `asyncify_start_rewind(ptr)`, `asyncify_stop_rewind()`, `asyncify_get_state()` — kept identical on purpose, so that during bring-up a `wasm-opt --asyncify` module and an ash-instrumented module are interchangeable under the same driver and can be run against each other. The fake-global trick for call results, which matters more than it looks: a `local.set $x (call f)` must not write `$x` on the unwinding path, or `$x` is spilled holding a forged zero, and the forged zero is a silent wrong answer of exactly the class the vectorizer notes describe. And the two-tier runtime marking: the function that calls `start_unwind` is state-changing but *not* instrumented, and the function that calls `stop_unwind`/`start_rewind` is forced non-state-changing so the analysis stops climbing there.

Concretely, that fixes the shape of two small pieces of hand-written ash_std code. `guest::yield_now` becomes the top-most runtime:

```rust
pub fn yield_now() {
    match asyncify_get_state() {
        REWINDING => { asyncify_stop_rewind(); }          // resumed here; just return
        _ => { asyncify_start_unwind(current_fiber_buffer()); }
    }
}
```

and `Fiber::resume` becomes the bottom-most runtime: set `__stack_pointer` to the fiber's saved SP, call `asyncify_start_rewind(buf)` if the fiber is `Suspended`, invoke the fiber's entry function, then on return check the state — `Unwinding` means it suspended, so `asyncify_stop_unwind()`, save SP, mark `Suspended`, return. Because `resume` is forced non-state-changing, the scheduler above it is never instrumented and never pays for this.

**Where we diverge, and why.** Every divergence below has the same root cause: Binaryen is handed an arbitrary module and must assume the worst about it, while ash emitted the module from its own AIR pipeline through its own LLVM configuration and knows what it put there.

*We do not flatten.* This is the largest divergence and it is covered in §3.

*The suspend set is seeded from ash's own safepoints, not from "every import".* Binaryen's default seeds are every import plus every function containing a `call_indirect`; on `t.wasm` that marks 85-90% of functions state-changing and roughly doubles the module. ash already emits `hlp_fiber_poll` at AIR loop back-edges from both backends (`crates/ash/src/llvm/function.rs:2034-2035`, `crates/ash/src/cranelift/backend.rs:448`) — compiler-placed safepoints that Binaryen would have had to guess at. Those call sites, plus the `env.ash_host_fiber_yield` import, plus an explicit list of blocking natives, are the *only* suspend points. On `t.wasm` there are 131 poll sites in 73 of 1,958 functions; on `threads.wasm`, 280 in 128 of 3,041. The instrumented set is the backward closure of those seeds over the call graph, cut at the bottom-most runtime.

*Indirect calls get an exact target set instead of a blanket assumption.* Binaryen offers two options, and both are wrong for us: assume every `call_indirect` can suspend (2,127 sites in `t.wasm`, 2,596 in `threads.wasm` — this is the whole blowup), or `asyncify-ignore-indirect`, which is a correctness knob and not a size knob, and which is unsound here because Haxe closures, virtual dispatch and the HL function table all go through the table. The third option is available only to us: a `call_indirect` can reach exactly the functions in the table, and the linker *builds the table* — `plan` assigns every slot from relocations and element segments (`link.rs:343-395`), and `emit` fixes the table at `minimum == maximum == slots + 1` (`link.rs:1258-1266`) so it cannot grow at runtime. So the indirect target set is not a guess, it is a list we already have, and each site's edge set narrows further to the entries whose type matches the site's type index. That is the single highest-value thing this transform does that `wasm-opt` cannot, and it is also the number that decides whether the feature is affordable — see §7.

*Rewind stays skip-forward.* Both the Asyncify study and the Flatten study suggest replacing the linear skip with a recorded resume label and a `br_table` dispatch, on the grounds that rewind is O(function body size). I am declining that for v1 on a structural objection: wasm branches target *enclosing* labels only, so you cannot jump into a block, and a resume dispatch is therefore not one `br_table` but a ladder of them, one per nesting level, threaded through every enclosing block, loop and `try_table`. That is a relooper, and it also destroys the free property in §3 that re-entering a `try_table` reinstalls its handler. The cost argument is weaker than it looks anyway: rewind is O(the bodies of the frames actually on the suspended fiber's stack), which for a Haxe method a dozen frames deep is a dozen bodies, not the module. If measurement says otherwise the ladder is available later; it is a rewrite of one function in the transform, not of the design.

*The analysis is computed once, not re-walked.* Binaryen's `canChangeState` re-walks the subtree at every visited node with an explicit `TODO: caching, this is O(N^2)` at `Asyncify.cpp:809`. We compute one bit per function in a worklist pass — the same shape as `mark_reachable` (`link.rs:516-621`), which already exists in this crate — and one bit per instruction during the single decode.

*Each fiber gets its own shadow stack and its own side stack.* Asyncify never touches `__stack_pointer`: the shadow-frame allocation and its restore both sit inside `if (state == Normal)`, so on unwind a suspending frame's shadow allocation is simply left in place and on rewind it is not re-allocated — only the frame-pointer local is restored from the spill. Three of the four studies observed this independently. That behaviour is *exactly right* for a per-fiber stack and catastrophic for a shared one, and the linker gives exactly one shadow stack of `LinkOptions::stack_size` (65,536 bytes, `link.rs:37-42`). So `Fiber::with_stack_size` allocates a region, `Fiber::resume` swaps `__stack_pointer` at entry and saves it at suspend, and the transform synthesizes two more trivial helpers — `ash_fiber_get_sp()` / `ash_fiber_set_sp(i32)`, two instructions each on global 0 — because Rust has no way to touch `__stack_pointer` and the linker is the only thing that knows it is global 0.

*The side stack is the fiber's GC-visible stack.* The unwind buffer holds spilled locals, and those locals hold HL object pointers. It must therefore be allocated outside the GC heap (or explicitly rooted) *and* handed to the collector's conservative range scan — the two halves of the same fact, and the failure mode of getting only the first half is the recurring "Rust heap has no GC root" class already in the project memory. This is what lets `stack_range()` stop returning null.

## 3. The EH problem, decided

Binaryen cannot process ash's modules, and it is not one blocker but three, each behind the last. `Flatten` classifies `try_table` as a control-flow structure but has no case for it and aborts at `Flatten.cpp:231` — reproduced twice, independently, on `t.wasm`. Behind that, Asyncify refuses any live local without a byte size, so an `exnref` local is fatal at `Asyncify.cpp:1717-1723` — reproduced on a `try_table`-free module, so fixing Flatten buys you the next crash rather than a working pass. Behind *that*, `AsyncifyFlow::process` has zero `TryTable` cases and ends in its own `WASM_UNREACHABLE`. And we cannot retreat to legacy `try`/`catch`, the one input Binaryen can flatten, because `crates/ash/src/target_abi.rs:192-206` already litigated that: legacy EH is what LLVM defaults to and what every current engine refuses, which is why ash passes `-wasm-use-legacy-eh=false`. Nor can we exclude the EH-bearing functions, because in `threads.wasm` the 21 of them are `dispatch`, `execute`, `runCases`, `runFixtures`, `runTest` and two hot loops — the exact frames a fiber suspends inside. A removelist that excludes them excludes the feature.

**The decision: ash's transform does not flatten, at all.** Flatten exists to make the operand stack empty *everywhere*, because Asyncify has to restore control at a point where nothing may be pending on the stack and wasm gives no way to spill it. We need the stack empty only *at suspend points*, and ash's emitter already delivers that. Measured over the `$ash_f*` functions in `t.wasm`: 95.28% of call sites have zero operand-stack values pending, 3.85% have one (always a pure `local.get`/`const`/`global.get`), 0.69% have two, and five sites in the entire module have three. More decisively, **100% of the 131 `hlp_fiber_poll` sites in `t.wasm` and all 280 in `threads.wasm` are bare statements with an empty operand stack** — which is unsurprising, because ash emits them itself. The narrow normalisation that replaces Flatten is: for each call in operand position within a function we choose to instrument, spill its result to a fresh local before the enclosing statement and replace it with a `local.get`. Hoisting a pure `local.get`/`const`/`global.get` past a call is unconditionally safe because wasm locals are frame-private and invisible to a callee, which covers the 3.85%; only the five deep sites need general effect-ordering care. This is also what avoids the 25x locals blowup Binaryen produces: flatten's temporaries are a per-expression cost over 347,856 instructions, while ours is a per-instrumented-call-site cost.

On `try_table` itself, three rules:

**Suspending inside a `try_table` body is allowed and needs no special work.** `try_table` is a structured instruction, so skip-forward rewind re-executes it on the way back in and the handler is reinstalled for free. This is exactly what Asyncify does with legacy `Try`.

**Landing pads are recognised, not constructed.** LLVM's SjLj lowering emits precisely one shape, `local.set $n (block (result T) (try_table (catch $tag $block) BODY) (unreachable))`, and it emits it every time: all 6 value-flowing blocks in `t.wasm` and all 42 in `threads.wasm` are directly under a `local.set`, with no exceptions. That violates Flatten's "control flow must not flow values" rule but not the invariant behind it, because a block's result exists only on the block's exit edge — nothing is pending during the body and the value is consumed in zero instructions. We pattern-match it and leave it alone.

**No suspend point may occur while an `exnref` is live, and this is an assert rather than an assumption.** In both sample modules every `throw_ref` is the last instruction of its function, immediately after the `local.set` that produced the `exnref`, so the value is never live across a call. But that is a property of this LLVM's lowering, not a guarantee, so the transform checks liveness and refuses the link naming the offending function rather than trusting it. (`exnref` in a table or a global validates under Binaryen's validator as a fallback; nobody has run either in an engine, so that is a note, not a plan.)

There is one thing this section cannot yet decide, and I would rather say so than paper over it: whether ash's wasm SjLj lowering keeps any *module-global* state (LLVM's older Emscripten SjLj used `__THREW__`-style globals). If it does, that state is per-call-stack and must be saved and restored per fiber alongside `__stack_pointer`. None of the four studies looked; it is the first thing to check at staging step 3.

## 4. The relocation problem, decided

The linker's central invariant is that nothing moves: relocations write fixed-width slots — always five bytes for a LEB, always four raw — at absolute byte offsets into `code_payload`, and `object.rs`'s header calls recomputing those offsets "the step that silently corrupts a module." Inserting instructions breaks that three ways at once, and all three are silent: every relocation at or after the insert names the wrong byte and, being a fixed five-byte write, lands across an opcode boundary in a module that may still validate; every `code_bodies` range after the insert is stale; and `relocations_in`'s bisection (`link.rs:631-648`) returns the wrong slice, so `mark_reachable` sees functions that call nothing and tree shaking deletes live code.

**The decision costs nothing: the rewrite runs after `apply_relocations`.** By `link.rs:138` every relocation has been spent — `emit` reads only `func_out`, `code_bodies` and `code_payload` for the code section and never touches an offset again, and `constructor_body` resolves symbols by index, not by offset. So a transform sitting between `apply_relocations` and `emit` cannot invalidate an offset anyone will read, and as a bonus it sees *final output function indices* in every call immediate, which means the call-graph analysis of §2 needs no access to the symbol table at all. This one placement decision removes the entire problem; there is no offset map, no monotone delta table, no re-sorting.

The cost is paid elsewhere, in index space, because `plan` has already frozen the numbering. Four consequences, all handled in `plan` rather than in the rewrite: new functions must be appended *after* `ctors_index = imports.len() + kept_functions` (`link.rs:1254`) or the start section silently points at a helper and the module instantiates and does nothing; new globals must be appended after the last GOT global, never inserted at index 3, because GOT globals are numbered `first_got + n` and those numbers are already written into patch sites; any new function type must go through the type dedup map `emit` already depends on; and the name section (`link.rs:1453-1481`) is built from `func_out`, so the transform must contribute names for its synthesized functions or every trap inside them prints `<wasm function N>` — the exact regression the name section exists to prevent. Tree shaking is a subtler trap: the synthesized helpers are created after shaking and so are safe, but `hlp_fiber_poll` and the scheduler entry points are *input* symbols, and if they are not in `LinkOptions::roots` they are removed before the transform ever sees a suspend point. The failure mode is a module that links and validates with no fibers in it.

One free win falls out. Because instrumented bodies are re-encoded anyway, they can be re-encoded with *minimal* LEBs, recovering the five-byte relocation padding — at least 85,711 bytes, ≥10.1% of `t.wasm`'s code section, and 8.6% of `threads.wasm`'s. This is legal only after patching, which is where we already are. I am keeping it behind its own flag (`--wasm-repack`, applied to all bodies rather than just instrumented ones) so that a decoder bug shows up as a repack failure rather than as a fiber failure; the two risks should not be entangled during bring-up.

## 5. The DSL question

**No — not now, and the bar for later is specific.** ISLE is a term-rewriting language over a typed tree, and its three products are overlap detection, priority resolution and trie-based match scheduling, all of which exist to tame the ~7,315 mutually-overlapping rules across Cranelift's four backends. It brings a 9,150-line compiler, a build step, its own error taxonomy, and a version-skew decision between the two `cranelift-isle` copies already in `Cargo.lock`. Against that, the linker's entire rule surface today is `patch()` — eight disjoint `match` arms over an enum tag, with no overlap, no priority and no nesting — and the instrumenter's dispatch will be about fifteen opcode forms. The break-even is not close.

But the real argument is not scale, it is shape. This transform is dominated by three whole-function or whole-module analyses whose results must be *agreed on* by code emitted at opposite ends of the same function: a call-graph fixpoint, a liveness computation producing an ordered spill set, and a per-function call-site ordinal counter. Binaryen's output makes the stakes visible — one function spills seven of its twenty-five locals at offsets 0..24 in the prologue and stores the same seven to the same offsets in the epilogue. Get that order wrong and the module validates, runs, and resumes a fiber with its locals permuted. ISLE has nowhere to hold that invariant: it has no state, no ordering, no fixpoints (`recursion.rs` *errors* on cyclic terms), and its only mutation mechanism is an unchecked impurity flag with no cross-rule ordering guarantee. It also cannot express the input: its patterns are trees over fixed-arity variants with no sequence form, and a wasm body is a sequence. And the crate's stated principle is to be checkable rather than clever, tested by differential comparison against `wasm-ld`; generated matchers with no source-level breakpoints are directly against that.

**What does earn its place today is one abstraction, and it is not a pass trait.** A pass trait with a single implementer is premature. What is load-bearing is a **body cursor**: a decode/annotate/re-encode loop that runs `wasmparser`'s `OperatorsReader` in lockstep with a `FuncValidator`, and exposes per instruction the operand-stack height and types, the control depth and enclosing frame kinds, and — the part that will otherwise cause a silent bug — a branch-label remapper, so that *wrapping a region in a new block is a supported operation* rather than a manual renumber of every `br`, `br_if` and `br_table` inside it. Branch immediates are relative; a wrong-but-in-range label produces a module that validates and jumps to the wrong place, which is the same failure class `lib.rs:33-44` already calls this crate's one dangerous property. The validator's operand-stack introspection is also what makes §3 possible: it gives the typed stack state on the *flat* operator stream, which is what Flatten exists to fake, without ever building a tree and therefore without ever reaching the `try_table` path that kills Flatten. The decoder/encoder bridge itself is nearly free — `wasm-encoder`'s `reencode` module covers the whole opcode space including EH, and it is one Cargo feature word away since `wasmparser` is already a direct dependency at `Cargo.toml:8`. Declare the feature explicitly rather than relying on workspace unification, or the crate breaks when built alone.

The one genuinely ISLE-shaped future here is a wasm peephole optimizer, which ash does not have and has not asked for. If that arrives, start with a `match` on a short sliding window, and reach for a DSL only past roughly a hundred rules and the first pair that actually shadow each other — noting that Cranelift itself skips overlap checking for `multi` constructors, and the peephole driver *is* a multi constructor, so the DSL's headline safety property is off in exactly the place it would be used.

## 6. Staging

Everything sits behind `LinkOptions::fibers` (default `false`) and a `--wasm-fibers` CLI flag. The gate is not "the code path is skipped" but "the emitted module is byte-identical to today", asserted in CI by a byte-diff of a linked `t.wasm` with the flag off. That is the same discipline the linker already uses against `wasm-ld`.

**Step 0 — the body cursor, identity only.** Decode and re-encode every body of `t.wasm` and `threads.wasm` with no transformation. Testable alone: the re-encoded module validates, runs the existing wasm test suite, and matches Binaryen's own round-trip instruction counts. This step also produces the repack win, and it is where a decoder bug is cheapest to find.

**Step 1 — the suspend-set analysis, with no rewrite at all.** Emit a sorted list of functions the analysis would instrument. This is the step that decides the feature, and it costs nothing to run (see §7). Differential oracle: `wasm-opt --asyncify --pass-arg=asyncify-verbose` on the same module, sorted — Binaryen's own output ordering is non-deterministic and must be sorted or the diff is noise. Ash's set must be a strict *subset* of Binaryen's under matching seeds; any function in ours and not theirs is a bug in ours.

**Step 2 — the rewrite, on EH-free modules, differentially tested against `wasm-opt`.** Take a fiber program whose functions carry no `try_table`, instrument it both ways, drive both with the same host using the five shared export names, and compare output. This is the oracle window and it is narrow but real — exactly as `wasm-ld` is the linker's oracle for the cases it covers and silent for the ones it does not. It closes the moment EH is involved.

**Step 3 — EH functions.** Landing-pad pattern match, the `exnref`-liveness assert, and the SjLj module-global question from §3. Validated by running the threads suite, whose 21 EH-bearing functions are the frames that matter.

**Step 4 — the scheduler.** Per-fiber shadow-stack regions and unwind buffers, the `__stack_pointer` swap in `Fiber::resume`, GC rooting and `stack_range()`. A two-fiber ping-pong with a deep call chain and canaries below each SP is the test that means something.

**Step 5 — measurement, on the NUC.** The interesting arm is *instrumented but never suspending*, because that is what every non-fiber program pays. Per the standing rule, no perf number is real until Linux says so.

The assert mode ships in step 2, not later: Binaryen's own header warns that mis-instrumentation is silent and that users reach code paths your tests did not. Ours has three parts — uninstrumented functions snapshot the state at entry and trap if a call changed it; each spill record carries a magic word and a local count checked on restore; and the bounds check goes in the push, not only at the API boundary, so a side-stack overflow is reported by the frame that overran it rather than at `stop_unwind` (Binaryen has this as an open TODO).

## 7. What could sink this

**The indirect closure swallows the module.** This is by far the largest risk and it is the reason step 1 comes before any rewriting. The measured bracket is stark: with Binaryen's defaults, 85-90% of functions instrumented and the module at roughly 1.9x; with `ignore-indirect` and a single named suspend import, 107 functions and +0.55%. The whole difference is the indirect-call policy, and `ignore-indirect` is not available to us. Our exact address-taken, type-partitioned edge set is strictly better than Binaryen's blanket rule and unsound in neither direction — but **nobody has measured the transitive closure of ash's poll-site callers under it.** The seeds are small (73 and 128 functions); the closure is unknown. The linker's own documentation warns that ash reaches most functions through a table written by relocations into data, which is an argument that the address-taken set is large. The early signal is the cheapest in the plan: print the number at step 1. If instrumenting `threads.wasm` costs more than about a fifth of its functions, the code-section figure to budget against is not 1.9x but 3.5-4x — the module ratio in `docs/wasm-target.md:757` is the *module* figure and understates the code figure by about 1.8x, because `t.wasm`'s code section goes 852,013 → 3,031,843 while its 1.34MB data section does not move. At that point the answer is either an HL-level (findex) refinement of the table edges, or accepting that this row stays a fallback behind JSPI.

**A spill set that disagrees with itself.** Prologue and epilogue must agree on which locals are saved *and in what order*, and nothing will tell you if they do not: the module validates, runs, and resumes with permuted locals. This is the same failure class as a wrong relocation, which is precisely what this crate's design principle was written for. The early signal is the magic-word-and-count check in every spill record, on from step 2, plus the state-unchanged assert in uninstrumented functions — both cheap, both useless if added after the first mysterious wrong answer rather than before.

**Shadow-stack aliasing between fibers.** Asyncify deliberately leaves a suspended frame's shadow allocation in place, which is correct for one coroutine and wrong for N on one `__stack_pointer`, and the linker gives exactly one 64KB region. Get the per-fiber regions or the swap wrong and a second fiber allocates on top of the first one's live frames — again a wrong answer, not a trap, and it will appear only under a workload with two fibers deep in different call chains. The early signal is the step 4 ping-pong test with canaries; the structural mitigation is that the region comes from outside the GC heap and its bounds are known, so a canary check is affordable to leave on in debug builds.

Two smaller things worth naming because they are cheap to check and expensive to discover late: `return_call` closes this door permanently — a tail call cannot be wrapped in an unwind check because the frame is already gone — and `t.wasm` currently has none, so if anyone enables tail calls for wasm codegen this design must be told first. And the `exnref` liveness property is LLVM's, not ours; the assert exists so that a future LLVM changing its SjLj lowering fails the link loudly instead of corrupting a fiber quietly.

**Where the studies left gaps.** Nobody has run the proposed transform; everything about ash's version above is inference from Binaryen's source plus static measurements of ash's output. The two Asyncify cost measurements disagree slightly (1,759/1,958 functions and +92.8% versus 1,669/1,963 and 1.86x) because they used different pass arguments — treat the range, not either number, as the default-Asyncify cost. The operand-stack statistics were taken from `wasm-dis`'s folded rendering rather than the binary, so a byte-level decoder may bin a few sites differently; the direction of the claim is safe (a non-`local.get`/`const` operand *is*, by construction, a value on the stack) but the exact percentages may shift by a fraction. The `exnref`-in-a-table and `exnref`-in-a-global fallbacks passed Binaryen's validator only and have never been in an engine. And `asyncify-ignore-unwind-from-catch` is documented in Binaryen's header but does not exist anywhere in its source — a reminder that that header is documentation, not a specification, and that we should read the code before copying a promise from it.
## 8. Step 1, run early — and it refutes §2's central bet

The plan above puts the suspend-set measurement first because it decides the
feature and costs nothing. It was run on `threads.wasm` (3,041 functions)
before any code was written, from the disassembly, and the answer is not the
one §2 expected.

| suspend-set policy | instrumented |
| --- | --- |
| direct calls only | 280 (9.2%) |
| **exact address-taken, type-partitioned** | **2,617 (86.1%)** |
| Binaryen's blanket "any `call_indirect` suspends" | 2,632 (86.6%) |

§2 claims the type-partitioned edge set is "the single highest-value thing
this transform does that `wasm-opt` cannot". It is worth **half a percentage
point**. The reason is structural and was there to be measured: the module has
only **41 distinct function types** for 3,041 functions, and **36 of them
already contain a suspending target**, because 58.8% of functions are
address-taken and Haxe's compiled output shares signatures heavily. Once any
suspending function is in the table under a common signature, every
`call_indirect` of that signature must be assumed to suspend, and the closure
swallows the module.

Narrowing the *seeds* does not help either, which rules out the obvious next
idea. Seeding only from real blocking primitives rather than from every
compiler-placed `hlp_fiber_poll` safepoint takes the seed count from 129 to
27 and leaves the closure at 2,617 — identical. The closure is dominated by
indirect edges, not by how many places can block.

So the cost to budget is §7's expensive case: ~86% of functions instrumented
and a code section around 3.5x, not the 1.9x module figure. §7 named the two
ways out of exactly this position, and the first of them is now the only one
with any evidence behind it: an **HL-level (findex) refinement**, where ash
narrows each indirect call site using what it knows before wasm erases it --
which closures can actually flow to a given call site in AIR -- and records
that as link-time metadata. Nobody has measured whether that narrows 36 of 41
types down to something useful. Until someone does, this row stays a fallback
behind JSPI, and the honest summary is that the transform is buildable, the
mechanism is understood, and the *analysis* is what does not yet pay.

Method note: measured from `wasm-dis` output by extracting each function's
`(type $N)`, its direct `call` targets and its `call_indirect (type $M)`
sites, taking address-taken functions from the element segments, then running
the closure to a fixpoint. It is an approximation of what the real analysis
would see -- a byte-level decoder may bin a few sites differently -- but the
gap between 9.2% and 86.1% is far too wide for that to change the conclusion.

## 9. The same question asked one level up — and the answer changes

§8 measured the suspend set on the linked *module* and concluded the analysis
does not pay. That measurement was sound but it was taken at the only level
Binaryen can see. Asked at the level ash still knows, the answer is different,
and §7's named fallback — an HL-level refinement of the indirect edges — is
worth substantially more than the wasm-level type partitioning was.

`crates/ash/examples/fiber_suspend_set.rs` computes the closure over the HL
call graph, where a `CallMethod`/`CallThis` names a vtable SLOT rather than a
signature, and a `CallClosure` can reach only a function some `*Closure`
opcode actually turned into a closure:

| program | functions | direct only | HL-level | every indirect reaches anything |
| --- | --- | --- | --- | --- |
| `t.hl` | 357 | 2.5% | **11.5%** | 19.6% |
| `threads.hl` | 749 | 16.3% | **35.8%** | 37.9% |
| `unit.hl` | 8,688 | 15.9% | **27.3%** | 27.7% |

The comparison that matters is like-for-like, on the same program's Haxe
functions. In the linked `threads.wasm`, the wasm-level closure takes **754 of
836 `ash_f*` functions, 90.2%**. The HL analysis of the same program's
bytecode takes **35.8%**. That is the refinement paying, and the reason is
visible in the shape: the module has 19 vtable slots with a median fan-out of
6, where the wasm view has 41 signatures over an address-taken set covering
58.8% of the module.

Two honest qualifications. First, the narrowing itself is worth little at this
level either — HL 35.8% against a blanket HL 37.9% — so the win is not the
slot/closure precision, it is asking at a level where the call graph is the
Haxe program rather than the Haxe program plus a linked runtime. Second, these
numbers cover only bytecode functions. The linked module's other 2,205
functions are ash_std and libc, of which the wasm closure sweeps in 85.2%;
that part is not covered by this analysis and must not be assumed away. It is
also the part ash has the most direct knowledge of, since the runtime is our
own code and the set of runtime functions that can reach a suspend is a
property we can state rather than infer — but stating it is work, and until
someone does it the projected total remains a projection.

So the position at the end of this note is the opposite of §8's: the analysis
is affordable at the HL level, the mechanism was already settled in §2 to §4,
and what stands between this design and an implementation is the runtime-side
annotation plus the staging in §6 — not a missing idea.

## 10. §8 re-measured with a real decoder, and what it says about seeds

§8's method note conceded that its numbers came from `wasm-dis` text and that
"a byte-level decoder may bin a few sites differently". That concession can be
withdrawn. The analysis now lives in the linker
(`crates/ash_wasm_link/src/suspend.rs`) and runs over the module bytes through
the same `wasmparser` decoder the transform will use;
`tests/suspend_set.rs` is the measurement, so it survives an emitter change
instead of being a number in a document.

It reproduces §8 exactly — 2,617 and 2,632 on `threads.wasm`, to the function —
and extends it to two more modules:

| module | functions | address-taken | direct only | TypedTable | AnyIndirect |
| --- | --- | --- | --- | --- | --- |
| `t.wasm` | 1,958 | 61.9% | 5.4% | **83.0%** | 83.8% |
| `threads.wasm` | 3,041 | 58.8% | 11.0% | **86.1%** | 86.6% |
| `unit_gc.wasm` | 11,661 | 87.5% | 26.4% | **73.9%** | 74.2% |

The gap between the two sound policies is 0.8, 0.5 and 0.3 points. §8's
conclusion was not an artefact of one program.

The new result is about seeds, and it is stronger than §8's version of it. §8
narrowed the seeds from 129 safepoints to 27 blocking primitives and saw no
change. This goes the other way: starting from the single `ash_host_fiber_yield`
import and *adding* every blocking socket and wasi call — `poll_oneoff`,
`sched_yield`, `sock_*`, `ash_host_socket_*` — moves the closure by **2
functions out of 3,041**, while moving the direct-only lower bound from 11.0%
to 26.6%. The seeds visibly matter to the call graph and are invisible in the
answer.

That has a consequence for §9's open item. §9 ended by saying the projected
total depends on runtime-side annotation: stating which ash_std functions can
reach a suspend, rather than inferring it. At the wasm level that work is now
known not to pay — the closure is set by table fan-in, and a more precise seed
set is swallowed by it within two functions. The annotation is only worth
writing on the HL side of §9, where the seeds still propagate through a call
graph that has not been merged with libc's.

## 11. Step 1 closed against its oracle, and Step 2's oracle does not exist

§6 set the exit condition for Step 1: ash's suspend set must be a strict
*subset* of Binaryen's under matching seeds, since Binaryen's blanket indirect
rule can only ever be looser. That check has now run.

Both tools were seeded from the same single import
(`--pass-arg=asyncify-imports@env.ash_host_fiber_yield`) on `threads.wasm`,
Binaryen's list taken from `--pass-arg=asyncify-verbose` and ash's written by
`ASH_LINK_TEST_NAMES` in `tests/suspend_set.rs`:

- ash `TypedTable`: **2,617** functions
- Binaryen: **2,632** functions and the import
- in ash's and not Binaryen's: **0**
- in Binaryen's and not ash's: **15**, which is exactly `AnyIndirect` minus
  `TypedTable`

So the two analyses agree function for function under the blanket policy, and
the type partitioning removes precisely those fifteen. §8's conclusion is
confirmed from the other side: the refinement is correct, and it is worth
fifteen functions.

Run Binaryen single-threaded (`BINARYEN_CORES=1`) or the verbose output is
useless: it prints from a worker pool without holding a lock, so names and
`[asyncify]` prefixes interleave into each other mid-line.

**Step 2's oracle, on the other hand, is not available at all.** §6 planned to
test the rewrite differentially against `wasm-opt --asyncify` on an EH-free
module. Two things stand in the way, and both are now observed rather than
predicted:

- **Binaryen 116 cannot read an ash module.** `[parse exception: invalid wasm
  type: -23]` -- the `exnref` byte of the final EH proposal. Every module we
  have hits it, including the smallest.
- **Binaryen 132 reads it, analyses it, and then crashes**: `UNREACHABLE
  executed at src/passes/Flatten.cpp:231`. §3 predicted this from reading
  Flatten's source; it is now a stack trace.

And there is no EH-free ash module to fall back to: the prelinked runtime
object uses setjmp, so every module ash links carries `try_table`. If Step 2
is to have a differential oracle it has to be a *synthetic* one -- a small C
program compiled to wasm32-wasip1 with a yield import, instrumented both ways
and run against the same host. That is worth building, but it must be
recognised for what it is: an oracle for the transform's mechanics on shapes
ash does not emit, silent on the shapes it does.

Which raises the value of the checks that do cover ash's own output.
`crates/ash_wasm_link/src/cursor.rs` is the decode/annotate/re-encode loop §5
argued for, and it is checked three ways on real modules: an identity rewrite
of all 3,041 bodies of `threads.wasm` reproduces the operator stream exactly
(and is 4.4% smaller, the relocation padding recovered); wrapping every one of
those bodies in a new block -- which moves every branch in the module that
leaves its function -- still validates; and the wrapped `t.wasm`, all 1,958
bodies, runs and prints exactly what the original printed. Wrapped
`threads.wasm` produces the same twelve lines as the original and stops at the
same place, which is a pre-existing hang in `testShutdown_finishesSubmittedTasks`
and not something the rewrite introduced.

## 12. Flatten's job, measured — and it is nearly free

§7 listed the operand-stack statistics among "where the studies left gaps",
because they were read off `wasm-dis`'s folded rendering rather than the
binary. `crates/ash_wasm_link/src/fiber.rs` now does the transform, so the
number is exact.

The transform is local, not a Flatten: at each call site the operand stack is
popped into locals and pushed straight back. The values are unchanged and in
the same order, and each one now also sits in a local an unwind can spill.
Nothing is restructured, no tree is built, and `try_table` is not a special
case — which is the whole reason this exists, since Binaryen's Flatten aborts
on it (§11).

Over the 2,618 functions the suspend analysis selects in `threads.wasm`:

| | |
| --- | --- |
| functions refused | **0** |
| call sites | 25,328 |
| already empty (only the call's own arguments) | **24,449 — 96.5%** |
| values moved through a local | 3,057 |
| of those, live under a call | 1,098 |
| locals added | 1,545 |
| module size | 3,604,264 → 3,459,906 (**−4.0%**) |

`t.wasm` is the same shape: 97.7% of call sites need nothing, 683 locals
added, −3.4%, and the spilled module runs and prints exactly what the original
printed.

Three things in that table were open questions the design could not answer:

**Both refusal shapes are absent.** §7 named `return_call` as the thing that
"closes this door permanently", and a value belonging to an enclosing frame is
the case that would force the global restructure. Neither occurs anywhere in
ash's output. Both are still refused per function rather than assumed away —
the decision is taken in a first walk whose output is discarded, so a function
is never left half instrumented — but today nothing triggers either.

**LLVM has already done the flattening.** 96.5% of call sites have nothing on
the stack but the call's own arguments, because the wasm backend keeps values
in locals across anything interesting. Flatten exists to manufacture a
property ash's output very nearly has already. The remaining 3.5% cost 6,114
instructions in a body of 793,780 operators.

**The transform makes the module smaller.** Re-encoding recovers more
relocation padding (§10's −4.4%) than the instrumentation adds. That does not
survive the prologue and epilogue work still to come, but it does mean the
stack handling — the part with no precedent to copy — is not what will cost.

## 13. The rewind jump, built and run

The remaining structural unknown was how a rewind gets back to the call it
suspended at, since wasm has no `goto` and Binaryen's answer depends on Flatten
having made the body a flat sequence first.

**Ladders, one per control frame.** Each frame gets a run of nested blocks
whose ends sit at that frame's resume points, and a `br_table` at the top that
leaves the ladder at the right one. Landing in a nested frame is the same
problem one level down, so they compose: the outer ladder jumps to just before
the inner frame, and the inner one takes over. Two shapes need no special
handling and it is worth saying why:

- A **loop** is resumed by jumping into its body. Which iteration this is lives
  in the locals, and a rewind restores those, so there is nothing else to
  reconstruct.
- An **`if`** has two sequences sharing one entry. Each arm gets its own
  ladder; the condition steers, and it is restored the same way a call's
  operands are.

**The stack has to be empty where a ladder ends**, which is the whole reason
§12 came first. Where a call's operands are all `local.get`s and constants, the
target goes before them and the rewind re-executes them at no cost; otherwise
they go through locals and the target sits between the stores and the loads.
The same analysis serves the unwind: a value a rewind can recompute is one an
unwind need not save.

**The `br_table` sizing was worth catching.** A frame's call ordinals are a
contiguous range — ordinals follow body order and a frame is a contiguous span
of it — so each table spans only that range, and an ordinal outside it
underflows an unsigned index straight to the default arm. Sizing tables to the
whole function instead costs an entry per call per *enclosing* frame, which is
quadratic where this is not: on `t.wasm`, 390,163 entries against 86,531, and
+29.6% against +16.6%.

| | `t.wasm` | `threads.wasm` |
| --- | --- | --- |
| functions given a dispatch | 1,625 | 2,617 |
| refused | **0** | **0** |
| ladders | 11,634 | 22,965 |
| blocks added | 33,480 | 68,641 |
| `br_table` entries | 86,531 | 214,631 |
| resume points needing no spill | 5,983 (51%) | 12,909 (51%) |
| module size | +16.6% | +22.9% |

Half of all resume points cost nothing at all, which is a weaker result than
§12's 96.5% and for a different reason: §12 asked whether anything was live
*under* the arguments, while a ladder needs the arguments themselves to be
recomputable, and about half of ash's call sites compute an argument rather
than loading one.

Tested by running what it builds. The unit tests drive a transformed module
under `wasmtime` and check which calls execute: resuming at each call of a flat
body, through two nested frames, into either arm of an `if`, and inside a loop
where exactly one iteration is entered part-way and the rest run whole. On the
linked modules, every instrumented function takes a dispatch with none refused,
both validate, and with the resume value left at zero `t.wasm` prints exactly
what it printed and `threads.wasm` reaches the same place.

What is left is the state machine: the side stack, the unwind that spills and
returns, the prologue that restores and sets the resume value, and the
scheduler of §6 step 4. The jump machinery those drive is now built, and so is
the evidence that it costs about a fifth of a module rather than the 3.5x §8
budgeted for.

## 14. A fiber that suspends and comes back

The state machine is written and it works. Three `i32` globals, exported so the
host can drive them, do what Asyncify needs five exported functions for —
`ash_fiber_state` (0 running, 1 unwinding, 2 rewinding, Asyncify's encoding
kept so the runtime side reads against a published one), `ash_fiber_data`
pointing at the side stack's two-word header, and `ash_fiber_resume` carrying
the ordinal a rewind is heading for.

- **Prologue.** If rewinding, take this frame's record back off the side stack,
  set the resume value from the ordinal it holds, and restore every local. The
  ladders of §13 do the rest.
- **Epilogue**, after every call that could suspend: if unwinding, push this
  frame's record and return. The bounds check is in the push, so an overflow is
  reported by the frame that overran, which Binaryen's own version leaves as a
  TODO.
- Records come off in the reverse of the order they went on, which is what
  lines the outermost frame — saved last, restored first — up with the order a
  rewind re-enters frames in.

§7 named "a spill set that disagrees with itself" as the failure nothing would
report. It cannot happen here by construction: prologue and epilogue read one
list of locals in index order, and the scratch pointer is the only local not in
it, because restoring it would overwrite the pointer being read through.

**The exnref assumption does not hold, and it is cheap that it doesn't.** §3
took it from LLVM that a reference is never live across a call. Measured by
refusing any function holding one: 3 in `t.wasm`, **21 in `threads.wasm` — its
21 EH bodies exactly**. The check is on presence rather than liveness, so a
liveness pass would likely recover most of them, but at these numbers refusing
is affordable. What is *not* affordable is refusing silently: a function the
analysis says can be on the stack at a suspend does not become safe by going
uninstrumented, so it traps instead, in the frame that would have been wrong.
That is 37 places in `t.wasm` and 445 in `threads.wasm` — §6's assert mode,
shipped with the transform rather than after the first mysterious answer.

| | `t.wasm` | `threads.wasm` |
| --- | --- | --- |
| functions instrumented | 1,622 | 2,596 |
| refused (a reference-typed value) | 3 | 21 |
| traps in refused-but-reachable functions | 37 | 445 |
| unwind checks | 9,764 | 19,192 |
| side-stack bytes per frame, summed | 77,496 | 132,088 (≈51 each) |
| module size | **+35.7%** | **+43.0%** |

Against §8's budget of a 3.5x code section, that is the good end of the range,
and it is now a measurement rather than an estimate.

The test that means something runs it: a fiber suspends inside a nested call,
both frames save themselves and return, the host sets the state to rewinding
and calls again, and both frames come back exactly where they stopped — past
the calls they had already made and not past the ones they had not. On the
linked modules the transform validates, and with the state left at zero
`t.wasm` prints exactly what it printed.

**Where the size went.** The first working version wrote every local at every
call site, which is (call sites x locals) and was +98.1% and +172.9%. The whole
body now sits inside a block whose end holds one copy of the save sequence, and
a call site that finds itself unwinding stores its ordinal and branches there:
six instructions instead of about `3L + 10`. That is the 35.7% and 43.0% above,
a fourfold cut, with the round trip and the linked module's own output
unchanged.

What is left on the table is liveness — saving only the locals actually live
across a call, rather than all of them. It would shrink both the side-stack
frame (51 bytes average) and the save sequence, and it is the same analysis
that would turn the 21 EH refusals into a handful. Neither is needed for
correctness.

## 15. Wiring it in, and what the runtime must do next

The transform is behind `LinkOptions::fibers` (default false) and reached by
`ASH_WASM_FIBERS=1` at the one place ash calls the linker. The gate is
structural rather than disciplinary: with the flag off, `link` returns exactly
what `emit` produced and nothing in `fiber.rs` runs, so the byte-identical
guarantee §6 asked for needs no CI check to hold. `fiber::instrument` is the
one definition of what the transform is, and `link` only decides whether to
call it.

It refuses twice rather than doing half a job. A module that does not import
`env.ash_host_fiber_yield` has no suspend point, so instrumenting it would
cost every function in the suspend set to produce a program that can never
suspend. And a module already exporting one of the three global names would be
invalid at instantiation, with nothing pointing at the cause.

One detail is load-bearing and was nearly got wrong: the globals are
**appended**, past the GOT block. GOT global indices start at 3
(`link.rs:412`) and are already written into patch sites by
`apply_relocations`. Inserting three globals there gives every GOT reference a
wrong address in a module that validates perfectly — this crate's signature
failure. Appending renumbers nothing.

`scripts/haxe_conformance.py` gains a `wasm-fibers` arm, keyed and pathed
separately from `wasm`. That separation is the point: the module cache and
output path were keyed on the program alone, so a second wasm-shaped arm
sharing them would be handed the first arm's module and publish a duplicate of
one column under the other's name. It is not in CI: it is a full wasm link per
program, and the transform has not yet earned that.

### The four things the runtime has to get right

The linker's half is done. None of the following is:

**The side stack must be a GC root.** This is the one that produces a wrong
answer rather than a failure. `Fiber::stack_range` currently returns
`(null, 0)` and says, correctly for today, that "a wasm module's stack lives in
the engine, not in linear memory, so there is nothing here to scan". The
transform makes that false: a suspended fiber's locals are written into linear
memory, and any of them may be an object pointer. Left unscanned, an object
reachable only from a suspended fiber is collected and the fiber resumes onto
freed memory. `stack_range` is already the hook — it has to start returning the
fiber's side-stack window.

**The side stack must come from the guest, not the host.** The unit tests here
pick the literal addresses 1024 and 8192, which is fine for a module with no
data and wrong for every real one: `__heap_base` and `__heap_end` are baked in
at link time and guest memory is grown by the guest's own allocator. A host
that picks addresses will eventually write over data.

**Each fiber needs its own shadow stack.** The linker gives exactly one, 65,536
bytes, and the transform deliberately leaves a suspended frame's shadow
allocation in place — correct for one coroutine, wrong for N on one
`__stack_pointer`. Two fibers deep in different call chains and the second
allocates over the first's live frames. `__stack_pointer` is global 0 and is
not exported today; exporting it hands every host arbitrary write access to the
guest's stack pointer, so it should be gated on the fibers flag or replaced
with a pair of synthesized accessors.

**A fiber must not suspend under a trap.** The transform emits a trap in every
function it could not instrument but which the analysis says can be on the
stack at a suspend: 37 places in `t.wasm`, 445 in `threads.wasm`, and 6,855 in
the unit suite. Those are honest — they turn a wrong answer into a report — but
a scheduler that drives a fiber into one gets an abort, not a suspension.

And one question nobody has answered: whether ash's wasm setjmp lowering keeps
module-global state of its own. If it does, it is per-call-stack and has to be
saved and restored per fiber alongside `__stack_pointer`.

## 16. The gate turned out not to be checkable, and now is

§6 asked for the fibers gate to be enforced by a byte-diff of a linked `t.wasm`
with the flag off. Checking that revealed the check could not have worked:
**ash did not build the same program to the same bytes.** Three builds of one
program with one compiler gave three different objects — identical size, about
a hundred bytes apart, a different result every process — in the shared AOT
path, so native binaries too.

The cause was one map read six ways. `c_ptr_to_type_index` was many-to-one:
the `HPACKED`/`HNULL`/`HREF` arm converted its type parameter through a path
that did not consult the shared cache, and the index is not registered until
the enclosing conversion returns, so a parameter reached mid-recursion had a
second descriptor boxed for it — exactly three per program, the primitives
under `Null<...>`. Six places then asked that map the reverse question, which
descriptor represents this index, by scanning it. A `HashMap` scan answers in
hash order, seeded per process, and the winner reached the object: a constant's
type header, a `safe_cast` operand, an `HNULL`'s `tparam`.

Both halves are fixed. Reverse lookups go through a canonical index-to-
descriptor map (first-wins, deterministic because the build order is), and the
duplicate is no longer created. Sorting would not have worked, and this is
worth remembering: the map's keys are heap addresses, so any pointer-derived
ordering is still allocator-dependent.

Builds are now byte-reproducible — five of five objects and three of three
linked modules identical — the objects are 126 bytes smaller for the
descriptors no longer emitted, and `ASH_WASM_FIBERS=0` produces a module byte-
identical to an unset build. The gate §6 asked for is now a thing that can be
checked rather than a thing that would have passed for the wrong reason.

Behaviour is unmoved: across the 63 test programs, interp, jit and AOT agree on
58, and the five that differ are the two mandelbrots — the documented
FP-contraction difference, with AOT matching clang's `-ffp-contract=on` value
exactly — and three socket tests with nothing to connect to.

## 17. The transform against the conformance suite

The transform had been checked on eight modules. It has now been run against
the Haxe suite, both arms in one pass so nothing about the machine differs
between them: `--modes wasm,wasm-fibers`, per-case isolation, 1,195 cases.

| | `wasm` | `wasm-fibers` |
| --- | --- | --- |
| cases attempted | 1,069 | 1,069 |
| passed | **1,069 (100%)** | **1,069 (100%)** |
| failed / crashed / timed out | 0 / 0 / 0 | 0 / 0 / 0 |
| assertions | 10,971 / 10,971 | 10,971 / 10,971 |
| empty on this target | 126 | 126 |
| module | 27,118,878 bytes | 34,291,810 (+26.4%) |

Not one case changed answer, and the 126 the harness records as not-OK are
`EMPTY` — utest reporting no runnable tests on this target — the same 126 in
both arms. The threads suite times out identically under both (120,015ms and
120,018ms), which is the pre-existing hang in
`testShutdown_finishesSubmittedTasks` and not something the transform
introduced.

`scripts/compare_conformance_arms.py` is what says that, and it exists because
the summary cannot: two arms can both report 100% while failing different
cases. It compares the tallies and the not-OK *sets* per program.

On that evidence the arm is now in CI, last in `--modes`. Last matters: the
headline is the first mode with a summary and every top-level number derives
from it, so a new arm at the front silently redefines what the site publishes.

What this does and does not establish. It establishes that instrumenting every
function in the suspend set is inert when nothing suspends — which is the
property every non-fiber program relies on, and the one that would have been
violated by a mis-renumbered branch or a spill that clobbered a live value. It
establishes nothing about suspending, because with the state global at zero no
fiber ever does. That still needs the runtime.

## 18. A Haxe thread suspends on wasm

```
A worker started
B main got 1
C worker resumed with 9
D main got 2
```

A `sys.thread` worker blocks inside `Deque.pop(true)`, main runs and sends it a
value, and the worker resumes at the point it stopped. That is byte for byte
what the interpreter prints natively. The same program built without the
transform prints its first line and exits: the worker blocks, nothing can take
it off the stack, and main never wakes. The program is in
`scratchpad/fibtest/T2.hx` in shape and is worth keeping, because it is the
smallest thing that a run-to-completion backend cannot fake — a worker that
must be resumed *in the middle* rather than started at a convenient moment.

At suite scale, the Haxe threads suite:

| | `wasm` | `wasm-fibers` |
| --- | --- | --- |
| result | TIMEOUT at 120,012ms | **PASS in 11,378ms** |
| cases reached | 4 | 9 |
| tests passed | **0 of 8** | **22 of 22** |

Four things had to meet, and three of them were only discovered by running it.

**`ash_fiber_enter` must not be inlined.** It is the edge an unwind stops at,
and its body is one call, so LLVM inlines it into `Fiber::resume` given the
chance — taking with it the frame the whole design rests on and the name the
linker looks it up by. `#[inline(never)]` is load-bearing, not a hint.

**Refusal had to become about liveness, not presence.** A reference cannot be
written to linear memory, and refusing every function holding one refused the
worker's own body: LLVM's setjmp lowering leaves an `exnref` local in almost
every compiled Haxe closure. Refusing only where one might still hold something
*across a resume point* — approximated by the span between a local's first
write and its last read, which is a superset of real liveness and so errs the
safe way — takes `threads.wasm` from 21 refusals and 445 traps to none of
either. That also settles the question §14 left open: LLVM does not leave an
`exnref` live across a call.

**Each fiber needs its own shadow stack**, and the linker exports
`__stack_pointer` when fibers are on so the runtime can swap it. The transform
deliberately leaves a suspended frame's shadow allocation in place, which is
right for one coroutine and wrong for two: the frames between the suspend and
the scheduler return, restore the pointer *above* the suspended fiber's frames,
and the next allocation writes over them. It presented as an out-of-bounds read
at a wild address three lines into the test, and it is the hazard §15 named.

**The state has to be reachable from the guest.** A global the linker adds
after the guest is compiled has no name the guest can refer to, so
`ash_host_fiber_state` and `ash_host_fiber_arm` are imports and the host reads
and writes the globals on the guest's behalf. `arm` also swaps the shadow stack
and hands back the pointer it replaced, which is how a fiber gets its own
region and gives the caller's back.

The side stack and the shadow stack share one allocation, so the collector has
one range to scan and a side stack running into the shadow stack is caught by
the bounds check the transform already emits. Both are registered as GC roots
on wasm now: a suspended fiber's locals are in linear memory and may be the
only reference to an object.
