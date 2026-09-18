# wasm: suspending a fiber inside a module

With `ASH_WASM_FIBERS=1` at build time a Haxe thread can stop part-way
through a call inside a single-threaded wasm module and resume later, with
no engine feature and no host driver. The flag sets `LinkOptions::fibers`;
with it off, `link` returns exactly what `emit` produced and nothing in
`fiber.rs` runs, so the byte-identical guarantee holds structurally.

`std/src/fiber.rs` routes every fiber operation through four calls that
`ash_wasm_runtime::guest` implements. Without the transform, `resume` runs
the body to completion and `yield_now` returns immediately. With it,
`yield_now` unwinds the wasm call stack out to `Fiber::resume` and returns
there, and `stack_range()`/`saved_sp()` stop returning null because a
suspended fiber's live values are spilled into linear memory where the
collector can scan them. JSPI cannot offer that: the engine holds the stack.

Not provided: parallelism, suspension from inside a host callback,
reentrancy (`Fiber::resume` may not be called from a suspendable frame).

## Borrowed from Asyncify

The three-state machine (`Normal=0`, `Unwinding=1`, `Rewinding=2`) in one
mutable i32 global; the side-stack layout of a `{ current_pos, end }` header
and a stack growing up holding per frame a call index then the live locals
padded to four bytes; and the five-function API under Binaryen's names, so a
`wasm-opt --asyncify` module and an ash-instrumented one are interchangeable
under one driver.

The fake-global trick for call results is load-bearing: a
`local.set $x (call f)` must not write `$x` on the unwinding path, or `$x` is
spilled holding a forged zero.

## Where it diverges

Binaryen is handed an arbitrary module; ash emitted this one and knows what
it put there.

**Seeds are ash's own safepoints.** Binaryen seeds every import plus every
function containing a `call_indirect`, which marks most functions
state-changing and roughly doubles the module. ash already emits
`hlp_fiber_poll` at AIR loop back edges from both backends, so those sites
plus `env.ash_host_fiber_yield` plus a list of blocking natives are the only
suspend points: 131 sites in 73 of 1,958 functions in the unit suite, 280 in
128 of 3,041 in the threads suite.

**Indirect calls get an exact target set.** Binaryen offers "every
`call_indirect` may suspend" or `asyncify-ignore-indirect`, which is unsound
here since closures, virtual dispatch and the HL function table all go
through the table. The linker builds the table and fixes it at
`minimum == maximum`, so the target set is a list it already has, narrowed
per site to entries whose type matches.

**Rewind is skip-forward.** A recorded resume label would need a `br_table`
ladder per nesting level threaded through every enclosing block, loop and
`try_table` — a relooper — and would lose the property that re-entering a
`try_table` reinstalls its handler for free. Rewind is O(the bodies of
frames on the suspended stack), not O(module).

**Each fiber owns its shadow stack and side stack.** Asyncify never touches
`__stack_pointer`, so a suspending frame's shadow allocation is left in
place — fine for one coroutine, catastrophic for N on the single 64 KB stack
the linker provides.

## No flatten

Binaryen cannot process ash's modules: `Flatten` has no `try_table` case;
Asyncify rejects any live local without a byte size, so an `exnref` local is
fatal; `AsyncifyFlow::process` has no `TryTable` case. Legacy `try`/`catch`
is what every current engine refuses. Excluding EH-bearing functions
excludes the feature — in the threads suite those 21 are `dispatch`,
`execute`, `runCases`, `runTest` and two hot loops, exactly the frames a
fiber suspends inside.

Flatten exists to empty the operand stack everywhere; it needs to be empty
only at suspend points, and ash's emitter already delivers that: every poll
site in both suites is a bare statement with an empty operand stack. What
replaces Flatten is narrow: for each call in operand position in an
instrumented function, spill its result to a fresh local before the
enclosing statement. Hoisting a pure operand past a call is safe because
wasm locals are frame-private.

Three rules for `try_table`:

- Suspending inside a body needs no special work; skip-forward rewind
  re-executes the structured instruction and reinstalls the handler.
- Landing pads are recognised, not constructed. LLVM's SjLj lowering emits
  one shape, always directly under a `local.set`.
- No suspend point may occur while an `exnref` is live. Checked, not
  assumed: the transform refuses the link and names the function.

## Relocations

The linker's invariant is that nothing moves: relocations write fixed-width
slots at absolute offsets. Inserting instructions would break that silently,
so **the transform runs after `apply_relocations`**, where every relocation
has been spent. `emit` reads only `func_out`, `code_bodies` and
`code_payload` and never touches an offset again, and it sees final output
function indices, so the call-graph analysis needs no symbol table.

The cost moves to index space, all handled in `plan`:

- New functions append after `ctors_index`, or the start section points at a
  helper and the module instantiates and does nothing.
- New globals append past the GOT block. GOT indices start at 3 and are
  already written into patch sites.
- New types go through the existing dedup map.
- Synthesized functions get names, or every trap in them prints
  `<wasm function N>`.
- `hlp_fiber_poll` and the scheduler entry points are input symbols. If not
  in `LinkOptions::roots`, tree shaking removes them before the transform
  sees a suspend point, and the module links with no fibers in it.

## Refusals

A module that does not import `env.ash_host_fiber_yield` has no suspend
point and is not instrumented. A module already exporting one of the three
global names would be invalid at instantiation, so that refuses too.

## Results

Both conformance arms, per-case isolation, 1,195 cases: identical not-OK
sets (`scripts/compare_conformance_arms.py` compares the sets per program,
since two arms can both report 100% while failing different cases). Module
size +26%. This proves instrumentation is inert when nothing suspends.

The threads suite does suspend: `wasm` times out at 120 s with 0 of 8 tests,
`wasm-fibers` passes 22 of 22 in 11 s. The smallest case: a `sys.thread`
worker blocks in `Deque.pop(true)`, main sends it a value, and the worker
resumes at the point it stopped.

The `wasm-fibers` arm goes last in `--modes`: the headline is the first mode
with a summary and every top-level number derives from it.

## Four runtime details

- **`ash_fiber_enter` must not be inlined.** It is the edge an unwind stops
  at; its body is one call, so LLVM inlines it into `Fiber::resume` given the
  chance. `#[inline(never)]` is load-bearing.
- **Refusal is about liveness, not presence.** LLVM's setjmp lowering leaves
  an `exnref` local in almost every compiled closure. Refusing only where one
  could be held across a resume point (first write to last read, a superset
  of real liveness) took the threads suite from 21 refusals and 445 traps to
  none.
- **Each fiber needs its own shadow stack.** The linker exports
  `__stack_pointer` when fibers are on so the runtime can swap it. Otherwise
  the frames between the suspend and the scheduler restore the pointer above
  the suspended fiber's frames and the next allocation overwrites them.
- **The state must be reachable from the guest.** A global the linker adds
  after the guest is compiled has no name the guest can refer to, so
  `ash_host_fiber_state` and `ash_host_fiber_arm` are imports the host
  answers on the guest's behalf; `arm` also swaps the shadow stack.

The side stack and shadow stack share one allocation, so the collector scans
one range and a side stack running into the shadow stack hits the bounds
check the transform emits. Both are GC roots: a suspended fiber's locals may
be an object's only reference.

## Open

Whether ash's wasm setjmp lowering keeps module-global state of its own; if
so it is per call stack and must be saved and restored per fiber with
`__stack_pointer`. `return_call` would close this door: a tail call cannot
be wrapped in an unwind check because the frame is gone. No module has any
today; enabling tail calls for wasm codegen must revisit this.
