# Suspending a fiber inside a wasm module

A Haxe thread can stop part-way through a call inside a single-threaded wasm
module and start again later, with no engine feature and no host driver. Turn
it on with `ASH_WASM_FIBERS=1`, which sets `LinkOptions::fibers`; with the flag
off `link` returns exactly what `emit` produced and nothing in `fiber.rs` runs,
so the byte-identical guarantee holds structurally rather than by CI check.

`std/src/fiber.rs` routes every fiber operation through four calls that
`ash_wasm_runtime::guest` implements. Without the transform, `resume` runs the
body to completion and `yield_now` returns immediately. With it, `yield_now`
unwinds the wasm call stack out to `Fiber::resume` and returns *there*, and
`stack_range()`/`saved_sp()` stop returning null, because a suspended fiber's
live values are spilled into linear memory where the collector can scan them.
JSPI cannot offer that, because the engine holds the stack.

It does not give parallelism, suspension from inside a host callback, or
reentrancy — `Fiber::resume` may not be called from a suspendable frame.

## What it borrows from Asyncify

The three-state machine (`Normal=0`, `Unwinding=1`, `Rewinding=2`) in one
mutable i32 global; the side-stack layout of a two-word `{ current_pos, end }`
header and a stack growing up, holding per frame a call index then the live
locals padded to four bytes; and the five-function API under Binaryen's exact
names, so a `wasm-opt --asyncify` module and an ash-instrumented one are
interchangeable under one driver.

The fake-global trick for call results matters more than it looks: a
`local.set $x (call f)` must not write `$x` on the unwinding path, or `$x` is
spilled holding a forged zero — a silent wrong answer.

## Where it diverges, and why

Binaryen is handed an arbitrary module and must assume the worst. ash emitted
the module from its own pipeline and knows what it put there.

**Seeds come from ash's own safepoints.** Binaryen seeds every import plus
every function containing a `call_indirect`, which marks 85–90% of functions
state-changing and roughly doubles the module. ash already emits
`hlp_fiber_poll` at AIR loop back-edges from both backends, so those sites plus
`env.ash_host_fiber_yield` plus a list of blocking natives are the only suspend
points — 131 sites in 73 of 1,958 functions in `t.wasm`, 280 in 128 of 3,041 in
`threads.wasm`.

**Indirect calls get an exact target set.** Binaryen offers "assume every
`call_indirect` suspends" (the whole blowup) or `asyncify-ignore-indirect`,
which is a correctness knob and unsound here since Haxe closures, virtual
dispatch and the HL function table all go through the table. The third option
is available only to us: the linker *builds* the table, and fixes it at
`minimum == maximum` so it cannot grow, so the target set is a list we already
have — narrowed per site to entries whose type matches.

**Rewind stays skip-forward.** A recorded resume label would need a `br_table`
ladder per nesting level, threaded through every enclosing block, loop and
`try_table` — that is a relooper, and it destroys the property that re-entering
a `try_table` reinstalls its handler for free. Rewind is O(the bodies of frames
actually on the suspended stack), not O(module).

**Each fiber owns its shadow stack and side stack.** Asyncify never touches
`__stack_pointer`, so a suspending frame's shadow allocation is left in place —
right for one coroutine, catastrophic for N on the single 64KB stack the linker
provides.

## No flatten, at all

Binaryen cannot process ash's modules, and it is three blockers deep: `Flatten`
has no `try_table` case and aborts; behind it Asyncify rejects any live local
without a byte size, so an `exnref` local is fatal; behind that
`AsyncifyFlow::process` has no `TryTable` case at all. Retreating to legacy
`try`/`catch` is not available — that is what every current engine refuses,
which is why ash passes `-wasm-use-legacy-eh=false`. Excluding EH-bearing
functions excludes the feature: in `threads.wasm` those 21 are `dispatch`,
`execute`, `runCases`, `runTest` and two hot loops, the exact frames a fiber
suspends inside.

Flatten exists to empty the operand stack *everywhere*. We need it empty only
at suspend points, and ash's emitter already delivers that: **100% of the 131
poll sites in `t.wasm` and all 280 in `threads.wasm` are bare statements with
an empty operand stack**, which is unsurprising because ash emits them. Across
all `$ash_f*` call sites, 95.28% have nothing pending, 3.85% have one pure
`local.get`/`const`/`global.get`, and five sites in the module have three.

What replaces Flatten is narrow: for each call in operand position in a
function being instrumented, spill its result to a fresh local before the
enclosing statement. Hoisting a pure operand past a call is unconditionally
safe because wasm locals are frame-private. This also avoids Binaryen's 25x
locals blowup — flatten's temporaries cost per expression over 347,856
instructions, ours cost per instrumented call site.

Three rules for `try_table`:

- **Suspending inside a body needs no special work.** Skip-forward rewind
  re-executes the structured instruction and reinstalls the handler.
- **Landing pads are recognised, not constructed.** LLVM's SjLj lowering emits
  exactly one shape, and emits it every time — all 6 value-flowing blocks in
  `t.wasm` and all 42 in `threads.wasm` sit directly under a `local.set`.
- **No suspend point may occur while an `exnref` is live**, checked rather than
  assumed: the transform refuses the link and names the function.

## The relocation decision

The linker's invariant is that nothing moves — relocations write fixed-width
slots at absolute offsets, and recomputing those is "the step that silently
corrupts a module". Inserting instructions breaks it three ways at once, all
silent.

**The transform runs after `apply_relocations`,** where every relocation has
been spent. `emit` reads only `func_out`, `code_bodies` and `code_payload` and
never touches an offset again, so nothing the transform does can invalidate an
offset anyone reads — and it sees final output function indices, so the
call-graph analysis needs no symbol table.

The cost moves to index space, and all of it is handled in `plan`:

- New functions append **after** `ctors_index`, or the start section points at
  a helper and the module instantiates and does nothing.
- New globals append past the GOT block. GOT indices start at 3 and are already
  written into patch sites; inserting there gives every GOT reference a wrong
  address in a module that validates perfectly.
- New types go through the existing dedup map.
- The transform must contribute names for synthesized functions, or every trap
  inside them prints `<wasm function N>`.
- `hlp_fiber_poll` and the scheduler entry points are *input* symbols. If they
  are not in `LinkOptions::roots`, tree shaking removes them before the
  transform sees a suspend point, and the module links and validates with no
  fibers in it.

## Refusals

The transform refuses rather than doing half a job. A module that does not
import `env.ash_host_fiber_yield` has no suspend point, so instrumenting it
would cost the whole suspend set to produce a program that can never suspend.
A module already exporting one of the three global names would be invalid at
instantiation with nothing pointing at the cause.

## Results

Haxe conformance, both arms in one pass, per-case isolation, 1,195 cases:

| | `wasm` | `wasm-fibers` |
| --- | --- | --- |
| passed | 1,069 (100%) | 1,069 (100%) |
| assertions | 10,971 / 10,971 | 10,971 / 10,971 |
| module | 27,118,878 bytes | 34,291,810 (+26.4%) |

Not one case changed answer. `scripts/compare_conformance_arms.py` is what
establishes that, because a summary cannot: two arms can both report 100% while
failing different cases, so it compares the not-OK *sets* per program.

This proves instrumentation is inert when nothing suspends — the property every
non-fiber program relies on, and what a mis-renumbered branch or a clobbering
spill would have broken. It proves nothing about suspending, since with the
state global at zero no fiber does.

The threads suite does suspend:

| | `wasm` | `wasm-fibers` |
| --- | --- | --- |
| result | TIMEOUT at 120,012ms | PASS in 11,378ms |
| tests passed | 0 of 8 | 22 of 22 |

The smallest case that cannot be faked: a `sys.thread` worker blocks inside
`Deque.pop(true)`, main sends it a value, and the worker resumes *at the point
it stopped* — byte for byte what the interpreter prints natively. Without the
transform the program prints its first line and exits.

The `wasm-fibers` conformance arm goes **last** in `--modes`. The headline is
the first mode with a summary and every top-level number derives from it, so a
new arm at the front silently redefines what the site publishes.

## Four things the runtime had to get right

- **`ash_fiber_enter` must not be inlined.** It is the edge an unwind stops at
  and its body is one call, so LLVM inlines it into `Fiber::resume` given the
  chance — taking the frame the design rests on and the name the linker looks
  it up by. `#[inline(never)]` is load-bearing.
- **Refusal is about liveness, not presence.** LLVM's setjmp lowering leaves an
  `exnref` local in almost every compiled Haxe closure, so refusing every
  function holding one refused the worker's own body. Refusing only where one
  could be held *across a resume point* — approximated by first write to last
  read, a superset of real liveness — takes `threads.wasm` from 21 refusals and
  445 traps to none of either.
- **Each fiber needs its own shadow stack.** The linker exports
  `__stack_pointer` when fibers are on so the runtime can swap it. Without
  that, the frames between the suspend and the scheduler restore the pointer
  *above* the suspended fiber's frames and the next allocation writes over
  them — presenting as an out-of-bounds read at a wild address.
- **The state must be reachable from the guest.** A global the linker adds
  after the guest is compiled has no name the guest can refer to, so
  `ash_host_fiber_state` and `ash_host_fiber_arm` are imports and the host reads
  and writes the globals on the guest's behalf. `arm` also swaps the shadow
  stack and hands back the pointer it replaced.

The side stack and shadow stack share one allocation, so the collector scans
one range and a side stack running into the shadow stack is caught by the
bounds check the transform emits. Both are GC roots on wasm: a suspended
fiber's locals are in linear memory and may be an object's only reference.

## Open

Whether ash's wasm setjmp lowering keeps module-global state of its own. If it
does, that state is per-call-stack and must be saved and restored per fiber
alongside `__stack_pointer`.

`return_call` would close this door permanently — a tail call cannot be wrapped
in an unwind check because the frame is already gone. `t.wasm` has none today,
so anyone enabling tail calls for wasm codegen has to be told first.
