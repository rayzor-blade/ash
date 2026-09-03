# Loop vectorization in AIR: what widens, what refuses, and why

Status 2026-09-03. **AIR widens loops.** `air::v2::passes::widen` runs at O3,
widens by a fixed factor of four with a scalar epilogue, and all three tiers
plus the SSA walker execute the vector instructions it produces. `serialize`
scalarizes them back, so the opcode paths never see a vector.

That is a change from this document's earlier status, which read "nothing in
the product vectorizes a loop". What has *not* changed is the corpus: one loop
per program widens, and the reasons the rest refuse are measured below rather
than guessed at.

`ASH_AIR_NO_WIDEN=1` turns the pass off. It is the first thing to try against a
wrong answer, because the widener is the one O3 pass that rewrites arithmetic,
so "same result with the widener off" separates it from the inliner and SROA.

## What ships

Five instructions carry vectors through the IR: `VecLoad`, `VecStore`,
`VecSplat`, `VecBinOp`, `VecReduce`. They are produced only by the widener,
only at O3, and they are valid only after `serialize` has taken its scalar
snapshot — vector values never reach de-SSA and never need an HL register,
which is what makes them cheap to represent.

Working today: runtime trip counts through a scalar epilogue, guard hoisting,
affine addressing (`i << 2`, `i + k`, `i * c`), contiguity decided by the
element's own byte width, and integer reductions, whose vector partials are
collapsed by a `VecReduce` on a block spliced onto the exit edge.

The width gate is `lanes_fit`: `element_bytes * 4 <= 16`. An `i64` element at
four lanes is 256 bits, which no NEON register holds, and the consequences of
emitting one are in rule 4 below.

## The four soundness rules

Each was learned from a wrong answer, not from a failing build. Every one of
them produced IR that verified.

1. **Only a loop-invariant scalar may be broadcast.** `acc += i * 3 + 1`
   splatted the lane-zero term four times and a hot-loop test returned
   497032704 where the answer is 1198000000.
2. **Every use of a widened value must be one the emit stage rewrites.** A
   widened value reaching a phi, a field store, or anything past the loop is
   still naming a definition that was just replaced.
3. **What comes after the loop is the remainder's value, not the vector
   loop's.** The vector loop stops at `start + (n & ~3)`, so `return i` past it
   was up to three short. Verification passes either way, because both values
   are defined and in scope.
4. **The IR must not hold a vector the machine cannot.** A widened `i64x4`
   made Cranelift refuse the whole function and LLVM's own-module path refuse
   it too, and because a tier-0 refusal was not remembered, a stub-bridge call
   re-lowered the same function on every call: 311,362 declines in 156 seconds,
   with the game frozen and its audio still playing. The rule that lowering
   must be total covers *types*, not only instructions.

Two more that generalize past this pass: a pass that deletes a definition must
call `compact_values`, and a pass that mints a constant is only safe for
consumers that read AIR rather than the serialized form.

## What the corpus says

Measure with `cargo run --example vec_survey -- <file.hl>`; `ASH_VEC_ONLY=`
spells out every loop. The survey reads the pass's own record through
`take_outcomes`, because re-running the analysis on transformed IR reports
every success as a refusal, and it drops the optimized-IR cache between files
since that cache is keyed by findex alone.

One loop widens per program, and it is the same one every time: the stdlib's
array fill. The blockers, in order:

| blocker | count | what it is |
|---|---|---|
| call in body | 47 | a Haxe array write carries the grow-on-demand call in its bounds-check slow path |
| may alias | 26 | a store paired with an access through a base the analysis cannot separate |
| bounds-check diamond | — | an array read is `i <u len ? a[i] : 0`, so the value reaches the accumulator through a phi |

The "may alias" count went *up*, from one, when affine addressing made those
addresses visible at all. That is a better answer than not seeing them.

The diamond is rule 2 in a different costume, and it is why reductions widen
nothing new. Removing it needs if-conversion under a lane mask, and that is the
next piece of work.

## Which loops could ever vectorize

| loop | across iterations? | why |
|---|---|---|
| nbody `advance` inner `j` loop | no, as written | `bodies[j].x` reads a field of a per-iteration pointer: array-of-structs, so consecutive lanes are not contiguous and would need a gather NEON does not have |
| nbody final `for (body in bodies)` | yes, easily | elementwise over an array, no loop-carried values |
| mandelbrot escape loop | no | `z = z² + c` is serial by construction |
| mandelbrot pixel loop | in principle | vectorize across pixels, but trip counts diverge per lane, so it needs masking and a per-lane exit |
| call benches | no | loop-carried multiply chain, chosen so the work survives optimization |

nbody is worth dwelling on, because it is the loop everyone expects to widen.
It refuses on data layout, and LLVM hits the same wall from the other side:
asked to vectorize the same loop, LLVM emits SLP across the x/y/z triple
instead, packing two of three spatial components into one register and
collapsing them immediately with a horizontal `faddp`, while `fsqrt` and `fdiv`
stay scalar. One iteration still computes one body pair. The largest unlock
here is not the transform, it is an array-of-structs to struct-of-arrays layout
change, or gather support.

The Cranelift tier emits no SIMD of its own: a full CLIF dump for nbody
contains eighteen scalar floating-point instructions and zero vector types. It
executes the vectors AIR hands it, and finds none by itself.

## What is next, in order

1. **If-conversion under a lane mask**, which retires the bounds-check diamond
   and lets reductions over array reads widen.
2. **A per-width vector factor** — `f64x2`, `i64x2` — so 64-bit elements widen
   by two rather than being refused by `lanes_fit`.
3. **Hoisting the grow-on-demand call** out of an array write's slow path,
   which needs something to vouch that the callee is "ensure capacity".
4. **Alias disambiguation** strong enough to separate a store from an access
   through an unrelated base, or a runtime overlap guard where it cannot.
5. **Masked, divergent loops** last: per-lane exit masks are strictly harder
   than the uniform case.

Both sibling projects stalled at the same place, which is worth knowing before
spending months here: zyntax's three vectorization passes have no memory
dependence analysis and emit no runtime overlap guard, and rayzor's
`LoopVectorizationPass` is O3-only, requires a compile-time-constant trip
count, and does no dependence analysis either. In both, the SIMD that actually
ships came from hand-written kernels rather than the automatic pass.

## Testing

Gate any change on `o3_preserves_semantics`, which executes a widened loop with
an epilogue and compares against the unoptimized version, and on the parity
matrix: `TestVectorize` covers lengths zero through twelve against the width of
four. Floating-point reduction reassociation changes results, so a float
reduction needs the same explicit policy decision the FMA contraction already
documents.

Any widened stdlib loop is instantiated once per element type, so test the wide
ones: `Array<Int>` and `Array<haxe.Int64>` are different code, and rule 4 was
found in the second.
