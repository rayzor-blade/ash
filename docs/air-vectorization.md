# AIR loop vectorization

`air::v2::passes::widen` widens loops by four with a scalar epilogue. It runs
at O3 only. All three tiers and the SSA walker execute the vector instructions;
`serialize` scalarizes them back, so the opcode paths never see a vector.

`ASH_AIR_NO_WIDEN=1` turns it off. Try that first against a wrong answer: the
widener is the only O3 pass that rewrites arithmetic, so an unchanged result
with it off rules it out and points at the inliner or SROA instead.

## The instructions

`VecLoad`, `VecStore`, `VecSplat`, `VecBinOp`, `VecReduce`. Only the widener
produces them, only at O3, and only after `serialize` has taken its scalar
snapshot. Vector values never reach de-SSA and never need an HL register.

Supported: runtime trip counts through the epilogue, guard hoisting, affine
addressing (`i << 2`, `i + k`, `i * c`), contiguity from the element's byte
width, and integer reductions, whose partials collapse through a `VecReduce` on
a block spliced onto the exit edge.

The width gate is `lanes_fit`: `element_bytes * 4 <= 16`. An `i64` at four
lanes is 256 bits, which no NEON register holds.

## Four soundness rules

Each came from a wrong answer, not a failing build. Every one produced IR that
verified.

1. **Only a loop-invariant scalar may be broadcast.** `acc += i * 3 + 1`
   splatted the lane-zero term four times and returned 497032704 instead of
   1198000000.
2. **Every use of a widened value must be one the emit stage rewrites.** A
   widened value reaching a phi, a field store, or anything past the loop still
   names a definition that was just replaced.
3. **What follows the loop is the remainder's value, not the vector loop's.**
   The vector loop stops at `start + (n & ~3)`, so `return i` after it was up
   to three short. Verification passes either way — both values are defined and
   in scope.
4. **The IR must not hold a vector the machine cannot.** A widened `i64x4` made
   both Cranelift and LLVM's own-module path refuse the function, and because a
   tier-0 refusal was not remembered, a stub-bridge call re-lowered it every
   call: 311,362 declines in 156 seconds, game frozen with audio still playing.
   Totality of lowering covers *types*, not just instructions.

Two more that outlive this pass: a pass that deletes a definition must call
`compact_values`, and a pass that mints a constant is only safe for consumers
reading AIR rather than the serialized form.

## What the corpus widens

Exactly one loop widens in each program in the corpus, and it is the stdlib's
array fill every time.

| blocker | count | what it is |
|---|---|---|
| call in body | 47 | a Haxe array write carries grow-on-demand in its bounds-check slow path |
| may alias | 26 | a store paired with an access through a base the analysis cannot separate |
| bounds-check diamond | — | `i <u len ? a[i] : 0`, so the value reaches the accumulator through a phi |

"May alias" rose from one when affine addressing made those addresses visible
at all, which is a better answer than not seeing them. The diamond is rule 2 in
another form, and it is why reductions widen nothing new.

Measure with `cargo run --example vec_survey -- <file.hl>`; `ASH_VEC_ONLY=`
lists every loop. The survey reads the pass's own record via `take_outcomes`,
because re-running the analysis on transformed IR reports every success as a
refusal, and it drops the optimized-IR cache between files since that cache is
keyed by findex alone.

## Which loops could ever widen

| loop | widens? | why |
|---|---|---|
| nbody `advance` inner `j` | no, as written | `bodies[j].x` is array-of-structs, so lanes are not contiguous and would need a gather NEON lacks |
| nbody `for (body in bodies)` | yes | elementwise, no loop-carried values |
| mandelbrot escape | no | `z = z² + c` is serial |
| mandelbrot pixel loop | in principle | trip counts diverge per lane, so it needs masking and per-lane exit |
| call benches | no | loop-carried multiply chain, chosen to survive optimization |

nbody refuses on data layout, and LLVM hits the same wall from the other side:
asked to vectorize it, LLVM emits SLP across the x/y/z triple, packs two of
three components into one register, collapses them with a horizontal `faddp`,
and leaves `fsqrt` and `fdiv` scalar. One iteration still computes one body
pair. The unlock is an array-of-structs to struct-of-arrays layout change, or
gather support — not a better transform.

The Cranelift tier emits no SIMD of its own. A full CLIF dump for nbody has
eighteen scalar floating-point instructions and zero vector types; it executes
the vectors AIR hands it and finds none itself.

## Next, in order

1. **If-conversion under a lane mask** — retires the bounds-check diamond and
   lets reductions over array reads widen.
2. **Per-width vector factor** (`f64x2`, `i64x2`) so 64-bit elements widen by
   two instead of being refused by `lanes_fit`.
3. **Hoist grow-on-demand** out of an array write's slow path, which needs
   something to vouch that the callee is "ensure capacity".
4. **Alias disambiguation** strong enough to separate a store from an access
   through an unrelated base, or a runtime overlap guard where it cannot.
5. **Masked, divergent loops** last: per-lane exit masks are strictly harder.

Both sibling projects stalled in the same place. zyntax's three vectorization
passes have no memory dependence analysis and emit no runtime overlap guard;
rayzor's `LoopVectorizationPass` is O3-only, needs a compile-time-constant trip
count, and does no dependence analysis either. In both, the SIMD that shipped
was hand-written kernels.

## Testing

Gate changes on `o3_preserves_semantics`, which executes a widened loop with an
epilogue against the unoptimized version, and on the parity matrix:
`TestVectorize` covers lengths zero through twelve against a width of four.

Float reduction reassociation changes results, so it needs the same explicit
policy decision FMA contraction already has.

Test the wide element types. A widened stdlib loop is instantiated per element
type, so `Array<Int>` and `Array<haxe.Int64>` are different code — rule 4 was
found in the second.
