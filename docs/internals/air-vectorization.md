# AIR loop vectorization

`air::v2::passes::widen` widens loops by four with a scalar epilogue. O3
only. All tiers and the SSA walker execute the vector instructions;
`serialize` scalarizes them back, so the opcode interpreter never sees a
vector.

`ASH_AIR_NO_WIDEN=1` turns it off. It is the first switch to flip on a wrong
answer: the widener is the only O3 pass that rewrites arithmetic, so an
unchanged result rules it out and points at the inliner or SROA.

## Instructions

`VecLoad`, `VecStore`, `VecSplat`, `VecBinOp`, `VecReduce`. Only the widener
produces them, only at O3, and only after `serialize` has taken its scalar
snapshot. Vector values never reach de-SSA and never need an HL register.
(`VecOp`, `VecExtract` and `VecInsert` are the ash-simd forms and come from
lowering and SROA, not from here.)

Supported: runtime trip counts through the epilogue, guard hoisting, affine
addressing (`i << 2`, `i + k`, `i * c`), contiguity from the element's byte
width, and integer reductions closed by a `VecReduce` on a block spliced
onto the exit edge.

The width gate is `lanes_fit`: `element_bytes * 4 <= 16`. An `i64` at four
lanes is 256 bits, which no 128-bit register holds.

## Soundness rules

Each of these produced IR that verified and a wrong answer at run time.

1. **Only a loop-invariant scalar may be broadcast.** Splatting a term that
   varies per lane duplicates lane zero four times.
2. **Every use of a widened value must be one the emit stage rewrites.** A
   widened value reaching a phi, a field store, or anything past the loop
   still names a definition that was just replaced.
3. **What follows the loop is the remainder's value, not the vector loop's.**
   The vector loop stops at `start + (n & ~3)`; `return i` after it is up to
   three short unless it reads the epilogue's counter.
4. **The IR must not hold a vector the machine cannot.** A widened `i64x4`
   made both backends refuse the function, and because a tier-0 refusal was
   not remembered the function was re-lowered on every call. Totality of
   lowering covers types, not only instructions.

Two more that outlive this pass: a pass that deletes a definition calls
`compact_values`, and a pass that mints a constant is only safe for
consumers reading AIR rather than the serialized form (`pending_ints` covers
the serialized form now).

## What the corpus widens

One loop per program, and it is the stdlib's array fill every time. The
blockers, from the survey:

| blocker | count | |
|---|---|---|
| call in body | 47 | a Haxe array write carries grow-on-demand in its bounds-check slow path |
| may alias | 26 | a store paired with an access through a base the analysis cannot separate |
| bounds-check diamond | — | `i <u len ? a[i] : 0`: the value reaches the accumulator through a phi (rule 2) |

`cargo run --example vec_survey -- <file.hl>` runs the survey; `ASH_VEC_ONLY=`
lists every loop. It reads the pass's own record via `take_outcomes`, because
re-running the analysis on transformed IR reports every success as a
refusal, and it drops the optimized-IR cache between files since that cache
is keyed by findex alone.

## Which loops could ever widen

| loop | widens? | why |
|---|---|---|
| nbody `advance` inner `j` | no, as written | `bodies[j].x` is array-of-structs; lanes are not contiguous and would need a gather |
| nbody `for (body in bodies)` | yes | elementwise, no loop-carried values |
| mandelbrot escape | no | `z = z² + c` is serial |
| mandelbrot pixel loop | in principle | trip counts diverge per lane: needs masking and per-lane exit |
| call benches | no | loop-carried multiply chain, chosen to survive optimization |

nbody's blocker is layout. LLVM reaches the same wall from the other side:
asked to vectorize it, it SLP-packs two of the three components, collapses
them with a horizontal add, and leaves `fsqrt` and `fdiv` scalar; one
iteration still computes one body pair. The unlock is struct-of-arrays or a
gather, not a better transform.

Cranelift emits SIMD only for the vector forms AIR hands it; it has no loop
vectorizer of its own. On a program where the widener refuses every loop its
CLIF holds no vector type.

## Next, in order

1. If-conversion under a lane mask — retires the bounds-check diamond and
   lets reductions over array reads widen.
2. Per-width vector factor (`f64x2`, `i64x2`) so 64-bit elements widen by two
   instead of being refused.
3. Hoist grow-on-demand out of an array write's slow path, which needs
   something to vouch that the callee is "ensure capacity".
4. Alias disambiguation strong enough to separate a store from an access
   through an unrelated base, or a runtime overlap guard.
5. Masked, divergent loops last.

## Testing

`o3_preserves_semantics` executes a widened loop with an epilogue against the
unoptimized version; `TestVectorize` in the parity matrix covers lengths zero
through twelve at width four. Float reduction reassociation changes results
and needs the same explicit policy decision FMA contraction has. Test the
wide element types: a widened stdlib loop is instantiated per element type,
so `Array<Int>` and `Array<haxe.Int64>` are different code, and rule 4 was
found in the second.
