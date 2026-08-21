# Loop vectorization in AIR: why we have none, and what it needs

Status 2026-08-21. **Nothing in the product vectorizes a loop.** Not the
Cranelift tier, not the LLVM tier, not AIR. What LLVM contributes is SLP —
straight-line vectorization *within one iteration* — which is a different
transform that leaves the iteration count untouched.

## What the machine code actually shows

nbody's inner loop, disassembled from the LLVM tier's own IR
(`llc -mcpu=apple-m1`):

```asm
    fsub    d4, d2, d4          ; dz  — scalar
    ldur    q5, [x17, #8]       ; load {x, y} as one 128-bit pair
    fsub.2d v5, v1, v5          ; {dx, dy} — two lanes
    fmul.2d v6, v5, v5          ; {dx², dy²}
    faddp.2d d6, v6             ; HORIZONTAL add: dx² + dy²
    fmadd   d6, d4, d4, d6      ; + dz²  — scalar
    fsqrt   d6, d6              ; scalar
    fdiv    d6, d0, d6          ; scalar
```

`faddp` is a horizontal reduce, and `fsqrt`/`fdiv` are scalar. That is the
signature of **SLP on the x/y/z triple**: it packs two of the three spatial
components into one register for the subtraction and squaring, then collapses
them immediately. One loop iteration still computes one body pair.

A *loop* vectorizer would do the opposite: keep two (or four) **iterations** in
flight, so `fsqrt` and `fdiv` become `fsqrt.2d`/`fdiv.2d` and there is no
horizontal op in the loop at all. That transform exists nowhere in our stack.

The Cranelift tier is further back still: a full CLIF dump for nbody contains
18 scalar FP instructions and **zero** vector types — no `f64x2`, no `f32x4`,
nothing. It emits no SIMD of any kind.

## Which of our loops are actually vectorizable

| loop | vectorizable across iterations? | why |
|---|---|---|
| nbody `advance` inner `j` loop | **yes**, with work | `b[j]` reads/writes are independent per `j`; `a.vx/vy/vz` are **reductions**; needs a proof that `a` and `b[j]` do not alias (guaranteed by `j` starting at `i+1`, but the compiler must establish it) |
| nbody final `for (body in bodies)` | **yes**, easily | pure elementwise over an array, no loop-carried values |
| mandelbrot escape loop | **no** | `z = z² + c` is serial by construction |
| mandelbrot pixel loop | yes *in principle* | vectorize across **pixels** — the classic mandelbrot SIMD shape — but trip counts diverge per lane, so it needs masking and a per-lane exit |
| call benches (`sum = sum*31 + i%8`) | **no** | loop-carried multiply chain, chosen deliberately by the benchmark so the work survives optimization |

So the payoff is concentrated in nbody and in masked outer-loop mandelbrot,
and the Cranelift tier gains on every shape because it currently emits nothing.

## What blocks it in AIR — corrected

Earlier notes listed three blockers. One of them does not apply to a loop
vectorizer, and that changes the shape of the work:

1. **No type space for a lane count.** `TypeRef` is an opaque index into the
   embedder's HL type table; `air` never interprets it, and its only type
   predicate is "is this float". A vector type has nowhere to live today.
   Lane info can ride on the instruction as an immediate instead of inventing
   a type space — which is what rayzor does, deliberately, because
   register-type inference is lost across inlining.

2. **Values are pinned to HL registers.** `ValueData { ty, reg }`, where `reg`
   is where de-SSA assigns the value. A 128-bit value has no HL register.

3. ~~Every instruction must round-trip to scalar bytecode.~~ **Not a blocker
   here.** The serialized array exists for the interpreter's SSA body and the
   LLVM tier; the Cranelift tier calls `lower_air_function(..., &opt.ir, ...)`
   and reads the **IR directly**. A vectorizer that runs as a late,
   codegen-only pass — after `serialize` has taken its scalar snapshot —
   produces vector values that never need an HL opcode at all. That is also
   the correct place for it on general principle: vectorization is a
   target-shaped transform, not a source-semantics one.

   Consequence: blocker 2 softens too. Vector values are exempt from the
   register invariant precisely because they never reach de-SSA.

The genuine gap is **analysis**, not representation. `analysis.rs` gives us
`CfgInfo`, dominators, a `LoopForest` and an `AliasClass` lattice. A loop
vectorizer additionally needs:

- **induction variable + trip count** recognition (affine `i = i0 + k*n`);
- **stride analysis** on memory accesses, so `b[j].x` is known to walk memory
  at a constant stride;
- **loop-carried dependence testing**, including recognizing a reduction
  (`a.vx -= ...`) as a legal accumulator rather than a barrier;
- **alias disambiguation** strong enough to prove `a` and `b[j]` are distinct,
  or a runtime overlap guard when it cannot.

Both sibling projects list exactly this analysis as their outstanding hole:
zyntax's three vectorization passes (~4.7k lines) have no memory dependence
analysis and emit no runtime overlap guard, and rayzor's `LoopVectorizationPass`
(2056 lines) is O3-only, requires a compile-time-constant trip count, and does
no dependence analysis either. In both projects the SIMD that actually ships
comes from hand-written kernels, not the automatic pass — which is a warning
about where the effort really goes.

## What the analysis pass actually found

`crates/air/src/v2/vectorize.rs` now answers this per loop, and
`cargo run --example vec_report -- <file.hl>` reports it. Over nbody,
mandelbrot, stdlib, jsonparse and a call bench — **388 loops, 5
vectorizable**:

```
224  call in body                 89  multiple exits
151  non-affine memory access     83  no induction variable
 71/65/11  Cell{Get,Incr,Set}     58  Cast (before no-op casts were allowed)
```

Two hypotheses died on contact with the data, and both are worth recording
because they are the obvious guesses:

* **"The multi-exit loops are bounds checks."** They are not. Teaching the
  analysis that a throw-only exit is a guard rather than divergence moved the
  count by zero. Dumping them shows genuine early-`break` loops —
  `while (cond) { ...; if (x) break; }` — which is what scanning and parsing
  code is made of, and stdlib/jsonparse are most of this corpus. Those need
  early-exit vectorization (speculate, mask, find-first), a substantially
  harder transform than the uniform case.
* **"nbody's inner loop just needs a vectorizer."** It does not. It is
  refused because `bodies[j].x` reads a field of a per-iteration *pointer*:
  array-of-structs, so consecutive lanes are not contiguous and would need a
  gather that NEON does not have. This is the same wall LLVM hit, which is
  why it emitted SLP on the x/y/z triple instead of vectorizing across `j`.

So the honest reading of this corpus is that its loops are mostly not
classically vectorizable — the scanning loops break early, the FP kernel is
blocked by data layout, and mandelbrot's escape loop is serial by
construction. A loop vectorizer is still worth building, but its payoff here
is bounded by those three facts, and the largest single unlock is **not** the
transform: it is either an AoS→SoA layout change or gather support.

## Ordered path

1. **Analysis first, transform second.** Induction/trip-count + stride +
   dependence testing on top of the existing `LoopForest`. This is the
   majority of the work and it is independently testable against the corpus
   before any codegen changes.
2. **A codegen-only vector value.** Lane count as an instruction immediate;
   vector values valid only after `serialize`, never reaching de-SSA.
3. **Widen + epilogue.** The standard transform: vector body for
   `trip - trip % W` iterations, scalar epilogue for the remainder. Reductions
   get a vector accumulator plus one horizontal collapse *after* the loop —
   which is exactly the `faddp` that today sits *inside* it.
4. **Cranelift lowering.** CLIF's `fadd`/`fmul`/`fma`/`fsqrt`/`fdiv` are
   already polymorphic over lane types, so this is short, and it is the tier
   with zero SIMD today.
5. **LLVM tier.** Either teach it to consume the vector IR the same way, or
   let its own vectorizer keep the shapes it already finds — the tier that
   gains least should not gate the tier that gains most.
6. **Masked/divergent loops** (mandelbrot across pixels) last: per-lane exit
   masks are a strictly harder transform than the uniform case.

FP reduction reassociation changes results, so step 3 needs the same explicit
policy decision the FMA contraction already documents.
