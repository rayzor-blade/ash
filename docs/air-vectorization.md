# Why AIR does not vectorize, and what it would take

Status as of 2026-08-21: **AIR has no vector notion at all** — no vector type,
no vector instruction, no vectorizing pass. The Cranelift tier therefore emits
purely scalar code, and the only SIMD in the product is whatever LLVM's own
loop vectorizer finds after the fact. This is a gap in our IR, not a property
of the backends: Cranelift's CLIF has full 128-bit SIMD on both targets, and
it is emitting none of it because we never ask.

## Measured state (nbody, the FP kernel with independent lanes)

| tier | vector arithmetic | evidence |
|---|---|---|
| LLVM | **235 vector ops in IR → 173 NEON instructions** | `fmul.2d` ×65, `fmla.2d` ×42, `fsub.2d` ×22, `faddp.2d` ×22, `fmls.2d` ×21, `dup.2d` ×1, against 198 scalar FP — assembled from the dumped IR with `llc -mcpu=apple-m1` |
| Cranelift | **zero** | full CLIF dump for nbody: 18 scalar FP ops; `f64x2`/`f32x4`/`i32x4`/`i8x16` appear zero times |

Read the IR carefully when checking this: the count of `<2 x double>` *text*
matches is misleading, because most of those are vector loads, stores,
`insertelement` and `shufflevector`, which are memory traffic rather than
arithmetic. Match `= fmul <flags> <N x T>` — the flags (`contract`) sit
between the mnemonic and the type.

## Why AIR cannot express a vector today

Three structural blockers, in order of depth:

1. **There is no type space to name `f64x2` in.** `TypeRef` is an opaque `u32`
   index into the *embedder's* HL type table, and `air` never interprets it —
   the crate's only type predicate is "is this float", carried as a sorted
   `Vec<TypeRef>` and answered by binary search. A lane count has nowhere to
   live. Either AIR mints a synthetic TypeRef space the embedder is taught to
   skip, or lane information rides on the instruction as an immediate.

2. **Every value is pinned to an HL register.** `ValueData { ty, reg }`, where
   `reg` is the register de-SSA assigns the value back to. A 128-bit value has
   no HL register to be assigned back to. The pass framework states this as a
   hard invariant, and `privatize` exists to maintain it.

3. **Every instruction must round-trip to scalar HL bytecode.** `serialize.rs`
   emits an HL opcode array that the interpreter and the LLVM tier both read.
   `Fma` is the existing precedent for an IR-only instruction and it pays this
   tax explicitly: with no HL fused-multiply-add opcode, it serializes back to
   `Mul tmp, a, b` + `Add dst, tmp, c` through an appended temporary. A vector
   instruction would have to scalarize the same way (emit N scalar ops), or
   the serialize path would need permission to fail for vector functions.

Supporting gap: `analysis.rs` has `CfgInfo`, dominators, a `LoopForest` and an
`AliasClass` lattice — but **no loop-carried dependence test and no
stride/affine index analysis**, which is the analysis a vectorizer is mostly
made of.

## How the sibling projects do it

Both vectorize in their own IR and lower vectors themselves. Neither relies on
a backend autovectorizer.

**zyntax** — `HirType::Vector(elem, lanes)` plus 9 vector instructions; three
passes (`auto_vectorize.rs`, `loop_vectorize.rs`, `reduction_vectorize.rs`,
~4.7k lines together), a target-width model (`target_vector.rs`), and FMA
contraction (`fma_contract.rs`). Lowering is implemented independently for
Cranelift CLIF, LLVM via inkwell, wasm v128, and their bytecode interpreter,
with IR-verification tests asserting on emitted LLVM IR and WAT text. Known
gap on their side: no memory dependence or alias analysis, acknowledged as a
TODO, and no runtime overlap guard.

**rayzor** — `IrType::Vector { element, count }` plus 13 `Vector*` opcodes
(`VectorLoad/Store/BinOp/Splat/Extract/Insert/Reduce/UnaryOp/MinMax/Dot/
Shuffle/Convert/Narrow`), lowered to CLIF (`F32X4`/`I32X4`/`I8X16`, `splat`,
`extractlane`, `fma`, `iadd_pairwise`, `swizzle`), LLVM, wasm and C. Two of
their opcodes are explicit cross-backend *contracts* — `VectorShuffle`
documents the intersection of `pshufb` / `tbl1` / `i8x16.swizzle` semantics so
one opcode lowers to one instruction everywhere, and `VectorDot` names its
signedness variants. They carry lane type on the instruction rather than
inferring it, because register-type inference is lost across inlining. Their
`LoopVectorizationPass` (2056 lines) is O3-only, needs a constant trip count,
and does no dependence analysis; in practice most of their SIMD comes from a
hand-written `@:coreType` SIMD API that lowers straight to those opcodes.

The lesson from both: the **opcode set and its lowering contracts** are the
durable part; the automatic pass is the thin, fragile part that arrives later.

## The path for AIR

Ordered so that each step is useful on its own:

1. **Vector values in the IR.** Pick the type-space answer (synthetic TypeRef
   vs. lane immediates on instructions) and the register answer (most likely:
   vector values are exempt from `ValueData::reg`, with a validity rule that
   they never survive to `serialize`). This is the decision that gates
   everything else.
2. **A small vector opcode set with lowering contracts**, in rayzor's style:
   elementwise binop, splat, load/store, extract/insert, and a reduction.
   Each one documented by the *intersection* of what aarch64 NEON and x86-64
   SSE2/AVX2 can do in one instruction.
3. **Cranelift lowering first.** It is the tier with zero SIMD today, so it
   gains most, and CLIF's polymorphic `fadd`/`fmul`/`fma` over lane types
   makes the lowering short.
4. **A source-level SIMD API** (the rayzor/zyntax pattern) so hand-written
   kernels can reach the opcodes without waiting for the automatic pass.
5. **Dependence analysis, then the automatic pass.** Stride/affine index
   analysis plus a loop-carried dependence test; without those a loop
   vectorizer is unsound, and both sibling projects list exactly this as their
   outstanding hole.

Reduction-shaped loops (nbody's accumulation) need step 5's dependence work
plus a reassociation decision, since fp reduction reassociation changes
results — the same class of decision the FMA policy already documents.
