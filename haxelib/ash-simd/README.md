# ash-simd

128-bit vector primitives for HashLink programs.

- `ash.simd.Vec`: one static per primitive, operating memory to memory on
  16-byte slots of `hl.Bytes` (`f32x4Add(dst, di, a, ai, b, bi)`, ...).
  Types: f32x4, f64x2, i32x4, i16x8, i8x16, u8x16.
- `ash.simd.Float32x4`, `ash.simd.Int32x4`: value types over their own
  16-byte `hl.Bytes`, with operators.

On stock HashLink the primitives are `simd.hdll`, built from
`crates/ash_hdll_simd` in the ash repository (`cargo build --release -p
ash_hdll_simd`, then rename the cdylib to `simd.hdll`), and every value-type
operator allocates its 16-byte result. On ash they are part of the runtime:
nothing is shipped beside the program. The load, store, add, multiply,
greater than and select value helpers are Haxe-inline so the compiled tiers
can keep their local vector chains in registers even inside a large caller.
A result stored in an object field still allocates; use `Vec` with
reusable slots when a hot loop must
update shared vectors without allocating.

Build with `-lib ash-simd`, or `-cp` pointing at this directory. The API and
the lane semantics are documented in `docs/simd.md` of the ash repository.

For a scalar, value-vector, and reusable-slot comparison, compile
`crates/ash/test/tests/BenchSimdVsScalar.hx` from the ash repository with
`-cp haxelib/ash-simd`. Run one mode per process to measure its elapsed time
and GC bytes separately.
