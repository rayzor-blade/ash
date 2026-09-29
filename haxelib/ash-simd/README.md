# ash-simd

128-bit vector primitives for HashLink programs.

- `ash.simd.Vec`: one static per primitive, operating memory to memory on
  16-byte slots of `hl.Bytes` (`f32x4Add(dst, di, a, ai, b, bi)`, ...).
  Types: f32x4, f64x2, i32x4, i16x8, i8x16, u8x16.
- `ash.simd.Float32x4`, `ash.simd.Int32x4`: value types over their own
  16-byte `hl.Bytes`, with operators.

The release's `ash-simd-<version>.zip` contains native HDLLs for every
supported platform. Install it with `haxelib install ash-simd-<version>.zip`.
When a project compiles with `-lib ash-simd`, `extraParams.hxml` copies the
matching `simd.hdll` beside the generated `.hl` file for stock HashLink.
Ash uses its built-in primitives and ignores the copied HDLL. Every value-type
operator allocates its 16-byte result on stock HashLink. On ash the compiled
tiers keep local vector chains in registers. The load, store, add, multiply,
greater than and select value helpers are Haxe-inline so the compiled tiers
can keep their local vector chains in registers even inside a large caller.
A result stored in an object field still allocates; use `Vec` with reusable
slots when a hot loop must update shared vectors without allocating.

Stock HashLink must be able to find the output directory on its native
library search path. On Linux, use `LD_LIBRARY_PATH=build hl build/main.hl`
for output `build/main.hl`, or install `simd.hdll` in HashLink's library
directory.

Build with `-lib ash-simd`, or `-cp` pointing at this directory. A source
checkout used with `-cp` does not run `extraParams.hxml`; copy `simd.hdll`
manually for stock HashLink. The API and lane semantics are documented in
`docs/simd.md` of the ash repository.

For a scalar, value-vector, and reusable-slot comparison, compile
`crates/ash/test/tests/BenchSimdVsScalar.hx` from the ash repository with
`-cp haxelib/ash-simd`. Run one mode per process to measure its elapsed time
and GC bytes separately.
