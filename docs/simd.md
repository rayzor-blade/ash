# ash-simd

128-bit vector operations for Haxe. Source: [haxelib/ash-simd](../haxelib/ash-simd).
Build with `-lib ash-simd` or `-cp haxelib/ash-simd`.

The library is one set of primitives with two runtimes behind it. On stock
HashLink the primitives are `simd.hdll`; on ash they are part of the
runtime and the compiled tiers emit them as vector instructions. Both are
built from the same Rust source, and CI runs the same fixture on `hl` and on
every ash engine, so the lane semantics below hold on both.

## Representation

A vector is 16 bytes of an `hl.Bytes`, addressed by `(bytes, byteOffset)`.
Lanes are read and written with the ordinary `hl.Bytes` accessors
(`getF32`/`setF32`, `getI32`/`setI32`, …). Loads and stores are unaligned;
a destination may alias a source. Nothing is bounds-checked, as with
`hl.Bytes` itself.

Types: `f32x4`, `f64x2`, `i32x4`, `i16x8`, `i8x16`, `u8x16`; `v128` for the
bitwise operations, which do not care about lanes.

## Value types

`ash.simd.Float32x4` and `ash.simd.Int32x4` are abstracts over a 16-byte
`hl.Bytes` of their own. Each operation returns a fresh vector.

```haxe
import ash.simd.Float32x4;
import ash.simd.Int32x4;

var v = Float32x4.make(1, 2, 3, 4);
var w = Float32x4.splat(0.5);
var r = (v + w) * v - w;          // + - * / and unary -
r.x; r.get(3);                     // lanes
var m = v.gt(Float32x4.splat(2.5));            // Int32x4 mask: all ones where true
var s = Float32x4.select(m, v, w);              // lanes of v where m is set, else w
v.sum(); v.minLane(); v.maxLane();
v.min(w); v.max(w); v.abs(); v.sqrt(); v.fma(w, r);   // v * w + r, one rounding
v.toInt32x4();                                  // saturating, NaN → 0
v.store(bytes, 32); Float32x4.load(bytes, 32);
```

`Int32x4` has `+ - *` (wrapping), `& | ^ ~`, `<<` and `>>` (arithmetic, count
masked to 0..31), `min max abs`, the six comparisons, `select`, `sum minLane
maxLane`, `toFloat32x4`, and the same `make splat load store get set x y z w`.
It is also the mask type every four-lane comparison returns.

**On stock HashLink** every operator allocates its 16-byte result; a hot loop
should use the slot form below. **On ash** a value that does not leave the
function costs nothing: the optimiser removes the allocation and keeps the
vector in a register, including across loop iterations. A value stored into a
field or array, passed to a non-inlined function, or returned is written to
memory at that point. `ASH_SROA_WHY=1` prints the reason for any vector that
stayed in memory.

## Slot form: `ash.simd.Vec`

One static per primitive, memory to memory. This is the HDLL surface and
the intrinsic surface; the value types are written on top of it.

```haxe
import ash.simd.Vec;

var acc = new hl.Bytes(16), prod = new hl.Bytes(16);
Vec.f32x4Splat(acc, 0, 0);
var i = 0;
while (i < n) {
    Vec.f32x4Mul(prod, 0, a, i << 2, b, i << 2);
    Vec.f32x4Add(acc, 0, acc, 0, prod, 0);
    i += 4;
}
var dot = Vec.f32x4Sum(acc, 0);
```

Argument order is destination first: `f32x4Add(dst, di, a, ai, b, bi)`.

| Group | Primitives (per type prefix) |
|---|---|
| float arithmetic (`f32x4`, `f64x2`) | `Add Sub Mul Div Min Max Abs Neg Sqrt Fma Splat` |
| integer arithmetic (`i32x4`, `i16x8`, `i8x16`, `u8x16`) | `Add Sub Mul Min Max Splat Shl Shr`; `Abs Neg` on the signed types |
| comparisons, every type | `Eq Ne Lt Le Gt Ge` → lane mask |
| reductions, every type | `Sum MinLane MaxLane` → scalar |
| arrays, every type | `LoadArray(dst, di, arr, index)`, `StoreArray(arr, index, src, si)` over `hl.NativeArray<T>` |
| `v128` | `And Or Xor Not Select(dst, di, mask, mi, a, ai, b, bi) Copy(dst, di, src, si)` |
| conversions | `f32x4ToI32x4`, `i32x4ToF32x4` |

## Lane semantics

Fixed by the fixture, identical on `hl` and every ash engine:

- Integer `Add Sub Mul Neg Abs` wrap. `abs(MIN) == MIN`.
- Shift counts are masked to the lane width. `Shr` is arithmetic on signed
  types and logical on `u8x16`.
- Float `Min`/`Max` are IEEE 754-2019 minimum/maximum: a NaN operand gives
  NaN, and -0 orders below +0.
- Comparisons write all ones or all zeros in the operand's lane width;
  `Ne` is true for NaN, the ordered comparisons are false.
- `Select` is bitwise: `(mask & a) | (~mask & b)`.
- Reductions fold from lane 0 in order. Integer sums wrap in the lane width
  before widening to `Int` (an `i8x16` of sixteen 100s sums to 64).
- `f32x4ToI32x4` saturates; NaN becomes 0.
- `Fma` rounds once. It is never formed from a separate `Mul` and `Add`.
- `Splat` on a narrow integer type keeps the low bits of the `Int`.

## Stock HashLink

Build the HDLL from the ash repository and place it where `hl` finds HDLLs
(beside `hl`, or in its library directory):

```sh
cargo build --release -p ash_hdll_simd
cp target/release/libash_hdll_simd.so simd.hdll      # .dylib / .dll on macOS / Windows
```

On x86-64 the HDLL compiles each primitive twice and picks the AVX2+FMA
version at first call when the CPU has it; on arm64 NEON is the baseline.

## Reference program

`crates/ash/test/tests/TestSimd.hx` exercises every primitive against a
scalar reference — NaN, signed zero, saturation, wrapping, unaligned slots,
aliasing — and `examples/simd/Main.hx` is a dot product written three ways.
