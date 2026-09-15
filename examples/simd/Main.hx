// A dot product three ways: scalar, the Float32x4 value type with its
// operators, and the ash.simd.Vec slot form a hot loop would use.
//
//   haxe -cp haxelib/ash-simd -main Main -hl examples/simd/dot.hl --cwd examples/simd
//   ash examples/simd/dot.hl          (or: hl examples/simd/dot.hl with simd.hdll)

import ash.simd.Float32x4;
import ash.simd.Vec;

class Main {
	static inline var N = 1 << 16;

	static function scalar(a:hl.Bytes, b:hl.Bytes):Single {
		var acc:Single = 0;
		for (i in 0...N)
			acc += a.getF32(i << 2) * b.getF32(i << 2);
		return acc;
	}

	// Each iteration allocates its products and its running sum; readable,
	// and on ash the compiled tiers keep both in registers.
	static function values(a:hl.Bytes, b:hl.Bytes):Single {
		var acc = Float32x4.splat(0);
		var i = 0;
		while (i < N) {
			acc = acc + Float32x4.load(a, i << 2) * Float32x4.load(b, i << 2);
			i += 4;
		}
		return acc.sum();
	}

	// Two scratch slots and no allocation: the products land in `prod`, the
	// running sum accumulates in place.
	static function slots(a:hl.Bytes, b:hl.Bytes):Single {
		var acc = new hl.Bytes(16);
		var prod = new hl.Bytes(16);
		Vec.f32x4Splat(acc, 0, 0);
		var i = 0;
		while (i < N) {
			Vec.f32x4Mul(prod, 0, a, i << 2, b, i << 2);
			Vec.f32x4Add(acc, 0, acc, 0, prod, 0);
			i += 4;
		}
		return Vec.f32x4Sum(acc, 0);
	}

	static function main() {
		var a = new hl.Bytes(N * 4);
		var b = new hl.Bytes(N * 4);
		for (i in 0...N) {
			a.setF32(i << 2, (i % 17) * 0.25);
			b.setF32(i << 2, (i % 13) * 0.5);
		}
		Sys.println("scalar " + scalar(a, b));
		Sys.println("values " + values(a, b));
		Sys.println("slots  " + slots(a, b));

		var v = Float32x4.make(1, 2, 3, 4);
		var w = Float32x4.splat(0.5);
		Sys.println("(v + w) * v - w = " + ((v + w) * v - w).toString());
		Sys.println("v / w = " + (v / w).toString() + ", -v = " + (-v).toString());
		Sys.println("v > 2.5: " + v.gt(Float32x4.splat(2.5)).toString());
		Sys.println("select: " + Float32x4.select(v.gt(Float32x4.splat(2.5)), v, w).toString());
	}
}
