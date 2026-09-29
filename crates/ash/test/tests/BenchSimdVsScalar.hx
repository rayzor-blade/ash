// Compare ash-simd's value API with four scalar Single lanes. Run one mode per
// process so the GC byte delta belongs to that mode. The local cases show
// whether temporary vectors disappear; the field cases force a vector result
// to survive in an object. The dot cases include loads from the same buffers.
//
// haxe -cp crates/ash/test/tests -cp haxelib/ash-simd -main BenchSimdVsScalar -hl /tmp/bench_simd_vs_scalar.hl
// ash --mode jit --jit-tier llvm /tmp/bench_simd_vs_scalar.hl value-local 5000000
// ASH_GC_TLAB=0 makes gc_bytes exact, but changes timing; use it only to
// check allocations. With TLABs enabled the byte counter charges regions.
// Modes: scalar-local, value-local, scalar-field, value-field,
//        scalar-dot, value-dot, slots-dot.

import ash.simd.Float32x4;
import ash.simd.Vec;

class ScalarLanes {
	public var x:Single;
	public var y:Single;
	public var z:Single;
	public var w:Single;

	public function new() {
		x = 1;
		y = 2;
		z = 3;
		w = 4;
	}

	public function sum():Float
		return x + y + z + w;
}

class VectorLanes {
	public var v:Float32x4;

	public function new()
		v = Float32x4.make(1, 2, 3, 4);

	public function sum():Float
		return v.sum();
}

class BenchSimdVsScalar {
	static inline var DOT_N = 1 << 18;

	static function scalarLocal(n:Int):Float {
		var x:Single = 1, y:Single = 2, z:Single = 3, w:Single = 4;
		var dx:Single = 0.001, dy:Single = 0.002, dz:Single = 0.003, dw:Single = 0.004;
		var i = 0;
		while (i < n) {
			x += dx;
			y += dy;
			z += dz;
			w += dw;
			i++;
		}
		return x + y + z + w;
	}

	static function valueLocal(n:Int):Float {
		var v = Float32x4.make(1, 2, 3, 4);
		var d = Float32x4.make(0.001, 0.002, 0.003, 0.004);
		var i = 0;
		while (i < n) {
			v = v + d;
			i++;
		}
		return v.sum();
	}

	static function scalarField(p:ScalarLanes, n:Int):Float {
		var dx:Single = 0.001, dy:Single = 0.002, dz:Single = 0.003, dw:Single = 0.004;
		var i = 0;
		while (i < n) {
			p.x += dx;
			p.y += dy;
			p.z += dz;
			p.w += dw;
			i++;
		}
		return p.sum();
	}

	static function valueField(p:VectorLanes, d:Float32x4, n:Int):Float {
		var i = 0;
		while (i < n) {
			p.v = p.v + d;
			i++;
		}
		return p.sum();
	}

	static function scalarDot(a:hl.Bytes, b:hl.Bytes, rounds:Int):Float {
		var total:Float = 0;
		for (r in 0...rounds) {
			b.setF32(0, r);
			var x:Single = 0, y:Single = 0, z:Single = 0, w:Single = 0;
			var i = 0;
			while (i < DOT_N) {
				var off = i << 2;
				x += a.getF32(off) * b.getF32(off);
				y += a.getF32(off + 4) * b.getF32(off + 4);
				z += a.getF32(off + 8) * b.getF32(off + 8);
				w += a.getF32(off + 12) * b.getF32(off + 12);
				i += 4;
			}
			total += x + y + z + w;
		}
		return total;
	}

	static function valueDot(a:hl.Bytes, b:hl.Bytes, rounds:Int):Float {
		var total:Float = 0;
		for (r in 0...rounds) {
			b.setF32(0, r);
			var acc = Float32x4.splat(0);
			var i = 0;
			while (i < DOT_N) {
				acc = acc + Float32x4.load(a, i << 2) * Float32x4.load(b, i << 2);
				i += 4;
			}
			total += acc.sum();
		}
		return total;
	}

	static function slotsDot(a:hl.Bytes, b:hl.Bytes, rounds:Int):Float {
		var aa = new hl.Bytes(16), bb = new hl.Bytes(16);
		var acc = new hl.Bytes(16), product = new hl.Bytes(16);
		var total:Float = 0;
		for (r in 0...rounds) {
			b.setF32(0, r);
			Vec.f32x4Splat(acc, 0, 0);
			var i = 0;
			while (i < DOT_N) {
				Vec.v128Copy(aa, 0, a, i << 2);
				Vec.v128Copy(bb, 0, b, i << 2);
				Vec.f32x4Mul(product, 0, aa, 0, bb, 0);
				Vec.f32x4Add(acc, 0, acc, 0, product, 0);
				i += 4;
			}
			total += Vec.f32x4Sum(acc, 0);
		}
		return total;
	}

	static function main() {
		var args = Sys.args();
		if (args.length != 2)
			throw "usage: BenchSimdVsScalar <mode> <iterations/rounds>";
		var mode = args[0];
		var n = Std.parseInt(args[1]);
		if (n == null || n <= 0)
			throw "iterations/rounds must be positive";

		var sp = new ScalarLanes();
		var vp = new VectorLanes();
		var delta = Float32x4.make(0.001, 0.002, 0.003, 0.004);
		var a = new hl.Bytes(DOT_N * 4);
		var b = new hl.Bytes(DOT_N * 4);
		for (i in 0...DOT_N) {
			a.setF32(i << 2, ((i * 7) % 13) * 0.125);
			b.setF32(i << 2, ((i * 5) % 11) * 0.25);
		}

		// First call compiles the measured function in JIT mode.
		switch (mode) {
			case "scalar-local": scalarLocal(1000);
			case "value-local": valueLocal(1000);
			case "scalar-field": scalarField(new ScalarLanes(), 1000);
			case "value-field": valueField(new VectorLanes(), delta, 1000);
			case "scalar-dot": scalarDot(a, b, 1);
			case "value-dot": valueDot(a, b, 1);
			case "slots-dot": slotsDot(a, b, 1);
			default: throw "unknown mode " + mode;
		}

		var before = hl.Gc.stats().totalAllocated;
		var start = haxe.Timer.stamp();
		var result:Float = switch (mode) {
			case "scalar-local": scalarLocal(n);
			case "value-local": valueLocal(n);
			case "scalar-field": scalarField(sp, n);
			case "value-field": valueField(vp, delta, n);
			case "scalar-dot": scalarDot(a, b, n);
			case "value-dot": valueDot(a, b, n);
			case "slots-dot": slotsDot(a, b, n);
			default: 0;
		};
		var seconds = haxe.Timer.stamp() - start;
		var bytes = hl.Gc.stats().totalAllocated - before;
		Sys.println(mode + " n=" + n + " seconds=" + seconds + " gc_bytes=" + Std.int(bytes) + " result=" + result);
	}
}
