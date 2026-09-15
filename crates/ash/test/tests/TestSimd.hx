// Parity fixture for ash-simd: every primitive against a scalar reference,
// with NaN, signed zero, saturation, wrapping, unaligned slots and aliasing.
// Prints every result so the engines can be diffed, and counts the lanes
// that disagree with the reference so a run without an oracle still judges.
// Built with `-cp haxelib/ash-simd`; stock hl needs simd.hdll on its path.

import ash.simd.Vec;
import ash.simd.Float32x4;
import ash.simd.Int32x4;

class TestSimd {
	static var fails = 0;

	static function check(name:String, ok:Bool) {
		if (!ok) {
			fails++;
			Sys.println("FAIL " + name);
		}
	}

	static function fmt(x:Float):String {
		if (Math.isNaN(x))
			return "NaN";
		if (x == 0)
			return 1 / x < 0 ? "-0" : "0";
		return Std.string(x);
	}

	static function showF32(tag:String, b:hl.Bytes, off:Int) {
		var s = [];
		for (i in 0...4)
			s.push(fmt(b.getF32(off + i * 4)));
		Sys.println(tag + " = [" + s.join(", ") + "]");
	}

	static function showF64(tag:String, b:hl.Bytes, off:Int) {
		Sys.println(tag + " = [" + fmt(b.getF64(off)) + ", " + fmt(b.getF64(off + 8)) + "]");
	}

	static function showI32(tag:String, b:hl.Bytes, off:Int) {
		var s = [];
		for (i in 0...4)
			s.push(Std.string(b.getI32(off + i * 4)));
		Sys.println(tag + " = [" + s.join(", ") + "]");
	}

	static function showI16(tag:String, b:hl.Bytes, off:Int) {
		var s = [];
		for (i in 0...8)
			s.push(Std.string((b.getUI16(off + i * 2) << 16) >> 16));
		Sys.println(tag + " = [" + s.join(", ") + "]");
	}

	static function showI8(tag:String, b:hl.Bytes, off:Int) {
		var s = [];
		for (i in 0...16)
			s.push(Std.string((b.getUI8(off + i) << 24) >> 24));
		Sys.println(tag + " = [" + s.join(", ") + "]");
	}

	static function showU8(tag:String, b:hl.Bytes, off:Int) {
		var s = [];
		for (i in 0...16)
			s.push(Std.string(b.getUI8(off + i)));
		Sys.println(tag + " = [" + s.join(", ") + "]");
	}

	// Lane-by-lane bit equality between two slots.
	static function sameBits(a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Bool {
		for (i in 0...4)
			if (a.getI32(ai + i * 4) != b.getI32(bi + i * 4))
				return false;
		return true;
	}

	static function f32(a:Single, b:Single, c:Single, d:Single):hl.Bytes {
		var r = new hl.Bytes(16);
		r.setF32(0, a);
		r.setF32(4, b);
		r.setF32(8, c);
		r.setF32(12, d);
		return r;
	}

	static function i32(a:Int, b:Int, c:Int, d:Int):hl.Bytes {
		var r = new hl.Bytes(16);
		r.setI32(0, a);
		r.setI32(4, b);
		r.setI32(8, c);
		r.setI32(12, d);
		return r;
	}

	static function testF32x4() {
		Sys.println("-- f32x4");
		var a = f32(1.5, -2.25, 3, 0.125);
		var b = f32(0.5, 4, -1, 8);
		var r = new hl.Bytes(16);
		var e = new hl.Bytes(16);

		Vec.f32x4Add(r, 0, a, 0, b, 0);
		showF32("add", r, 0);
		for (i in 0...4) {
			var x:Single = a.getF32(i * 4) + b.getF32(i * 4);
			e.setF32(i * 4, x);
		}
		check("f32x4 add", sameBits(r, 0, e, 0));

		Vec.f32x4Sub(r, 0, a, 0, b, 0);
		showF32("sub", r, 0);
		for (i in 0...4) {
			var x:Single = a.getF32(i * 4) - b.getF32(i * 4);
			e.setF32(i * 4, x);
		}
		check("f32x4 sub", sameBits(r, 0, e, 0));

		Vec.f32x4Mul(r, 0, a, 0, b, 0);
		showF32("mul", r, 0);
		for (i in 0...4) {
			var x:Single = a.getF32(i * 4) * b.getF32(i * 4);
			e.setF32(i * 4, x);
		}
		check("f32x4 mul", sameBits(r, 0, e, 0));

		Vec.f32x4Div(r, 0, a, 0, b, 0);
		showF32("div", r, 0);
		for (i in 0...4) {
			var x:Single = a.getF32(i * 4) / b.getF32(i * 4);
			e.setF32(i * 4, x);
		}
		check("f32x4 div", sameBits(r, 0, e, 0));

		Vec.f32x4Abs(r, 0, a, 0);
		showF32("abs", r, 0);
		Vec.f32x4Neg(r, 0, a, 0);
		showF32("neg", r, 0);
		check("f32x4 neg", r.getF32(0) == -1.5 && r.getF32(4) == 2.25);

		var sq = f32(4, 2, 0.25, -1);
		Vec.f32x4Sqrt(r, 0, sq, 0);
		showF32("sqrt", r, 0);
		check("f32x4 sqrt", r.getF32(0) == 2 && r.getF32(8) == 0.5 && Math.isNaN(r.getF32(12)));

		// Exact products and sums, so fused and unfused agree.
		var c = f32(1, 2, 3, 4);
		Vec.f32x4Fma(r, 0, a, 0, b, 0, c, 0);
		showF32("fma", r, 0);
		check("f32x4 fma", r.getF32(0) == 1.75 && r.getF32(4) == -7 && r.getF32(8) == 0 && r.getF32(12) == 5);

		Vec.f32x4Splat(r, 0, 2.5);
		showF32("splat", r, 0);
		check("f32x4 splat", r.getF32(0) == 2.5 && r.getF32(12) == 2.5);

		Sys.println("sum = " + fmt(Vec.f32x4Sum(a, 0)));
		Sys.println("minLane = " + fmt(Vec.f32x4MinLane(a, 0)));
		Sys.println("maxLane = " + fmt(Vec.f32x4MaxLane(a, 0)));
		check("f32x4 sum", Vec.f32x4Sum(a, 0) == 2.375);
		check("f32x4 minLane", Vec.f32x4MinLane(a, 0) == -2.25);
		check("f32x4 maxLane", Vec.f32x4MaxLane(a, 0) == 3);

		// NaN, infinities and signed zero through min/max and the compares.
		// The zero lanes are written as bits: the Haxe compiler pools float
		// constants by value, and -0.0 and 0.0 are the same value to it.
		var n = f32(Math.NaN, Math.POSITIVE_INFINITY, 1, 1);
		n.setI32(8, 0x80000000);
		var m = f32(1, Math.NEGATIVE_INFINITY, 1, Math.NaN);
		m.setI32(8, 0);
		Vec.f32x4Min(r, 0, n, 0, m, 0);
		showF32("min", r, 0);
		check("f32x4 min nan", Math.isNaN(r.getF32(0)) && Math.isNaN(r.getF32(12)));
		check("f32x4 min inf", r.getF32(4) == Math.NEGATIVE_INFINITY);
		check("f32x4 min -0", r.getF32(8) == 0 && 1 / r.getF32(8) < 0);
		Vec.f32x4Max(r, 0, n, 0, m, 0);
		showF32("max", r, 0);
		check("f32x4 max nan", Math.isNaN(r.getF32(0)) && Math.isNaN(r.getF32(12)));
		check("f32x4 max inf", r.getF32(4) == Math.POSITIVE_INFINITY);
		check("f32x4 max +0", r.getF32(8) == 0 && 1 / r.getF32(8) > 0);

		Vec.f32x4Eq(r, 0, n, 0, m, 0);
		showI32("eq", r, 0);
		check("f32x4 eq", r.getI32(0) == 0 && r.getI32(8) == -1 && r.getI32(12) == 0);
		Vec.f32x4Ne(r, 0, n, 0, m, 0);
		showI32("ne", r, 0);
		check("f32x4 ne", r.getI32(0) == -1 && r.getI32(8) == 0 && r.getI32(12) == -1);
		Vec.f32x4Lt(r, 0, n, 0, m, 0);
		showI32("lt", r, 0);
		check("f32x4 lt", r.getI32(0) == 0 && r.getI32(4) == 0 && r.getI32(8) == 0);
		Vec.f32x4Le(r, 0, n, 0, m, 0);
		showI32("le", r, 0);
		check("f32x4 le", r.getI32(8) == -1);
		Vec.f32x4Gt(r, 0, n, 0, m, 0);
		showI32("gt", r, 0);
		check("f32x4 gt", r.getI32(4) == -1 && r.getI32(8) == 0);
		Vec.f32x4Ge(r, 0, n, 0, m, 0);
		showI32("ge", r, 0);
		check("f32x4 ge", r.getI32(4) == -1 && r.getI32(8) == -1 && r.getI32(12) == 0);

		// Unaligned slots: operands and result at odd byte offsets.
		var buf = new hl.Bytes(64);
		buf.blit(1, a, 0, 16);
		buf.blit(19, b, 0, 16);
		Vec.f32x4Add(buf, 37, buf, 1, buf, 19);
		showF32("add@37", buf, 37);
		Vec.f32x4Add(e, 0, a, 0, b, 0);
		check("f32x4 unaligned", sameBits(buf, 37, e, 0));

		// Aliasing: in place, and a destination overlapping a source by three
		// lanes.
		var p = f32(1, 2, 3, 4);
		Vec.f32x4Mul(p, 0, p, 0, p, 0);
		showF32("inplace", p, 0);
		check("f32x4 in place", p.getF32(0) == 1 && p.getF32(12) == 16);
		var q = new hl.Bytes(32);
		q.blit(0, f32(1, 2, 3, 4), 0, 16);
		q.blit(16, f32(5, 6, 7, 8), 0, 16);
		Vec.f32x4Add(q, 4, q, 0, q, 0);
		showF32("overlap", q, 4);
		check("f32x4 overlap", q.getF32(4) == 2 && q.getF32(8) == 4 && q.getF32(12) == 6 && q.getF32(16) == 8);

		// NativeArray traffic.
		var arr = new hl.NativeArray<Single>(8);
		for (i in 0...8)
			arr[i] = i * 0.5;
		Vec.f32x4LoadArray(r, 0, arr, 2);
		showF32("loadArray", r, 0);
		check("f32x4 loadArray", r.getF32(0) == 1 && r.getF32(12) == 2.5);
		Vec.f32x4StoreArray(arr, 4, a, 0);
		Sys.println("storeArray = [" + [for (i in 0...8) fmt(arr[i])].join(", ") + "]");
		check("f32x4 storeArray", arr[4] == 1.5 && arr[7] == 0.125 && arr[3] == 1.5);
	}

	static function testF64x2() {
		Sys.println("-- f64x2");
		var a = new hl.Bytes(16);
		a.setF64(0, 1.25);
		a.setF64(8, -3);
		var b = new hl.Bytes(16);
		b.setF64(0, 0.1);
		b.setF64(8, Math.NaN);
		var r = new hl.Bytes(16);
		Vec.f64x2Add(r, 0, a, 0, b, 0);
		showF64("add", r, 0);
		check("f64x2 add", r.getF64(0) == 1.25 + 0.1 && Math.isNaN(r.getF64(8)));
		Vec.f64x2Sub(r, 0, a, 0, b, 0);
		showF64("sub", r, 0);
		Vec.f64x2Mul(r, 0, a, 0, b, 0);
		showF64("mul", r, 0);
		check("f64x2 mul", r.getF64(0) == 1.25 * 0.1);
		Vec.f64x2Div(r, 0, a, 0, b, 0);
		showF64("div", r, 0);
		Vec.f64x2Min(r, 0, a, 0, b, 0);
		showF64("min", r, 0);
		check("f64x2 min", r.getF64(0) == 0.1 && Math.isNaN(r.getF64(8)));
		Vec.f64x2Max(r, 0, a, 0, b, 0);
		showF64("max", r, 0);
		check("f64x2 max", r.getF64(0) == 1.25);
		Vec.f64x2Abs(r, 0, a, 0);
		showF64("abs", r, 0);
		Vec.f64x2Neg(r, 0, a, 0);
		showF64("neg", r, 0);
		Vec.f64x2Sqrt(r, 0, a, 0);
		showF64("sqrt", r, 0);
		check("f64x2 sqrt", r.getF64(0) == Math.sqrt(1.25) && Math.isNaN(r.getF64(8)));
		var c = new hl.Bytes(16);
		c.setF64(0, 2);
		c.setF64(8, 2);
		Vec.f64x2Fma(r, 0, a, 0, a, 0, c, 0);
		showF64("fma", r, 0);
		check("f64x2 fma", r.getF64(0) == 3.5625 && r.getF64(8) == 11);
		Vec.f64x2Splat(r, 0, -0.5);
		showF64("splat", r, 0);
		Vec.f64x2Lt(r, 0, a, 0, b, 0);
		Sys.println("lt = [" + r.getI32(0) + ", " + r.getI32(4) + ", " + r.getI32(8) + ", " + r.getI32(12) + "]");
		check("f64x2 lt", r.getI32(0) == 0 && r.getI32(4) == 0 && r.getI32(8) == 0);
		Vec.f64x2Ge(r, 0, a, 0, b, 0);
		check("f64x2 ge", r.getI32(0) == -1 && r.getI32(4) == -1 && r.getI32(8) == 0);
		Sys.println("sum = " + fmt(Vec.f64x2Sum(a, 0)));
		Sys.println("minLane = " + fmt(Vec.f64x2MinLane(a, 0)));
		Sys.println("maxLane = " + fmt(Vec.f64x2MaxLane(a, 0)));
		check("f64x2 sum", Vec.f64x2Sum(a, 0) == -1.75);
		var arr = new hl.NativeArray<Float>(4);
		for (i in 0...4)
			arr[i] = i + 0.25;
		Vec.f64x2LoadArray(r, 0, arr, 1);
		showF64("loadArray", r, 0);
		check("f64x2 loadArray", r.getF64(0) == 1.25 && r.getF64(8) == 2.25);
		Vec.f64x2StoreArray(arr, 2, a, 0);
		check("f64x2 storeArray", arr[2] == 1.25 && arr[3] == -3);
	}

	static function testI32x4() {
		Sys.println("-- i32x4");
		var a = i32(0x7fffffff, -0x80000000, 7, -7);
		var b = i32(1, -1, 3, 3);
		var r = new hl.Bytes(16);
		Vec.i32x4Add(r, 0, a, 0, b, 0);
		showI32("add", r, 0);
		check("i32x4 add wraps", r.getI32(0) == -0x80000000 && r.getI32(4) == 0x7fffffff && r.getI32(8) == 10);
		Vec.i32x4Sub(r, 0, a, 0, b, 0);
		showI32("sub", r, 0);
		check("i32x4 sub wraps", r.getI32(0) == 0x7ffffffe && r.getI32(4) == -0x7fffffff && r.getI32(12) == -10);
		Vec.i32x4Mul(r, 0, a, 0, a, 0);
		showI32("mul", r, 0);
		check("i32x4 mul wraps", r.getI32(0) == 1 && r.getI32(4) == 0 && r.getI32(8) == 49);
		Vec.i32x4Min(r, 0, a, 0, b, 0);
		showI32("min", r, 0);
		check("i32x4 min", r.getI32(0) == 1 && r.getI32(4) == -0x80000000 && r.getI32(12) == -7);
		Vec.i32x4Max(r, 0, a, 0, b, 0);
		showI32("max", r, 0);
		check("i32x4 max", r.getI32(0) == 0x7fffffff && r.getI32(4) == -1 && r.getI32(12) == 3);
		Vec.i32x4Abs(r, 0, a, 0);
		showI32("abs", r, 0);
		check("i32x4 abs", r.getI32(4) == -0x80000000 && r.getI32(12) == 7);
		Vec.i32x4Neg(r, 0, a, 0);
		showI32("neg", r, 0);
		check("i32x4 neg", r.getI32(0) == -0x7fffffff && r.getI32(4) == -0x80000000 && r.getI32(8) == -7);
		Vec.i32x4Splat(r, 0, -9);
		showI32("splat", r, 0);
		check("i32x4 splat", r.getI32(0) == -9 && r.getI32(12) == -9);
		Vec.i32x4Shl(r, 0, a, 0, 33);
		showI32("shl33", r, 0);
		check("i32x4 shl masks", r.getI32(8) == 14 && r.getI32(0) == -2);
		Vec.i32x4Shr(r, 0, a, 0, 1);
		showI32("shr1", r, 0);
		check("i32x4 shr arithmetic", r.getI32(4) == -0x40000000 && r.getI32(12) == -4);
		Vec.i32x4Eq(r, 0, a, 0, b, 0);
		showI32("eq", r, 0);
		Vec.i32x4Ne(r, 0, a, 0, b, 0);
		showI32("ne", r, 0);
		Vec.i32x4Lt(r, 0, a, 0, b, 0);
		showI32("lt", r, 0);
		check("i32x4 lt signed", r.getI32(0) == 0 && r.getI32(4) == -1 && r.getI32(12) == -1);
		Vec.i32x4Le(r, 0, a, 0, b, 0);
		showI32("le", r, 0);
		Vec.i32x4Gt(r, 0, a, 0, b, 0);
		showI32("gt", r, 0);
		Vec.i32x4Ge(r, 0, a, 0, b, 0);
		showI32("ge", r, 0);
		check("i32x4 ge", r.getI32(0) == -1 && r.getI32(4) == 0);
		Sys.println("sum = " + Vec.i32x4Sum(a, 0));
		Sys.println("minLane = " + Vec.i32x4MinLane(a, 0));
		Sys.println("maxLane = " + Vec.i32x4MaxLane(a, 0));
		check("i32x4 sum wraps", Vec.i32x4Sum(a, 0) == -1);
		check("i32x4 minLane", Vec.i32x4MinLane(a, 0) == -0x80000000);
		check("i32x4 maxLane", Vec.i32x4MaxLane(a, 0) == 0x7fffffff);

		var f = f32(3e9, -3e9, 2.7, -2.7);
		Vec.f32x4ToI32x4(r, 0, f, 0);
		showI32("f32->i32", r, 0);
		check("f32x4 to i32x4 saturates", r.getI32(0) == 0x7fffffff && r.getI32(4) == -0x80000000 && r.getI32(8) == 2 && r.getI32(12) == -2);
		var nan = f32(Math.NaN, Math.POSITIVE_INFINITY, Math.NEGATIVE_INFINITY, -0.0);
		Vec.f32x4ToI32x4(r, 0, nan, 0);
		showI32("f32->i32 nan", r, 0);
		check("f32x4 to i32x4 nan", r.getI32(0) == 0 && r.getI32(4) == 0x7fffffff && r.getI32(8) == -0x80000000 && r.getI32(12) == 0);
		var big = i32(16777217, -1, 0, 0x7fffffff);
		Vec.i32x4ToF32x4(r, 0, big, 0);
		showF32("i32->f32", r, 0);
		check("i32x4 to f32x4 rounds", r.getF32(0) == 16777216 && r.getF32(4) == -1 && r.getF32(12) == 2147483648.0);

		var arr = new hl.NativeArray<Int>(8);
		for (i in 0...8)
			arr[i] = i * 3;
		Vec.i32x4LoadArray(r, 0, arr, 3);
		showI32("loadArray", r, 0);
		check("i32x4 loadArray", r.getI32(0) == 9 && r.getI32(12) == 18);
		Vec.i32x4StoreArray(arr, 0, a, 0);
		Sys.println("storeArray = [" + [for (i in 0...8) Std.string(arr[i])].join(", ") + "]");
		check("i32x4 storeArray", arr[0] == 0x7fffffff && arr[3] == -7 && arr[4] == 12);
	}

	static function testI16x8() {
		Sys.println("-- i16x8");
		var a = new hl.Bytes(16);
		var b = new hl.Bytes(16);
		for (i in 0...8) {
			a.setUI16(i * 2, [32767, -32768, 100, -100, 0, 1, -1, 300][i] & 0xffff);
			b.setUI16(i * 2, [1, -1, 3, 3, 0, -1, -1, 300][i] & 0xffff);
		}
		var r = new hl.Bytes(16);
		Vec.i16x8Add(r, 0, a, 0, b, 0);
		showI16("add", r, 0);
		check("i16x8 add wraps", (r.getUI16(0) << 16) >> 16 == -32768 && (r.getUI16(2) << 16) >> 16 == 32767 && r.getUI16(14) == 600);
		Vec.i16x8Sub(r, 0, a, 0, b, 0);
		showI16("sub", r, 0);
		Vec.i16x8Mul(r, 0, a, 0, b, 0);
		showI16("mul", r, 0);
		check("i16x8 mul wraps", (r.getUI16(14) << 16) >> 16 == 90000 - 65536 - 65536 + 65536);
		Vec.i16x8Min(r, 0, a, 0, b, 0);
		showI16("min", r, 0);
		check("i16x8 min signed", (r.getUI16(2) << 16) >> 16 == -32768 && (r.getUI16(12) << 16) >> 16 == -1);
		Vec.i16x8Max(r, 0, a, 0, b, 0);
		showI16("max", r, 0);
		Vec.i16x8Abs(r, 0, a, 0);
		showI16("abs", r, 0);
		check("i16x8 abs", (r.getUI16(2) << 16) >> 16 == -32768 && r.getUI16(6) == 100);
		Vec.i16x8Neg(r, 0, a, 0);
		showI16("neg", r, 0);
		Vec.i16x8Splat(r, 0, 70000);
		showI16("splat", r, 0);
		check("i16x8 splat truncates", r.getUI16(0) == 70000 - 65536);
		Vec.i16x8Shl(r, 0, a, 0, 17);
		showI16("shl17", r, 0);
		check("i16x8 shl masks", r.getUI16(4) == 200);
		Vec.i16x8Shr(r, 0, a, 0, 2);
		showI16("shr2", r, 0);
		check("i16x8 shr arithmetic", (r.getUI16(2) << 16) >> 16 == -8192 && (r.getUI16(6) << 16) >> 16 == -25);
		Vec.i16x8Eq(r, 0, a, 0, b, 0);
		showI16("eq", r, 0);
		Vec.i16x8Ne(r, 0, a, 0, b, 0);
		showI16("ne", r, 0);
		Vec.i16x8Lt(r, 0, a, 0, b, 0);
		showI16("lt", r, 0);
		check("i16x8 lt signed", r.getUI16(2) == 0xffff && r.getUI16(0) == 0 && r.getUI16(10) == 0);
		Vec.i16x8Le(r, 0, a, 0, b, 0);
		showI16("le", r, 0);
		Vec.i16x8Gt(r, 0, a, 0, b, 0);
		showI16("gt", r, 0);
		Vec.i16x8Ge(r, 0, a, 0, b, 0);
		showI16("ge", r, 0);
		Sys.println("sum = " + Vec.i16x8Sum(a, 0));
		Sys.println("minLane = " + Vec.i16x8MinLane(a, 0));
		Sys.println("maxLane = " + Vec.i16x8MaxLane(a, 0));
		check("i16x8 sum wraps", Vec.i16x8Sum(a, 0) == 299);
		check("i16x8 minLane", Vec.i16x8MinLane(a, 0) == -32768);
		check("i16x8 maxLane", Vec.i16x8MaxLane(a, 0) == 32767);
		var arr = new hl.NativeArray<hl.UI16>(12);
		for (i in 0...12)
			arr[i] = i * 1000;
		Vec.i16x8LoadArray(r, 0, arr, 2);
		showI16("loadArray", r, 0);
		check("i16x8 loadArray", r.getUI16(0) == 2000 && r.getUI16(14) == 9000);
		Vec.i16x8StoreArray(arr, 4, a, 0);
		Sys.println("storeArray = [" + [for (i in 0...12) Std.string(arr[i])].join(", ") + "]");
		check("i16x8 storeArray", arr[4] == 32767 && arr[11] == 300 && arr[3] == 3000);
	}

	static function testI8x16() {
		Sys.println("-- i8x16");
		var a = new hl.Bytes(16);
		var b = new hl.Bytes(16);
		for (i in 0...16) {
			a.setUI8(i, (i * 37 - 100) & 0xff);
			b.setUI8(i, (i * 11 + 3) & 0xff);
		}
		a.setUI8(0, 127);
		a.setUI8(1, 0x80);
		b.setUI8(0, 1);
		b.setUI8(1, 0xff);
		var r = new hl.Bytes(16);
		Vec.i8x16Add(r, 0, a, 0, b, 0);
		showI8("add", r, 0);
		check("i8x16 add wraps", r.getUI8(0) == 0x80 && r.getUI8(1) == 127);
		Vec.i8x16Sub(r, 0, a, 0, b, 0);
		showI8("sub", r, 0);
		Vec.i8x16Mul(r, 0, a, 0, b, 0);
		showI8("mul", r, 0);
		check("i8x16 mul", r.getUI8(2) == ((-26 * 25) & 0xff));
		Vec.i8x16Min(r, 0, a, 0, b, 0);
		showI8("min", r, 0);
		check("i8x16 min signed", r.getUI8(0) == 1 && r.getUI8(1) == 0x80);
		Vec.i8x16Max(r, 0, a, 0, b, 0);
		showI8("max", r, 0);
		check("i8x16 max signed", r.getUI8(0) == 127 && r.getUI8(1) == 0xff);
		Vec.i8x16Abs(r, 0, a, 0);
		showI8("abs", r, 0);
		check("i8x16 abs", r.getUI8(1) == 0x80 && r.getUI8(2) == 26);
		Vec.i8x16Neg(r, 0, a, 0);
		showI8("neg", r, 0);
		Vec.i8x16Splat(r, 0, 300);
		showI8("splat", r, 0);
		check("i8x16 splat truncates", r.getUI8(0) == 44 && r.getUI8(15) == 44);
		Vec.i8x16Shl(r, 0, a, 0, 9);
		showI8("shl9", r, 0);
		check("i8x16 shl masks", r.getUI8(0) == 0xfe);
		Vec.i8x16Shr(r, 0, a, 0, 1);
		showI8("shr1", r, 0);
		check("i8x16 shr arithmetic", r.getUI8(1) == 0xc0 && r.getUI8(0) == 63);
		Vec.i8x16Eq(r, 0, a, 0, b, 0);
		showI8("eq", r, 0);
		Vec.i8x16Ne(r, 0, a, 0, b, 0);
		showI8("ne", r, 0);
		Vec.i8x16Lt(r, 0, a, 0, b, 0);
		showI8("lt", r, 0);
		check("i8x16 lt signed", r.getUI8(0) == 0 && r.getUI8(1) == 0xff);
		Vec.i8x16Le(r, 0, a, 0, b, 0);
		showI8("le", r, 0);
		Vec.i8x16Gt(r, 0, a, 0, b, 0);
		showI8("gt", r, 0);
		Vec.i8x16Ge(r, 0, a, 0, b, 0);
		showI8("ge", r, 0);
		Sys.println("sum = " + Vec.i8x16Sum(a, 0));
		Sys.println("minLane = " + Vec.i8x16MinLane(a, 0));
		Sys.println("maxLane = " + Vec.i8x16MaxLane(a, 0));
		check("i8x16 minLane", Vec.i8x16MinLane(a, 0) == -128);
		check("i8x16 maxLane", Vec.i8x16MaxLane(a, 0) == 127);
		var hundred = new hl.Bytes(16);
		Vec.i8x16Splat(hundred, 0, 100);
		Sys.println("sum100 = " + Vec.i8x16Sum(hundred, 0));
		check("i8x16 sum wraps", Vec.i8x16Sum(hundred, 0) == 64);
		var arr = new hl.NativeArray<hl.UI8>(20);
		for (i in 0...20)
			arr[i] = i * 7;
		Vec.i8x16LoadArray(r, 0, arr, 3);
		showI8("loadArray", r, 0);
		check("i8x16 loadArray", r.getUI8(0) == 21 && r.getUI8(15) == 126);
		Vec.i8x16StoreArray(arr, 2, a, 0);
		Sys.println("storeArray = [" + [for (i in 0...20) Std.string(arr[i])].join(", ") + "]");
		check("i8x16 storeArray", arr[2] == 127 && arr[3] == 0x80 && arr[18] == 126);
	}

	static function testU8x16() {
		Sys.println("-- u8x16");
		var a = new hl.Bytes(16);
		var b = new hl.Bytes(16);
		for (i in 0...16) {
			a.setUI8(i, (i * 37) & 0xff);
			b.setUI8(i, (i * 11 + 3) & 0xff);
		}
		a.setUI8(0, 255);
		a.setUI8(1, 128);
		b.setUI8(0, 1);
		b.setUI8(1, 1);
		var r = new hl.Bytes(16);
		Vec.u8x16Add(r, 0, a, 0, b, 0);
		showU8("add", r, 0);
		check("u8x16 add wraps", r.getUI8(0) == 0 && r.getUI8(1) == 129);
		Vec.u8x16Sub(r, 0, b, 0, a, 0);
		showU8("sub", r, 0);
		check("u8x16 sub wraps", r.getUI8(0) == 2 && r.getUI8(1) == 129);
		Vec.u8x16Mul(r, 0, a, 0, b, 0);
		showU8("mul", r, 0);
		Vec.u8x16Min(r, 0, a, 0, b, 0);
		showU8("min", r, 0);
		check("u8x16 min unsigned", r.getUI8(0) == 1 && r.getUI8(1) == 1);
		Vec.u8x16Max(r, 0, a, 0, b, 0);
		showU8("max", r, 0);
		check("u8x16 max unsigned", r.getUI8(0) == 255 && r.getUI8(1) == 128);
		Vec.u8x16Splat(r, 0, -1);
		showU8("splat", r, 0);
		check("u8x16 splat truncates", r.getUI8(0) == 255);
		Vec.u8x16Shl(r, 0, a, 0, 1);
		showU8("shl1", r, 0);
		check("u8x16 shl", r.getUI8(0) == 254 && r.getUI8(1) == 0);
		Vec.u8x16Shr(r, 0, a, 0, 1);
		showU8("shr1", r, 0);
		check("u8x16 shr logical", r.getUI8(0) == 127 && r.getUI8(1) == 64);
		Vec.u8x16Eq(r, 0, a, 0, b, 0);
		showU8("eq", r, 0);
		Vec.u8x16Ne(r, 0, a, 0, b, 0);
		showU8("ne", r, 0);
		Vec.u8x16Lt(r, 0, a, 0, b, 0);
		showU8("lt", r, 0);
		check("u8x16 lt unsigned", r.getUI8(0) == 0 && r.getUI8(1) == 0);
		Vec.u8x16Le(r, 0, a, 0, b, 0);
		showU8("le", r, 0);
		Vec.u8x16Gt(r, 0, a, 0, b, 0);
		showU8("gt", r, 0);
		check("u8x16 gt unsigned", r.getUI8(0) == 255 && r.getUI8(1) == 255);
		Vec.u8x16Ge(r, 0, a, 0, b, 0);
		showU8("ge", r, 0);
		Sys.println("sum = " + Vec.u8x16Sum(a, 0));
		Sys.println("minLane = " + Vec.u8x16MinLane(a, 0));
		Sys.println("maxLane = " + Vec.u8x16MaxLane(a, 0));
		check("u8x16 minLane", Vec.u8x16MinLane(a, 0) == 3);
		check("u8x16 maxLane", Vec.u8x16MaxLane(a, 0) == 255);
		var arr = new hl.NativeArray<hl.UI8>(20);
		for (i in 0...20)
			arr[i] = 250 + i;
		Vec.u8x16LoadArray(r, 0, arr, 1);
		showU8("loadArray", r, 0);
		check("u8x16 loadArray", r.getUI8(0) == 251 && r.getUI8(5) == 0 && r.getUI8(15) == 10);
		Vec.u8x16StoreArray(arr, 4, a, 0);
		Sys.println("storeArray = [" + [for (i in 0...20) Std.string(arr[i])].join(", ") + "]");
		check("u8x16 storeArray", arr[4] == 255 && arr[5] == 128 && arr[3] == 253);
	}

	static function testV128() {
		Sys.println("-- v128");
		var a = i32(0x0f0f0f0f, -1, 0, 0x12345678);
		var b = i32(0x00ff00ff, 0x55555555, -1, 0x0000ffff);
		var r = new hl.Bytes(16);
		Vec.v128And(r, 0, a, 0, b, 0);
		showI32("and", r, 0);
		check("v128 and", r.getI32(0) == 0x000f000f && r.getI32(4) == 0x55555555 && r.getI32(8) == 0 && r.getI32(12) == 0x5678);
		Vec.v128Or(r, 0, a, 0, b, 0);
		showI32("or", r, 0);
		check("v128 or", r.getI32(0) == 0x0fff0fff && r.getI32(8) == -1 && r.getI32(12) == 0x1234ffff);
		Vec.v128Xor(r, 0, a, 0, b, 0);
		showI32("xor", r, 0);
		check("v128 xor", r.getI32(0) == 0x0ff00ff0 && r.getI32(4) == ~0x55555555);
		Vec.v128Not(r, 0, a, 0);
		showI32("not", r, 0);
		check("v128 not", r.getI32(0) == ~0x0f0f0f0f && r.getI32(4) == 0 && r.getI32(8) == -1);
		var mask = i32(-1, 0, 0x0000ffff, 0);
		var x = i32(1, 2, 0x11223344, 4);
		var y = i32(10, 20, 0x55667788, 40);
		Vec.v128Select(r, 0, mask, 0, x, 0, y, 0);
		showI32("select", r, 0);
		check("v128 select", r.getI32(0) == 1 && r.getI32(4) == 20 && r.getI32(8) == 0x55663344 && r.getI32(12) == 40);
		var wide = new hl.Bytes(40);
		Vec.v128Copy(wide, 3, x, 0);
		Vec.v128Copy(r, 0, wide, 3);
		showI32("copy", r, 0);
		check("v128 copy", r.getI32(0) == 1 && r.getI32(12) == 4 && wide.getI32(7) == 2);
	}

	static function testValues() {
		Sys.println("-- values");
		var a = Float32x4.make(1, 2, 3, 4);
		var b = Float32x4.splat(0.5);
		var c = Float32x4.make(-1, 0, 1, 2);
		var r = (a + b) * c - a;
		Sys.println(r.toString());
		check("Float32x4 chain", r.x == -2.5 && r.y == -2 && r.z == 0.5 && r.w == 5);
		Sys.println((a / b).toString());
		Sys.println((-a).toString());
		Sys.println(a.min(c).toString() + " " + a.max(c).toString());
		Sys.println(c.abs().sqrt().toString());
		Sys.println(a.fma(b, c).toString());
		check("Float32x4 fma", a.fma(b, c).w == 4);
		var m = a.gt(Float32x4.splat(2.5));
		Sys.println(m.toString());
		var s = Float32x4.select(m, a, c);
		Sys.println(s.toString());
		check("Float32x4 select", s.x == -1 && s.y == 0 && s.z == 3 && s.w == 4);
		Sys.println(a.sum() + " " + a.minLane() + " " + a.maxLane());
		check("Float32x4 sum", a.sum() == 10);
		Sys.println(a.eq(a).toString() + " " + a.ne(a).toString() + " " + a.lt(c).toString() + " " + a.le(c).toString() + " " + a.ge(c).toString());
		var bytes = new hl.Bytes(48);
		a.store(bytes, 20);
		var back = Float32x4.load(bytes, 20);
		Sys.println(back.toString());
		check("Float32x4 store/load", back.get(3) == 4 && bytes.getF32(20) == 1);
		back.set(1, 9);
		Sys.println(back.toString() + " " + a.toString());
		check("Float32x4 set", back.y == 9 && a.y == 2);
		Sys.println(a.toInt32x4().toString() + " " + Float32x4.make(2.9, -2.9, 1e10, Math.NaN).toInt32x4().toString());

		var i = Int32x4.make(1, -2, 3, 0x7fffffff);
		var j = Int32x4.splat(2);
		var k = (i + j) * j - i;
		Sys.println(k.toString());
		check("Int32x4 chain", k.x == 5 && k.y == 2 && k.z == 7 && k.w == -2147483645);
		Sys.println((i << 4).toString() + " " + (i >> 1).toString() + " " + (~i).toString());
		Sys.println((i & j).toString() + " " + (i | j).toString() + " " + (i ^ j).toString());
		Sys.println(i.min(j).toString() + " " + i.max(j).toString() + " " + i.abs().toString() + " " + (-i).toString());
		Sys.println(i.eq(j).toString() + " " + i.lt(j).toString() + " " + i.ge(j).toString());
		Sys.println(Int32x4.select(i.lt(j), i, j).toString());
		check("Int32x4 select", Int32x4.select(i.lt(j), i, j).y == -2 && Int32x4.select(i.lt(j), i, j).z == 2);
		Sys.println(i.sum() + " " + i.minLane() + " " + i.maxLane());
		check("Int32x4 sum wraps", i.sum() == -0x80000000 + 1);
		Sys.println(i.toFloat32x4().toString());
		i.store(bytes, 4);
		Sys.println(Int32x4.load(bytes, 4).toString());
	}

	static function main() {
		testF32x4();
		testF64x2();
		testI32x4();
		testI16x8();
		testI8x16();
		testU8x16();
		testV128();
		testValues();
		Sys.println(fails == 0 ? "all ok" : fails + " failures");
		if (fails != 0)
			Sys.exit(1);
	}
}
