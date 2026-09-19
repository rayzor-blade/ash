// Loops the widener turns into vectors of lanes narrower and wider than
// Int: 16 bytes, 8 shorts, 2 64-bit ints, 2 doubles. The lane count comes
// from the element width, so every loop is one 128-bit vector per trip; a
// backend that assumed four lanes would address a quarter or four times the
// memory of each.
//
// The induction steps are spelled `i = 1 + i` on purpose: Haxe compiles
// `i = i + 1` and `i++` to OIncr, which the widener does not retime. The
// expected values are computed afterwards in ordinary `for` loops.
//
// Every function runs many times so the tiers promote it, and the lengths
// leave remainders for the scalar epilogue at every width.
class TestVecLanes {
	static var bad = 0;

	static function check(what:String, got:Int, want:Int) {
		if (got != want) {
			bad++;
			Sys.println("MISMATCH " + what + " got=" + got + " want=" + want);
		}
	}

	static function checkF(what:String, got:Float, want:Float) {
		if (got != want) {
			bad++;
			Sys.println("MISMATCH " + what + " got=" + got + " want=" + want);
		}
	}

	// Byte lanes.
	static function fillBytes(a:hl.NativeArray<hl.UI8>, n:Int, v:hl.UI8):Void {
		var i = 0;
		while (i < n) {
			a[i] = v;
			i = 1 + i;
		}
	}

	// In place, against a broadcast: two arrays would be a pair of bases
	// the alias analysis cannot separate.
	static function addBytes(a:hl.NativeArray<hl.UI8>, n:Int, k:hl.UI8):Void {
		var i = 0;
		while (i < n) {
			a[i] = a[i] + k;
			i = 1 + i;
		}
	}

	// A reduction into a byte: sixteen partial sums that wrap at 8 bits,
	// folded into a register that wraps the same way.
	static function sumBytes(a:hl.NativeArray<hl.UI8>, n:Int):hl.UI8 {
		var sum:hl.UI8 = 0;
		var i = 0;
		while (i < n) {
			sum = sum + a[i];
			i = 1 + i;
		}
		return sum;
	}

	// Short lanes.
	static function fillShorts(a:hl.NativeArray<hl.UI16>, n:Int, v:hl.UI16):Void {
		var i = 0;
		while (i < n) {
			a[i] = v;
			i = 1 + i;
		}
	}

	static function addShorts(a:hl.NativeArray<hl.UI16>, n:Int, k:hl.UI16):Void {
		var i = 0;
		while (i < n) {
			a[i] = a[i] + k;
			i = 1 + i;
		}
	}

	// 64-bit lanes, two to a vector: the width that used to be refused.
	static function fillLongs(a:hl.NativeArray<hl.I64>, n:Int, v:hl.I64):Void {
		var i = 0;
		while (i < n) {
			a[i] = v;
			i = 1 + i;
		}
	}

	static function sumLongs(a:hl.NativeArray<hl.I64>, n:Int):hl.I64 {
		var sum:hl.I64 = 0;
		var i = 0;
		while (i < n) {
			sum = sum + a[i];
			i = 1 + i;
		}
		return sum;
	}

	// Double lanes.
	static function fillFloats(a:hl.NativeArray<Float>, n:Int, v:Float):Void {
		var i = 0;
		while (i < n) {
			a[i] = v;
			i = 1 + i;
		}
	}

	static function scaleFloats(a:hl.NativeArray<Float>, n:Int, k:Float):Void {
		var i = 0;
		while (i < n) {
			a[i] = a[i] * k;
			i = 1 + i;
		}
	}

	static function main() {
		var lens = [64, 67, 33, 17, 5, 1, 0];
		var rounds = 200;
		for (round in 0...rounds) {
			for (n in lens) {
				// Bytes: fill into a longer array, then add with wrap.
				var a8 = new hl.NativeArray<hl.UI8>(n + 16);
				for (i in 0...n + 16) a8[i] = 255;
				fillBytes(a8, n, 40 + round % 100);
				var b8 = new hl.NativeArray<hl.UI8>(n + 16);
				for (i in 0...n + 16) b8[i] = (i * 37 + round) & 255;
				addBytes(b8, n, 200 + round % 50);
				var got = 0;
				var want = 0;
				for (i in 0...n + 16) {
					// Read into Int first: arithmetic on hl.UI8 wraps at 8 bits.
					var ai:Int = a8[i];
					var bi:Int = b8[i];
					got += ai * (i + 1) + bi * (i + 3);
					var w = i < n ? 40 + round % 100 : 255;
					var x = i < n ? (((i * 37 + round) & 255) + 200 + round % 50) & 255 : (i * 37 + round) & 255;
					want += w * (i + 1) + x * (i + 3);
				}
				check("bytes " + n + " round " + round, got, want);
				var wrap = 0;
				for (i in 0...n) {
					var bi:Int = b8[i];
					wrap = (wrap + bi) & 255;
				}
				var gotWrap:Int = sumBytes(b8, n);
				check("sumBytes " + n + " round " + round, gotWrap, wrap);

				// Shorts.
				var a16 = new hl.NativeArray<hl.UI16>(n + 8);
				for (i in 0...n + 8) a16[i] = 65535;
				fillShorts(a16, n, 30000 + round);
				var b16 = new hl.NativeArray<hl.UI16>(n + 8);
				for (i in 0...n + 8) b16[i] = (i * 1001 + round) & 65535;
				addShorts(b16, n, 60000 + round);
				got = 0;
				want = 0;
				for (i in 0...n + 8) {
					var ai:Int = a16[i];
					var bi:Int = b16[i];
					got += ai * (i + 1) + bi * (i + 3);
					var w = i < n ? 30000 + round : 65535;
					var x = i < n ? (((i * 1001 + round) & 65535) + 60000 + round) & 65535 : (i * 1001 + round) & 65535;
					want += w * (i + 1) + x * (i + 3);
				}
				check("shorts " + n + " round " + round, got, want);

				// Longs: values past 32 bits so a lane truncated to Int shows.
				var a64 = new hl.NativeArray<hl.I64>(n + 2);
				for (i in 0...n + 2) a64[i] = -1;
				var big:hl.I64 = 1;
				big = big << 40;
				fillLongs(a64, n, big + round);
				var gotL:hl.I64 = 0;
				var wantL:hl.I64 = 0;
				for (i in 0...n + 2) {
					gotL = gotL + a64[i] * (i + 1);
					wantL = wantL + (i < n ? big + round : -1) * (i + 1);
				}
				check("longs " + n + " round " + round, (gotL == wantL) ? 1 : 0, 1);
				check("sumLongs " + n + " round " + round, (sumLongs(a64, n) == (big + round) * n) ? 1 : 0, 1);

				// Doubles.
				var af = new hl.NativeArray<Float>(n + 2);
				for (i in 0...n + 2) af[i] = -1.5;
				fillFloats(af, n, 0.25 + round);
				var bf = new hl.NativeArray<Float>(n + 2);
				for (i in 0...n + 2) bf[i] = i * 0.5 + round;
				scaleFloats(bf, n, 3.0);
				var gotF = 0.0;
				var wantF = 0.0;
				for (i in 0...n + 2) {
					gotF += af[i] * (i + 1) + bf[i];
					wantF += (i < n ? 0.25 + round : -1.5) * (i + 1) + (i < n ? (i * 0.5 + round) * 3.0 : i * 0.5 + round);
				}
				checkF("floats " + n + " round " + round, gotF, wantF);
			}
		}
		if (bad > 0) {
			Sys.println("FAILED " + bad);
			Sys.exit(1);
		}
		Sys.println("OK");
	}
}
