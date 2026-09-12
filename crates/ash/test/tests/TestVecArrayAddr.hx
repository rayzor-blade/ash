// Loops the widener turns into vector loads and stores over an
// hl.NativeArray (MemAccess::Array: the index counts ELEMENTS from the
// array's data) and over hl.Bytes (MemAccess::Mem: the index counts bytes).
// A backend that forms an Array lane address the way it forms a Bytes one
// reads the array header and the wrong elements.
//
// The induction steps are spelled `i = 1 + i` on purpose: Haxe compiles
// `i = i + 1` and `i++` to OIncr, which the widener does not retime, so
// those loops stay scalar and would test nothing here. The expected values
// are computed afterwards in ordinary `for` loops, which do use OIncr.
//
// Every function runs many times so the tiers promote it and the widened
// body is what executes; lengths that do not divide the lane count run
// their remainder through the scalar epilogue.
class TestVecArrayAddr {
	static var bad = 0;

	static function check(what:String, got:Int, want:Int) {
		if (got != want) {
			bad++;
			Sys.println("MISMATCH " + what + " got=" + got + " want=" + want);
		}
	}

	// Array-kind vector load into a reduction.
	static function sumArray(a:hl.NativeArray<Int>, n:Int):Int {
		var sum = 0;
		var i = 0;
		while (i < n) {
			sum = sum + a[i];
			i = 1 + i;
		}
		return sum;
	}

	// Array-kind vector store of a broadcast value.
	static function fillArray(a:hl.NativeArray<Int>, n:Int, v:Int):Void {
		var i = 0;
		while (i < n) {
			a[i] = v;
			i = 1 + i;
		}
	}

	// Array-kind load, lane arithmetic against a broadcast, then a reduction.
	static function dotConst(a:hl.NativeArray<Int>, n:Int, k:Int):Int {
		var acc = 0;
		var i = 0;
		while (i < n) {
			acc = acc + a[i] * k;
			i = 1 + i;
		}
		return acc;
	}

	// Mem-kind vector load: hl.Bytes is indexed in bytes, so the walk steps
	// by 4 and the trip count is a constant so no remainder loop is needed.
	static function sumBytes(b:hl.Bytes):Int {
		var sum = 0;
		var p = 0;
		while (p < 256) {
			sum = sum + b.getI32(p);
			p = 4 + p;
		}
		return sum;
	}

	static function main() {
		var lens = [64, 67, 4, 5, 1, 0];
		var rounds = 200;
		for (round in 0...rounds) {
			for (n in lens) {
				var a = new hl.NativeArray<Int>(n);
				for (i in 0...n) a[i] = i * 7 - 100 + round;
				var wantSum = 0;
				var wantDot = 0;
				for (i in 0...n) {
					wantSum += i * 7 - 100 + round;
					wantDot += (i * 7 - 100 + round) * 3;
				}
				check("sumArray " + n + " round " + round, sumArray(a, n), wantSum);
				check("dotConst " + n + " round " + round, dotConst(a, n, 3), wantDot);

				var b = new hl.NativeArray<Int>(n + 4);
				for (i in 0...n + 4) b[i] = -1;
				fillArray(b, n, 11 + round);
				var got = 0;
				var want = 0;
				for (i in 0...n + 4) {
					got += b[i] * (i + 1);
					want += (i < n ? 11 + round : -1) * (i + 1);
				}
				check("fillArray " + n + " round " + round, got, want);
			}
			var bytes = new hl.Bytes(256);
			var wantBytes = 0;
			for (i in 0...64) {
				bytes.setI32(i << 2, i * 7 - 100 + round);
				wantBytes += i * 7 - 100 + round;
			}
			check("sumBytes round " + round, sumBytes(bytes), wantBytes);
		}
		if (bad > 0) {
			Sys.println("FAILED " + bad);
			Sys.exit(1);
		}
		Sys.println("OK");
	}
}
