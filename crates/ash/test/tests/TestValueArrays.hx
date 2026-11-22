/**
	Value arrays survive collections. Several hl.NativeArray values of
	different element types and lengths are kept while major collections run
	between bursts of garbage arrays; every element is then checked.
**/
class TestValueArrays {
	static function expected(k:Int, i:Int):Float {
		return k * 1000 + i * 0.5;
	}

	static function main() {
		var keepF = new Array<hl.NativeArray<Single>>();
		var keepD = new Array<hl.NativeArray<Float>>();
		var keepI = new Array<hl.NativeArray<Int>>();
		for (k in 0...6) {
			var f = new hl.NativeArray<Single>(1000 + k * 37);
			var d = new hl.NativeArray<Float>(300 + k * 11);
			var n = new hl.NativeArray<Int>(4 + k);
			for (i in 0...f.length) f[i] = expected(k, i);
			for (i in 0...d.length) d[i] = expected(k, i);
			for (i in 0...n.length) n[i] = k * 1000 + i;
			keepF.push(f);
			keepD.push(d);
			keepI.push(n);
		}
		for (round in 0...5) {
			hl.Gc.major();
			for (j in 0...300) {
				var junk = new hl.NativeArray<Single>(4096);
				for (i in 0...junk.length) junk[i] = -1.0;
				var small = new hl.NativeArray<Int>(8);
				for (i in 0...small.length) small[i] = -1;
			}
		}
		var ok = true;
		for (k in 0...6) {
			for (i in 0...keepF[k].length) if (keepF[k][i] != expected(k, i)) ok = false;
			for (i in 0...keepD[k].length) if (keepD[k][i] != expected(k, i)) ok = false;
			for (i in 0...keepI[k].length) if (keepI[k][i] != k * 1000 + i) ok = false;
		}
		Sys.println("done " + ok);
	}
}
