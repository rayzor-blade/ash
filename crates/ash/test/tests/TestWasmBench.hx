// The same numeric work, timed inside the program (LLVM-built wasm) and from
// the host (directly emitted wasm).
class TestWasmBench {
	static function fib(n:Int):Int {
		return n < 2 ? n : fib(n - 1) + fib(n - 2);
	}

	static function loopSum(n:Int):Int {
		var s = 0;
		for (i in 0...n) s += i % 7 * (i & 3);
		return s;
	}

	static function mandel(cx:Float, cy:Float, max:Int):Int {
		var x = 0.0, y = 0.0, i = 0;
		while (i < max && x * x + y * y <= 4.0) {
			var t = x * x - y * y + cx;
			y = 2.0 * x * y + cy;
			x = t;
			i++;
		}
		return i;
	}

	static function grid(n:Int):Int {
		var total = 0;
		for (j in 0...n)
			for (i in 0...n)
				total += mandel(i / n * 3.0 - 2.0, j / n * 2.0 - 1.0, 200);
		return total;
	}

	static function time(name:String, f:Void->Int) {
		var t0 = haxe.Timer.stamp();
		var r = f();
		var ms = Std.int((haxe.Timer.stamp() - t0) * 1000.0);
		Sys.println(name + " " + r + " " + ms + "ms");
	}

	static function main() {
		time("fib", function() return fib(35));
		time("loopSum", function() return loopSum(200000000));
		time("grid", function() return grid(400));
	}
}
