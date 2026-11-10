// Numeric functions for the direct wasm emitter. main prints one result per
// call, in the order the Rust test calls them.
class TestWasmNumeric {
	static function fib(n:Int):Int {
		return n < 2 ? n : fib(n - 1) + fib(n - 2);
	}

	static function sumSquares(n:Int):Int {
		var s = 0;
		for (i in 0...n) s += i * i;
		return s;
	}

	static function collatz(n:Int):Int {
		var steps = 0;
		while (n != 1) {
			n = (n % 2 == 0) ? n >> 1 : 3 * n + 1;
			steps++;
		}
		return steps;
	}

	static function gcd(a:Int, b:Int):Int {
		while (b != 0) {
			var t = b;
			b = a % b;
			a = t;
		}
		return a;
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

	static function bits(a:Int, b:Int):Int {
		return ((a << 3) ^ (b >>> 2)) & 0xFFFF | (a >> 1);
	}

	static function neg(a:Int):Int {
		return -a;
	}

	static function pick(n:Int):Int {
		switch (n) {
			case 0: return 10;
			case 1: return 20;
			case 2: return 30;
			default: return -1;
		}
	}

	static function scaled(a:Int, b:Float):Int {
		return Std.int((a * 1.5 + b) / 3.0 * 1000.0);
	}

	static function quotient(a:Int, b:Int):Int {
		return Std.int(a / b);
	}

	static function wide(a:haxe.Int64, b:haxe.Int64):haxe.Int64 {
		return a * b + (a >> 3);
	}

	static function main() {
		for (n in [0, 1, 2, 10, 20, 25]) Sys.println("fib=" + fib(n));
		for (n in [0, 1, 10, 100, 1000]) Sys.println("sumSquares=" + sumSquares(n));
		for (n in [1, 6, 27, 97]) Sys.println("collatz=" + collatz(n));
		Sys.println("gcd=" + gcd(12, 18));
		Sys.println("gcd=" + gcd(17, 5));
		Sys.println("gcd=" + gcd(0, 9));
		Sys.println("gcd=" + gcd(270, 192));
		Sys.println("mandel=" + mandel(0.0, 0.0, 100));
		Sys.println("mandel=" + mandel(1.0, 1.0, 100));
		Sys.println("mandel=" + mandel(-0.5, 0.5, 200));
		Sys.println("mandel=" + mandel(0.3, 0.5, 500));
		Sys.println("bits=" + bits(5, 9));
		Sys.println("bits=" + bits(1234, 5678));
		Sys.println("bits=" + bits(-7, 3));
		for (n in [5, -5, 0]) Sys.println("neg=" + neg(n));
		for (n in [0, 1, 2, 3, -1]) Sys.println("pick=" + pick(n));
		Sys.println("scaled=" + scaled(4, 0.5));
		Sys.println("scaled=" + scaled(-7, 2.25));
		Sys.println("quotient=" + quotient(17, 5));
		Sys.println("quotient=" + quotient(-17, 5));
		Sys.println("wide=" + haxe.Int64.toStr(wide(haxe.Int64.ofInt(123456789), haxe.Int64.ofInt(1000))));
	}
}
