// Numeric kernels: what the first lowering slice covers. Each is lowered to AIR and run by the
// reference interpreter, and compared with the same function written in Rust.
class Main {
	static function fib(n:Int):Int
		return n < 2 ? n : fib(n - 1) + fib(n - 2);

	static function sum(n:Int):Int {
		var s = 0;
		var i = 0;
		while (i < n) {
			s += i;
			i++;
		}
		return s;
	}

	static function gcd(a:Int, b:Int):Int {
		while (b != 0) {
			var t = b;
			b = a % b;
			a = t;
		}
		return a;
	}

	static function collatz(n:Int):Int {
		var steps = 0;
		while (n != 1) {
			if (n % 2 == 0)
				n = n >> 1;
			else
				n = n * 3 + 1;
			steps++;
		}
		return steps;
	}

	static function mix(x:Float, n:Int):Float {
		var r = x;
		for (i in 0...n)
			r = r * 1.5 + i;
		return r;
	}

	static function isPrime(n:Int):Bool {
		if (n < 2)
			return false;
		var i = 2;
		while (i * i <= n) {
			if (n % i == 0)
				return false;
			i++;
		}
		return true;
	}

	static function abs(x:Int):Int
		return x < 0 ? -x : x;

	static function clamp(x:Float, lo:Float, hi:Float):Float {
		if (x < lo)
			return lo;
		if (x > hi)
			return hi;
		return x;
	}

	static function both(a:Int, b:Int):Bool
		return a > 0 && b > 0 || a < -10;

	static function sumDo(n:Int):Int {
		var s = 0;
		var i = 0;
		do {
			s += i;
			i++;
		} while (i < n);
		return s;
	}

	static function firstBig(n:Int):Int {
		var i = 0;
		while (true) {
			if (i * i > n)
				break;
			i++;
		}
		return i;
	}

	static function skipEven(n:Int):Int {
		var s = 0;
		var i = 0;
		while (i < n) {
			i++;
			if (i % 2 == 0)
				continue;
			s += i;
		}
		return s;
	}

	static function nested(n:Int):Int {
		var count = 0;
		for (i in 0...n)
			for (j in 0...i)
				if ((i + j) % 3 == 0)
					count++;
		return count;
	}

	// Std.int(a / b) on two integers is an integer division; on floats it cuts the quotient
	static function quotient(a:Int, b:Int):Int
		return Std.int(a / b);

	static function cut(x:Float):Int
		return Std.int(x);

	static function ratio(x:Float, y:Float):Int
		return Std.int(x / y);

	// UInt and integer constants compare unsigned; the values arrive as their 32-bit patterns
	static function above(a:UInt, b:UInt):Bool
		return a > b;

	static function atMost(a:UInt, b:UInt):Bool
		return a <= b;

	static function below(a:UInt, b:UInt):Bool
		return a < b;

	static function atLeast(a:UInt, b:UInt):Bool
		return a >= b;

	static function overFive(a:UInt):Bool
		return a > 5;

	static function main() {
		trace(above(1, 2) && atMost(1, 2) && below(1, 2) && atLeast(1, 2) && overFive(9));
		trace(quotient(7, 2) + cut(2.5) + ratio(7.0, 2.0));
		trace(fib(10) + sum(10) + gcd(12, 18) + collatz(27) + abs(-5) + firstBig(50) + skipEven(9) + sumDo(5) + nested(6));
		trace(isPrime(97) && both(1, 2));
		trace(mix(1.0, 3) + clamp(5.0, 0.0, 1.0));
	}
}
