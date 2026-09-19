// Loop-carried values that trade places. After copy propagation a swap is
// two phis reading each other's destination and a rotation is a chain of
// them; a de-SSA consumer that assigns one at a time turns `a, b = b, a`
// into `a, b = b, b`. Every engine has to print what the interpreter does,
// and each loop runs long enough to be compiled and entered mid-way.
class TestPhiCopies {
	static function swap(n:Int):Int {
		var a = 1;
		var b = 2;
		for (i in 0...n) {
			var t = a;
			a = b;
			b = t;
		}
		return a * 10 + b;
	}

	static function rotate(n:Int):Int {
		var a = 1;
		var b = 2;
		var c = 3;
		for (i in 0...n) {
			var t = a;
			a = b;
			b = c;
			c = t;
		}
		return a * 100 + b * 10 + c;
	}

	static function fib(n:Int):Int {
		var a = 0;
		var b = 1;
		for (i in 0...n) {
			var t = a + b;
			a = b;
			b = t;
		}
		return a;
	}

	static function chain(n:Int):Int {
		// prev = cur; cur = next: a phi whose source is the other phi's
		// destination in the same block, which is what GVN makes of a
		// linked-list walk.
		var prev = 0;
		var cur = 1;
		var next = 2;
		for (i in 0...n) {
			prev = cur;
			cur = next;
			next = prev + cur;
		}
		return prev * 1000 + cur;
	}

	static function main() {
		for (n in [0, 1, 2, 3, 7, 100, 100001, 300000]) {
			Sys.println(n + ": " + swap(n) + " " + rotate(n) + " " + fib(n % 40) + " " + chain(n % 30));
		}
	}
}
