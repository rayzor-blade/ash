class Counter {
	public var n:Int;

	public function new() {
		n = 0;
	}

	public function bump(by:Int):Int {
		n += by;
		return n;
	}
}

class Main {
	static function fib(n:Int):Int {
		return n < 2 ? n : fib(n - 1) + fib(n - 2);
	}

	static function main() {
		Sys.println("fib " + fib(15));
		var c = new Counter();
		for (i in 0...5)
			c.bump(i);
		Sys.println("counter " + c.n);
		var a = [3, 1, 2];
		a.push(7);
		var s = 0;
		for (i in 0...a.length)
			s += a[i];
		Sys.println("sum " + s + " of " + a.length);
		Sys.println("sqrt " + Math.sqrt(2.25));
	}
}
