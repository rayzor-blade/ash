// Closures made by directly emitted code: over a static function, over an
// instance method, and over a lambda that captures a variable. They are
// called by name, through a function value, and from a collection.
// main prints one value per line.
final class Counter {
	public var n:Int;

	public function new(n:Int) {
		this.n = n;
	}

	public function bump(k:Int):Int {
		n += k;
		return n;
	}
}

class TestWasmClosure {
	static function twice(x:Int):Int {
		return x * 2;
	}

	static function applyTwice(f:Int->Int, x:Int):Int {
		return f(f(x));
	}

	static function viaStatic(x:Int):Int {
		return applyTwice(twice, x);
	}

	static function viaMethod(c:Counter, x:Int):Int {
		return applyTwice(c.bump, x);
	}

	static function viaLambda(base:Int, x:Int):Int {
		var f = function(y:Int) return y * 3 + base;
		return applyTwice(f, x);
	}

	static function pick(which:Int):Int->Int {
		return which == 0 ? twice : function(y:Int) return y - 1;
	}

	static function makeBumper(c:Counter):Int->Int {
		return c.bump;
	}

	static function main() {
		Sys.println("static=" + viaStatic(5));
		var c = new Counter(100);
		Sys.println("method=" + viaMethod(c, 7) + "," + c.n);
		Sys.println("lambda=" + viaLambda(10, 2));
		Sys.println("pick=" + pick(0)(9) + "," + pick(1)(9));
		var bump = makeBumper(c);
		Sys.println("bumper=" + bump(1) + "," + bump(2) + "," + c.n);
	}
}
