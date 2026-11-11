// Exceptions caught and passed on, enums built and taken apart, references to
// locals, narrow unsigned integers, and fields of a Dynamic found by name.
// main prints one value per line.
enum Tree {
	Leaf(v:Int);
	Node(l:Tree, r:Tree);
	Named(name:String, w:Float);
	Empty;
}

class MyErr {
	public var code:Int;

	public function new(code:Int) {
		this.code = code;
	}
}

class TestWasmFlow {
	static function sum(t:Tree):Int {
		return switch t {
			case Leaf(v): v;
			case Node(l, r): sum(l) + sum(r);
			case Named(_, w): Std.int(w);
			case Empty: 0;
		}
	}

	static function build(d:Int):Tree {
		return d == 0 ? Leaf(1) : Node(build(d - 1), build(d - 1));
	}

	static function describe(t:Tree):String {
		return switch t {
			case Leaf(v): "leaf" + v;
			case Node(_, _): "node";
			case Named(n, w): n + "/" + w;
			case Empty: "empty";
		}
	}

	static function thrower(n:Int):Int {
		if (n > 3)
			throw new MyErr(n);
		return n;
	}

	static function guarded(n:Int):Int {
		try {
			return thrower(n) + 1;
		} catch (e:MyErr) {
			return -e.code;
		}
	}

	static function nested(n:Int):String {
		try {
			try {
				thrower(n);
				return "ok";
			} catch (e:String) {
				return "str";
			}
		} catch (e:MyErr) {
			return "err" + e.code;
		}
	}

	static function rethrown(n:Int):Int {
		try {
			try {
				return thrower(n);
			} catch (e:MyErr) {
				throw e;
			}
		} catch (e:MyErr) {
			return e.code * 10;
		}
	}

	static function afterwards(n:Int):Int {
		var c = 0;
		try {
			if (n > 0)
				throw "x";
			c += 1;
		} catch (e:String) {
			c += 10;
		}
		c += 100;
		return c;
	}

	static function inLoop(n:Int):Int {
		var total = 0;
		for (i in 0...n) {
			try {
				total += thrower(i);
			} catch (e:Dynamic) {
				total += 1000;
			}
		}
		return total;
	}

	static function bump(r:hl.Ref<Int>, by:Int):Void {
		r.set(r.get() + by);
	}

	static function counter():Int {
		var x = 5;
		bump(x, 3);
		bump(x, 4);
		return x;
	}

	static function narrow(a:Int):Int {
		var u:hl.UI8 = a;
		var v:hl.UI16 = a * 200;
		var w:hl.UI8 = u + (200 : hl.UI8);
		return (u : Int) + (v : Int) * 7 + (w : Int);
	}

	static function dynGet(o:Dynamic):Int {
		return o.x + o.y;
	}

	static function dynSet(o:Dynamic):Void {
		o.count = 7;
		o.name = "n";
		o.flag = true;
		o.ratio = 0.25;
	}

	static function main() {
		Sys.println("sum=" + sum(build(5)) + "," + sum(Named("a", 2.5)) + "," + sum(Empty));
		Sys.println("describe=" + describe(Leaf(4)) + "," + describe(build(1)) + "," + describe(Named("w", 1.5)) + "," + describe(Empty));
		Sys.println("guarded=" + guarded(2) + "," + guarded(9));
		Sys.println("nested=" + nested(1) + "," + nested(8));
		Sys.println("rethrown=" + rethrown(2) + "," + rethrown(6));
		Sys.println("afterwards=" + afterwards(0) + "," + afterwards(1));
		Sys.println("loop=" + inLoop(6));
		Sys.println("counter=" + counter());
		Sys.println("narrow=" + narrow(3) + "," + narrow(100) + "," + narrow(250));
		Sys.println("dynget=" + dynGet({x: 3, y: 4}));
		var o:Dynamic = {};
		dynSet(o);
		Sys.println("dynset=" + o.count + "," + o.name + "," + o.flag + "," + o.ratio);
	}
}
