// Calls through a pointer: a method of a class, of an interface and of a
// structural type, and closures called with the signature they were made
// with and with another. main prints one value per line.
interface Shape {
	function area():Int;
	function label():String;
}

class Square implements Shape {
	var s:Int;

	public function new(s:Int) {
		this.s = s;
	}

	public function area():Int {
		return s * s;
	}

	public function label():String {
		return "square";
	}
}

class Rect implements Shape {
	var w:Int;
	var h:Int;

	public function new(w:Int, h:Int) {
		this.w = w;
		this.h = h;
	}

	public function area():Int {
		return w * h;
	}

	public function label():String {
		return "rect";
	}
}

class Base {
	public var v:Int;

	public function new(v:Int) {
		this.v = v;
	}

	public function get():Int {
		return v;
	}

	public function twice():Int {
		return get() * 2;
	}

	public function scale(k:Float):Float {
		return get() * k;
	}
}

class Derived extends Base {
	override public function get():Int {
		return v + 100;
	}
}

typedef HasArea = {function area():Int;}

class TestWasmDispatch {
	static function sumAreas(a:Array<Shape>):Int {
		var t = 0;
		for (s in a)
			t += s.area();
		return t;
	}

	static function labels(a:Array<Shape>):String {
		var out = "";
		for (s in a)
			out += s.label() + ";";
		return out;
	}

	static function baseTwice(b:Base):Int {
		return b.twice();
	}

	static function baseScale(b:Base, k:Float):Float {
		return b.scale(k);
	}

	static function viaStructure(x:HasArea):Int {
		return x.area();
	}

	static function applyInt(f:Int->Int, x:Int):Int {
		return f(f(x));
	}

	static function applyFloat(f:Float->Float, x:Float):Float {
		return f(x) + 0.5;
	}

	static function applyTwo(f:(Int, Int) -> Int, a:Int, b:Int):Int {
		return f(a, b);
	}

	static function applyBool(f:Int->Bool, x:Int):Bool {
		return f(x);
	}

	static function applyVoid(f:Int->Void, x:Int):Void {
		f(x);
	}

	static function callDyn(f:Dynamic, x:Int):Dynamic {
		return f(x);
	}

	static function offset(d:Int):Int->Int {
		return function(y:Int) return y + d;
	}

	static function main() {
		var shapes:Array<Shape> = [new Square(3), new Rect(2, 5), new Square(4)];
		Sys.println("areas=" + sumAreas(shapes));
		Sys.println("labels=" + labels(shapes));
		Sys.println("base=" + baseTwice(new Base(21)));
		Sys.println("derived=" + baseTwice(new Derived(21)));
		Sys.println("scale=" + baseScale(new Derived(1), 0.5));
		Sys.println("structure=" + viaStructure(new Square(6)) + "," + viaStructure(new Rect(3, 3)) + "," + viaStructure({area: function() return 7}));
		Sys.println("closure=" + applyInt(offset(5), 1) + "," + applyInt(function(y) return y * y, 3));
		Sys.println("float=" + applyFloat(function(y) return y * 2, 1.25));
		Sys.println("two=" + applyTwo(function(a, b) return a - b, 10, 4));
		Sys.println("bool=" + applyBool(function(y) return y > 3, 5) + "," + applyBool(function(y) return y > 3, 1));
		var seen = 0;
		applyVoid(function(y) seen += y, 9);
		Sys.println("void=" + seen);
		Sys.println("dyn=" + callDyn(offset(7), 3) + "," + callDyn(function(y:Int) return "s" + y, 4));
		var adapted:Dynamic = offset(1);
		Sys.println("adapted=" + adapted(10));
	}
}
