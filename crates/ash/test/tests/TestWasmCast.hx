// Conversions made by directly emitted code: numbers boxed into Dynamic and
// Null<T>, boxes unboxed again (including a null one), objects viewed as a
// structural type, and references that need no work. main prints one value
// per line.
typedef Pair = {
	var a:Int;
	var b:Float;
}

class Both {
	public var a:Int;
	public var b:Float;

	public function new(a:Int, b:Float) {
		this.a = a;
		this.b = b;
	}
}

class TestWasmCast {
	static function boxInt(x:Int):Dynamic {
		return x;
	}

	static function boxFloat(x:Float):Dynamic {
		return x;
	}

	static function boxBool(x:Bool):Dynamic {
		return x;
	}

	static function boxObject(o:Both):Dynamic {
		return o;
	}

	static function boxBytes(b:hl.Bytes):Dynamic {
		return b;
	}

	static function unboxInt(d:Dynamic):Int {
		return d;
	}

	static function unboxFloat(d:Dynamic):Float {
		return d;
	}

	static function unboxBool(d:Dynamic):Bool {
		return d;
	}

	static function orMinusOne(n:Null<Int>):Int {
		return n == null ? -1 : n;
	}

	static function maybe(x:Int, keep:Bool):Null<Int> {
		return keep ? x : null;
	}

	static function wide(x:Int):Null<haxe.Int64> {
		return x;
	}

	static function view(o:Both):Pair {
		return o;
	}

	static function readView(p:Pair):Float {
		return p.a + p.b;
	}

	static function main() {
		Sys.println("box=" + boxInt(7) + "," + boxFloat(2.5) + "," + boxBool(true));
		var o = new Both(3, 0.5);
		Sys.println("object=" + (boxObject(o) == o));
		var b = new hl.Bytes(4);
		Sys.println("bytes=" + (boxBytes(b) != null));
		Sys.println("unbox=" + unboxInt(boxInt(41)) + "," + unboxFloat(boxFloat(1.25)) + "," + unboxBool(boxBool(true)));
		Sys.println("unbox_null=" + unboxInt(null) + "," + unboxFloat(null) + "," + unboxBool(null));
		Sys.println("null_int=" + orMinusOne(5) + "," + orMinusOne(null) + "," + orMinusOne(maybe(9, true)) + "," + orMinusOne(maybe(9, false)));
		Sys.println("wide=" + haxe.Int64.toStr(wide(123456)));
		Sys.println("view=" + readView(view(o)));
	}
}
