// References through the direct wasm backend: equality of boxed and object
// values, null checks that raise a catchable error, fields of every kind,
// global slots and type constants. main prints one value per line.
class Box {
	public var flag:Bool;
	public var count:Int;
	public var ratio:Float;
	public var big:haxe.Int64;
	public var label:String;
	public var next:Box;

	public function new() {}
}

class TestWasmDynamic {
	static var counter = 0;
	static var shared:Box;

	static function checkEq<T>(a:T, b:T):Bool {
		return a == b;
	}

	static function fill(x:Box, n:Int):Int {
		x.flag = n > 2;
		x.count = n * 3;
		x.ratio = n * 0.5;
		x.big = haxe.Int64.ofInt(n) * 1000000000;
		x.label = "n" + n;
		x.next = shared;
		counter += n;
		return x.count + (x.flag ? 1 : 0);
	}

	static function touch(x:Box):Int {
		return x.count;
	}

	static function main() {
		Sys.println("eq_int=" + checkEq(2000000, 2000000));
		Sys.println("eq_int_different=" + checkEq(1, 2));
		Sys.println("eq_string=" + checkEq("basicstring", "basicstring"));
		Sys.println("eq_string_different=" + checkEq("a", "b"));
		var a = [];
		var b = [];
		Sys.println("eq_array_same=" + checkEq(a, a));
		Sys.println("eq_array_other=" + checkEq(a, b));

		shared = new Box();
		shared.count = 99;
		var x = new Box();
		Sys.println("fill=" + fill(x, 5));
		Sys.println("fill_again=" + fill(x, 1));
		Sys.println("flag=" + x.flag);
		Sys.println("count=" + x.count);
		Sys.println("ratio=" + x.ratio);
		Sys.println("big=" + haxe.Int64.toStr(x.big));
		Sys.println("label=" + x.label);
		Sys.println("next=" + (x.next == shared));
		Sys.println("next_count=" + x.next.count);
		Sys.println("counter=" + counter);

		var caught = "none";
		try {
			var n:Box = null;
			touch(n);
		} catch (e:Dynamic) {
			caught = "caught";
		}
		Sys.println("null_access=" + caught);
		Sys.println("class_name=" + Type.getClassName(Type.getClass(x)));
	}
}
