// What the compiler plugin writes beyond the program's code: declaration metadata, an
// `__init__` block, optional arguments, a type parameter's constraint, runtime metadata,
// defines and a resource.
interface Shape {
	function area():Float;
}

@:ash.layout({align: 16, fields: ["x", "y"]})
class Point implements Shape {
	@:ash.noThrow public var x:Int;
	public var y(get, never):Int;

	public function new(x:Int) {
		this.x = x;
	}

	function get_y():Int
		return x * 2;

	public function area():Float
		return x + y;
}

@:ash.pure("a", -1, 2.5, true, null, foo.bar)
@author("native")
class Main {
	@:ash.hot static var counter = 0;
	static var started = false;

	static function __init__() {
		started = true;
	}

	static dynamic function replaceable():Int
		return 1;

	static function opt(a:Int, ?b:Int, c:Int = 3):Int
		return a + c;

	static function constrained<T:Shape>(t:T):Float
		return t.area();

	static function main() {
		var @:ash.keep local = 1;
		counter += @:ash.unchecked(1, 2) opt(local);
		trace(constrained(new Point(counter)) + replaceable());
		trace(haxe.Resource.getString("hello"));
	}
}
