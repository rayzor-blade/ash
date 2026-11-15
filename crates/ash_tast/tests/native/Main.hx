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

// A native handle type, and a generic abstract: the compiler unwraps both with their arguments.
abstract Handle(hl.Abstract<"fixture_handle">) {}

abstract Box<T>(Array<T>) {
	public function new(items:Array<T>)
		this = items;
}

// An override gives the method a virtual slot; two generic methods each have a `T`; a structure
// can contain itself through a typedef and reach a generic function.
class Base {
	public function new() {}

	public function name():String
		return "base";
}

class Derived extends Base {
	override public function name():String
		return "derived";
}

typedef Rec = {field:Rec}

// A generic class: `Pair<Int, String>` is one class to HashLink, which stores both fields boxed.
class Pair<A, B> {
	public var first:A;
	public var second:B;

	public function new(a:A, b:B) {
		first = a;
		second = b;
	}

	public function swap():Pair<B, A>
		return new Pair(second, first);
}

@:ash.pure("a", -1, 2.5, true, null, foo.bar)
@author("native")
class Main {
	@:ash.hot static var counter = 0;
	static var started = false;
	static var handle:Handle;
	static var boxed:Box<Int> = new Box([1]);

	static function __init__() {
		started = true;
	}

	static dynamic function replaceable():Int
		return 1;

	static function opt(a:Int, ?b:Int, c:Int = 3):Int
		return a + c;

	static function constrained<T:Shape>(t:T):Float
		return t.area();

	static function strings<T:String>(a:Array<T>):Int
		return a.length;

	static function floats<T:Float>(a:Array<T>):Int
		return a.length;

	@:generic static function gen<T>(v:T) {}

	static function main() {
		var @:ash.keep local = 1;
		counter += @:ash.unchecked(1, 2) opt(local);
		trace(constrained(new Point(counter)) + replaceable());
		trace(haxe.Resource.getString("hello"));
		trace(strings(["x"]) + floats([1.5]) + new Derived().name().length);
		gen((null:Rec));
		trace(new Pair(1, "x").swap().first);
	}
}
