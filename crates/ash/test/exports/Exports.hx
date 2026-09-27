// Members a host calls by symbol; driven by exports_driver.c through
// crates/ash/tests/aot_exports.rs.
@:keep
class Counter {
	public static var made = 0;

	public var count:Int;

	public function new(start:Int) {
		count = start;
		made++;
	}

	public function bump(by:Int):Int {
		count += by;
		return count;
	}

	public static function add(a:Int, b:Int):Int
		return a + b;

	public static function fail(x:Int):Int {
		if (x > 0)
			throw "refused";
		return x;
	}

	public static function half(x:Float):Float
		return x / 2;
}

@:keep
class Twice extends Counter {
	public function new(start:Int)
		super(start);

	override public function bump(by:Int):Int {
		count += 2 * by;
		return count;
	}
}

class Exports {
	static function main() {
		Sys.println("drive " + drive());
	}

	@:hlNative("exports_test", "drive")
	static function drive():Int
		return 0;
}
