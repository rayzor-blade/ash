// A cast that fails inside `try` must reach the `catch`, on every engine.
//
// `safecast`: Std.isOfType given an Int where a class belongs, a Dynamic to
// class cast the runtime rejects.
// `callmethod`: Reflect.callMethod passing an object of the wrong class.
// `variance`: an Array<Float> where an Array<Null<Float>> is typed, a class
// cast whose `__cast` answers null.
//
// The program is aborted if a failure escapes, so each section runs as its
// own process, selected by the first argument.

class Box {
	public function new() {}
}

class Other {
	public function new() {}
}

class TestCastInTry {
	static function check(v:Dynamic, t:Dynamic):Bool
		return Std.isOfType(v, t);

	static function take(b:Box):String
		return "took " + b;

	static function maybe(v:Null<String>):Null<Float>
		return v == null ? null : Std.parseFloat(v);

	static function main() {
		try {
			switch (Sys.args()[0]) {
				case "safecast":
					Sys.println("isOfType: " + check(new Box(), 3));
				case "callmethod":
					var r:Dynamic = Reflect.callMethod(null, take, [new Other()]);
					Sys.println("returned: " + r);
				case "variance":
					var height = maybe(null);
					var box = switch "0 0 10 10" {
						case null: [0.0, 0.0, 1.0, height];
						case _: [0.0, 0.0, 10.0, 10.0];
					}
					Sys.println("not cast: " + box[3]);
				case other:
					Sys.println("unknown section " + other);
			}
		} catch (e:Dynamic) {
			Sys.println("caught: " + e);
		}
		Sys.println("after");
	}
}
