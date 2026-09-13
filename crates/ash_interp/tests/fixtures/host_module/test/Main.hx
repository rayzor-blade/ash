package test;

// A class the program declares against a host module: the natives it names
// are registered by the host at load time, never looked for in a library.
//
// The instance method wraps a static native that takes the receiver. An
// `@:hlNative` directly on an instance method of a class is allotted a
// findex that genhl never emits, so the call has no target.
@:keep class Greeter {
	var handle:hl.Abstract<"host_obj">;

	function new() {}

	@:hlNative("host", "greeter_make") public static function make():Greeter {
		return null;
	}

	@:hlNative("host", "greeter_bump") static function greeter_bump(g:Greeter):Int {
		return 0;
	}

	public inline function bump():Int {
		return greeter_bump(this);
	}

	/** A function the host makes: a var-args closure over a C entry, as
		`Reflect.makeVarArgs` makes, which Haxe calls as any function. */
	@:hlNative("host", "greeter_adder") public static function adder():Int->Int {
		return null;
	}

	/** A native the host takes by record: an object, a float and a bool
		in, a float out, through one C entry that unpacks by kind. */
	@:hlNative("host", "greeter_scale") public static function scale(g:Greeter, by:Float, on:Bool):Float {
		return 0.0;
	}
}

class Main {
	static function bump(g:Greeter):Int {
		return g.bump();
	}

	static function main() {
		// Enough calls for a tiered run to promote `bump`, so the compiled
		// body reaches the host native too, before the answer is taken from
		// a fresh object.
		var warm = Greeter.make();
		for (i in 0...400)
			bump(warm);
		var g = Greeter.make();
		bump(g);
		bump(g);
		var add = Greeter.adder();
		Sys.println(bump(g) + add(5));
		// 30 * 1.5 + 10, then 30 * 0.5 with nothing added.
		Sys.println(Greeter.scale(g, 1.5, true) + Greeter.scale(g, 0.5, false));
	}
}
