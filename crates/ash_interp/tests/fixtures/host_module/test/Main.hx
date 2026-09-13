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
		Sys.println(bump(g));
	}
}
