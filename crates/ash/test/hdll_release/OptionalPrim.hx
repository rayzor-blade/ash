// Two primitives from an optional library (`?sdl`) that no sdl.hdll exports.
// HashLink binds each to a stub that raises when called, so the program
// starts, skips the first, and catches the second.
class OptionalPrim {
	@:hlNative("?sdl", "no_such_prim_skipped") static function skipped():Int { return 0; }
	@:hlNative("?sdl", "no_such_prim_called") static function called():Int { return 0; }

	static function main() {
		Sys.println("start");
		if (Sys.args().length > 99) Sys.println(skipped());
		Sys.println("after-skip");
		try {
			Sys.println(called());
		} catch (e:Dynamic) {
			Sys.println("caught: " + e);
		}
		Sys.println("end");
	}
}
