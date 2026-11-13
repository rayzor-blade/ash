// A library primitive named like a function of the runtime: ash_std exports
// `hlp_future_state`, so a phone program cannot also link a library's
// `hlp_future_state`.
class TestNativeClash {
	@:hlNative("clash", "future_state") static function state(a:Int):Int { return 0; }
	static function main() {
		Sys.println("state=" + state(1));
	}
}
