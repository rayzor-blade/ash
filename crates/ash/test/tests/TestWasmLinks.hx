class Box {
	public var tag = 0;

	public function new() {}
}

/** Natives a host links ahead of time; see tests/aot_links.rs for the links. */
class TestWasmLinks {
	@:hlNative("links_test", "add") static function add(a:Int, b:Int):Int return 0;
	@:hlNative("links_test", "twice64") static function twice64(v:Int):Int return 0;
	@:hlNative("links_test", "negate") static function negate(flag:Int):Bool return false;
	@:hlNative("links_test", "half") static function half(x:Float):Float return 0;
	@:hlNative("links_test", "boxed_half") static function boxedHalf(word:hl.I64):hl.I64 return word;
	@:hlNative("links_test", "bump") static function bump(v:Int):Int return 0;
	@:hlNative("links_test", "add_counted") static function addCounted(a:Int, b:Int):Int return 0;
	@:hlNative("links_test", "add_flagged") static function addFlagged(a:Int, b:Int):Int return 0;
	@:hlNative("links_test", "set_pending") static function setPending(v:Int):Void {}
	@:hlNative("links_test", "ticks") static function ticks():Int return 0;
	@:hlNative("links_test", "bind") static function bind(box:Box, seed:Int):Void {}
	@:hlNative("links_test", "bound_made") static function boundMade():Int return 0;
	@:hlNative("links_test", "bound_is") static function boundIs(box:Box):Bool return false;
	@:hlNative("links_test", "bound_typed") static function boundTyped():Bool return false;

	static function main():Void {
		Sys.println("add " + add(40, 2));
		Sys.println("twice64 " + twice64(-21) + " " + twice64(2147483647));
		Sys.println("negate " + negate(5) + " " + negate(0));
		Sys.println("half " + half(3.0));

		var word:haxe.Int64 = haxe.io.FPHelper.doubleToI64(3.0);
		var halved:haxe.Int64 = boxedHalf(word);
		Sys.println("boxed " + haxe.io.FPHelper.i64ToDouble(halved.low, halved.high));
		// A word that carries the tag of a boxed value is not a number.
		var tagged:haxe.Int64 = boxedHalf(haxe.Int64.make(0x7ffc0000, 1));
		Sys.println("boxed-nan " + tagged.high + " " + tagged.low);

		Sys.println("bump " + bump(1));

		Sys.println("ticks " + ticks());
		Sys.println("counted " + addCounted(3, 4) + " ticks " + ticks());
		setPending(0);
		Sys.println("flagged-off " + addFlagged(4, 5) + " ticks " + ticks());
		setPending(1);
		Sys.println("flagged-on " + addFlagged(5, 6) + " ticks " + ticks());

		var box = new Box();
		bind(box, 4);
		Sys.println("bind " + boundMade() + " " + boundIs(box) + " " + boundTyped());
	}
}
