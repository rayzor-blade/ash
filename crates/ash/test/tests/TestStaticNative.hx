// Calls three primitives of a library the program does not carry: two the
// app links in, and one nothing defines, which raises only when reached.
class TestStaticNative {
	@:hlNative("toy", "toy_add") static function add(a:Int, b:Int):Int { return 0; }
	@:hlNative("toy", "toy_scale") static function scale(x:Float, k:Float):Float { return 0.0; }
	@:hlNative("toy", "toy_absent") static function absent(a:Int):Int { return 0; }
	static function main() {
		Sys.println("add=" + add(40, 2));
		Sys.println("scale=" + scale(1.5, 4.0));
		var n = 0;
		for (i in 0...1000) n += add(i, 1);
		Sys.println("sum=" + n);
		if (Sys.args().length > 0) Sys.println("absent=" + absent(1));
	}
}
