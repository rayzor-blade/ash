// Calls natives of 9, 12 and 14 integer arguments: past the registers, so the
// last ones travel on the stack. Each argument has its own weight, so a value
// that lands in the wrong slot changes the sum.
class TestWideNative {
	@:hlNative("wide", "wide_9") static function w9(a0:Int, a1:Int, a2:Int, a3:Int, a4:Int, a5:Int, a6:Int, a7:Int, a8:Int):Int { return 0; }
	@:hlNative("wide", "wide_12") static function w12(a0:Int, a1:Int, a2:Int, a3:Int, a4:Int, a5:Int, a6:Int, a7:Int, a8:Int, a9:Int, a10:Int, a11:Int):Int { return 0; }
	@:hlNative("wide", "wide_14") static function w14(a0:Int, a1:Int, a2:Int, a3:Int, a4:Int, a5:Int, a6:Int, a7:Int, a8:Int, a9:Int, a10:Int, a11:Int, a12:Int, a13:Int):Int { return 0; }
	@:hlNative("wide", "wide_flags") static function flags(a0:Int, a1:Int, a2:Int, a3:Int, a4:Int, a5:Int, a6:Int, a7:Int, b0:Bool, b1:Bool, b2:Bool, a8:Int, b3:Bool):Int { return 0; }
	@:hlNative("wide", "wide_mixed") static function mixed(a0:Int, b1:Float, a2:Int, f3:Single, a4:Int, b5:Float, a6:Int, f7:Single, a8:Int, b9:Float, a10:Int, f11:Single, a12:Int, b13:Float):Float { return 0.0; }
	@:hlNative("wide", "wide_single") static function single(a0:Int, a1:Int, a2:Int, a3:Int, a4:Int, a5:Int, a6:Int, a7:Int, a8:Int, f9:Single):Single { return 0.0; }
	static function main() {
		Sys.println("w9=" + w9(1, 2, 3, 4, 5, 6, 7, 8, 9));
		Sys.println("w12=" + w12(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12));
		Sys.println("w14=" + w14(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14));
		Sys.println("flags=" + flags(1, 2, 3, 4, 5, 6, 7, 8, true, false, true, 9, true));
		Sys.println("mixed=" + mixed(1, 2.0, 3, 4.0, 5, 6.0, 7, 8.0, 9, 10.0, 11, 12.0, 13, 14.0));
		Sys.println("single=" + single(1, 2, 3, 4, 5, 6, 7, 8, 9, 2.5));
	}
}
