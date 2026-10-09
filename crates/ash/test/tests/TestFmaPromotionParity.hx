// A multiply-add in a function with no loop, called before and after the
// function is compiled: both calls have to round the same way. 0.1 * 10 is
// not exactly 1, so fused and separate rounding give different answers.
class TestFmaPromotionParity {
	static function mulAdd(a:Float, b:Float, c:Float):Float {
		return a * b + c;
	}

	static function sample(a:Float):Float {
		return mulAdd(a, 10.0, -1.0);
	}

	static function repeat(n:Int, a:Float):Float {
		var r = sample(a);
		return n == 0 ? r : repeat(n - 1, a);
	}

	static function main() {
		var a = Std.parseFloat("0.1");
		var before = sample(a);
		// Short runs: the interpreter's call depth is bounded.
		repeat(400, a);
		repeat(400, a);
		Sys.sleep(0.3);
		var after = repeat(400, a);
		Sys.println('before=$before after=$after same=${before == after}');
	}
}
