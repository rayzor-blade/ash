// `Std.int(f) == f` is how a Float is asked whether it is integral:
// haxe.format.JsonParser.parseNumber decides Int-or-Float with it, and so does
// Std.isOfType(_, Int) on HashLink. LLVM's fptosi is poison when the value does
// not fit, and an optimizer allowed to assume the conversion fit folded the
// test to true for 1e10 -- Json.parse("10000000000") came back as the Int
// -2147483648 -- once the surrounding bodies became inlinable (TestJson,
// TestReflect in the Haxe suite, AOT only). The conversion is saturating now.
//
// The out-of-range integer itself is deliberately never printed: x86-64
// HashLink answers INT_MIN where aarch64 HashLink and ash's interpreter
// saturate. What every engine must agree on is that it is NOT the float.
class TestFloatToIntRange {
	@:pure(false) static function classify(f:Float):String {
		var i = Std.int(f);
		return if (i == f) "int" else "float";
	}

	@:pure(false) static function parse(s:String):Dynamic {
		return haxe.Json.parse(s);
	}

	static function main() {
		for (s in ["10000000000", "-10000000000", "4294967296", "2147483647", "-2147483648", "42", "-42", "1.5", "0", "1e300"]) {
			var f = Std.parseFloat(s);
			var v:Dynamic = parse(s);
			trace(s + ": " + classify(f) + " isInt=" + Std.isOfType(v, Int) + " isFloat=" + Std.isOfType(v, Float) + " same=" + (v == f));
		}
		// Named rather than printed: eval spells these "nan"/"infinity"/
		// "neg_infinity" and HashLink "NaN"/"inf"/"-inf", and this case is
		// about the conversion, not about how a special formats.
		var names = ["NaN", "+inf", "-inf"];
		var specials = [Math.NaN, Math.POSITIVE_INFINITY, Math.NEGATIVE_INFINITY];
		for (i in 0...specials.length) {
			var f = specials[i];
			var d:Dynamic = f;
			trace(names[i] + ": " + classify(f) + " isInt=" + Std.isOfType(d, Int));
		}
	}
}
