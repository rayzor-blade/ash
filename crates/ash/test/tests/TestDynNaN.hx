// Dynamic comparisons with NaN are IEEE in every engine: NaN equals nothing (itself
// included) and satisfies no ordering test. The rounds repeat so the helpers are
// compiled before the last one is printed.
class TestDynNaN {
	static function eq(a:Dynamic, b:Dynamic):Bool return a == b;
	static function ne(a:Dynamic, b:Dynamic):Bool return a != b;
	static function lt(a:Dynamic, b:Dynamic):Bool return a < b;
	static function le(a:Dynamic, b:Dynamic):Bool return a <= b;
	static function gt(a:Dynamic, b:Dynamic):Bool return a > b;
	static function ge(a:Dynamic, b:Dynamic):Bool return a >= b;

	static function row(label:String, a:Dynamic, b:Dynamic):String
		return label + " == " + eq(a, b) + " != " + ne(a, b) + " < " + lt(a, b) + " <= " + le(a, b) + " > " + gt(a, b) + " >= " + ge(a, b);

	static function main() {
		var nan:Dynamic = Math.NaN;
		var one:Dynamic = 1.0;
		var two:Dynamic = 2;
		var last = "";
		// Enough calls for the helpers above to be compiled before the last round.
		for (i in 0...400) {
			last = row("nan,nan", nan, nan) + "\n" + row("nan,1.0", nan, one) + "\n" + row("1.0,nan", one, nan) + "\n"
				+ row("nan,2", nan, two) + "\n" + row("2,nan", two, nan) + "\n" + row("1.0,1.0", one, one) + "\n" + row("1.0,2", one, two);
		}
		Sys.println(last);
	}
}
