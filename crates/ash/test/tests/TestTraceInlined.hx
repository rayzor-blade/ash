// Stack traces through code the compiled tiers inline. At O3 `leaf`, `mid`
// and `top` fold into `main`, so a native walk sees one frame where the
// interpreter sees four; the tier's source map has to give the callees
// back, at their own lines, for haxe.CallStack and for the uncaught report
// on stderr. The interpreter is the oracle, so every engine has to print
// the same frames it does.
class TestTraceInlined {
	static function leaf(n:Int):Int {
		if (n > 2) throw "deep";
		return n + 1;
	}
	static function mid(n:Int):Int {
		var x = leaf(n);
		return x * 2;
	}
	static function top(n:Int):Int {
		return mid(n) + 1;
	}
	static function where():String {
		return haxe.CallStack.toString(haxe.CallStack.callStack());
	}
	static function main() {
		var t = 0;
		for (i in 0...300) t += top(i & 1);
		Sys.println("t=" + t);
		Sys.println(where());
		try {
			top(7);
		} catch (e:Dynamic) {
			Sys.println("caught " + e + haxe.CallStack.toString(haxe.CallStack.exceptionStack()));
		}
		top(9);
	}
}
