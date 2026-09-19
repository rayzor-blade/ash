import haxe.CallStack;

// A stack captured in an interpreted function reached from compiled code
// through the stub bridge. The three captures -- callStack, a constructed
// exception, a thrown one -- must name the capturing line at the top and
// agree on depth, whichever engine ran the frames above; the interpreter
// is the oracle.

class TestTraceBridge {
	static var sink = 0;

	// Hot: promoted early.
	static function hot(i:Int):Int {
		var s = 0;
		for (k in 0...50) s += (i + k) % 7;
		return s;
	}

	// Cold until the very end: first called from compiled code.
	static function cold(tag:String):String {
		var a = CallStack.callStack();
		var b = new haxe.Exception('').stack;
		var c = try throw new haxe.Exception('') catch (e:haxe.Exception) e.stack;
		return tag + " call=" + top(a) + " new=" + top(b) + " throw=" + top(c) + " depth=" + a.length + "/" + b.length + "/" + c.length;
	}

	static function top(s:CallStack):String {
		if (s.length == 0) return "<empty>";
		return switch (s[0]) {
			case FilePos(_, f, l, _): f + ":" + l;
			case Method(c, m): "m:" + c + "." + m;
			case _: "other";
		}
	}

	static function driver(n:Int):String {
		var r = "";
		for (i in 0...n) {
			sink += hot(i);
			if (i == n - 1) r = cold("late");
		}
		return r;
	}

	static function main() {
		Sys.println(cold("early"));
		Sys.println(driver(20000));
		Sys.println(cold("after"));
	}
}
