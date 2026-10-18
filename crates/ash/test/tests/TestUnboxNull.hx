// charCodeAt answers Null<Int>. Every way a program can read it, at the edges
// where null, 0 and the largest unit meet: before the start, at the end, a NUL
// character, U+FFFF, and arithmetic on the result.
class TestUnboxNull {
	static function describe(s:String):String {
		var out = [];
		for (i in -1...s.length + 2) {
			var c = s.charCodeAt(i);
			if (c == null)
				out.push("null");
			else if (c == 65)
				out.push("A");
			else if (c == 0)
				out.push("nul");
			else if (c != 66)
				out.push("#" + (c + 1));
			else
				out.push("B");
		}
		return out.join(",");
	}

	static function digits(s:String):Int {
		var n = 0;
		var i = 0;
		var c;
		while ((c = s.charCodeAt(i)) != null) {
			if (c >= 48 && c <= 57)
				n = n * 10 + (c - 48);
			i++;
		}
		return n;
	}

	static function countIfNull(s:String, from:Int):Int {
		var nulls = 0;
		for (i in from...from + 4)
			if (s.charCodeAt(i) == null)
				nulls++;
		return nulls;
	}

	static function main() {
		Sys.println(describe("AB"));
		Sys.println(describe(""));
		Sys.println(describe("A" + String.fromCharCode(0) + String.fromCharCode(0xFFFF) + "z"));
		Sys.println(digits("a1b2c3 45"));
		Sys.println(digits(""));
		Sys.println(countIfNull("xy", -2));
		Sys.println(countIfNull("xy", 0));
		var big = String.fromCharCode(0xFFFF);
		var c = big.charCodeAt(0);
		Sys.println(c == 65535);
		Sys.println(c != 65535);
		Sys.println(c + 1);
	}
}
