class Main {
	// not ASCII, so the length in UTF-16 units is not the length in bytes
	static function greeting():String {
		return "héllo";
	}

	static function pick(n:Int):String {
		var s = "ab";
		if (n > 0)
			s = "xyz";
		return s;
	}

	// the same literal as in `pick`: one constant, not two
	static function again():String {
		return "ab";
	}

	static function size(n:Int):Int {
		return greeting().length + pick(n).length;
	}

	static function main() {}
}
