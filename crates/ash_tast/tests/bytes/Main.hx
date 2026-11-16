class Main {
	// keeps the std's own array methods in the program: they are what the test lowers
	static function flip(a:Array<Int>):Void {
		a.reverse();
	}

	static function has(a:Array<Int>, x:Int):Bool {
		return a.contains(x);
	}

	static function check(n:Int):Int {
		if (n < 0)
			throw "negative";
		return n * 2;
	}

	static function main() {}
}
