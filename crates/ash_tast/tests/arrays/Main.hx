class Main {
	static function sum(a:Array<Int>):Int {
		var total = 0;
		var i = 0;
		while (i < a.length) {
			total += a[i];
			i++;
		}
		return total;
	}

	static function dot(a:Array<Float>, b:Array<Float>):Float {
		var s = 0.0;
		for (i in 0...a.length)
			s += a[i] * b[i];
		return s;
	}

	// an index past the end reads the element type's default
	static function at(a:Array<Int>, i:Int):Int {
		return a[i];
	}

	static function main() {}
}
