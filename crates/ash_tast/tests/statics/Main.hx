class Config {
	public static var base:Int = 10;
	public static var scale:Float = 2.5;
	// reads a static declared above it, so the order of initialization shows
	public static var table:Int = base * 3;
	public static var counter:Int = 0;

	public static function bump():Int {
		counter++;
		return counter;
	}

	public static function scaled(x:Int):Float {
		return x * scale;
	}
}

class Main {
	static var total:Int = 7;

	static function work(n:Int):Int {
		total += n;
		return total + Config.table + Config.bump();
	}

	static function main() {}
}
