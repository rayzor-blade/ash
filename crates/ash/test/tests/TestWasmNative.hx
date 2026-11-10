// Calls into the runtime from directly emitted code: math natives, string
// natives and their results of every kind. main prints one value per line.
class TestWasmNative {
	static function rounding(x:Float):Int {
		return Math.floor(x) + Math.ceil(x) * 10 + Math.round(x) * 100;
	}

	static function classify(x:Float):Int {
		return (Math.isNaN(x) ? 1 : 0) + (Math.isFinite(x) ? 2 : 0);
	}

	static function powers(a:Float, b:Float):Float {
		return Math.pow(a, b) + Math.sqrt(a * a);
	}

	static function curve(x:Float):Int {
		return Math.round(Math.sin(x) * 1000) + Math.round(Math.cos(x) * 1000);
	}

	static function code(s:String, i:Int):Int {
		return s.charCodeAt(i);
	}

	static function span(s:String, from:Int, len:Int):String {
		return s.substr(from, len);
	}

	static function where(s:String, t:String):Int {
		return s.indexOf(t);
	}

	static function main() {
		for (x in [0.5, 1.5, -1.5, 2.0, -0.25]) {
			Sys.println("rounding=" + rounding(x));
		}
		Sys.println("classify=" + classify(1.0) + "," + classify(0.0 / 0.0) + "," + classify(1.0 / 0.0));
		Sys.println("powers=" + powers(2.0, 10.0) + "," + powers(3.0, 2.0));
		Sys.println("curve=" + curve(0.0) + "," + curve(1.0) + "," + curve(2.5));
		Sys.println("code=" + code("hello", 1) + "," + code("hello", 4));
		Sys.println("span=" + span("direct backend", 7, 4));
		Sys.println("where=" + where("direct backend", "back") + "," + where("abc", "z"));
	}
}
