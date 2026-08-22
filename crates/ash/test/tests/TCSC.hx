class TCSC {
	static function widen(d:Dynamic):String {
		return cast d;
	}

	static function main() {
		var acc = 0;
		var last = "";
		for (i in 0...200000) {
			var d:Dynamic = (i % 7 == 0) ? null : "abc";
			var s = widen(d);
			if (s == null) acc++; else acc += s.length;
			last = s;
		}
		Sys.println("acc=" + acc);
		Sys.println("last=" + last);
	}
}
