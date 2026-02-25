class TestMinBuf3 {
	static function main():Void {
		Sys.println("test 1");
		var s:String = "hello";
		Sys.println("test 2: s.length = " + s.length);
		Sys.println("test 3: s = " + s);
		var buf = new StringBuf();
		Sys.println("test 4: buf created");
		buf.addSub(s, 0);
		Sys.println("test 5: addSub done");
		buf.add(s);
		Sys.println("test 6: add done");
	}
}
