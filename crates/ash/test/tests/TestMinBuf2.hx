class TestMinBuf2 {
	static function main():Void {
		Sys.println("test 1");
		var buf = new StringBuf();
		Sys.println("test 2: buf created");
		var result = buf.toString();
		Sys.println("test 3: empty toString: " + result);
		var s = "hello";
		Sys.println("test 4: string len: " + s.length);
		buf.addChar(65);
		Sys.println("test 5: addChar done");
		result = buf.toString();
		Sys.println("test 6: toString: " + result);
	}
}
