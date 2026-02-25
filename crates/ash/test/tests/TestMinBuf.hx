class TestMinBuf {
	static function main():Void {
		Sys.println("test 1");
		var buf = new StringBuf();
		Sys.println("test 2: buf created");
		buf.add("hello");
		Sys.println("test 3: added hello");
		buf.add(", ");
		Sys.println("test 4: added separator");
		buf.add("world");
		Sys.println("test 5: added world");
		var result = buf.toString();
		Sys.println("result: " + result);
	}
}
