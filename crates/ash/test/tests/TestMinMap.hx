class TestMinMap {
	static function main():Void {
		Sys.println("test 1");
		var m = new Map<String, Int>();
		m.set("Alice", 30);
		m.set("Bob", 25);
		Sys.println("test 2: set done");
		Sys.println("Alice: " + m.get("Alice"));
		Sys.println("test 3: get done");
		m.set("Charlie", 35);
		Sys.println("test 4: exists=" + m.exists("Charlie"));
		m.set("Diana", 28);
		Sys.println("test 5: set Diana");
		m.remove("Bob");
		Sys.println("test 6: remove Bob");
		var count = 0;
		for (_ in m) {
			count++;
		}
		Sys.println("test 7: count=" + count);
	}
}
