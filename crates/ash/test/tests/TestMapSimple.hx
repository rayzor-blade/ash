class TestMapSimple {
    static function main():Void {
        var m = new Map<String, Int>();
        m.set("x", 42);

        Sys.println("exists: " + m.exists("x"));
        Sys.println("get: " + m.get("x"));

        for (v in m) {
            Sys.println("value: " + v);
        }
        Sys.println("done");
    }
}
