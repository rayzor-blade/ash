class TestMapIter {
    static function main():Void {
        var m = new Map<String, Int>();
        m.set("a", 1);
        m.set("b", 2);
        m.set("c", 3);
        
        Sys.println("has a: " + m.exists("a"));
        Sys.println("get a: " + m.get("a"));
        
        var count = 0;
        for (v in m) {
            Sys.println("val: " + v);
            count++;
        }
        Sys.println("count: " + count);
        
        // Also try keys
        var kcount = 0;
        for (k in m.keys()) {
            Sys.println("key: " + k);
            kcount++;
        }
        Sys.println("kcount: " + kcount);
    }
}
