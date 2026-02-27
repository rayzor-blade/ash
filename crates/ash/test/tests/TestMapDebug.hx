class TestMapDebug {
    static function main():Void {
        var m = new Map<String, Int>();
        m.set("a", 1);
        m.set("b", 2);
        m.set("c", 3);

        // Test keys() as array
        var ka:Array<String> = [];
        for (k in m.keys()) {
            ka.push(k);
        }
        Sys.println("keys len: " + ka.length);
        
        // Test via value iteration  
        var va:Array<Int> = [];
        for (v in m) {
            va.push(v);
        }
        Sys.println("vals len: " + va.length);
        
        // Direct iterator access
        var it = m.iterator();
        Sys.println("hasNext: " + it.hasNext());
        
        Sys.println("done");
    }
}
