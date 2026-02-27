class TestMapDebug3 {
    static function main():Void {
        var m = new Map<String, Int>();
        m.set("a", 1);
        m.set("b", 2);
        m.set("c", 3);

        var it = m.iterator();
        Sys.println("hasNext1: " + it.hasNext());
        if (it.hasNext()) {
            var v = it.next();
            Sys.println("next1: " + v);
        }
        Sys.println("done");
    }
}
