class TestStdEReg {
    static function assertEq(label:String, expected:Dynamic, actual:Dynamic):Void {
        if (expected != actual) {
            throw new haxe.Exception(label + " expected=" + expected + " actual=" + actual);
        }
    }

    static function assertTrue(label:String, cond:Bool):Void {
        if (!cond) {
            throw new haxe.Exception(label + " expected true");
        }
    }

    static function main():Void {
        var re = ~/a+b/g;
        assertTrue("match", re.match("xxaaabyy"));
        assertEq("matched0", "aaab", re.matched(0));
        assertEq("left", "xx", re.matchedLeft());
        assertEq("right", "yy", re.matchedRight());

        var replaced = re.replace("aaab zz ab", "X");
        assertEq("replace", "X zz X", replaced);

        var split = ~/[,;]+/g.split("a,b;;c");
        assertEq("splitLen", 3, split.length);
        assertEq("split2", "c", split[2]);

        var mapped = ~/([0-9]+)/g.map("a1b22", function(r:EReg):String {
            return "[" + r.matched(1) + "]";
        });
        assertEq("map", "a[1]b[22]", mapped);

        Sys.println("OK TestStdEReg");
    }
}
