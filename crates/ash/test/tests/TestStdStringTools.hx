import StringTools;

class TestStdStringTools {
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
        var s = "  HashLink  ";
        assertEq("trim", "HashLink", StringTools.trim(s));
        assertEq("lpad", "0007", StringTools.lpad("7", "0", 4));
        assertEq("rpad", "7___", StringTools.rpad("7", "_", 4));
        assertEq("replace", "axbxc", StringTools.replace("a-b-c", "-", "x"));
        assertTrue("startsWith", StringTools.startsWith("abcdef", "abc"));
        assertTrue("endsWith", StringTools.endsWith("abcdef", "def"));
        assertTrue("contains", StringTools.contains("abcdef", "cde"));

        var encoded = StringTools.urlEncode("a b+c");
        assertEq("urlDecode", "a b+c", StringTools.urlDecode(encoded));

        Sys.println("OK TestStdStringTools");
    }
}
