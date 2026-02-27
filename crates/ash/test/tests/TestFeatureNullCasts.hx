class TestFeatureNullCasts {
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
        var n:Null<Int> = null;
        assertTrue("null_check", n == null);
        n = 42;
        assertEq("null_value", 42, n);

        var d:Dynamic = "123";
        assertTrue("is_string", Std.isOfType(d, String));
        assertEq("parse_int", 123, Std.parseInt(cast d));

        var f:Float = 3.9;
        var i:Int = Std.int(f);
        assertEq("float_to_int", 3, i);

        Sys.println("OK TestFeatureNullCasts");
    }
}
