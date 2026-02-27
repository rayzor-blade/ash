class TestStdDate {
    static function assertEq(label:String, expected:Dynamic, actual:Dynamic):Void {
        if (expected != actual) {
            throw new haxe.Exception(label + " expected=" + expected + " actual=" + actual);
        }
    }

    static function main():Void {
        var d = Date.fromString("2020-01-02 03:04:05");
        assertEq("year", 2020, d.getFullYear());
        assertEq("month", 0, d.getMonth());
        assertEq("day", 2, d.getDate());
        assertEq("hour", 3, d.getHours());
        assertEq("min", 4, d.getMinutes());
        assertEq("sec", 5, d.getSeconds());
        assertEq("toString", "2020-01-02 03:04:05", d.toString());

        var d2 = Date.fromTime(d.getTime());
        assertEq("roundtrip", d.toString(), d2.toString());

        Sys.println("OK TestStdDate");
    }
}
