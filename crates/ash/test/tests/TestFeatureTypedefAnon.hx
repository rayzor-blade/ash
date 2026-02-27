typedef Point = {
    var x:Int;
    var y:Int;
}

class TestFeatureTypedefAnon {
    static function assertEq(label:String, expected:Dynamic, actual:Dynamic):Void {
        if (expected != actual) {
            throw new haxe.Exception(label + " expected=" + expected + " actual=" + actual);
        }
    }

    static function len2(p:Point):Int {
        return p.x * p.x + p.y * p.y;
    }

    static function main():Void {
        var p:Point = {x: 3, y: 4};
        assertEq("len2", 25, len2(p));

        var dyn:Dynamic = {name: "A", score: 7};
        assertEq("reflect_name", "A", Reflect.field(dyn, "name"));
        Reflect.setField(dyn, "score", 9);
        assertEq("reflect_score", 9, Reflect.field(dyn, "score"));

        Sys.println("OK TestFeatureTypedefAnon");
    }
}
