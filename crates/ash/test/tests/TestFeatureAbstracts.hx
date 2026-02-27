abstract Meter(Float) from Float to Float {
    public inline function new(v:Float) this = v;

    @:op(A + B)
    public static inline function add(a:Meter, b:Meter):Meter {
        return new Meter((a : Float) + (b : Float));
    }

    public inline function cm():Int {
        return Std.int((this : Float) * 100);
    }
}

enum abstract Flag(Int) from Int to Int {
    var Off = 0;
    var On = 1;
}

class TestFeatureAbstracts {
    static function assertEq(label:String, expected:Dynamic, actual:Dynamic):Void {
        if (expected != actual) {
            throw new haxe.Exception(label + " expected=" + expected + " actual=" + actual);
        }
    }

    static function main():Void {
        var a:Meter = 1.25;
        var b:Meter = 0.75;
        var c:Meter = a + b;
        assertEq("meter_add", 200, c.cm());

        var f:Flag = Flag.On;
        assertEq("enum_abstract", 1, (f : Int));

        Sys.println("OK TestFeatureAbstracts");
    }
}
