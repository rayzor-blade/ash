class Box<T> {
    public var value:T;

    public function new(v:T) {
        value = v;
    }

    public function map<U>(f:T->U):Box<U> {
        return new Box<U>(f(value));
    }
}

class TestFeatureGenerics {
    static function assertEq(label:String, expected:Dynamic, actual:Dynamic):Void {
        if (expected != actual) {
            throw new haxe.Exception(label + " expected=" + expected + " actual=" + actual);
        }
    }

    static function first<T>(arr:Array<T>):T {
        return arr[0];
    }

    static function main():Void {
        var b = new Box<Int>(5);
        var b2 = b.map(function(v) return "v=" + v);
        assertEq("box_map", "v=5", b2.value);

        var xs = [10, 20, 30];
        assertEq("generic_fn", 10, first(xs));

        var ys = [for (i in 0...4) i];
        assertEq("generic_array_len", 4, ys.length);
        assertEq("generic_array_last", 3, ys[3]);

        Sys.println("OK TestFeatureGenerics");
    }
}
