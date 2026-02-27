enum Tree {
    Leaf(v:Int);
    Node(left:Tree, right:Tree);
}

class TestFeatureEnumsPattern {
    static function assertEq(label:String, expected:Dynamic, actual:Dynamic):Void {
        if (expected != actual) {
            throw new haxe.Exception(label + " expected=" + expected + " actual=" + actual);
        }
    }

    static function sum(t:Tree):Int {
        return switch (t) {
            case Leaf(v): v;
            case Node(l, r): sum(l) + sum(r);
        };
    }

    static function classify(t:Tree):String {
        return switch (t) {
            case Leaf(v) if (v > 10): "big";
            case Leaf(_): "small";
            case Node(_, _): "node";
        };
    }

    static function main():Void {
        var t = Node(Leaf(3), Node(Leaf(11), Leaf(1)));
        assertEq("sum", 15, sum(t));
        assertEq("classify1", "big", classify(Leaf(12)));
        assertEq("classify2", "small", classify(Leaf(2)));
        assertEq("classify3", "node", classify(t));

        var arr = [1, 2, 3];
        var out = switch (arr) {
            case [1, x, 3]: x;
            default: -1;
        };
        assertEq("array_pattern", 2, out);

        Sys.println("OK TestFeatureEnumsPattern");
    }
}
