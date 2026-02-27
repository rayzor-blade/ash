class Counter {
    var i:Int;
    var max:Int;

    public function new(max:Int) {
        this.i = 0;
        this.max = max;
    }

    public function hasNext():Bool {
        return i < max;
    }

    public function next():Int {
        return i++;
    }
}

class TestFeatureIterators {
    static function assertEq(label:String, expected:Dynamic, actual:Dynamic):Void {
        if (expected != actual) {
            throw new haxe.Exception(label + " expected=" + expected + " actual=" + actual);
        }
    }

    static function main():Void {
        var squares = [for (i in 0...5) i * i];
        assertEq("comp_len", 5, squares.length);
        assertEq("comp_last", 16, squares[4]);

        var evens = [for (i in 0...8) if (i % 2 == 0) i];
        assertEq("filter_comp_len", 4, evens.length);
        assertEq("filter_comp_last", 6, evens[3]);

        var sum = 0;
        for (x in new Counter(4)) {
            sum += x;
        }
        assertEq("custom_iterator", 6, sum); // 0+1+2+3

        Sys.println("OK TestFeatureIterators");
    }
}
