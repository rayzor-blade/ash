class TestStdArrayOps {
    static function assertEq(label:String, expected:Dynamic, actual:Dynamic):Void {
        if (expected != actual) {
            throw new haxe.Exception(label + " expected=" + expected + " actual=" + actual);
        }
    }

    static function main():Void {
        var a = [1, 2, 3];
        a.push(4);
        assertEq("push_len", 4, a.length);
        assertEq("pop", 4, a.pop());

        var b = a.concat([9, 10]);
        assertEq("concat_len", 5, b.length);
        assertEq("slice", 2, b.slice(1, 3)[0]);

        var c = [5, 1, 4, 2, 3];
        c.sort(function(x, y) return x - y);
        assertEq("sort_first", 1, c[0]);
        assertEq("sort_last", 5, c[4]);

        var d = [1, 2, 3, 4, 5];
        var removed = d.splice(1, 2);
        assertEq("splice_removed_len", 2, removed.length);
        assertEq("splice_len", 3, d.length);
        d.reverse();
        assertEq("reverse_first", 5, d[0]);

        Sys.println("OK TestStdArrayOps");
    }
}
