import haxe.io.Bytes;

class TestStdBytes {
    static function assertEq(label:String, expected:Dynamic, actual:Dynamic):Void {
        if (expected != actual) {
            throw new haxe.Exception(label + " expected=" + expected + " actual=" + actual);
        }
    }

    static function main():Void {
        var b = Bytes.alloc(8);
        for (i in 0...8) {
            b.set(i, i * 3);
        }

        assertEq("len", 8, b.length);
        assertEq("get5", 15, b.get(5));

        var copy = Bytes.alloc(8);
        copy.blit(0, b, 0, 8);
        assertEq("copy5", 15, copy.get(5));

        var sub = copy.sub(2, 3);
        assertEq("subLen", 3, sub.length);
        assertEq("sub0", 6, sub.get(0));

        var msg = Bytes.ofString("hashlink");
        assertEq("ofStringLen", 8, msg.length);
        assertEq("subString", "sh", msg.sub(2, 2).toString());

        var from = Bytes.ofString("ABC");
        b.blit(1, from, 0, 3);
        assertEq("blit1", 65, b.get(1));
        assertEq("blit3", 67, b.get(3));

        Sys.println("OK TestStdBytes");
    }
}
