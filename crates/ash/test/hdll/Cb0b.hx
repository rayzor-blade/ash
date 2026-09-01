// Case #0 variant: identical shape (static fn, one Int arg), except the
// closure is handed to the native library from a HELPER frame that then
// returns. After handOff() returns, no live Haxe frame register holds the
// vclosure -- the malloc'd C struct is the only reference. This separates
// "ash roots HDLL-held closures" from "the caller's dead frame register
// happened to still point at it".
class Cb0b {
    @:hlNative("cb0", "store")
    static function nativeStore(f:Int->Int):Void {}

    @:hlNative("cb0", "invoke")
    static function nativeInvoke(x:Int):Int { return 0; }

    static function triple(x:Int):Int { return x * 3 + 1; }

    static function handOff():Void {
        nativeStore(triple);
    }

    static function main() {
        Sys.println("case0b: static fn stored from a helper frame");
        handOff();
        var n = 200000;
        var e = Sys.getEnv("CB0_CHURN");
        if (e != null) n = Std.parseInt(e);
        var sink = 0;
        for (i in 0...n) {
            var a = [i, i + 1, i + 2];
            sink += a[2];
            var s = "s" + i;
            if (s.length == 0) sink++;
        }
        Sys.println("churn sink: " + sink);
        var r = nativeInvoke(14);
        Sys.println("invoke(14) = " + r + "  (expect 43)");
        Sys.println(r == 43 ? "CASE0B: PASS" : "CASE0B: FAIL");
    }
}
