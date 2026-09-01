// CASE #2 -- static Haxe functions with MANY arguments (6..10), handed to a
// native HDLL which later calls them back through hl_dyn_call.
//
// Each function is a weighted sum with distinct prime weights, so a dropped,
// duplicated, or mis-positioned argument produces a distinct wrong number
// rather than accidentally matching.
//
//   argument i is (100 + i)  as Int, or (100 + i) + 0.5 as Float
//   weights   p = [2,3,5,7,11,13,17,19,23,29]
//
// Every closure is invoked TWICE from native code: once immediately after
// being stored (no GC pressure in between) and once after a heavy allocation
// churn. An ABI/marshalling bug fails BOTH; a missing-GC-root bug fails only
// the second.

class TestHdllCb2 {
    @:hlNative("cb2test", "store6")
    static function nStore6(f:(Int, Int, Int, Int, Int, Int) -> Int):Void {}
    @:hlNative("cb2test", "store7")
    static function nStore7(f:(Int, Int, Int, Int, Int, Int, Int) -> Int):Void {}
    @:hlNative("cb2test", "store8")
    static function nStore8(f:(Int, Int, Int, Int, Int, Int, Int, Int) -> Int):Void {}
    @:hlNative("cb2test", "store9")
    static function nStore9(f:(Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Int):Void {}
    @:hlNative("cb2test", "storemix")
    static function nStoreMix(f:(Int, Float, Int, Float, Int, Float, Int, Float, Int) -> Float):Void {}
    @:hlNative("cb2test", "store10")
    static function nStore10(f:(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Int):Void {}

    @:hlNative("cb2test", "invokei")
    static function nInvokeI(idx:Int):Int { return 0; }
    @:hlNative("cb2test", "invoked")
    static function nInvokeD(idx:Int):Float { return 0.0; }

    public static function f6(a:Int, b:Int, c:Int, d:Int, e:Int, f:Int):Int {
        return a * 2 + b * 3 + c * 5 + d * 7 + e * 11 + f * 13;
    }

    public static function f7(a:Int, b:Int, c:Int, d:Int, e:Int, f:Int, g:Int):Int {
        return a * 2 + b * 3 + c * 5 + d * 7 + e * 11 + f * 13 + g * 17;
    }

    public static function f8(a:Int, b:Int, c:Int, d:Int, e:Int, f:Int, g:Int, h:Int):Int {
        return a * 2 + b * 3 + c * 5 + d * 7 + e * 11 + f * 13 + g * 17 + h * 19;
    }

    public static function f9(a:Int, b:Int, c:Int, d:Int, e:Int, f:Int, g:Int, h:Int, i:Int):Int {
        return a * 2 + b * 3 + c * 5 + d * 7 + e * 11 + f * 13 + g * 17 + h * 19 + i * 23;
    }

    public static function fmix9(a:Int, b:Float, c:Int, d:Float, e:Int, f:Float, g:Int, h:Float, i:Int):Float {
        return a * 2 + b * 3 + c * 5 + d * 7 + e * 11 + f * 13 + g * 17 + h * 19 + i * 23;
    }

    public static function f10(a:Int, b:Int, c:Int, d:Int, e:Int, f:Int, g:Int, h:Int, i:Int, j:Int):Int {
        return a * 2 + b * 3 + c * 5 + d * 7 + e * 11 + f * 13 + g * 17 + h * 19 + i * 23 + j * 29;
    }

    static var okAll = true;

    static function checkI(label:String, got:Int, want:Int):Void {
        var pass = got == want;
        if (!pass) okAll = false;
        Sys.println(label + ": got=" + got + " want=" + want + (pass ? "  OK" : "  WRONG"));
    }

    static function checkD(label:String, got:Float, want:Float):Void {
        var pass = Math.abs(got - want) < 1e-9;
        if (!pass) okAll = false;
        Sys.println(label + ": got=" + got + " want=" + want + (pass ? "  OK" : "  WRONG"));
    }

    static function main() {
        var ten = Sys.getEnv("CB2_TEN") != null;

        // --- oracle: call each function directly from Haxe -------------------
        var w6 = f6(100, 101, 102, 103, 104, 105);
        var w7 = f7(100, 101, 102, 103, 104, 105, 106);
        var w8 = f8(100, 101, 102, 103, 104, 105, 106, 107);
        var w9 = f9(100, 101, 102, 103, 104, 105, 106, 107, 108);
        var wmix = fmix9(100, 101.5, 102, 103.5, 104, 105.5, 106, 107.5, 108);
        var w10 = f10(100, 101, 102, 103, 104, 105, 106, 107, 108, 109);
        Sys.println("oracle f6=" + w6 + " f7=" + w7 + " f8=" + w8 + " f9=" + w9
            + " fmix9=" + wmix + " f10=" + w10);

        // --- hand the closures to the native library -------------------------
        nStore6(f6);
        nStore7(f7);
        nStore8(f8);
        nStore9(f9);
        nStoreMix(fmix9);
        if (ten) nStore10(f10);

        // --- immediate callbacks (no GC pressure yet) ------------------------
        Sys.println("-- immediate --");
        checkI("imm f6", nInvokeI(0), w6);
        checkI("imm f7", nInvokeI(1), w7);
        checkI("imm f8", nInvokeI(2), w8);
        checkI("imm f9", nInvokeI(3), w9);
        checkD("imm fmix9", nInvokeD(4), wmix);
        if (ten) checkI("imm f10", nInvokeI(5), w10);

        // --- churn the heap so the collector has a reason to run -------------
        var sink = 0;
        for (i in 0...200000) {
            var a = [i, i + 1, i + 2];
            sink += a[2];
            var s = "s" + i;
            if (s.length == 0) sink++;
        }
        Sys.println("churn sink: " + sink);

        // --- callbacks again, after churn ------------------------------------
        Sys.println("-- after churn --");
        checkI("gc  f6", nInvokeI(0), w6);
        checkI("gc  f7", nInvokeI(1), w7);
        checkI("gc  f8", nInvokeI(2), w8);
        checkI("gc  f9", nInvokeI(3), w9);
        checkD("gc  fmix9", nInvokeD(4), wmix);
        if (ten) checkI("gc  f10", nInvokeI(5), w10);

        Sys.println(okAll ? "CB2 TEST: PASS" : "CB2 TEST: FAIL");
    }
}
