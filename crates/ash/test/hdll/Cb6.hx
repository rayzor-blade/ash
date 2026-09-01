// CASE #6: the callback RETURNS A STRING.
//
//   Haxe -> cb6.store(mkstr)      native keeps the raw vclosure*, no GC root
//   ...heap churn...
//   Haxe -> cb6.invoke(14)        native hl_dyn_call's it and hands the
//                                 returned String back to Haxe
//   Haxe -> cb6.hold_check(1,2)   native calls it twice and checks that the
//                                 FIRST returned String survives the second
//                                 call (the return value is heap-allocated
//                                 inside the callee and is then held only by
//                                 a C stack slot)
//
// Haxe semantics fix the answers:
//   mkstr(14) == "cb6:43"
//   hold_check(1,2): mkstr(1)=="cb6:4" must still read "cb6:4" afterwards -> 1
//
// CB6_BOUND=1 stores a bound instance closure instead of a static one.
class Maker {
    public var tag:String;
    public function new(t:String) { tag = t; }
    public function make(x:Int):String { return tag + ":" + (x * 3 + 1); }
}

class Cb6 {
    @:hlNative("cb6", "store")
    static function nativeStore(f:Int->String):Void {}

    @:hlNative("cb6", "invoke")
    static function nativeInvoke(x:Int):String { return null; }

    @:hlNative("cb6", "hold_check")
    static function nativeHoldCheck(a:Int, b:Int):Int { return 0; }

    @:hlNative("cb6", "stash_ret")
    static function nativeStashRet(x:Int):Void {}

    @:hlNative("cb6", "check_ret")
    static function nativeCheckRet():Int { return 0; }

    // A NEGATIVE argument makes the callee allocate heavily before it returns.
    // hold_check uses that for its second call, so the FIRST returned String --
    // which by then is referenced only from a C stack slot in the hdll -- has
    // to survive a real collection.
    static function mkstr(x:Int):String {
        if (x < 0) {
            var sink = 0;
            for (i in 0...(-x)) {
                var a = [i, i + 1, i + 2];
                sink += a[2];
                var s = "t" + i;
                if (s.length == 0) sink++;
            }
            return "cb6:churn" + (sink == 0 ? "?" : "");
        }
        return "cb6:" + (x * 3 + 1);
    }

    static function main() {
        if (Sys.getEnv("CB6_BOUND") != null) {
            Sys.println("mode: bound instance closure");
            nativeStore(new Maker("cb6").make);
        } else {
            Sys.println("mode: static closure");
            nativeStore(mkstr);
        }

        // churn the heap so the collector has a reason to run between the
        // store and the call
        var cn = Sys.getEnv("CB6_CHURN");
        var n = cn == null ? 200000 : Std.parseInt(cn);
        var sink = 0;
        for (i in 0...n) {
            var a = [i, i + 1, i + 2];
            sink += a[2];
            var s = "s" + i;
            if (s.length == 0) sink++;
        }
        Sys.println("churn sink: " + sink);

        var r = nativeInvoke(14);
        Sys.println('invoke(14) = "' + r + '" len=' + (r == null ? -1 : r.length) + '  (expect "cb6:43" len=6)');
        Sys.println(r == "cb6:43" ? "CB6 RET: PASS" : "CB6 RET: FAIL");

        var hn = Sys.getEnv("CB6_HOLD");
        var hb = hn == null ? -50000 : -Std.parseInt(hn);
        var h = nativeHoldCheck(1, hb);
        Sys.println("hold_check(1," + hb + ") = " + h + "  (expect 1)");
        Sys.println(h == 1 ? "CB6 HOLD: PASS" : "CB6 HOLD: FAIL");

        // The returned String is parked in the hdll's malloc'd struct, we
        // return to Haxe (so no C frame or register holds it any more), churn
        // the heap, and only then does native read it back.
        nativeStashRet(7);            // mkstr(7) == "cb6:22"
        var sink2 = 0;
        for (i in 0...n) {
            var a = [i, i + 1, i + 2];
            sink2 += a[2];
            var s = "u" + i;
            if (s.length == 0) sink2++;
        }
        Sys.println("churn2 sink: " + sink2);
        var k = nativeCheckRet();
        Sys.println("check_ret = " + k + "  (expect 1)");
        Sys.println(k == 1 ? "CB6 STASHRET: PASS" : "CB6 STASHRET: FAIL");
    }
}
