// CASE #0 (control): static function, one Int argument, called back from a
// native library through hl_dyn_call.
//
//   triple(14) == 14 * 3 + 1 == 43
//
// CB0_CHURN=<n> scales the allocation loop between store() and invoke()
// (default 200000). Use a small value with ASH_GC_STRESS=1, which collects on
// every Nth allocation and is otherwise unbearably slow.
class Cb0 {
    @:hlNative("cb0", "store")
    static function nativeStore(f:Int->Int):Void {}

    @:hlNative("cb0", "invoke")
    static function nativeInvoke(x:Int):Int { return 0; }

    static function triple(x:Int):Int {
        return x * 3 + 1;
    }

    static function main() {
        Sys.println("case0: static fn, one Int arg");

        // 1. hand the closure to the native library; it keeps the raw pointer
        //    in malloc'd memory and registers no GC root.
        nativeStore(triple);

        // 2. churn the heap so the collector has a reason to run between
        //    storing the closure and calling it.
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

        // 3. native code calls the stored closure through hl_dyn_call.
        var r = nativeInvoke(14);
        Sys.println("invoke(14) = " + r + "  (expect 43)");

        // 4. sanity: the same closure called directly from Haxe.
        var direct = triple(14);
        Sys.println("direct triple(14) = " + direct + "  (expect 43)");

        Sys.println((r == 43 && direct == 43) ? "CASE0: PASS" : "CASE0: FAIL");
    }
}
