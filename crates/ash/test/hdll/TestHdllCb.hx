// Minimal end-to-end hl_dyn_call callback test.
//
//   Haxe -> cbtest.store(closure)   native keeps the raw vclosure* in a
//                                   malloc'd struct, registers no GC root
//   ... heavy allocation in Haxe ...
//   Haxe -> cbtest.invoke(14)       native calls hl_dyn_call(c, args, 1)
//
//   triple(14) == 3*14 + 1 == 43   <- the only correct answer
//
// CBTEST_BOUND=1 stores a BOUND instance closure whose receiver is allocated
// inline, so the malloc'd C struct is the only remaining reference to it.
// That is the shape that would expose a missing GC root; combine with
// ASH_GC_STRESS=1 / ASH_GC_NO_RECLAIM=1.
class Adder {
    public var base:Int;
    public function new(b:Int) { base = b; }
    public function apply(x:Int):Int { return x * 3 + 1 + base; }
}

class TestHdllCb {
    @:hlNative("cbtest", "store")
    static function nativeStore(f:Int->Int):Void {}

    @:hlNative("cbtest", "invoke")
    static function nativeInvoke(x:Int):Int { return 0; }

    static function triple(x:Int):Int {
        return x * 3 + 1;
    }

    static function main() {
        // 1. hand the closure to the native library; it keeps the raw pointer
        if (Sys.getEnv("CBTEST_BOUND") != null) {
            Sys.println("mode: bound instance closure");
            nativeStore(new Adder(0).apply);
        } else {
            Sys.println("mode: static closure");
            nativeStore(triple);
        }

        // 2. churn the heap so the collector has a reason to run between
        //    storing the closure and calling it
        var sink = 0;
        for (i in 0...200000) {
            var a = [i, i + 1, i + 2];
            sink += a[2];
            var s = "s" + i;
            if (s.length == 0) sink++;
        }
        Sys.println("churn sink: " + sink);

        // 3. native code calls the stored closure through hl_dyn_call
        var r = nativeInvoke(14);
        Sys.println("invoke(14) = " + r + "  (expect 43)");
        Sys.println(r == 43 ? "CB TEST: PASS" : "CB TEST: FAIL");
    }
}
