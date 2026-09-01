// CASE #10 -- a closure stored by native code, then called after heavy
// allocation.  The point is to force a collection BETWEEN store and call while
// the malloc'd C struct is the ONLY remaining reference to the closure.
//
//   store(0, triple)                 static closure   Int -> Int
//   store(1, new Adder(7).apply)     bound instance closure (receiver alive
//                                    only through the closure's `value`)
//   store(2, capturing lambda)       captured env alive only through `value`
//
//   ... every Haxe reference dropped, deep frame scrub, 200k allocations ...
//
//   invoke(0, 14) == 14*3 + 1          == 43
//   invoke(1, 14) == 14*3 + 1 + 7      == 50
//   invoke(2, 14) == 14*5 + 100        == 170
//
// Those three numbers are pure Haxe arithmetic; nothing about GC, engines or
// hl_dyn_call can legitimately change them.

class Adder {
    public var base:Int;
    public function new(b:Int) { base = b; }
    public function apply(x:Int):Int { return x * 3 + 1 + base; }
}

class Cb10 {
    @:hlNative("cb10test", "store")
    static function nativeStore(idx:Int, f:Int->Int):Void {}

    @:hlNative("cb10test", "invoke")
    static function nativeInvoke(idx:Int, x:Int):Int { return 0; }

    @:hlNative("cb10test", "peek")
    static function nativePeek(idx:Int):Void {}

    @:hlNative("cb10test", "collect")
    static function nativeCollect():Void {}

    static function triple(x:Int):Int {
        return x * 3 + 1;
    }

    // Each store happens inside its own @:noinline frame that returns
    // immediately, so the closure (and, for slots 1 and 2, the object it
    // points at) is unreachable from Haxe the moment the helper returns.
    @:noinline static function storeStatic():Void {
        nativeStore(0, triple);
    }

    @:noinline static function storeBound():Void {
        nativeStore(1, new Adder(7).apply);
    }

    @:noinline static function storeCapture():Void {
        var k = 100;
        nativeStore(2, function(x:Int):Int { return x * 5 + k; });
    }

    // Overwrite the frames the three helpers used, so a conservative stack
    // scan cannot keep the closures alive off stale words.
    @:noinline static function scrub(depth:Int):Int {
        if (depth <= 0) return 0;
        var a0 = depth * 7 + 1;
        var a1 = a0 ^ 0x5a5a;
        var a2 = a1 + depth;
        var a3 = a2 * 3;
        var a4 = a3 - depth;
        var a5 = a4 ^ 0x1234;
        var a6 = a5 + a0;
        var a7 = a6 - a1;
        return (a7 & 0xff) + scrub(depth - 1);
    }

    @:noinline static function churn(n:Int):Int {
        var sink = 0;
        for (i in 0...n) {
            var a = [i, i + 1, i + 2];
            sink += a[2];
            var s = "s" + i;
            if (s.length == 0) sink++;
            var o = new Adder(i);
            sink += o.base & 1;
        }
        return sink;
    }

    static function check(name:String, got:Int, want:Int):Bool {
        var ok = got == want;
        Sys.println("  " + name + ": got " + got + " want " + want + (ok ? "  OK" : "  WRONG"));
        return ok;
    }

    static function main() {
        Sys.println("== CASE 10: stored closure called after heavy allocation ==");

        storeStatic();
        storeBound();
        storeCapture();

        var scrubbed = scrub(64);
        var sink = churn(200000);
        Sys.println("churn sink: " + sink + " scrub: " + scrubbed);

        // a second scrub + churn, so at least one collection is very likely
        // even without ASH_GC_STRESS
        scrubbed += scrub(64);
        sink += churn(100000);

        // optional: force a collection from inside a native call
        nativeCollect();

        nativePeek(0);
        nativePeek(1);
        nativePeek(2);

        Sys.println("results:");
        var ok = true;
        ok = check("static  invoke(0,14)", nativeInvoke(0, 14), 43) && ok;
        ok = check("bound   invoke(1,14)", nativeInvoke(1, 14), 50) && ok;
        ok = check("capture invoke(2,14)", nativeInvoke(2, 14), 170) && ok;

        Sys.println(ok ? "CB10: PASS" : "CB10: FAIL");
    }
}
