// CASE 15 -- closure passed, native returns, Haxe drops its reference,
//            native calls it later.
//
//   makeAndHandOff()   builds a capturing closure and hands it to the native
//                      library, which stores the raw vclosure* in malloc'd
//                      memory (never scanned by the collector) and returns.
//                      When makeAndHandOff() returns, Haxe holds NO reference
//                      to the closure or to its captured environment.
//   clobberStack()     deep recursion that overwrites the stack region the
//                      closure pointer used to live in, so a stale
//                      conservative root cannot keep it alive by accident.
//   churn()            heavy allocation, so the collector has a reason to run.
//   nativeInvoke(21)   the native library calls it through hl_dyn_call.
//
// Haxe semantics fix the answer exactly:
//   f(x) == x * env.mul + base  with env.mul == 3 and base == 7
//   f(21) == 21*3 + 7 == 70
// and the callback returns -999 if its captured object was corrupted.
//
// CB15_STATIC=1  store a non-capturing static-function closure instead
//                (same arithmetic, same expected 70).

class Cb15Env {
    public var mul:Int;
    public var tag:Int;
    public function new(m:Int) { mul = m; tag = 0xC0FFEE; }
}

class Cb15 {
    @:hlNative("cb15test", "store")
    static function nativeStore(f:Int->Int):Void {}

    @:hlNative("cb15test", "invoke")
    static function nativeInvoke(x:Int):Int { return 0; }

    @:hlNative("cb15test", "corrupt")
    static function nativeCorrupt():Int { return 0; }

    // force a full collection from the native side, with the closure
    // reachable only from malloc'd C memory
    @:hlNative("cb15test", "gcmajor")
    static function nativeGcMajor():Void {}

    @:hlNative("cb15test", "storeobj")
    static function nativeStoreObj(o:Dynamic):Void {}

    // hand a plain object across the same boundary, then drop it
    static function handOffObject():Void {
        var o = new Cb15Env(9);
        nativeStoreObj(o);
    }

    // non-capturing variant, same arithmetic
    static function staticCb(x:Int):Int {
        return x * 3 + 7;
    }

    // Build the closure and hand it off. Everything it touches is local, so
    // when this returns the malloc'd C struct is the only thing that still
    // points at the closure and at its captured environment.
    static function makeAndHandOff():Void {
        var env = new Cb15Env(3);
        var base = 7;
        var cb = function(x:Int):Int {
            if (env.tag != 0xC0FFEE) return -999;
            return x * env.mul + base;
        };
        nativeStore(cb);
    }

    // Overwrite the stack so a stale slot cannot act as a conservative root.
    static function clobberStack(depth:Int):Int {
        if (depth <= 0) return 0;
        var pad = [depth, depth ^ 0x5A5A, depth + 3, depth * 7, depth - 11,
                   depth * 13, depth ^ 0x33, depth + 101];
        var acc = pad[0] + pad[7];
        return acc + clobberStack(depth - 1);
    }

    static function churn(n:Int):Int {
        var sink = 0;
        for (i in 0...n) {
            var a = [i, i + 1, i + 2];
            sink += a[2];
            var s = "c15-" + i;
            if (s.length == 0) sink++;
        }
        return sink;
    }

    static function main() {
        // Pre-churn BEFORE handing the closure over. ash reclaims whole
        // blocks only, so a closure allocated at the very start of the heap
        // is retained by the bootstrap objects sharing its block. Filling and
        // abandoning many blocks first puts the closure in a fresh block whose
        // other occupants are all short-lived -- the block CAN become empty.
        var pre = churn(150000);
        Sys.println("prechurn=" + pre);

        if (Sys.getEnv("CB15_STATIC") != null) {
            Sys.println("mode: static (non-capturing) closure");
            nativeStore(staticCb);
        } else {
            Sys.println("mode: capturing closure (env.mul=3, base=7)");
            makeAndHandOff();
        }
        handOffObject();

        // Haxe now holds nothing. Clobber the stack, then churn the heap.
        var clob = clobberStack(300);
        var sink = churn(200000);
        var clob2 = clobberStack(300);
        Sys.println("clobber=" + (clob + clob2) + " churn=" + sink);

        // Deterministic: collect while Haxe holds no reference at all.
        nativeGcMajor();
        var sink2 = churn(50000);
        var clob3 = clobberStack(300);
        nativeGcMajor();
        Sys.println("phase2 clobber=" + clob3 + " churn=" + sink2);

        var r = nativeInvoke(21);
        Sys.println("invoke(21) = " + r + "  (expect 70)");
        Sys.println("closure bytes corrupted since store: " + nativeCorrupt());
        Sys.println(r == 70 ? "CB15: PASS" : "CB15: FAIL");
    }
}
