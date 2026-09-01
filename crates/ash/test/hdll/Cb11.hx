// CASE #11: a closure held only by native code across an explicit gc_major.
//
// The closure is built inside a @:noinline helper and handed straight to the
// native library. When that helper returns, the Haxe program keeps NO
// reference to it: no local, no field, no static, no container. The only
// remaining reference in the process is the raw vclosure* inside the HDLL's
// malloc'd struct.
//
// Then the program asks for a collection by name -- hl.Gc.major() -- and only
// afterwards does the native side call it back through hl_dyn_call.
//
// CB11_KIND=static (default) -> a static function closure   , invoke(14) = 43
// CB11_KIND=bound            -> a bound instance closure    , invoke(14) = 50
// CB11_KIND=lambda           -> a capturing local closure   , invoke(14) = 50
//
// Those answers are plain Haxe arithmetic; whatever the GC does, they are the
// only correct outputs.
class Cb11Recv {
    public var base:Int;
    public function new(b:Int) { base = b; }
    public function apply(x:Int):Int { return x * 3 + 1 + base; }
}

class Cb11 {
    @:hlNative("cb11test", "store")
    static function nativeStore(f:Int->Int):Void {}

    @:hlNative("cb11test", "invoke")
    static function nativeInvoke(x:Int):Int { return 0; }

    // Control channel: an object dropped at the same instant as the closure,
    // held only by the same malloc'd struct. Its fate answers "does this
    // region get reclaimed at all?"
    @:hlNative("cb11test", "store_ctl")
    static function nativeStoreCtl(o:Dynamic):Void {}

    // Rooting probe: GC memory allocated by native code, reachable only from
    // a C static. Its fate is the control for the whole experiment.
    @:hlNative("cb11test", "probe_alloc")
    static function nativeProbeAlloc():Void {}

    @:hlNative("cb11test", "probe_check")
    static function nativeProbeCheck():Void {}

    static function triple(x:Int):Int {
        return x * 3 + 1;
    }

    // Burn `n` dead allocations, so the block the closure lands in can be
    // made to contain nothing but garbage. Ash's sweep is block-granular
    // (plus line recycling), so a closure sharing a block with a live object
    // is retained no matter what the marker decided -- that would let this
    // test pass for the wrong reason.
    @:noinline static function burn(n:Int):Int {
        var s = 0;
        for (i in 0...n) {
            var a = [i, i + 1, i + 2, i + 3];
            s += a[3];
        }
        return s;
    }

    // Build the closure and hand it off. Nothing survives this frame.
    @:noinline static function handOff(kind:String):Void {
        var isolate = Sys.getEnv("CB11_ISOLATE") != null;
        // fill out the current block so the closure starts a fresh one
        if (isolate && burn(600) == -1) Sys.println("unreachable");
        switch (kind) {
            case "bound":
                nativeStore(new Cb11Recv(7).apply);
            case "lambda":
                var cap = 7;
                nativeStore(function(x:Int):Int { return x * 3 + 1 + cap; });
            default:
                nativeStore(triple);
        }
        nativeStoreCtl(new Cb11Recv(99));
        // fill the REST of the closure's block (and several more) with
        // garbage, so that block holds nothing live but the closure itself
        if (isolate && burn(8000) == -1) Sys.println("unreachable");
    }

    // Overwrite the machine stack the handoff used, so a conservative stack
    // scan cannot keep the closure alive by accident. Without this the test
    // would silently pass for the wrong reason.
    @:noinline static function scrub(depth:Int):Int {
        if (depth <= 0) return 0;
        var pad = [depth, depth + 1, depth + 2, depth + 3, depth + 4, depth + 5, depth + 6, depth + 7];
        var s = 0;
        for (v in pad) s += v;
        return s + scrub(depth - 1);
    }

    static function main() {
        var kind = Sys.getEnv("CB11_KIND");
        if (kind == null) kind = "static";
        var expect = (kind == "bound" || kind == "lambda") ? 50 : 43;
        Sys.println("kind: " + kind + "  expect invoke(14) = " + expect);

        // 1. hand the closure to native code; no Haxe reference remains
        handOff(kind);
        if (Sys.getEnv("CB11_PROBE") != null) nativeProbeAlloc();

        // 2. scrub the stack frame the handoff left behind
        var scrubbed = scrub(120);
        if (scrubbed == -1) Sys.println("unreachable " + scrubbed);

        // 3. ask for a collection BY NAME, twice, with only C holding it
        Sys.println("gc: hl.Gc.major() x2 with only native code holding the closure");
        hl.Gc.major();
        hl.Gc.major();

        // 3b. CB11_REUSE=1: after the collection, allocate hard so that any
        //     memory the collector freed is handed back out and overwritten.
        //     Without this, block-level reclamation can leave a dead closure
        //     byte-intact and the test passes for the wrong reason.
        if (Sys.getEnv("CB11_REUSE") != null) {
            var sink = 0;
            for (i in 0...300000) {
                var a = [i, i + 1, i + 2, i + 3];
                sink += a[3];
                var s = "r" + i;
                if (s.length == 0) sink++;
            }
            Sys.println("reuse churn sink: " + sink);
            hl.Gc.major();
        }

        if (Sys.getEnv("CB11_PROBE") != null) nativeProbeCheck();

        // 4. native code calls it back through hl_dyn_call
        var r = nativeInvoke(14);
        Sys.println("invoke(14) = " + r + "  (expect " + expect + ")");
        Sys.println(r == expect ? "CB11: PASS" : "CB11: FAIL");
    }
}
