// Invariants, independent loop-carried phis, a pointer and a float must all
// survive the same snapshot. The callee catches its own throw; run has no
// active trap at the transfer point.
class TestRetierSnapshot {
    static var victim:Dynamic = null;
    static var plain = Sys.getEnv("ASH_RETIER_TEST_PLAIN") == "1";
    static function step():Dynamic {
        if (plain) return null;
        try { return victim.field; } catch (e:Dynamic) { return null; }
    }
    static function main() {
        // A closure entry is registered with the tiering broker in --mode
        // jit, unlike a direct callee only compiled transitively into main.
        var callbacks:Array<Void->Bool> = [run];
        if (callbacks[0]()) throw "corrupted re-tier return";
    }
    static function run():Bool {
        var n = 3000000;
        var box = { total: 0 };
        var weight = 0.125;
        var sum = 0.0;
        var a = 1, b = 2;
        var iters = 0, nulls = 0, back = 0, prev = -1;
        for (i in 0...n) {
            iters++;
            if (i <= prev) back++;
            prev = i;
            if (step() == null) nulls++;
            box.total++;
            sum += weight;
            var tmp = a;
            a = b;
            b = tmp;
        }
        Sys.println('iters=$iters nulls=$nulls back=$back last=$prev box=${box.total} sum=$sum pair=$a,$b');
        // Exercise the widened compiled-entry return ABI, including zero.
        return back != 0;
    }
}
