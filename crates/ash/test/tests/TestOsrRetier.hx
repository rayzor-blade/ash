// A loop long enough to be handed to a JIT tier mid-flight, calling a function
// that throws and catches on every iteration.
//
// The throw is what makes an iteration slow enough for the tier-2 compile to
// land while the loop is still running, which is when the hand-off happens.
// Every counter here is loop-carried, so a hand-off that does not bring the
// live registers with it shows up as a wrong total rather than a crash.
class TestOsrRetier {
    static var victim:Dynamic = null;
    static function step():Dynamic {
        try { return victim.field; } catch (e:Dynamic) { return null; }
    }
    static function main() {
        var iters = 0, nulls = 0, back = 0, prev = -1;
        for (i in 0...200000) {
            iters++;
            if (i <= prev) back++;
            prev = i;
            if (step() == null) nulls++;
        }
        Sys.println('iters=$iters nulls=$nulls back=$back last=$prev');
    }
}
