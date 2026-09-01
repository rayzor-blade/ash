// CASE #9: an ANONYMOUS LAMBDA CAPTURING LOCALS, handed to a native library
// and later invoked from there through hl_dyn_call.
//
// Why this case matters: it is not enough for the vclosure to survive. A
// capturing lambda in HashLink is a closure WITH A VALUE -- vclosure.hasValue
// is 1 and vclosure.value points at the captured environment (the capture
// object / ref cells holding base, tag, counter, log). If the environment is
// collected but the vclosure is not, the call still "succeeds" and returns a
// plausible-looking-but-wrong number: exactly the "corrupt result" the user
// reported.
//
//   Haxe -> handOff()             builds the lambda inside a scope that then
//                                 ENDS, and passes it straight to native code;
//                                 no Haxe variable keeps a reference
//   ... heavy allocation in Haxe ...
//   Haxe -> cb9test.invoke(5)     native: hl_dyn_call(c, {5}, 1)
//   ... heavy allocation in Haxe ...
//   Haxe -> cb9test.invoke(3)     native: hl_dyn_call(c, {3}, 1)
//
// The lambda captures:
//   base    : Int     (immutable capture)
//   tag     : String  (heap object, only reachable through the environment)
//   counter : Int     (MUTABLE capture -> ref cell; proves state persists)
//   log     : Array   (heap object, mutated by the lambda)
//
// Required results by Haxe semantics (base=7, tag.length=6, log starts len 3):
//   invoke(5): counter=1, log.length=4 -> 7*1000 + 5*10 + 1 + 4 + 6 = 7061
//   invoke(3): counter=2, log.length=5 -> 7*1000 + 3*10 + 2 + 5 + 6 = 7043
//
// Env switches:
//   CB9_KEEP=1   Haxe also keeps a static reference to the lambda, so it is
//                rooted the ordinary way. Default (DROP) leaves the malloc'd
//                C struct as the only holder -- the shape a missing GC root
//                exposes.
//   CB9_WARM=1   make the lambda body hot before handing it over, so a tiering
//                engine can put a real compiled address in vclosure.fun.
// Native side: CB9_HLP=1, CB9_ROOT=object|slot|value, see cb9test.c.

class Cb9 {
    @:hlNative("cb9test", "store")
    static function nativeStore(f:Int->Int):Void {}

    @:hlNative("cb9test", "invoke")
    static function nativeInvoke(x:Int):Int { return 0; }

    @:hlNative("cb9test", "peek")
    static function nativePeek():Void {}

    @:hlNative("cb9test", "dump")
    static function nativeDump(path:hl.Bytes):Void {}

    @:hlNative("cb9test", "store_plain")
    static function nativeStorePlain(f:Int->Int):Void {}

    @:hlNative("cb9test", "invoke_plain")
    static function nativeInvokePlain(x:Int):Int { return 0; }

    // Control arm: a plain static function, captured nothing.
    static function plainFn(x:Int):Int { return x * 2 + 1; }

    // only populated under CB9_KEEP=1
    static var kept:Int->Int = null;

    // Builds an anonymous lambda over four locals and returns it. When this
    // function returns, the ONLY reference to base/tag/counter/log is the
    // captured environment hanging off the vclosure.
    static function makeLambda():Int->Int {
        var base = 7;
        var tag = "lambda";
        var counter = 0;
        var log = [base, base + 1, base + 2];
        return function(x:Int):Int {
            counter++;
            log.push(x);
            return base * 1000 + x * 10 + counter + log.length + tag.length;
        };
    }

    // Hand the lambda to native code from a frame that then goes away, so no
    // live Haxe register still points at it.
    static function handOff():Void {
        nativeStore(makeLambda());
        // same call site, same GC timeline: a non-capturing static closure
        nativeStorePlain(plainFn);
    }

    // Overwrite stale machine-stack / interpreter-register slots that might
    // still hold a conservatively-scanned copy of the closure pointer left
    // behind by handOff(). Without this, an unrooted object can survive purely
    // because a dead stack slot still points at it.
    static function scrub(d:Int):Int {
        if (d <= 0) return 0;
        var a0 = d * 7;  var a1 = d * 11; var a2 = d * 13; var a3 = d * 17;
        var a4 = d * 19; var a5 = d * 23; var a6 = d * 29; var a7 = d * 31;
        var r = scrub(d - 1);
        return r + a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7;
    }

    static var churnN:Int = 200000;

    static function churn(tag:String):Void {
        var sink = 0;
        for (i in 0...churnN) {
            var a = [i, i + 1, i + 2];
            sink += a[2];
            var s = "s" + i;
            if (s.length == 0) sink++;
        }
        Sys.println("churn " + tag + " sink: " + sink);
    }

    static function main() {
        var churnEnv = Sys.getEnv("CB9_CHURN");
        if (churnEnv != null) {
            var n = Std.parseInt(churnEnv);
            if (n != null) churnN = n;
        }

        var keep = Sys.getEnv("CB9_KEEP") != null;
        Sys.println(keep ? "mode: KEEP (Haxe holds a ref)" : "mode: DROP (native struct is the only holder)");

        if (Sys.getEnv("CB9_WARM") != null) {
            var acc = 0;
            var w = makeLambda();
            for (i in 0...300000) acc += w(i);
            Sys.println("warm acc: " + acc);
        }

        // CB9_PRECHURN=1 fills the heap with dead objects BEFORE the lambda is
        // built, so the closure and its captured environment land in a fresh,
        // otherwise-dead region rather than next to the startup constants.
        if (Sys.getEnv("CB9_PRECHURN") != null) churn("0");

        if (keep) {
            kept = makeLambda();
            nativeStore(kept);
            nativeStorePlain(plainFn);
        } else {
            handOff();
        }

        if (Sys.getEnv("CB9_SCRUB") != null) Sys.println("scrub: " + scrub(900));

        churn("1");
        nativePeek();
        var dumpPath = Sys.getEnv("CB9_DUMPFILE");
        if (dumpPath != null) nativeDump(@:privateAccess dumpPath.toUtf8());

        var r1 = nativeInvoke(5);
        Sys.println("invoke(5) = " + r1 + "  (expect 7061)");

        churn("2");

        var r2 = nativeInvoke(3);
        Sys.println("invoke(3) = " + r2 + "  (expect 7043)");

        var p = nativeInvokePlain(6);
        Sys.println("invokePlain(6) = " + p + "  (expect 13)");

        var ok = r1 == 7061 && r2 == 7043 && p == 13;

        // CB9_ROUNDS=N: keep churning and re-invoking. The k-th invocation of
        // the lambda with argument x must return
        //     base*1000 + x*10 + k + (3+k) + tag.length = 7009 + 10*x + 2*k
        // so every extra round re-checks the captured counter, array and
        // string after another wave of collections.
        var roundsEnv = Sys.getEnv("CB9_ROUNDS");
        if (roundsEnv != null) {
            var rounds = Std.parseInt(roundsEnv);
            if (rounds == null) rounds = 0;
            var k = 2;
            for (i in 0...rounds) {
                churn("r" + i);
                k++;
                var x = 1 + (i % 4);
                var got = nativeInvoke(x);
                var want = 7009 + 10 * x + 2 * k;
                Sys.println("round " + i + ": invoke(" + x + ") = " + got + "  (expect " + want + ")");
                if (got != want) ok = false;
            }
        }

        Sys.println(ok ? "CB9 TEST: PASS" : "CB9 TEST: FAIL");
    }
}
