// CASE 13 -- closure called repeatedly from a native loop.
//
//   Haxe -> cb13test.store(closure)   native keeps the raw vclosure*, no root
//   ... heavy allocation in Haxe ...
//   Haxe -> cb13test.pump(N)          native runs N x hl_dyn_call(c,args,1)
//
// The callback allocates on every call, so a collection can land in the middle
// of the native loop. Haxe semantics fix the answer exactly:
//
//   f(x) == x*3 + 1 + 7
//   pump(N) == sum over i in 0...N of (i*3 + 1 + 7)
//   with N = 2000 that is 6013000, with zero bad calls and 2000 completed.
//
// CB13_BOUND=1  store a BOUND instance closure (receiver reachable only from
//               the malloc'd C struct) instead of a static-function closure.
// CB13_N=<n>    change the loop count.
// CB13_DEEP=1   hand the closure over from a nested helper frame that is then
//               popped and clobbered by a recursive scrubber, so no live Haxe
//               frame slot can still be holding the pointer when the collector
//               runs. Distinguishes "correctly rooted" from "accidentally
//               retained by a conservative scan of a still-live frame".
// CB13_NOCHURN=1 skip the pre-pump allocation churn, so the closure is still
//               alive when the native loop starts and the ONLY allocation
//               pressure comes from the callback itself. With a large CB13_N
//               the collection then lands in the MIDDLE of the native loop:
//               the first k calls succeed and call k+1 fails. That is the
//               shape this case is named for.

class Cb13Acc {
    public var base:Int;
    public var calls:Int;
    public function new(b:Int) { base = b; calls = 0; }
    public function apply(x:Int):Int {
        calls++;
        // allocate on every call so the collector can run inside the native loop
        var a = [x, x + 1, x + 2];
        var s = "v" + x;
        if (s.length == 0) return -1;
        return a[0] * 3 + 1 + base;
    }
}

class Cb13 {
    @:hlNative("cb13test", "store")
    static function nativeStore(f:Int->Int):Void {}

    @:hlNative("cb13test", "store_gen")
    static function nativeStoreGen(f:Int->Int):Void {}

    @:hlNative("cb13test", "pump")
    static function nativePump(n:Int):Int { return 0; }

    @:hlNative("cb13test", "badcount")
    static function nativeBad():Int { return 0; }

    @:hlNative("cb13test", "firstbad")
    static function nativeFirstBad():Int { return 0; }

    @:hlNative("cb13test", "completed")
    static function nativeCompleted():Int { return 0; }

    // Allocation engine: called from native between target calls so the
    // collector runs while nothing on the native stack names the target.
    static function generate(x:Int):Int {
        var a = [x, x + 1, x + 2, x + 3];
        var s = "g" + x;
        var t = s + s + s;
        return a[0] + t.length;
    }

    static function tripleStatic(x:Int):Int {
        var a = [x, x + 1, x + 2];
        var s = "w" + x;
        if (s.length == 0) return -1;
        return a[0] * 3 + 1 + 7;
    }

    static function handOff():Void {
        if (Sys.getEnv("CB13_BOUND") != null) {
            nativeStore(new Cb13Acc(7).apply);
        } else {
            nativeStore(tripleStatic);
        }
    }

    // Recursive filler: overwrites whatever stack / register-file storage the
    // popped handOff() frame used, so a stale copy of the closure pointer
    // cannot survive there.
    static function scrub(depth:Int):Int {
        if (depth <= 0) return 0;
        var a = [depth, depth + 1, depth + 2, depth + 3];
        var b = [a[0], a[1], a[2], a[3]];
        var c = "z" + depth;
        var d = c + c;
        var e = [d.length, b[0], b[1], b[2]];
        return e[0] + scrub(depth - 1);
    }

    static function main() {
        var n = 2000;
        var envN = Sys.getEnv("CB13_N");
        if (envN != null) {
            var p = Std.parseInt(envN);
            if (p != null) n = p;
        }

        if (Sys.getEnv("CB13_SCRUB") != null) nativeStoreGen(generate);
        var bound = Sys.getEnv("CB13_BOUND") != null;
        if (Sys.getEnv("CB13_DEEP") != null) {
            Sys.println("mode: " + (bound ? "bound instance closure" : "static closure")
                + " (base=7), handed off from a popped frame");
            handOff();
            var scrubbed = scrub(400);
            if (scrubbed < 0) Sys.println("unreachable " + scrubbed);
        } else if (bound) {
            Sys.println("mode: bound instance closure (base=7)");
            nativeStore(new Cb13Acc(7).apply);
        } else {
            Sys.println("mode: static closure (base=7)");
            nativeStore(tripleStatic);
        }

        // churn between store and pump so the closure is old and unreferenced
        if (Sys.getEnv("CB13_NOCHURN") == null) {
            var sink = 0;
            for (i in 0...100000) {
                var a = [i, i + 1, i + 2];
                sink += a[2];
                var s = "s" + i;
                if (s.length == 0) sink++;
            }
            Sys.println("churn sink: " + sink);
        } else {
            Sys.println("churn: skipped (allocation pressure comes only from the callback)");
        }

        var checkSum = n <= 30000;   // beyond that the sum overflows Int32
        var expected = 0;
        if (checkSum) for (i in 0...n) expected += i * 3 + 1 + 7;

        var got = nativePump(n);
        if (checkSum)
            Sys.println("pump(" + n + ") sum = " + got + "  (expect " + expected + ")");
        else
            Sys.println("pump(" + n + ") sum = " + got + "  (sum check skipped, would overflow Int32)");
        Sys.println("completed = " + nativeCompleted() + "/" + n
            + ", bad = " + nativeBad() + ", firstbad = " + nativeFirstBad());
        var ok = (!checkSum || got == expected) && (nativeBad() == 0) && (nativeCompleted() == n);
        Sys.println(ok ? "CB13: PASS" : "CB13: FAIL");
    }
}
