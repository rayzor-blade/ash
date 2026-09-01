// CASE #14 -- a FOREIGN native thread calls hl_dyn_call on a Haxe closure.
//
//   Haxe -> cb14test.store(triple)     native keeps the raw vclosure*, no root
//   ... heap churn ...
//   phase 1 (serial)     : native spawns a pthread, that thread calls
//                          hl_dyn_call(triple, i) for i in 1..N, then joins.
//   phase 2 (concurrent) : native spawns a pthread and RETURNS; Haxe keeps
//                          allocating while the foreign thread calls back.
//
//   triple(x) == x*3 + 1
//   sum over i in 1..N of (3i+1) = 3*N*(N+1)/2 + N
//   N = 8  ->  3*36 + 8 = 116
//
// Env: CB14_ROOT=slot|object, CB14_REGISTER=1, CB14_HLP=1, CB14_VERBOSE=1,
//      CB14_PHASE=serial|concurrent (default: both)

class Cb14 {
    @:hlNative("cb14test", "store")
    static function nativeStore(f:Int->Int):Void {}

    @:hlNative("cb14test", "run_serial")
    static function nativeRunSerial(n:Int):Int { return 0; }

    @:hlNative("cb14test", "spawn")
    static function nativeSpawn(n:Int):Void {}

    @:hlNative("cb14test", "join")
    static function nativeJoin():Int { return 0; }

    @:hlNative("cb14test", "stat")
    static function nativeStat(which:Int):Int { return 0; }

    static var N = 8;

    static function triple(x:Int):Int {
        return x * 3 + 1;
    }

    static function churn(rounds:Int):Int {
        var sink = 0;
        for (i in 0...rounds) {
            var a = [i, i + 1, i + 2];
            sink += a[2];
            var s = "s" + i;
            if (s.length == 0) sink++;
        }
        return sink;
    }

    static function expected():Int {
        var e = 0;
        for (i in 1...(N + 1)) e += i * 3 + 1;
        return e;
    }

    static function report(label:String, got:Int) {
        var want = expected();
        Sys.println(label + ": sum=" + got + "  expect=" + want
            + "  ok=" + nativeStat(0) + " null=" + nativeStat(1)
            + " wrong=" + nativeStat(2) + " funChanged=" + nativeStat(3));
        if (nativeStat(2) > 0)
            Sys.println(label + ": first wrong at i=" + nativeStat(4) + " value=" + nativeStat(5));
        Sys.println(label + ": " + (got == want ? "PASS" : "FAIL"));
    }

    static function main() {
        var phase = Sys.getEnv("CB14_PHASE");
        var scale = Sys.getEnv("CB14_CHURN");
        var div = (scale == null) ? 1 : Std.parseInt(scale);
        if (div == null || div < 1) div = 1;
        Sys.println("cb14: foreign-thread hl_dyn_call, N=" + N + " expect sum=" + expected());

        // 1. hand the closure over; native keeps the raw pointer
        nativeStore(triple);

        // 2. churn so a collection has reason to run before any callback
        Sys.println("churn A sink: " + churn(Std.int(50000 / div)));

        // 3a. SERIAL: Haxe parked in native while the foreign thread calls back
        if (phase == null || phase == "serial") {
            var s = nativeRunSerial(N);
            report("serial", s);
        }

        // 3b. CONCURRENT: foreign thread calls back while Haxe allocates
        if (phase == null || phase == "concurrent") {
            nativeSpawn(N);
            Sys.println("churn B sink: " + churn(Std.int(120000 / div)));
            var c = nativeJoin();
            report("concurrent", c);
        }

        Sys.println("cb14: done");
    }
}
