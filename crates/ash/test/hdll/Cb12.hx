// CASE #12 -- two closures stored natively, called INTERLEAVED.
//
// The point of two closures is that a rooting bug becomes PARTIAL and
// therefore visible as data corruption rather than as a clean crash: the
// collector can reclaim one while the other survives.
//
// Every shape below is arranged to produce the SAME expected numbers, so the
// shape switch changes only the GC exposure, never the correct answer:
//     A(x) = x*3 + 1        B(x) = x*7 + 35
//
//   CB12_SHAPE unset  : A = STATIC closure        B = capturing lambda
//                       (calibration on the single-closure harness shows the
//                        static closure is the fragile shape)
//   CB12_SHAPE=bound  : A = bound instance closure, B = capturing lambda
//   CB12_SHAPE=static : A and B are both STATIC closures
//
// setup() hands both to cb12test.hdll and returns, so its frame dies and the
// only remaining reference to either closure is the raw vclosure* in the
// library's malloc'd struct.
//
// Correct answers, from Haxe semantics alone:
//   callA(14) = 43     callB(14) = 133
//   callB(15) = 140    callA(15) = 46
//   pump(3): A(1)=4 B(1)=42 A(2)=7 B(2)=49 A(3)=10 B(3)=56  -> sum 168
//   distinct = 1

class Mul {
    public var k:Int;
    public function new(k:Int) { this.k = k; }
    public function apply(x:Int):Int { return x * k + 1; }
}

class Cb12 {
    @:hlNative("cb12test", "store_a")
    static function storeA(f:Int->Int):Void {}

    @:hlNative("cb12test", "store_b")
    static function storeB(f:Int->Int):Void {}

    @:hlNative("cb12test", "call_a")
    static function callA(x:Int):Int { return 0; }

    @:hlNative("cb12test", "call_b")
    static function callB(x:Int):Int { return 0; }

    @:hlNative("cb12test", "pump")
    static function pump(n:Int):Int { return 0; }

    @:hlNative("cb12test", "distinct")
    static function distinct():Int { return 0; }

    static function triple(x:Int):Int { return x * 3 + 1; }
    static function septuple(x:Int):Int { return x * 7 + 35; }

    static function makeCapturing():Int->Int {
        var cap = 5;
        var arr = [10, 20, 30];
        return function(x:Int):Int { return x * 7 + cap + arr[2]; };
    }

    // stores happen in a frame that dies, so no Haxe-side reference survives.
    //
    // CB12_SEP (default 40000) is churn inserted BETWEEN the two stores. It
    // matters: allocated back to back the two closures land in the same GC
    // block, and ash reclaims whole blocks, so either both survive or both
    // die -- the interleaved case degenerates into the single-closure case.
    // Separating them is what makes "one collected, one alive" reachable.
    static function setup():Void {
        var shape = Sys.getEnv("CB12_SHAPE");
        var sepEnv = Sys.getEnv("CB12_SEP");
        var sep = sepEnv == null ? 40000 : Std.parseInt(sepEnv);
        if (sep == null) sep = 40000;
        Sys.println("separation churn between stores: " + sep);
        if (shape == "bound") {
            Sys.println("shape: A=bound instance closure  B=capturing lambda");
            storeA(new Mul(3).apply);
            if (churn(sep) == -1) Sys.println("unreachable");
            storeB(makeCapturing());
        } else if (shape == "static") {
            Sys.println("shape: A=static closure  B=static closure");
            storeA(triple);
            if (churn(sep) == -1) Sys.println("unreachable");
            storeB(septuple);
        } else {
            Sys.println("shape: A=static closure  B=capturing lambda");
            storeA(triple);
            if (churn(sep) == -1) Sys.println("unreachable");
            storeB(makeCapturing());
        }
    }

    static function churn(n:Int):Int {
        var sink = 0;
        for (i in 0...n) {
            var a = [i, i + 1, i + 2];
            sink += a[2];
            var s = "c" + i;
            if (s.length == 0) sink++;
            var m = new Mul(i);
            sink += m.k & 1;
        }
        return sink;
    }

    static function check(label:String, got:Int, want:Int):Bool {
        var ok = got == want;
        Sys.println(label + " = " + got + "  (expect " + want + ")  " + (ok ? "ok" : "WRONG"));
        return ok;
    }

    static function main() {
        setup();

        var sink = churn(150000);
        if (sink == -1) Sys.println("unreachable");

        var pass = true;

        // interleaved, Haxe-driven, with churn between rounds
        pass = check("callA(14)", callA(14), 43) && pass;
        pass = check("callB(14)", callB(14), 133) && pass;

        sink += churn(150000);
        if (sink == -1) Sys.println("unreachable");

        pass = check("callB(15)", callB(15), 140) && pass;
        pass = check("callA(15)", callA(15), 46) && pass;

        sink += churn(150000);
        if (sink == -1) Sys.println("unreachable");

        // interleaved inside ONE native call (the process_events shape)
        pass = check("pump(3)", pump(3), 168) && pass;

        pass = check("distinct", distinct(), 1) && pass;

        Sys.println("churn sink: " + sink);
        Sys.println(pass ? "CB12: PASS" : "CB12: FAIL");
    }
}
