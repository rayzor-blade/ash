// Same bare method name in several classes, each called directly by a hot
// caller in its own class.
//
// `HLFunction::name` is the Haxe field name, not a qualified one, so all five
// `value` functions below share one symbol -- and in a real program that is
// the common case: 4638 of a game's 9020 function symbols collide, 810 of them
// under the single name `_`. The own-module promotion path used to resolve a
// callee it left as a declaration through that symbol.
//
// This test does NOT reproduce that: measured on the game with ASH_BIND_AUDIT,
// every difference between name and findex resolution was on a unique
// `Fun_<findex>` name, so the ambiguity was never actually mis-resolving. It is
// kept as a regression guard over same-named statics reached through the
// own-module path (`--mode jit` takes it for every promotion), not as a
// reproducer.
class A {
    public static function value():Int return 1;
    public static function run(n:Int):Int {
        var s = 0;
        for (i in 0...n) s += value();
        return s;
    }
}

class B {
    public static function value():Int return 2;
    public static function run(n:Int):Int {
        var s = 0;
        for (i in 0...n) s += value();
        return s;
    }
}

class C {
    public static function value():Int return 3;
    public static function run(n:Int):Int {
        var s = 0;
        for (i in 0...n) s += value();
        return s;
    }
}

class D {
    public static function value():Int return 4;
    public static function run(n:Int):Int {
        var s = 0;
        for (i in 0...n) s += value();
        return s;
    }
}

class E {
    public static function value():Int return 5;
    public static function run(n:Int):Int {
        var s = 0;
        for (i in 0...n) s += value();
        return s;
    }
}

class TestNameCollision {
    // Promotion to the optimising tier counts INTERPRETED invocations, so the
    // callers must be entered many times; a single long-running call never
    // gets there. Small n, many calls.
    static var ITERS = 16;
    static var CALLS = 60000;

    static function check(name:String, got:Int, want:Int):Bool {
        if (got == want) {
            Sys.println(name + " ok");
            return true;
        }
        Sys.println(name + " WRONG got=" + got + " want=" + want);
        return false;
    }

    static function main() {
        var n = ITERS;
        var bad = 0;
        for (k in 0...CALLS) {
            if (A.run(n) != n * 1) bad++;
            if (B.run(n) != n * 2) bad++;
            if (C.run(n) != n * 3) bad++;
            if (D.run(n) != n * 4) bad++;
            if (E.run(n) != n * 5) bad++;
        }
        // Report once at the end: a misbind that only starts after promotion
        // would otherwise scroll past in a wall of identical lines.
        var ok = check("A", A.run(n), n * 1);
        ok = check("B", B.run(n), n * 2) && ok;
        ok = check("C", C.run(n), n * 3) && ok;
        ok = check("D", D.run(n), n * 4) && ok;
        ok = check("E", E.run(n), n * 5) && ok;
        Sys.println("wrong results during warmup: " + bad);
        Sys.println((ok && bad == 0) ? "PASS" : "FAIL");
    }
}
