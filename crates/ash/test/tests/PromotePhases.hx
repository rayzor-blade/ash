// Hot functions that become hot one after another, so promotions arrive
// after the first one has already had the shared LLVM module emitted.
class PromotePhases {
    static function h0(x:Int):Int return (x * 3) ^ (x >> 1);
    static function g0(x:Int):Int return h0(x) + h0(x + 1);
    static function f0(n:Int):Int {
        var s = 0;
        for (k in 0...n) s += g0(k + s);
        return s;
    }
    static function h1(x:Int):Int return (x * 4) ^ (x >> 2);
    static function g1(x:Int):Int return h1(x) + h1(x + 1);
    static function f1(n:Int):Int {
        var s = 0;
        for (k in 0...n) s += g1(k + s);
        return s;
    }
    static function h2(x:Int):Int return (x * 5) ^ (x >> 3);
    static function g2(x:Int):Int return h2(x) + h2(x + 1);
    static function f2(n:Int):Int {
        var s = 0;
        for (k in 0...n) s += g2(k + s);
        return s;
    }
    static function h3(x:Int):Int return (x * 6) ^ (x >> 4);
    static function g3(x:Int):Int return h3(x) + h3(x + 1);
    static function f3(n:Int):Int {
        var s = 0;
        for (k in 0...n) s += g3(k + s);
        return s;
    }
    static function h4(x:Int):Int return (x * 7) ^ (x >> 5);
    static function g4(x:Int):Int return h4(x) + h4(x + 1);
    static function f4(n:Int):Int {
        var s = 0;
        for (k in 0...n) s += g4(k + s);
        return s;
    }
    static function h5(x:Int):Int return (x * 8) ^ (x >> 6);
    static function g5(x:Int):Int return h5(x) + h5(x + 1);
    static function f5(n:Int):Int {
        var s = 0;
        for (k in 0...n) s += g5(k + s);
        return s;
    }
    static function h6(x:Int):Int return (x * 9) ^ (x >> 7);
    static function g6(x:Int):Int return h6(x) + h6(x + 1);
    static function f6(n:Int):Int {
        var s = 0;
        for (k in 0...n) s += g6(k + s);
        return s;
    }
    static function h7(x:Int):Int return (x * 10) ^ (x >> 1);
    static function g7(x:Int):Int return h7(x) + h7(x + 1);
    static function f7(n:Int):Int {
        var s = 0;
        for (k in 0...n) s += g7(k + s);
        return s;
    }

    static function main() {
        var t = 0;
        for (r in 0...12000) t += f0(200);
        for (r in 0...12000) t += f1(200);
        for (r in 0...12000) t += f2(200);
        for (r in 0...12000) t += f3(200);
        for (r in 0...12000) t += f4(200);
        for (r in 0...12000) t += f5(200);
        for (r in 0...12000) t += f6(200);
        for (r in 0...12000) t += f7(200);
        Sys.println(t);
    }
}
