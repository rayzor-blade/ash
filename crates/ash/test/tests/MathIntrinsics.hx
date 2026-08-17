// Edge cases for the Math primitives that both JIT tiers emit as machine
// instructions instead of calls into ash_std (see crates/ash/src/intrinsics.rs).
//
// The interesting cases are the ones where the obvious instruction is WRONG:
//
//   * Math.round is floor(x + 0.5), which disagrees with IEEE round-half-away
//     at negative halves: round(-2.5) is -2, not -3.
//   * Math.floor/ceil/round return Int, and ash_std casts with Rust's `as`,
//     which saturates. NaN becomes 0 and out-of-range clamps to Int min/max,
//     where a plain fptosi would be poison.
//
// Every value here must agree across the interpreter and both compiled tiers.
class MathIntrinsics {
    static function main() {
        var inf = Math.POSITIVE_INFINITY;
        var nan = Math.NaN;

        // sqrt
        Sys.println("sqrt(4)=" + Math.sqrt(4.0));
        Sys.println("sqrt(2)=" + Math.sqrt(2.0));
        Sys.println("sqrt(0)=" + Math.sqrt(0.0));
        Sys.println("sqrt(-1) isNaN=" + Math.isNaN(Math.sqrt(-1.0)));
        Sys.println("sqrt(inf)=" + Math.sqrt(inf));

        // abs
        Sys.println("abs(-3.5)=" + Math.abs(-3.5));
        Sys.println("abs(0)=" + Math.abs(0.0));
        Sys.println("abs(-inf)=" + Math.abs(-inf));

        // ffloor / fceil / fround keep Float type
        Sys.println("ffloor(-2.5)=" + Math.ffloor(-2.5));
        Sys.println("fceil(-2.5)=" + Math.fceil(-2.5));
        Sys.println("fround(-2.5)=" + Math.fround(-2.5));
        Sys.println("fround(2.5)=" + Math.fround(2.5));
        Sys.println("fround(-0.5)=" + Math.fround(-0.5));

        // floor / ceil / round return Int. Note round(-2.5) == -2.
        Sys.println("floor(2.7)=" + Math.floor(2.7));
        Sys.println("floor(-2.7)=" + Math.floor(-2.7));
        Sys.println("ceil(2.1)=" + Math.ceil(2.1));
        Sys.println("ceil(-2.1)=" + Math.ceil(-2.1));
        Sys.println("round(2.5)=" + Math.round(2.5));
        Sys.println("round(-2.5)=" + Math.round(-2.5));
        Sys.println("round(-3.5)=" + Math.round(-3.5));
        Sys.println("round(0.4)=" + Math.round(0.4));

        // Saturating conversions: these are the ones a non-saturating
        // instruction would turn into poison.
        Sys.println("floor(nan)=" + Math.floor(nan));
        Sys.println("ceil(nan)=" + Math.ceil(nan));
        Sys.println("round(nan)=" + Math.round(nan));
        Sys.println("floor(1e18)=" + Math.floor(1e18));
        Sys.println("floor(-1e18)=" + Math.floor(-1e18));
        Sys.println("ceil(inf)=" + Math.ceil(inf));
        Sys.println("floor(-inf)=" + Math.floor(-inf));

        // isNaN / isFinite
        Sys.println("isNaN(nan)=" + Math.isNaN(nan));
        Sys.println("isNaN(1.0)=" + Math.isNaN(1.0));
        Sys.println("isFinite(1.0)=" + Math.isFinite(1.0));
        Sys.println("isFinite(inf)=" + Math.isFinite(inf));
        Sys.println("isFinite(-inf)=" + Math.isFinite(-inf));
        Sys.println("isFinite(nan)=" + Math.isFinite(nan));

        // Values that reach the intrinsics through a loop, so the hot path a
        // tier actually promotes is covered too, not just straight-line code.
        var acc = 0.0;
        var n = 0;
        for (i in 0...200000) {
            var x = (i % 1000) - 500.5;
            acc += Math.sqrt(Math.abs(x));
            n += Math.round(x) + Math.floor(x) + Math.ceil(x);
        }
        Sys.println("loop acc=" + Std.int(acc * 1000));
        Sys.println("loop n=" + n);
    }
}
