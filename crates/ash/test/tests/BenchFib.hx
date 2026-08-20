// Recursive fibonacci -- the call-depth benchmark of the heavy set
// (mandelbrot / nbody / fib / inlined_call / method_call / free_call).
//
// Where the call-latency kernels measure one call in a flat loop, this
// measures a call TREE: fib(40) is 331,160,281 invocations nested up to 40
// frames deep, so it exercises prologue/epilogue, argument marshaling and
// the return path rather than loop back-edges. That makes it the one bench
// in the set that a tier can only win by making calls themselves cheap --
// there is no loop body to optimize.
//
// It is also the benchmark that most depends on `main` NOT being the only
// hot function: fib itself is invoked ~30M times, so it crosses both the
// tier-0 (100) and tier-1 (10000) thresholds within the first millisecond,
// which is exactly the promotion path under test.
//
// n = 40: 331,160,281 invocations. That is the figure the published Haxe
// target numbers and the rayzor/zyntax suites quote, so this row is
// directly comparable against them rather than against itself.
class BenchFib {
    static function fib(n:Int):Int {
        if (n < 2) return n;
        return fib(n - 1) + fib(n - 2);
    }

    static function main() {
        Sys.println("BenchFib " + fib(40));
    }
}
