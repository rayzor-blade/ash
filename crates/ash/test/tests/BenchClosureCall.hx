// Part of the call-latency set: bench_inlined_call / bench_free_call /
// bench_method_call / bench_closure_call. Same loop, same 100M
// iterations, same checksum; only how the step is reached differs.
//
// The accumulator is `sum * 31 + (i % 8)`, not `sum + (i % 8)`. Integer
// addition is associative, so LLVM reassociates the reduction, vectorizes
// it, and -- because the callee is pure -- eliminates the call outright.
// That version reported 5.18ms for the baseline and 5.22ms for the call,
// which measures the optimizer rather than a call: zyntax's LLVM tier
// publishes 1.8ms on the same shape for the same reason. A multiply chain
// is a true loop-carried dependency, so the work survives and the delta
// between kernels means something.
// Indirect call through a closure: the target is in a register, so it is
// neither named in the instruction nor reachable through a vtable slot.
class BenchClosureCall {
    static function step(acc:Int, i:Int):Int {
        return acc * 31 + (i % 8);
    }
    static function main() {
        var f:(Int, Int) -> Int = (Sys.time() < 0.0) ? (a, b) -> a : step;
        var sum = 0;
        var i = 0;
        while (i < 100000000) {
            sum = f(sum, i);
            i = i + 1;
        }
        Sys.println("BenchClosureCall " + sum);
    }
}
