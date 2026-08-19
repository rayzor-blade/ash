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
// Vtable dispatch. The receiver comes from Sys.time() and two subclasses
// exist, so nothing can prove which arrives: with a single `new Stepper()`
// in view of the loop, LLVM devirtualized, inlined and vectorized it, and
// 100M dispatches "ran" in 6.04ms. The choice is made once, outside the
// loop, so the body carries no branch the other kernels lack.
class Stepper {
    public function new() {}
    public function step(acc:Int, i:Int):Int { return acc * 31 + (i % 8); }
}
class Stepper2 extends Stepper {
    public function new() { super(); }
    override public function step(acc:Int, i:Int):Int { return acc * 31 + (i % 8) + 0; }
}
class BenchMethodCall {
    static function main() {
        var s:Stepper = (Sys.time() < 0.0) ? new Stepper2() : new Stepper();
        var sum = 0;
        var i = 0;
        while (i < 100000000) {
            sum = s.step(sum, i);
            i = i + 1;
        }
        Sys.println("BenchMethodCall " + sum);
    }
}
