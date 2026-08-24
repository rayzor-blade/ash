// The pathological order for a demand gate. The leaf is called 40 times per
// iteration in straight-line code, and each iteration burns ~200us inside
// heavy()'s OWN frame, so the caller's back-edge count crawls at one per
// iteration. The leaf's tier-1 request -- invocation ~200 at iteration 5,
// landing once tier-0's ~2ms compile installs, around iteration 10 -- fires
// long BEFORE the caller's 64th back-edge publishes demand for it. A gate
// that refuses permanently caps the leaf at the middle tier for the whole
// run; one that re-proposes promotes it a few doublings later.
class Stepper {
    public function new() {}
    public function step(acc:Int, i:Int):Int { return acc * 31 + (i % 8); }
}
class Stepper2 extends Stepper {
    public function new() { super(); }
    override public function step(acc:Int, i:Int):Int { return acc * 31 + (i % 8) + 0; }
}
class BenchLateDemand {
    static function heavy(x:Int):Int {
        var s = x;
        var m = 0;
        while (m < 2000) { s = (s ^ (s << 1)) + m; m = m + 1; }
        return s;
    }
    static function main() {
        var st:Stepper = (Sys.time() < 0.0) ? new Stepper2() : new Stepper();
        var sum = 0;
        var j = 0;
        while (j < 100000) {
            sum = sum + heavy(j);
            sum = st.step(sum, 0); sum = st.step(sum, 1); sum = st.step(sum, 2); sum = st.step(sum, 3);
            sum = st.step(sum, 4); sum = st.step(sum, 5); sum = st.step(sum, 6); sum = st.step(sum, 7);
            sum = st.step(sum, 0); sum = st.step(sum, 1); sum = st.step(sum, 2); sum = st.step(sum, 3);
            sum = st.step(sum, 4); sum = st.step(sum, 5); sum = st.step(sum, 6); sum = st.step(sum, 7);
            sum = st.step(sum, 0); sum = st.step(sum, 1); sum = st.step(sum, 2); sum = st.step(sum, 3);
            sum = st.step(sum, 4); sum = st.step(sum, 5); sum = st.step(sum, 6); sum = st.step(sum, 7);
            sum = st.step(sum, 0); sum = st.step(sum, 1); sum = st.step(sum, 2); sum = st.step(sum, 3);
            sum = st.step(sum, 4); sum = st.step(sum, 5); sum = st.step(sum, 6); sum = st.step(sum, 7);
            sum = st.step(sum, 0); sum = st.step(sum, 1); sum = st.step(sum, 2); sum = st.step(sum, 3);
            sum = st.step(sum, 4); sum = st.step(sum, 5); sum = st.step(sum, 6); sum = st.step(sum, 7);
            j = j + 1;
        }
        Sys.println("BenchLateDemand " + sum);
    }
}
