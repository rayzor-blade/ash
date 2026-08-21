// Field read+write cost, against the same baseline the call kernels use.
//
// Same 100M iterations and the same `sum * 31 + (i % 8)` chain, but the
// accumulator lives in an object field instead of a local. The delta is
// what a field access costs once the offset is resolved at compile time.
//
// The receiver comes from Sys.time() so nothing can constant-fold which
// object it is, and `escape` is called once with it afterwards so the
// object is observably live -- without that a compiler is free to promote
// the field to a register for the whole loop and the kernel measures
// promotion rather than access.
class Holder {
    public var acc:Int;
    public function new() { acc = 0; }
}

class BenchFieldAccess {
    static var sink:Holder = null;
    static function escape(h:Holder):Void { sink = h; }

    static function main() {
        var h = (Sys.time() < 0.0) ? new Holder() : new Holder();
        var i = 0;
        while (i < 100000000) {
            h.acc = h.acc * 31 + (i % 8);
            i = i + 1;
        }
        escape(h);
        Sys.println("BenchFieldAccess " + h.acc);
    }
}
