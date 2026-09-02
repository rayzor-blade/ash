// A Haxe `throw` escaping a COMPILED frame, over and over.
//
// The throw is ordinary control flow, but it reaches the interpreter as an
// error out of the compiled call boundary, and the hybrid path used to read
// that as "this function failed to compile": it retired the findex for the
// rest of the run and re-entered the function from its first opcode, so the
// side effects the compiled attempt had already committed happened twice.
//
// `sideEffects` is the gate. It counts one increment per call and must equal
// the call count in every mode -- a duplicated re-execution shows up as
// 400001, which is what hybrid printed while the interpreter and the
// whole-module JIT printed 400000. The loop is long enough that the function
// is promoted well before the first throw.
class TestThrowAcrossTiers {
    static var sideEffects = 0;
    static function work(i:Int):Int {
        sideEffects++;
        if (i % 100000 == 99999) throw "boom";
        var s = 0;
        for (k in 0...50) s += k * (i & 1023);
        return s;
    }
    static function main() {
        var total = 0;
        var caught = 0;
        for (i in 0...400000) {
            try { total += work(i); }
            catch (e:Dynamic) { caught++; }
        }
        Sys.println("total=" + total);
        Sys.println("caught=" + caught);
        Sys.println("sideEffects=" + sideEffects);
    }
}
