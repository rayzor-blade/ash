// Array element read+write cost, same recipe as BenchFieldAccess.
//
// The accumulator lives in element 0 of a one-element array, so the loop
// does an indexed load and store each iteration on top of the arithmetic.
class BenchArrayAccess {
    static var sink:Array<Int> = null;
    static function escape(a:Array<Int>):Void { sink = a; }

    static function main() {
        var a = [0];
        if (Sys.time() < 0.0) a.push(1);
        var i = 0;
        while (i < 100000000) {
            a[0] = a[0] * 31 + (i % 8);
            i = i + 1;
        }
        escape(a);
        Sys.println("BenchArrayAccess " + a[0]);
    }
}
