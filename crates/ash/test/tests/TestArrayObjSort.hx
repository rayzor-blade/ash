// Array<String>.sort — once the flagship crash, now a regression net.
//
// The defect: OSafeCast between two unrelated HOBJ classes (ArrayObj ->
// ArrayDyn, via ArrayObj.sort's `cast this`) was lowered as a raw pointer
// copy in THREE places, where HashLink converts through the source class's
// inherited __cast (ArrayBase.__cast -> ArrayDyn.alloc(this, false)). The
// callee then read ArrayDyn's field 0 (array, a pointer) out of an ArrayObj
// whose field 0 is the inherited length:Int — which is why the fault
// address always equalled the array's length.
//
// The analysis this file used to carry blamed hlp_safe_cast answering true
// and a possible hl_type_obj collision. Both were wrong: hlp_safe_cast is
// never consulted on this path at all. Kept stated here because the wrong
// version was committed and cited; three independent investigations
// converged on the refutation.
class TestArrayObjSort {
    static function main() {
        var f = function(a:String, b:String) return a < b ? -1 : (a > b ? 1 : 0);
        var one = ["only"];
        one.sort(f);
        Sys.println("n=1 ok");
        var many = ["pear", "apple", "fig", "date"];
        many.sort(f);
        Sys.println("Checksum: " + many.join(","));
    }
}
