// KNOWN FAILING — Array<String>.sort segfaults at any length, including 1.
//
// Not registered in parity_cases.toml; it is here so the repro is committed
// alongside the analysis rather than living in a scratch directory.
//
// What is known:
//   * Reproduces under --mode interp AND --mode jit, so it is not an
//     interpreter-only defect.
//   * Array<Int>.sort is fine — that goes through the bsort_i32 native.
//     Only the object-array path fails, and HashLink has no native object
//     sort: hl.types.ArrayObj.sort does `haxe.ds.ArraySort.sort(cast this, f)`.
//   * The faulting address equals the array's length every time — 1 for a
//     one-element array, 4 for a four-element one. An int is reaching a
//     pointer deref, i.e. type confusion, not an out-of-bounds walk.
//   * It is not the comparator: string `<`/`>` were separately broken and are
//     now fixed (TestStringOrder), and a hand-written bubble sort over the
//     same array now sorts correctly. Passing a correct comparator to
//     Array.sort still crashes.
//   * It is not generic dispatch on its own: a generic `get<T>(a:Array<T>,i)`,
//     a generic swap, and a comparator passed as a parameter and called all
//     work. The distinguishing move is ArrayObj's `cast this` to Array<T>.
//   * Predates this session's work; reproduces on binaries built before it.
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
