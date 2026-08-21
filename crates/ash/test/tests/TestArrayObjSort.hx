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
//   * The faulting function is `HLInterpreter::op_field_get`, from
//     ASH_CRASH_BACKTRACE=1. It faults dereferencing its *object* operand,
//     which means a register that should hold an object holds an integer —
//     so an earlier field read returned the wrong field.
//   * That fits the layout exactly. `ArrayObj<T> extends ArrayBase`, and
//     ArrayBase contributes `length:Int` ahead of ArrayObj's own
//     `array:hl.NativeArray<Dynamic>`. Reading `array` one index low yields
//     `length` — an Int whose value is precisely the fault address we see.
//     `ArrayDyn` is the other suspect: its `length` is a property
//     (`length(get,never)`), not a stored field, so any field numbering that
//     counts properties shifts `array` and `allowReinterpret` by one.
//   * The chain, from ASH_DBG_FIELD=1 and ASH_DBG_SC=1 plus
//     `dump_air <file>.hl 27 --ops`:
//
//         42: SafeCast  { dst: Reg(20), src: Reg(22) }   ArrayObj -> ArrayDyn
//         51: New       { dst: Reg(22) }
//         52: SetField  { obj: Reg(22), field: 1, src: <NativeArray> }
//         54: SetField  { obj: Reg(22), field: 0, src: <ArraySize> }
//         62: Field     { dst: Reg(38), obj: Reg(20), field: 0 }   -> Ptr(0x2)
//         64: Field     { dst: Reg(37), obj: Reg(38), field: 0 }   -> SIGSEGV
//
//     Reg(22) is an ArrayObj: field 0 is `length` (Int, from ArrayBase),
//     field 1 is `array`. Reg(20) is read as ArrayDyn, whose field 0 is
//     `array:ArrayBase` — so field 0 yields the Int 2 and pc 64 dereferences
//     it. The registers hold the SAME pointer, so the SafeCast returned its
//     source unchanged.
//   * That only happens on one path: hlp_dyn_castp returns `*data` when
//     `t == to || hlp_safe_cast(t, to)`. Everything else either allocates
//     (castFun, to_virtual) or ends at `invalid_cast` returning null — and a
//     null would fault near 0x0, not at the array length. So
//     hlp_safe_cast(ArrayObj, ArrayDyn) is answering TRUE.
//   * It should answer false. ArrayObj's chain is ArrayObj -> ArrayBase ->
//     ArrayAccess; ArrayDyn's is ArrayDyn -> ArrayAccess. ArrayDyn is not in
//     it. The super-chain walk in std/src/types.rs (~line 270) reads
//     correctly, which points at its INPUTS rather than its logic: two
//     distinct HL types resolving to the same `hl_type_obj*` would make
//     `o == oto` true immediately. convert_type_ref_to_c_cached is the
//     obvious place for such a collision to originate.
//   * A correct cast here must WRAP, not reinterpret — HashLink's castFun is
//     how ArrayObj becomes an ArrayDyn holding it. Note ash sets castFun in
//     one object-init path (std/src/obj.rs:825) and forces it to None in
//     another (std/src/obj.rs:983); if the short helper runs for these types
//     the wrapper can never be built even once the safe_cast is fixed.
//   * THE SAME INSTRUCTION faults on the official Haxe suite. pc and lr agree
//     in their low 12 bits (0x878 / 0x91c — ASLR only shifts by whole pages)
//     across this repro, tests/unit and tests/sys. One defect is holding up
//     the entire conformance suite; see scripts/haxe_conformance.py.
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
