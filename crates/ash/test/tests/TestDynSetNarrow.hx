// Narrow values written through a dynamic field.
//
// HI32, HBOOL, HUI8 and HUI16 all reach the same setter, hlp_dyn_seti, whose
// value parameter is i32 — but their registers are not: a Bool loads as i1.
// The JIT passed the loaded value straight through, so any program that set a
// Bool on a Dynamic built a call the LLVM verifier rejected:
//
//     Call parameter type does not match function signature!
//       %dynset_src8 = load i1, ptr %reg_18, align 1
//
// A rejected module is not a partial failure — the whole program falls back to
// the interpreter. Reported from a Heaps game, whose per-frame state objects
// carry exactly this shape.
class TestDynSetNarrow {
    static function main() {
        var d:Dynamic = {};

        // The i1 case: the one that was actually broken.
        var flag = true;
        d.enabled = flag;
        d.disabled = false;

        // The i32 case, which already worked, so a fix that widens
        // indiscriminately gets caught here.
        d.count = 42;

        // Round-trip through Reflect as well, since that reaches the setter by
        // a different route than a literal field assignment.
        Reflect.setField(d, "viaReflect", true);

        var sum = 0;
        if (d.enabled) sum += 1;
        if (d.disabled) sum += 10;
        if (Reflect.field(d, "viaReflect")) sum += 100;
        sum += d.count;

        Sys.println("enabled=" + d.enabled + " disabled=" + d.disabled
                    + " count=" + d.count + " viaReflect=" + Reflect.field(d, "viaReflect"));
        Sys.println("Checksum: " + sum);
    }
}
