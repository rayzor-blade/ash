// Reading fields of a structure backed by a Reflect-built object.
//
// This was written to reproduce a real defect: the HVIRTUAL `Field` fallback
// used to read every absent field with `hlp_dyn_getp` and store the boxed
// pointer into the destination register whatever its type was. IT DOES NOT
// REPRODUCE IT. The values come back correct on a binary from before that fix
// as well as after, both orderings below -- populate-then-cast and
// cast-then-populate -- so this path evidently keeps taking the fast vfields
// route. The defect is real in the emitted IR (see the commit, and
// ASH_CHECK_REG_STORES), but nothing here demonstrates it at run time.
//
// Kept only as coverage for virtual field reads over a Reflect-built object.
// Do not read a pass here as evidence about that fallback.
typedef Point = {
    var x : Float;
    var n : Int;
    var s : String;
}

class TestVirtualDynField {
    static function readBack(p : Point):String {
        // Kept out of main so the reads land in a promoted function.
        return p.x + "|" + p.n + "|" + p.s;
    }

    static function main() {
        // Cast FIRST, populate after: `hlp_init_virtual` maps only the
        // fields the backing object has at cast time, so these three get no
        // vfields slot and every read has to take the dynamic fallback. Doing
        // it the other way round populates vfields and takes the fast path,
        // which is why the obvious version of this test proves nothing.
        var d : Dynamic = {};
        var p : Point = d;
        Reflect.setField(d, "x", 1.5);
        Reflect.setField(d, "n", 42);
        Reflect.setField(d, "s", "ok");

        var ok = true;
        // Many iterations so the reader reaches the optimising tier.
        for (i in 0...200000) {
            if (readBack(p) != "1.5|42|ok") ok = false;
        }
        Sys.println("virtual dyn field: " + readBack(p));
        Sys.println(ok ? "PASS" : "FAIL");
    }
}
