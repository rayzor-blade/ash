// Reading a virtual's field when the backing object has no slot for it.
//
// `Field` on an HVIRTUAL takes a fast path when the field is in `vfields`, and
// otherwise falls back to a dynamic get on the underlying value. That fallback
// used to call `hlp_dyn_getp` for every destination -- the POINTER getter --
// and store what it returned into whatever register `dst` is. For an Int field
// that put 8 bytes of boxed pointer into a 4-byte slot and read back its low
// half; for a Float field it reinterpreted a pointer as a double, which is a
// denormal near zero rather than the number. A whole-program audit of a game
// found 588 sites of the first shape and 292 of the second.
//
// A Reflect-built object has no vfields entry for these, so assigning it to a
// structure type forces the fallback for one Int and one Float destination.
// Run under ASH_CHECK_REG_STORES=1 to check the emitted IR as well as the
// values: a store into a register slot of the wrong width is reported there
// even when this path is not taken at run time.
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
