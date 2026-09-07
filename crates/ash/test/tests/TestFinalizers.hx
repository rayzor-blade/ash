// The collector closes a file handle nobody closed.
//
// Every iteration abandons a `FileInput`. Under a low RLIMIT_NOFILE the loop
// can only finish if something hands the descriptors back, and nothing in
// Haxe's `sys.io` does: it is the finalizer on the handle block that closes
// them. See `crates/ash_cli/tests/finalizers.rs` for the limit this runs
// under.
class TestFinalizers {
    static function main() {
        var path = Sys.args()[0];
        sys.io.File.saveContent(path, "x");
        var opened = 0;
        for (i in 0...4000) {
            var f = sys.io.File.read(path, true);
            f.readByte();
            opened++;
            // A handle block is 24 bytes, so four thousand of them never come
            // near the collection trigger. The descriptors run out long
            // before the bytes do, which is why this asks explicitly.
            if (i % 64 == 63) hl.Gc.major();
        }
        Sys.println("opened " + opened);
    }
}
