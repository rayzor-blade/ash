// The program's own argv, i.e. Sys.args().
//
// Stock hl's contract: everything on the command line AFTER the .hl file
// belongs to the program, hyphenated or not, and Sys.args() returns exactly
// that in order. ash previously failed a step earlier — its CLI rejected
// the program's arguments as unknown options and refused to start — and
// even with pass-through the runtime side (hlp_sys_init) existed with no
// caller, so Sys.args() answered from nothing.
//
// The parity harness passes this case's program_args to ash, the reference
// hl, and the oracle generator identically; anything less and the
// comparison would assert nothing.
class TestSysArgs {
    static function main() {
        var a = Sys.args();
        Sys.println("argc=" + a.length);
        for (i in 0...a.length)
            Sys.println("  [" + i + "] " + a[i]);
        // Positional integrity as a single number: order swaps change it.
        var sum = 0;
        for (i in 0...a.length)
            sum += (i + 1) * a[i].length;
        Sys.println("Checksum: " + sum);
    }
}
