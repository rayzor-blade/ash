// Relational comparison between strings.
//
// `<` and `>` on strings reach the same JSLt/JSGt opcodes integers do, but
// with HOBJ (String) operands. Those fell through to NanBoxedValue::compare,
// which has no ordering for pointers and answers None, so every one of them
// was false. Equality was unaffected — it has its own String path — which is
// why 43 corpus programs and the whole stdlib test missed this.
class TestStringOrder {
    static function main() {
        var checks = 0;

        // Ordering by first differing code unit.
        if ("apple" < "pear") checks += 1;
        if ("pear" > "apple") checks += 2;
        if (!("pear" < "apple")) checks += 4;

        // A common prefix orders by length.
        if ("fig" < "figs") checks += 8;
        if ("figs" > "fig") checks += 16;

        // Equal strings are neither less nor greater, but are <= and >=.
        if (!("fig" < "fig") && !("fig" > "fig")) checks += 32;
        if ("fig" <= "fig" && "fig" >= "fig") checks += 64;

        // Through variables, so the operands are registers rather than
        // constants the compiler might fold.
        var a = "alpha", b = "beta";
        if (a < b && b > a) checks += 128;

        // Non-ASCII, where the comparison is over UTF-16 code units.
        if ("a" < "é" && "é" < "世") checks += 256;

        // NOTE: the sort that surfaced this is deliberately not exercised
        // here. Array<String>.sort segfaults independently of comparison —
        // see TestArrayObjSort.hx — and this case must be able to pass while
        // that is outstanding.

        Sys.println("Checksum: " + checks);
    }
}
