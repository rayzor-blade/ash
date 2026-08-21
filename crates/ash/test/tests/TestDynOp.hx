// Arithmetic where at least one operand is Dynamic, i.e. std@hlp_dyn_op.
//
// The official Haxe unit suite reaches this constantly and ash did not
// implement it at all, so the native was unresolved and the suite could not
// start. The semantics have one trap worth pinning: the arithmetic operators
// go through dyn_castd and therefore always yield a Float, even for two
// boxed Ints, while the shifts and bitwise operators yield an Int. That is
// HashLink's behaviour, not an accident of ours, and code that adds two
// Dynamics and then asks Std.isOfType(v, Int) can tell the difference.
class TestDynOp {
    static function main() {
        var a:Dynamic = 7;
        var b:Dynamic = 2;
        var f:Dynamic = 2.5;

        // Arithmetic: Float results.
        var sum:Dynamic = a + b;
        var dif:Dynamic = a - b;
        var mul:Dynamic = a * b;
        var div:Dynamic = a / b;
        var mod:Dynamic = a % b;
        Sys.println("add=" + sum + " sub=" + dif + " mul=" + mul
                    + " div=" + div + " mod=" + mod);

        // Mixed int/float.
        var mix:Dynamic = a + f;
        Sys.println("mixed=" + mix);

        // Bitwise and shifts: Int results.
        var shl:Dynamic = a << b;
        var shr:Dynamic = a >> b;
        var ushr:Dynamic = a >>> b;
        var and:Dynamic = a & b;
        var or:Dynamic = a | b;
        var xor:Dynamic = a ^ b;
        Sys.println("shl=" + shl + " shr=" + shr + " ushr=" + ushr
                    + " and=" + and + " or=" + or + " xor=" + xor);

        // A negative operand exercises the unsigned shift specifically, which
        // is the one place the int path cannot be written as a plain C shift.
        var neg:Dynamic = -8;
        var negUshr:Dynamic = neg >>> 1;
        var negShr:Dynamic = neg >> 1;
        Sys.println("neg_ushr=" + negUshr + " neg_shr=" + negShr);

        // A single number that moves if any one of these regresses. The
        // arithmetic results are floored so the checksum stays an Int even
        // though dyn_op handed back Floats.
        var checksum = Std.int(sum) + Std.int(dif) * 2 + Std.int(mul) * 3
            + Std.int(div * 100) + Std.int(mod) * 5
            + shl * 7 + shr * 11 + ushr * 13 + and * 17 + or * 19 + xor * 23
            + negShr * 29;
        Sys.println("Checksum: " + checksum);
    }
}
