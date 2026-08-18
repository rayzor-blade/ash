// Locals that stay live across a trap region.
//
// `OEndTrap`'s operand is Haxe's `OEndTrap of bool` — a flag, not a register.
// An interpreter that reads it as one and nulls that slot clears r0 or r1,
// whichever local happens to live there, so a `try` that assigns and then exits
// *normally* loses the assignment. Nothing else in the corpus reads a local
// after a non-throwing try, which is why the defect survived 43 programs.
class TestTrapLocals {
    static function normalExit():String {
        var acc = "s";
        try {
            acc += "|in";
        } catch (e:Dynamic) {
            acc += "|caught";
        }
        return acc + "|out";
    }

    static function throwingExit():String {
        var acc = "s";
        try {
            acc += "|in";
            throw "boom";
        } catch (e:String) {
            acc += "|caught:" + e;
        }
        return acc + "|out";
    }

    static function counters():String {
        // Low-numbered int locals are what the flag operand collides with.
        var a = 1, b = 2, c = 3;
        for (i in 0...4) {
            try {
                a += i;
                if (i == 2) throw "skip";
                b += i;
            } catch (e:String) {
                c += 10;
            }
        }
        return a + "," + b + "," + c;
    }

    static function nested():String {
        var outer = "o";
        try {
            outer += "|a";
            try {
                outer += "|b";
            } catch (e:Dynamic) {
                outer += "|inner";
            }
            outer += "|c";
        } catch (e:Dynamic) {
            outer += "|outer";
        }
        return outer + "|end";
    }

    static function main():Void {
        Sys.println(normalExit());
        Sys.println(throwingExit());
        Sys.println(counters());
        Sys.println(nested());
    }
}
