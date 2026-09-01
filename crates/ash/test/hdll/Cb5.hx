// CASE #5: a class instance passed through Dynamic to a Haxe closure that is
// invoked from native code via hl_dyn_call, and MUTATED by that callee.
//
//   Haxe -> cb5test.store(bump, derive, new Cell(10,"a"))
//              native keeps the raw vclosure*/vobj* in a malloc'd struct,
//              registers no GC root
//   ... heavy allocation in Haxe ...
//   Haxe -> cb5test.invoke()      native: hl_dyn_call(bump,   {obj}, 1)
//   Haxe -> cb5test.fetch()       the SAME instance, now mutated
//   Haxe -> cb5test.invoke_obj()  native: hl_dyn_call(derive, {obj}, 1)
//   Haxe -> cb5test.fetch2()      the instance the callee allocated
//
// Required output by Haxe semantics:
//   invoke()   = 25          (10*2 + 5)
//   cell.n     = 25          mutation visible through the native-held pointer
//   cell.s     = a!
//   derived.n  = 125         (25 + 100)
//   derived.s  = a!?
//
// CB5_KEEP=1 makes Haxe hold static references to the closures and the cell,
// so they are rooted by the Haxe stack/globals. Default (no CB5_KEEP) drops
// every Haxe reference: the malloc'd C struct is then the ONLY thing holding
// them, which is the shape a missing GC root exposes. Combine with
// ASH_GC_STRESS=1 / ASH_GC_NO_RECLAIM=1.

class Cell {
    public var n:Int;
    public var s:String;
    public function new(n:Int, s:String) { this.n = n; this.s = s; }
    public function toString():String { return "Cell(" + n + "," + s + ")"; }
}

class Cb5 {
    @:hlNative("cb5test", "store")
    static function nativeStore(f:Cell->Int, g:Cell->Cell, o:Cell):Void {}

    @:hlNative("cb5test", "invoke")
    static function nativeInvoke():Int { return 0; }

    @:hlNative("cb5test", "invoke_obj")
    static function nativeInvokeObj():Void {}

    @:hlNative("cb5test", "fetch")
    static function nativeFetch():Dynamic { return null; }

    @:hlNative("cb5test", "fetch2")
    static function nativeFetch2():Dynamic { return null; }

    // mutates the instance handed to it through the Dynamic argument slot
    static function bump(c:Cell):Int {
        c.n = c.n * 2 + 5;
        c.s = c.s + "!";
        return c.n;
    }

    // allocates a NEW instance derived from the argument
    static function derive(c:Cell):Cell {
        return new Cell(c.n + 100, c.s + "?");
    }

    // only populated under CB5_KEEP=1
    static var keptInt:Cell->Int = null;
    static var keptObj:Cell->Cell = null;
    static var keptCell:Cell = null;

    // The store call lives in its own frame so that, once it returns, no
    // live register or spill slot of main() still holds the closure or the
    // instance. Without this the conservative stack scanner roots all three
    // for the whole of main and no rooting bug can ever show.
    static function handOff():Void {
        nativeStore(bump, derive, new Cell(10, "a"));
    }

    // Overwrite the popped frame so stale words cannot keep acting as roots.
    static function scrub(d:Int):Int {
        if (d == 0) return 0;
        var a0 = d * 7; var a1 = d * 11; var a2 = d * 13; var a3 = d * 17;
        var a4 = d * 19; var a5 = d * 23; var a6 = d * 29; var a7 = d * 31;
        var b = [a0, a1, a2, a3, a4, a5, a6, a7];
        return b[d & 7] + scrub(d - 1);
    }

    static function churn(tag:String):Void {
        var sink = 0;
        for (i in 0...200000) {
            var a = [i, i + 1, i + 2];
            sink += a[2];
            var s = "s" + i;
            if (s.length == 0) sink++;
        }
        Sys.println("churn " + tag + " sink: " + sink);
    }

    static function main() {
        var keep = Sys.getEnv("CB5_KEEP") != null;
        Sys.println(keep ? "mode: KEEP (Haxe holds refs)" : "mode: DROP (native struct is the only holder)");

        // CB5_WARM=1 makes the two callees hot BEFORE their closures are
        // taken, so a tiering engine can put a real compiled address in
        // vclosure.fun instead of the findex+1 stub sentinel. hlp_call_method
        // dispatches on exactly that: a sentinel goes to the interpreter
        // closure-runner, a real address goes to hlc_static_call.
        if (Sys.getEnv("CB5_WARM") != null) {
            var acc = 0;
            for (i in 0...300000) {
                var w = new Cell(i, "w");
                acc += bump(w);
                acc += derive(w).n;
            }
            Sys.println("warm acc: " + acc);
        }

        // Churn BEFORE storing so the closures and the instance are allocated
        // out in fresh blocks among short-lived garbage, rather than in the
        // permanently-retained startup block at the bottom of the heap.
        churn("0");

        if (keep) {
            keptInt = bump;
            keptObj = derive;
            keptCell = new Cell(10, "a");
            nativeStore(keptInt, keptObj, keptCell);
        } else {
            handOff();
            Sys.println("scrub: " + scrub(96));
        }

        churn("1");

        // 1. object arg, mutated in place by the callee
        var r = nativeInvoke();
        Sys.println("invoke() = " + r + "  (expect 25)");

        var back:Cell = cast nativeFetch();
        if (back == null) {
            Sys.println("fetch() = null  (expect Cell(25,a!))");
        } else {
            Sys.println("cell.n = " + back.n + "  (expect 25)");
            Sys.println("cell.s = " + back.s + "  (expect a!)");
        }

        churn("2");

        // 2. object arg in, freshly allocated object out
        nativeInvokeObj();
        var d:Cell = cast nativeFetch2();
        if (d == null) {
            Sys.println("fetch2() = null  (expect Cell(125,a!?))");
        } else {
            Sys.println("derived.n = " + d.n + "  (expect 125)");
            Sys.println("derived.s = " + d.s + "  (expect a!?)");
        }

        var ok = r == 25
            && back != null && back.n == 25 && back.s == "a!"
            && d != null && d.n == 125 && d.s == "a!?";
        Sys.println(ok ? "CB5 TEST: PASS" : "CB5 TEST: FAIL");
    }
}
