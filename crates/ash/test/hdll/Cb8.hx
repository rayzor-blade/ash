// CASE #8 -- INSTANCE METHOD (bound) closures called back from a native
// library through hl_dyn_call.
//
// A bound closure carries hasValue=1, value=<receiver>, and a STRIPPED
// function type whose fun->parent is the FULL type the underlying code
// implements. hl_dyn_call has to prepend the receiver and dispatch against the
// full type. Every case below is a bound closure except slot 0 (a static
// function, the control) -- so any divergence between slot 0 and the rest is
// specific to the bound shape.
//
// The receivers in slots 1, 4, 5, 6, 7, 8, 9, 11 are allocated INLINE at the
// store call and never stored in a Haxe variable: after the store returns, the
// malloc'd C struct holds the only reference to the closure, and the closure
// holds the only reference to the receiver. Slots 2, 3 and 10 keep a Haxe-side
// reference as a control.
//
// Env switches live in cb8test.c: CB8_HLP, CB8_ROOT=object|slot|value|both,
// CB8_VERBOSE. Combine with ASH_GC_STRESS=1 / ASH_GC_NO_RECLAIM=1.

class Adder8 {
    public var base:Int;
    public var label:String;
    public function new(b:Int) { base = b; label = "unset"; }
    public function apply(x:Int):Int { return x * 3 + 1 + base; }
    public function name(x:Int):String { return "a" + (x + base); }
    public function mix(i:Int, f:Float, s:String):Int { return i + Std.int(f) + s.length + base; }
    public function setLabel(s:String):Void { label = s; }
}

class Counter8 {
    public var n:Int;
    public function new() { n = 0; }
    public function bump(k:Int):Int { n += k; return n; }
}

class Base8 {
    public var base:Int;
    public function new() { base = 0; }
    public function who(x:Int):Int { return 1000 + x; }
}

class Derived8 extends Base8 {
    public function new() { super(); }
    override public function who(x:Int):Int { return 2000 + x; }
}

interface IFace8 {
    public function ifn(x:Int):Int;
}

class Impl8 implements IFace8 {
    public var base:Int;
    public function new() { base = 0; }
    public function ifn(x:Int):Int { return 3000 + x; }
}

class Cb8 {
    @:hlNative("cb8test", "store_ii")
    static function storeII(idx:Int, f:Int->Int, name:String):Void {}

    @:hlNative("cb8test", "store_is")
    static function storeIS(idx:Int, f:Int->String, name:String):Void {}

    @:hlNative("cb8test", "store_mix")
    static function storeMix(idx:Int, f:(Int, Float, String) -> Int, name:String):Void {}

    @:hlNative("cb8test", "store_void")
    static function storeVoid(idx:Int, f:String->Void, name:String):Void {}

    @:hlNative("cb8test", "invoke_i")
    static function invokeI(idx:Int, arg:Int):Int { return 0; }

    @:hlNative("cb8test", "invoke_s")
    static function invokeS(idx:Int, arg:Int):Dynamic { return null; }

    @:hlNative("cb8test", "invoke_mix")
    static function invokeMix(idx:Int, i:Int, f:Float, s:String):Int { return 0; }

    @:hlNative("cb8test", "invoke_mixd")
    static function invokeMixD(idx:Int, i:Int, f:Dynamic, s:String):Int { return 0; }

    @:hlNative("cb8test", "invoke_void")
    static function invokeVoid(idx:Int, s:String):Void {}

    @:hlNative("cb8test", "dump")
    static function dump(idx:Int):Void {}

    // Haxe-side references -- the rooted controls.
    static var keptAdder:Adder8;
    static var keptCounter:Counter8;
    static var keptLabelled:Adder8;

    static function statTriple(x:Int):Int { return x * 3 + 1; }

    static var fails = 0;

    static function check(label:String, got:String, expect:String):Void {
        var ok = got == expect;
        if (!ok) fails++;
        Sys.println("  " + label + " -> " + got + "   (expect " + expect + ")  " + (ok ? "OK" : "MISMATCH"));
    }

    static function checkCall(label:String, f:Void->Dynamic, expect:String):Void {
        var got:String;
        try {
            got = Std.string(f());
        } catch (e:Dynamic) {
            got = "EXCEPTION(" + Std.string(e) + ")";
        }
        check(label, got, expect);
    }

    static function churn():Void {
        var sink = 0;
        for (i in 0...150000) {
            var a = [i, i + 1, i + 2];
            sink += a[2];
            var s = "s" + i;
            if (s.length == 0) sink++;
        }
        Sys.println("churn sink: " + sink);
    }

    static function main() {
        // ---- 1. hand every closure to the native library ------------------
        storeII(0, statTriple, "0-static");                      // control, hasValue=0

        storeII(1, new Adder8(100).apply, "1-inline");           // receiver unrooted

        keptAdder = new Adder8(200);
        storeII(2, keptAdder.apply, "2-kept");                   // receiver rooted in Haxe

        keptCounter = new Counter8();
        storeII(3, keptCounter.bump, "3-kept-mut");              // mutating, rooted

        storeII(4, new Counter8().bump, "4-inline-mut");         // mutating, unrooted

        storeII(5, (new Derived8() : Base8).who, "5-override");  // virtual dispatch

        storeII(6, (new Impl8() : IFace8).ifn, "6-iface");       // interface-typed

        var cap = 7;                                             // captured local
        storeII(7, function(x:Int):Int { return x * 100 + cap; }, "7-capture");

        storeIS(8, new Adder8(100).name, "8-ret-string");        // pointer return

        storeMix(9, new Adder8(5).mix, "9-mixed-args");          // 3 args + receiver

        keptLabelled = new Adder8(0);
        storeVoid(10, keptLabelled.setLabel, "10-void");         // void return, side effect

        var refl = new Adder8(300);
        var rf:Dynamic = Reflect.field(refl, "apply");
        storeII(11, cast rf, "11-reflect");                      // method closure via Reflect
        refl = null;

        // ---- 2. shape of the stored closures ------------------------------
        Sys.println("== shapes ==");
        for (i in 0...12) dump(i);

        // ---- 3. churn so an unrooted closure/receiver would be collected ---
        churn();

        // ---- 4. native code calls each one through hl_dyn_call -------------
        Sys.println("== results ==");
        checkCall("0  static(14)       ", () -> invokeI(0, 14), "43");
        checkCall("1  inline.apply(14) ", () -> invokeI(1, 14), "143");
        checkCall("2  kept.apply(14)   ", () -> invokeI(2, 14), "243");
        checkCall("3  kept.bump(1)     ", () -> invokeI(3, 1), "1");
        checkCall("3  kept.bump(2)     ", () -> invokeI(3, 2), "3");
        check("3  kept.n              ", Std.string(keptCounter.n), "3");
        checkCall("4  inline.bump(1)   ", () -> invokeI(4, 1), "1");
        checkCall("4  inline.bump(2)   ", () -> invokeI(4, 2), "3");
        checkCall("5  override.who(14) ", () -> invokeI(5, 14), "2014");
        checkCall("6  iface.ifn(14)    ", () -> invokeI(6, 14), "3014");
        checkCall("7  capture(14)      ", () -> invokeI(7, 14), "1407");
        checkCall("8  inline.name(14)  ", () -> invokeS(8, 14), "a114");
        try {
            invokeVoid(10, "hello");
            check("10 kept.label        ", keptLabelled.label, "hello");
        } catch (e:Dynamic) {
            check("10 kept.label        ", "EXCEPTION(" + Std.string(e) + ")", "hello");
        }
        checkCall("11 reflect.apply(14) ", () -> invokeI(11, 14), "343");

        // re-check the rooted receivers are still intact after everything
        check("2  kept.base          ", Std.string(keptAdder.base), "200");

        // Bound closure with a Float parameter, reached through a prim whose
        // own arguments are all integer/pointer shaped.
        checkCall("9d mixd(14,2.5,abc) ", () -> invokeMixD(9, 14, (2.5 : Dynamic), "abc"), "24");

        Sys.println(fails == 0 ? "CB8 core: PASS" : "CB8 core: FAIL (" + fails + " mismatches)");

        // Slot 9 through a prim that takes the Float UNBOXED goes last, on
        // purpose: on some engines that prim call is a FATAL error rather than
        // a Haxe exception, and it would otherwise truncate every case after
        // it.
        Sys.println("== epilogue: unboxed-float prim ==");
        checkCall("9  mix(14,2.5,abc)  ", () -> invokeMix(9, 14, 2.5, "abc"), "24");
        Sys.println(fails == 0 ? "CB8 all: PASS" : "CB8 all: FAIL (" + fails + " mismatches)");
    }
}
