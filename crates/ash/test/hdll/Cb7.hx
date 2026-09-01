// CASE #7 -- closures that RETURN Dynamic, called from a native library
// through hl_dyn_call. The point of interest is the native side UNBOXING the
// vdynamic* that hl_dyn_call hands back.
//
// Shape mirrors hxDatachannel: the closure is handed to native code, native
// code keeps the raw vclosure* in a malloc'd struct with no GC root, the Haxe
// program then churns the heap, and only afterwards does native code call it.
//
// Env switches: CB7_HLP=1, CB7_ROOT=object|slot, CB7_CAST=1 (see cb7test.c),
// plus ASH_GC_STRESS=1 / ASH_GC_NO_RECLAIM=1.

class Adder7 {
    public var base:Int;
    public function new(b:Int) { base = b; }
    public function apply(x:Int):Dynamic { return x * 3 + 1 + base; }
}

class Cb7 {
    @:hlNative("cb7test", "store_d")
    static function storeD(idx:Int, f:Int->Dynamic):Void {}

    @:hlNative("cb7test", "store_i")
    static function storeI(idx:Int, f:Int->Int):Void {}

    @:hlNative("cb7test", "store_dd")
    static function storeDD(idx:Int, f:Dynamic->Dynamic):Void {}

    @:hlNative("cb7test", "invoke")
    static function invoke(idx:Int, arg:Int):Dynamic { return null; }

    @:hlNative("cb7test", "invoke_f")
    static function invokeF(idx:Int, arg:Float):Dynamic { return null; }

    @:hlNative("cb7test", "invoke_heap")
    static function invokeHeap(idx:Int, arg:Int):Dynamic { return null; }

    @:hlNative("cb7test", "invoke_null")
    static function invokeNull(idx:Int):Dynamic { return null; }

    // ---- the closures under test -------------------------------------
    static function retInt(x:Int):Dynamic { return x * 3 + 1; }
    static function retFloat(x:Int):Dynamic { return x + 0.5; }
    static function retBool(x:Int):Dynamic { return x > 0; }
    static function retString(x:Int):Dynamic { return "v" + x; }
    static function retNull(x:Int):Dynamic { return null; }
    static function retObj(x:Int):Dynamic { return { a: x, b: "z" }; }
    static function retArr(x:Int):Dynamic { return [x, x + 1]; }
    static function retIntTyped(x:Int):Int { return x * 3 + 1; }   // control: dyn_call must BOX
    static function dynEcho(v:Dynamic):Dynamic {
        if (v == null) return "was-null";
        return v;
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

    static function show(label:String, f:Void->Dynamic, expect:String):Void {
        var got:String;
        try {
            got = Std.string(f());
        } catch (e:Dynamic) {
            got = "EXCEPTION(" + Std.string(e) + ")";
        }
        Sys.println("  " + label + " -> " + got + "   (expect " + expect + ")  "
            + (got == expect ? "OK" : "MISMATCH"));
    }

    static function main() {
        // 1. hand every closure to the native library
        storeD(0, retInt);
        storeD(1, retFloat);
        storeD(2, retBool);
        storeD(3, retString);
        storeD(4, retNull);
        storeD(5, retObj);
        storeD(6, retArr);
        storeD(7, new Adder7(100).apply);
        storeI(8, retIntTyped);
        storeDD(9, dynEcho);

        // 2. churn so an unrooted closure would be collected
        churn();

        // 3. native code calls each one through hl_dyn_call and unboxes
        Sys.println("== results ==");
        show("0 retInt(14)     ", () -> invoke(0, 14), "43");
        show("1 retFloat(14)   ", () -> invoke(1, 14), "14.5");
        show("2 retBool(14)    ", () -> invoke(2, 14), "true");
        show("3 retString(14)  ", () -> invoke(3, 14), "v14");
        show("4 retNull(14)    ", () -> invoke(4, 14), "null");
        show("5 retObj(14).a   ", () -> { var o:Dynamic = invoke(5, 14); o == null ? null : o.a; }, "14");
        show("6 retArr(14).len ", () -> { var a:Dynamic = invoke(6, 14); a == null ? null : (cast a : Array<Dynamic>).length; }, "2");
        show("7 bound(14)      ", () -> invoke(7, 14), "143");
        show("8 retIntTyped(14)", () -> invoke(8, 14), "43");
        // NOTE: dynEcho hands the argument pointer straight back to Haxe, so
        // the argument MUST be GC-heap allocated. Feeding it a `vdynamic` on
        // the native stack frame dangles as soon as the prim returns -- that
        // is a bug in the HDLL, not in the runtime, and it is why invoke_heap
        // and invoke_f allocate through hl_alloc_dynamic.
        show("9 dynEcho(14)    ", () -> invokeHeap(9, 14), "14");
        show("9 dynEcho(14)==14", () -> (invokeHeap(9, 14) == 14), "true");
        show("9 dynEcho(2.5)   ", () -> invokeF(9, 2.5), "2.5");
        show("9 dynEcho(null)  ", () -> invokeNull(9), "was-null");
        Sys.println("== done ==");
    }
}
