// CASE #3: mixed scalar arguments (Int / Float / Bool) through hl_dyn_call.
//
// Each callback prints exactly what it RECEIVED, so argument corruption is
// visible directly, and every result is checked against the same function
// called normally from Haxe -- the ground truth by Haxe semantics.
//
// The driver uses INT-ONLY native prims (cb3test.case_run) because the
// mixed-signature prims are unreachable in interp/hybrid (see cb3test.c).
// CB3_MIXPRIM=1 additionally exercises the mixed-signature prims.
//
//   CB3_CHURN=<n>   heap churn between store and invoke (default 200000)
//   CB3_ROOT=object|slot, CB3_HLP=1  are read by the native side.

class Cb3Recv {
    public var base:Float;
    public function new(b:Float) { base = b; }
    public function m(i:Int, d:Float, b:Bool):Float {
        Sys.println("  [haxe] bnd i=" + i + " d=" + d + " b=" + b + " base=" + base);
        return base + i + d + (b ? 0.25 : -0.25);
    }
}

class TestHdllCb3 {
    @:hlNative("cb3test", "store_i")  static function storeI(f:Int->Float->Bool->Int):Void {}
    @:hlNative("cb3test", "store_f")  static function storeF(f:Int->Float->Bool->Float):Void {}
    @:hlNative("cb3test", "store_b")  static function storeB(f:Int->Float->Bool->Bool):Void {}
    @:hlNative("cb3test", "store_r")  static function storeR(f:Bool->Float->Int->Float):Void {}
    @:hlNative("cb3test", "store_w")  static function storeW(f:Int->Float->Int->Float->Bool->Float->Int->Float):Void {}
    @:hlNative("cb3test", "store_m")  static function storeM(f:Int->Float->Bool->Float):Void {}

    @:hlNative("cb3test", "case_run")  static function caseRun(idx:Int):Int { return 0; }
    @:hlNative("cb3test", "last_kind") static function lastKind():Int { return -1; }
    @:hlNative("cb3test", "last_null") static function lastNull():Int { return 0; }

    @:hlNative("cb3test", "invoke_i") static function invokeI(i:Int, d:Float, b:Bool):Int { return 0; }
    @:hlNative("cb3test", "invoke_f") static function invokeF(i:Int, d:Float, b:Bool):Float { return 0; }
    @:hlNative("cb3test", "invoke_b") static function invokeB(i:Int, d:Float, b:Bool):Bool { return false; }
    @:hlNative("cb3test", "invoke_r") static function invokeR(b:Bool, d:Float, i:Int):Float { return 0; }
    @:hlNative("cb3test", "invoke_w") static function invokeW(i1:Int, d1:Float, i2:Int, d2:Float, b:Bool, d3:Float, i3:Int):Float { return 0; }
    @:hlNative("cb3test", "invoke_m") static function invokeM(i:Int, d:Float, b:Bool):Float { return 0; }

    // ---- the callbacks -----------------------------------------------------
    static function fnI(i:Int, d:Float, b:Bool):Int {
        Sys.println("  [haxe] fnI i=" + i + " d=" + d + " b=" + b);
        return i * 1000 + Std.int(d * 100) + (b ? 7 : 3);
    }
    static function fnF(i:Int, d:Float, b:Bool):Float {
        Sys.println("  [haxe] fnF i=" + i + " d=" + d + " b=" + b);
        return d * i + (b ? 0.5 : -0.5);
    }
    static function fnB(i:Int, d:Float, b:Bool):Bool {
        Sys.println("  [haxe] fnB i=" + i + " d=" + d + " b=" + b);
        return (i > 0) && (d > 1.0) && b;
    }
    static function fnR(b:Bool, d:Float, i:Int):Float {
        Sys.println("  [haxe] fnR b=" + b + " d=" + d + " i=" + i);
        return (b ? 1000.0 : -1000.0) + d - i;
    }
    static function fnW(i1:Int, d1:Float, i2:Int, d2:Float, b:Bool, d3:Float, i3:Int):Float {
        Sys.println("  [haxe] fnW i1=" + i1 + " d1=" + d1 + " i2=" + i2 + " d2=" + d2
                    + " b=" + b + " d3=" + d3 + " i3=" + i3);
        return i1 + d1 * 2 + i2 * 3 + d2 * 4 + (b ? 5 : -5) + d3 * 6 + i3 * 7;
    }

    static var fails = 0;
    static function chk(name:String, got:Float, want:Float) {
        var ok = got == want;
        if (!ok) fails++;
        Sys.println((ok ? "PASS " : "FAIL ") + name + ": got=" + got + " want=" + want);
    }
    // HI32=3 HF64=6 HBOOL=7
    static function chkCase(name:String, idx:Int, want:Float, wantKind:Int) {
        var got = caseRun(idx);
        var k = lastKind();
        var w = Math.round(want * 1000);
        var ok = (got == w) && (lastNull() == 0);
        if (!ok) fails++;
        Sys.println((ok ? "PASS " : "FAIL ") + name + ": got=" + (got / 1000.0)
            + " want=" + want + "  retKind=" + k + (k == wantKind ? "" : " (EXPECTED " + wantKind + ")")
            + (lastNull() == 1 ? "  [NULL RETURN]" : ""));
    }

    static function main() {
        var churn = 200000;
        var cs = Sys.getEnv("CB3_CHURN");
        if (cs != null) churn = Std.parseInt(cs);

        var recv = new Cb3Recv(100.5);

        storeI(fnI);
        storeF(fnF);
        storeB(fnB);
        storeR(fnR);
        storeW(fnW);
        storeM(recv.m);

        var sink = 0;
        for (i in 0...churn) {
            var a = [i, i + 1, i + 2];
            sink += a[2];
            var s = "s" + i;
            if (s.length == 0) sink++;
        }
        Sys.println("churn sink: " + sink);

        // ground truth: the same functions called normally from Haxe
        var eI1 = fnI(5, 2.5, true);      var eI2 = fnI(-3, 0.5, false);
        var eF1 = fnF(5, 2.5, true);      var eF2 = fnF(-3, 0.5, false);
        var eB1 = fnB(5, 2.5, true);      var eB2 = fnB(-3, 0.5, false);
        var eR1 = fnR(true, 1.25, 9);     var eR2 = fnR(false, -2.5, 7);
        var eW  = fnW(1, 1.5, 2, 2.5, true, 3.5, 4);
        var eM  = recv.m(5, 2.5, true);
        Sys.println("expected: I=" + eI1 + "/" + eI2 + " F=" + eF1 + "/" + eF2
            + " B=" + eB1 + "/" + eB2 + " R=" + eR1 + "/" + eR2 + " W=" + eW + " M=" + eM);

        Sys.println("=== int-only driver (all engines) ===");
        chkCase("case0 fnI(5,2.5,true)",   0, eI1, 3);
        chkCase("case1 fnI(-3,0.5,false)", 1, eI2, 3);
        chkCase("case2 fnF(5,2.5,true)",   2, eF1, 6);
        chkCase("case3 fnF(-3,0.5,false)", 3, eF2, 6);
        chkCase("case4 fnB(5,2.5,true)",   4, eB1 ? 1 : 0, 7);
        chkCase("case5 fnB(-3,0.5,false)", 5, eB2 ? 1 : 0, 7);
        chkCase("case6 fnR(true,1.25,9)",  6, eR1, 6);
        chkCase("case7 fnR(false,-2.5,7)", 7, eR2, 6);
        chkCase("case8 fnW(7 args)",       8, eW,  6);
        chkCase("case9 bound m(5,2.5,t)",  9, eM,  6);

        if (Sys.getEnv("CB3_MIXPRIM") != null) {
            Sys.println("=== mixed-signature native prims ===");
            chk("mix I", invokeI(5, 2.5, true), eI1);
            chk("mix F", invokeF(5, 2.5, true), eF1);
            chk("mix B", invokeB(5, 2.5, true) ? 1 : 0, eB1 ? 1 : 0);
            chk("mix R", invokeR(true, 1.25, 9), eR1);
            chk("mix W", invokeW(1, 1.5, 2, 2.5, true, 3.5, 4), eW);
            chk("mix M", invokeM(5, 2.5, true), eM);
        }

        Sys.println(fails == 0 ? "CB3 TEST: PASS" : "CB3 TEST: FAIL (" + fails + " mismatches)");
    }
}
