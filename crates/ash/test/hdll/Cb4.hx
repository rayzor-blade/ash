// CASE #4 -- hl_dyn_call with String and Bytes (pointer-typed) arguments.
//
//   handOver()  : allocates a String and an hl.Bytes at RUNTIME (not constant
//                 pool entries) and hands both, plus the callback closure, to
//                 the native library, which keeps only raw pointers in a
//                 malloc'd struct. handOver's frame then dies, so those three
//                 GC objects have no Haxe-side reference left at all.
//   churn       : 200k array + string allocations, so the collector has every
//                 reason to run before the callback is used.
//   nativeInvoke: the library boxes the stored String/Bytes and calls
//                 hl_dyn_call(joiner, args, 3).
//
// joiner("cb4-payload-42", <bytes 'A','B','C','D'>, 4)
//   = "cb4-payload-42" + "|" + "ABCD" + "|" + "4"
//   = "cb4-payload-42|ABCD|4"        <- the only correct answer.

class Cb4 {
    @:hlNative("cb4", "store")
    static function nativeStore(f:(String, hl.Bytes, Int) -> String):Void {}

    @:hlNative("cb4", "keep_str")
    static function nativeKeepStr(s:String):Void {}

    @:hlNative("cb4", "keep_bytes")
    static function nativeKeepBytes(b:hl.Bytes, len:Int):Void {}

    @:hlNative("cb4", "invoke")
    static function nativeInvoke():String { return null; }

    // The callback. Pointer-typed args in, pointer-typed value out.
    static function joiner(s:String, b:hl.Bytes, n:Int):String {
        var out = new StringBuf();
        out.add(s == null ? "<NULLSTR>" : s);
        out.add("|");
        if (b == null) {
            out.add("<NULLBYTES>");
        } else {
            var i = 0;
            while (i < n) {
                out.addChar(b.getUI8(i));
                i++;
            }
        }
        out.add("|");
        out.add(n);
        return out.toString();
    }

    // Everything the native side will hold is created here and nowhere else,
    // so nothing survives on the Haxe side once this returns.
    static function handOver():Void {
        var payload = "cb4-" + "payload-" + Std.string(21 * 2);
        Sys.println("haxe: handing over string = " + payload);
        nativeKeepStr(payload);

        var hb = new hl.Bytes(4);
        hb.setUI8(0, 65);
        hb.setUI8(1, 66);
        hb.setUI8(2, 67);
        hb.setUI8(3, 68);
        Sys.println("haxe: handing over bytes  = ABCD (4)");
        nativeKeepBytes(hb, 4);

        nativeStore(joiner);
    }

    // Overwrite the dead frames left by handOver(): a conservative scan of a
    // stale interpreter/native stack slot is not a GC root, it is luck, and
    // this removes the luck.
    static function scrub(d:Int):Int {
        if (d == 0) return 0;
        var a = d * 7; var b = d * 11; var c = d * 13; var e = d * 17;
        var f = d * 19; var g = d * 23; var h = d * 29; var i2 = d * 31;
        var j = d * 37; var k = d * 41; var l = d * 43; var m = d * 47;
        return a + b + c + e + f + g + h + i2 + j + k + l + m + scrub(d - 1);
    }

    static function main() {
        var expected = "cb4-payload-42|ABCD|4";

        handOver();

        if (Sys.getEnv("CB4_SCRUB") != null) {
            Sys.println("haxe: scrub = " + scrub(400));
        }

        var sink = 0;
        for (i in 0...200000) {
            var a = [i, i + 1, i + 2];
            sink += a[2];
            var s = "s" + i;
            if (s.length == 0) sink++;
        }
        Sys.println("churn sink: " + sink);

        // CB4_MAJOR=1 forces a real collection right before the callback, so
        // the test does not depend on an engine happening to trigger one.
        if (Sys.getEnv("CB4_MAJOR") != null) {
            Sys.println("haxe: hl.Gc.major()");
            hl.Gc.major();
            hl.Gc.major();
        }

        var r = nativeInvoke();
        Sys.println("invoke() = " + (r == null ? "<null>" : Std.string(r)));
        Sys.println("expected = " + expected);
        Sys.println(r == expected ? "CB4 TEST: PASS" : "CB4 TEST: FAIL");
    }
}
