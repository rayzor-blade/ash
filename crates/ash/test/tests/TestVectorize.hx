// Loops whose length is only known at runtime.
//
// The vectorizer widens by 4 and runs the leftover iterations in a scalar
// copy, so the interesting lengths are the ones that do NOT divide 4: if the
// remainder is mishandled the last elements keep their initial value, or the
// widened loop runs one vector too far and writes past the end.
class TestVectorize {
    static var sink:Array<Int> = null;
    static function escape(a:Array<Int>):Void { sink = a; }

    // len comes from Sys.time() so no pass can fold it to a constant.
    static function fill(len:Int):Int {
        var a = new Array<Int>();
        for (i in 0...len) a.push(0);
        var i = 0;
        while (i < len) {
            a[i] = i * 3 + 1;
            i = i + 1;
        }
        var sum = 0;
        for (i in 0...len) sum = sum + a[i];
        escape(a);
        return sum;
    }

    static function scale(len:Int):Float {
        var a = new Array<Float>();
        for (i in 0...len) a.push(i * 0.5);
        var i = 0;
        while (i < len) {
            a[i] = a[i] * 2.0 + 1.0;
            i = i + 1;
        }
        var sum = 0.0;
        for (i in 0...len) sum = sum + a[i];
        return sum;
    }

    static function main() {
        var base = Std.int(Sys.time()) * 0;
        for (n in 0...13) {
            var len = base + n;
            Sys.println("fill " + len + " = " + fill(len));
        }
        for (n in 0...13) {
            var len = base + n;
            Sys.println("scale " + len + " = " + scale(len));
        }
    }
}
