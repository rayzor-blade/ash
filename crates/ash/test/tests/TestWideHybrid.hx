class TestWideHybrid {
    @:noInline static function wide(
        a0:Int, a1:Int, a2:Int, a3:Int,
        a4:Int, a5:Int, a6:Int, a7:Int,
        a8:Int, a9:Int, a10:Int, a11:Int,
        a12:Int, a13:Int, a14:Int, a15:Int
    ):Int {
        var total = a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7
            + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15;
        for (n in 0...2) total += n;
        return total - 1;
    }

    static function main():Void {
        var bad = 0;
        var call = wide;
        for (i in 0...200000) {
            if (call(i, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15) != i + 120)
                bad++;
        }
        Sys.println('wide-hybrid bad=' + bad);
    }
}
