class TestTieredHotLoop {
    static function hot(n:Int):Int {
        var acc = 0;
        var i = 0;
        while (i < n) {
            acc += i * 3 + 1;
            i++;
        }
        return acc;
    }

    static function main() {
        var total = 0;
        for (k in 0...20000) {
            total += hot(200);
        }
        Sys.println("OK TestTieredHotLoop " + total);
    }
}
