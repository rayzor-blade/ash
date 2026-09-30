class TestF32TierParity {
    static function calculate(seed:Single):Single {
        var a:Single = seed * 0.03125;
        var b:Single = a + 0.01234567;
        var c:Single = b * 1.2345678;
        var d:Single = c + seed * 0.0078125;
        return d;
    }

    static function main():Void {
        var seed:Single = Std.parseFloat("32.29193878173828");
        var expected = calculate(seed);
        for (_ in 0...5000) {
            var result = calculate(seed);
            if (result != expected) throw 'tier changed f32 result: $expected vs $result';
        }
        Sys.println('f32=$expected');
    }
}
