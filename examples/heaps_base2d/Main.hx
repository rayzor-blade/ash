class Main extends hxd.App {
    var time:Float = 0;

    override function init() {
        trace("=== Main.init() called ===");
        engine.backgroundColor = 0xFF2244AA;
    }

    override function update(dt:Float) {
        time += dt;
        var r = Std.int(128 + 127 * Math.sin(time));
        var g = Std.int(128 + 127 * Math.sin(time * 0.7));
        var b = Std.int(128 + 127 * Math.sin(time * 1.3));
        engine.backgroundColor = 0xFF000000 | (r << 16) | (g << 8) | b;
    }

    static function main() {
        trace("=== Main.main() called ===");
        new Main();
    }
}
