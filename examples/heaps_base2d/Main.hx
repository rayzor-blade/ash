class Main extends hxd.App {
    var time:Float = 0;

    override function init() {
        // No text/font — just colored rectangles to prove rendering works
        engine.backgroundColor = 0xFF2244AA;
    }

    override function update(dt:Float) {
        time += dt;
        // Cycle background color
        var r = Std.int(128 + 127 * Math.sin(time));
        var g = Std.int(128 + 127 * Math.sin(time * 0.7));
        var b = Std.int(128 + 127 * Math.sin(time * 1.3));
        engine.backgroundColor = 0xFF000000 | (r << 16) | (g << 8) | b;
    }

    static function main() {
        new Main();
    }
}
