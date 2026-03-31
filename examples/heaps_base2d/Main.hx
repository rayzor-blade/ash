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
        new Main();
        // Pause the sentinel — its tick() deadlocks in single-threaded mode
        @:privateAccess hxd.System.sentinel.pause = true;
        // Let Timer.delay(onCreate, 1ms) expire
        Sys.sleep(0.05);
        // Now fire pending timers via EventLoop.progress
        trace("=== Calling progress (sentinel paused) ===");
        sys.thread.Thread.current().events.progress();
        trace("=== progress returned, loopFunc: " + (hxd.System.getCurrentLoop() != null));
        // Drive render loop
        while (true) {
            @:privateAccess hxd.System.mainLoop();
            Sys.sleep(0.016);
        }
    }
}
