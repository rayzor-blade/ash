class Panel {
    public var count:Int;

    public function new(count:Int) {
        this.count = count;
    }

    // What a template makes: closures over `base`.
    public function render():Array<() -> Int> {
        var base = count;
        return [
            () -> base * 2,
        ];
    }
}

class TestHotReloadClosures {
    // The same in every version and called until it is compiled, so a closure
    // of the reloaded program reaches it from compiled code.
    static function apply(f:() -> Int, n:Int):Int {
        var total = 0;
        for (i in 0...n) total += f();
        return total;
    }

    static function main() {
        var panel = new Panel(5);
        var before = panel.render();
        Sys.println("ready");
        var reloaded = false;
        var deadline = Sys.time() + 30.0;
        while (Sys.time() < deadline) {
            apply(before[0], 200);
            if (hl.Api.checkReload()) {
                reloaded = true;
                break;
            }
            Sys.sleep(0.02);
        }
        Sys.println(reloaded ? "reloaded" : "no-reload");
        // Called from this frame, which began before the reload.
        Sys.println("old " + [for (f in before) f()].join(","));
        var fresh = panel.render();
        Sys.println("new " + [for (f in fresh) f()].join(","));
        Sys.println("reflect " + [for (f in fresh) Reflect.callMethod(null, f, [])].join(","));
        Sys.println("apply " + [for (f in fresh) apply(f, 10)].join(","));
        Sys.println("done");
    }
}
