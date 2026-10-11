// Classes a reload adds: the program is built as v1, reloads as a later
// version once it says `ready`, and then renders its panel. With ROUNDS set it
// takes that many reloads, saying `next` after each but the last.
typedef Tagged = {function tag():String;}

class View {
    public var id:Int;

    public function new() {
        id = 1;
    }

    public function name():String
        return "view";
}

class Child extends View {
    public var n:Int;

    public function new(n:Int) {
        super();
        this.n = n;
    }

    override public function name():String
        return "child " + n;

    public function tag():String
        return "c" + n;
}

class Table extends View {
    public static var names = ["a", "b", "c"];
    public static var prefix = "t:" + names.length;

    public function new() {
        super();
    }

    override public function name():String
        return prefix + names.join("");

    public function tag():String
        return prefix;
}

class Panel extends View {
    public var count:Int;

    public function new(count:Int) {
        super();
        this.count = count;
    }

    public function source():Tagged
        return new Table();

    public function render():Array<String> {
        var out = [new Child(count).name()];
        out.push(new Table().name());
        return out;
    }
}

class TestHotReloadClasses {
    static function main() {
        var panel = new Panel(5);
        var source = panel.source();
        var rounds = Std.parseInt(Sys.getEnv("ROUNDS") ?? "1") ?? 1;
        Sys.println("ready");
        for (round in 0...rounds) {
            var reloaded = false;
            var deadline = Sys.time() + 30.0;
            while (Sys.time() < deadline) {
                if (hl.Api.checkReload()) {
                    reloaded = true;
                    break;
                }
                Sys.sleep(0.001);
            }
            Sys.println(reloaded ? "reloaded" : "no-reload");
            Sys.println("render " + panel.render().join(","));
            Sys.println("again " + panel.render().join(","));
            source = panel.source();
            Sys.println("source " + source.tag());
            Sys.println(round + 1 < rounds ? "next" : "done");
        }
    }
}
