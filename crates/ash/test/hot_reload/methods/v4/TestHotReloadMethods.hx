// Methods a reload adds, removes and overrides: the program is built as v1,
// reloads as a later version once it says `ready`, and then shows a leaf and
// a sibling through their base class. With ROUNDS set it takes that many
// reloads, saying `next` after each but the last.
class Base {
    public function new() {}

    public function name():String
        return "base";

    public function describe():String
        return "[" + name() + "]";
}

class Leaf extends Base {
    public var n:Int;

    public function new(n:Int) {
        super();
        this.n = n;
    }

    override public function name():String
        return "leaf" + n;
}

class Other extends Base {
    override public function name():String
        return "other";
}

class Show {
    public static function render(leaf:Leaf, base:Base):Array<String> {
        return ["name " + leaf.name(), "describe " + base.describe()];
    }
}

class TestHotReloadMethods {
    static function main() {
        var leaf = new Leaf(5);
        var base:Base = leaf;
        // Keeps Base.name virtual in every version.
        var other:Base = new Other();
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
                Sys.sleep(0.02);
            }
            Sys.println(reloaded ? "reloaded" : "no-reload");
            Sys.println("render " + Show.render(leaf, base).join(","));
            Sys.println("other " + other.name());
            Sys.println(round + 1 < rounds ? "next" : "done");
        }
    }
}
