// Fields a reload adds to a class that is running: the program is built as
// v1, reloads as a later version once it says `ready`, and then shows an
// object made before the reload, and a fresh one. With ROUNDS set it takes
// that many reloads, saying `next` after each but the last.
class Item {
    public var id:Int;

    public function new(id:Int) {
        this.id = id;
    }
}

class Widget extends Item {
    public var label:String;
    public var hits:Int;
    public var items:Array<Int>;

    public function new(id:Int, label:String) {
        super(id);
        this.label = label;
        this.hits = 0;
        this.items = [1, 2, 3];
    }

    public function hit():Int {
        hits++;
        return hits;
    }
}

class Show {
    public static function render(w:Widget):Array<String> {
        return ["id " + w.id, "label " + w.label, "hits " + w.hit(), "items " + w.items.length];
    }

    public static function hot(w:Widget):Int
        return w.id + w.hits;

    public static function describe(w:Widget):String
        return w.id + "," + w.label + "," + w.hits;

    // Touches every widget, which sit next to each other in memory, and counts
    // those whose neighbours' fields were written over.
    public static function sweep(many:Array<Widget>):String {
        var sum = 0;
        var bad = 0;
        for (m in many) {
            if (m.label != "m" + (m.id - 10))
                bad++;
            sum += m.id + m.hits;
        }
        return sum + " bad " + bad;
    }
}

class TestHotReloadFields {
    static function main() {
        var w = new Widget(1, "w");
        var many = [for (i in 0...300) new Widget(i + 10, "m" + i)];
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
            Sys.println("render " + Show.render(w).join(";"));
            // Reads fields of an object made before the reload, over and over,
            // long enough for the reader to be compiled.
            var total = 0;
            for (i in 0...200) {
                total += Show.hot(w);
                Sys.sleep(0.002);
            }
            Sys.println("hot " + total);
            Sys.println("many " + Show.sweep(many));
            Sys.println("fresh " + Show.describe(new Widget(2, "f")));
            Sys.println(round + 1 < rounds ? "next" : "done");
        }
    }
}
