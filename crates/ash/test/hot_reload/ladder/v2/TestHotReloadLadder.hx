// A middle class with a virtual method of its own, below a root class that
// gains a slot: the program is built as v1, reloads as a later version once it
// says `ready`, and then shows a leaf through each class. With ROUNDS set it
// takes that many reloads, saying `next` after each but the last.
class A {
    public function new() {}

    public function f():String
        return "a.f";

    public function describe():String
        return "A";
}

class B extends A {
    override public function f():String
        return "b.f";

    public function h():String
        return "b.h";

    public function helper():String
        return "b.helper";

    public function toString():String
        return "B!";

    override public function describe():String
        return "B";
}

class C extends B {
    override public function h():String
        return "c.h";
}

class D extends B {
    override public function h():String
        return "d.h";
}

class Show {
    public static function render(c:C, b:B, a:A):Array<String> {
        return ["f " + c.f(), "h " + b.h(), "helper " + c.helper(), "byname " + Reflect.callMethod(c, Reflect.field(c, "helper"), []), "describe " + a.describe(), "str " + c];
    }
}

class TestHotReloadLadder {
    static function main() {
        var c = new C();
        var b:B = c;
        var a:A = c;
        var d:B = new D();
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
            Sys.println("render " + Show.render(c, b, a).join(","));
            Sys.println("oldh " + b.h());
            Sys.println("d " + d.h());
            Sys.println(round + 1 < rounds ? "next" : "done");
        }
    }
}
