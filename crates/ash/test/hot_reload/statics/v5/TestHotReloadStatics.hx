// Statics of a class that is already running: the program is built as v1 and
// reloads as a later version once it says `ready`, then calls the class.
class Systems {
    static var n:Int = 1;

    public static function label():String
        return "v1";

    public static function tick():Int
        return n + 1;

    static var extra:Int;
}

class TestHotReloadStatics {
    static function main() {
        Sys.println("ready");
        var deadline = Sys.time() + 30.0;
        while (Sys.time() < deadline) {
            if (hl.Api.checkReload()) {
                Sys.println("reloaded");
                Sys.println("label " + Systems.label());
                Sys.println("tick " + Systems.tick());
                return;
            }
            Sys.sleep(0.001);
        }
    }
}
