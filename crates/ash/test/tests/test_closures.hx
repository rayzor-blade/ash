class Main {
    static function apply(f:Void->Void) {
        f();
    }

    static function testStaticClosure() {
        var f = function() {
            Sys.println("closure ok");
        };
        f();
    }

    static function testCallback() {
        apply(function() {
            Sys.println("callback ok");
        });
    }

    static function main() {
        testStaticClosure();
        testCallback();
        Sys.println("done");
    }
}
