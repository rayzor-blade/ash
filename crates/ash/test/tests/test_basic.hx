class Main {
    static function testPrint() {
        Sys.println("hello");
        Sys.println("world");
    }

    static function testCondition() {
        var x = 5;
        if (x > 3) {
            Sys.println("gt works");
        } else {
            Sys.println("FAIL gt");
        }
        if (x < 10) {
            Sys.println("lt works");
        } else {
            Sys.println("FAIL lt");
        }
    }

    static function testLoop() {
        var i = 0;
        while (i < 3) {
            Sys.println("loop");
            i++;
        }
    }

    static function main() {
        testPrint();
        testCondition();
        testLoop();
        Sys.println("done");
    }
}
