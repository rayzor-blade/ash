class Main {
    static function throwAndCatch() {
        try {
            throw "caught";
        } catch (e:Dynamic) {
            Sys.println("catch ok");
        }
    }

    static function noException() {
        try {
            Sys.println("try ok");
        } catch (e:Dynamic) {
            Sys.println("FAIL");
        }
    }

    static function main() {
        noException();
        throwAndCatch();
        Sys.println("done");
    }
}
