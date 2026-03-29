class TestHotReload {
    static function getMessage():String {
        return "v2";
    }

    static function main() {
        Sys.println("start " + getMessage());

        var reloaded = false;
        for (i in 0...100) {
            if (hl.Api.checkReload()) {
                reloaded = true;
                Sys.println("reloaded " + getMessage());
                break;
            }
            var j = 0;
            while (j < 1000000) {
                j++;
            }
        }

        if (!reloaded) {
            Sys.println("no-reload " + getMessage());
        }

        Sys.println("done " + getMessage());
    }
}
