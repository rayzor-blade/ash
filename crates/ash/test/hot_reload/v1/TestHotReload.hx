class TestHotReload {
    static function getMessage():String {
        return "v1";
    }

    static function main() {
        Sys.println("start " + getMessage());

        // Poll for reload: up to 100 iterations with busy-wait between checks
        var reloaded = false;
        for (i in 0...100) {
            if (hl.Api.checkReload()) {
                reloaded = true;
                Sys.println("reloaded " + getMessage());
                break;
            }
            // Busy-wait ~50ms
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
