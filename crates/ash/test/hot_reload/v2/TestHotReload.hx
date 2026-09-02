class TestHotReload {
    static function getMessage():String {
        return "v2";
    }

    static function main() {
        Sys.println("start " + getMessage());

        // Poll on the CLOCK, not on a spin count.
        //
        // This used to busy-wait a million increments per iteration, a number
        // calibrated when the interpreter was the only engine. Under the
        // tiered ladder the whole 100-iteration poll finishes in under a
        // millisecond -- long before the harness has finished writing the new
        // bytecode -- so the program exited having never seen a reload and
        // the test reported that reload was broken. Waiting on wall time
        // makes the window a property of the test rather than of how fast
        // the engine happens to be.
        var reloaded = false;
        var deadline = Sys.time() + 10.0;
        while (Sys.time() < deadline) {
            if (hl.Api.checkReload()) {
                reloaded = true;
                Sys.println("reloaded " + getMessage());
                break;
            }
            Sys.sleep(0.02);
        }

        if (!reloaded) {
            Sys.println("no-reload " + getMessage());
        }

        Sys.println("done " + getMessage());
    }
}
