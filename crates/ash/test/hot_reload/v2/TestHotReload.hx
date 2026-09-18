class TestHotReload {
    static function getMessage():String {
        return "v2";
    }

    // Hot enough to be promoted after the reload. A body the tier lowers
    // from the program as it was at startup gives the old sum.
    static function hot(n:Int):Int {
        var s = 0;
        for (i in 0...n) s += i * 2;
        return s;
    }

    // Unchanged between versions and promoted only after the reload, so
    // the tier has to lower it from the program it was pointed at then.
    static function warm(n:Int):Int {
        var s = 0;
        for (i in 0...n) s += i;
        return s;
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

        // Past any promotion threshold, so the ladder compiles `hot` from
        // whatever program the tier holds now and the rest of the loop runs
        // that code.
        var t = 0;
        for (k in 0...5000) t += hot(100) + warm(100);
        Sys.println("after " + hot(10) + " " + t);

        Sys.println("done " + getMessage());
    }
}
