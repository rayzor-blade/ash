import sys.thread.Thread;
// A thread parked reading standard input must not stop a sibling thread from
// running, even when both share one scheduler thread.
class TestStdinFibers {
	static var ticks = 0;
	static function main() {
		Thread.create(() -> {
			try {
				Sys.stdin().readByte();
			} catch (e:Dynamic) {}
		});
		Thread.create(() -> {
			while (true) {
				ticks++;
				Sys.sleep(0.01);
			}
		});
		Sys.sleep(0.5);
		Sys.println(ticks >= 5 ? "ran" : "starved " + ticks);
		Sys.exit(0);
	}
}
