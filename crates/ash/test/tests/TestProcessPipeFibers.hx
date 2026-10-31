import sys.io.Process;
import sys.thread.Thread;

// A thread parked reading an idle child's stdout must not stop a sibling
// thread from running, even when both share one scheduler thread.
class TestProcessPipeFibers {
	static var ticks = 0;

	static function main() {
		var p = new Process("sleep", ["300"]);
		Thread.create(() -> {
			try {
				p.stdout.readByte();
			} catch (e:Dynamic) {}
		});
		Thread.create(() -> {
			while (true) {
				ticks++;
				Sys.sleep(0.01);
			}
		});
		Sys.sleep(0.5);
		var seen = ticks;
		p.kill();
		Sys.println(seen >= 5 ? "ran" : "starved " + seen);
	}
}
