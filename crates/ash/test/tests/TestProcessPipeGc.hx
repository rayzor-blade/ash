import sys.io.Process;
import sys.thread.Thread;

// A thread parked reading an idle child's stdout must not hold up a
// collection triggered by another thread.
class TestProcessPipeGc {
	static function main() {
		var p = new Process("sleep", ["300"]);
		Thread.create(() -> {
			try {
				p.stdout.readByte();
			} catch (e:Dynamic) {}
		});
		Sys.sleep(0.2);
		var keep:Array<Array<Int>> = [];
		for (i in 0...200000) {
			keep.push([i, i + 1, i + 2]);
			if (keep.length > 5000)
				keep = [];
		}
		p.kill();
		Sys.println("collected");
	}
}
