// Multiple producers, one consumer, over a mutex and a counting semaphore --
// the shape hxDatachannel uses to hand callbacks from libdatachannel's own
// threads to the Haxe main thread.
//
// Haxe threads are cooperative fibers here, so this exercises the fiber side
// of the primitives. The threads a native library starts have no fiber, and
// that path is covered by the `foreign_thread_tests` in std/src/thread.rs.
import sys.thread.Thread;
import sys.thread.Mutex;
import sys.thread.Semaphore;

class TestMpsc {
	static var mutex = new Mutex();
	static var sem = new Semaphore(0);
	static var queue:Array<Int> = [];

	static inline var PRODUCERS = 4;
	static inline var PER = 250;
	static inline var TOTAL = PRODUCERS * PER;

	static function producer(base:Int) {
		for (i in 0...PER) {
			mutex.acquire();
			queue.push(base + i);
			sem.release();
			mutex.release();
		}
	}

	static function main() {
		for (p in 0...PRODUCERS) {
			var base = p * PER;
			Thread.create(() -> producer(base));
		}

		var seen = [for (_ in 0...TOTAL) false];
		var drained = 0;
		while (drained < TOTAL) {
			// Blocking acquire: one permit means one queued item exists.
			sem.acquire();
			mutex.acquire();
			var v = queue.pop();
			mutex.release();
			if (v == null) {
				Sys.println("FAIL a permit outlived its item");
				return;
			}
			if (seen[v]) {
				Sys.println("FAIL item drained twice: " + v);
				return;
			}
			seen[v] = true;
			drained++;
		}

		mutex.acquire();
		var leftover = queue.length;
		mutex.release();
		if (leftover != 0) {
			Sys.println("FAIL queue left non-empty: " + leftover);
			return;
		}
		for (i in 0...TOTAL) {
			if (!seen[i]) {
				Sys.println("FAIL item never arrived: " + i);
				return;
			}
		}
		Sys.println("OK TestMpsc drained=" + drained);
	}
}
