import sys.thread.Thread;

// An exception that escapes a thread body ends that thread only.
class TestThreadThrow {
	static function main() {
		Thread.create(() -> {
			Sys.println("in thread");
			throw "thread body threw";
		});
		Sys.sleep(0.3);
		Sys.println("survived");
	}
}
