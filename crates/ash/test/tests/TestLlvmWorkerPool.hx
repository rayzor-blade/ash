import sys.thread.FixedThreadPool;
import sys.thread.Lock;

class TestLlvmWorkerPool {
	static var values = new hl.NativeArray<Float>(4096);

	static function work(start:Int, end:Int):Void {
		for (round in 0...10) {
			for (i in start...end) {
				var x = values[i];
				values[i] = x + Math.sqrt(x * x + 1.0) * 0.001;
			}
		}
	}

	static function run(workers:Int):Void {
		var pool = new FixedThreadPool(workers);
		var done = new Lock();
		for (round in 0...20) {
			for (worker in 0...workers) {
				var from = worker * 1024;
				pool.run(() -> {
					work(from, from + 1024);
					done.release();
				});
			}
			for (worker in 0...workers) done.wait();
		}
		pool.shutdown();
		Sys.println('pool $workers ${values[42]}');
	}

	static function main():Void {
		for (i in 0...4096) values[i] = i * 0.001;
		run(1);
		run(2);
		run(4);
	}
}
