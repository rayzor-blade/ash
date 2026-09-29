import sys.thread.FixedThreadPool;
import sys.thread.Lock;

/** Short-job control for worker placement and frame-barrier latency. */
class BenchWorkerBarriers {
	static function batch(pool:FixedThreadPool, done:Lock, jobs:Int):Void {
		for (_ in 0...jobs) pool.run(() -> done.release());
		for (_ in 0...jobs) done.wait();
	}

	static function main():Void {
		var args = Sys.args();
		var jobs = args.length > 0 ? Std.parseInt(args[0]) : 16;
		var frames = args.length > 1 ? Std.parseInt(args[1]) : 20;
		var recreate = args.length > 2 && args[2] == "recreate";
		if (jobs == null || jobs < 1 || frames == null || frames < 1)
			throw "jobs and frames must be positive";

		var pool = new FixedThreadPool(4);
		var done = new Lock();
		batch(pool, done, 4);
		var start = Sys.time();
		for (_ in 0...frames) batch(pool, done, jobs);
		var elapsedMs = (Sys.time() - start) * 1000;
		pool.shutdown();
		if (recreate) {
			var second = new FixedThreadPool(4);
			batch(second, done, 4);
			second.shutdown();
		}
		Sys.println('barrier jobs=$jobs frames=$frames done=${jobs * frames} ms=$elapsedMs recreated=$recreate');
	}
}
