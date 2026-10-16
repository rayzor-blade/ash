import sys.thread.FixedThreadPool;
import sys.thread.Lock;

// A function with one un-nested loop that is first reached on a pool worker,
// and not called from the main thread until the pool is shut down. Under
// `--mode jit` it must still move from the Cranelift baseline to the top tier.
class TestWorkerPromote {
	static var sink = new haxe.ds.Vector<Float>(8);

	static function job(slot:Int, work:Int):Void {
		var x = 0.0;
		for (i in 0...work)
			x += Math.sqrt(i + slot);
		sink[slot] = x;
	}

	static function main() {
		var workers = 2;
		var work = 20000;
		var pool = new FixedThreadPool(workers);
		var done = new Lock();
		var t0 = Sys.time();
		var rounds = 0;
		while (Sys.time() - t0 < 3.0) {
			for (w in 0...workers)
				pool.run(() -> {
					job(w, work);
					done.release();
				});
			for (_ in 0...workers)
				done.wait();
			rounds++;
		}
		pool.shutdown();
		var pooled = [for (w in 0...workers) sink[w]];
		var same = true;
		for (w in 0...workers) {
			job(w, work);
			if (sink[w] != pooled[w])
				same = false;
		}
		Sys.println(same ? "worker promote ok" : "worker promote mismatch");
	}
}
