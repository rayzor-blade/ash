import ash.Future;

class TestAshFuture {
	static function require(ok:Bool, message:String):Void {
		if (!ok) throw message;
	}

	static function main():Void {
		var ready = new Future<Int>();
		require(!ready.isReady(), "new future is ready");
		require(ready.resolve(42), "first resolution");
		require(ready.isReady() && !ready.isRejected(), "resolved state");
		require(ready.await() == 42, "ready before await");
		require(!ready.resolve(7) && !ready.reject("late"), "duplicate completion");

		var pending = new Future<String>();
		var gate = new sys.thread.Lock();
		sys.thread.Thread.create(() -> {
			gate.wait();
			pending.resolve("done");
		});
		hl.Gc.major();
		gate.release();
		require(pending.await() == "done", "await before ready");

		var rooted = new Future<{value:String}>();
		rooted.resolve({value: "retained"});
		hl.Gc.major();
		require(rooted.await().value == "retained", "resolved value survives GC");

		var rejected = new Future<Int>();
		require(rejected.reject("failed"), "reject");
		hl.Gc.major();
		var caught = false;
		try rejected.await() catch (e:Dynamic) caught = Std.string(e) == "failed";
		require(caught && rejected.isRejected(), "rejection observed");

		var continued = new Future<Int>();
		var signal = new sys.thread.Lock();
		continued.then(v -> { if (v == 9) signal.release(); });
		require(continued.resolve(9), "continuation resolution");
		require(signal.wait(2), "continuation ran");
		Sys.println("future ok");
	}
}
