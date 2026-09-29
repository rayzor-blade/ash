import ash.Future;

class TestAshFutureNative {
	@:hlNative("future_test", "create")
	static function create():Future<Dynamic> return null;

	@:hlNative("future_test", "resolve")
	static function resolve(f:Future<Dynamic>, value:Dynamic):Bool return false;

	@:hlNative("future_test", "reject")
	static function reject(f:Future<Dynamic>, error:Dynamic):Bool return false;

	@:hlNative("future_test", "create_abandoned")
	static function createAbandoned():Bool return false;

	@:hlNative("future_test", "finish_abandoned")
	static function finishAbandoned():Bool return false;

	static function require(ok:Bool):Void {
		if (!ok) throw "future C ABI failed";
	}

	static function main():Void {
		var ready = create();
		require(resolve(ready, "ready"));
		hl.Gc.major();
		require(ready.await() == "ready");
		require(!resolve(ready, "late") && !reject(ready, "late"));

		var pending = create();
		var gate = new sys.thread.Lock();
		sys.thread.Thread.create(() -> {
			gate.wait();
			resolve(pending, "pending");
		});
		hl.Gc.major();
		require(!pending.isReady());
		gate.release();
		require(pending.await() == "pending");

		var rejected = create();
		require(reject(rejected, "failed"));
		hl.Gc.major();
		var caught = false;
		try rejected.await() catch (e:Dynamic) caught = e == "failed";
		require(caught);
		require(rejected.isRejected());

		require(createAbandoned());
		hl.Gc.major();
		require(finishAbandoned());

		var continued = create();
		var signal = new sys.thread.Lock();
		continued.then(value -> {
			if (value == "continued") signal.release();
		});
		require(resolve(continued, "continued"));
		require(signal.wait(2));
		Sys.println("future C ABI ok");
	}
}
