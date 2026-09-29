import ash.Future;

class TestAshFutureStockNative {
	@:hlNative("ash_future", "test_create_abandoned")
	static function createAbandoned():Bool return false;

	@:hlNative("ash_future", "test_finish_abandoned")
	static function finishAbandoned():Bool return false;

	@:hlNative("ash_future", "test_create_foreign")
	static function createForeign():Future<Dynamic> return null;

	static function main():Void {
		if (!createAbandoned()) throw "create abandoned";
		hl.Gc.major();
		if (!finishAbandoned()) throw "pending root lost";

		for (i in 0...32) {
			var foreign = createForeign();
			hl.Gc.major();
			if (foreign.await() != null || !foreign.isReady()) throw 'foreign completion $i';
		}
		hl.Gc.major();
		Sys.println("future stock native ok");
	}
}
