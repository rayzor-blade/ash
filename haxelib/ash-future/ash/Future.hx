package ash;

/** A value completed once by Ash native code or a wasm side module. */
abstract Future<T>(hl.Abstract<"ash_future">) {
	/** Create a pending future. A native library may also call hlp_future_create. */
	public inline function new()
		this = create();

	/** Park the current Ash fiber until the value or rejection is available. */
	public function await():T
		return cast wait(this);

	public function isReady():Bool
		return state(this) != 0;

	public function isRejected():Bool
		return state(this) == 2;

	/** Attach a continuation without running Haxe code on a native callback thread. */
	public function then(onValue:T->Void, ?onError:Dynamic->Void):Void {
		var self:Future<T> = cast this;
		sys.thread.Thread.create(() -> {
			var value:T;
			try {
				value = self.await();
			} catch (e:Dynamic) {
				if (onError != null)
					onError(e);
				else
					throw e;
				return;
			}
			onValue(value);
		});
	}

	/** Returns false when this future was already completed. */
	public function resolve(value:T):Bool
		return complete(this, cast value);

	/** Returns false when this future was already completed. */
	public function reject(error:Dynamic):Bool
		return fail(this, error);

	@:hlNative("std", "future_create")
	static function create():hl.Abstract<"ash_future"> return null;

	@:hlNative("std", "future_resolve")
	static function complete(f:hl.Abstract<"ash_future">, value:Dynamic):Bool return false;

	@:hlNative("std", "future_reject")
	static function fail(f:hl.Abstract<"ash_future">, error:Dynamic):Bool return false;

	@:hlNative("std", "future_state")
	static function state(f:hl.Abstract<"ash_future">):Int return 0;

	@:hlNative("std", "future_await")
	static function wait(f:hl.Abstract<"ash_future">):Dynamic return null;
}
