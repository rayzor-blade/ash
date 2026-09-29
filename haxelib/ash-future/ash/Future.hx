package ash;

/** A value completed once by Ash native code or a wasm side module. */
abstract Future<T>(hl.Abstract<"ash_future">) {
	/** Create a pending future. A native library may also call hlp_future_create. */
	public inline function new()
		this = create();

	/** Park the current Ash fiber until the value or rejection is available. */
	public function await():T {
		var value = wait(this);
		#if ash_future_stock
		// HashLink raises with longjmp, which cannot cross Rust frames.
		if (state(this) == 2) throw value;
		#end
		return cast value;
	}

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

	#if ash_future_stock
	@:hlNative("ash_future", "create")
	#else
	@:hlNative("std", "future_create")
	#end
	static function create():hl.Abstract<"ash_future"> return null;

	#if ash_future_stock
	@:hlNative("ash_future", "resolve")
	#else
	@:hlNative("std", "future_resolve")
	#end
	static function complete(f:hl.Abstract<"ash_future">, value:Dynamic):Bool return false;

	#if ash_future_stock
	@:hlNative("ash_future", "reject")
	#else
	@:hlNative("std", "future_reject")
	#end
	static function fail(f:hl.Abstract<"ash_future">, error:Dynamic):Bool return false;

	#if ash_future_stock
	@:hlNative("ash_future", "state")
	#else
	@:hlNative("std", "future_state")
	#end
	static function state(f:hl.Abstract<"ash_future">):Int return 0;

	#if ash_future_stock
	@:hlNative("ash_future", "await")
	#else
	@:hlNative("std", "future_await")
	#end
	static function wait(f:hl.Abstract<"ash_future">):Dynamic return null;
}
