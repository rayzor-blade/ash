package wgpu;

/** Work the GPU was asked for that has not finished yet. **/
abstract Request(Int) from Int to Int {
	public inline function new(id : Int) {
		this = id;
	}

	/** Whether the result can be collected. Never blocks. **/
	public var ready(get, never) : Bool;

	inline function get_ready() : Bool {
		return _Native.request_ready(this);
	}

	/**
		Waits for the result and collects it.

		Yields between checks rather than spinning, so other threads keep
		running and a page stays responsive while the GPU works.
	**/
	public function await() : Int {
		while (!_Native.request_ready(this)) {
			yield();
		}
		return _Native.request_result(this);
	}

	/** Gives up the rest of this time slice. **/
	static inline function yield() : Void {
		Sys.sleep(0);
	}
}
