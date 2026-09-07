package wgpu;

/**
	Something started on the GPU that has not finished.

	Waiting happens here rather than inside the primitive. A primitive that
	blocked could not work in a page: a promise settles only once the task
	returns, and a call that never returns is a call that never gets an answer.
	Looping here, in the program's own module, is what the fiber transform can
	suspend.
**/
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
		Waits for the result, then collects it. Yields between polls so a page
		can settle the promise this is waiting on.
	**/
	public function await() : Int {
		while (!_Native.request_ready(this)) {
			yield();
		}
		return _Native.request_result(this);
	}

	/**
		Hands the rest of this slice back. On wasm the fiber transform turns
		this into a suspension, which is what lets the host run; natively it
		just lets another thread go.

		A wait long enough to matter must also tell the collector, or a thread
		parked here never reaches a safepoint. Nothing in this milestone waits
		-- every request is settled before it is handed out -- so there is
		nowhere yet to put that.
	**/
	static inline function yield() : Void {
		Sys.sleep(0);
	}
}
