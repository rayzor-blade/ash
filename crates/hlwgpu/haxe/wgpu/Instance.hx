package wgpu;

/** The entry point. Zero if this machine or page has no WebGPU at all. **/
abstract Instance(Int) from Int to Int {
	public inline function new(handle : Int) {
		this = handle;
	}

	public static inline function create() : Instance {
		return new Instance(_Native.instance_create());
	}

	public var ok(get, never) : Bool;

	inline function get_ok() : Bool {
		return this != 0;
	}

	/**
		Starts an adapter request. The result is 0 if nothing matched, which is
		a real answer on a machine with no supported GPU.
	**/
	public inline function requestAdapter(highPerformance = true) : Request {
		return new Request(_Native.adapter_request(this, highPerformance ? 1 : 0));
	}

	/** Waits for an adapter. **/
	public inline function adapter(highPerformance = true) : Adapter {
		return new Adapter(requestAdapter(highPerformance).await());
	}

	public inline function destroy() : Void {
		_Native.instance_destroy(this);
		this = 0;
	}
}
