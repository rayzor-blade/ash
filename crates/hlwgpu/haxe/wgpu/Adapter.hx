package wgpu;

/**
	A GPU the machine offers.

	Destroy it when finished: ash runs no finalizers, so nothing else will.
**/
abstract Adapter(Int) from Int to Int {
	public inline function new(handle : Int) {
		this = handle;
	}

	public var ok(get, never) : Bool;

	inline function get_ok() : Bool {
		return this != 0;
	}

	/** What the driver calls this device. **/
	public var name(get, never) : String;

	function get_name() : String {
		var bytes = _Native.adapter_name(this);
		return bytes == null ? "" : @:privateAccess String.fromUCS2(bytes);
	}

	public var backend(get, never) : Backend;

	inline function get_backend() : Backend {
		return _Native.adapter_backend(this);
	}

	/** Starts opening the device. **/
	public inline function requestDevice() : Request {
		return new Request(_Native.device_request(this));
	}

	/** Waits for the device. **/
	public inline function device() : Device {
		return new Device(requestDevice().await());
	}

	public inline function limit(which : Limit) : Int {
		return _Native.adapter_limit(this, which);
	}

	public inline function destroy() : Void {
		_Native.adapter_destroy(this);
		this = 0;
	}
}
