package wgpu;

/**
	GPU memory.

	Destroy it when finished. Nothing frees it when it goes out of scope.
**/
abstract Buffer(Int) from Int to Int {
	public var ok(get, never) : Bool;

	inline function get_ok() : Bool {
		return this != 0;
	}

	/**
		Reads `length` bytes back from the GPU, waiting for them to arrive.
		Returns null if the buffer could not be mapped.

		The buffer must have been created with `MapRead`.
	**/
	public function read(device : Device, offset : Int, length : Int) : haxe.io.Bytes {
		var mapping = new Request(_Native.buffer_map_begin(device, this, offset, length));
		if (mapping.await() == 0) {
			return null;
		}
		var out = haxe.io.Bytes.alloc(length);
		var copied = _Native.buffer_copy_out(this, offset, @:privateAccess out.b, length);
		_Native.buffer_unmap(this);
		return copied ? out : null;
	}

	public inline function destroy() : Void {
		_Native.buffer_destroy(this);
		this = 0;
	}
}
