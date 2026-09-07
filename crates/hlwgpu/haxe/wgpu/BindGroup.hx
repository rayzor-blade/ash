package wgpu;

/** The buffers a pipeline reads and writes. **/
abstract BindGroup(Int) from Int to Int {
	public var ok(get, never) : Bool;

	inline function get_ok() : Bool {
		return this != 0;
	}

	public inline function destroy() : Void {
		_Native.bind_group_destroy(this);
		this = 0;
	}
}
