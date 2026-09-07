package wgpu;

/** How a shader reads a texture. **/
abstract Sampler(Int) from Int to Int {
	public var ok(get, never) : Bool;

	inline function get_ok() : Bool {
		return this != 0;
	}

	public inline function destroy() : Void {
		_Native.sampler_destroy(this);
		this = 0;
	}
}
