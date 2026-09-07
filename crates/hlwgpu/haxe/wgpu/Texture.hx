package wgpu;

/** An image on the GPU. **/
abstract Texture(Int) from Int to Int {
	public var ok(get, never) : Bool;

	inline function get_ok() : Bool {
		return this != 0;
	}

	public inline function view() : TextureView {
		return _Native.texture_view(this);
	}

	public inline function destroy() : Void {
		_Native.texture_destroy(this);
		this = 0;
	}
}
