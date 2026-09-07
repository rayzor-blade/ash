package wgpu;

/** What a render pass draws into. **/
abstract TextureView(Int) from Int to Int {
	public var ok(get, never) : Bool;

	inline function get_ok() : Bool {
		return this != 0;
	}

	public inline function destroy() : Void {
		_Native.view_destroy(this);
		this = 0;
	}
}
