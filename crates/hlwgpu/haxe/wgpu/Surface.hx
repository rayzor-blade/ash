package wgpu;

/** Somewhere frames are shown, rather than read back. **/
abstract Surface(Int) from Int to Int {
	public inline function new(handle : Int) {
		this = handle;
	}

	/** From a native window. Zero if the platform is one we cannot address. **/
	public static function fromWindow(instance : Instance, window : WindowSource) : Surface {
		return new Surface(_Native.surface_create(instance, window.surfacePlatform(), window.surfaceHandle(0),
			window.surfaceHandle(1), window.surfaceHandle(2), window.surfaceHandle(3)));
	}

	public var ok(get, never) : Bool;

	inline function get_ok() : Bool {
		return this != 0;
	}

	/** What this surface would rather be configured as. **/
	public inline function preferredFormat(adapter : Adapter) : TextureFormat {
		return _Native.surface_preferred_format(this, adapter);
	}

	public inline function configure(device : Device, width : Int, height : Int, format : TextureFormat) : Void {
		_Native.surface_configure(device, this, width, height, format);
	}

	/**
		The view to draw this frame into. Returns 0 if the surface needs
		configuring again, which is usually because the window was resized.
	**/
	public inline function acquire() : TextureView {
		return _Native.surface_acquire(this);
	}

	/** Call after the work drawing the frame has been submitted. **/
	public inline function present(queue : Queue) : Void {
		_Native.surface_present(queue, this);
	}

	public inline function destroy() : Void {
		_Native.surface_destroy(this);
		this = 0;
	}
}
