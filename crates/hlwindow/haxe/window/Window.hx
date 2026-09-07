package window;

/**
	A native window.

	Reports the raw handles a graphics library needs to draw into it, so one
	can be handed this directly.
**/
// A class rather than an abstract over the handle: an abstract carries its
// methods but does not unify structurally, and satisfying a graphics
// library's window shape is the whole point. One window costs one allocation.
class Window {
	var handle : Int;

	function new(handle : Int) {
		this.handle = handle;
	}

	public static function open(title : String, width : Int, height : Int) : Window {
		return new Window(_Native.open(@:privateAccess title.bytes, width, height));
	}

	public var ok(get, never) : Bool;

	function get_ok() : Bool {
		return handle != 0;
	}

	/**
		In pixels, not in the units `open` took: a 640x360 window is 1280x720
		on a Retina display, and that is the size a surface wants.
	**/
	public var width(get, never) : Int;

	function get_width() : Int {
		return _Native.width(handle);
	}

	public var height(get, never) : Int;

	function get_height() : Int {
		return _Native.height(handle);
	}

	/** Drains what is waiting. Never blocks. **/
	public function poll() : Events {
		return _Native.poll(handle);
	}

	/** Which set of raw handles `surfaceHandle` reports. **/
	public function surfacePlatform() : Int {
		return _Native.platform(handle);
	}

	/** A field of the raw handle: 0 and 1 the window's, 2 and 3 the display's. **/
	public function surfaceHandle(which : Int) : haxe.Int64 {
		return _Native.raw(handle, which);
	}

	public function close() : Void {
		_Native.close(handle);
		handle = 0;
	}
}
