// GENERATED from `window.api`. Edit the declaration, not this file.

package window;

/**
	The primitives, one to one. A program is not meant to call these:
	the classes beside them are the API. `window.hdll` answers them natively,
	and a host answers them on wasm.
**/
@:keep
class _Native {
	@:hlNative("window", "open")
	public static function open(title : hl.Bytes, width : Int, height : Int) : Int {
		return 0;
	}

	// Drains the events waiting and returns what happened: 1 if the window was
	// asked to close, 2 if it was resized. Never blocks.
	@:hlNative("window", "poll")
	public static function poll(window : Int) : Int {
		return 0;
	}

	@:hlNative("window", "width")
	public static function width(window : Int) : Int {
		return 0;
	}

	@:hlNative("window", "height")
	public static function height(window : Int) : Int {
		return 0;
	}

	// Which set of raw handles `raw` reports: 1 AppKit, 2 Win32, 3 Xlib,
	// 4 Wayland, 0 none this library knows.
	@:hlNative("window", "platform")
	public static function platform(window : Int) : Int {
		return 0;
	}

	// A pointer-sized field of the raw handle, by index: 0 and 1 are the window's,
	// 2 and 3 the display's. What each means depends on `platform`, and hlwgpu is
	// what puts them back together.
	@:hlNative("window", "raw")
	public static function raw(window : Int, which : Int) : haxe.Int64 {
		return 0;
	}

	@:hlNative("window", "close")
	public static function close(window : Int) : Void {
		return;
	}

}
