package wgpu;

/**
	Anything that can report where a native window is.

	Whatever created your window matches this if it implements both methods.
**/
typedef WindowSource = {
	/** Which set of raw handles `surfaceHandle` reports. **/
	function surfacePlatform() : Int;

	/** A field of the raw handle: 0 and 1 the window's, 2 and 3 the display's. **/
	function surfaceHandle(which : Int) : haxe.Int64;
}
