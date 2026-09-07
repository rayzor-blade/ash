package wgpu;

/**
	Anything that can say where a native window is.

	Whatever opened the window fits this if it can answer both calls below.
**/
typedef WindowSource = {
	/** Which set of raw handles `surfaceHandle` reports. **/
	function surfacePlatform() : Int;

	/** A field of the raw handle: 0 and 1 the window's, 2 and 3 the display's. **/
	function surfaceHandle(which : Int) : haxe.Int64;
}
