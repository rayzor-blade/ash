package wgpu;

/**
	Anything that can say where a native window is.

	A shape rather than a type, so this library still depends on nothing:
	`hlwindow` happens to fit it, and so would anything else that can report
	the same handles.
**/
typedef WindowSource = {
	function surfacePlatform() : Int;
	function surfaceHandle(which : Int) : haxe.Int64;
}
