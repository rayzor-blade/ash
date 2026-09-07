package wgpu;

/** The same list, in the same order, as `FORMATS` in `js/prelude.js`. **/
enum abstract TextureFormat(Int) from Int to Int {
	var Rgba8Unorm = 0;
	var Bgra8Unorm = 1;
	var Rgba8UnormSrgb = 2;
	var Depth32Float = 3;
}
