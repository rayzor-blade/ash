package wgpu;

/** How the texels of a texture are stored. **/
enum abstract TextureFormat(Int) from Int to Int {
	var Rgba8Unorm = 0;
	var Bgra8Unorm = 1;
	var Rgba8UnormSrgb = 2;
	var Depth32Float = 3;
	var Bgra8UnormSrgb = 4;

	/** What `Surface.preferredFormat` answers for one we have no name for. **/
	var Unknown = -1;
}
