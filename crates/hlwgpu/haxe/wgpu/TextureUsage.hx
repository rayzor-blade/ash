package wgpu;

/** WebGPU's GPUTextureUsage bits, which wgpu numbers identically. **/
enum abstract TextureUsage(Int) from Int to Int {
	var CopySrc = 1;
	var CopyDst = 2;
	var TextureBinding = 4;
	var StorageBinding = 8;
	var RenderAttachment = 16;

	@:op(A | B)
	static inline function or(a : TextureUsage, b : TextureUsage) : TextureUsage {
		return cast((a : Int) | (b : Int));
	}
}
