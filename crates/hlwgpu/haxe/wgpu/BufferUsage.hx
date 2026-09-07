package wgpu;

/** WebGPU's GPUBufferUsage bits, which wgpu numbers identically. **/
enum abstract BufferUsage(Int) from Int to Int {
	var MapRead = 1;
	var MapWrite = 2;
	var CopySrc = 4;
	var CopyDst = 8;
	var Index = 16;
	var Vertex = 32;
	var Uniform = 64;
	var Storage = 128;
	var Indirect = 256;
	var QueryResolve = 512;

	@:op(A | B)
	static inline function or(a : BufferUsage, b : BufferUsage) : BufferUsage {
		return cast((a : Int) | (b : Int));
	}
}
