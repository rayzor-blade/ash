package wgpu;

/** What the adapter is running on. `WebGpu` is the only one a page reports. **/
enum abstract Backend(Int) from Int to Int {
	var Unknown = 0;
	var Vulkan = 1;
	var Metal = 2;
	var Dx12 = 3;
	var Gl = 4;
	var WebGpu = 5;

	public function toString() : String {
		return switch (cast this : Backend) {
			case Vulkan: "Vulkan";
			case Metal: "Metal";
			case Dx12: "D3D12";
			case Gl: "OpenGL";
			case WebGpu: "WebGPU";
			case Unknown: "unknown";
		}
	}
}
