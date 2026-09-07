// GENERATED from `spec/webgpu.idl` via `wgpu.api`. Edit neither.

package wgpu;

/** `PrimitiveTopology` from the WebGPU IDL: its values, in its order. **/
enum abstract PrimitiveTopology(Int) from Int to Int {
	var PointList = 0;
	var LineList = 1;
	var LineStrip = 2;
	var TriangleList = 3;
	var TriangleStrip = 4;
}
