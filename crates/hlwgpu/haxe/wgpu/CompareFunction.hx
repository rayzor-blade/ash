// GENERATED from `spec/webgpu.idl` via `wgpu.api`. Edit neither.

package wgpu;

/** `CompareFunction` from the WebGPU IDL: its values, in its order. **/
enum abstract CompareFunction(Int) from Int to Int {
	var Never = 0;
	var Less = 1;
	var Equal = 2;
	var LessEqual = 3;
	var Greater = 4;
	var NotEqual = 5;
	var GreaterEqual = 6;
	var Always = 7;
}
