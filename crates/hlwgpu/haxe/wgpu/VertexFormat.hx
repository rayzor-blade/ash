package wgpu;

/** The same list, in the same order, as `VERTEX_FORMATS` in `js/prelude.js`. **/
enum abstract VertexFormat(Int) from Int to Int {
	var Float32x2 = 0;
	var Float32x3 = 1;
	var Float32x4 = 2;
	var Uint32 = 3;
}
