package wgpu;

/** How one field of a vertex is stored. **/
enum abstract VertexFormat(Int) from Int to Int {
	var Float32x2 = 0;
	var Float32x3 = 1;
	var Float32x4 = 2;
	var Uint32 = 3;
}
