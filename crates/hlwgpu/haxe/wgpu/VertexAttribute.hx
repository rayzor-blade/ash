package wgpu;

/** One field of a vertex, and where the shader reads it. **/
typedef VertexAttribute = {
	var format : VertexFormat;
	var offset : Int;
	var location : Int;
}
