package wgpu;

/** A pipeline with a vertex buffer layout open. **/
abstract PipelineVertexBuffer(Int) from Int to Int {
	/**
		The fields of this buffer, packed one after another at consecutive
		shader locations.

		The usual case, and two fewer numbers per field to get wrong. Use
		`attribute` where the placement is not that.
	**/
	public function attributes(...formats : VertexFormat) : PipelineVertexBuffer {
		for (format in formats) {
			_Native.pipeline_attribute_packed(this, format);
		}
		return this;
	}

	/** One field, placed exactly. **/
	public inline function attribute(format : VertexFormat, offset : Int, location : Int) : PipelineVertexBuffer {
		_Native.pipeline_attribute(this, format, offset, location);
		return this;
	}

	public inline function vertexBuffer(stride = 0, step : VertexStepMode = Vertex) : PipelineVertexBuffer {
		_Native.pipeline_vertex_buffer(this, stride, step);
		return this;
	}

	public inline function target(format : TextureFormat, writeMask = 0xF) : PipelineTargets {
		_Native.pipeline_target(this, format, writeMask);
		return cast this;
	}

	public inline function depth(format : TextureFormat, write = true, compare : CompareFunction = Less) : PipelineVertexBuffer {
		_Native.pipeline_depth(this, format, write, compare);
		return this;
	}

	public inline function primitive(topology : PrimitiveTopology = TriangleList, cull : CullMode = None,
			front : FrontFace = Ccw) : PipelineVertexBuffer {
		_Native.pipeline_primitive(this, topology, cull, front);
		return this;
	}
}
