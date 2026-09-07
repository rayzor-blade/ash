package wgpu;

/** A pipeline with shaders, which still has nothing to draw into. **/
abstract PipelineStages(Int) from Int to Int {
	/**
		Starts describing a vertex buffer. Attributes added next describe
		this one.

		Leave `stride` alone and it is worked out from the attributes, which
		is what you want for a packed layout.
	**/
	public inline function vertexBuffer(stride = 0, step : VertexStepMode = Vertex) : PipelineVertexBuffer {
		_Native.pipeline_vertex_buffer(this, stride, step);
		return cast this;
	}

	/** Adds somewhere to draw. A blend added next applies to this target. **/
	public inline function target(format : TextureFormat, writeMask = 0xF) : PipelineTargets {
		_Native.pipeline_target(this, format, writeMask);
		return cast this;
	}

	public inline function depth(format : TextureFormat, write = true, compare : CompareFunction = Less) : PipelineStages {
		_Native.pipeline_depth(this, format, write, compare);
		return this;
	}

	public inline function primitive(topology : PrimitiveTopology = TriangleList, cull : CullMode = None,
			front : FrontFace = Ccw) : PipelineStages {
		_Native.pipeline_primitive(this, topology, cull, front);
		return this;
	}
}
