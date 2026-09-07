package wgpu;

/**
	A render pipeline under construction, before it has shaders.

	The builder's type changes as it is filled in, so the compiler enforces
	what the primitives cannot check for themselves: an attribute needs an open
	vertex buffer, a blend needs an open colour target, and a pipeline needs a
	shader and somewhere to draw before it can be built. Each state is an
	abstract over the same handle, so the chain costs nothing at runtime.
**/
abstract PipelineBuilder(Int) from Int to Int {
	/** Both stages, usually from one WGSL module. **/
	public inline function shader(module : Shader, vertex : String, fragment : String) : PipelineStages {
		_Native.pipeline_shader(this, module, @:privateAccess vertex.bytes, @:privateAccess fragment.bytes);
		return cast this;
	}
}
