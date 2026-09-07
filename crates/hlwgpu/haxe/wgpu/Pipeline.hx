package wgpu;

/** Either kind of pipeline, for the calls that take both. **/
abstract Pipeline(Int) from Int to Int {
	@:from static inline function ofCompute(v : ComputePipeline) : Pipeline {
		return cast(v : Int);
	}

	@:from static inline function ofRender(v : RenderPipeline) : Pipeline {
		return cast(v : Int);
	}
}
