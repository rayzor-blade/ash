package wgpu;

/** A render pipeline. Its bind group layout is inferred from the shader. **/
abstract RenderPipeline(Int) from Int to Int {
	public var ok(get, never) : Bool;

	inline function get_ok() : Bool {
		return this != 0;
	}

	public inline function destroy() : Void {
		_Native.render_pipeline_destroy(this);
		this = 0;
	}
}
