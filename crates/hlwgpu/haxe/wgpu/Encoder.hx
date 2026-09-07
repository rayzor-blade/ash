package wgpu;

/** Records commands. Spent once submitted: finishing one twice is not legal. **/
abstract Encoder(Int) from Int to Int {
	/** One pass with one pipeline, one bind group and one dispatch. **/
	public inline function compute(pipeline : ComputePipeline, bindings : BindGroup, x : Int, y = 1, z = 1) : Void {
		_Native.encoder_compute(this, pipeline, bindings, x, y, z);
	}

	public inline function copyBuffer(src : Buffer, srcOffset : Int, dst : Buffer, dstOffset : Int, size : Int) : Void {
		_Native.encoder_copy_buffer(this, src, srcOffset, dst, dstOffset, size);
	}

	/** Opens a pass that clears `target` and keeps what is drawn into it. **/
	public inline function beginRender(target : TextureView, r : Float, g : Float, b : Float, a = 1.0) : Void {
		_Native.encoder_render_begin(this, target, r, g, b, a);
	}

	public inline function setPipeline(pipeline : RenderPipeline) : Void {
		_Native.render_set_pipeline(this, pipeline);
	}

	public inline function setVertexBuffer(slot : Int, buffer : Buffer) : Void {
		_Native.render_set_vertex_buffer(this, slot, buffer);
	}

	public inline function draw(vertices : Int, instances = 1) : Void {
		_Native.render_draw(this, vertices, instances);
	}

	public inline function endRender() : Void {
		_Native.encoder_render_end(this);
	}

	/** `bytesPerRow` must be a multiple of 256. That is WebGPU's rule. **/
	public inline function copyTextureToBuffer(texture : Texture, buffer : Buffer, width : Int, height : Int, bytesPerRow : Int) : Void {
		_Native.encoder_copy_texture_to_buffer(this, texture, buffer, width, height, bytesPerRow);
	}

	public inline function submit(queue : Queue) : Void {
		_Native.encoder_submit(this, queue);
		this = 0;
	}
}
