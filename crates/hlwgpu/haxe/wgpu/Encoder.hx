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

	public inline function submit(queue : Queue) : Void {
		_Native.encoder_submit(this, queue);
		this = 0;
	}
}
