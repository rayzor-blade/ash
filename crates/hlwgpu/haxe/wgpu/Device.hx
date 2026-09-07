package wgpu;

/** An open GPU. Everything else is made from one. **/
abstract Device(Int) from Int to Int {
	public inline function new(handle : Int) {
		this = handle;
	}

	public var ok(get, never) : Bool;

	inline function get_ok() : Bool {
		return this != 0;
	}

	public var queue(get, never) : Queue;

	inline function get_queue() : Queue {
		return _Native.device_queue(this);
	}

	public inline function buffer(size : Int, usage : BufferUsage) : Buffer {
		return _Native.buffer_create(this, size, usage);
	}

	public inline function shader(wgsl : String) : Shader {
		return _Native.shader_create(this, @:privateAccess wgsl.bytes);
	}

	public inline function computePipeline(module : Shader, entry = "main") : ComputePipeline {
		return _Native.compute_pipeline_create(this, module, @:privateAccess entry.bytes);
	}

	/** Buffers in binding order, for `group`. **/
	public function bindGroup(pipeline : ComputePipeline, group : Int, buffers : Array<Buffer>) : BindGroup {
		var packed = haxe.io.Bytes.alloc(buffers.length * 4);
		for (i in 0...buffers.length) {
			packed.setInt32(i * 4, buffers[i]);
		}
		return _Native.bind_group_create(this, pipeline, group, @:privateAccess packed.b, buffers.length);
	}

	public inline function encoder() : Encoder {
		return _Native.encoder_create(this);
	}

	/** Lets finished work report itself. **/
	public inline function poll() : Void {
		_Native.device_poll(this);
	}

	public inline function destroy() : Void {
		_Native.device_destroy(this);
		this = 0;
	}
}
