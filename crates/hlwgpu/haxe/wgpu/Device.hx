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

	/** Buffers, texture views and samplers in binding order, for `group`. **/
	public function bindGroup(pipeline : Pipeline, group : Int, bound : Array<Binding>) : BindGroup {
		var packed = haxe.io.Bytes.alloc(bound.length * 4);
		for (i in 0...bound.length) {
			packed.setInt32(i * 4, bound[i]);
		}
		return _Native.bind_group_create(this, pipeline, group, @:privateAccess packed.b, bound.length);
	}

	public inline function sampler(filter : FilterMode = Nearest, address : AddressMode = ClampToEdge) : Sampler {
		return _Native.sampler_create(this, filter, address);
	}

	public inline function texture(width : Int, height : Int, format : TextureFormat, usage : TextureUsage) : Texture {
		return _Native.texture_create(this, width, height, format, usage);
	}

	/** Starts a render pipeline. See `PipelineBuilder`. **/
	public inline function pipeline() : PipelineBuilder {
		return _Native.pipeline_begin(this);
	}

	/** One vertex buffer and one colour target, which is the common case. **/
	public function renderPipeline(module : Shader, vertex : String, fragment : String, format : TextureFormat, stride : Int,
			attributes : Array<VertexAttribute>) : RenderPipeline {
		var layout = pipeline().shader(module, vertex, fragment).vertexBuffer(stride);
		for (a in attributes) {
			layout.attribute(a.format, a.offset, a.location);
		}
		return layout.target(format).build();
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
