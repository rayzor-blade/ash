// GENERATED from `wgpu.api`. Edit the declaration, not this file.

package wgpu;

/**
	The primitives, one to one. A program is not meant to call these:
	the classes beside them are the API. `wgpu.hdll` answers them natively,
	and a host answers them on wasm.
**/
@:keep
class _Native {
	// The entry point. Natively this is a wgpu Instance; in a page it is
	// `navigator.gpu` itself, which is why a page with WebGPU disabled returns 0
	// here rather than failing later with something less obvious.
	@:hlNative("wgpu", "instance_create")
	public static function instance_create() : Int {
		return 0;
	}

	@:hlNative("wgpu", "instance_destroy")
	public static function instance_destroy(inst : Int) : Void {
		return;
	}

	// Returns a request id, not an adapter. `requestAdapter` is a promise in a
	// page and nothing may block on it, so async work is started here and
	// collected through `request_ready` / `request_result`. Natively it resolves
	// before this returns; the id exists anyway, so both look the same.
	// 
	// `power` is 0 for low-power and 1 for high-performance.
	@:hlNative("wgpu", "adapter_request")
	public static function adapter_request(inst : Int, power : Int) : Int {
		return 0;
	}

	@:hlNative("wgpu", "request_ready")
	public static function request_ready(request : Int) : Bool {
		return false;
	}

	// Only meaningful once `request_ready` is true. Reading early raises rather
	// than answering zero, so a caller that polls wrongly finds out here.
	@:hlNative("wgpu", "request_result")
	public static function request_result(request : Int) : Int {
		return 0;
	}

	// UTF-16 and NUL-terminated, which is what `String.fromUCS2` expects.
	@:hlNative("wgpu", "adapter_name")
	public static function adapter_name(adapter : Int) : hl.Bytes {
		return null;
	}

	// 0 unknown, 1 vulkan, 2 metal, 3 dx12, 4 gl, 5 webgpu.
	@:hlNative("wgpu", "adapter_backend")
	public static function adapter_backend(adapter : Int) : Int {
		return 0;
	}

	// One limit by index, so that adding a limit does not add a primitive. The
	// indices are declared once, in Haxe, as `wgpu.Limit`.
	@:hlNative("wgpu", "adapter_limit")
	public static function adapter_limit(adapter : Int, which : Int) : Int {
		return 0;
	}

	@:hlNative("wgpu", "adapter_destroy")
	public static function adapter_destroy(adapter : Int) : Void {
		return;
	}

	// The queue comes back with the device, so `device_queue` needs no request of
	// its own.
	@:hlNative("wgpu", "device_request")
	public static function device_request(adapter : Int) : Int {
		return 0;
	}

	@:hlNative("wgpu", "device_queue")
	public static function device_queue(device : Int) : Int {
		return 0;
	}

	// Lets finished work report itself. A page does this from the event loop, so
	// there is nothing to do there.
	@:hlNative("wgpu", "device_poll")
	public static function device_poll(device : Int) : Void {
		return;
	}

	@:hlNative("wgpu", "device_destroy")
	public static function device_destroy(device : Int) : Void {
		return;
	}

	// `usage` is WebGPU's GPUBufferUsage bitmask, which wgpu numbers identically:
	// 1 MAP_READ, 2 MAP_WRITE, 4 COPY_SRC, 8 COPY_DST, 128 STORAGE.
	@:hlNative("wgpu", "buffer_create")
	public static function buffer_create(device : Int, size : Int, usage : Int) : Int {
		return 0;
	}

	@:hlNative("wgpu", "queue_write_buffer")
	public static function queue_write_buffer(queue : Int, buffer : Int, offset : Int, data : hl.Bytes, len : Int) : Void {
		return;
	}

	// Starts a read mapping. Poll it with `request_ready`, then `buffer_copy_out`.
	@:hlNative("wgpu", "buffer_map_begin")
	public static function buffer_map_begin(device : Int, buffer : Int, offset : Int, size : Int) : Int {
		return 0;
	}

	// Copies from a mapped buffer into `out`. False if it was not mapped.
	@:hlNative("wgpu", "buffer_copy_out")
	public static function buffer_copy_out(buffer : Int, offset : Int, out : hl.Bytes, len : Int) : Bool {
		return false;
	}

	@:hlNative("wgpu", "buffer_unmap")
	public static function buffer_unmap(buffer : Int) : Void {
		return;
	}

	@:hlNative("wgpu", "buffer_destroy")
	public static function buffer_destroy(buffer : Int) : Void {
		return;
	}

	@:hlNative("wgpu", "shader_create")
	public static function shader_create(device : Int, wgsl : hl.Bytes) : Int {
		return 0;
	}

	@:hlNative("wgpu", "shader_destroy")
	public static function shader_destroy(shader : Int) : Void {
		return;
	}

	// Layout is inferred from the shader, so a bind group only needs its buffers.
	@:hlNative("wgpu", "compute_pipeline_create")
	public static function compute_pipeline_create(device : Int, shader : Int, entry : hl.Bytes) : Int {
		return 0;
	}

	@:hlNative("wgpu", "pipeline_destroy")
	public static function pipeline_destroy(pipeline : Int) : Void {
		return;
	}

	// `buffers` is `count` handles, one per binding, in binding order.
	@:hlNative("wgpu", "bind_group_create")
	public static function bind_group_create(device : Int, pipeline : Int, group : Int, buffers : hl.Bytes, count : Int) : Int {
		return 0;
	}

	@:hlNative("wgpu", "bind_group_destroy")
	public static function bind_group_destroy(bindgroup : Int) : Void {
		return;
	}

	@:hlNative("wgpu", "encoder_create")
	public static function encoder_create(device : Int) : Int {
		return 0;
	}

	// A whole compute pass: one pipeline, one bind group, one dispatch. Passes
	// with several dispatches get their own primitives when something needs them.
	@:hlNative("wgpu", "encoder_compute")
	public static function encoder_compute(encoder : Int, pipeline : Int, bindgroup : Int, x : Int, y : Int, z : Int) : Void {
		return;
	}

	@:hlNative("wgpu", "encoder_copy_buffer")
	public static function encoder_copy_buffer(encoder : Int, src : Int, src_offset : Int, dst : Int, dst_offset : Int, size : Int) : Void {
		return;
	}

	// Finishes the encoder and submits it. The encoder is spent afterwards.
	@:hlNative("wgpu", "encoder_submit")
	public static function encoder_submit(encoder : Int, queue : Int) : Void {
		return;
	}

	// Finishes when everything submitted so far has run. Takes the device because
	// natively the callback fires only when one is polled, and polling is what
	// `request_ready` does while waiting.
	@:hlNative("wgpu", "queue_work_done")
	public static function queue_work_done(device : Int, queue : Int) : Int {
		return 0;
	}

}
