/**
	Runs a compute shader and checks every value it wrote.

	Integer arithmetic is bit-reproducible across GPUs, so this asserts exact
	numbers rather than comparing within a tolerance. It also exercises the
	whole of the asynchronous path: a mapping is started, polled until the
	device reports it done, and only then read.
**/
class Compute {
	static inline var COUNT = 256;
	static inline var GROUP = 64;

	static var SHADER = "
@group(0) @binding(0) var<storage, read_write> data : array<u32>;

@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) id : vec3<u32>) {
	if (id.x < arrayLength(&data)) {
		data[id.x] = data[id.x] * 2u + 1u;
	}
}
";

	static function main() {
		var instance = wgpu.Instance.create();
		var adapter = instance.adapter();
		if (!adapter.ok) {
			Sys.println("no adapter");
			Sys.exit(1);
		}
		var device = adapter.device();
		if (!device.ok) {
			Sys.println("no device");
			Sys.exit(1);
		}
		var queue = device.queue;

		var bytes = COUNT * 4;
		var work = device.buffer(bytes, Storage | CopyDst | CopySrc);
		var staging = device.buffer(bytes, MapRead | CopyDst);

		var input = haxe.io.Bytes.alloc(bytes);
		for (i in 0...COUNT) {
			input.setInt32(i * 4, i);
		}
		queue.write(work, 0, input);

		var shader = device.shader(SHADER);
		var pipeline = device.computePipeline(shader);
		var bindings = device.bindGroup(pipeline, 0, [work]);

		var encoder = device.encoder();
		encoder.compute(pipeline, bindings, Std.int(COUNT / GROUP));
		encoder.copyBuffer(work, 0, staging, 0, bytes);
		encoder.submit(queue);

		var out = staging.read(device, 0, bytes);
		if (out == null) {
			Sys.println("readback failed");
			Sys.exit(1);
		}

		var wrong = 0;
		for (i in 0...COUNT) {
			var expected = i * 2 + 1;
			var got = out.getInt32(i * 4);
			if (got != expected) {
				if (wrong < 3) {
					Sys.println('  [$i] expected $expected, got $got');
				}
				wrong++;
			}
		}

		Sys.println(wrong == 0 ? 'compute: all $COUNT values exact' : 'compute: $wrong of $COUNT wrong');

		bindings.destroy();
		pipeline.destroy();
		shader.destroy();
		staging.destroy();
		work.destroy();
		device.destroy();
		adapter.destroy();
		instance.destroy();
		Sys.println("destroyed cleanly");
		Sys.exit(wrong == 0 ? 0 : 1);
	}
}
