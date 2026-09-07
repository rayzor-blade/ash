/**
	One quad drawn four times, each instance moved and tinted by a second
	vertex buffer that advances per instance rather than per vertex.

	Two buffers with different step modes, and shader locations that carry on
	from the first buffer into the second.
**/
class Instanced {
	static inline var SIZE = 64;
	static inline var ROW = SIZE * 4;

	static var SHADER = "
struct VsOut {
	@builtin(position) pos : vec4<f32>,
	@location(0) tint : vec4<f32>,
}

@vertex
fn vs(@location(0) corner : vec2<f32>, @location(1) offset : vec2<f32>, @location(2) tint : vec4<f32>) -> VsOut {
	var out : VsOut;
	out.pos = vec4<f32>(corner + offset, 0.0, 1.0);
	out.tint = tint;
	return out;
}

@fragment
fn fs(in : VsOut) -> @location(0) vec4<f32> {
	return in.tint;
}
";

	static function bytes(values : Array<Float>) : haxe.io.Bytes {
		var out = haxe.io.Bytes.alloc(values.length * 4);
		for (i in 0...values.length) {
			out.setFloat(i * 4, values[i]);
		}
		return out;
	}

	static function pixel(image : haxe.io.Bytes, x : Int, y : Int) : String {
		var at = y * ROW + x * 4;
		return image.get(at) + "," + image.get(at + 1) + "," + image.get(at + 2) + "," + image.get(at + 3);
	}

	static function main() {
		var instance = wgpu.Instance.create();
		var adapter = instance.adapter();
		var device = adapter.device();
		if (!device.ok) {
			Sys.println("no device");
			Sys.exit(1);
		}
		var queue = device.queue;

		var target = device.texture(SIZE, SIZE, Rgba8Unorm, RenderAttachment | CopySrc);
		var view = target.view();
		var readback = device.buffer(SIZE * ROW, MapRead | CopyDst);

		// A small quad about the origin, the same for every instance.
		var quad = bytes([-0.2, -0.2, 0.2, -0.2, 0.2, 0.2, -0.2, -0.2, 0.2, 0.2, -0.2, 0.2]);
		var corners = device.buffer(quad.length, Vertex | CopyDst);
		queue.write(corners, 0, quad);

		// Where each instance goes, and what colour it is.
		var perInstance = bytes([
			-0.5,  0.5, 1.0, 0.0, 0.0, 1.0,
			 0.5,  0.5, 0.0, 1.0, 0.0, 1.0,
			-0.5, -0.5, 0.0, 0.0, 1.0, 1.0,
			 0.5, -0.5, 1.0, 1.0, 1.0, 1.0
		]);
		var instances = device.buffer(perInstance.length, Vertex | CopyDst);
		queue.write(instances, 0, perInstance);

		var shader = device.shader(SHADER);
		var pipeline = device.pipeline()
			.shader(shader, "vs", "fs")
			.vertexBuffer().attributes(Float32x2)
			.vertexBuffer(0, Instance).attributes(Float32x2, Float32x4)
			.target(Rgba8Unorm)
			.build();

		var encoder = device.encoder();
		encoder.beginRender(view, 0.0, 0.0, 0.0);
		encoder.setPipeline(pipeline);
		encoder.setVertexBuffer(0, corners);
		encoder.setVertexBuffer(1, instances);
		encoder.draw(6, 4);
		encoder.endRender();
		encoder.copyTextureToBuffer(target, readback, SIZE, SIZE, ROW);
		encoder.submit(queue);

		var out = readback.read(device, 0, SIZE * ROW);
		if (out == null) {
			Sys.println("readback failed");
			Sys.exit(1);
		}

		// +y is up in clip space and row 0 is the top, so +0.5 is an early row.
		var checks = [
			{ what: "upper left instance", x: 16, y: 16, want: "255,0,0,255" },
			{ what: "upper right instance", x: 48, y: 16, want: "0,255,0,255" },
			{ what: "lower left instance", x: 16, y: 48, want: "0,0,255,255" },
			{ what: "lower right instance", x: 48, y: 48, want: "255,255,255,255" },
			{ what: "between them is untouched", x: 32, y: 32, want: "0,0,0,255" }
		];

		var failed = 0;
		for (check in checks) {
			var got = pixel(out, check.x, check.y);
			if (got != check.want) {
				Sys.println('  ${check.what}: wanted ${check.want}, got $got');
				failed++;
			}
		}
		Sys.println(failed == 0 ? 'instanced: all ${checks.length} regions exact' : 'instanced: $failed of ${checks.length} wrong');

		pipeline.destroy();
		shader.destroy();
		instances.destroy();
		corners.destroy();
		readback.destroy();
		view.destroy();
		target.destroy();
		device.destroy();
		adapter.destroy();
		instance.destroy();
		Sys.exit(failed == 0 ? 0 : 1);
	}
}
