/**
	One draw writing two colour targets at once.

	A deferred renderer needs this: colour, normals and depth come out of a
	single pass. The two targets get different colours, so reading both back
	shows each went where the fragment shader said and not to the same place.
**/
class MultiTarget {
	static inline var SIZE = 64;
	static inline var ROW = SIZE * 4;

	static var SHADER = "
@vertex
fn vs(@location(0) pos : vec2<f32>) -> @builtin(position) vec4<f32> {
	return vec4<f32>(pos, 0.0, 1.0);
}

struct Targets {
	@location(0) first : vec4<f32>,
	@location(1) second : vec4<f32>,
}

@fragment
fn fs() -> Targets {
	var out : Targets;
	out.first = vec4<f32>(1.0, 0.0, 0.0, 1.0);
	out.second = vec4<f32>(0.0, 0.0, 1.0, 1.0);
	return out;
}
";

	static function main() {
		var instance = wgpu.Instance.create();
		var adapter = instance.adapter();
		var device = adapter.device();
		if (!device.ok) {
			Sys.println("no device");
			Sys.exit(1);
		}
		var queue = device.queue;

		var first = device.texture(SIZE, SIZE, Rgba8Unorm, RenderAttachment | CopySrc);
		var second = device.texture(SIZE, SIZE, Rgba8Unorm, RenderAttachment | CopySrc);
		var firstView = first.view();
		var secondView = second.view();
		var readback = device.buffer(SIZE * ROW, MapRead | CopyDst);

		var corners = [-1.0, -1.0, 1.0, -1.0, 1.0, 1.0, -1.0, -1.0, 1.0, 1.0, -1.0, 1.0];
		var data = haxe.io.Bytes.alloc(corners.length * 4);
		for (i in 0...corners.length) {
			data.setFloat(i * 4, corners[i]);
		}
		var vertices = device.buffer(data.length, Vertex | CopyDst);
		queue.write(vertices, 0, data);

		var shader = device.shader(SHADER);
		var pipeline = device.pipeline()
			.shader(shader, "vs", "fs")
			.vertexBuffer().attributes(Float32x2)
			.target(Rgba8Unorm)
			.target(Rgba8Unorm)
			.build();

		var encoder = device.encoder();
		encoder.renderPass()
			.colour(firstView, 0.0, 0.0, 0.0)
			.colour(secondView, 0.0, 0.0, 0.0)
			.begin();
		encoder.setPipeline(pipeline);
		encoder.setVertexBuffer(0, vertices);
		encoder.draw(6);
		encoder.endRender();
		encoder.submit(queue);

		function centreOf(texture : wgpu.Texture) : String {
			var copier = device.encoder();
			copier.copyTextureToBuffer(texture, readback, SIZE, SIZE, ROW);
			copier.submit(queue);
			var image = readback.read(device, 0, SIZE * ROW);
			if (image == null) {
				return "unreadable";
			}
			var at = (SIZE >> 1) * ROW + (SIZE >> 1) * 4;
			return image.get(at) + "," + image.get(at + 1) + "," + image.get(at + 2) + "," + image.get(at + 3);
		}

		var failed = 0;
		for (check in [{ what: "location 0", got: centreOf(first), want: "255,0,0,255" },
			{ what: "location 1", got: centreOf(second), want: "0,0,255,255" }]) {
			if (check.got != check.want) {
				Sys.println('  ${check.what}: wanted ${check.want}, got ${check.got}');
				failed++;
			}
		}
		Sys.println(failed == 0 ? "multitarget: one draw filled both targets, exactly"
			: 'multitarget: $failed of 2 wrong');

		pipeline.destroy();
		shader.destroy();
		vertices.destroy();
		readback.destroy();
		secondView.destroy();
		firstView.destroy();
		second.destroy();
		first.destroy();
		device.destroy();
		adapter.destroy();
		instance.destroy();
		Sys.exit(failed == 0 ? 0 : 1);
	}
}
