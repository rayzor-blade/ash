/**
	Draws a triangle into an offscreen texture and checks the pixels.

	Every colour here is 0 or 1 in each channel, so the unorm conversion is
	exact and the expected bytes are the same on any GPU. Only pixels far from
	an edge are checked, since that is where rasterisation is not a matter of
	opinion.
**/
class Render {
	static inline var SIZE = 64;
	// copyTextureToBuffer wants rows a multiple of 256 bytes, and 64 RGBA
	// pixels is exactly one.
	static inline var ROW = SIZE * 4;

	static var SHADER = "
@vertex
fn vs(@location(0) pos : vec2<f32>) -> @builtin(position) vec4<f32> {
	return vec4<f32>(pos, 0.0, 1.0);
}

@fragment
fn fs() -> @location(0) vec4<f32> {
	return vec4<f32>(1.0, 0.0, 0.0, 1.0);
}
";

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

		// A triangle around the middle, well clear of every corner.
		var corners = [-0.8, -0.8, 0.8, -0.8, 0.0, 0.8];
		var vertexData = haxe.io.Bytes.alloc(corners.length * 4);
		for (i in 0...corners.length) {
			vertexData.setFloat(i * 4, corners[i]);
		}
		var vertices = device.buffer(vertexData.length, Vertex | CopyDst);
		queue.write(vertices, 0, vertexData);

		var shader = device.shader(SHADER);
		var pipeline = device.renderPipeline(shader, "vs", "fs", Rgba8Unorm, 8, [
			{ format: Float32x2, offset: 0, location: 0 }
		]);

		var encoder = device.encoder();
		encoder.beginRender(view, 0.0, 0.0, 1.0);
		encoder.setPipeline(pipeline);
		encoder.setVertexBuffer(0, vertices);
		encoder.draw(3);
		encoder.endRender();
		encoder.copyTextureToBuffer(target, readback, SIZE, SIZE, ROW);
		encoder.submit(queue);

		var image = readback.read(device, 0, SIZE * ROW);
		if (image == null) {
			Sys.println("readback failed");
			Sys.exit(1);
		}

		var checks = [
			{ what: "top-left is the clear colour", x: 0, y: 0, want: "0,0,255,255" },
			{ what: "top-right is the clear colour", x: SIZE - 1, y: 0, want: "0,0,255,255" },
			{ what: "centre is the triangle", x: SIZE >> 1, y: SIZE >> 1, want: "255,0,0,255" },
			{ what: "below centre is the triangle", x: SIZE >> 1, y: (SIZE >> 1) + 8, want: "255,0,0,255" }
		];

		var failed = 0;
		for (check in checks) {
			var got = pixel(image, check.x, check.y);
			if (got != check.want) {
				Sys.println('  ${check.what}: wanted ${check.want}, got $got');
				failed++;
			}
		}
		Sys.println(failed == 0 ? 'render: all ${checks.length} pixels exact' : 'render: $failed of ${checks.length} wrong');

		pipeline.destroy();
		shader.destroy();
		vertices.destroy();
		readback.destroy();
		view.destroy();
		target.destroy();
		device.destroy();
		adapter.destroy();
		instance.destroy();
		Sys.println("destroyed cleanly");
		Sys.exit(failed == 0 ? 0 : 1);
	}
}
