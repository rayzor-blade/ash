/**
	Two overlapping quads blended additively, built with the fluent builder.

	Additive blending of 0-or-1 channels is exact, so the overlap is yellow to
	the byte on any GPU. Where they do not overlap each keeps its own colour,
	which is what shows the blend is happening rather than the second draw
	simply winning.
**/
class Blend {
	static inline var SIZE = 64;
	static inline var ROW = SIZE * 4;

	static var SHADER = "
struct VsOut {
	@builtin(position) pos : vec4<f32>,
	@location(0) tint : vec4<f32>,
}

@vertex
fn vs(@location(0) pos : vec2<f32>, @location(1) tint : vec4<f32>) -> VsOut {
	var out : VsOut;
	out.pos = vec4<f32>(pos, 0.0, 1.0);
	out.tint = tint;
	return out;
}

@fragment
fn fs(in : VsOut) -> @location(0) vec4<f32> {
	return in.tint;
}
";

	static function pixel(image : haxe.io.Bytes, x : Int, y : Int) : String {
		var at = y * ROW + x * 4;
		return image.get(at) + "," + image.get(at + 1) + "," + image.get(at + 2) + "," + image.get(at + 3);
	}

	/** Two triangles covering x0..x1, all in one colour. **/
	static function quad(into : Array<Float>, x0 : Float, x1 : Float, r : Float, g : Float, b : Float) {
		var corners = [[x0, -1.0], [x1, -1.0], [x1, 1.0], [x0, -1.0], [x1, 1.0], [x0, 1.0]];
		for (c in corners) {
			into.push(c[0]);
			into.push(c[1]);
			into.push(r);
			into.push(g);
			into.push(b);
			into.push(1.0);
		}
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

		var values = new Array<Float>();
		quad(values, -1.0, 0.5, 1.0, 0.0, 0.0); // red, left three quarters
		quad(values, -0.5, 1.0, 0.0, 1.0, 0.0); // green, right three quarters
		var vertexData = haxe.io.Bytes.alloc(values.length * 4);
		for (i in 0...values.length) {
			vertexData.setFloat(i * 4, values[i]);
		}
		var vertices = device.buffer(vertexData.length, Vertex | CopyDst);
		queue.write(vertices, 0, vertexData);

		var shader = device.shader(SHADER);
		// Position then colour, packed: the stride and both offsets follow
		// from the formats, so there is nothing here to get wrong.
		var pipeline = device.pipeline()
			.shader(shader, "vs", "fs")
			.vertexBuffer().attributes(Float32x2, Float32x4)
			.target(Rgba8Unorm).blend(One, One)
			.build();

		var encoder = device.encoder();
		encoder.beginRender(view, 0.0, 0.0, 0.0);
		encoder.setPipeline(pipeline);
		encoder.setVertexBuffer(0, vertices);
		encoder.draw(Std.int(values.length / 6));
		encoder.endRender();
		encoder.copyTextureToBuffer(target, readback, SIZE, SIZE, ROW);
		encoder.submit(queue);

		var out = readback.read(device, 0, SIZE * ROW);
		if (out == null) {
			Sys.println("readback failed");
			Sys.exit(1);
		}

		var mid = SIZE >> 1;
		var checks = [
			{ what: "left is only the red quad", x: 8, want: "255,0,0,255" },
			{ what: "middle is both, added", x: mid, want: "255,255,0,255" },
			{ what: "right is only the green quad", x: SIZE - 8, want: "0,255,0,255" }
		];

		var failed = 0;
		for (check in checks) {
			var got = pixel(out, check.x, mid);
			if (got != check.want) {
				Sys.println('  ${check.what}: wanted ${check.want}, got $got');
				failed++;
			}
		}
		Sys.println(failed == 0 ? 'blend: all ${checks.length} regions exact' : 'blend: $failed of ${checks.length} wrong');

		pipeline.destroy();
		shader.destroy();
		vertices.destroy();
		readback.destroy();
		view.destroy();
		target.destroy();
		device.destroy();
		adapter.destroy();
		instance.destroy();
		Sys.exit(failed == 0 ? 0 : 1);
	}
}
