/**
	A near quad drawn first, then a far one over the top of it.

	The far quad covers the whole target and is drawn second, so without a
	depth test it would win everywhere and the result would be uniformly green.
	It only loses on the left because the near quad is already there, which is
	what makes this a test of depth rather than of draw order.
**/
class Depth {
	static inline var SIZE = 64;
	static inline var ROW = SIZE * 4;

	static var SHADER = "
struct VsOut {
	@builtin(position) pos : vec4<f32>,
	@location(0) tint : vec4<f32>,
}

@vertex
fn vs(@location(0) pos : vec3<f32>, @location(1) tint : vec4<f32>) -> VsOut {
	var out : VsOut;
	out.pos = vec4<f32>(pos, 1.0);
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

	/** Two triangles covering x0..x1 at a fixed depth, all in one colour. **/
	static function quad(into : Array<Float>, x0 : Float, x1 : Float, z : Float, r : Float, g : Float, b : Float) {
		var corners = [[x0, -1.0], [x1, -1.0], [x1, 1.0], [x0, -1.0], [x1, 1.0], [x0, 1.0]];
		for (c in corners) {
			into.push(c[0]);
			into.push(c[1]);
			into.push(z);
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
		var depthBuffer = device.texture(SIZE, SIZE, Depth32Float, RenderAttachment);
		var depthView = depthBuffer.view();
		var readback = device.buffer(SIZE * ROW, MapRead | CopyDst);

		var values = new Array<Float>();
		quad(values, -1.0, 0.0, 0.2, 1.0, 0.0, 0.0); // near, left half, red
		quad(values, -1.0, 1.0, 0.8, 0.0, 1.0, 0.0); // far, everywhere, green
		var vertexData = haxe.io.Bytes.alloc(values.length * 4);
		for (i in 0...values.length) {
			vertexData.setFloat(i * 4, values[i]);
		}
		var vertices = device.buffer(vertexData.length, Vertex | CopyDst);
		queue.write(vertices, 0, vertexData);

		var shader = device.shader(SHADER);
		var pipeline = device.pipeline()
			.shader(shader, "vs", "fs")
			.vertexBuffer().attributes(Float32x3, Float32x4)
			.target(Rgba8Unorm)
			.depth(Depth32Float, true, Less)
			.build();

		var encoder = device.encoder();
		encoder.beginRenderDepth(view, depthView, 0.0, 0.0, 0.0);
		encoder.setPipeline(pipeline);
		encoder.setVertexBuffer(0, vertices);
		encoder.draw(Std.int(values.length / 7));
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
			{ what: "the near quad survives being drawn over", x: 16, want: "255,0,0,255" },
			{ what: "the far quad shows where nothing is nearer", x: 48, want: "0,255,0,255" }
		];

		var failed = 0;
		for (check in checks) {
			var got = pixel(out, check.x, mid);
			if (got != check.want) {
				Sys.println('  ${check.what}: wanted ${check.want}, got $got');
				failed++;
			}
		}
		Sys.println(failed == 0 ? 'depth: all ${checks.length} regions exact' : 'depth: $failed of ${checks.length} wrong');

		pipeline.destroy();
		shader.destroy();
		vertices.destroy();
		readback.destroy();
		depthView.destroy();
		depthBuffer.destroy();
		view.destroy();
		target.destroy();
		device.destroy();
		adapter.destroy();
		instance.destroy();
		Sys.exit(failed == 0 ? 0 : 1);
	}
}
