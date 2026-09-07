/**
	Pins the coordinate conventions, which are the ones people arrive with the
	wrong idea about.

	WebGPU is not OpenGL here. Clip space has +y upward, but the framebuffer's
	first row is the TOP, so +y lands in the early rows of a readback. And
	depth runs 0 to 1, not -1 to 1, so a vertex at z = -0.5 is behind the near
	plane and is clipped away rather than drawn in front.

	Neither is visible in a symmetric test, which is why they get their own.
**/
class Conventions {
	static inline var SIZE = 64;
	static inline var ROW = SIZE * 4;

	static var SHADER = "
@vertex
fn vs(@location(0) pos : vec3<f32>) -> @builtin(position) vec4<f32> {
	return vec4<f32>(pos, 1.0);
}

@fragment
fn fs() -> @location(0) vec4<f32> {
	return vec4<f32>(1.0, 0.0, 0.0, 1.0);
}
";

	/** Two triangles over x -1..1 and y y0..y1, all at depth z. **/
	static function quad(y0 : Float, y1 : Float, z : Float) : haxe.io.Bytes {
		var corners = [[-1.0, y0], [1.0, y0], [1.0, y1], [-1.0, y0], [1.0, y1], [-1.0, y1]];
		var out = haxe.io.Bytes.alloc(corners.length * 3 * 4);
		var at = 0;
		for (c in corners) {
			out.setFloat(at, c[0]);
			out.setFloat(at + 4, c[1]);
			out.setFloat(at + 8, z);
			at += 12;
		}
		return out;
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
		var vertices = device.buffer(72, Vertex | CopyDst);

		var shader = device.shader(SHADER);
		var pipeline = device.pipeline().shader(shader, "vs", "fs")
			.vertexBuffer().attributes(Float32x3)
			.target(Rgba8Unorm)
			.build();

		function drawAndRead(geometry : haxe.io.Bytes) : haxe.io.Bytes {
			queue.write(vertices, 0, geometry);
			var encoder = device.encoder();
			encoder.beginRender(view, 0.0, 0.0, 0.0);
			encoder.setPipeline(pipeline);
			encoder.setVertexBuffer(0, vertices);
			encoder.draw(6);
			encoder.endRender();
			encoder.copyTextureToBuffer(target, readback, SIZE, SIZE, ROW);
			encoder.submit(queue);
			return readback.read(device, 0, SIZE * ROW);
		}

		inline function filled(image : haxe.io.Bytes, y : Int) : Bool {
			return image.get(y * ROW + (SIZE >> 1) * 4) == 255;
		}

		var failed = 0;
		inline function check(what : String, held : Bool) {
			if (!held) {
				Sys.println('  ' + what);
				failed++;
			}
		}

		// Clip space +y is up, and the first row of the buffer is the top.
		var upper = drawAndRead(quad(0.0, 1.0, 0.5));
		check("+y did not land in the early rows", filled(upper, 8));
		check("+y also filled the late rows", !filled(upper, 56));

		// Depth runs 0 to 1: behind the near plane is clipped, not drawn.
		var behind = drawAndRead(quad(-1.0, 1.0, -0.5));
		check("z = -0.5 was drawn, so depth is not 0..1", !filled(behind, 8) && !filled(behind, 56));

		// And 0 to 1 really does include both ends.
		var atNear = drawAndRead(quad(-1.0, 1.0, 0.0));
		check("z = 0 was clipped, so the near plane is wrong", filled(atNear, 32));

		Sys.println(failed == 0 ? "conventions: clip space and depth range are WebGPU's"
			: 'conventions: $failed wrong');

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
