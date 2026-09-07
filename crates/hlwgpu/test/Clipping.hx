/**
	The two ways of restricting where a draw lands, which do different things.

	A scissor rectangle cuts: what falls outside it is thrown away and the rest
	is untouched. A viewport squeezes: the whole of clip space is fitted into
	the rectangle instead. Drawing the same full-target quad under each is what
	tells them apart.
**/
class Clipping {
	static inline var SIZE = 64;
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

		// The whole of clip space, so anything less than the whole target is
		// the restriction and not the geometry.
		var corners = [-1.0, -1.0, 1.0, -1.0, 1.0, 1.0, -1.0, -1.0, 1.0, 1.0, -1.0, 1.0];
		var data = haxe.io.Bytes.alloc(corners.length * 4);
		for (i in 0...corners.length) {
			data.setFloat(i * 4, corners[i]);
		}
		var vertices = device.buffer(data.length, Vertex | CopyDst);
		queue.write(vertices, 0, data);

		var shader = device.shader(SHADER);
		var pipeline = device.pipeline().shader(shader, "vs", "fs")
			.vertexBuffer().attributes(Float32x2)
			.target(Rgba8Unorm)
			.build();

		function draw(restrict : wgpu.Encoder -> Void) : haxe.io.Bytes {
			var encoder = device.encoder();
			encoder.beginRender(view, 0.0, 0.0, 0.0);
			encoder.setPipeline(pipeline);
			restrict(encoder);
			encoder.setVertexBuffer(0, vertices);
			encoder.draw(6);
			encoder.endRender();
			encoder.copyTextureToBuffer(target, readback, SIZE, SIZE, ROW);
			encoder.submit(queue);
			return readback.read(device, 0, SIZE * ROW);
		}

		var failed = 0;
		function check(what : String, image : haxe.io.Bytes, x : Int, y : Int, want : String) {
			var got = pixel(image, x, y);
			if (got != want) {
				Sys.println('  $what at $x,$y: wanted $want, got $got');
				failed++;
			}
		}

		// Cut to the middle 32x32: inside is drawn, outside keeps the clear.
		var cut = draw(e -> e.setScissorRect(16, 16, 32, 32));
		check("scissor, inside", cut, 32, 32, "255,0,0,255");
		check("scissor, outside", cut, 4, 4, "0,0,0,255");
		check("scissor, just outside", cut, 15, 32, "0,0,0,255");
		check("scissor, just inside", cut, 16, 32, "255,0,0,255");

		// Squeeze the whole quad into the left half: the right half is empty
		// even though the geometry still covers all of clip space.
		var squeezed = draw(e -> e.setViewport(0, 0, SIZE / 2, SIZE));
		check("viewport, inside", squeezed, 16, 32, "255,0,0,255");
		check("viewport, outside", squeezed, 48, 32, "0,0,0,255");
		check("viewport, top edge", squeezed, 16, 1, "255,0,0,255");

		Sys.println(failed == 0 ? "clipping: scissor cuts and viewport squeezes, exactly"
			: 'clipping: $failed wrong');

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
