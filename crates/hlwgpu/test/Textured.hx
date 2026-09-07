/**
	Draws a textured quad and checks each quadrant.

	A 2x2 texture sampled with nearest filtering onto a 64x64 target puts one
	texel in each quadrant exactly, and every colour is 0 or 255 per channel,
	so the expected bytes are the same on any GPU. Exercises an index buffer,
	a texture upload, a sampler, and a bind group holding two different kinds
	of thing.
**/
class Textured {
	static inline var SIZE = 64;
	static inline var ROW = SIZE * 4;

	static var SHADER = "
struct VsOut {
	@builtin(position) pos : vec4<f32>,
	@location(0) uv : vec2<f32>,
}

@group(0) @binding(0) var image : texture_2d<f32>;
@group(0) @binding(1) var nearest : sampler;

@vertex
fn vs(@location(0) pos : vec2<f32>, @location(1) uv : vec2<f32>) -> VsOut {
	var out : VsOut;
	out.pos = vec4<f32>(pos, 0.0, 1.0);
	out.uv = uv;
	return out;
}

@fragment
fn fs(in : VsOut) -> @location(0) vec4<f32> {
	return textureSample(image, nearest, in.uv);
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

		// Top row red then green, bottom row blue then white.
		var texels = haxe.io.Bytes.alloc(16);
		var colours = [255, 0, 0, 255, 0, 255, 0, 255, 0, 0, 255, 255, 255, 255, 255, 255];
		for (i in 0...colours.length) {
			texels.set(i, colours[i]);
		}
		var image = device.texture(2, 2, Rgba8Unorm, TextureBinding | CopyDst);
		queue.writeTexture(image, texels, 2, 2, 8);

		var target = device.texture(SIZE, SIZE, Rgba8Unorm, RenderAttachment | CopySrc);
		var view = target.view();
		var readback = device.buffer(SIZE * ROW, MapRead | CopyDst);

		// A quad over the whole target: position then texture coordinate.
		var corners = [-1.0, -1.0, 0.0, 1.0, 1.0, -1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, -1.0, 1.0, 0.0, 0.0];
		var vertexData = haxe.io.Bytes.alloc(corners.length * 4);
		for (i in 0...corners.length) {
			vertexData.setFloat(i * 4, corners[i]);
		}
		var vertices = device.buffer(vertexData.length, Vertex | CopyDst);
		queue.write(vertices, 0, vertexData);

		var order = [0, 1, 2, 0, 2, 3];
		var indexData = haxe.io.Bytes.alloc(order.length * 2);
		for (i in 0...order.length) {
			indexData.setUInt16(i * 2, order[i]);
		}
		var indices = device.buffer(indexData.length, Index | CopyDst);
		queue.write(indices, 0, indexData);

		var shader = device.shader(SHADER);
		var pipeline = device.renderPipeline(shader, "vs", "fs", Rgba8Unorm, 16, [
			{ format: Float32x2, offset: 0, location: 0 },
			{ format: Float32x2, offset: 8, location: 1 }
		]);
		var sampler = device.sampler(Nearest, ClampToEdge);
		var imageView = image.view();
		var bindings = device.bindGroup(pipeline, 0, [imageView, sampler]);

		var encoder = device.encoder();
		encoder.beginRender(view, 0.0, 0.0, 0.0);
		encoder.setPipeline(pipeline);
		encoder.setBindGroup(0, bindings);
		encoder.setVertexBuffer(0, vertices);
		encoder.setIndexBuffer(indices, Uint16);
		encoder.drawIndexed(order.length);
		encoder.endRender();
		encoder.copyTextureToBuffer(target, readback, SIZE, SIZE, ROW);
		encoder.submit(queue);

		var out = readback.read(device, 0, SIZE * ROW);
		if (out == null) {
			Sys.println("readback failed");
			Sys.exit(1);
		}

		var quarter = SIZE >> 2;
		var checks = [
			{ what: "top-left texel", x: quarter, y: quarter, want: "255,0,0,255" },
			{ what: "top-right texel", x: SIZE - quarter, y: quarter, want: "0,255,0,255" },
			{ what: "bottom-left texel", x: quarter, y: SIZE - quarter, want: "0,0,255,255" },
			{ what: "bottom-right texel", x: SIZE - quarter, y: SIZE - quarter, want: "255,255,255,255" }
		];

		var failed = 0;
		for (check in checks) {
			var got = pixel(out, check.x, check.y);
			if (got != check.want) {
				Sys.println('  ${check.what}: wanted ${check.want}, got $got');
				failed++;
			}
		}
		Sys.println(failed == 0 ? 'textured: all ${checks.length} quadrants exact' : 'textured: $failed of ${checks.length} wrong');

		bindings.destroy();
		sampler.destroy();
		imageView.destroy();
		pipeline.destroy();
		shader.destroy();
		indices.destroy();
		vertices.destroy();
		readback.destroy();
		view.destroy();
		target.destroy();
		image.destroy();
		device.destroy();
		adapter.destroy();
		instance.destroy();
		Sys.println("destroyed cleanly");
		Sys.exit(failed == 0 ? 0 : 1);
	}
}
