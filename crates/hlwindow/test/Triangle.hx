/**
	A triangle in a window, drawn every frame until it is closed.

	Uses both libraries at once, which is the point: `hlwindow` owns the
	window and `hlwgpu` owns the drawing, and neither knows about the other --
	`Window` fits `wgpu.WindowSource` only by shape.

	Takes a frame limit as its first argument so a check can run it without a
	window being left open; with none it runs until closed.
**/
class Triangle {
	static var SHADER = "
@vertex
fn vs(@location(0) pos : vec2<f32>) -> @builtin(position) vec4<f32> {
	return vec4<f32>(pos, 0.0, 1.0);
}

@fragment
fn fs() -> @location(0) vec4<f32> {
	return vec4<f32>(0.95, 0.45, 0.1, 1.0);
}
";

	static function main() {
		var limit = Sys.args().length > 0 ? Std.parseInt(Sys.args()[0]) : -1;

		var win = window.Window.open("hlwgpu", 640, 360);
		if (!win.ok) {
			Sys.println("no window");
			Sys.exit(1);
		}

		var instance = wgpu.Instance.create();
		var adapter = instance.adapter();
		var device = adapter.device();
		var queue = device.queue;

		var surface = wgpu.Surface.fromWindow(instance, win);
		if (!surface.ok) {
			Sys.println("no surface on this window");
			Sys.exit(1);
		}
		var format = surface.preferredFormat(adapter);
		if (format == Unknown) {
			Sys.println("this surface wants a format hlwgpu has no name for");
			Sys.exit(1);
		}
		surface.configure(device, win.width, win.height, format);

		var corners = [-0.8, -0.6, 0.8, -0.6, 0.0, 0.8];
		var vertexData = haxe.io.Bytes.alloc(corners.length * 4);
		for (i in 0...corners.length) {
			vertexData.setFloat(i * 4, corners[i]);
		}
		var vertices = device.buffer(vertexData.length, Vertex | CopyDst);
		queue.write(vertices, 0, vertexData);

		var shader = device.shader(SHADER);
		var pipeline = device.renderPipeline(shader, "vs", "fs", format, 8, [
			{ format: Float32x2, offset: 0, location: 0 }
		]);

		Sys.println('window ${win.width}x${win.height}, surface format $format on ' + adapter.backend.toString());

		var frames = 0;
		while (limit < 0 || frames < limit) {
			var events = win.poll();
			if (events.closed) {
				break;
			}
			if (events.resized) {
				surface.configure(device, win.width, win.height, format);
			}

			var view = surface.acquire();
			if (!view.ok) {
				// Out of date: reconfigure and take the next one.
				surface.configure(device, win.width, win.height, format);
				continue;
			}

			var encoder = device.encoder();
			encoder.beginRender(view, 0.06, 0.07, 0.09);
			encoder.setPipeline(pipeline);
			encoder.setVertexBuffer(0, vertices);
			encoder.draw(3);
			encoder.endRender();
			encoder.submit(queue);
			surface.present(queue);
			frames++;
		}

		Sys.println('$frames frames');

		pipeline.destroy();
		shader.destroy();
		vertices.destroy();
		surface.destroy();
		device.destroy();
		adapter.destroy();
		instance.destroy();
		win.close();
		Sys.println("closed cleanly");
	}
}
