/**
	Sends bytes the long way round and checks they come back unchanged.

	Buffer to texture, texture to texture, texture back to buffer. Every step
	is a copy, so anything that reorders rows, mistakes a stride or drops a
	channel shows up as a byte that differs. Also clears part of a buffer,
	which is the one way to zero GPU memory without uploading zeroes to it.
**/
class Copies {
	// 64 RGBA pixels is 256 bytes, which is the row alignment a texture copy
	// insists on.
	static inline var WIDTH = 64;
	static inline var HEIGHT = 4;
	static inline var ROW = WIDTH * 4;
	static inline var TOTAL = ROW * HEIGHT;

	static function main() {
		var instance = wgpu.Instance.create();
		var adapter = instance.adapter();
		var device = adapter.device();
		if (!device.ok) {
			Sys.println("no device");
			Sys.exit(1);
		}
		var queue = device.queue;

		var pattern = haxe.io.Bytes.alloc(TOTAL);
		for (i in 0...TOTAL) {
			pattern.set(i, (i * 7 + 11) & 0xFF);
		}

		var upload = device.buffer(TOTAL, CopySrc | CopyDst);
		queue.write(upload, 0, pattern);

		var first = device.texture(WIDTH, HEIGHT, Rgba8Unorm, CopyDst | CopySrc);
		var second = device.texture(WIDTH, HEIGHT, Rgba8Unorm, CopyDst | CopySrc);
		var readback = device.buffer(TOTAL, MapRead | CopyDst);

		var encoder = device.encoder();
		encoder.copyBufferToTexture(upload, ROW, first, WIDTH, HEIGHT);
		encoder.copyTextureToTexture(first, second, WIDTH, HEIGHT);
		encoder.copyTextureToBuffer(second, readback, WIDTH, HEIGHT, ROW);
		encoder.submit(queue);

		var out = readback.read(device, 0, TOTAL);
		if (out == null) {
			Sys.println("readback failed");
			Sys.exit(1);
		}

		var failed = 0;
		var differing = 0;
		for (i in 0...TOTAL) {
			if (out.get(i) != pattern.get(i)) {
				if (differing < 3) {
					Sys.println('  byte $i: wanted ${pattern.get(i)}, got ${out.get(i)}');
				}
				differing++;
			}
		}
		if (differing > 0) {
			Sys.println('  $differing of $TOTAL bytes differ after the round trip');
			failed++;
		}

		// Clearing zeroes a range and leaves the rest alone.
		var scratch = device.buffer(TOTAL, CopySrc | CopyDst);
		queue.write(scratch, 0, pattern);
		var clearer = device.encoder();
		clearer.clearBuffer(scratch, 256, 256);
		clearer.copyBuffer(scratch, 0, readback, 0, TOTAL);
		clearer.submit(queue);

		var cleared = readback.read(device, 0, TOTAL);
		if (cleared == null) {
			Sys.println("second readback failed");
			Sys.exit(1);
		}
		if (cleared.get(300) != 0) {
			Sys.println('  inside the cleared range: wanted 0, got ${cleared.get(300)}');
			failed++;
		}
		if (cleared.get(100) != pattern.get(100) || cleared.get(600) != pattern.get(600)) {
			Sys.println("  outside the cleared range was disturbed");
			failed++;
		}

		Sys.println(failed == 0 ? 'copies: $TOTAL bytes survived buffer to texture to texture to buffer, and a clear'
			: 'copies: $failed wrong');

		readback.destroy();
		scratch.destroy();
		second.destroy();
		first.destroy();
		upload.destroy();
		device.destroy();
		adapter.destroy();
		instance.destroy();
		Sys.exit(failed == 0 ? 0 : 1);
	}
}
