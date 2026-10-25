import ash.channel.Channel;
import haxe.io.Bytes;

/**
	Exercises an ash-channel against the page's echo transport: every frame the
	program sends comes back. The frames are `u32 big-endian length + UTF-8
	JSON`, one per message, as a session protocol would use.
**/
class Echo {
	static function frame(value:Dynamic):Bytes {
		final json = Bytes.ofString(haxe.Json.stringify(value));
		final out = Bytes.alloc(4 + json.length);
		out.set(0, (json.length >> 24) & 255);
		out.set(1, (json.length >> 16) & 255);
		out.set(2, (json.length >> 8) & 255);
		out.set(3, json.length & 255);
		out.blit(4, json, 0, json.length);
		return out;
	}

	static function parse(bytes:Bytes):Dynamic {
		final n = (bytes.get(0) << 24) | (bytes.get(1) << 16) | (bytes.get(2) << 8) | bytes.get(3);
		if (n != bytes.length - 4)
			throw 'frame says $n bytes, holds ${bytes.length - 4}';
		return haxe.Json.parse(bytes.getString(4, n));
	}

	static function check(ok:Bool, what:String) {
		if (!ok)
			throw 'FAILED: $what';
	}

	static function padded(n:Int, size:Int):Bytes {
		// A frame of exactly `size` bytes: header, then JSON padded with spaces.
		final body = haxe.Json.stringify({n: n});
		final json = Bytes.alloc(size - 4);
		json.fill(0, json.length, 0x20);
		json.blit(0, Bytes.ofString(body), 0, body.length);
		final out = Bytes.alloc(size);
		out.set(0, (json.length >> 24) & 255);
		out.set(1, (json.length >> 16) & 255);
		out.set(2, (json.length >> 8) & 255);
		out.set(3, json.length & 255);
		out.blit(4, json, 0, json.length);
		return out;
	}

	static function main() {
		// Small rings, so frames wrap many times.
		final channel = Channel.open("ring_echo", 8192);
		var frames = 0;

		// 1. One frame round trip, and the wait for the transport to open is invisible.
		channel.send(frame({hello: "world", n: 1}));
		final first = parse(channel.recv());
		check(first.hello == "world" && first.n == 1, "first round trip");
		frames++;

		// 2. A blocked recv: the echo comes back after a delay, so the fiber parks and is woken.
		final before = Sys.time();
		channel.send(frame({n: 2}));
		final second = parse(channel.recv());
		final waited = Std.int((Sys.time() - before) * 1000);
		check(second.n == 2, "second round trip");
		frames++;

		// 3. Many frames of many sizes, one at a time: the rings wrap again and again.
		for (i in 0...400) {
			final size = 32 + (i * 53) % 2800;
			final sent = padded(i, size);
			channel.send(sent);
			final got = channel.recv();
			check(got.length == sent.length, 'frame $i length ${got.length} != ${sent.length}');
			check(got.compare(sent) == 0, 'frame $i bytes');
			frames++;
		}

		// 4. Pipelined: many sent before any is taken, taken in order.
		for (i in 0...30)
			channel.send(frame({seq: i, text: "pipelined " + i}));
		for (i in 0...30) {
			final got = parse(channel.recv());
			check(got.seq == i, 'pipelined $i came back as ${got.seq}');
			frames++;
		}

		// 5. The size limit: half a ring less 8 bytes goes, one more cannot.
		final half = 8192 >> 1;
		final biggest = padded(5, half - 8);
		channel.send(biggest);
		check(channel.recv().length == biggest.length, "the largest frame");
		frames++;
		var refused = false;
		try {
			channel.send(Bytes.alloc(half - 7));
		} catch (e:Dynamic) {
			refused = true;
		}
		check(refused, "a frame over half a ring is refused");

		// 6. State, then close: a recv after close throws ChannelClosed.
		check(channel.isOpen, "isOpen");
		channel.close();
		var closed = false;
		try {
			channel.recv();
		} catch (e:ash.channel.Channel.ChannelClosed) {
			closed = true;
		}
		check(closed, "recv after close throws ChannelClosed");

		Sys.println('ring-echo ok: $frames frames, first blocked recv waited ${waited} ms');
	}
}
