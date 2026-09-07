import sys.thread.Thread;
import sys.thread.Deque;

/**
	Haxe threads drawing at the same time, into one picture.

	Every thread owns a horizontal band of the framebuffer and the entities
	inside it, and writes only its own pixels. There is no lock here and
	nothing to contend for: the threads really are simultaneous, and the
	picture is the evidence. A band that stops moving is a thread that stopped.

	They allocate while they draw -- a fresh trail per entity per frame -- on
	purpose. Allocating on several threads at once is what used to lose
	objects the collector could not see, and a demo that only did arithmetic
	would not show that it no longer does.

	Where an entity is is a function of the clock, not of how many frames have
	been drawn, so the picture is the same picture whatever rate a band
	manages.

	Each band holds itself to the rate the host shows, and that is not a
	politeness. Uncapped, a band draws around three thousand frames a second,
	allocates some twenty megabytes a second doing it, and nobody sees more
	than sixty of them: the frames past the sixtieth are invisible, and the
	garbage behind them is not. Four threads of that bury the collector, every
	stop-the-world pauses the thread that presents, and the picture arrives in
	lurches. Drawing what is shown and no more is what makes it real time.

	The main thread presents. It never touches the pixels: it hands the host
	the address of the buffer the threads are writing, sixty times a second,
	and the host shows it. In a browser that host is a Worker with an
	OffscreenCanvas, which is the only way to draw from inside a call that
	never returns to the event loop.
**/
class Entities {
	static inline var W = 640;
	static inline var H = 360;
	static inline var PER_BAND = 40;
	static inline var R = 7;
	static inline var TRAIL = 12;
	// How far back in time each trail sample is taken. Long enough to see,
	// short enough that a trail stays inside its own band.
	static inline var TRAIL_STEP = 0.035;
	// What every band draws at and what the main thread presents at. One
	// number, because a band drawing faster than this is drawing frames
	// nobody will see.
	static inline var FPS = 60.0;

	@:hlNative("std", "canvas_present")
	static function present(data:hl.Bytes, width:Int, height:Int):Bool {
		return false;
	}

	static var fb:hl.Bytes;
	static var pixels:Int;
	static var bands:Int;

	/** Little-endian RGBA, which is the byte order a canvas reads. */
	static inline function rgba(r:Int, g:Int, b:Int):Int {
		return 0xFF000000 | (b << 16) | (g << 8) | r;
	}

	/** One colour per thread, so the eye can tell which drew what. */
	static function bandColour(id:Int):Int {
		var sixth = (id * 6.0) / bands;
		var c = 235;
		var x = Std.int(c * (1 - Math.abs((sixth % 2) - 1)));
		return switch (Std.int(sixth)) {
			case 0: rgba(c, x, 60);
			case 1: rgba(x, c, 60);
			case 2: rgba(60, c, x);
			case 3: rgba(60, x, c);
			case 4: rgba(x, 60, c);
			default: rgba(c, 60, x);
		}
	}

	/**
		Where something travelling at a constant speed is after bouncing off
		both ends of `lo..hi` -- a triangle wave, and the reason a position
		here needs no history to compute.
	**/
	static inline function bounce(v:Float, lo:Float, hi:Float):Float {
		var span = hi - lo;
		if (span <= 0) return lo;
		var t = (v - lo) % (2 * span);
		if (t < 0) t += 2 * span;
		return lo + (t <= span ? t : 2 * span - t);
	}

	static function band(id:Int, seconds:Float, fps:Float):Int {
		var y0 = Std.int((H * id) / bands);
		var y1 = Std.int((H * (id + 1)) / bands);
		var colour = bandColour(id);
		var r = colour & 0xFF, g = (colour >> 8) & 0xFF, b = (colour >> 16) & 0xFF;
		var dim = rgba(12, 12, 16);
		var edge = rgba(Std.int(r * 0.22), Std.int(g * 0.22), Std.int(b * 0.22));

		var seed = id * 7919 + 13;
		inline function rand(n:Int):Int {
			seed = (seed * 1103515245 + 12345) & 0x3FFFFFFF;
			return seed % n;
		}

		var lo = y0 + R, hi = y1 - R - 1;
		var xs = [], ys = [], vxs = [], vys = [];
		for (i in 0...PER_BAND) {
			xs.push(R + rand(W - 2 * R) * 1.0);
			ys.push(lo + (hi > lo ? rand(hi - lo) : 0) * 1.0);
			// Pixels per second, and never zero in either axis, so nothing
			// slides along an edge for the whole run.
			vxs.push((70 + rand(130)) * (rand(2) == 0 ? -1.0 : 1.0));
			vys.push((40 + rand(70)) * (rand(2) == 0 ? -1.0 : 1.0));
		}

		var frames = 0;
		var period = 1 / fps;
		var started = Sys.time();
		var due = started + period;
		var now = 0.0;
		while (now < seconds) {
			now = Sys.time() - started;

			for (y in y0...y1) {
				var row = pixels + y * W * 4;
				var c = y == y0 ? edge : dim;
				for (x in 0...W) fb.setI32(row + x * 4, c);
			}

			for (i in 0...PER_BAND) {
				// Allocated fresh every frame, and dropped: this is the part
				// that exercises the collector while the other bands run.
				var trail = [];
				for (t in 0...TRAIL) {
					var at = now - t * TRAIL_STEP;
					trail.push({
						x: Std.int(bounce(xs[i] + vxs[i] * at, R, W - R - 1)),
						y: Std.int(bounce(ys[i] + vys[i] * at, lo, hi))
					});
				}

				var t = trail.length;
				while (t-- > 0) {
					var p = trail[t];
					var fade = (TRAIL - t) / TRAIL;
					var c = rgba(Std.int(r * fade), Std.int(g * fade), Std.int(b * fade));
					var rr = t == 0 ? R : 1 + Std.int((R - 1) * fade);
					var oy = -rr;
					while (oy <= rr) {
						var py = p.y + oy;
						if (py > y0 && py < y1) {
							var row = pixels + py * W * 4;
							var ox = -rr;
							while (ox <= rr) {
								var px = p.x + ox;
								if (px >= 0 && px < W) fb.setI32(row + px * 4, c);
								ox++;
							}
						}
						oy++;
					}
				}
			}

			frames++;
			// The host reads this to show that every band is still moving.
			fb.setI32(id * 4, frames);

			// Sleep out the rest of this frame's slice. Late is not made up
			// for: a band that fell behind starts its next frame now rather
			// than drawing several with no gap to catch up, which would show
			// as a stutter and buy nothing.
			var spent = Sys.time();
			if (spent < due) Sys.sleep(due - spent);
			due += period;
			if (due < spent) due = spent + period;
		}
		return frames;
	}

	static function main() {
		bands = 4;
		var seconds = 15.0;
		var args = Sys.args();
		if (args.length > 0) {
			var n = Std.parseInt(args[0]);
			if (n != null && n > 0) bands = n;
		}
		if (args.length > 1) {
			var s = Std.parseFloat(args[1]);
			if (!Math.isNaN(s) && s > 0) seconds = s;
		}
		// Third argument: the rate the BANDS hold themselves to, and only
		// them. Presenting stays at the display rate whatever this says --
		// there was never a reason to show more frames than a display can,
		// and a presenter told to run at a hundred thousand asks the fiber
		// scheduler to park it a hundred thousand times a second, which
		// deadlocks the program at startup and reproduces nothing.
		//
		// Uncapping the bands is how the collector is put under a load a page
		// must never give it.
		var fps = FPS;
		if (args.length > 2) {
			var f = Std.parseFloat(args[2]);
			if (!Math.isNaN(f) && f > 0) fps = f;
		}

		pixels = bands * 4;
		fb = new hl.Bytes(pixels + W * H * 4);
		for (i in 0...bands) fb.setI32(i * 4, 0);
		for (p in 0...(W * H)) fb.setI32(pixels + p * 4, rgba(12, 12, 16));

		Sys.println('$bands threads, one band each, ${W}x$H at ${fps}fps, ${seconds}s');

		var done = new Deque<Int>();
		var started = Sys.time();
		for (i in 0...bands) Thread.create(() -> done.add(band(i, seconds, fps)));

		// Present while they draw. Nothing is synchronised: a frame may catch
		// a band mid-update, which at this rate is invisible and is the honest
		// picture of several threads writing one buffer.
		var shown = 0;
		var frame = fb.offset(pixels);
		var period = 1 / FPS;
		var due = started + period;
		while (Sys.time() - started < seconds) {
			if (present(frame, W, H)) shown++;
			// Against a deadline, not a fixed nap: showing a frame costs real
			// time -- in a page it is a copy out of shared memory and a
			// `putImageData` -- and sleeping a whole period on top of that
			// would settle below the rate the bands are drawing at.
			var spent = Sys.time();
			if (spent < due) Sys.sleep(due - spent);
			due += period;
			if (due < spent) due = spent + period;
		}

		// Read before joining, while the threads are still drawing, because
		// that is when a heap nothing is reclaiming shows it. A page can say
		// the picture moved; only this says what it cost. `currentMemory` is
		// blocks in use, so it is the retention measure directly: a build that
		// traces dead neighbours reads hundreds of megabytes here against a
		// live set of one or two.
		var heap = hl.Gc.stats().currentMemory / (1024 * 1024);

		var total = 0;
		for (i in 0...bands) total += done.pop(true);
		var elapsed = Sys.time() - started;

		Sys.println("");
		for (i in 0...bands) {
			var f = fb.getI32(i * 4);
			Sys.println('band $i: $f frames, ${Math.round(f / elapsed)}/s');
		}
		Sys.println('$total band-frames in ${Math.round(elapsed * 1000)}ms, ${Math.round(total / elapsed)}/s');
		Sys.println('heap in use:   ${Math.round(heap)} MB while drawing');
		Sys.println(shown > 0 ? '$shown frames presented' : "no display on this host; frames drawn but not shown");
	}
}
