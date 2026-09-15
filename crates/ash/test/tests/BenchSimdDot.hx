// Dot product over two float buffers with the ash-simd value type: four
// lanes per step through Float32x4 operators, the accumulator kept as a
// vector. On stock HashLink every operator allocates its result; on ash the
// slot promotion keeps the chain in a register.

import ash.simd.Float32x4;

class BenchSimdDot {
	static inline var N = 1 << 18;
	static inline var ROUNDS = 300;

	static function dot(a:hl.Bytes, b:hl.Bytes):Single {
		var acc = Float32x4.splat(0);
		var i = 0;
		while (i < N) {
			acc = acc + Float32x4.load(a, i << 2) * Float32x4.load(b, i << 2);
			i += 4;
		}
		return acc.sum();
	}

	static function main() {
		var a = new hl.Bytes(N * 4);
		var b = new hl.Bytes(N * 4);
		for (i in 0...N) {
			a.setF32(i << 2, ((i * 7) % 13) * 0.125);
			b.setF32(i << 2, ((i * 5) % 11) * 0.25);
		}
		var total:Float = 0;
		for (r in 0...ROUNDS) {
			// A different scale per round, so the rounds cannot be hoisted
			// into one.
			b.setF32(0, r);
			total += dot(a, b);
		}
		Sys.println("BenchSimdDot " + Std.int(total));
	}
}
