// Particles pulled toward the origin, positions and velocities as Float32x4
// (x, y, z, 0): every step is a chain of vector operators with a horizontal
// sum for the distance. Exercises the value type through fields of an
// object, where the vector must be materialised between steps.

import ash.simd.Float32x4;

class Particle {
	public var pos:Float32x4;
	public var vel:Float32x4;

	public function new(pos:Float32x4, vel:Float32x4) {
		this.pos = pos;
		this.vel = vel;
	}
}

class BenchSimdParticles {
	static inline var N = 4096;
	static inline var STEPS = 2000;

	static function step(ps:Array<Particle>, dt:Float32x4, soft:Float32x4) {
		for (p in ps) {
			var r2 = (p.pos * p.pos).sum() + 0.01;
			var inv = Float32x4.splat(-1.0 / (r2 * Math.sqrt(r2)));
			p.vel = p.vel + p.pos * inv * dt;
			p.pos = p.pos + p.vel * dt;
		}
	}

	static function main() {
		var ps = new Array<Particle>();
		for (i in 0...N) {
			var a = i * 0.37;
			var pos = Float32x4.make(Math.cos(a) * (1 + i % 7), Math.sin(a) * (1 + i % 5), (i % 3) - 1, 0);
			var vel = Float32x4.make(-Math.sin(a) * 0.1, Math.cos(a) * 0.1, 0, 0);
			ps.push(new Particle(pos, vel));
		}
		var dt = Float32x4.splat(0.001);
		var soft = Float32x4.splat(0.01);
		for (s in 0...STEPS)
			step(ps, dt, soft);
		var sum:Float = 0;
		for (p in ps)
			sum += p.pos.x + p.pos.y + p.pos.z;
		Sys.println("BenchSimdParticles " + Std.int(sum * 1000));
	}
}
