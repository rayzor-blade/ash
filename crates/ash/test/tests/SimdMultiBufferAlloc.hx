// Snapshot of chimpmonk/repro/ash_simd_alloc/Repro.hx. Keep the inline call
// in main: moving it to a small fixture function hides the inlining-budget bug.
import ash.simd.Float32x4;
import ash.simd.Vec;
import hl.Bytes;

class SimdMultiBufferAlloc {
	static inline var COUNT = 4096;
	static inline var DT:Single = 1.0 / 60.0;

	static function small(iterations:Int):Single {
		var a = Float32x4.splat(1.0);
		var b = Float32x4.splat(0.0001);
		for (i in 0...iterations)
			a = a + b;
		return a.sum();
	}

	// The physics method is Haxe-inline; retaining that call-site shape is required here.
	static inline function integrate(count:Int, px:Bytes, py:Bytes, pz:Bytes, vx:Bytes, vy:Bytes, vz:Bytes,
		mx:Bytes, my:Bytes, mz:Bytes, gx:Single, gy:Single, gz:Single, dt:Single):Void {
		var vDt = Float32x4.splat(dt);
		var vGxDt = Float32x4.splat(gx * dt);
		var vGyDt = Float32x4.splat(gy * dt);
		var vGzDt = Float32x4.splat(gz * dt);
		var vZero = Float32x4.splat(0.0);

		var i = 0;
		while (i < count) {
			var off = i << 2;
			var imx = Float32x4.load(mx, off);
			var imy = Float32x4.load(my, off);
			var imz = Float32x4.load(mz, off);
			var agx = Float32x4.select(imx.gt(vZero), vGxDt, vZero);
			var agy = Float32x4.select(imy.gt(vZero), vGyDt, vZero);
			var agz = Float32x4.select(imz.gt(vZero), vGzDt, vZero);

			var nextVx = Float32x4.load(vx, off) + agx;
			var nextVy = Float32x4.load(vy, off) + agy;
			var nextVz = Float32x4.load(vz, off) + agz;
			nextVx.store(vx, off);
			nextVy.store(vy, off);
			nextVz.store(vz, off);

			var nextPx = Float32x4.load(px, off) + nextVx * vDt;
			var nextPy = Float32x4.load(py, off) + nextVy * vDt;
			var nextPz = Float32x4.load(pz, off) + nextVz * vDt;
			nextPx.store(px, off);
			nextPy.store(py, off);
			nextPz.store(pz, off);
			i += 4;
		}
	}

	static function integrateWrapped(count:Int, px:Bytes, py:Bytes, pz:Bytes, vx:Bytes, vy:Bytes, vz:Bytes,
		mx:Bytes, my:Bytes, mz:Bytes, gx:Single, gy:Single, gz:Single, dt:Single):Void {
		integrate(count, px, py, pz, vx, vy, vz, mx, my, mz, gx, gy, gz, dt);
	}

	static function integrateSlots(count:Int, px:Bytes, py:Bytes, pz:Bytes, vx:Bytes, vy:Bytes, vz:Bytes,
		mx:Bytes, my:Bytes, mz:Bytes, scratch:Bytes):Void {
		var i = 0;
		while (i < count) {
			var off = i << 2;
			Vec.f32x4Gt(scratch, 80, mx, off, scratch, 0);
			Vec.v128Select(scratch, 96, scratch, 80, scratch, 16, scratch, 0);
			Vec.f32x4Add(vx, off, vx, off, scratch, 96);
			Vec.f32x4Fma(px, off, vx, off, scratch, 64, px, off);
			Vec.f32x4Gt(scratch, 80, my, off, scratch, 0);
			Vec.v128Select(scratch, 96, scratch, 80, scratch, 32, scratch, 0);
			Vec.f32x4Add(vy, off, vy, off, scratch, 96);
			Vec.f32x4Fma(py, off, vy, off, scratch, 64, py, off);
			Vec.f32x4Gt(scratch, 80, mz, off, scratch, 0);
			Vec.v128Select(scratch, 96, scratch, 80, scratch, 48, scratch, 0);
			Vec.f32x4Add(vz, off, vz, off, scratch, 96);
			Vec.f32x4Fma(pz, off, vz, off, scratch, 64, pz, off);
			i += 4;
		}
	}

	static function filled(value:Single):Bytes {
		var b = new Bytes(COUNT << 2);
		for (i in 0...COUNT)
			b.setF32(i << 2, value);
		return b;
	}

	static function main():Void {
		var args = Sys.args();
		if (args.length < 1 || args.length > 2) {
			Sys.println('usage: Repro.hl small|integration|wrapped|slots [iterations]');
			Sys.exit(2);
		}
		var mode = args[0];
		var iterations = args.length == 2 ? Std.parseInt(args[1]) : 100;
		if (iterations == null || iterations <= 0) {
			Sys.println('iterations must be a positive integer');
			Sys.exit(2);
		}

		if (mode == 'small') {
			small(1000);
			var before = hl.Gc.stats().totalAllocated;
			var start = Sys.time();
			var result = small(iterations);
			var seconds = Sys.time() - start;
			var allocated = hl.Gc.stats().totalAllocated - before;
			Sys.println('mode=$mode iterations=$iterations seconds=$seconds gc_bytes=$allocated checksum=$result');
			return;
		}

		if (mode != 'integration' && mode != 'wrapped' && mode != 'slots') {
			Sys.println('unknown mode: $mode');
			Sys.exit(2);
		}

		var px = filled(0.37), py = filled(0.74), pz = filled(1.11);
		var vx = filled(0.1), vy = filled(0.2), vz = filled(0.3);
		var mx = filled(1.0), my = filled(1.0), mz = filled(1.0);
		var scratch = new Bytes(112);
		Vec.f32x4Splat(scratch, 0, 0.0);
		Vec.f32x4Splat(scratch, 16, 0.1 * DT);
		Vec.f32x4Splat(scratch, 32, -9.81 * DT);
		Vec.f32x4Splat(scratch, 48, 0.3 * DT);
		Vec.f32x4Splat(scratch, 64, DT);

		for (i in 0...500) {
			if (mode == 'integration')
				integrate(COUNT, px, py, pz, vx, vy, vz, mx, my, mz, 0.1, -9.81, 0.3, DT);
			else if (mode == 'wrapped')
				integrateWrapped(COUNT, px, py, pz, vx, vy, vz, mx, my, mz, 0.1, -9.81, 0.3, DT);
			else
				integrateSlots(COUNT, px, py, pz, vx, vy, vz, mx, my, mz, scratch);
		}
		var before = hl.Gc.stats().totalAllocated;
		var start = Sys.time();
		for (i in 0...iterations) {
			if (mode == 'integration')
				integrate(COUNT, px, py, pz, vx, vy, vz, mx, my, mz, 0.1, -9.81, 0.3, DT);
			else if (mode == 'wrapped')
				integrateWrapped(COUNT, px, py, pz, vx, vy, vz, mx, my, mz, 0.1, -9.81, 0.3, DT);
			else
				integrateSlots(COUNT, px, py, pz, vx, vy, vz, mx, my, mz, scratch);
		}
		var seconds = Sys.time() - start;
		var allocated = hl.Gc.stats().totalAllocated - before;
		var checksum = px.getF32(0) + py.getF32(0) + pz.getF32(0);
		Sys.println('mode=$mode iterations=$iterations bodies=$COUNT seconds=$seconds gc_bytes=$allocated checksum=$checksum');
	}
}
