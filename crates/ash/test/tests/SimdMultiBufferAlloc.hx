// Three independent vector updates expose stale slot value IDs in AIR SROA.
// The value API should allocate no temporary bytes in this loop.
import ash.simd.Float32x4;

class SimdMultiBufferAlloc {
	static inline var COUNT = 256;

	static function step(px:hl.Bytes, py:hl.Bytes, pz:hl.Bytes,
		vx:hl.Bytes, vy:hl.Bytes, vz:hl.Bytes):Void {
		var delta = Float32x4.splat(0.125);
		var dt = Float32x4.splat(0.01);
		var i = 0;
		while (i < COUNT) {
			var off = i << 2;
			var nx = Float32x4.load(vx, off) + delta;
			var ny = Float32x4.load(vy, off) + delta;
			var nz = Float32x4.load(vz, off) + delta;
			nx.store(vx, off);
			ny.store(vy, off);
			nz.store(vz, off);
			(Float32x4.load(px, off) + nx * dt).store(px, off);
			(Float32x4.load(py, off) + ny * dt).store(py, off);
			(Float32x4.load(pz, off) + nz * dt).store(pz, off);
			i += 4;
		}
	}

	static function main():Void {
		var px = new hl.Bytes(COUNT << 2), py = new hl.Bytes(COUNT << 2), pz = new hl.Bytes(COUNT << 2);
		var vx = new hl.Bytes(COUNT << 2), vy = new hl.Bytes(COUNT << 2), vz = new hl.Bytes(COUNT << 2);
		step(px, py, pz, vx, vy, vz);
		var before = hl.Gc.stats().totalAllocated;
		for (i in 0...100)
			step(px, py, pz, vx, vy, vz);
		var bytes = hl.Gc.stats().totalAllocated - before;
		Sys.println('gc_bytes=$bytes checksum=${px.getF32(0) + py.getF32(0) + pz.getF32(0)}');
	}
}
