// Compiled HVIRTUAL dispatch under heap pressure — the shape a heaps/MBHaxe
// scene load actually has, and the one no existing ash benchmark covers.
//
// Two ingredients, both required:
//   * calls go through a STRUCTURAL type, so compiled code lowers them to
//     hlp_vcall_dyn rather than a vtable slot;
//   * every call allocates, so the cost of boxing arguments and of any
//     per-call scratch shows up as GC pressure rather than being hidden.
// A tight loop over one method (no allocation) misses the second entirely and
// reports a speed-up while a real load gets slower.

typedef Entity = {
	function step(dt:Int):Int;
	function tag():String;
	function children():Array<Int>;
}

class Prop {
	public var id:Int;
	var cache:Array<Int>;
	public function new(id) { this.id = id; cache = [for (i in 0...4) i * id]; }
	public function step(dt:Int):Int {
		var acc = 0;
		for (c in cache) acc += c * dt;
		return acc;
	}
	public function tag():String return "prop:" + id;          // allocates
	public function children():Array<Int> return [id, id + 1];  // allocates
}

class Mover {
	public var id:Int;
	public function new(id) this.id = id;
	public function step(dt:Int):Int return id * dt + (id % 3);
	public function tag():String return "mover:" + id;
	public function children():Array<Int> return [for (i in 0...3) id * i];
}

class BenchDynDispatch {
	// Separate function so it promotes and its dispatches become COMPILED
	// HVIRTUAL calls.
	static function tick(world:Array<Entity>, dt:Int):Int {
		var sum = 0;
		for (e in world) {
			sum += e.step(dt);
			var kids = e.children();      // allocation per dispatch
			for (k in kids) sum += k;
		}
		return sum;
	}

	static function names(world:Array<Entity>):Int {
		var n = 0;
		for (e in world) n += e.tag().length;   // string allocation per dispatch
		return n;
	}

	static function main() {
		// A large RETAINED graph, which is the ingredient that makes this
		// resemble a scene load. Allocation rate alone is not enough: with a
		// tiny live set every collection is trivially cheap and the profile
		// hides whatever the dispatch path is doing. A scene holds its level
		// data live, so each mark has to walk it, and THAT is what turns
		// per-dispatch garbage into stalls.
		var retained:Array<Array<Int>> = [];
		for (i in 0...40000) retained.push([for (j in 0...8) i + j]);

		var world:Array<Entity> = [];
		for (i in 0...64) world.push(i % 2 == 0 ? new Prop(i) : new Mover(i));
		var sum = 0;
		var t0 = haxe.Timer.stamp();
		for (round in 0...20000) {
			sum += tick(world, round % 16);
			if (round % 64 == 0) sum += names(world);
		}
		var dt = haxe.Timer.stamp() - t0;
		// Keep the graph observably live to the end.
		sum += retained[retained.length - 1][0];
		// Only the checksum goes to stdout: this case is a parity gate as well
		// as a benchmark, and a timing line would differ on every run.
		Sys.println("Checksum: " + sum);
		if (Sys.getEnv("ASH_BENCH_TIME") != null)
			Sys.stderr().writeString("elapsed_ms: " + Math.round(dt * 1000) + "\n");
	}
}
