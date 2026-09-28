// Refs to interpreter registers passed into compiled callees: optional basic
// arguments (compiled as hl.Ref<T>) read by the callee, and explicit
// hl.Ref<T> parameters it writes through.
class Opt {
	public var f:Single;
	public var d:Float;
	public var i:Int;
	public var b:Bool;

	public function new(f:Single = 0.0, d:Float = 0.0, i:Int = 0, b:Bool = false) {
		this.f = f;
		this.d = d;
		this.i = i;
		this.b = b;
	}
}

class LoopOpt {
	public var i:Int;

	public function new(i:Int = 0) {
		this.i = i;
	}
}

class TestRefArgs {
	static function bumpF32(r:hl.Ref<Single>):Void {
		r.set(r.get() + 1.5);
	}

	static function bumpF64(r:hl.Ref<Float>):Void {
		r.set(r.get() * 2.0);
	}

	static function bumpI32(r:hl.Ref<Int>):Void {
		r.set(r.get() + 7);
	}

	static function flip(r:hl.Ref<Bool>):Void {
		r.set(!r.get());
	}

	// Wider than the --jit-max-args the test passes, so this loop stays
	// interpreted while the callees above are compiled.
	static function drive(n:Int, p1:Int, p2:Int, p3:Int, p4:Int, p5:Int, p6:Int):Int {
		var bad = 0;
		for (k in 0...n) {
			if (k < 100) {
				if (new LoopOpt(k).i != k || new LoopOpt(k & 0xffff).i != (k & 0xffff))
					bad++;
			}
			var ef:Single = 0.5 * (k & 7);
			var ed = 0.25 * k;
			var ei = k * 3 + 1;
			var eb = (k & 1) == 1;
			var o = new Opt(ef, ed, ei, eb);
			if (o.f != ef || o.d != ed || o.i != ei || o.b != eb)
				bad++;

			var f:Single = 0.5 * (k & 3);
			bumpF32(f);
			if (f != 0.5 * (k & 3) + 1.5)
				bad++;

			var d:Float = 0.125 * k;
			bumpF64(d);
			if (d != 0.25 * k)
				bad++;

			var i = k;
			bumpI32(i);
			if (i != k + 7)
				bad++;

			var b = (k & 1) == 0;
			flip(b);
			if (b != ((k & 1) == 1))
				bad++;
		}
		return bad + p1 + p2 + p3 + p4 + p5 + p6;
	}

	static function main() {
		Sys.println('ref-args bad=' + drive(200000, 0, 0, 0, 0, 0, 0));
	}
}
