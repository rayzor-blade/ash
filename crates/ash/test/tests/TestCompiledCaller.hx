/**
	A function with nested loops takes its OSR hand-off during its first call,
	which postpones its top-tier promote. Its caller is compiled by then, or
	before, so every later call comes from compiled code, which ticks nothing.
	The promote is paid when the hand-off frame returns into a compiled
	caller, and is not postponed for a function a compiled caller entered.

	With the argument `wait`, main pauses before the first call so that the
	caller is compiled first and enters the function through the stub bridge.
**/
class CallerWorld {
	static inline var N = 4096;
	static inline var REPS = 3000;
	var a:Array<Single>;
	var b:Array<Single>;
	var energy:Single = 0;

	public function new() {
		a = [for (i in 0...N) ((i % 7) * 0.25 + 1.0 : Single)];
		b = [for (i in 0...N) ((i % 5) * 0.125 : Single)];
	}

	public function step():Void {
		var s:Single = 0;
		for (r in 0...REPS)
			for (i in 0...N) {
				var v:Single = a[i] * 0.999 + b[i];
				a[i] = v;
				s += v;
			}
		energy = s;
	}

	public function frame(f:Int):Void {
		if (f >= 30)
			step();
	}

	public function sum():Float {
		return energy;
	}
}

class TestCompiledCaller {
	static function main() {
		var wait = Sys.args().indexOf("wait") >= 0;
		var w = new CallerWorld();
		var total = 0.0;
		for (f in 0...60) {
			if (f == 30 && wait)
				Sys.sleep(0.3);
			w.frame(f);
			if (f >= 30)
				Sys.sleep(0.05);
			total += w.sum();
		}
		Sys.println("done " + (total > 0));
	}
}
