/**
	A function with nested loops is entered once and its running frame leaves
	through a re-tier exit, which postpones its top-tier promote. The program
	waits long enough for that decision to be made and then calls the function
	again, from an interpreted caller. That second entry owes it the promote.
**/
class TestHandoffReentry {
	static inline var N = 4096;
	static inline var REPS = 1000;

	static function kernel(a:Array<Single>, b:Array<Single>):Single {
		var s:Single = 0;
		for (r in 0...REPS)
			for (i in 0...N) {
				var v:Single = a[i] * 0.999 + b[i];
				a[i] = v;
				s += v;
			}
		return s;
	}

	static function once(f:() -> Single):Single {
		return f();
	}

	static function main() {
		var a:Array<Single> = [for (i in 0...N) ((i % 7) * 0.25 + 1.0 : Single)];
		var b:Array<Single> = [for (i in 0...N) ((i % 5) * 0.125 : Single)];
		var f = () -> kernel(a, b);
		var total:Float = once(f);
		Sys.sleep(2.5);
		total += once(f);
		Sys.sleep(2.5);
		total += once(f);
		Sys.println("done " + (total > 0));
	}
}
