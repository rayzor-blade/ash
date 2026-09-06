import sys.thread.Thread;
import sys.thread.Deque;

/**
	A wasm build gets this wrong, and native does not. Reduced from a larger
	program by bisection; every line left is one the failure needs.

	    haxe -cp . -main RootLost -hl rootlost.hl
	    ash rootlost.hl                                  # right answers
	    ash --build r.wasm --target wasm32-wasip1 rootlost.hl
	    ash-wasm-run r.wasm                              # `expected` is garbage

	`expected` comes back holding fragments of other objects -- UTF-16 from
	the strings `churn` built -- so something writes where it should not,
	rather than a root being lost. It takes all three of:

	  - a loop bound that is NOT a compile-time constant (`Std.parseInt`
	    here, `Sys.args()` in the original; a literal 4 passes),
	  - fibers (`Thread.create` and a `Deque` the main frame blocks on),
	  - enough allocation in the thread bodies to collect several times.

	Any one of them removed and it passes: with a literal bound it passes,
	without the threads it passes, and the same shape with a `Deque<Int>`
	rather than strings passes. It is not the dynamic sort -- adding that
	alone to the passing version changes nothing.
**/
class RootLost {
	static function churn(seed:Int, rounds:Int):Int {
		var sum = 0;
		for (r in 0...rounds) {
			var a = [];
			for (i in 0...500) a.push('$seed-$r-$i');
			for (s in a) sum = (sum + s.length) & 0xFFFFFF;
			var m = new Map<String, Int>();
			for (i in 0...200) m.set('k$i', i * seed);
			for (k => v in m) sum = (sum + v) & 0xFFFFFF;
		}
		return sum;
	}

	static function main() {
		var n = Std.parseInt("4");
		if (n == null) n = 4;
		var kept = [11, 22, 33];
		var expected = [for (i in 0...n) churn(i, 1)];

		var done = new Deque<String>();
		for (i in 0...n) Thread.create(() -> done.add('$i:${churn(i, 30)}'));
		var got = [for (i in 0...n) done.pop(true)];
		got.sort(Reflect.compare);

		Sys.println('got ${got.length} answers');
		Sys.println('kept = ${kept.join(",")} (want 11,22,33)');
		Sys.println('expected = ${expected.join(",")} (want 4 numbers)');
	}
}
