/**
	What runs in the page. Not a hello world: it exercises the parts of the
	VM a page has no other way to show working -- strings and their UTF-16
	crossing, maps, closures, exceptions, integer and float arithmetic -- and
	prints what it found, so a browser that shows the right numbers has run
	Haxe rather than merely loaded a module.
**/
class Demo {
	static function main() {
		Sys.println("ash — running HashLink in WebAssembly");
		Sys.println("");

		var t0 = haxe.Timer.stamp();

		// Arithmetic, and enough of it to be worth timing.
		var primes = sieve(50000);
		Sys.println('primes below 50,000: ${primes.length} (last ${primes[primes.length - 1]})');

		// Floating point, where a wrong answer would be a wrong VM rather
		// than a wrong program.
		var pi = 0.0;
		for (k in 0...200000) pi += (k % 2 == 0 ? 1.0 : -1.0) / (2 * k + 1);
		Sys.println('4 * Leibniz(200,000) = ${Math.round(pi * 4 * 1e6) / 1e6}');

		// Strings cross this boundary as UTF-16, so a mangled one shows up
		// here and nowhere else.
		var text = "héllo wörld — ünicode ✓";
		Sys.println('string: "$text" (${text.length} chars, reversed "${reverse(text)}")');

		// Maps, closures and sorting: ordinary Haxe, and the reason to run a
		// VM in a page at all.
		var counts = new Map<String, Int>();
		for (word in "the quick brown fox jumps over the lazy dog the fox".split(" "))
			counts.set(word, (counts.exists(word) ? counts.get(word) : 0) + 1);
		var words = [for (k in counts.keys()) k];
		words.sort((a, b) -> counts.get(b) - counts.get(a));
		Sys.println('most common word: "${words[0]}" x${counts.get(words[0])}');

		// Exceptions, which on wasm are setjmp lowered into the exception
		// handling instructions -- the reason a module needs `exnref`.
		try {
			throw "caught by Haxe";
		} catch (e:String) {
			Sys.println('exception: $e');
		}

		// A thread that has to resume in the MIDDLE of its body. A browser
		// has one thread and cannot block it, so this only works because the
		// module was built with `ASH_WASM_FIBERS=1`: the worker suspends
		// inside `pop`, main runs, and the worker carries on from where it
		// stopped. Without the transform it would run to its first block and
		// stay there.
		var toWorker = new sys.thread.Deque<Int>();
		var toMain = new sys.thread.Deque<String>();
		sys.thread.Thread.create(() -> {
			var first = toWorker.pop(true);
			toMain.add('woke with $first');
			var second = toWorker.pop(true);
			toMain.add('woke again with $second');
		});
		toWorker.add(1);
		Sys.println('thread: ${toMain.pop(true)}');
		toWorker.add(2);
		Sys.println('thread: ${toMain.pop(true)}');

		var ms = Math.round((haxe.Timer.stamp() - t0) * 1000);
		Sys.println("");
		Sys.println('all of that took ${ms}ms');
	}

	static function sieve(n:Int):Array<Int> {
		var composite = [for (_ in 0...n + 1) false];
		var out = [];
		for (i in 2...n + 1) {
			if (composite[i]) continue;
			out.push(i);
			// `i * i` is where a sieve overflows: Haxe's Int is 32 bits on
			// every target, and above 46,340 the square wraps negative and
			// indexes the array backwards. Comparing against `n / i` asks the
			// same question without ever computing the product.
			if (i > Std.int(n / i)) continue;
			var j = i * i;
			while (j <= n) { composite[j] = true; j += i; }
		}
		return out;
	}

	static function reverse(s:String):String {
		var out = new StringBuf();
		var i = s.length;
		while (i-- > 0) out.addChar(s.charCodeAt(i));
		return out.toString();
	}
}
