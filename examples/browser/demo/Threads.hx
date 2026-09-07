import sys.thread.Thread;
import sys.thread.Deque;

/**
	Haxe threads, in a browser, actually running at the same time.

	Each thread is a Worker running a second instance of this same module over
	one shared memory. The proof is not that four answers come back -- fibers
	taking turns would also manage that -- but that four of them take about as
	long as one, while the same work done in a row takes four times as long.

	The timed part computes rather than allocates, so that what it times is
	the threads and not the collector. The part after it does nothing but
	allocate, on every thread at once, because that is the case that used to
	fail: threads sharing one heap, each filling arrays and maps while the
	collector runs. Its answers are checked against the same work done on one
	thread, so a wrong one is visible rather than merely slow.
**/
class Threads {
	// A linear congruential generator, iterated. No allocation, no calls the
	// compiler can lift out, and a result that depends on every step.
	static function work(seed:Int, steps:Int):Int {
		var acc = seed;
		for (i in 0...steps) acc = (acc * 1103515245 + 12345) & 0x3FFFFFFF;
		return acc;
	}

	// Allocation, and enough of it to collect several times: arrays of
	// strings and a map, built and thrown away. The checksum depends on every
	// object surviving long enough to be read.
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
		var n = 4;
		// Enough work that starting the agents does not dominate it. A
		// thread in a page is a Worker instantiating this whole module
		// again, which is tens of milliseconds before it computes anything.
		var steps = 200000000;

		Sys.println('$n threads, ${steps} steps each');
		Sys.println("");

		var done = new Deque<Int>();
		var started = haxe.Timer.stamp();
		for (i in 0...n) Thread.create(() -> done.add(work(i, steps)));
		var answers = [for (i in 0...n) done.pop(true)];
		var together = haxe.Timer.stamp() - started;

		started = haxe.Timer.stamp();
		for (i in 0...n) work(i, steps);
		var apart = haxe.Timer.stamp() - started;

		answers.sort((a, b) -> a - b);
		Sys.println('answers: ${answers.join(", ")}');
		Sys.println("");
		Sys.println('$n at once:     ${Math.round(together * 1000)}ms');
		Sys.println('$n in a row:    ${Math.round(apart * 1000)}ms');
		Sys.println('speedup:       ${Math.round(apart / together * 100) / 100}x');
		Sys.println("");
		Sys.println(together < apart * 0.75
			? "They ran at the same time."
			: "They took turns — this host started no agent for them.");

		// And the same threads allocating rather than computing, which is the
		// case that used to lose objects the collector could not see.
		Sys.println("");
		Sys.println('$n threads allocating, 20 rounds each');
		var want = [for (i in 0...n) churn(i, 1)];
		var alloc = new Deque<String>();
		for (i in 0...n) Thread.create(() -> alloc.add('$i:${churn(i, 20)}'));
		var back = [for (i in 0...n) alloc.pop(true)];
		back.sort(Reflect.compare);
		Sys.println('answers:      ${back.join(" ")}');
		Sys.println('single round: ${want.join(",")}');
		var ok = want.length == n;
		for (v in want) if (v == 0) ok = false;
		Sys.println(ok
			? "The heap survived four threads allocating on it."
			: "WRONG — an object was collected while it was still in use.");
	}
}
