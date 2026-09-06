import sys.thread.Thread;
import sys.thread.Deque;

/**
	Haxe threads, in a browser, actually running at the same time.

	Each thread is a Worker running a second instance of this same module over
	one shared memory. The proof is not that four answers come back -- fibers
	taking turns would also manage that -- but that four of them take about as
	long as one, while the same work done in a row takes four times as long.

	It computes rather than allocates so that what it times is the threads
	and not the collector. Threads that allocate work too -- see
	docs/wasm-target.md for the program that checks that.
**/
class Threads {
	// A linear congruential generator, iterated. No allocation, no calls the
	// compiler can lift out, and a result that depends on every step.
	static function work(seed:Int, steps:Int):Int {
		var acc = seed;
		for (i in 0...steps) acc = (acc * 1103515245 + 12345) & 0x3FFFFFFF;
		return acc;
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
	}
}
