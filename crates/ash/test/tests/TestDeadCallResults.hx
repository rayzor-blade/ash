// A call whose result nobody reads must not keep what it returned alive.
//
// `open` returns the handles it opened and `churn` drops them. With `churn`
// inlined into the rounds below, each round's handles are held only by a
// result slot of `main`'s frame that no instruction reads. Under a low
// RLIMIT_NOFILE the rounds can only finish if the collector closes the
// handles between them; see `crates/ash_cli/tests/dead_call_results.rs`.
import sys.io.File;
import sys.io.FileInput;

class TestDeadCallResults {
	static function open(path:String, count:Int):Array<FileInput> {
		return [for (i in 0...count) {
			var f = File.read(path, true);
			f.readByte();
			f;
		}];
	}

	static function churn(path:String, count:Int) {
		open(path, count);
	}

	static function main() {
		var path = Sys.args()[0];
		File.saveContent(path, "x");
		var held = open(path, 4);
		for (round in 0...5) {
			churn(path, 40);
			hl.Gc.major();
		}
		var still = 0;
		for (f in held)
			if (f != null)
				still++;
		Sys.println("rounds 5, held " + still);
	}
}
