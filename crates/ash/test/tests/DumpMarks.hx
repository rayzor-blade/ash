class DumpMarks {
	static var keep:Array<Array<Int>> = [[0]];

	static function main() {
		var out = Sys.getEnv("ASH_TEST_DUMP");
		hl.Gc.dumpMemory(out == null ? "dump_marks.dump" : out);
		for (i in 0...20000)
			keep.push([i, i + 1, i + 2]);
		hl.Gc.major();
		var junk = [];
		for (i in 0...200000)
			junk.push([i, i, i]);
		var bad = 0;
		for (i in 0...20000) {
			var a = keep[i + 1];
			if (a == null || a[0] != i || a[2] != i + 2)
				bad++;
		}
		Sys.println("bad=" + bad);
	}
}
