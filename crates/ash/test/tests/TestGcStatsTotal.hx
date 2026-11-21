// hl.Gc.stats() reports what the program allocated, to the object, not in
// whole allocation regions.
class TestGcStatsTotal {
	static var keep:Dynamic;
	static var bytes:Float;
	static var objects:Float;

	// Reading the counters allocates their result, which lands in the window
	// that ends at the next read.
	static function snap() {
		var s = hl.Gc.stats();
		bytes = s.totalAllocated;
		objects = s.allocationCount;
	}

	static function main() {
		for (i in 0...8) {
			snap();
			keep = [1, 2, 3];
		}

		var emptyBytes = [];
		var emptyObjects = [];
		for (i in 0...6) {
			snap();
			var b = bytes;
			var o = objects;
			snap();
			emptyBytes.push(bytes - b);
			emptyObjects.push(objects - o);
		}
		var overheadBytes = emptyBytes[0];
		var overheadObjects = emptyObjects[0];

		var singleBytes = [];
		var singleObjects = [];
		for (i in 0...6) {
			snap();
			var b = bytes;
			var o = objects;
			keep = [1, 2, 3];
			snap();
			singleBytes.push(bytes - b - overheadBytes);
			singleObjects.push(objects - o - overheadObjects);
		}

		var n = 5000;
		snap();
		var b = bytes;
		var o = objects;
		for (i in 0...n)
			keep = [1, 2, 3];
		snap();
		var bulkBytes = (bytes - b - overheadBytes) / n;
		var bulkObjects = (objects - o - overheadObjects) / n;

		var monotone = true;
		snap();
		var last = bytes;
		var lastObjects = objects;
		for (i in 0...400) {
			keep = [i, i + 1];
			snap();
			if (bytes < last || objects < lastObjects)
				monotone = false;
			last = bytes;
			lastObjects = objects;
		}

		Sys.println("empty " + emptyBytes.join(",") + " " + emptyObjects.join(","));
		Sys.println("single " + singleBytes.join(",") + " " + singleObjects.join(","));
		Sys.println("bulk " + bulkBytes + " " + bulkObjects);
		Sys.println("monotone " + monotone);
	}
}
