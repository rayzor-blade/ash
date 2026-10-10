// Functions called once whose hot loop sits inside another loop. Each inner
// loop reads values its outer loop defines on every pass, so an entry at the
// inner header gets those values from the interpreter on the first pass and
// from the outer body on every later one.
class TestOsrNested {
	static function nested():Int {
		var total = 0;
		var i = 0;
		while (i < 4000) {
			var base = i * 7 + 1;
			var j = 0;
			while (j < 700) {
				total = (total + base * j) & 0xffffff;
				j++;
			}
			total = (total ^ base) & 0xffffff;
			i++;
		}
		return total;
	}

	// A `break` leaves the inner loop to a block the outer loop's exit test
	// also reaches.
	static function leaving():Int {
		var total = 0;
		var i = 0;
		while (i < 3000) {
			var mark = i & 15;
			var j = 0;
			while (j < 1000) {
				if (j > mark * 50 + 20)
					break;
				total = (total + mark * j + i) & 0xffffff;
				j++;
			}
			total = (total + mark) & 0xffffff;
			i++;
		}
		return total;
	}

	static function deep():Int {
		var total = 0;
		for (a in 0...40) {
			var x = a * 3 + 1;
			for (b in 0...40) {
				var y = x + b;
				for (c in 0...600) {
					total = (total + x * y + c) & 0xffffff;
				}
				total = (total ^ y) & 0xffffff;
			}
		}
		return total;
	}

	static function main() {
		Sys.println('nested=' + nested() + ' ' + leaving() + ' ' + deep());
	}
}
