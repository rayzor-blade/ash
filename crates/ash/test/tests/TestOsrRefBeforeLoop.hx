// Functions called once whose hot loops are reached through refs. In `work`
// the ref is passed to a call that has returned before either loop starts; the
// first loop has two back edges (`continue` goes to the header), the second
// has one. In `carried` a ref is made on every iteration and the call writes
// through it, so the counter lives in a cell the loop carries.
class TestOsrRefBeforeLoop {
	static function bump(r:hl.Ref<Int>):Void {
		r.set(r.get() + 7);
	}

	static function work():Int {
		var seed = 3;
		bump(seed);
		var total = 0;
		var i = 0;
		while (i < 400000) {
			i++;
			if (i % 3 == 0)
				continue;
			total = (total + i * seed) & 0xffffff;
		}
		var k = 0;
		while (k < 400000) {
			total = (total ^ (k * 31 + seed)) & 0xffffff;
			k++;
		}
		return total;
	}

	static function carried():Int {
		var seed = 3;
		var total = 0;
		var i = 0;
		while (i < 1500000) {
			bump(seed);
			total = (total + i * seed) & 0xffffff;
			i++;
		}
		return total;
	}

	static function main() {
		Sys.println('total=' + work() + ' ' + carried());
	}
}
