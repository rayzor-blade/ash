// Functions called once whose hot loop has more than one way back to its
// header: a `continue` is a second jump to it. An entry at the header takes
// the state the interpreter has and runs whichever of the loop's blocks come
// next, so each shape below must give the interpreter's answer.
class TestOsrLatches {
	// Two continues and the fall-through.
	static function plain():Int {
		var i = 0;
		var s = 0;
		while (i < 600000) {
			i++;
			if (i % 3 == 0)
				continue;
			if (i % 5 == 0) {
				s += 2;
				continue;
			}
			s = (s + (i & 7)) & 0xffffff;
		}
		return s;
	}

	// Float state carried round the loop, updated differently on each path.
	static function floats():Float {
		var i = 0;
		var x = 1.0;
		var y = 0.5;
		while (i < 400000) {
			i++;
			if (i % 4 == 0) {
				x = x * 0.999 + 0.001;
				continue;
			}
			if (i % 7 == 0) {
				y = y + x * 0.01;
				continue;
			}
			x = x + 0.0001;
			y = y * 0.9999;
		}
		return x + y;
	}

	// An inner loop with a continue, inside an outer loop that defines what
	// the inner one reads.
	static function nested():Int {
		var total = 0;
		var a = 0;
		while (a < 900) {
			var base = a * 5 + 1;
			var b = 0;
			while (b < 600) {
				b++;
				if ((b + a) % 3 == 0)
					continue;
				total = (total + base * b) & 0xffffff;
			}
			total = (total ^ base) & 0xffffff;
			a++;
		}
		return total;
	}

	// The outer loop is the one with the continue.
	static function outer():Int {
		var total = 0;
		var a = 0;
		while (a < 2000) {
			a++;
			if (a % 4 == 1)
				continue;
			var b = 0;
			while (b < 250) {
				total = (total + a * b) & 0xffffff;
				b++;
			}
			if (a % 9 == 0)
				continue;
			total = (total + a) & 0xffffff;
		}
		return total;
	}

	// Writes to an array on every path, a continue, and a break.
	static function arrays():Int {
		var a = [for (_ in 0...64) 0];
		var i = 0;
		var s = 0;
		while (i < 500000) {
			i++;
			var k = i & 63;
			if (i % 5 == 0) {
				a[k] += 3;
				continue;
			}
			if (i > 499990)
				break;
			a[k] += i & 3;
			s = (s + a[k]) & 0xffffff;
		}
		return s + a[7];
	}

	// A call that is not inlined on one path only.
	@:noInline static function step(v:Int):Int {
		return (v * 31 + 7) & 0xffff;
	}

	static function calls():Int {
		var i = 0;
		var v = 1;
		var s = 0;
		while (i < 300000) {
			i++;
			if (i % 2 == 0) {
				v = step(v);
				continue;
			}
			s = (s + v) & 0xffffff;
		}
		return s;
	}

	static function main() {
		Sys.println('latches=' + plain() + ' ' + floats() + ' ' + nested() + ' ' + outer() + ' ' + arrays() + ' ' + calls());
	}
}
