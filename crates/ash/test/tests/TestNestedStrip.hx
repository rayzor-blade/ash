// Counted loops of every awkward shape. Each prints one line; every engine,
// with and without the nested strip, must print the same text.
class Box {
	public var n:Int;
	public function new(n:Int) this.n = n;
}

class TestNestedStrip {
	@:noInline static function call(i:Int):Int return i * 3 + 1;

	@:noInline static function sum(n:Int):Int {
		var s = 0;
		for (i in 0...n) s += i * 3 + 1;
		return s;
	}

	@:noInline static function soa(pos:hl.NativeArray<Single>, vel:hl.NativeArray<Single>, n:Int, g:Single, dt:Single):Void {
		for (i in 0...n) {
			vel[i] = vel[i] + g * dt;
			pos[i] = pos[i] + vel[i] * dt;
		}
	}

	@:noInline static function stepTwo(n:Int):Int {
		var s = 0, i = 0;
		while (i < n) { s += i; i += 2; }
		return s;
	}

	@:noInline static function negativeStart(n:Int):Int {
		var s = 0, i = -300;
		while (i < n) { s += i; i++; }
		return s;
	}

	// Bounds at the edge of Int, so the end of a strip must not wrap.
	@:noInline static function between(lo:Int, hi:Int):Int {
		var s = 0, i = lo;
		while (i < hi) { s++; i++; }
		return s;
	}

	@:noInline static function exitValue(n:Int):Int {
		var i = 0;
		while (i < n) i++;
		return i;
	}

	@:noInline static function breaks(a:hl.NativeArray<Int>, n:Int, stop:Int):Int {
		var s = 0;
		for (i in 0...n) {
			if (a[i] > stop) break;
			if (a[i] % 3 == 0) continue;
			s += a[i];
		}
		return s;
	}

	@:noInline static function withCall(n:Int):Int {
		var s = 0;
		for (i in 0...n) s += call(i);
		return s;
	}

	@:noInline static function countDown(n:Int):Int {
		var s = 0, i = n;
		while (i > 0) { s += i; i--; }
		return s;
	}

	@:noInline static function grid(r:Int, c:Int):Int {
		var s = 0;
		for (y in 0...r) for (x in 0...c) s += y * 7 + x;
		return s;
	}

	// The bound is read from an object the loop changes, so it is not invariant.
	@:noInline static function movingBound(b:Box):Int {
		var i = 0, s = 0;
		while (i < b.n) {
			i++;
			s += i;
			if (i == 10) b.n = 700;
		}
		return s;
	}

	static function main() {
		var sizes = [0, 1, 2, 3, 255, 256, 257, 511, 512, 513, 1000, 4096, 4097];
		for (n in sizes) {
			Sys.println('sum $n ${sum(n)}');
			Sys.println('stepTwo $n ${stepTwo(n)}');
			Sys.println('negativeStart $n ${negativeStart(n)}');
			Sys.println('exitValue $n ${exitValue(n)}');
			Sys.println('withCall $n ${withCall(n)}');
			Sys.println('countDown $n ${countDown(n)}');
			var pos = new hl.NativeArray<Single>(n + 1), vel = new hl.NativeArray<Single>(n + 1);
			for (i in 0...n + 1) { pos[i] = i * 0.5; vel[i] = i * 0.25; }
			for (_ in 0...3) soa(pos, vel, n, -9.81, 1 / 60);
			var acc:Single = 0;
			for (i in 0...n + 1) acc += pos[i] + vel[i];
			Sys.println('soa $n $acc ${pos[n]} ${n > 0 ? pos[n - 1] : 0}');
			var a = new hl.NativeArray<Int>(n + 1);
			for (i in 0...n + 1) a[i] = (i * 7919) % 1009;
			Sys.println('breaks $n ${breaks(a, n, 900)} ${breaks(a, n, 5000)}');
			Sys.println('grid $n ${grid(n % 40, n % 70)}');
			Sys.println('movingBound $n ${movingBound(new Box(n))}');
		}
		// Parsed, so no pass can see the bounds as constants.
		var p = (s:String) -> Std.parseInt(s);
		Sys.println('between ${between(p("2147482900"), p("2147483647"))} ${between(p("2147483000"), p("2147483300"))} ${between(p("-2147483648"), p("-2147483000"))} ${between(p("5"), p("5"))} ${between(p("9"), p("3"))}');
	}
}
