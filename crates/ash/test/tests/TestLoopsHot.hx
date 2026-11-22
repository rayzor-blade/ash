/**
	A function with a loop is called only from compiled code, so nothing ticks
	its call count, and each call is too short for its own frame to hand off.
	Its loop iterations are what must take it to the top tier.
**/
class TestLoopsHot {
	static function kernel(a:hl.Bytes, n:Int):Single {
		var s:Single = 0;
		for (i in 0...n) {
			var o = i << 2;
			s += a.getF32(o) * 0.5;
			a.setF32(o, s);
		}
		return s;
	}

	static function caller(a:hl.Bytes, n:Int):Single {
		return kernel(a, n);
	}

	static function init(a:hl.Bytes) {
		for (i in 0...1024)
			a.setF32(i << 2, 1.0);
	}

	static function main() {
		var a = new hl.Bytes(4 * 1024);
		init(a);
		var t:Single = 0;
		// Called straight-line from interpreted code, so the interpreter's own
		// ladder compiles the kernel; each call probes the loop, and the total is far below any threshold.
		t += kernel(a, 100);
		t += kernel(a, 100);
		t += kernel(a, 100);
		t += kernel(a, 100);
		t += kernel(a, 100);
		t += kernel(a, 100);
		t += kernel(a, 100);
		t += kernel(a, 100);
		t += kernel(a, 100);
		t += kernel(a, 100);
		t += kernel(a, 100);
		t += kernel(a, 100);
		Sys.sleep(0.3);
		// The caller is compiled after a few calls, and then reaches the
		// kernel without ticking it.
		for (i in 0...3000)
			t += caller(a, 1024);
		Sys.sleep(0.5);
		for (i in 0...3000)
			t += caller(a, 1024);
		Sys.println("done " + (t != 0));
	}
}
