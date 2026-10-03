// A primitive passed through an interface typed by an erased `T` travels as a
// boxed Dynamic. A callee that declares Bool/Int/Float must see the value, not
// the box, whether it runs interpreted or compiled. The loop makes `set` hot
// enough to compile while `main` keeps calling it from the interpreter.
//
// Prints the number of calls that delivered the wrong value, per kind.

interface ISink<T> {
	function set(v:T):Void;
}

class BoolSink implements ISink<Bool> {
	public var last = false;

	public function new() {}

	public function set(v:Bool):Void {
		last = v;
	}
}

class IntSink implements ISink<Int> {
	public var last = 0;

	public function new() {}

	public function set(v:Int):Void {
		last = v;
	}
}

class FloatSink implements ISink<Float> {
	public var last = 0.0;

	public function new() {}

	public function set(v:Float):Void {
		last = v;
	}
}

enum Holder<T> {
	Const(v:T);
}

class TestBoxedPrimitiveArg {
	static function main() {
		var b = new BoolSink(), i = new IntSink(), f = new FloatSink();
		var bs:ISink<Bool> = b, is:ISink<Int> = i, fs:ISink<Float> = f;
		var wrongBool = 0, wrongInt = 0, wrongFloat = 0;
		for (n in 0...20000) {
			var wantBool = n % 2 == 0;
			var wantInt = n * 3 + 1;
			var wantFloat = n + 0.5;
			switch (Const(wantBool)) {
				case Const(v): bs.set(v);
			}
			switch (Const(wantInt)) {
				case Const(v): is.set(v);
			}
			switch (Const(wantFloat)) {
				case Const(v): fs.set(v);
			}
			if (b.last != wantBool)
				wrongBool++;
			if (i.last != wantInt)
				wrongInt++;
			if (f.last != wantFloat)
				wrongFloat++;
		}
		Sys.println('wrong bool=$wrongBool int=$wrongInt float=$wrongFloat');
	}
}
