// set in the constructor and never again
class Config {
	public var scale:Int;
	public var limit:Int;

	public function new(scale:Int, limit:Int) {
		this.scale = scale;
		this.limit = limit;
	}
}

// `total` changes after construction; `step` does not
class Counter {
	public var total:Int;
	public var step:Int;

	public function new(step:Int) {
		this.step = step;
		this.total = 0;
	}

	public function add():Void {
		total += step;
	}
}

// the constructor calls a method on `this` before `b` is set, so nothing here is safe to treat
// as fixed: `peek` reads `b` while it still holds its default
class Leaky {
	public var b:Int;
	public var seen:Int;

	public function new() {
		seen = peek();
		b = 7;
	}

	function peek():Int {
		return b;
	}
}

class Base {
	public var id:Int;

	public function new(id:Int) {
		this.id = id;
	}
}

// writes its own field and calls its parent's constructor, and nothing else
class Derived extends Base {
	public var extra:Int;

	public function new(id:Int, extra:Int) {
		super(id);
		this.extra = extra;
	}
}

class Main {
	// reads `limit` on every iteration while calling a method that writes `total`
	static function run(c:Config, k:Counter):Int {
		var i = 0;
		while (i < c.limit) {
			k.add();
			i++;
		}
		return k.total * c.scale;
	}

	static function go(n:Int):Int {
		return run(new Config(2, n), new Counter(3));
	}

	static function make(n:Int):Int {
		var l = new Leaky();
		var d = new Derived(n, n + 1);
		return l.b + l.seen + d.id + d.extra;
	}

	static function main() {}
}
