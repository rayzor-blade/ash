class Shape {
	public var k:Int;

	public function new(k:Int) {
		this.k = k;
	}

	public function area():Int {
		return 0;
	}

	public function twice():Int {
		return area() * 2;
	}
}

class Square extends Shape {
	public function new(k:Int) {
		super(k);
	}

	override public function area():Int {
		return k * k;
	}
}

class Tri extends Shape {
	public function new(k:Int) {
		super(k);
	}

	override public function area():Int {
		return (k * k) >> 1;
	}
}

class Plain extends Shape {
	public function new() {
		super(1);
	}
}

class Counter {
	public var hits:Int;
	public var limit:Int;

	public function new(limit:Int) {
		this.hits = 0;
		this.limit = limit;
	}

	public function note(k:Int):Void {}
}

class Loud extends Counter {
	public function new(limit:Int) {
		super(limit);
	}

	override public function note(k:Int):Void {
		hits += k;
	}
}

class Quiet extends Counter {
	public function new(limit:Int) {
		super(limit);
	}

	override public function note(k:Int):Void {}
}

class Main {
	// the receiver is a Square and no class below it overrides area
	static function square(k:Int):Int {
		var s = new Square(k);
		return s.area();
	}

	// Plain inherits area from Shape, and nothing below Plain overrides it
	static function plain():Int {
		var p = new Plain();
		return p.area();
	}

	// the receiver may be a Shape or any class below it
	static function areaOf(s:Shape):Int {
		return s.area();
	}

	static function mixed(k:Int):Int {
		return areaOf(new Square(k)) + areaOf(new Tri(k)) + areaOf(new Shape(k)) + areaOf(new Plain());
	}

	// twice is declared in Shape and no class overrides it; inside it, `this` may be any Shape
	static function twiceSquare(k:Int):Int {
		return new Square(k).twice();
	}

	// reads `limit` through a call that runs Counter.note, Loud.note or Quiet.note: of those
	// only Loud.note writes anything, and it writes `hits`
	static function drive(c:Counter):Int {
		var i = 0;
		while (i < c.limit) {
			c.note(i);
			i++;
		}
		return c.hits;
	}

	static function loud(n:Int):Int {
		return drive(new Loud(n));
	}

	static function quiet(n:Int):Int {
		return drive(new Quiet(n));
	}

	static function main() {}
}
