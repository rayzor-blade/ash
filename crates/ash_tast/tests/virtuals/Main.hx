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

	static function main() {}
}
