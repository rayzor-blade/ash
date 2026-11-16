class Body {
	public var x:Float;

	public function new(x:Float) {
		this.x = x;
	}
}

class Main {
	static function make(n:Int):Array<Body> {
		var a = new Array<Body>();
		for (i in 0...n)
			a.push(new Body(i));
		return a;
	}

	static function empty():Array<Body> {
		return new Array<Body>();
	}

	static function total(a:Array<Body>):Float {
		var s = 0.0;
		for (i in 0...a.length)
			s += a[i].x;
		return s;
	}

	static function at(a:Array<Body>, i:Int):Body {
		return a[i];
	}

	static function size(a:Array<Body>):Int {
		return a.length;
	}

	static function put(a:Array<Body>, i:Int, b:Body):Void {
		a[i] = b;
	}

	static function bump(a:Array<Float>, i:Int):Void {
		a[i] += 1.5;
	}

	static function ints(n:Int):Array<Int> {
		var a = new Array<Int>();
		for (i in 0...n)
			a.push(i * i);
		return a;
	}

	static function lit():Array<Int> {
		return [3, 1, 2];
	}

	static function floats(x:Float):Array<Float> {
		return [1.5, x];
	}

	static function pair(a:Body, b:Body):Array<Body> {
		return [a, b];
	}

	static function none():Array<Int> {
		return [];
	}

	static function main() {
		trace(total(make(3)));
	}
}
