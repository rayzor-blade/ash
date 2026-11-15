class Acc {
	public var total:Int;
	public var limit:Int;
	public var scale:Int;

	public function new(limit:Int, scale:Int) {
		this.total = 0;
		this.limit = limit;
		this.scale = scale;
	}

	// writes `total` and nothing else
	public function bump(k:Int):Void {
		total += k * scale;
	}

	// reads `limit` on every iteration, and calls a method that does not write it
	public function run():Int {
		var i = 0;
		while (i < limit) {
			bump(i);
			i++;
		}
		return total;
	}

	public function reset():Void {
		total = 0;
	}
}

class Point {
	public var x:Float;
	public var y:Float;

	public function new(x:Float, y:Float) {
		this.x = x;
		this.y = y;
	}

	public function dist2():Float {
		return x * x + y * y;
	}
}

class Main {
	static function accumulate(limit:Int, scale:Int):Int {
		var a = new Acc(limit, scale);
		return a.run();
	}

	static function twice(limit:Int, scale:Int):Int {
		var a = new Acc(limit, scale);
		var first = a.run();
		a.reset();
		return first + a.run();
	}

	static function norm(x:Float, y:Float):Float {
		var p = new Point(x, y);
		return p.dist2();
	}

	static function main() {}
}
