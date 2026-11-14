enum Shape {
	Circle(r:Float);
	Rect(w:Float, h:Float);
	Empty;
}

typedef Point = {x:Int, y:Int};
// A structure that names itself through Null<T>, which once sent the exporter into a loop.
typedef Node = {value:Int, next:Null<Node>};

class Main {
	static function area(s:Shape):Float {
		return switch (s) {
			case Circle(r): 3.14 * r * r;
			case Rect(w, h): w * h;
			case Empty: 0;
		}
	}

	static function main() {
		var shapes = [Circle(1.0), Rect(2, 3), Empty];
		var total = 0.0;
		for (s in shapes)
			total += area(s);
		var p:Point = {x: 1, y: 2};
		var n:Node = {value: 1, next: null};
		var f = function(a:Int) return a + p.x;
		try {
			if (f(1) > 100)
				throw "big";
		} catch (e:String) {
			trace(e);
		}
		var i = 0;
		while (i < 3)
			i++;
		var maybe:Null<Int> = null;
		trace(total + (maybe == null ? 0 : maybe) + n.value);
	}
}
