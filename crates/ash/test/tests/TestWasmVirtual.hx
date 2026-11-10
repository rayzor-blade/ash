// Fields of structural types through the direct wasm backend: a class
// instance viewed as the structure (fields found by pointer), a structure
// literal, and a dynamic object missing some fields (read and written by
// name). main prints one value per line.
typedef Point = {
	var x:Int;
	var y:Float;
	var tag:String;
	var on:Bool;
	var big:haxe.Int64;
	var ?next:Dynamic;
}

class Cell {
	public var x:Int;
	public var y:Float;
	public var tag:String;
	public var on:Bool;
	public var big:haxe.Int64;
	public var next:Dynamic;

	public function new() {}
}

class TestWasmVirtual {
	static var shared:Cell;

	static function total(p:Point):Float {
		return p.x + p.y;
	}

	static function assign(p:Point, n:Int):Int {
		p.x = n;
		p.y = n * 0.5;
		p.on = n > 1;
		p.big = haxe.Int64.ofInt(n) * 1000000000;
		return p.x + (p.on ? 1 : 0);
	}

	static function link(p:Point, q:Point):Bool {
		p.tag = q.tag;
		p.next = q;
		return p.next == q;
	}

	static function describe(p:Point):String {
		return p.x + "," + p.y + "," + p.tag + "," + p.on + "," + haxe.Int64.toStr(p.big) + "," + (p.next == shared);
	}

	static function main() {
		shared = new Cell();
		var c = new Cell();
		Sys.println("cell_assign=" + assign(c, 4));
		c.tag = "cell";
		var other = new Cell();
		other.tag = "other";
		Sys.println("cell_link=" + link(c, other) + "," + c.tag);
		Sys.println("cell_total=" + total(c));
		Sys.println("cell_describe=" + describe(c));
		Sys.println("cell_fields=" + c.x + "," + c.tag + "," + c.on);

		var lit:Point = {x: 1, y: 2.5, tag: "lit", on: false, big: haxe.Int64.ofInt(7)};
		Sys.println("lit_total=" + total(lit));
		Sys.println("lit_assign=" + assign(lit, 3));
		Sys.println("lit_link=" + link(lit, c) + "," + lit.tag);
		Sys.println("lit_describe=" + describe(lit));

		var d:Dynamic = {x: 9};
		var partial:Point = cast d;
		Sys.println("partial_x=" + partial.x);
		Sys.println("partial_assign=" + assign(partial, 6));
		Sys.println("partial_link=" + link(partial, c) + "," + partial.tag);
		Sys.println("partial_total=" + total(partial));
		Sys.println("partial_describe=" + describe(partial));
		Sys.println("partial_dyn=" + Reflect.field(d, "tag") + "," + Reflect.field(d, "y"));
	}
}
