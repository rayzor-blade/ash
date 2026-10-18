// A population the heap census must find: 5000 instances of Cell, a 1 MiB byte
// buffer and an array of 20000 integers, all kept alive. The program writes the
// memory dump to the file named by its first argument.
class Cell {
	public var a:Int;
	public var b:Float;
	public var next:Cell;

	public function new(a:Int)
		this.a = a;
}

class TestHeapCensus {
	static var cells:Array<Cell> = [];
	static var blob:haxe.io.Bytes;
	static var numbers:Array<Int> = [];

	static function main() {
		for (i in 0...5000) {
			var c = new Cell(i);
			if (i > 0)
				c.next = cells[i - 1];
			cells.push(c);
		}
		blob = haxe.io.Bytes.alloc(1 << 20);
		for (i in 0...20000)
			numbers.push(i);
		hl.Gc.major();
		hl.Gc.dumpMemory(Sys.args()[0]);
		Sys.println("cells " + cells.length + " blob " + blob.length + " numbers " + numbers.length);
	}
}
