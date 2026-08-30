// A large retained heap plus steady garbage, so collections happen with a
// live set comparable to a loaded game level. Marking cost scales with what
// survives, and that is the half worth measuring.
//
//   bench_gc_pause.hl [liveMB] [collections]
class Node {
	public var a:Node;
	public var b:Node;
	public var v:Int;

	public function new(v:Int) {
		this.v = v;
	}
}

class BenchGcPause {
	static function build(n:Int):Array<Node> {
		var live = new Array<Node>();
		var prev:Node = null;
		for (i in 0...n) {
			var node = new Node(i);
			// Chain them so marking has to trace, not just sweep a flat array.
			node.a = prev;
			if ((i & 3) == 0)
				node.b = prev;
			prev = node;
			live.push(node);
		}
		return live;
	}

	static function main() {
		var args = Sys.args();
		var liveMb = args.length > 0 ? Std.parseInt(args[0]) : 256;
		var rounds = args.length > 1 ? Std.parseInt(args[1]) : 12;

		// ~48 bytes per node with its header and three fields.
		var nodes = Std.int((liveMb * 1024 * 1024) / 48);
		var live = build(nodes);
		Sys.println("live nodes: " + live.length);

		// Steady garbage, to force collections against that live set.
		var sink = 0;
		for (r in 0...rounds) {
			var junk = build(Std.int(nodes / 4));
			sink += junk[junk.length - 1].v;
			junk = null;
		}
		Sys.println("done sink=" + sink + " live still=" + live.length);
	}
}
