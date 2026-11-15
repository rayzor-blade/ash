class Node {
	public var value:Int;
	public var next:Node;

	public function new(value:Int, next:Node) {
		this.value = value;
		this.next = next;
	}
}

class Tree {
	public var left:Tree;
	public var right:Tree;

	public function new(left:Tree, right:Tree) {
		this.left = left;
		this.right = right;
	}
}

class Main {
	static function build(count:Int):Node {
		var head:Node = null;
		var i = 0;
		while (i < count) {
			head = new Node(i + 1, head);
			i++;
		}
		return head;
	}

	static function sum(list:Node):Int {
		var total = 0;
		while (list != null) {
			total += list.value;
			list = list.next;
		}
		return total;
	}

	static function length(list:Node):Int {
		if (list == null)
			return 0;
		return 1 + length(list.next);
	}

	static function make(depth:Int):Tree {
		if (depth == 0)
			return new Tree(null, null);
		return new Tree(make(depth - 1), make(depth - 1));
	}

	static function count(t:Tree):Int {
		if (t == null)
			return 0;
		return 1 + count(t.left) + count(t.right);
	}

	static function leaves(t:Tree):Int {
		if (t.left == null && t.right == null)
			return 1;
		return leaves(t.left) + leaves(t.right);
	}

	static function sumOf(n:Int):Int {
		return sum(build(n));
	}

	static function lengthOf(n:Int):Int {
		return length(build(n));
	}

	static function nodes(depth:Int):Int {
		return count(make(depth));
	}

	static function leafCount(depth:Int):Int {
		return leaves(make(depth));
	}

	static function main() {}
}
