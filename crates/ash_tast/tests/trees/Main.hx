// The producer and consumer of a recursive tree, as the binary-trees benchmark has them: the
// engines can fold a tree that is built and checked at once into one recursion that builds nothing.
class TreeNode {
	var left:TreeNode;
	var right:TreeNode;
	var item:Int;

	public function new(left, right, item) {
		this.left = left;
		this.right = right;
		this.item = item;
	}

	public function itemCheck():Int {
		if (left == null)
			return item;
		return item + left.itemCheck() - right.itemCheck();
	}
}

class Main {
	static function bottomUpTree(item:Int, depth:Int):TreeNode {
		if (depth > 0)
			return new TreeNode(bottomUpTree(2 * item - 1, depth - 1), bottomUpTree(2 * item, depth - 1), item);
		return new TreeNode(null, null, item);
	}

	static function main() {
		var check = 0;
		for (depth in 4...9)
			check += bottomUpTree(0, depth).itemCheck();
		Sys.println("check " + check);
	}
}
