// An Array<Dynamic> is read through its class's getDyn and setDyn, and trace writes its position
// object and its Dynamic arguments through the std's Log.
class Main {
	static function main() {
		var d = new Array<Dynamic>();
		d.push(5);
		d.push("x");
		var first:Int = d[0];
		d[1] = 7;
		var second:Int = d[1];
		Sys.println("dyn " + d.length + " " + first + " " + second);
		trace("traced " + first);
	}
}
