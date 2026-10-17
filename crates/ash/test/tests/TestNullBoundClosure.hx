// A closure that captures null is still a bound closure: the callee's first
// parameter is the captured null, and the call's own arguments follow it.
class Draft {
	public final bone:String;

	public function new(bone:String)
		this.bone = bone;
}

class TestNullBoundClosure {
	static function main() {
		var drafts = [for (i in 0...3) new Draft("bone" + i)];
		var withNull = drafts.concat([new Draft(null)]);
		var none:String = null;
		var f = (s:String) -> s == none;
		Sys.println("direct " + f("bone0") + " " + f(null));
		Sys.println("exists null " + Lambda.exists(withNull, d -> d.bone == none));
		Sys.println("exists absent " + Lambda.exists(drafts, d -> d.bone == none));
		Sys.println("count " + Lambda.count(withNull, d -> d.bone != none));
		Sys.println("map " + Lambda.map(withNull, d -> d.bone == none ? "n" : d.bone).join(","));
		Sys.println("filter " + withNull.filter(d -> d.bone == none).length);
		var obj:Draft = null;
		Sys.println("object " + Lambda.exists(drafts, d -> d == obj));
		var other = drafts[1];
		Sys.println("bound object " + Lambda.exists(drafts, d -> d == other));
	}
}
