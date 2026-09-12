// A closure over a virtual method taken from a receiver whose static class
// neither declares nor overrides it: the override lives in a subclass, so
// the closure has to be built from the vtable slot, and the slot is not an
// index into the static class's own method list.
class A {
	public function new() {}
	public function g():String return "g";
	public function h():String return "h";
	public function f():String return "A.f";
}
class C extends A {
	public var tag:Int = 3;
}
class D extends C {
	override function f():String return "D.f";
}
class TestVirtualClosureInherited {
	static function main() {
		var c:C = new C();
		var cf = c.f;
		Sys.println(cf());
		var d:C = new D();
		var df = d.f;
		Sys.println(df());
		var dh = d.h;
		Sys.println(dh());
		var a:A = new D();
		var af = a.f;
		Sys.println(af());
	}
}
