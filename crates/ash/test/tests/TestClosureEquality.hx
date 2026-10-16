// Closures compared through Dynamic (Array.remove, Array.indexOf): the same
// function over the same bound value is one closure, anything else is not.
class Counter {
	public var n:Int;

	public function new(n:Int)
		this.n = n;

	public function bump():Void
		n++;

	public function other():Void
		n--;
}

class TestClosureEquality {
	static var seen = 0;

	static function capture(value:Int):Void->Void
		return () -> seen = value;

	static function statik():Void {}

	static function main() {
		var first = capture(1), second = capture(2);
		var callbacks = [first, second];
		callbacks.remove(second);
		callbacks[0]();
		Sys.println('capture remaining=${callbacks.length} seen=$seen');

		var a = new Counter(0), b = new Counter(0);
		var methods:Array<Void->Void> = [a.bump, b.bump];
		Sys.println('same object ${methods.indexOf(a.bump)} other object ${methods.indexOf(b.bump)}');
		Sys.println('other method ${methods.indexOf(a.other)}');
		methods.remove(b.bump);
		methods[0]();
		Sys.println('method remaining=${methods.length} a=${a.n} b=${b.n}');

		var statics:Array<Void->Void> = [statik, first];
		Sys.println('static ${statics.indexOf(statik)} capture ${statics.indexOf(first)} missing ${statics.indexOf(second)}');
		Sys.println('compareMethods ${Reflect.compareMethods(first, second)} ${Reflect.compareMethods(first, first)} ${Reflect.compareMethods(a.bump, a.bump)} ${Reflect.compareMethods(a.bump, b.bump)}');
	}
}
