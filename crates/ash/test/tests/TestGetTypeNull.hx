// `hl_typeof(null)` is the void type. Type.typeof(null), Reflect.isFunction(null)
// and a JSON printer walking an object with a null field all read a null
// Dynamic's type; a tier that loaded `->t` without a null test faulted on the
// first null field the game serialized at startup. Runs once per program, so
// only a whole-program compile (AOT) ever executes it compiled.
class Holder {
	public var name:String;
	public var missing:String;
	public var count:Int;
	public var cb:Void->Void;
	public function new() {
		name = "holder";
		missing = null;
		count = 3;
		cb = null;
	}
}

class TestGetTypeNull {
	static function main() {
		var n:Dynamic = null;
		trace("typeof(null)=" + Type.typeof(n));
		trace("isFunction(null)=" + Reflect.isFunction(n));
		trace("isObject(null)=" + Reflect.isObject(n));
		trace("isEnumValue(null)=" + Reflect.isEnumValue(n));
		trace("json=" + haxe.Json.stringify({a: null, b: 1, c: "x"}));
		trace("holder=" + haxe.Json.stringify(new Holder()));
		var h:Dynamic = new Holder();
		for (f in Reflect.fields(h)) {
			var v = Reflect.field(h, f);
			trace(f + ": fn=" + Reflect.isFunction(v) + " type=" + Type.typeof(v));
		}
		trace("compare=" + Reflect.compare(n, n) + "," + Reflect.compare(n, 1));
	}
}
