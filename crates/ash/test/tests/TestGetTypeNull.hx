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
	// Object field order is not defined -- neither for an anonymous structure
	// nor for Reflect.fields -- so comparing the raw text compares an
	// implementation detail. Re-emit with the keys sorted: what is under test
	// is that the same fields carry the same values.
	static function canonical(json:String):String {
		var o = haxe.Json.parse(json);
		var fs = Reflect.fields(o);
		fs.sort(Reflect.compare);
		var parts = [
			for (f in fs)
				haxe.Json.stringify(f) + ":" + haxe.Json.stringify(Reflect.field(o, f))
		];
		return "{" + parts.join(",") + "}";
	}

	// Std.string of a Class is target-specific spelling -- `$String` on
	// HashLink, `Class<String>` on eval -- and says nothing about typeof
	// itself. Name the class instead.
	static function typeName(v:Dynamic):String {
		return switch (Type.typeof(v)) {
			case TClass(c): "TClass(" + Type.getClassName(c) + ")";
			case t: Std.string(t);
		}
	}

	static function main() {
		var n:Dynamic = null;
		trace("typeof(null)=" + Type.typeof(n));
		trace("isFunction(null)=" + Reflect.isFunction(n));
		trace("isObject(null)=" + Reflect.isObject(n));
		trace("isEnumValue(null)=" + Reflect.isEnumValue(n));
		trace("json=" + canonical(haxe.Json.stringify({a: null, b: 1, c: "x"})));
		trace("holder=" + canonical(haxe.Json.stringify(new Holder())));
		var h:Dynamic = new Holder();
		var fields = Reflect.fields(h);
		fields.sort(Reflect.compare);
		for (f in fields) {
			var v = Reflect.field(h, f);
			trace(f + ": fn=" + Reflect.isFunction(v) + " type=" + typeName(v));
		}
		trace("compare=" + Reflect.compare(n, n) + "," + Reflect.compare(n, 1));
	}
}
