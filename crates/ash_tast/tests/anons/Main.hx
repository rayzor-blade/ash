typedef Point = {x:Int, y:Int};

class Main {
	// the literal's fields are the type's: a structure with its fields set by position
	static function make(a:Int, b:Int):Point {
		return {x: a, y: b};
	}

	// as a Dynamic, the literal is still the structure
	static function loose(a:Int):Dynamic {
		return {x: a, y: a};
	}

	// a literal of four fields for a type of five, the fifth optional: made by name, then converted
	static function where():haxe.PosInfos {
		return {fileName: "a.hx", lineNumber: 7, className: "C", methodName: "m"};
	}

	// trace passes its position as a literal of four fields to a type of five: a dynamic object,
	// converted to the structure
	static function shout(n:Int):Void {
		trace(n);
	}

	static function main() {}
}
