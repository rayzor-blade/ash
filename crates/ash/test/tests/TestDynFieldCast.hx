// A failed cast raised while reading a dynamic field must reach the Haxe
// catch in every execution mode, wherever the read happens.
typedef Command = {run:Dynamic->Dynamic};

class Holder {
	public var items:Dynamic;

	public function new(items:Dynamic)
		this.items = items;

	public function floats():Float {
		var d:Array<Float> = items.v;
		return d[1] * 2.0;
	}
}

class TestDynFieldCast {
	static function viaStruct(c:Command, args:Dynamic):Dynamic {
		try {
			return c.run(args);
		} catch (e:Dynamic) {
			return "caught " + Std.string(e);
		}
	}

	static function direct(args:Dynamic):Dynamic {
		try {
			var d:Array<Float> = args.v;
			return d[1] * 2.0;
		} catch (e:Dynamic) {
			return "caught " + Std.string(e);
		}
	}

	static function viaMethod(h:Holder):Dynamic {
		try {
			return h.floats();
		} catch (e:Dynamic) {
			return "caught " + Std.string(e);
		}
	}

	static function main() {
		var ints:Dynamic = {v: [0, 90, 0]};
		var floats:Dynamic = {v: [0.0, 45.0, 0.0]};
		var convert:Command = {run: a -> {
			var d:Array<Float> = a.v;
			return d[1] * 2.0;
		}};
		Sys.println(viaStruct(convert, ints));
		Sys.println(viaStruct(convert, floats));
		Sys.println(direct(ints));
		Sys.println(direct(floats));
		Sys.println(viaMethod(new Holder(ints)));
		Sys.println(viaMethod(new Holder(floats)));
	}
}
