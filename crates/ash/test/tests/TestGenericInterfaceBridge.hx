// Calls through a generic interface slot, whose return is typed `T`, next to
// the same calls made directly and through a non-generic interface. Every
// line below must read the same on every engine as on stock HashLink.
//
// `single`: an f32 returned through `IGet<Single>` must keep its value.
// `throw`: an exception from a method reached through `IFail<Int>` must reach
// the caller's `try`.
// `dynamic`: a method called through a Dynamic closure or Reflect.callMethod
// must box its primitive result by its own type, and take an f32 argument. The program is aborted if it does not, so each section
// runs as its own process, selected by the first argument.

interface IGet<T> {
	function get():T;
}

class GetSingle implements IGet<Single> {
	public function new() {}

	public function get():Single
		return 200.5;
}

class GetInt implements IGet<Int> {
	public function new() {}

	public function get():Int
		return 200;
}

class GetFloat implements IGet<Float> {
	public function new() {}

	public function get():Float
		return 200.5;
}

interface IFail<T> {
	function fail():T;
}

interface IFailInt {
	function fail():Int;
}

class Fails implements IFail<Int> implements IFailInt {
	public function new() {}

	public function fail():Int {
		throw "boom";
	}
}

class Methods {
	public function new() {}

	public function single():Single
		return 200.5;

	public function float():Float
		return 200.5;

	public function int():Int
		return 7;

	public function twice(x:Single):Single
		return x * 2;
}

class TestGenericInterfaceBridge {
	static function main() {
		switch Sys.args()[0] {
			case "single":
				var direct = new GetSingle();
				Sys.println("direct Single = " + direct.get());
				var viaSingle:IGet<Single> = direct;
				Sys.println("IGet<Single> = " + viaSingle.get());
				var viaInt:IGet<Int> = new GetInt();
				Sys.println("IGet<Int> = " + viaInt.get());
				var viaFloat:IGet<Float> = new GetFloat();
				Sys.println("IGet<Float> = " + viaFloat.get());

			case "throw":
				var f = new Fails();
				try {
					f.fail();
				} catch (e:Dynamic) {
					Sys.println("direct: caught " + e);
				}
				var plain:IFailInt = f;
				try {
					plain.fail();
				} catch (e:Dynamic) {
					Sys.println("IFailInt: caught " + e);
				}
				var generic:IFail<Int> = f;
				try {
					generic.fail();
				} catch (e:Dynamic) {
					Sys.println("IFail<Int>: caught " + e);
				}
				Sys.println("returned");

			case "dynamic":
				var m = new Methods();
				var single:Dynamic = m.single;
				Sys.println("Dynamic Single = " + single());
				var float:Dynamic = m.float;
				Sys.println("Dynamic Float = " + float());
				var int:Dynamic = m.int;
				Sys.println("Dynamic Int = " + int());
				var twice:Dynamic = m.twice;
				Sys.println("Dynamic Single arg = " + twice(21.25));
				Sys.println("callMethod Single = " + Reflect.callMethod(m, Reflect.field(m, "single"), []));

			case other:
				Sys.println("unknown section: " + other);
				Sys.exit(2);
		}
	}
}
