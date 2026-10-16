// Dynamic calls with as many arguments as the runtime allows (nine, `this`
// included): the ninth is passed on the stack on every target.
class TestDynCallManyArgs {
	public var base:Int;

	public function new(b:Int)
		base = b;

	public function sum8(a:Int, b:Int, c:Int, d:Int, e:Int, f:Int, g:Int, h:Int):Int
		return base + a + b * 2 + c * 3 + d * 4 + e * 5 + f * 6 + g * 7 + h * 8;

	public function mixed8(a:Int, x:Float, b:Int, y:Float, c:Int, z:Float, d:Int, w:Float):Float
		return base + a + x + b * 2 + y * 2 + c * 3 + z * 3 + d * 4 + w * 4;

	public function floats8(a:Float, b:Float, c:Float, d:Float, e:Float, f:Float, g:Float, h:Float):Float
		return base + a + b * 2 + c * 3 + d * 4 + e * 5 + f * 6 + g * 7 + h * 8;

	public function strs8(a:String, b:String, c:String, d:String, e:String, f:String, g:String, h:String):String
		return a + b + c + d + e + f + g + h;

	static function stat9(a:Int, b:Int, c:Int, d:Int, e:Int, f:Int, g:Int, h:Int, i:Int):Int
		return a + b * 2 + c * 3 + d * 4 + e * 5 + f * 6 + g * 7 + h * 8 + i * 9;

	static function statf9(a:Float, b:Float, c:Float, d:Float, e:Float, f:Float, g:Float, h:Float, i:Float):Float
		return a + b * 2 + c * 3 + d * 4 + e * 5 + f * 6 + g * 7 + h * 8 + i * 9;

	static function statm9(a:Int, x:Float, b:Int, y:Float, c:Int, z:Float, d:Int, w:Float, e:Int):Float
		return a + x + b * 2 + y * 2 + c * 3 + z * 3 + d * 4 + w * 4 + e * 5;

	static function stats9(a:String, b:String, c:String, d:String, e:String, f:String, g:String, h:String, i:String):String
		return a + b + c + d + e + f + g + h + i;

	static function main() {
		var m = new TestDynCallManyArgs(100);
		var d:Dynamic = m;
		Sys.println("dyn sum8: " + d.sum8(1, 2, 3, 4, 5, 6, 7, 8));
		Sys.println("dyn mixed8: " + d.mixed8(1, 0.5, 2, 1.5, 3, 2.5, 4, 3.5));
		Sys.println("dyn floats8: " + d.floats8(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5));
		Sys.println("dyn strs8: " + d.strs8("a", "b", "c", "d", "e", "f", "g", "h"));
		Sys.println("reflect sum8: " + Reflect.callMethod(m, Reflect.field(m, "sum8"), [1, 2, 3, 4, 5, 6, 7, 8]));
		var f:Dynamic = stat9;
		Sys.println("dyn static9: " + f(1, 2, 3, 4, 5, 6, 7, 8, 9));
		Sys.println("reflect static9: " + Reflect.callMethod(null, stat9, [1, 2, 3, 4, 5, 6, 7, 8, 9]));
		var g:Dynamic = statf9;
		Sys.println("dyn staticf9: " + g(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5));
		Sys.println("reflect staticf9: " + Reflect.callMethod(null, statf9, [1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5]));
		var h:Dynamic = statm9;
		Sys.println("dyn staticm9: " + h(1, 0.5, 2, 1.5, 3, 2.5, 4, 3.5, 5));
		var s:Dynamic = stats9;
		Sys.println("dyn statics9: " + s("a", "b", "c", "d", "e", "f", "g", "h", "i"));
	}
}
