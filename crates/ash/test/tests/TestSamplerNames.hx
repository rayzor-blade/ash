// Two hot functions the sampler must name: an instance method (a proto of
// Spinner) and a static function (a binding of TestSamplerNames's statics).
class Spinner {
	public var n:Int = 0;

	public function new() {}

	public function spin(count:Int):Int {
		var s = 0;
		for (i in 0...count)
			s = (s * 31 + i) & 0xFFFFFF;
		n = s;
		return s;
	}
}

class TestSamplerNames {
	@:noInline static function hot(count:Int):Int {
		var s = 0;
		for (i in 0...count)
			s = (s ^ (i * 7)) & 0xFFFFF;
		return s;
	}

	static function main() {
		var spinner = new Spinner();
		Sys.println("done " + (spinner.spin(30000000) + hot(30000000)));
	}
}
