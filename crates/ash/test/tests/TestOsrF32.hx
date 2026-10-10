// A loop over an f32 register in a function called once: the interpreter holds
// the register as the f64 it widens to, and the compiled entry has to narrow it
// back before the loop carries on.
class TestOsrF32 {
	static function run():Float {
		var acc:Single = 0.0;
		var i = 0;
		while (i < 3000000) {
			acc += (i & 255) * 0.001;
			i++;
		}
		return acc;
	}

	static function main() {
		Sys.println('f32=' + run());
	}
}
