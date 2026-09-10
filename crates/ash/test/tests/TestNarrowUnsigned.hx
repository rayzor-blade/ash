// Arithmetic on HUI8 and HUI16, which are UNSIGNED in HashLink.
//
// The Cranelift tier keeps these at their narrow machine width, so every
// operation that reads a sign has to be told they have none: widening is a
// zero-extension, and division, remainder and the ordering comparisons take
// their unsigned forms. Read as signed, 0x80..0xFF and 0x8000..0xFFFF order
// below zero and `/` and `%` answer for a different number.
//
// Each narrow loop carries one operation and one accumulator and nothing
// else. Whether a value reaches the operation at its narrow width depends on
// the shape of the code around it, and a loop that also computes the expected
// answer widens everything first and stops asking the question -- so the
// expectations are computed afterwards, in separate Int-only loops.
class TestNarrowUnsigned {
	static function udiv(a:hl.UI16, b:hl.UI16):Int return Std.int(a / b);
	static function umod(a:hl.UI16, b:hl.UI16):Int return a % b;
	static function ugt(a:hl.UI16, b:hl.UI16):Bool return a > b;
	static function bmod(a:hl.UI8, b:hl.UI8):Int return a % b;
	static function bgt(a:hl.UI8, b:hl.UI8):Bool return a > b;

	static inline var N = 200000;

	static var bad = 0;

	static function check(what:String, got:Int, want:Int) {
		if (got != want) {
			bad++;
			Sys.println("MISMATCH " + what + " got=" + got + " want=" + want);
		}
	}

	static function main() {
		var sdiv = 0;
		for (i in 0...N) {
			var a:hl.UI16 = (i % 65000) + 1;
			var b:hl.UI16 = (i % 251) + 1;
			sdiv += udiv(a, b);
		}
		var smod = 0;
		for (i in 0...N) {
			var a:hl.UI16 = (i % 65000) + 1;
			var b:hl.UI16 = (i % 251) + 1;
			smod += umod(a, b);
		}
		var ngt = 0;
		for (i in 0...N) {
			var a:hl.UI16 = (i % 65000) + 1;
			var b:hl.UI16 = (i % 251) + 1;
			if (ugt(a, b)) ngt++;
		}
		var sbmod = 0;
		for (i in 0...N) {
			var p:hl.UI8 = i % 256;
			var q:hl.UI8 = (i % 100) + 1;
			sbmod += bmod(p, q);
		}
		var nbgt = 0;
		for (i in 0...N) {
			var p:hl.UI8 = i % 256;
			var q:hl.UI8 = i % 100;
			if (bgt(p, q)) nbgt++;
		}

		var xdiv = 0, xmod = 0, xgt = 0, xbmod = 0, xbgt = 0;
		for (i in 0...N) {
			var a = (i % 65000) + 1;
			var b = (i % 251) + 1;
			xdiv += Std.int(a / b);
			xmod += a % b;
			if (a > b) xgt++;
			var p = i % 256;
			xbmod += p % ((i % 100) + 1);
			if (p > i % 100) xbgt++;
		}

		check("udiv", sdiv, xdiv);
		check("umod", smod, xmod);
		check("ugt", ngt, xgt);
		check("bmod", sbmod, xbmod);
		check("bgt", nbgt, xbgt);

		Sys.println("udiv=" + sdiv + " umod=" + smod + " ugt=" + ngt);
		Sys.println("bmod=" + sbmod + " bgt=" + nbgt);
		Sys.println("Checksum: " + (sdiv + smod + ngt + sbmod + nbgt));
		if (bad != 0) {
			Sys.println("FAILED: " + bad + " narrow unsigned operations read as signed");
			Sys.exit(1);
		}
	}
}
