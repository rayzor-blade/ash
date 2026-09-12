// A field read through Dynamic that lands in a Bool, UI8 or UI16 register,
// and a byte or word read from hl.Bytes that lands in a UI8 or UI16 register.
//
// Haxe itself reads a Dynamic field into a Dynamic register and casts, and
// reads bytes into an Int register and narrows, so the compiled program only
// reaches those two-step shapes. The narrow_reg_stores test retypes this
// program's bytecode so the DynGet / GetI8 / GetI16 write the narrow register
// directly, which is the shape HashLink's own JIT accepts; the printed values
// have to survive both.
//
// Every reader keeps an Int local live across the narrow read: a store wider
// than the narrow slot lands in a neighbouring register, and the guard is the
// neighbour that shows it. The narrow value is widened to Int before it meets
// the guard, because UI8 + Int is a UI8 addition in Haxe.
class TestNarrowDynGet {
	static function dynBool(d:Dynamic, salt:Int):Int {
		var guard = salt * 3 + 1;
		var b:Bool = untyped d.flag;
		return (b ? 1 : 0) + guard;
	}

	static function dynU8(d:Dynamic, salt:Int):Int {
		var guard = salt * 5 + 2;
		var u:hl.UI8 = untyped d.byte;
		var v:Int = u;
		return v + guard;
	}

	static function dynU16(d:Dynamic, salt:Int):Int {
		var guard = salt * 7 + 3;
		var u:hl.UI16 = untyped d.word;
		var v:Int = u;
		return v + guard;
	}

	static function byteU8(b:hl.Bytes, i:Int, salt:Int):Int {
		var guard = salt * 11 + 4;
		var u:hl.UI8 = b.getUI8(i);
		var v:Int = u;
		return v + guard;
	}

	static function byteU16(b:hl.Bytes, i:Int, salt:Int):Int {
		var guard = salt * 13 + 5;
		var u:hl.UI16 = b.getUI16(i);
		var v:Int = u;
		return v + guard;
	}

	static inline var N = 300;

	static var bad = 0;

	static function check(what:String, got:Int, want:Int) {
		if (got != want) {
			bad++;
			Sys.println("MISMATCH " + what + " got=" + got + " want=" + want);
		}
	}

	static function main() {
		var bytes = haxe.io.Bytes.alloc(8);
		bytes.set(0, 201);
		bytes.set(1, 7);
		bytes.setUInt16(2, 61000);
		var raw = bytes.getData();

		var sbool = 0, su8 = 0, su16 = 0, sb8 = 0, sb16 = 0;
		for (i in 0...N) {
			var d:Dynamic = {flag: (i & 1) == 0, byte: 200 + (i % 50), word: 60000 + i};
			sbool += dynBool(d, i);
			su8 += dynU8(d, i);
			su16 += dynU16(d, i);
			bytes.set(0, 200 + (i % 56));
			bytes.setUInt16(2, 65000 + i);
			sb8 += byteU8(raw, 0, i);
			sb16 += byteU16(raw, 2, i);
		}

		var xbool = 0, xu8 = 0, xu16 = 0, xb8 = 0, xb16 = 0;
		for (i in 0...N) {
			xbool += ((i & 1) == 0 ? 1 : 0) + i * 3 + 1;
			xu8 += 200 + (i % 50) + i * 5 + 2;
			xu16 += 60000 + i + i * 7 + 3;
			xb8 += 200 + (i % 56) + i * 11 + 4;
			xb16 += 65000 + i + i * 13 + 5;
		}

		check("dynBool", sbool, xbool);
		check("dynU8", su8, xu8);
		check("dynU16", su16, xu16);
		check("byteU8", sb8, xb8);
		check("byteU16", sb16, xb16);

		Sys.println("dynBool=" + sbool + " dynU8=" + su8 + " dynU16=" + su16);
		Sys.println("byteU8=" + sb8 + " byteU16=" + sb16);
		Sys.println("Checksum: " + (sbool + su8 + su16 + sb8 + sb16));
		if (bad != 0) {
			Sys.println("FAILED: " + bad + " narrow reads clobbered a neighbour or lost bits");
			Sys.exit(1);
		}
	}
}
