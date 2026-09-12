// ORefOffset advances a ref by ELEMENTS of the ref's parameter type, the way
// HashLink's JIT does (`hl_type_size(dst->t->tparam)`), not by bytes.
//
// The ref is taken over the data of an hl.NativeArray, so an offset of 1 or 2
// lands on a neighbouring element and reads back a known value; a byte-scaled
// offset lands inside an element and reads back a different number. Each
// element width that is not 1 gets its own array: UI16, Int, Float, I64, and
// a pointer-sized String. Refs over locals only check offset(0), because the
// memory next to a local is nobody's.
//
// Every read goes through `probe`, which takes the ref as an argument so the
// compiler cannot fold the offset away. The case checks its own answers and
// exits non-zero on a mismatch.
class TestRefOffset {
	static var bad = 0;

	static function check(what:String, got:String, want:String) {
		Sys.println(what + " = " + got);
		if (got != want) {
			bad++;
			Sys.println("MISMATCH " + what + " got=" + got + " want=" + want);
		}
	}

	static function probeInt(r:hl.Ref<Int>, n:Int):Int return r.offset(n).get();
	static function probeU16(r:hl.Ref<hl.UI16>, n:Int):Int return r.offset(n).get();
	static function probeFloat(r:hl.Ref<Float>, n:Int):Float return r.offset(n).get();
	static function probeI64(r:hl.Ref<hl.I64>, n:Int):hl.I64 return r.offset(n).get();
	static function probeStr(r:hl.Ref<String>, n:Int):String return r.offset(n).get();

	static function setInt(r:hl.Ref<Int>, n:Int, v:Int) r.offset(n).set(v);
	static function setFloat(r:hl.Ref<Float>, n:Int, v:Float) r.offset(n).set(v);

	static function main() {
		var ints = new hl.NativeArray<Int>(4);
		for (i in 0...4) ints[i] = 1000 + i * 111;
		var ri:hl.Ref<Int> = ints.getRef();
		check("int[0]", "" + probeInt(ri, 0), "1000");
		check("int[1]", "" + probeInt(ri, 1), "1111");
		check("int[2]", "" + probeInt(ri, 2), "1222");
		check("int[3]", "" + probeInt(ri, 3), "1333");
		setInt(ri, 2, -5);
		check("int[2] after set", "" + ints[2], "-5");
		check("int[3] after set", "" + ints[3], "1333");

		var shorts = new hl.NativeArray<hl.UI16>(4);
		for (i in 0...4) shorts[i] = 0x100 * (i + 1) + i;
		var rs:hl.Ref<hl.UI16> = shorts.getRef();
		check("u16[1]", "" + probeU16(rs, 1), "" + (0x200 + 1));
		check("u16[2]", "" + probeU16(rs, 2), "" + (0x300 + 2));
		check("u16[3]", "" + probeU16(rs, 3), "" + (0x400 + 3));

		var floats = new hl.NativeArray<Float>(4);
		for (i in 0...4) floats[i] = 0.5 + i;
		var rf:hl.Ref<Float> = floats.getRef();
		check("float[1]", "" + probeFloat(rf, 1), "1.5");
		check("float[2]", "" + probeFloat(rf, 2), "2.5");
		check("float[3]", "" + probeFloat(rf, 3), "3.5");
		setFloat(rf, 1, -0.25);
		check("float[1] after set", "" + floats[1], "-0.25");
		check("float[2] after set", "" + floats[2], "2.5");

		var longs = new hl.NativeArray<hl.I64>(3);
		for (i in 0...3) longs[i] = (i + 1) * 7;
		var rl:hl.Ref<hl.I64> = longs.getRef();
		check("i64[1]", "" + probeI64(rl, 1), "14");
		check("i64[2]", "" + probeI64(rl, 2), "21");

		var strs = new hl.NativeArray<String>(3);
		strs[0] = "zero";
		strs[1] = "one";
		strs[2] = "two";
		var rp:hl.Ref<String> = strs.getRef();
		check("str[1]", probeStr(rp, 1), "one");
		check("str[2]", probeStr(rp, 2), "two");

		var x = 42;
		var rx = hl.Ref.make(x);
		check("local int", "" + probeInt(rx, 0), "42");
		var y = 6.75;
		var ry = hl.Ref.make(y);
		check("local float", "" + probeFloat(ry, 0), "6.75");

		if (bad > 0) {
			Sys.println("FAILED " + bad);
			Sys.exit(1);
		}
		Sys.println("OK");
	}
}
