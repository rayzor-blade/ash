// hl.I64 values built from Int literals, negative and at the 32-bit edges.
//
// Haxe emits every one as an Int constant followed by OToInt into the I64
// register. HashLink's JIT also accepts OInt straight into an I64 register,
// sign-extending the pool value; the int_into_i64 test retypes this
// program's bytecode into that shape, and the printed values have to
// survive both.
class TestIntIntoI64 {
	static var g:hl.I64 = -7;

	static function neg():hl.I64 return -5;
	static function minInt():hl.I64 return -2147483648;
	static function maxInt():hl.I64 return 2147483647;
	static function minusOne():hl.I64 return -1;

	static function lt(v:hl.I64, w:hl.I64):Bool return v < w;

	static function main() {
		var a:hl.I64 = -3;
		var sum = a + neg() + minInt() + maxInt() + minusOne() + g;
		Sys.println("sum=" + Std.string(sum));
		Sys.println("neg<0=" + lt(neg(), 0));
		Sys.println("min<max=" + lt(minInt(), maxInt()));
		Sys.println("minusOne<min=" + lt(minusOne(), minInt()));
		var arr = new hl.NativeArray<hl.I64>(3);
		arr[0] = -9;
		arr[1] = minInt();
		arr[2] = maxInt();
		var total:hl.I64 = 0;
		for (i in 0...3) total = total + arr[i];
		Sys.println("total=" + Std.string(total));
	}
}
