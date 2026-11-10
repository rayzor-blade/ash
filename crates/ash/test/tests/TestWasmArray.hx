// Arrays and raw memory through the direct wasm backend: typed arrays of
// every element width, native arrays and their sizes, byte buffers read and
// written at several widths, and reference stores into them.
// main prints one value per line.
class Item {
	public var n:Int;

	public function new(n:Int) {
		this.n = n;
	}
}

class TestWasmArray {
	static function sumInts(a:Array<Int>):Int {
		var s = 0;
		for (i in 0...a.length) s += a[i];
		return s;
	}

	static function scale(a:Array<Float>, k:Float):Float {
		var s = 0.0;
		for (i in 0...a.length) {
			a[i] = a[i] * k;
			s += a[i];
		}
		return s;
	}

	static function nativeSum(a:hl.NativeArray<Int>):Int {
		var s = 0;
		for (i in 0...a.length) s += a[i];
		return s;
	}

	static function nativeFill(a:hl.NativeArray<Float>, v:Float):Float {
		for (i in 0...a.length) a[i] = v + i;
		return a[a.length - 1];
	}

	static function nativeSize(a:hl.NativeArray<Int>):Int {
		return a == null ? -1 : a.length;
	}

	static function swapItems(a:hl.NativeArray<Item>, i:Int, j:Int):Int {
		var t = a[i];
		a[i] = a[j];
		a[j] = t;
		return a[0].n * 10 + a[1].n;
	}

	static function bytesRoundTrip(b:hl.Bytes, n:Int):Int {
		for (i in 0...n) b.setUI8(i, i * 37 + 5);
		var s = 0;
		for (i in 0...n) s += b.getUI8(i);
		b.setUI16(20, 51234);
		return s + b.getUI16(2) + b.getI32(4) + b.getUI16(20);
	}

	static function bytesWide(b:hl.Bytes):Float {
		b.setI32(0, -123456);
		b.setF64(8, 2.75);
		b.setF32(16, 1.5);
		return b.getI32(0) + b.getF64(8) + b.getF32(16);
	}

	static function main() {
		Sys.println("sum_ints=" + sumInts([1, 2, 3, 40, -5]));
		Sys.println("scale=" + scale([1.5, 2.5, 4.0], 2.0));
		var ints = new hl.NativeArray<Int>(5);
		for (i in 0...5) ints[i] = i * i;
		Sys.println("native_sum=" + nativeSum(ints));
		Sys.println("native_size=" + nativeSize(ints) + "," + nativeSize(null));
		var floats = new hl.NativeArray<Float>(4);
		Sys.println("native_fill=" + nativeFill(floats, 0.25) + "," + floats[0]);
		var items = new hl.NativeArray<Item>(2);
		items[0] = new Item(7);
		items[1] = new Item(9);
		Sys.println("swap=" + swapItems(items, 0, 1) + "," + items[1].n);
		var b = new hl.Bytes(32);
		Sys.println("bytes=" + bytesRoundTrip(b, 16));
		Sys.println("wide=" + bytesWide(b));
	}
}
