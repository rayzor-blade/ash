package ash.simd;

/**
	Four `Int` lanes in a 16-byte `hl.Bytes` of their own. Arithmetic wraps.

	Also the mask type: a comparison on any four-lane vector yields an
	`Int32x4` whose lanes are all ones or all zeros.
**/
abstract Int32x4(hl.Bytes) {
	inline function new(b:hl.Bytes)
		this = b;

	static inline function raw(v:Int32x4):hl.Bytes
		return cast v;

	static function alloc():Int32x4
		return new Int32x4(new hl.Bytes(16));

	public static function splat(x:Int):Int32x4 {
		var r = alloc();
		Vec.i32x4Splat(raw(r), 0, x);
		return r;
	}

	public static function make(x:Int, y:Int, z:Int, w:Int):Int32x4 {
		var b = new hl.Bytes(16);
		b.setI32(0, x);
		b.setI32(4, y);
		b.setI32(8, z);
		b.setI32(12, w);
		return new Int32x4(b);
	}

	public static function load(b:hl.Bytes, offset:Int):Int32x4 {
		var r = new hl.Bytes(16);
		r.blit(0, b, offset, 16);
		return new Int32x4(r);
	}

	public function store(b:hl.Bytes, offset:Int):Void
		b.blit(offset, this, 0, 16);

	public function get(i:Int):Int
		return this.getI32(i << 2);

	public function set(i:Int, v:Int):Void
		this.setI32(i << 2, v);

	public var x(get, never):Int;
	public var y(get, never):Int;
	public var z(get, never):Int;
	public var w(get, never):Int;

	function get_x():Int
		return this.getI32(0);

	function get_y():Int
		return this.getI32(4);

	function get_z():Int
		return this.getI32(8);

	function get_w():Int
		return this.getI32(12);

	@:op(A + B) public function add(b:Int32x4):Int32x4 {
		var r = alloc();
		Vec.i32x4Add(raw(r), 0, this, 0, raw(b), 0);
		return r;
	}

	@:op(A - B) public function sub(b:Int32x4):Int32x4 {
		var r = alloc();
		Vec.i32x4Sub(raw(r), 0, this, 0, raw(b), 0);
		return r;
	}

	@:op(A * B) public function mul(b:Int32x4):Int32x4 {
		var r = alloc();
		Vec.i32x4Mul(raw(r), 0, this, 0, raw(b), 0);
		return r;
	}

	@:op(-A) public function neg():Int32x4 {
		var r = alloc();
		Vec.i32x4Neg(raw(r), 0, this, 0);
		return r;
	}

	@:op(A & B) public function and(b:Int32x4):Int32x4 {
		var r = alloc();
		Vec.v128And(raw(r), 0, this, 0, raw(b), 0);
		return r;
	}

	@:op(A | B) public function or(b:Int32x4):Int32x4 {
		var r = alloc();
		Vec.v128Or(raw(r), 0, this, 0, raw(b), 0);
		return r;
	}

	@:op(A ^ B) public function xor(b:Int32x4):Int32x4 {
		var r = alloc();
		Vec.v128Xor(raw(r), 0, this, 0, raw(b), 0);
		return r;
	}

	@:op(~A) public function not():Int32x4 {
		var r = alloc();
		Vec.v128Not(raw(r), 0, this, 0);
		return r;
	}

	/** Shift left by `n` (masked to 0..31). **/
	@:op(A << B) public function shl(n:Int):Int32x4 {
		var r = alloc();
		Vec.i32x4Shl(raw(r), 0, this, 0, n);
		return r;
	}

	/** Arithmetic shift right by `n` (masked to 0..31). **/
	@:op(A >> B) public function shr(n:Int):Int32x4 {
		var r = alloc();
		Vec.i32x4Shr(raw(r), 0, this, 0, n);
		return r;
	}

	public function min(b:Int32x4):Int32x4 {
		var r = alloc();
		Vec.i32x4Min(raw(r), 0, this, 0, raw(b), 0);
		return r;
	}

	public function max(b:Int32x4):Int32x4 {
		var r = alloc();
		Vec.i32x4Max(raw(r), 0, this, 0, raw(b), 0);
		return r;
	}

	public function abs():Int32x4 {
		var r = alloc();
		Vec.i32x4Abs(raw(r), 0, this, 0);
		return r;
	}

	public function eq(b:Int32x4):Int32x4 {
		var r = alloc();
		Vec.i32x4Eq(raw(r), 0, this, 0, raw(b), 0);
		return r;
	}

	public function ne(b:Int32x4):Int32x4 {
		var r = alloc();
		Vec.i32x4Ne(raw(r), 0, this, 0, raw(b), 0);
		return r;
	}

	public function lt(b:Int32x4):Int32x4 {
		var r = alloc();
		Vec.i32x4Lt(raw(r), 0, this, 0, raw(b), 0);
		return r;
	}

	public function le(b:Int32x4):Int32x4 {
		var r = alloc();
		Vec.i32x4Le(raw(r), 0, this, 0, raw(b), 0);
		return r;
	}

	public function gt(b:Int32x4):Int32x4 {
		var r = alloc();
		Vec.i32x4Gt(raw(r), 0, this, 0, raw(b), 0);
		return r;
	}

	public function ge(b:Int32x4):Int32x4 {
		var r = alloc();
		Vec.i32x4Ge(raw(r), 0, this, 0, raw(b), 0);
		return r;
	}

	/** Lanes of `a` where `mask` is set, of `b` elsewhere. **/
	public static function select(mask:Int32x4, a:Int32x4, b:Int32x4):Int32x4 {
		var r = alloc();
		Vec.v128Select(raw(r), 0, raw(mask), 0, raw(a), 0, raw(b), 0);
		return r;
	}

	/** The lanes added, wrapping. **/
	public function sum():Int
		return Vec.i32x4Sum(this, 0);

	public function minLane():Int
		return Vec.i32x4MinLane(this, 0);

	public function maxLane():Int
		return Vec.i32x4MaxLane(this, 0);

	public function toFloat32x4():Float32x4 {
		var r = new hl.Bytes(16);
		Vec.i32x4ToF32x4(r, 0, this, 0);
		return cast r;
	}

	public function toString():String
		return "Int32x4(" + get_x() + ", " + get_y() + ", " + get_z() + ", " + get_w() + ")";
}
