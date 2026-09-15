package ash.simd;

/**
	Four `Single` lanes in a 16-byte `hl.Bytes` of their own.

	Each operation returns a fresh vector. On stock HashLink that is an
	allocation per result, so a hot loop is better written against
	`ash.simd.Vec` and scratch slots; on ash the compiled tiers replace these
	calls with vector instructions and keep the value in a register.

	Comparisons return an `Int32x4` mask, a lane of all ones where the
	comparison holds; `select` chooses lanes by such a mask.
**/
abstract Float32x4(hl.Bytes) {
	inline function new(b:hl.Bytes)
		this = b;

	static inline function raw(v:Float32x4):hl.Bytes
		return cast v;

	static function alloc():Float32x4
		return new Float32x4(new hl.Bytes(16));

	public static function splat(x:Single):Float32x4 {
		var r = alloc();
		Vec.f32x4Splat(raw(r), 0, x);
		return r;
	}

	public static function make(x:Single, y:Single, z:Single, w:Single):Float32x4 {
		var b = new hl.Bytes(16);
		b.setF32(0, x);
		b.setF32(4, y);
		b.setF32(8, z);
		b.setF32(12, w);
		return new Float32x4(b);
	}

	/** The 16 bytes at `offset` of `b`, copied. **/
	public static function load(b:hl.Bytes, offset:Int):Float32x4 {
		var r = alloc();
		Vec.v128Copy(raw(r), 0, b, offset);
		return r;
	}

	/** Copies the lanes into the 16 bytes at `offset` of `b`. **/
	public function store(b:hl.Bytes, offset:Int):Void
		Vec.v128Copy(b, offset, this, 0);

	public function get(i:Int):Single
		return this.getF32(i << 2);

	public function set(i:Int, v:Single):Void
		this.setF32(i << 2, v);

	public var x(get, never):Single;
	public var y(get, never):Single;
	public var z(get, never):Single;
	public var w(get, never):Single;

	function get_x():Single
		return this.getF32(0);

	function get_y():Single
		return this.getF32(4);

	function get_z():Single
		return this.getF32(8);

	function get_w():Single
		return this.getF32(12);

	@:op(A + B) public function add(b:Float32x4):Float32x4 {
		var r = alloc();
		Vec.f32x4Add(raw(r), 0, this, 0, raw(b), 0);
		return r;
	}

	@:op(A - B) public function sub(b:Float32x4):Float32x4 {
		var r = alloc();
		Vec.f32x4Sub(raw(r), 0, this, 0, raw(b), 0);
		return r;
	}

	@:op(A * B) public function mul(b:Float32x4):Float32x4 {
		var r = alloc();
		Vec.f32x4Mul(raw(r), 0, this, 0, raw(b), 0);
		return r;
	}

	@:op(A / B) public function div(b:Float32x4):Float32x4 {
		var r = alloc();
		Vec.f32x4Div(raw(r), 0, this, 0, raw(b), 0);
		return r;
	}

	@:op(-A) public function neg():Float32x4 {
		var r = alloc();
		Vec.f32x4Neg(raw(r), 0, this, 0);
		return r;
	}

	public function min(b:Float32x4):Float32x4 {
		var r = alloc();
		Vec.f32x4Min(raw(r), 0, this, 0, raw(b), 0);
		return r;
	}

	public function max(b:Float32x4):Float32x4 {
		var r = alloc();
		Vec.f32x4Max(raw(r), 0, this, 0, raw(b), 0);
		return r;
	}

	public function abs():Float32x4 {
		var r = alloc();
		Vec.f32x4Abs(raw(r), 0, this, 0);
		return r;
	}

	public function sqrt():Float32x4 {
		var r = alloc();
		Vec.f32x4Sqrt(raw(r), 0, this, 0);
		return r;
	}

	/** `this * b + c`, rounded once. **/
	public function fma(b:Float32x4, c:Float32x4):Float32x4 {
		var r = alloc();
		Vec.f32x4Fma(raw(r), 0, this, 0, raw(b), 0, raw(c), 0);
		return r;
	}

	public function eq(b:Float32x4):Int32x4 {
		var r = new hl.Bytes(16);
		Vec.f32x4Eq(r, 0, this, 0, raw(b), 0);
		return cast r;
	}

	public function ne(b:Float32x4):Int32x4 {
		var r = new hl.Bytes(16);
		Vec.f32x4Ne(r, 0, this, 0, raw(b), 0);
		return cast r;
	}

	public function lt(b:Float32x4):Int32x4 {
		var r = new hl.Bytes(16);
		Vec.f32x4Lt(r, 0, this, 0, raw(b), 0);
		return cast r;
	}

	public function le(b:Float32x4):Int32x4 {
		var r = new hl.Bytes(16);
		Vec.f32x4Le(r, 0, this, 0, raw(b), 0);
		return cast r;
	}

	public function gt(b:Float32x4):Int32x4 {
		var r = new hl.Bytes(16);
		Vec.f32x4Gt(r, 0, this, 0, raw(b), 0);
		return cast r;
	}

	public function ge(b:Float32x4):Int32x4 {
		var r = new hl.Bytes(16);
		Vec.f32x4Ge(r, 0, this, 0, raw(b), 0);
		return cast r;
	}

	/** Lanes of `a` where `mask` is set, of `b` elsewhere. **/
	public static function select(mask:Int32x4, a:Float32x4, b:Float32x4):Float32x4 {
		var r = alloc();
		Vec.v128Select(raw(r), 0, cast mask, 0, raw(a), 0, raw(b), 0);
		return r;
	}

	public function sum():Single
		return Vec.f32x4Sum(this, 0);

	public function minLane():Single
		return Vec.f32x4MinLane(this, 0);

	public function maxLane():Single
		return Vec.f32x4MaxLane(this, 0);

	/** Each lane converted to `Int`, saturating; NaN becomes 0. **/
	public function toInt32x4():Int32x4 {
		var r = new hl.Bytes(16);
		Vec.f32x4ToI32x4(r, 0, this, 0);
		return cast r;
	}

	public function toString():String
		return "Float32x4(" + get_x() + ", " + get_y() + ", " + get_z() + ", " + get_w() + ")";
}
