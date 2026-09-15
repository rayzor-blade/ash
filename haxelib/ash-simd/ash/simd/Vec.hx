package ash.simd;

/**
	The ash-simd primitives, one static per native of lib `simd`.

	A vector is 16 bytes of an `hl.Bytes` named by the bytes and a byte
	offset; every operation reads its operands from such slots and writes
	its result into one. Lanes are read and written with the `hl.Bytes`
	accessors (`getF32`/`setF32`, `getI32`/`setI32`, ...). Loads and stores
	are unaligned, and a destination may alias a source. Nothing is bounds
	checked.

	Lane semantics: integer arithmetic wraps and shifts mask the count to
	the lane width; float `min`/`max` give NaN for a NaN operand and order
	-0 below +0; comparisons write a lane of all ones or all zeros;
	`v128Select` takes bits of `a` where the mask is set, else `b`;
	reductions fold lanes in order from lane 0; `f32x4ToI32x4` saturates
	and maps NaN to 0.

	On stock HashLink these are `simd.hdll`. On ash they are part of the
	runtime, so nothing is shipped beside the program.
**/
extern class Vec {
	// f32x4
	@:hlNative("simd", "f32x4_add") public static function f32x4Add(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f32x4_sub") public static function f32x4Sub(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f32x4_mul") public static function f32x4Mul(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f32x4_div") public static function f32x4Div(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f32x4_min") public static function f32x4Min(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f32x4_max") public static function f32x4Max(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f32x4_abs") public static function f32x4Abs(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int):Void;
	@:hlNative("simd", "f32x4_neg") public static function f32x4Neg(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int):Void;
	@:hlNative("simd", "f32x4_sqrt") public static function f32x4Sqrt(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int):Void;
	@:hlNative("simd", "f32x4_fma") public static function f32x4Fma(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int, c:hl.Bytes, ci:Int):Void;
	@:hlNative("simd", "f32x4_splat") public static function f32x4Splat(dst:hl.Bytes, di:Int, x:Single):Void;
	@:hlNative("simd", "f32x4_eq") public static function f32x4Eq(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f32x4_ne") public static function f32x4Ne(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f32x4_lt") public static function f32x4Lt(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f32x4_le") public static function f32x4Le(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f32x4_gt") public static function f32x4Gt(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f32x4_ge") public static function f32x4Ge(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f32x4_sum") public static function f32x4Sum(a:hl.Bytes, ai:Int):Single;
	@:hlNative("simd", "f32x4_min_lane") public static function f32x4MinLane(a:hl.Bytes, ai:Int):Single;
	@:hlNative("simd", "f32x4_max_lane") public static function f32x4MaxLane(a:hl.Bytes, ai:Int):Single;
	@:hlNative("simd", "f32x4_load_array") public static function f32x4LoadArray(dst:hl.Bytes, di:Int, arr:hl.NativeArray<Single>, index:Int):Void;
	@:hlNative("simd", "f32x4_store_array") public static function f32x4StoreArray(arr:hl.NativeArray<Single>, index:Int, a:hl.Bytes, ai:Int):Void;

	// f64x2
	@:hlNative("simd", "f64x2_add") public static function f64x2Add(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f64x2_sub") public static function f64x2Sub(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f64x2_mul") public static function f64x2Mul(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f64x2_div") public static function f64x2Div(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f64x2_min") public static function f64x2Min(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f64x2_max") public static function f64x2Max(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f64x2_abs") public static function f64x2Abs(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int):Void;
	@:hlNative("simd", "f64x2_neg") public static function f64x2Neg(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int):Void;
	@:hlNative("simd", "f64x2_sqrt") public static function f64x2Sqrt(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int):Void;
	@:hlNative("simd", "f64x2_fma") public static function f64x2Fma(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int, c:hl.Bytes, ci:Int):Void;
	@:hlNative("simd", "f64x2_splat") public static function f64x2Splat(dst:hl.Bytes, di:Int, x:Float):Void;
	@:hlNative("simd", "f64x2_eq") public static function f64x2Eq(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f64x2_ne") public static function f64x2Ne(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f64x2_lt") public static function f64x2Lt(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f64x2_le") public static function f64x2Le(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f64x2_gt") public static function f64x2Gt(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f64x2_ge") public static function f64x2Ge(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "f64x2_sum") public static function f64x2Sum(a:hl.Bytes, ai:Int):Float;
	@:hlNative("simd", "f64x2_min_lane") public static function f64x2MinLane(a:hl.Bytes, ai:Int):Float;
	@:hlNative("simd", "f64x2_max_lane") public static function f64x2MaxLane(a:hl.Bytes, ai:Int):Float;
	@:hlNative("simd", "f64x2_load_array") public static function f64x2LoadArray(dst:hl.Bytes, di:Int, arr:hl.NativeArray<Float>, index:Int):Void;
	@:hlNative("simd", "f64x2_store_array") public static function f64x2StoreArray(arr:hl.NativeArray<Float>, index:Int, a:hl.Bytes, ai:Int):Void;

	// i32x4
	@:hlNative("simd", "i32x4_add") public static function i32x4Add(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i32x4_sub") public static function i32x4Sub(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i32x4_mul") public static function i32x4Mul(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i32x4_min") public static function i32x4Min(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i32x4_max") public static function i32x4Max(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i32x4_abs") public static function i32x4Abs(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int):Void;
	@:hlNative("simd", "i32x4_neg") public static function i32x4Neg(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int):Void;
	@:hlNative("simd", "i32x4_splat") public static function i32x4Splat(dst:hl.Bytes, di:Int, x:Int):Void;
	@:hlNative("simd", "i32x4_shl") public static function i32x4Shl(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, n:Int):Void;
	@:hlNative("simd", "i32x4_shr") public static function i32x4Shr(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, n:Int):Void;
	@:hlNative("simd", "i32x4_eq") public static function i32x4Eq(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i32x4_ne") public static function i32x4Ne(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i32x4_lt") public static function i32x4Lt(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i32x4_le") public static function i32x4Le(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i32x4_gt") public static function i32x4Gt(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i32x4_ge") public static function i32x4Ge(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i32x4_sum") public static function i32x4Sum(a:hl.Bytes, ai:Int):Int;
	@:hlNative("simd", "i32x4_min_lane") public static function i32x4MinLane(a:hl.Bytes, ai:Int):Int;
	@:hlNative("simd", "i32x4_max_lane") public static function i32x4MaxLane(a:hl.Bytes, ai:Int):Int;
	@:hlNative("simd", "i32x4_load_array") public static function i32x4LoadArray(dst:hl.Bytes, di:Int, arr:hl.NativeArray<Int>, index:Int):Void;
	@:hlNative("simd", "i32x4_store_array") public static function i32x4StoreArray(arr:hl.NativeArray<Int>, index:Int, a:hl.Bytes, ai:Int):Void;

	// i16x8
	@:hlNative("simd", "i16x8_add") public static function i16x8Add(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i16x8_sub") public static function i16x8Sub(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i16x8_mul") public static function i16x8Mul(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i16x8_min") public static function i16x8Min(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i16x8_max") public static function i16x8Max(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i16x8_abs") public static function i16x8Abs(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int):Void;
	@:hlNative("simd", "i16x8_neg") public static function i16x8Neg(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int):Void;
	@:hlNative("simd", "i16x8_splat") public static function i16x8Splat(dst:hl.Bytes, di:Int, x:Int):Void;
	@:hlNative("simd", "i16x8_shl") public static function i16x8Shl(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, n:Int):Void;
	@:hlNative("simd", "i16x8_shr") public static function i16x8Shr(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, n:Int):Void;
	@:hlNative("simd", "i16x8_eq") public static function i16x8Eq(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i16x8_ne") public static function i16x8Ne(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i16x8_lt") public static function i16x8Lt(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i16x8_le") public static function i16x8Le(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i16x8_gt") public static function i16x8Gt(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i16x8_ge") public static function i16x8Ge(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i16x8_sum") public static function i16x8Sum(a:hl.Bytes, ai:Int):Int;
	@:hlNative("simd", "i16x8_min_lane") public static function i16x8MinLane(a:hl.Bytes, ai:Int):Int;
	@:hlNative("simd", "i16x8_max_lane") public static function i16x8MaxLane(a:hl.Bytes, ai:Int):Int;
	@:hlNative("simd", "i16x8_load_array") public static function i16x8LoadArray(dst:hl.Bytes, di:Int, arr:hl.NativeArray<hl.UI16>, index:Int):Void;
	@:hlNative("simd", "i16x8_store_array") public static function i16x8StoreArray(arr:hl.NativeArray<hl.UI16>, index:Int, a:hl.Bytes, ai:Int):Void;

	// i8x16
	@:hlNative("simd", "i8x16_add") public static function i8x16Add(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i8x16_sub") public static function i8x16Sub(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i8x16_mul") public static function i8x16Mul(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i8x16_min") public static function i8x16Min(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i8x16_max") public static function i8x16Max(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i8x16_abs") public static function i8x16Abs(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int):Void;
	@:hlNative("simd", "i8x16_neg") public static function i8x16Neg(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int):Void;
	@:hlNative("simd", "i8x16_splat") public static function i8x16Splat(dst:hl.Bytes, di:Int, x:Int):Void;
	@:hlNative("simd", "i8x16_shl") public static function i8x16Shl(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, n:Int):Void;
	@:hlNative("simd", "i8x16_shr") public static function i8x16Shr(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, n:Int):Void;
	@:hlNative("simd", "i8x16_eq") public static function i8x16Eq(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i8x16_ne") public static function i8x16Ne(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i8x16_lt") public static function i8x16Lt(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i8x16_le") public static function i8x16Le(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i8x16_gt") public static function i8x16Gt(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i8x16_ge") public static function i8x16Ge(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "i8x16_sum") public static function i8x16Sum(a:hl.Bytes, ai:Int):Int;
	@:hlNative("simd", "i8x16_min_lane") public static function i8x16MinLane(a:hl.Bytes, ai:Int):Int;
	@:hlNative("simd", "i8x16_max_lane") public static function i8x16MaxLane(a:hl.Bytes, ai:Int):Int;
	@:hlNative("simd", "i8x16_load_array") public static function i8x16LoadArray(dst:hl.Bytes, di:Int, arr:hl.NativeArray<hl.UI8>, index:Int):Void;
	@:hlNative("simd", "i8x16_store_array") public static function i8x16StoreArray(arr:hl.NativeArray<hl.UI8>, index:Int, a:hl.Bytes, ai:Int):Void;

	// u8x16
	@:hlNative("simd", "u8x16_add") public static function u8x16Add(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "u8x16_sub") public static function u8x16Sub(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "u8x16_mul") public static function u8x16Mul(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "u8x16_min") public static function u8x16Min(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "u8x16_max") public static function u8x16Max(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "u8x16_splat") public static function u8x16Splat(dst:hl.Bytes, di:Int, x:Int):Void;
	@:hlNative("simd", "u8x16_shl") public static function u8x16Shl(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, n:Int):Void;
	@:hlNative("simd", "u8x16_shr") public static function u8x16Shr(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, n:Int):Void;
	@:hlNative("simd", "u8x16_eq") public static function u8x16Eq(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "u8x16_ne") public static function u8x16Ne(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "u8x16_lt") public static function u8x16Lt(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "u8x16_le") public static function u8x16Le(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "u8x16_gt") public static function u8x16Gt(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "u8x16_ge") public static function u8x16Ge(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "u8x16_sum") public static function u8x16Sum(a:hl.Bytes, ai:Int):Int;
	@:hlNative("simd", "u8x16_min_lane") public static function u8x16MinLane(a:hl.Bytes, ai:Int):Int;
	@:hlNative("simd", "u8x16_max_lane") public static function u8x16MaxLane(a:hl.Bytes, ai:Int):Int;
	@:hlNative("simd", "u8x16_load_array") public static function u8x16LoadArray(dst:hl.Bytes, di:Int, arr:hl.NativeArray<hl.UI8>, index:Int):Void;
	@:hlNative("simd", "u8x16_store_array") public static function u8x16StoreArray(arr:hl.NativeArray<hl.UI8>, index:Int, a:hl.Bytes, ai:Int):Void;

	// v128
	@:hlNative("simd", "v128_and") public static function v128And(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "v128_or") public static function v128Or(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "v128_xor") public static function v128Xor(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int):Void;
	@:hlNative("simd", "v128_not") public static function v128Not(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int):Void;
	@:hlNative("simd", "v128_select") public static function v128Select(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int, b:hl.Bytes, bi:Int, c:hl.Bytes, ci:Int):Void;

	// f32x4
	@:hlNative("simd", "f32x4_to_i32x4") public static function f32x4ToI32x4(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int):Void;

	// i32x4
	@:hlNative("simd", "i32x4_to_f32x4") public static function i32x4ToF32x4(dst:hl.Bytes, di:Int, a:hl.Bytes, ai:Int):Void;
}
