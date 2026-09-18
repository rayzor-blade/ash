//! The `ash-simd` primitives.
//!
//! A vector is 16 bytes in an `hl.Bytes`, named by the bytes and a byte
//! offset; every primitive reads its operands from such slots and writes its
//! result to one, so nothing here allocates and nothing crosses a call
//! boundary as a vector value. Loads and stores are unaligned. Every operand
//! is read in full before the result is written, so a destination may alias a
//! source.
//!
//! Lane semantics, which the parity fixture pins and the compiled tiers must
//! reproduce when they replace a call with a vector instruction:
//! - integer add/sub/mul/neg/abs wrap; shifts mask the count to the lane width;
//! - float min/max are IEEE 754-2019 minimum/maximum: a NaN operand gives NaN,
//!   and -0 orders below +0;
//! - comparisons write a lane of all ones or all zeros, the lane width of the
//!   operands; `v128_select` picks bits of `a` where the mask is set, else `b`;
//! - reductions fold lanes in order from lane 0; integer sums wrap in the lane
//!   width before widening to `Int`;
//! - `f32x4_to_i32x4` saturates and maps NaN to 0.
//!
//! The bodies are plain lane loops. They are what the interpreter and stock
//! HashLink run, so they are the definition, and the compiler vectorises them
//! on its own.
//!
//! Safety, for every primitive: each `(bytes, offset)` pair must address 16
//! readable (writable, for the destination) bytes, and an array argument must
//! be a `varray` with enough elements from `index` on. Nothing is checked, as
//! for `hl.Bytes` itself.
#![allow(clippy::missing_safety_doc)]
// The argument lists are the primitives' C signatures: a slot is two arguments.
#![allow(clippy::too_many_arguments)]

mod prims;

use core::ptr::{read_unaligned, write_unaligned};

/// One lane: how it is read from and written to a slot, and how a comparison
/// result is written in its width.
pub trait Lane: Copy {
    const BYTES: usize;
    /// # Safety
    /// `p` must point at `Self::BYTES` readable bytes.
    unsafe fn load(p: *const u8) -> Self;
    /// # Safety
    /// `p` must point at `Self::BYTES` writable bytes.
    unsafe fn store(p: *mut u8, v: Self);
    /// # Safety
    /// `p` must point at `Self::BYTES` writable bytes.
    unsafe fn store_mask(p: *mut u8, set: bool);
}

macro_rules! lane {
    ($t:ty, $bits:ty) => {
        impl Lane for $t {
            const BYTES: usize = core::mem::size_of::<$t>();
            #[inline(always)]
            unsafe fn load(p: *const u8) -> Self {
                unsafe { read_unaligned(p as *const $t) }
            }
            #[inline(always)]
            unsafe fn store(p: *mut u8, v: Self) {
                unsafe { write_unaligned(p as *mut $t, v) }
            }
            #[inline(always)]
            unsafe fn store_mask(p: *mut u8, set: bool) {
                unsafe { write_unaligned(p as *mut $bits, if set { <$bits>::MAX } else { 0 }) }
            }
        }
    };
}
lane!(f32, u32);
lane!(f64, u64);
lane!(i32, u32);
lane!(i16, u16);
lane!(i8, u8);
lane!(u8, u8);

#[inline(always)]
unsafe fn slot(p: *const u8, off: i32) -> *const u8 {
    unsafe { p.offset(off as isize) }
}

#[inline(always)]
unsafe fn slot_mut(p: *mut u8, off: i32) -> *mut u8 {
    unsafe { p.offset(off as isize) }
}

#[inline(always)]
unsafe fn read<T: Lane, const N: usize>(p: *const u8, off: i32) -> [T; N] {
    unsafe {
        let p = slot(p, off);
        let mut v = [T::load(p); N];
        for (i, lane) in v.iter_mut().enumerate() {
            *lane = T::load(p.add(i * T::BYTES));
        }
        v
    }
}

#[inline(always)]
unsafe fn write<T: Lane, const N: usize>(p: *mut u8, off: i32, v: [T; N]) {
    unsafe {
        let p = slot_mut(p, off);
        for (i, lane) in v.iter().enumerate() {
            T::store(p.add(i * T::BYTES), *lane);
        }
    }
}

#[inline(always)]
unsafe fn write_mask<T: Lane, const N: usize>(p: *mut u8, off: i32, m: [bool; N]) {
    unsafe {
        let p = slot_mut(p, off);
        for (i, set) in m.iter().enumerate() {
            T::store_mask(p.add(i * T::BYTES), *set);
        }
    }
}

#[inline(always)]
unsafe fn map1<T: Lane, const N: usize>(
    d: *mut u8,
    di: i32,
    a: *const u8,
    ai: i32,
    f: impl Fn(T) -> T,
) {
    unsafe {
        let mut v: [T; N] = read(a, ai);
        for lane in v.iter_mut() {
            *lane = f(*lane);
        }
        write(d, di, v);
    }
}

#[inline(always)]
unsafe fn map2<T: Lane, const N: usize>(
    d: *mut u8,
    di: i32,
    a: *const u8,
    ai: i32,
    b: *const u8,
    bi: i32,
    f: impl Fn(T, T) -> T,
) {
    unsafe {
        let mut x: [T; N] = read(a, ai);
        let y: [T; N] = read(b, bi);
        for (lane, y) in x.iter_mut().zip(y) {
            *lane = f(*lane, y);
        }
        write(d, di, x);
    }
}

#[inline(always)]
unsafe fn map3<T: Lane, const N: usize>(
    d: *mut u8,
    di: i32,
    a: *const u8,
    ai: i32,
    b: *const u8,
    bi: i32,
    c: *const u8,
    ci: i32,
    f: impl Fn(T, T, T) -> T,
) {
    unsafe {
        let mut x: [T; N] = read(a, ai);
        let y: [T; N] = read(b, bi);
        let z: [T; N] = read(c, ci);
        for ((lane, y), z) in x.iter_mut().zip(y).zip(z) {
            *lane = f(*lane, y, z);
        }
        write(d, di, x);
    }
}

#[inline(always)]
unsafe fn cmp<T: Lane, const N: usize>(
    d: *mut u8,
    di: i32,
    a: *const u8,
    ai: i32,
    b: *const u8,
    bi: i32,
    f: impl Fn(T, T) -> bool,
) {
    unsafe {
        let x: [T; N] = read(a, ai);
        let y: [T; N] = read(b, bi);
        let mut m = [false; N];
        for (i, (x, y)) in x.iter().zip(y).enumerate() {
            m[i] = f(*x, y);
        }
        write_mask::<T, N>(d, di, m);
    }
}

#[inline(always)]
unsafe fn fold<T: Lane, const N: usize>(a: *const u8, ai: i32, f: impl Fn(T, T) -> T) -> T {
    unsafe {
        let v: [T; N] = read(a, ai);
        let mut acc = v[0];
        for lane in &v[1..] {
            acc = f(acc, *lane);
        }
        acc
    }
}

/// The header of a HashLink `varray`; the elements follow it.
#[repr(C)]
struct VArray {
    t: *const u8,
    at: *const u8,
    size: i32,
    pad: i32,
}

#[inline(always)]
unsafe fn array_elements<T: Lane>(arr: *const u8, index: i32) -> *const u8 {
    unsafe {
        arr.add(core::mem::size_of::<VArray>())
            .offset(index as isize * T::BYTES as isize)
    }
}

#[inline(always)]
unsafe fn load_arr<T: Lane, const N: usize>(d: *mut u8, di: i32, arr: *const u8, index: i32) {
    unsafe {
        let v: [T; N] = read(array_elements::<T>(arr, index), 0);
        write(d, di, v);
    }
}

#[inline(always)]
unsafe fn store_arr<T: Lane, const N: usize>(arr: *mut u8, index: i32, a: *const u8, ai: i32) {
    unsafe {
        let v: [T; N] = read(a, ai);
        write(array_elements::<T>(arr, index) as *mut u8, 0, v);
    }
}

/// IEEE 754-2019 minimum/maximum, generic over the two float widths.
///
/// Written as selects rather than branches so the lane loop vectorises:
/// the ordered pick, then the equal case resolved through the sign bits
/// (which is what orders -0 below +0), then NaN propagated by an addition.
trait Float: Lane + PartialOrd + core::ops::Add<Output = Self> {
    type Bits: Copy + core::ops::BitOr<Output = Self::Bits> + core::ops::BitAnd<Output = Self::Bits>;
    fn is_nan(self) -> bool;
    fn to_bits(self) -> Self::Bits;
    fn from_bits(b: Self::Bits) -> Self;
}
impl Float for f32 {
    type Bits = u32;
    #[inline(always)]
    fn is_nan(self) -> bool {
        f32::is_nan(self)
    }
    #[inline(always)]
    fn to_bits(self) -> u32 {
        f32::to_bits(self)
    }
    #[inline(always)]
    fn from_bits(b: u32) -> Self {
        f32::from_bits(b)
    }
}
impl Float for f64 {
    type Bits = u64;
    #[inline(always)]
    fn is_nan(self) -> bool {
        f64::is_nan(self)
    }
    #[inline(always)]
    fn to_bits(self) -> u64 {
        f64::to_bits(self)
    }
    #[inline(always)]
    fn from_bits(b: u64) -> Self {
        f64::from_bits(b)
    }
}

#[inline(always)]
fn fmin<T: Float>(a: T, b: T) -> T {
    let ordered = if a < b { a } else { b };
    let equal = T::from_bits(a.to_bits() | b.to_bits());
    let picked = if a == b { equal } else { ordered };
    if a.is_nan() || b.is_nan() {
        a + b
    } else {
        picked
    }
}

#[inline(always)]
fn fmax<T: Float>(a: T, b: T) -> T {
    let ordered = if a > b { a } else { b };
    let equal = T::from_bits(a.to_bits() & b.to_bits());
    let picked = if a == b { equal } else { ordered };
    if a.is_nan() || b.is_nan() {
        a + b
    } else {
        picked
    }
}

/// One primitive. On x86-64 the body is compiled twice, for the baseline
/// the crate targets and with AVX2 and FMA enabled, and the export jumps
/// through a per-primitive slot that the first call points at whichever the
/// CPU supports; after that a call costs one load and an indirect jump, and
/// `mul_add` is a fused instruction rather than a libm call per lane. Other
/// targets have their vector unit in the baseline.
macro_rules! prim {
    (fn $name:ident($($arg:ident: $ty:ty),* $(,)?) $(-> $ret:ty)? $body:block) => {
        pub unsafe extern "C" fn $name($($arg: $ty),*) $(-> $ret)? {
            #[cfg(target_arch = "x86_64")]
            {
                use core::sync::atomic::{AtomicPtr, Ordering::Relaxed};
                type Entry = unsafe extern "C" fn($($ty),*) $(-> $ret)?;
                unsafe extern "C" fn narrow($($arg: $ty),*) $(-> $ret)? {
                    unsafe { $body }
                }
                #[target_feature(enable = "avx2,fma")]
                unsafe extern "C" fn wide($($arg: $ty),*) $(-> $ret)? {
                    unsafe { $body }
                }
                unsafe extern "C" fn resolve($($arg: $ty),*) $(-> $ret)? {
                    let chosen: Entry = if x86_wide() { wide } else { narrow };
                    ENTRY.store(chosen as *mut (), Relaxed);
                    unsafe { chosen($($arg),*) }
                }
                static ENTRY: AtomicPtr<()> = AtomicPtr::new(resolve as *mut ());
                let entry: Entry = unsafe { core::mem::transmute(ENTRY.load(Relaxed)) };
                unsafe { entry($($arg),*) }
            }
            #[cfg(not(target_arch = "x86_64"))]
            unsafe {
                $body
            }
        }
    };
}

#[cfg(target_arch = "x86_64")]
fn x86_wide() -> bool {
    std::arch::is_x86_feature_detected!("avx2") && std::arch::is_x86_feature_detected!("fma")
}

// The body for each shape carries the C-ABI argument list of its primitive;
// the export lists in ash_std and ash_hdll_simd mirror these shapes.
macro_rules! float_ops {
    ($t:ty, $n:literal,
     $add:ident, $sub:ident, $mul:ident, $div:ident, $min:ident, $max:ident,
     $abs:ident, $neg:ident, $sqrt_:ident, $fma_:ident, $splat:ident,
     $eq:ident, $ne:ident, $lt:ident, $le:ident, $gt:ident, $ge:ident,
     $sum:ident, $min_lane:ident, $max_lane:ident, $load:ident, $store:ident) => {
        prim!(
            fn $add(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                map2::<$t, $n>(d, di, a, ai, b, bi, |x, y| x + y)
            }
        );
        prim!(
            fn $sub(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                map2::<$t, $n>(d, di, a, ai, b, bi, |x, y| x - y)
            }
        );
        prim!(
            fn $mul(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                map2::<$t, $n>(d, di, a, ai, b, bi, |x, y| x * y)
            }
        );
        prim!(
            fn $div(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                map2::<$t, $n>(d, di, a, ai, b, bi, |x, y| x / y)
            }
        );
        prim!(
            fn $min(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                map2::<$t, $n>(d, di, a, ai, b, bi, fmin)
            }
        );
        prim!(
            fn $max(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                map2::<$t, $n>(d, di, a, ai, b, bi, fmax)
            }
        );
        prim!(
            fn $abs(d: *mut u8, di: i32, a: *const u8, ai: i32) {
                map1::<$t, $n>(d, di, a, ai, |x| x.abs())
            }
        );
        prim!(
            fn $neg(d: *mut u8, di: i32, a: *const u8, ai: i32) {
                map1::<$t, $n>(d, di, a, ai, |x| -x)
            }
        );
        prim!(
            fn $sqrt_(d: *mut u8, di: i32, a: *const u8, ai: i32) {
                map1::<$t, $n>(d, di, a, ai, |x| x.sqrt())
            }
        );
        prim!(
            fn $fma_(
                d: *mut u8,
                di: i32,
                a: *const u8,
                ai: i32,
                b: *const u8,
                bi: i32,
                c: *const u8,
                ci: i32,
            ) {
                map3::<$t, $n>(d, di, a, ai, b, bi, c, ci, |x, y, z| x.mul_add(y, z))
            }
        );
        prim!(
            fn $splat(d: *mut u8, di: i32, x: $t) {
                write(d, di, [x; $n])
            }
        );
        prim!(
            fn $eq(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                cmp::<$t, $n>(d, di, a, ai, b, bi, |x, y| x == y)
            }
        );
        prim!(
            fn $ne(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                cmp::<$t, $n>(d, di, a, ai, b, bi, |x, y| x != y)
            }
        );
        prim!(
            fn $lt(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                cmp::<$t, $n>(d, di, a, ai, b, bi, |x, y| x < y)
            }
        );
        prim!(
            fn $le(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                cmp::<$t, $n>(d, di, a, ai, b, bi, |x, y| x <= y)
            }
        );
        prim!(
            fn $gt(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                cmp::<$t, $n>(d, di, a, ai, b, bi, |x, y| x > y)
            }
        );
        prim!(
            fn $ge(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                cmp::<$t, $n>(d, di, a, ai, b, bi, |x, y| x >= y)
            }
        );
        prim!(
            fn $sum(a: *const u8, ai: i32) -> $t {
                fold::<$t, $n>(a, ai, |x, y| x + y)
            }
        );
        prim!(
            fn $min_lane(a: *const u8, ai: i32) -> $t {
                fold::<$t, $n>(a, ai, fmin)
            }
        );
        prim!(
            fn $max_lane(a: *const u8, ai: i32) -> $t {
                fold::<$t, $n>(a, ai, fmax)
            }
        );
        prim!(
            fn $load(d: *mut u8, di: i32, arr: *const u8, index: i32) {
                load_arr::<$t, $n>(d, di, arr, index)
            }
        );
        prim!(
            fn $store(arr: *mut u8, index: i32, a: *const u8, ai: i32) {
                store_arr::<$t, $n>(arr, index, a, ai)
            }
        );
    };
}

float_ops!(
    f32,
    4,
    f32x4_add,
    f32x4_sub,
    f32x4_mul,
    f32x4_div,
    f32x4_min,
    f32x4_max,
    f32x4_abs,
    f32x4_neg,
    f32x4_sqrt,
    f32x4_fma,
    f32x4_splat,
    f32x4_eq,
    f32x4_ne,
    f32x4_lt,
    f32x4_le,
    f32x4_gt,
    f32x4_ge,
    f32x4_sum,
    f32x4_min_lane,
    f32x4_max_lane,
    f32x4_load_array,
    f32x4_store_array
);
float_ops!(
    f64,
    2,
    f64x2_add,
    f64x2_sub,
    f64x2_mul,
    f64x2_div,
    f64x2_min,
    f64x2_max,
    f64x2_abs,
    f64x2_neg,
    f64x2_sqrt,
    f64x2_fma,
    f64x2_splat,
    f64x2_eq,
    f64x2_ne,
    f64x2_lt,
    f64x2_le,
    f64x2_gt,
    f64x2_ge,
    f64x2_sum,
    f64x2_min_lane,
    f64x2_max_lane,
    f64x2_load_array,
    f64x2_store_array
);

macro_rules! int_common {
    ($t:ty, $n:literal,
     $add:ident, $sub:ident, $mul:ident, $min:ident, $max:ident, $splat:ident,
     $shl:ident, $shr:ident,
     $eq:ident, $ne:ident, $lt:ident, $le:ident, $gt:ident, $ge:ident,
     $sum:ident, $min_lane:ident, $max_lane:ident, $load:ident, $store:ident) => {
        prim!(
            fn $add(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                map2::<$t, $n>(d, di, a, ai, b, bi, |x, y| x.wrapping_add(y))
            }
        );
        prim!(
            fn $sub(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                map2::<$t, $n>(d, di, a, ai, b, bi, |x, y| x.wrapping_sub(y))
            }
        );
        prim!(
            fn $mul(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                map2::<$t, $n>(d, di, a, ai, b, bi, |x, y| x.wrapping_mul(y))
            }
        );
        prim!(
            fn $min(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                map2::<$t, $n>(d, di, a, ai, b, bi, |x, y| if y < x { y } else { x })
            }
        );
        prim!(
            fn $max(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                map2::<$t, $n>(d, di, a, ai, b, bi, |x, y| if y > x { y } else { x })
            }
        );
        prim!(
            fn $splat(d: *mut u8, di: i32, x: i32) {
                write(d, di, [x as $t; $n])
            }
        );
        prim!(
            fn $shl(d: *mut u8, di: i32, a: *const u8, ai: i32, n: i32) {
                map1::<$t, $n>(d, di, a, ai, |x| x.wrapping_shl(n as u32))
            }
        );
        prim!(
            fn $shr(d: *mut u8, di: i32, a: *const u8, ai: i32, n: i32) {
                map1::<$t, $n>(d, di, a, ai, |x| x.wrapping_shr(n as u32))
            }
        );
        prim!(
            fn $eq(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                cmp::<$t, $n>(d, di, a, ai, b, bi, |x, y| x == y)
            }
        );
        prim!(
            fn $ne(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                cmp::<$t, $n>(d, di, a, ai, b, bi, |x, y| x != y)
            }
        );
        prim!(
            fn $lt(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                cmp::<$t, $n>(d, di, a, ai, b, bi, |x, y| x < y)
            }
        );
        prim!(
            fn $le(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                cmp::<$t, $n>(d, di, a, ai, b, bi, |x, y| x <= y)
            }
        );
        prim!(
            fn $gt(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                cmp::<$t, $n>(d, di, a, ai, b, bi, |x, y| x > y)
            }
        );
        prim!(
            fn $ge(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
                cmp::<$t, $n>(d, di, a, ai, b, bi, |x, y| x >= y)
            }
        );
        prim!(
            fn $sum(a: *const u8, ai: i32) -> i32 {
                fold::<$t, $n>(a, ai, |x, y| x.wrapping_add(y)) as i32
            }
        );
        prim!(
            fn $min_lane(a: *const u8, ai: i32) -> i32 {
                fold::<$t, $n>(a, ai, |x, y| if y < x { y } else { x }) as i32
            }
        );
        prim!(
            fn $max_lane(a: *const u8, ai: i32) -> i32 {
                fold::<$t, $n>(a, ai, |x, y| if y > x { y } else { x }) as i32
            }
        );
        prim!(
            fn $load(d: *mut u8, di: i32, arr: *const u8, index: i32) {
                load_arr::<$t, $n>(d, di, arr, index)
            }
        );
        prim!(
            fn $store(arr: *mut u8, index: i32, a: *const u8, ai: i32) {
                store_arr::<$t, $n>(arr, index, a, ai)
            }
        );
    };
}

macro_rules! int_signed {
    ($t:ty, $n:literal, $abs:ident, $neg:ident) => {
        prim!(
            fn $abs(d: *mut u8, di: i32, a: *const u8, ai: i32) {
                map1::<$t, $n>(d, di, a, ai, |x| x.wrapping_abs())
            }
        );
        prim!(
            fn $neg(d: *mut u8, di: i32, a: *const u8, ai: i32) {
                map1::<$t, $n>(d, di, a, ai, |x| x.wrapping_neg())
            }
        );
    };
}

int_common!(
    i32,
    4,
    i32x4_add,
    i32x4_sub,
    i32x4_mul,
    i32x4_min,
    i32x4_max,
    i32x4_splat,
    i32x4_shl,
    i32x4_shr,
    i32x4_eq,
    i32x4_ne,
    i32x4_lt,
    i32x4_le,
    i32x4_gt,
    i32x4_ge,
    i32x4_sum,
    i32x4_min_lane,
    i32x4_max_lane,
    i32x4_load_array,
    i32x4_store_array
);
int_signed!(i32, 4, i32x4_abs, i32x4_neg);
int_common!(
    i16,
    8,
    i16x8_add,
    i16x8_sub,
    i16x8_mul,
    i16x8_min,
    i16x8_max,
    i16x8_splat,
    i16x8_shl,
    i16x8_shr,
    i16x8_eq,
    i16x8_ne,
    i16x8_lt,
    i16x8_le,
    i16x8_gt,
    i16x8_ge,
    i16x8_sum,
    i16x8_min_lane,
    i16x8_max_lane,
    i16x8_load_array,
    i16x8_store_array
);
int_signed!(i16, 8, i16x8_abs, i16x8_neg);
int_common!(
    i8,
    16,
    i8x16_add,
    i8x16_sub,
    i8x16_mul,
    i8x16_min,
    i8x16_max,
    i8x16_splat,
    i8x16_shl,
    i8x16_shr,
    i8x16_eq,
    i8x16_ne,
    i8x16_lt,
    i8x16_le,
    i8x16_gt,
    i8x16_ge,
    i8x16_sum,
    i8x16_min_lane,
    i8x16_max_lane,
    i8x16_load_array,
    i8x16_store_array
);
int_signed!(i8, 16, i8x16_abs, i8x16_neg);
int_common!(
    u8,
    16,
    u8x16_add,
    u8x16_sub,
    u8x16_mul,
    u8x16_min,
    u8x16_max,
    u8x16_splat,
    u8x16_shl,
    u8x16_shr,
    u8x16_eq,
    u8x16_ne,
    u8x16_lt,
    u8x16_le,
    u8x16_gt,
    u8x16_ge,
    u8x16_sum,
    u8x16_min_lane,
    u8x16_max_lane,
    u8x16_load_array,
    u8x16_store_array
);

// Bit operations see the slot as two 64-bit halves; the lane type is
// immaterial.
prim!(
    fn v128_and(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
        map2::<u64, 2>(d, di, a, ai, b, bi, |x, y| x & y)
    }
);
prim!(
    fn v128_or(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
        map2::<u64, 2>(d, di, a, ai, b, bi, |x, y| x | y)
    }
);
prim!(
    fn v128_xor(d: *mut u8, di: i32, a: *const u8, ai: i32, b: *const u8, bi: i32) {
        map2::<u64, 2>(d, di, a, ai, b, bi, |x, y| x ^ y)
    }
);
prim!(
    fn v128_not(d: *mut u8, di: i32, a: *const u8, ai: i32) {
        map1::<u64, 2>(d, di, a, ai, |x| !x)
    }
);
// The 16 bytes as they are.
prim!(
    fn v128_copy(d: *mut u8, di: i32, a: *const u8, ai: i32) {
        map1::<u64, 2>(d, di, a, ai, |x| x)
    }
);
// `d = mask ? a : b`, bit by bit.
prim!(
    fn v128_select(
        d: *mut u8,
        di: i32,
        mask: *const u8,
        mi: i32,
        a: *const u8,
        ai: i32,
        b: *const u8,
        bi: i32,
    ) {
        map3::<u64, 2>(d, di, mask, mi, a, ai, b, bi, |m, x, y| (m & x) | (!m & y))
    }
);
lane!(u64, u64);

prim!(
    fn f32x4_to_i32x4(d: *mut u8, di: i32, a: *const u8, ai: i32) {
        let v: [f32; 4] = read(a, ai);
        write(d, di, v.map(|x| x as i32))
    }
);
prim!(
    fn i32x4_to_f32x4(d: *mut u8, di: i32, a: *const u8, ai: i32) {
        let v: [i32; 4] = read(a, ai);
        write(d, di, v.map(|x| x as f32))
    }
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn min_max_order_signed_zero_and_propagate_nan() {
        assert!(fmin(-0.0f32, 0.0).is_sign_negative());
        assert!(fmin(0.0f32, -0.0).is_sign_negative());
        assert!(fmax(-0.0f32, 0.0).is_sign_positive());
        assert!(fmax(0.0f32, -0.0).is_sign_positive());
        assert!(fmin(f32::NAN, 1.0).is_nan());
        assert!(fmax(1.0f64, f64::NAN).is_nan());
        assert_eq!(fmin(1.0f64, 2.0), 1.0);
        assert_eq!(fmax(1.0f64, 2.0), 2.0);
    }

    #[test]
    fn f32x4_min_keeps_negative_zero() {
        let a = [f32::NAN, f32::INFINITY, -0.0f32, 1.0];
        let b = [1.0f32, f32::NEG_INFINITY, 0.0, f32::NAN];
        let mut d = [0f32; 4];
        unsafe {
            f32x4_min(
                d.as_mut_ptr() as *mut u8,
                0,
                a.as_ptr() as *const u8,
                0,
                b.as_ptr() as *const u8,
                0,
            );
        }
        assert!(d[2].is_sign_negative(), "{d:?}");
    }
}
