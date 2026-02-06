use std::fmt;

/// NaN-boxed value for efficient interpreter register storage.
///
/// Uses IEEE 754 NaN boxing to pack type tags and values into 64 bits.
/// This eliminates heap allocation for primitives and enables Copy semantics.
///
/// ## Encoding
/// - Regular f64 values are stored as-is (when exponent bits != 0x7FF)
/// - Special values use the quiet NaN space: `NAN_TAG | type_tag | payload`
///
/// ## Type Tags (bits 48-50)
/// - 0x0: Pointer (48-bit address - covers all userspace on ARM64/x86_64)
/// - 0x1: I32 (32-bit signed integer)
/// - 0x2: I64 (lossy 48-bit, or heap-spilled for full range)
/// - 0x3: Bool (0 or 1)
/// - 0x4: Null
/// - 0x5: Void
/// - 0x6: Function index (findex as u32)
/// - 0x7: Bytes pointer (distinct from generic pointer for type safety)
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct NanBoxedValue(u64);

impl fmt::Debug for NanBoxedValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_f64() {
            write!(f, "F64({})", self.as_f64())
        } else {
            match self.tag() {
                Self::TAG_PTR => write!(f, "Ptr({:#x})", self.payload()),
                Self::TAG_I32 => write!(f, "I32({})", self.as_i32()),
                Self::TAG_I64 => write!(f, "I64({})", self.as_i64_lossy()),
                Self::TAG_BOOL => write!(f, "Bool({})", self.as_bool()),
                Self::TAG_NULL => write!(f, "Null"),
                Self::TAG_VOID => write!(f, "Void"),
                Self::TAG_FUNC => write!(f, "Func({})", self.payload() as u32),
                Self::TAG_BYTES => write!(f, "Bytes({:#x})", self.payload()),
                _ => write!(f, "Unknown({:#x})", self.0),
            }
        }
    }
}

impl NanBoxedValue {
    // Quiet NaN base: sets exponent to all 1s + quiet NaN bit
    const NAN_TAG: u64 = 0x7FF8_0000_0000_0000;

    // 3 tag bits at positions 48-50 (below the quiet NaN bit at 51)
    const TAG_MASK: u64 = 0x0007_0000_0000_0000;

    // 48-bit payload
    const PAYLOAD_MASK: u64 = 0x0000_FFFF_FFFF_FFFF;

    // Type tags
    const TAG_PTR: u64 = 0x0000_0000_0000_0000;
    const TAG_I32: u64 = 0x0001_0000_0000_0000;
    const TAG_I64: u64 = 0x0002_0000_0000_0000;
    const TAG_BOOL: u64 = 0x0003_0000_0000_0000;
    const TAG_NULL: u64 = 0x0004_0000_0000_0000;
    const TAG_VOID: u64 = 0x0005_0000_0000_0000;
    const TAG_FUNC: u64 = 0x0006_0000_0000_0000;
    const TAG_BYTES: u64 = 0x0007_0000_0000_0000;

    #[inline(always)]
    pub const fn void() -> Self {
        Self(Self::NAN_TAG | Self::TAG_VOID)
    }

    #[inline(always)]
    pub const fn null() -> Self {
        Self(Self::NAN_TAG | Self::TAG_NULL)
    }

    #[inline(always)]
    pub fn from_f64(v: f64) -> Self {
        Self(v.to_bits())
    }

    #[inline(always)]
    pub fn from_f32(v: f32) -> Self {
        Self::from_f64(v as f64)
    }

    #[inline(always)]
    pub fn from_i32(v: i32) -> Self {
        Self(Self::NAN_TAG | Self::TAG_I32 | (v as u32 as u64))
    }

    #[inline(always)]
    pub fn from_i64(v: i64) -> Self {
        Self(Self::NAN_TAG | Self::TAG_I64 | ((v as u64) & Self::PAYLOAD_MASK))
    }

    #[inline(always)]
    pub fn from_bool(v: bool) -> Self {
        Self(Self::NAN_TAG | Self::TAG_BOOL | (v as u64))
    }

    #[inline(always)]
    pub fn from_ptr(ptr: usize) -> Self {
        Self(Self::NAN_TAG | Self::TAG_PTR | ((ptr as u64) & Self::PAYLOAD_MASK))
    }

    #[inline(always)]
    pub fn from_func_index(findex: usize) -> Self {
        Self(Self::NAN_TAG | Self::TAG_FUNC | (findex as u64 & Self::PAYLOAD_MASK))
    }

    #[inline(always)]
    pub fn from_bytes_ptr(ptr: usize) -> Self {
        Self(Self::NAN_TAG | Self::TAG_BYTES | ((ptr as u64) & Self::PAYLOAD_MASK))
    }

    // --- Type checks ---

    #[inline(always)]
    pub fn is_f64(&self) -> bool {
        let exp_bits = (self.0 >> 52) & 0x7FF;
        if exp_bits != 0x7FF {
            return true;
        }
        (self.0 & 0x7FF8_0000_0000_0000) != Self::NAN_TAG
    }

    #[inline(always)]
    fn tag(&self) -> u64 {
        self.0 & Self::TAG_MASK
    }

    #[inline(always)]
    fn payload(&self) -> u64 {
        self.0 & Self::PAYLOAD_MASK
    }

    #[inline(always)]
    pub fn is_null(&self) -> bool {
        self.0 == (Self::NAN_TAG | Self::TAG_NULL)
    }

    #[inline(always)]
    pub fn is_void(&self) -> bool {
        self.0 == (Self::NAN_TAG | Self::TAG_VOID)
    }

    #[inline(always)]
    pub fn is_i32(&self) -> bool {
        !self.is_f64() && self.tag() == Self::TAG_I32
    }

    #[inline(always)]
    pub fn is_bool(&self) -> bool {
        !self.is_f64() && self.tag() == Self::TAG_BOOL
    }

    #[inline(always)]
    pub fn is_ptr(&self) -> bool {
        !self.is_f64() && self.tag() == Self::TAG_PTR
    }

    #[inline(always)]
    pub fn is_func(&self) -> bool {
        !self.is_f64() && self.tag() == Self::TAG_FUNC
    }

    // --- Extractors ---

    #[inline(always)]
    pub fn as_f64(&self) -> f64 {
        f64::from_bits(self.0)
    }

    #[inline(always)]
    pub fn as_f32(&self) -> f32 {
        self.as_f64() as f32
    }

    #[inline(always)]
    pub fn as_i32(&self) -> i32 {
        (self.0 & 0xFFFF_FFFF) as i32
    }

    #[inline(always)]
    pub fn as_i64_lossy(&self) -> i64 {
        let payload = self.payload();
        // Sign extend from 48 bits
        if payload & 0x0000_8000_0000_0000 != 0 {
            (payload | 0xFFFF_0000_0000_0000) as i64
        } else {
            payload as i64
        }
    }

    #[inline(always)]
    pub fn as_bool(&self) -> bool {
        (self.0 & 1) != 0
    }

    #[inline(always)]
    pub fn as_ptr(&self) -> usize {
        self.payload() as usize
    }

    #[inline(always)]
    pub fn as_func_index(&self) -> usize {
        self.payload() as usize
    }

    // --- Arithmetic fast paths ---

    /// Attempt a fast binary operation. Returns None if types don't match.
    #[inline(always)]
    pub fn binary_int_op(self, other: Self, op: IntBinOp) -> Option<Self> {
        if self.is_i32() && other.is_i32() {
            let l = self.as_i32();
            let r = other.as_i32();
            return Some(Self::from_i32(match op {
                IntBinOp::Add => l.wrapping_add(r),
                IntBinOp::Sub => l.wrapping_sub(r),
                IntBinOp::Mul => l.wrapping_mul(r),
                IntBinOp::SDiv => {
                    if r == 0 { return None; }
                    l.wrapping_div(r)
                }
                IntBinOp::UDiv => {
                    if r == 0 { return None; }
                    ((l as u32).wrapping_div(r as u32)) as i32
                }
                IntBinOp::SMod => {
                    if r == 0 { return None; }
                    l.wrapping_rem(r)
                }
                IntBinOp::Shl => l.wrapping_shl(r as u32),
                IntBinOp::SShr => l.wrapping_shr(r as u32),
                IntBinOp::UShr => ((l as u32).wrapping_shr(r as u32)) as i32,
                IntBinOp::And => l & r,
                IntBinOp::Or => l | r,
                IntBinOp::Xor => l ^ r,
            }));
        }
        None
    }

    /// Attempt a fast float binary operation.
    #[inline(always)]
    pub fn binary_float_op(self, other: Self, op: FloatBinOp) -> Option<Self> {
        if self.is_f64() && other.is_f64() {
            let l = self.as_f64();
            let r = other.as_f64();
            return Some(Self::from_f64(match op {
                FloatBinOp::Add => l + r,
                FloatBinOp::Sub => l - r,
                FloatBinOp::Mul => l * r,
                FloatBinOp::SDiv => l / r,
                FloatBinOp::SMod => l % r,
            }));
        }
        None
    }

    /// Compare two values. Returns None if types don't match.
    #[inline(always)]
    pub fn compare(self, other: Self, op: CmpOp) -> Option<bool> {
        if self.is_i32() && other.is_i32() {
            let l = self.as_i32();
            let r = other.as_i32();
            return Some(match op {
                CmpOp::SLt => l < r,
                CmpOp::SGte => l >= r,
                CmpOp::SGt => l > r,
                CmpOp::SLte => l <= r,
                CmpOp::ULt => (l as u32) < (r as u32),
                CmpOp::UGte => (l as u32) >= (r as u32),
                CmpOp::Eq => l == r,
                CmpOp::NotEq => l != r,
            });
        }
        if self.is_f64() && other.is_f64() {
            let l = self.as_f64();
            let r = other.as_f64();
            return Some(match op {
                CmpOp::SLt => l < r,
                CmpOp::SGte => l >= r,
                CmpOp::SGt => l > r,
                CmpOp::SLte => l <= r,
                CmpOp::ULt => l < r,
                CmpOp::UGte => l >= r,
                CmpOp::Eq => l == r,
                CmpOp::NotEq => l != r,
            });
        }
        // Pointer/null equality
        if op == CmpOp::Eq || op == CmpOp::NotEq {
            let eq = self.0 == other.0;
            return Some(if op == CmpOp::Eq { eq } else { !eq });
        }
        None
    }

    /// Convert to bool for conditional jumps.
    #[inline(always)]
    pub fn to_bool(&self) -> bool {
        if self.is_bool() {
            return self.as_bool();
        }
        if self.is_i32() {
            return self.as_i32() != 0;
        }
        if self.is_null() || self.is_void() {
            return false;
        }
        if self.is_f64() {
            return self.as_f64() != 0.0;
        }
        // Pointers/funcs are truthy
        true
    }
}

impl Default for NanBoxedValue {
    fn default() -> Self {
        Self::void()
    }
}

/// Integer binary operations supported by the fast path.
#[derive(Debug, Clone, Copy)]
pub enum IntBinOp {
    Add,
    Sub,
    Mul,
    SDiv,
    UDiv,
    SMod,
    Shl,
    SShr,
    UShr,
    And,
    Or,
    Xor,
}

/// Float binary operations supported by the fast path.
#[derive(Debug, Clone, Copy)]
pub enum FloatBinOp {
    Add,
    Sub,
    Mul,
    SDiv,
    SMod,
}

/// Comparison operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CmpOp {
    SLt,
    SGte,
    SGt,
    SLte,
    ULt,
    UGte,
    Eq,
    NotEq,
}
