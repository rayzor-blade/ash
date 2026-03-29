use crate::bytecode::DecodedBytecode;
use crate::hl;
use crate::hl_bindings::{hl_field_lookup, hl_obj_field, hl_obj_proto, hl_type_fun__bindgen_ty_2};
use crate::opcodes::Opcode;
use hlbc::types::{Function, Native};
use num_enum::{IntoPrimitive, TryFromPrimitive};
use std::ffi::c_void;
use std::mem;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;

pub type Str = flexstr::SharedStr;

#[derive(Debug, Default, Clone, Copy, IntoPrimitive, TryFromPrimitive, PartialEq)]
#[repr(u32)]
pub enum ValueTypeKind {
    HVOID = 0,
    HUI8 = 1,
    HUI16 = 2,
    HI32 = 3,
    HI64 = 4,
    HF32 = 5,
    HF64 = 6,
    HBOOL = 7,
    HBYTES = 8,
    HDYN = 9,
    HFUN = 10,
    HOBJ = 11,
    HARRAY = 12,
    HTYPE = 13,
    HREF = 14,
    HVIRTUAL = 15,
    HDYNOBJ = 16,
    HABSTRACT = 17,
    HENUM = 18,
    #[default]
    HNULL = 19,
    HMETHOD = 20,
    HSTRUCT = 21,
    HPACKED = 22,
    // ---------
    HLAST = 23,
}

/// Reference to a function or a native object
#[derive(Debug, Clone)]
pub enum FunPtr {
    Fun(Function),
    Native(Native),
}

// Array of argument counts for each opcode
pub const OP_NARGS: [i8; 102] = [
    2,  // OMov
    2,  // OInt
    2,  // OFloat
    2,  // OBool
    2,  // OBytes
    2,  // OString
    1,  // ONull
    3,  // OAdd
    3,  // OSub
    3,  // OMul
    3,  // OSDiv
    3,  // OUDiv
    3,  // OSMod
    3,  // OUMod
    3,  // OShl
    3,  // OSShr
    3,  // OUShr
    3,  // OAnd
    3,  // OOr
    3,  // OXor
    2,  // ONeg
    2,  // ONot
    1,  // OIncr
    1,  // ODecr
    2,  // OCall0
    3,  // OCall1
    4,  // OCall2
    5,  // OCall3
    6,  // OCall4
    -1, // OCallN
    -1, // OCallMethod
    -1, // OCallThis
    -1, // OCallClosure
    2,  // OStaticClosure
    3,  // OInstanceClosure
    3,  // OVirtualClosure
    2,  // OGetGlobal
    2,  // OSetGlobal
    3,  // OField
    3,  // OSetField
    2,  // OGetThis
    2,  // OSetThis
    3,  // ODynGet
    3,  // ODynSet
    2,  // OJTrue
    2,  // OJFalse
    2,  // OJNull
    2,  // OJNotNull
    3,  // OJSLt
    3,  // OJSGte
    3,  // OJSGt
    3,  // OJSLte
    3,  // OJULt
    3,  // OJUGte
    3,  // OJNotLt
    3,  // OJNotGte
    3,  // OJEq
    3,  // OJNotEq
    1,  // OJAlways
    2,  // OToDyn
    2,  // OToSFloat
    2,  // OToUFloat
    2,  // OToInt
    2,  // OSafeCast
    2,  // OUnsafeCast
    2,  // OToVirtual
    0,  // OLabel
    1,  // ORet
    1,  // OThrow
    1,  // ORethrow
    -1, // OSwitch
    1,  // ONullCheck
    2,  // OTrap
    1,  // OEndTrap
    3,  // OGetI8
    3,  // OGetI16
    3,  // OGetMem
    3,  // OGetArray
    3,  // OSetI8
    3,  // OSetI16
    3,  // OSetMem
    3,  // OSetArray
    1,  // ONew
    2,  // OArraySize
    2,  // OType
    2,  // OGetType
    2,  // OGetTID
    2,  // ORef
    2,  // OUnref
    2,  // OSetref
    -1, // OMakeEnum
    2,  // OEnumAlloc
    2,  // OEnumIndex
    4,  // OEnumField
    3,  // OSetEnumField
    0,  // OAssert
    2,  // ORefData
    3,  // ORefOffset
    0,  // ONop
    3,  // OPrefetch
    3,  // OAsm
    0,  // OLast
];

#[derive(Debug, Clone)]
pub struct TypeRef(pub usize);
impl Default for TypeRef {
    fn default() -> Self {
        Self(Default::default())
    }
}

#[derive(Debug, Clone)]
pub struct HLType {
    pub kind: hl::hl_type_kind,
    // Additional fields based on the kind
    pub abs_name: Option<String>,
    pub obj: Option<HLTypeObj>,
    pub fun: Option<HLTypeFun>,
    pub tenum: Option<HLTypeEnum>,
    pub virt: Option<HLTypeVirtual>,
    pub tparam: Option<TypeRef>,
}

impl Default for HLType {
    fn default() -> Self {
        Self {
            kind: hl::hl_type_kind_HLAST,
            obj: Default::default(),
            fun: Default::default(),
            tenum: Default::default(),
            virt: Default::default(),
            tparam: Default::default(),
            abs_name: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct HLTypeObj {
    pub name: String,
    pub super_: Option<TypeRef>,
    pub fields: Vec<HLObjField>,
    pub proto: Vec<HLObjProto>,
    pub bindings: Vec<i32>,
    pub global_value: u32,
}

#[derive(Debug, Clone, Default)]
pub struct HLTypeFun {
    pub args: Vec<TypeRef>,
    pub ret: TypeRef,
    pub parent: Option<TypeRef>,
    pub closure_type: Option<TypeRef>,
    pub closure: Option<Box<HLTypeFun>>,
}

#[derive(Debug, Clone, Default)]
pub struct HLTypeEnum {
    pub name: String,
    pub constructs: Vec<HLEnumConstruct>,
    pub global_value: u32,
}

#[derive(Debug, Clone, Default)]
pub struct HLObjField {
    pub name: String,
    pub type_: TypeRef,
    pub hashed_name: i32,
}

#[derive(Debug, Clone)]
pub struct HLObjProto {
    pub name: String,
    pub findex: i32,
    pub pindex: i32,
    pub hashed_name: i32,
}

#[derive(Debug, Clone, Default)]
pub struct HLEnumConstruct {
    pub name: String,
    pub params: Vec<TypeRef>,
    pub hasptr: bool,
    pub size: i32,
    pub offsets: Vec<i32>,
}

#[derive(Debug, Default, Clone)]
pub struct HLNative {
    pub lib: String,
    pub name: String,
    pub type_: TypeRef,
    pub findex: i32,
}

#[derive(Debug, Default, Clone)]
pub struct HLFunction {
    pub type_: TypeRef,
    pub findex: i32,
    pub ops: Vec<Opcode>,
    pub regs: Vec<TypeRef>,
    pub debug: Vec<i32>,
    pub ref_: i32,
    pub obj: Option<HLTypeObj>,
    pub field_name: Option<String>,
    pub field_ref: Option<Box<HLFunction>>,
}
impl HLFunction {
    pub fn name(&self) -> String {
        self.field_name
            .clone()
            .unwrap_or(format!("Fun_{}", self.findex))
    }

    /// Compute a stable CRC32 hash of this function's semantics.
    ///
    /// Incorporates the opcode stream, register types, and function type signature.
    /// Debug info is excluded so recompilations that only change line numbers
    /// don't trigger a reload.
    pub fn compute_hash(&self) -> u32 {
        use crate::bytecode::{H, H32};
        let mut h: u32 = 0;

        // Hash function type signature
        h = H32(h, self.type_.0 as u32);

        // Hash register types
        for reg in &self.regs {
            h = H32(h, reg.0 as u32);
        }

        // Hash opcode stream (discriminant + numeric fields)
        for op in &self.ops {
            h = hash_opcode(h, op);
        }

        h
    }
}

/// Hash a single opcode into a running CRC32 accumulator.
fn hash_opcode(mut h: u32, op: &Opcode) -> u32 {
    use crate::bytecode::{H, H32};

    // Hash the discriminant index as a tag byte
    h = H(h, std::mem::discriminant(op).__reprx());

    // Hash all numeric fields. The ordering must be deterministic.
    macro_rules! hr { ($r:expr) => { h = H32(h, $r.0 as u32); }; }
    macro_rules! hi { ($i:expr) => { h = H32(h, *$i as u32); }; }
    macro_rules! hargs { ($args:expr) => { for a in $args { hr!(a); } }; }

    match op {
        Opcode::Mov { dst, src } => { hr!(dst); hr!(src); }
        Opcode::Int { dst, ptr } => { hr!(dst); hi!(&ptr.0); }
        Opcode::Float { dst, ptr } => { hr!(dst); hi!(&ptr.0); }
        Opcode::Bool { dst, value } => { hr!(dst); h = H(h, *value as u8); }
        Opcode::Bytes { dst, ptr } => { hr!(dst); hi!(&ptr.0); }
        Opcode::String { dst, ptr } => { hr!(dst); hi!(&ptr.0); }
        Opcode::Null { dst } => { hr!(dst); }

        Opcode::Add { dst, a, b }
        | Opcode::Sub { dst, a, b }
        | Opcode::Mul { dst, a, b }
        | Opcode::SDiv { dst, a, b }
        | Opcode::UDiv { dst, a, b }
        | Opcode::SMod { dst, a, b }
        | Opcode::UMod { dst, a, b }
        | Opcode::Shl { dst, a, b }
        | Opcode::SShr { dst, a, b }
        | Opcode::UShr { dst, a, b }
        | Opcode::And { dst, a, b }
        | Opcode::Or { dst, a, b }
        | Opcode::Xor { dst, a, b } => { hr!(dst); hr!(a); hr!(b); }

        Opcode::Neg { dst, src } | Opcode::Not { dst, src } => { hr!(dst); hr!(src); }
        Opcode::Incr { dst } | Opcode::Decr { dst } => { hr!(dst); }

        Opcode::Call0 { dst, fun } => { hr!(dst); hi!(&fun.0); }
        Opcode::Call1 { dst, fun, arg0 } => { hr!(dst); hi!(&fun.0); hr!(arg0); }
        Opcode::Call2 { dst, fun, arg0, arg1 } => { hr!(dst); hi!(&fun.0); hr!(arg0); hr!(arg1); }
        Opcode::Call3 { dst, fun, arg0, arg1, arg2 } => { hr!(dst); hi!(&fun.0); hr!(arg0); hr!(arg1); hr!(arg2); }
        Opcode::Call4 { dst, fun, arg0, arg1, arg2, arg3 } => { hr!(dst); hi!(&fun.0); hr!(arg0); hr!(arg1); hr!(arg2); hr!(arg3); }
        Opcode::CallN { dst, fun, args } => { hr!(dst); hi!(&fun.0); hargs!(args); }
        Opcode::CallMethod { dst, field, args } => { hr!(dst); hi!(&field.0); hargs!(args); }
        Opcode::CallThis { dst, field, args } => { hr!(dst); hi!(&field.0); hargs!(args); }
        Opcode::CallClosure { dst, fun, args } => { hr!(dst); hr!(fun); hargs!(args); }
        Opcode::IndirectCall { dst, fun, args } => { hr!(dst); hi!(&fun.0); hargs!(args); }

        Opcode::StaticClosure { dst, fun } => { hr!(dst); hi!(&fun.0); }
        Opcode::InstanceClosure { dst, fun, obj } => { hr!(dst); hi!(&fun.0); hr!(obj); }
        Opcode::VirtualClosure { dst, obj, field } => { hr!(dst); hr!(obj); hr!(field); }

        Opcode::GetGlobal { dst, global } => { hr!(dst); hi!(&global.0); }
        Opcode::SetGlobal { global, src } => { hi!(&global.0); hr!(src); }

        Opcode::Field { dst, obj, field } => { hr!(dst); hr!(obj); hi!(&field.0); }
        Opcode::SetField { obj, field, src } => { hr!(obj); hi!(&field.0); hr!(src); }
        Opcode::GetThis { dst, field } => { hr!(dst); hi!(&field.0); }
        Opcode::SetThis { field, src } => { hi!(&field.0); hr!(src); }

        Opcode::DynGet { dst, obj, field } => { hr!(dst); hr!(obj); hi!(&field.0); }
        Opcode::DynSet { obj, field, src } => { hr!(obj); hi!(&field.0); hr!(src); }

        Opcode::JTrue { cond, offset }
        | Opcode::JFalse { cond, offset } => { hr!(cond); hi!(offset); }
        Opcode::JNull { reg, offset }
        | Opcode::JNotNull { reg, offset } => { hr!(reg); hi!(offset); }
        Opcode::JSLt { a, b, offset }
        | Opcode::JSGte { a, b, offset }
        | Opcode::JSGt { a, b, offset }
        | Opcode::JSLte { a, b, offset }
        | Opcode::JULt { a, b, offset }
        | Opcode::JUGte { a, b, offset }
        | Opcode::JNotLt { a, b, offset }
        | Opcode::JNotGte { a, b, offset }
        | Opcode::JEq { a, b, offset }
        | Opcode::JNotEq { a, b, offset } => { hr!(a); hr!(b); hi!(offset); }
        Opcode::JAlways { offset } => { hi!(offset); }

        Opcode::ToDyn { dst, src }
        | Opcode::ToSFloat { dst, src }
        | Opcode::ToUFloat { dst, src }
        | Opcode::ToInt { dst, src }
        | Opcode::SafeCast { dst, src }
        | Opcode::UnsafeCast { dst, src }
        | Opcode::ToVirtual { dst, src } => { hr!(dst); hr!(src); }

        Opcode::Ret { ret } => { hr!(ret); }
        Opcode::Throw { exc } | Opcode::Rethrow { exc } => { hr!(exc); }
        Opcode::Switch { reg, offsets, end } => { hr!(reg); for o in offsets { hi!(o); } hi!(end); }
        Opcode::NullCheck { reg } => { hr!(reg); }
        Opcode::Trap { exc, offset } => { hr!(exc); hi!(offset); }
        Opcode::EndTrap { exc } => { hr!(exc); }

        Opcode::GetI8 { dst, bytes, index }
        | Opcode::GetI16 { dst, bytes, index }
        | Opcode::GetMem { dst, bytes, index } => { hr!(dst); hr!(bytes); hr!(index); }
        Opcode::SetI8 { bytes, index, src }
        | Opcode::SetI16 { bytes, index, src }
        | Opcode::SetMem { bytes, index, src } => { hr!(bytes); hr!(index); hr!(src); }
        Opcode::GetArray { dst, array, index } => { hr!(dst); hr!(array); hr!(index); }
        Opcode::SetArray { array, index, src } => { hr!(array); hr!(index); hr!(src); }
        Opcode::ArraySize { dst, array } => { hr!(dst); hr!(array); }

        Opcode::New { dst } => { hr!(dst); }
        Opcode::Type { dst, ty } => { hr!(dst); hi!(&ty.0); }
        Opcode::GetType { dst, src } | Opcode::GetTID { dst, src } => { hr!(dst); hr!(src); }

        Opcode::Ref { dst, src } | Opcode::Unref { dst, src } => { hr!(dst); hr!(src); }
        Opcode::Setref { dst, value } => { hr!(dst); hr!(value); }

        Opcode::MakeEnum { dst, construct, args } => { hr!(dst); hi!(&construct.0); hargs!(args); }
        Opcode::EnumAlloc { dst, construct } => { hr!(dst); hi!(&construct.0); }
        Opcode::EnumIndex { dst, value } => { hr!(dst); hr!(value); }
        Opcode::EnumField { dst, value, construct, field } => { hr!(dst); hr!(value); hi!(&construct.0); hi!(&field.0); }
        Opcode::SetEnumField { value, field, src } => { hr!(value); hi!(&field.0); hr!(src); }

        Opcode::Assert => {}
        Opcode::RefData { dst, src } => { hr!(dst); hr!(src); }
        Opcode::RefOffset { dst, reg, offset } => { hr!(dst); hr!(reg); hr!(offset); }
        Opcode::Nop => {}
        Opcode::Label => {}
        Opcode::Prefetch { value, field, mode } => { hr!(value); hi!(&field.0); hi!(mode); }
        Opcode::Asm { mode, value, reg } => { hi!(mode); hi!(value); hr!(reg); }
    }
    h
}

/// Helper trait to extract discriminant as a u8 for hashing.
trait DiscriminantRepr {
    fn __reprx(&self) -> u8;
}
impl<T> DiscriminantRepr for std::mem::Discriminant<T> {
    fn __reprx(&self) -> u8 {
        // Discriminant doesn't expose its value, so hash its Debug repr
        use std::hash::{Hash, Hasher};
        struct U8Hasher(u64);
        impl Hasher for U8Hasher {
            fn finish(&self) -> u64 { self.0 }
            fn write(&mut self, bytes: &[u8]) {
                for &b in bytes { self.0 = self.0.wrapping_mul(31).wrapping_add(b as u64); }
            }
        }
        let mut hasher = U8Hasher(0);
        self.hash(&mut hasher);
        hasher.finish() as u8
    }
}

#[derive(Debug)]
pub struct HLVDynamic {
    pub type_: TypeRef,
    pub bool: Option<bool>,
    pub u8: Option<u8>,
    pub u16: Option<u16>,
    pub int: Option<i32>,
    pub int64: Option<i64>,
    pub float: Option<f32>,
    pub double: Option<f64>,
    pub bytes: Option<Vec<u8>>,
    pub ptr: Option<Box<dyn std::any::Any>>,
}

#[derive(Debug, Clone, Default)]
pub struct HLTypeVirtual {
    pub fields: Vec<HLObjField>,
    pub data_size: usize,
    pub indexes: Vec<i32>,
    pub lookup: HLFieldLookup,
}

#[derive(Debug, Clone, Default)]
pub struct HLFieldLookup {
    pub t: TypeRef,
    pub hashed_name: i32,
    pub field_index: usize,
}

#[derive(Debug, Clone, Default)]
pub struct HLConstant {
    pub global: u32,
    pub fields: Vec<i32>,
}
