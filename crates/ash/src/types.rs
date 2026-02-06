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
    pub fn name(&self)-> String {
        self.field_name.clone().unwrap_or(format!("Fun_{}", self.findex))
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


