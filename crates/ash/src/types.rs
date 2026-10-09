use crate::bytecode::DecodedBytecode;
use crate::hl;
use crate::hl_bindings::{hl_field_lookup, hl_obj_field, hl_obj_proto, hl_type_fun__bindgen_ty_2};
use crate::opcodes::Opcode;
use num_enum::{IntoPrimitive, TryFromPrimitive};
use std::ffi::c_void;
use std::mem;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;

/// Whether a value of this kind is its own Dynamic: a pointer to memory whose
/// first word is its `hl_type`. Every other kind is boxed by `hl_make_dyn` on
/// the way into a Dynamic -- primitives, and pointers without that header
/// such as bytes and abstracts. The runtime's `hl_is_dynamic` table.
pub fn kind_is_dynamic(kind: hl::hl_type_kind) -> bool {
    matches!(
        kind,
        hl::hl_type_kind_HDYN
            | hl::hl_type_kind_HFUN
            | hl::hl_type_kind_HOBJ
            | hl::hl_type_kind_HARRAY
            | hl::hl_type_kind_HVIRTUAL
            | hl::hl_type_kind_HDYNOBJ
            | hl::hl_type_kind_HENUM
            | hl::hl_type_kind_HNULL
    )
}

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

/// Ops in a chunk of a body decoded on its own; see [`HLFunction::op_at`].
pub const CHUNK_OPS: usize = 256;

/// A function's ops and their `(file, line)` debug pairs. The decoder may
/// leave them encoded, as the body's own bytes; the ops and the debug pairs
/// are each decoded the first time they are read, once, and the bytes are
/// dropped when both have been. A large body can also be decoded a chunk at
/// a time, for a caller that reads only the chunks it reaches.
#[derive(Debug, Default)]
pub struct FunctionBody {
    ops: std::sync::OnceLock<Vec<Opcode>>,
    debug: std::sync::OnceLock<Vec<i32>>,
    chunks: std::sync::OnceLock<Box<[std::sync::OnceLock<Box<[Opcode]>>]>>,
    encoded: std::sync::Mutex<Option<EncodedBody>>,
    /// Recorded by the decoder; see [`HLFunction::shape`].
    shape: Option<BodyShape>,
}

impl Clone for FunctionBody {
    /// A clone decodes into its own slots, so a copy of an encoded body
    /// never decodes the original's.
    fn clone(&self) -> Self {
        FunctionBody {
            ops: self.ops.clone(),
            debug: self.debug.clone(),
            chunks: std::sync::OnceLock::new(),
            encoded: std::sync::Mutex::new(self.lock_encoded().clone()),
            shape: self.shape,
        }
    }
}

/// What deciding how to run a body asks of its ops, recorded when the
/// decoder first reads them so that asking does not decode them again.
#[derive(Debug, Clone, Copy)]
pub struct BodyShape {
    /// A jump or switch target lies backward: the body can loop.
    pub back_edge: bool,
    /// A multiply writes a float register: the optimizer could fuse it.
    pub float_mul: bool,
    /// A `Prefetch` or an `Asm`, which the JIT does not compile.
    pub jit_unsupported: bool,
}

impl BodyShape {
    pub fn of(f: &HLFunction, types: &[HLType]) -> Self {
        BodyShape {
            back_edge: ops_have_back_edge(f.ops()),
            float_mul: ops_have_float_mul(f.ops(), &f.regs, types),
            jit_unsupported: f
                .ops()
                .iter()
                .any(|op| matches!(op, Opcode::Prefetch { .. } | Opcode::Asm { .. })),
        }
    }
}

/// Whether a jump or a switch target in `ops` lies backward.
pub fn ops_have_back_edge(ops: &[Opcode]) -> bool {
    ops.iter().any(|op| match op {
        Opcode::Switch { offsets, end, .. } => *end < 0 || offsets.iter().any(|o| *o < 0),
        _ => air::opcode_info::jump_offset(op).is_some_and(|o| o < 0),
    })
}

/// Whether a multiply in `ops` writes a float register.
pub fn ops_have_float_mul(ops: &[Opcode], regs: &[TypeRef], types: &[HLType]) -> bool {
    ops.iter().any(|op| match op {
        Opcode::Mul { dst, .. } => regs
            .get(dst.0 as usize)
            .and_then(|t| types.get(t.0))
            .is_some_and(|t| t.kind == hl::hl_type_kind_HF64 || t.kind == hl::hl_type_kind_HF32),
        _ => false,
    })
}

/// An undecoded body's bytes, and what decoding them needs.
#[derive(Clone)]
pub struct EncodedBody {
    pub(crate) bytes: Box<[u8]>,
    pub(crate) nops: usize,
    pub(crate) has_debug: bool,
    pub(crate) ndebug_files: usize,
    /// Where the debug pairs start in `bytes`.
    pub(crate) debug_at: usize,
    /// Where each [`CHUNK_OPS`] chunk of ops starts in `bytes`, for a body
    /// large enough to be decoded by chunk; empty otherwise.
    pub(crate) chunk_at: Box<[u32]>,
}

impl std::fmt::Debug for EncodedBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "EncodedBody {{ bytes: {}, nops: {} }}", self.bytes.len(), self.nops)
    }
}

impl FunctionBody {
    pub(crate) fn decoded(ops: Vec<Opcode>, debug: Vec<i32>) -> Self {
        FunctionBody {
            ops: std::sync::OnceLock::from(ops),
            debug: std::sync::OnceLock::from(debug),
            ..FunctionBody::default()
        }
    }

    pub(crate) fn encoded(body: EncodedBody, shape: BodyShape) -> Self {
        FunctionBody {
            encoded: std::sync::Mutex::new(Some(body)),
            shape: Some(shape),
            ..FunctionBody::default()
        }
    }

    fn lock_encoded(&self) -> std::sync::MutexGuard<'_, Option<EncodedBody>> {
        self.encoded.lock().unwrap_or_else(|e| e.into_inner())
    }

    /// Drop the bytes once nothing is left to decode from them.
    fn release_bytes(&self) {
        if self.ops.get().is_some() && self.debug.get().is_some() {
            *self.lock_encoded() = None;
        }
    }

    fn ops(&self) -> &Vec<Opcode> {
        if let Some(ops) = self.ops.get() {
            return ops;
        }
        let ops = self.ops.get_or_init(|| {
            self.lock_encoded()
                .as_ref()
                .map(|e| crate::bytecode::decode_ops(e, 0, e.nops))
                .unwrap_or_default()
        });
        self.release_bytes();
        ops
    }

    fn debug(&self) -> &Vec<i32> {
        if let Some(debug) = self.debug.get() {
            return debug;
        }
        let debug = self.debug.get_or_init(|| {
            self.lock_encoded()
                .as_ref()
                .map(crate::bytecode::decode_debug)
                .unwrap_or_default()
        });
        self.release_bytes();
        debug
    }

    /// Op `pc`, decoding only its chunk when the body is decoded by chunk.
    fn op_at(&self, pc: usize) -> Option<&Opcode> {
        if let Some(ops) = self.ops.get() {
            return ops.get(pc);
        }
        let chunks = self.chunks.get_or_init(|| {
            let n = self.lock_encoded().as_ref().map_or(0, |e| e.chunk_at.len());
            (0..n).map(|_| std::sync::OnceLock::new()).collect()
        });
        let Some(chunk) = chunks.get(pc / CHUNK_OPS) else {
            return self.ops().get(pc);
        };
        if let Some(ops) = chunk.get() {
            return ops.get(pc % CHUNK_OPS);
        }
        let decoded = self
            .lock_encoded()
            .as_ref()
            .map(|e| crate::bytecode::decode_chunk(e, pc / CHUNK_OPS));
        match decoded {
            Some(ops) => chunk.get_or_init(|| ops).get(pc % CHUNK_OPS),
            // The bytes went because the whole body was decoded meanwhile.
            None => self.ops().get(pc),
        }
    }

    fn get_mut(&mut self) -> (&mut Vec<Opcode>, &mut Vec<i32>) {
        self.ops();
        self.debug();
        (
            self.ops.get_mut().expect("decoded just above"),
            self.debug.get_mut().expect("decoded just above"),
        )
    }

    /// Whether the ops have been decoded whole, or never were encoded.
    pub fn is_decoded(&self) -> bool {
        self.ops.get().is_some() || self.lock_encoded().is_none()
    }

    /// Whether the ops are still encoded and can be decoded by chunk.
    pub fn is_chunked(&self) -> bool {
        self.ops.get().is_none()
            && self.lock_encoded().as_ref().is_some_and(|e| !e.chunk_at.is_empty())
    }

    /// The chunks decoded so far, for a body decoded by chunk.
    pub fn decoded_chunks(&self) -> usize {
        self.chunks
            .get()
            .map_or(0, |c| c.iter().filter(|c| c.get().is_some()).count())
    }

    /// How many ops a body still encoded holds; `None` once decoded.
    pub fn encoded_ops(&self) -> Option<usize> {
        if self.ops.get().is_some() {
            return None;
        }
        self.lock_encoded().as_ref().map(|e| e.nops)
    }
}

#[derive(Debug, Default, Clone)]
pub struct HLFunction {
    pub type_: TypeRef,
    pub findex: i32,
    /// The ops and debug pairs, reached through [`HLFunction::ops`] and
    /// [`HLFunction::debug`].
    body: FunctionBody,
    pub regs: Vec<TypeRef>,
    pub ref_: i32,
    pub obj: Option<HLTypeObj>,
    pub field_name: Option<String>,
    pub field_ref: Option<Box<HLFunction>>,
}
impl HLFunction {
    /// A function with this body and every other field at its default.
    pub fn with_body(ops: Vec<Opcode>, debug: Vec<i32>) -> Self {
        HLFunction {
            body: FunctionBody::decoded(ops, debug),
            ..HLFunction::default()
        }
    }

    pub(crate) fn set_body(&mut self, body: FunctionBody) {
        self.body = body;
    }

    pub fn body(&self) -> &FunctionBody {
        &self.body
    }

    /// The body's [`BodyShape`], from the decoder's record when there is one,
    /// else from the ops.
    pub fn shape(&self, types: &[HLType]) -> BodyShape {
        self.body.shape.unwrap_or_else(|| BodyShape::of(self, types))
    }

    pub fn ops(&self) -> &[Opcode] {
        self.body.ops()
    }

    /// The ops when they are already decoded, without decoding them.
    #[inline]
    pub fn decoded_ops(&self) -> Option<&[Opcode]> {
        self.body.ops.get().map(Vec::as_slice)
    }

    /// Op `pc`, decoding only the chunk that holds it when the body is
    /// decoded by chunk ([`FunctionBody::is_chunked`]); `None` past the end.
    pub fn op_at(&self, pc: usize) -> Option<&Opcode> {
        self.body.op_at(pc)
    }

    /// How many ops the body has, without decoding it.
    pub fn op_count(&self) -> usize {
        self.body.encoded_ops().unwrap_or_else(|| self.ops().len())
    }

    pub fn ops_mut(&mut self) -> &mut Vec<Opcode> {
        self.body.get_mut().0
    }

    pub fn set_ops(&mut self, ops: Vec<Opcode>) {
        *self.body.get_mut().0 = ops;
    }

    pub fn debug(&self) -> &[i32] {
        self.body.debug()
    }

    pub fn set_debug(&mut self, debug: Vec<i32>) {
        *self.body.get_mut().1 = debug;
    }

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
        for op in self.ops() {
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
    macro_rules! hr {
        ($r:expr) => {
            h = H32(h, $r.0 as u32);
        };
    }
    macro_rules! hi {
        ($i:expr) => {
            h = H32(h, *$i as u32);
        };
    }
    macro_rules! hargs {
        ($args:expr) => {
            for a in $args {
                hr!(a);
            }
        };
    }

    match op {
        Opcode::Mov { dst, src } => {
            hr!(dst);
            hr!(src);
        }
        Opcode::Int { dst, ptr } => {
            hr!(dst);
            hi!(&ptr.0);
        }
        Opcode::Float { dst, ptr } => {
            hr!(dst);
            hi!(&ptr.0);
        }
        Opcode::Bool { dst, value } => {
            hr!(dst);
            h = H(h, *value as u8);
        }
        Opcode::Bytes { dst, ptr } => {
            hr!(dst);
            hi!(&ptr.0);
        }
        Opcode::String { dst, ptr } => {
            hr!(dst);
            hi!(&ptr.0);
        }
        Opcode::Null { dst } => {
            hr!(dst);
        }

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
        | Opcode::Xor { dst, a, b } => {
            hr!(dst);
            hr!(a);
            hr!(b);
        }

        Opcode::Neg { dst, src } | Opcode::Not { dst, src } => {
            hr!(dst);
            hr!(src);
        }
        Opcode::Incr { dst } | Opcode::Decr { dst } => {
            hr!(dst);
        }

        Opcode::Call0 { dst, fun } => {
            hr!(dst);
            hi!(&fun.0);
        }
        Opcode::Call1 { dst, fun, arg0 } => {
            hr!(dst);
            hi!(&fun.0);
            hr!(arg0);
        }
        Opcode::Call2 {
            dst,
            fun,
            arg0,
            arg1,
        } => {
            hr!(dst);
            hi!(&fun.0);
            hr!(arg0);
            hr!(arg1);
        }
        Opcode::Call3 {
            dst,
            fun,
            arg0,
            arg1,
            arg2,
        } => {
            hr!(dst);
            hi!(&fun.0);
            hr!(arg0);
            hr!(arg1);
            hr!(arg2);
        }
        Opcode::Call4 {
            dst,
            fun,
            arg0,
            arg1,
            arg2,
            arg3,
        } => {
            hr!(dst);
            hi!(&fun.0);
            hr!(arg0);
            hr!(arg1);
            hr!(arg2);
            hr!(arg3);
        }
        Opcode::CallN { dst, fun, args } => {
            hr!(dst);
            hi!(&fun.0);
            hargs!(args);
        }
        Opcode::CallMethod { dst, field, args } => {
            hr!(dst);
            hi!(&field.0);
            hargs!(args);
        }
        Opcode::CallThis { dst, field, args } => {
            hr!(dst);
            hi!(&field.0);
            hargs!(args);
        }
        Opcode::CallClosure { dst, fun, args } => {
            hr!(dst);
            hr!(fun);
            hargs!(args);
        }
        Opcode::IndirectCall { dst, fun, args } => {
            hr!(dst);
            hi!(&fun.0);
            hargs!(args);
        }

        Opcode::StaticClosure { dst, fun } => {
            hr!(dst);
            hi!(&fun.0);
        }
        Opcode::InstanceClosure { dst, fun, obj } => {
            hr!(dst);
            hi!(&fun.0);
            hr!(obj);
        }
        Opcode::VirtualClosure { dst, obj, field } => {
            hr!(dst);
            hr!(obj);
            hr!(field);
        }

        Opcode::GetGlobal { dst, global } => {
            hr!(dst);
            hi!(&global.0);
        }
        Opcode::SetGlobal { global, src } => {
            hi!(&global.0);
            hr!(src);
        }

        Opcode::Field { dst, obj, field } => {
            hr!(dst);
            hr!(obj);
            hi!(&field.0);
        }
        Opcode::SetField { obj, field, src } => {
            hr!(obj);
            hi!(&field.0);
            hr!(src);
        }
        Opcode::GetThis { dst, field } => {
            hr!(dst);
            hi!(&field.0);
        }
        Opcode::SetThis { field, src } => {
            hi!(&field.0);
            hr!(src);
        }

        Opcode::DynGet { dst, obj, field } => {
            hr!(dst);
            hr!(obj);
            hi!(&field.0);
        }
        Opcode::DynSet { obj, field, src } => {
            hr!(obj);
            hi!(&field.0);
            hr!(src);
        }

        Opcode::JTrue { cond, offset } | Opcode::JFalse { cond, offset } => {
            hr!(cond);
            hi!(offset);
        }
        Opcode::JNull { reg, offset } | Opcode::JNotNull { reg, offset } => {
            hr!(reg);
            hi!(offset);
        }
        Opcode::JSLt { a, b, offset }
        | Opcode::JSGte { a, b, offset }
        | Opcode::JSGt { a, b, offset }
        | Opcode::JSLte { a, b, offset }
        | Opcode::JULt { a, b, offset }
        | Opcode::JUGte { a, b, offset }
        | Opcode::JNotLt { a, b, offset }
        | Opcode::JNotGte { a, b, offset }
        | Opcode::JEq { a, b, offset }
        | Opcode::JNotEq { a, b, offset } => {
            hr!(a);
            hr!(b);
            hi!(offset);
        }
        Opcode::JAlways { offset } => {
            hi!(offset);
        }

        Opcode::ToDyn { dst, src }
        | Opcode::ToSFloat { dst, src }
        | Opcode::ToUFloat { dst, src }
        | Opcode::ToInt { dst, src }
        | Opcode::SafeCast { dst, src }
        | Opcode::UnsafeCast { dst, src }
        | Opcode::ToVirtual { dst, src } => {
            hr!(dst);
            hr!(src);
        }

        Opcode::Ret { ret } => {
            hr!(ret);
        }
        Opcode::Throw { exc } | Opcode::Rethrow { exc } => {
            hr!(exc);
        }
        Opcode::Switch { reg, offsets, end } => {
            hr!(reg);
            for o in offsets {
                hi!(o);
            }
            hi!(end);
        }
        Opcode::NullCheck { reg } => {
            hr!(reg);
        }
        Opcode::Trap { exc, offset } => {
            hr!(exc);
            hi!(offset);
        }
        Opcode::EndTrap { exc } => {
            hr!(exc);
        }

        Opcode::GetI8 { dst, bytes, index }
        | Opcode::GetI16 { dst, bytes, index }
        | Opcode::GetMem { dst, bytes, index } => {
            hr!(dst);
            hr!(bytes);
            hr!(index);
        }
        Opcode::SetI8 { bytes, index, src }
        | Opcode::SetI16 { bytes, index, src }
        | Opcode::SetMem { bytes, index, src } => {
            hr!(bytes);
            hr!(index);
            hr!(src);
        }
        Opcode::GetArray { dst, array, index } => {
            hr!(dst);
            hr!(array);
            hr!(index);
        }
        Opcode::SetArray { array, index, src } => {
            hr!(array);
            hr!(index);
            hr!(src);
        }
        Opcode::ArraySize { dst, array } => {
            hr!(dst);
            hr!(array);
        }

        Opcode::New { dst } => {
            hr!(dst);
        }
        Opcode::Type { dst, ty } => {
            hr!(dst);
            hi!(&ty.0);
        }
        Opcode::GetType { dst, src } | Opcode::GetTID { dst, src } => {
            hr!(dst);
            hr!(src);
        }

        Opcode::Ref { dst, src } | Opcode::Unref { dst, src } => {
            hr!(dst);
            hr!(src);
        }
        Opcode::Setref { dst, value } => {
            hr!(dst);
            hr!(value);
        }

        Opcode::MakeEnum {
            dst,
            construct,
            args,
        } => {
            hr!(dst);
            hi!(&construct.0);
            hargs!(args);
        }
        Opcode::EnumAlloc { dst, construct } => {
            hr!(dst);
            hi!(&construct.0);
        }
        Opcode::EnumIndex { dst, value } => {
            hr!(dst);
            hr!(value);
        }
        Opcode::EnumField {
            dst,
            value,
            construct,
            field,
        } => {
            hr!(dst);
            hr!(value);
            hi!(&construct.0);
            hi!(&field.0);
        }
        Opcode::SetEnumField { value, field, src } => {
            hr!(value);
            hi!(&field.0);
            hr!(src);
        }

        Opcode::Assert => {}
        Opcode::RefData { dst, src } => {
            hr!(dst);
            hr!(src);
        }
        Opcode::RefOffset { dst, reg, offset } => {
            hr!(dst);
            hr!(reg);
            hr!(offset);
        }
        Opcode::Nop => {}
        Opcode::Label => {}
        Opcode::Prefetch { value, field, mode } => {
            hr!(value);
            hi!(&field.0);
            hi!(mode);
        }
        Opcode::Asm { mode, value, reg } => {
            hi!(mode);
            hi!(value);
            hr!(reg);
        }
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
            fn finish(&self) -> u64 {
                self.0
            }
            fn write(&mut self, bytes: &[u8]) {
                for &b in bytes {
                    self.0 = self.0.wrapping_mul(31).wrapping_add(b as u64);
                }
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

/// `findex -> "Class.method"` for every method a type declares.
///
/// The only spelling of a function that survives recompilation. A findex is a
/// POSITION and moves when anything is added before it -- adding a class
/// nobody calls shifted a benchmark's caller from 26 to 28 -- and
/// `HLFunction::compute_hash` folds in `TypeRef` indices, which are positions
/// for the same reason. `HLFunction::field_name` and `obj` look like they
/// would answer this and are never populated by the reader; the proto table
/// is where the association actually lives.
///
/// Functions with no declaring type -- closures, and the module entrypoint --
/// are absent. A caller should skip those rather than fall back to an index,
/// which would look durable and not be.
pub fn function_names(types: &[HLType]) -> std::collections::HashMap<u32, String> {
    let mut out = std::collections::HashMap::new();
    for t in types {
        let Some(obj) = t.obj.as_ref() else { continue };
        if obj.name.contains(char::is_whitespace) {
            continue; // the profile format is whitespace-separated
        }
        for p in &obj.proto {
            if p.findex < 0 || p.name.contains(char::is_whitespace) {
                continue;
            }
            out.entry(p.findex as u32)
                .or_insert_with(|| format!("{}.{}", obj.name, p.name));
        }
        // Statics and bound methods are not in the proto table -- that holds
        // virtuals. They arrive as `[field index, findex]` pairs, and the name
        // is the field's. Without these a static caller such as a program's
        // `main` has no durable name at all, which is most of the corpus.
        for pair in obj.bindings.chunks_exact(2) {
            let (fid, findex) = (pair[0], pair[1]);
            if fid < 0 || findex < 0 {
                continue;
            }
            let Some(field) = binding_field(types, obj, fid as usize) else {
                continue;
            };
            if field.name.contains(char::is_whitespace) {
                continue;
            }
            out.entry(findex as u32)
                .or_insert_with(|| format!("{}.{}", obj.name, field.name));
        }
    }
    out
}

/// The field a binding names.
///
/// A binding's field index is the one `hl_obj_field_fetch` resolves: it
/// counts every ancestor's fields first, and the type's own list starts after
/// them. Every `$Class` type of statics extends `hl.Class`, so indexing the
/// own list directly named no static method at all -- the whole static
/// surface of a program, its `main` included, fell through to `#hash`.
fn binding_field<'a>(
    types: &'a [HLType],
    obj: &'a HLTypeObj,
    fid: usize,
) -> Option<&'a HLObjField> {
    let mut inherited = 0usize;
    let mut parent = obj.super_.as_ref();
    while let Some(sup) = parent {
        let sup = types.get(sup.0)?.obj.as_ref()?;
        inherited += sup.fields.len();
        parent = sup.super_.as_ref();
    }
    obj.fields.get(fid.checked_sub(inherited)?)
}

/// A durable key for every function, name where there is one.
///
/// [`function_names`] covers what a type declares -- around four in five
/// functions of a typical module. The rest are closures and the entrypoint,
/// which no class lists, and Haxe puts a program's `main` among them, so
/// dropping them would mean a profile that says nothing about a hot loop
/// written at top level.
///
/// Those fall back to `#<compute_hash>`, which is weaker: the hash folds in
/// `TypeRef` indices, so adding a type anywhere moves it. It still survives
/// edits that do not add types, and being wrong costs only the guard -- the
/// emitted check re-reads the real target at run time. The '#' keeps the two
/// kinds apart so a name can never be read as a hash.
pub fn function_keys(
    types: &[HLType],
    functions: &[HLFunction],
) -> std::collections::HashMap<u32, String> {
    let mut out = function_names(types);
    for f in functions {
        out.entry(f.findex as u32)
            .or_insert_with(|| format!("#{}", f.compute_hash()));
    }
    out
}
