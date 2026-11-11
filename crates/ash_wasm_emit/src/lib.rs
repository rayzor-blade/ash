//! Direct emission of WebAssembly from optimized AIR, without LLVM.
//!
//! The crate depends only on `air`, so it builds for wasm and can run inside a
//! browser. What a program is made of (types, constants) is asked of a
//! [`Program`]; what comes out is a module whose functions are named
//! `f<findex>`.
//!
//! Control flow is structured from the dominator tree (Ramsey, "Beyond
//! Relooper"): a `loop` per loop header, a `block` per merge node, an `if` per
//! conditional branch. Phi copies happen on the edge that carries them.
//!
//! This covers the numeric subset: functions whose values are booleans, 32- and
//! 64-bit integers and floats, and whose instructions are arithmetic,
//! comparisons, casts between those, direct calls and branches. A function
//! outside it is reported with the reason, and so is any function that calls
//! one. Objects, strings, exceptions and the runtime are not handled yet.
//!
//! A fused multiply-add has no wasm instruction, so the module imports it:
//! `ash.fma_f64` and `ash.fma_f32`, each `(a, b, c) -> a * b + c` rounded once.
//! The host supplies them, as the runtime does for LLVM-built modules.
//!
//! The module also imports `ash.poll`, called when the word at address 0 of
//! its memory is nonzero. [`Options::polls`] makes every loop header test that
//! word; [`Options::shadow`] keeps the shadow call stack stack traces read
//! (`(file << 32) | line` stored into a frame per function).
#![forbid(unsafe_code)]

mod calls;
mod dynamic;
mod locals;
mod memory;
mod traps;
pub mod object;

/// The encoder the instructions of [`object::Body`] come from.
pub use wasm_encoder;

use air::v2::CfgInfo;
use air::v2::ir::{
    BinOp, CastKind, CondKind, Function, Instr, IntrinsicKind, MemAccess, Terminator, TypeRef, UnOp,
    ValueId,
};
use std::collections::HashMap;
use wasm_encoder as we;
use we::{BlockType, Instruction as I, ValType};

/// The kinds of value the emitter can hold. `Other` is anything it cannot.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ty {
    Void,
    Bool,
    I32,
    /// Unsigned 8 and 16 bit integers, held in an `i32` that is always
    /// zero-extended: whatever produces one wraps it.
    U8,
    U16,
    I64,
    F32,
    F64,
    /// A reference to anything on the heap: an object, string, array, closure,
    /// dynamic. All of them are one `i32` in wasm32, and nothing here knows
    /// what is behind it.
    Ptr,
    Other,
}

impl Ty {
    fn wasm(self) -> Option<ValType> {
        match self {
            Ty::Bool | Ty::I32 | Ty::Ptr | Ty::U8 | Ty::U16 => Some(ValType::I32),
            Ty::I64 => Some(ValType::I64),
            Ty::F32 => Some(ValType::F32),
            Ty::F64 => Some(ValType::F64),
            Ty::Void | Ty::Other => None,
        }
    }

    fn is_int(self) -> bool {
        matches!(self, Ty::Bool | Ty::I32 | Ty::I64 | Ty::U8 | Ty::U16)
    }

    /// The mask that wraps an `i32` to this type, for the narrow ones.
    fn mask(self) -> Option<i32> {
        match self {
            Ty::U8 => Some(0xff),
            Ty::U16 => Some(0xffff),
            _ => None,
        }
    }

    fn is_float(self) -> bool {
        matches!(self, Ty::F32 | Ty::F64)
    }
}

/// What the emitter needs to know about the program a function belongs to.
pub trait Program {
    fn ty(&self, t: TypeRef) -> Ty;
    /// An entry of the integer constant pool.
    fn int(&self, idx: usize) -> Option<i32>;
    /// An entry of the float constant pool.
    fn float(&self, idx: usize) -> Option<f64>;
    /// The symbol a function is known by to the linker, which is what an
    /// object defines for it and what a caller in another object names.
    fn symbol(&self, findex: usize) -> String;
    /// The signature of any function of the program, emitted here or not.
    fn signature(&self, findex: usize) -> Option<Sig>;
    /// Where field `field` of the object type `obj_ty` lives, as a byte offset
    /// from the object, and the kind it was declared with. `None` for a type
    /// whose fields are not at fixed offsets (a virtual, say).
    fn field(&self, obj_ty: TypeRef, field: usize) -> Option<(u32, Ty)>;
    /// The hashed name of field `field` of the virtual type `obj_ty`; `None`
    /// when the type is not a virtual or has no such field.
    fn virtual_field(&self, obj_ty: TypeRef, field: usize) -> Option<i32>;
    /// The symbol of the table of global slots, one pointer wide each.
    fn globals_symbol(&self) -> Option<String>;
    /// The symbol of the table of function addresses, one pointer wide each,
    /// indexed by findex.
    fn functions_symbol(&self) -> Option<String>;
    /// What kind of reference values of type `t` are, where it matters to a
    /// cast.
    fn shape(&self, t: TypeRef) -> Shape;
    /// `T` of the type `Null<T>`.
    fn null_param(&self, t: TypeRef) -> Option<TypeRef>;
    /// The index of the type of function `findex`, native or not.
    fn function_type(&self, findex: usize) -> Option<usize>;
    /// The symbol of the descriptor for type `type_index`.
    fn type_symbol(&self, type_index: usize) -> Option<String>;
    /// Whether two references of this type are compared by value, through the
    /// runtime, and not by address: dynamics, boxed numbers, objects (which
    /// may define a comparison) and virtuals.
    fn dynamic_compare(&self, t: TypeRef) -> bool;
    /// What the runtime's comparison answers for a pair with no order.
    fn invalid_comparison(&self) -> i32;
    /// How `New` allocates a value of this type.
    fn alloc(&self, type_index: usize) -> Option<Alloc>;
    /// The bytes one element of an array whose elements have type `t` takes.
    fn array_elem_size(&self, t: TypeRef) -> Option<u32>;
    /// Whether a store of a reference into the heap must mark its card for
    /// the generational collector.
    fn write_barriers(&self) -> bool;
    /// The signature, receiver first, of the function that method slot `slot`
    /// of the class `obj_ty` holds.
    fn method(&self, obj_ty: TypeRef, slot: usize) -> Option<Sig>;
    /// The function the class `obj_ty` itself names at method slot `slot`.
    fn method_function(&self, obj_ty: TypeRef, slot: usize) -> Option<usize>;
    /// Whether a value of this type is its own Dynamic: it starts with the
    /// type descriptor that says what it is.
    fn self_describing(&self, t: TypeRef) -> bool;
    /// The type `Dynamic`.
    fn dyn_type(&self) -> Option<TypeRef>;
    /// The signature, receiver first, of method `field` of the structural type
    /// `obj_ty`, as declared.
    fn virtual_method(&self, obj_ty: TypeRef, field: usize) -> Option<Sig>;
    /// The number a type descriptor holds as its kind, for a structural type.
    fn virtual_kind(&self) -> i32;
    /// Where field `field` of constructor `construct` of the enum `enum_ty`
    /// lives, as a byte offset from the value, and its kind.
    fn enum_field(&self, enum_ty: TypeRef, construct: usize, field: usize) -> Option<(u32, Ty)>;
    /// The hash of string `idx` of the string pool, as a field name.
    fn string_hash(&self, idx: usize) -> Option<i32>;
    /// The symbol of string `idx` of the pool, and of bytes constant `idx`.
    fn string_symbol(&self, idx: usize) -> Option<String>;
    fn bytes_symbol(&self, idx: usize) -> Option<String>;
    /// The type `Void`.
    fn void_type(&self) -> Option<TypeRef>;
    /// The kind number of type `t`, as a type descriptor holds it.
    fn type_kind(&self, t: TypeRef) -> Option<i32>;
    /// Whether `t` is the type of type descriptors.
    fn is_type(&self, t: TypeRef) -> bool;
    /// A native that lives in a library the host opens: the data symbol that
    /// holds its address once the library is open, and the names it is asked
    /// for by. The slot is zero when the library could not be opened.
    fn native_slot(&self, findex: usize) -> Option<NativeSlot>;
}

/// Where a program finds a native of a library the host opens.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NativeSlot {
    pub slot: String,
    pub lib: String,
    pub prim: String,
}

/// The prefix of the name of a data symbol that holds a C string the body
/// names; the rest of the name is the text.
const CSTR: &str = "ash_cstr:";
/// The same for text in UTF-16, which is what the runtime reads names in.
const UTF16: &str = "ash_utf16:";

/// The kinds of reference a cast tells apart.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Shape {
    /// `Dynamic`.
    Dyn,
    /// `Null<T>`.
    Null,
    /// A structural type, wrapped around the object it views.
    Virtual,
    Obj,
    Struct,
    /// A pointer with no type header of its own (bytes, a type, a reference,
    /// an abstract), which a Dynamic holds in a box.
    Boxed,
    Other,
}

/// How a value of a type is allocated.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Alloc {
    /// An object or struct, from its type descriptor. With a size and flags
    /// when the runtime can be told both up front.
    Object { sized: Option<(u32, u32)> },
    /// An anonymous object.
    DynObj,
    /// A virtual, from its type descriptor.
    Virtual,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Sig {
    pub params: Vec<Ty>,
    pub ret: Ty,
}

/// Why a function cannot be emitted.
#[derive(Clone, Debug)]
pub struct Unsupported(pub String);

type Res<T> = Result<T, Unsupported>;

fn unsupported<T>(why: impl Into<String>) -> Res<T> {
    Err(Unsupported(why.into()))
}

/// Imports every module declares, ahead of its functions: `ash.fma_f64`,
/// `ash.fma_f32` and `ash.poll`.
const IMPORTS: u32 = 3;
const FMA64: u32 = 0;
const FMA32: u32 = 1;
const POLL: u32 = 2;
/// The module's own helpers, after the imports: open a shadow frame, close it,
/// and test the poll word.
const SHADOW_PUSH: u32 = 3;
const SHADOW_POP: u32 = 4;
const POLL_CHECK: u32 = 5;
/// Where the functions of the program start.
const FIRST: u32 = IMPORTS + 3;
/// Memory layout: the poll word, a scratch frame for pushes past capacity, then
/// the frames.
const SCRATCH_FRAME: i32 = 64;
const FRAMES: i32 = 128;
const FRAME_CAP: i32 = 1 << 15;
const FRAME_BYTES: i32 = 16;
const MEMORY_PAGES: u64 = 9;

/// What the emitted code carries besides the program's own work.
#[derive(Clone, Copy, Debug, Default)]
pub struct Options {
    /// Test the poll word at the head of every loop.
    pub polls: bool,
    /// Open a shadow frame per function and store source positions into it.
    pub shadow: bool,
}

/// Which optional runtime functions a body ended up calling.
#[derive(Clone, Default)]
struct Used {
    fma: bool,
    /// Runtime functions called by name, from [`RT_FUNCS`].
    funcs: Vec<&'static str>,
    /// Data symbols the body takes the address of.
    data: Vec<String>,
}

pub struct Input<'a> {
    pub findex: usize,
    pub sig: Sig,
    pub body: &'a Function,
}

pub struct Emitted {
    pub bytes: Vec<u8>,
    /// The findex of each function in the module, in order.
    pub functions: Vec<usize>,
    pub skipped: Vec<(usize, String)>,
}

/// Emit every input that can be emitted, and report the rest.
pub fn emit_module<P: Program>(program: &P, inputs: &[Input]) -> Emitted {
    emit_module_with(program, inputs, Options::default())
}

/// [`emit_module`] with polls and shadow frames as asked.
pub fn emit_module_with<P: Program>(program: &P, inputs: &[Input], opts: Options) -> Emitted {
    let module_rt = Runtime {
        fma64: Ref::index(FMA64),
        fma32: Ref::index(FMA32),
        push: Ref::index(SHADOW_PUSH),
        pop: Ref::index(SHADOW_POP),
        poll: Ref::index(POLL_CHECK),
        epoch: None,
        funcs: HashMap::new(),
        any_func: false,
        data: HashMap::new(),
        frames: false,
    };
    // First pass: every input is assumed callable, to learn which fail on
    // their own and whom they call.
    let provisional: HashMap<usize, Callee> = inputs
        .iter()
        .enumerate()
        .map(|(i, inp)| {
            (
                inp.findex,
                Callee {
                    to: Ref::index(FIRST + i as u32),
                    sig: inp.sig.clone(),
                },
            )
        })
        .collect();
    let first = |findex: usize| provisional.get(&findex).cloned();
    let mut failure: Vec<Option<String>> = vec![None; inputs.len()];
    let mut callers: HashMap<usize, Vec<usize>> = HashMap::new();
    for (i, inp) in inputs.iter().enumerate() {
        match emit_function(program, inp, &first, &module_rt, opts) {
            Ok((_, called, _)) => {
                for callee in called {
                    callers.entry(callee).or_default().push(i);
                }
            }
            Err(e) => failure[i] = Some(e.0),
        }
    }
    // A function that calls one that failed fails with it: nothing else
    // defines the callee in a module.
    let mut work: Vec<usize> = (0..inputs.len()).filter(|i| failure[*i].is_some()).collect();
    while let Some(bad) = work.pop() {
        let findex = inputs[bad].findex;
        for &caller in callers.get(&findex).into_iter().flatten() {
            if failure[caller].is_none() {
                failure[caller] = Some(format!("calls f{findex}, which is not emitted"));
                work.push(caller);
            }
        }
    }

    // Second pass: the live functions, with their final indices.
    let live: Vec<usize> = (0..inputs.len())
        .filter(|i| failure[*i].is_none())
        .collect();
    let finals: HashMap<usize, Callee> = live
        .iter()
        .enumerate()
        .map(|(n, &i)| {
            (
                inputs[i].findex,
                Callee {
                    to: Ref::index(FIRST + n as u32),
                    sig: inputs[i].sig.clone(),
                },
            )
        })
        .collect();
    let second = |findex: usize| finals.get(&findex).cloned();

    let mut types = TypeTable::default();
    let fma64 = types.get(vec![ValType::F64; 3], vec![ValType::F64]);
    let fma32 = types.get(vec![ValType::F32; 3], vec![ValType::F32]);
    let void = types.get(vec![], vec![]);
    let push = types.get(vec![ValType::I32], vec![ValType::I32]);
    let mut funcs = we::FunctionSection::new();
    let mut code = we::CodeSection::new();
    prelude(&mut funcs, &mut code, push, void);
    let mut exports = we::ExportSection::new();
    let mut functions = Vec::new();
    let mut skipped: Vec<(usize, String)> = Vec::new();
    for (n, &i) in live.iter().enumerate() {
        let inp = &inputs[i];
        match emit_function(program, inp, &second, &module_rt, opts) {
            Ok((body, _, _)) => {
                let params: Vec<ValType> = inp.sig.params.iter().filter_map(|t| t.wasm()).collect();
                let results: Vec<ValType> = inp.sig.ret.wasm().into_iter().collect();
                funcs.function(types.get(params, results));
                code.function(&body.into_function());
                exports.export(
                    &format!("f{}", inp.findex),
                    we::ExportKind::Func,
                    FIRST + n as u32,
                );
                functions.push(inp.findex);
            }
            Err(e) => skipped.push((inp.findex, e.0)),
        }
    }
    for (i, why) in failure.into_iter().enumerate() {
        if let Some(why) = why {
            skipped.push((inputs[i].findex, why));
        }
    }

    let mut module = we::Module::new();
    let mut type_section = we::TypeSection::new();
    for (params, results) in &types.list {
        type_section
            .ty()
            .function(params.iter().copied(), results.iter().copied());
    }
    module.section(&type_section);
    let mut imports = we::ImportSection::new();
    imports.import("ash", "fma_f64", we::EntityType::Function(fma64));
    imports.import("ash", "fma_f32", we::EntityType::Function(fma32));
    imports.import("ash", "poll", we::EntityType::Function(void));
    module.section(&imports);
    module.section(&funcs);
    let mut memories = we::MemorySection::new();
    memories.memory(we::MemoryType {
        minimum: MEMORY_PAGES,
        maximum: None,
        memory64: false,
        shared: false,
        page_size_log2: None,
    });
    module.section(&memories);
    let mut globals = we::GlobalSection::new();
    globals.global(
        we::GlobalType {
            val_type: ValType::I32,
            mutable: true,
            shared: false,
        },
        &we::ConstExpr::i32_const(0),
    );
    module.section(&globals);
    exports.export("memory", we::ExportKind::Memory, 0);
    module.section(&exports);
    module.section(&code);
    Emitted {
        bytes: module.finish(),
        functions,
        skipped,
    }
}

/// A relocatable object holding the inputs that can be emitted. What it leaves
/// out is not an error: the functions in `skipped` are defined by whoever else
/// the linker is given, and the object calls them by symbol like any other
/// function it does not define.
pub struct ObjectEmitted {
    pub bytes: Vec<u8>,
    pub functions: Vec<usize>,
    pub skipped: Vec<(usize, String)>,
    /// Functions the object calls and does not define, which whoever links it
    /// must therefore export.
    pub imports: Vec<usize>,
    /// Data symbols the object takes the address of, which whoever defines
    /// them must export.
    pub data: Vec<String>,
}

/// The runtime symbols an object calls. `hlp_fiber_poll` is the safepoint the
/// fiber transform and the collector key on; `fma` and `fmaf` are libm's.
pub const RT_SHADOW_PUSH: &str = "hlp_shadow_push";
pub const RT_SHADOW_POP: &str = "hlp_shadow_pop";
pub const RT_POLL: &str = "hlp_fiber_poll";
/// The runtime functions a body may call by name, with their wasm signatures.
/// Pointers and sizes are one `i32` on wasm32.
const RT_FUNCS: &[(&str, &[ValType], &[ValType])] = &[
    ("hlp_null_access", &[], &[]),
    ("hlp_dyn_compare", &[ValType::I32, ValType::I32], &[ValType::I32]),
    ("hlp_alloc_obj", &[ValType::I32], &[ValType::I32]),
    (
        "hlp_alloc_obj_sized_flags",
        &[ValType::I32, ValType::I32, ValType::I32],
        &[ValType::I32],
    ),
    ("hlp_alloc_dynobj", &[], &[ValType::I32]),
    ("hlp_alloc_virtual", &[ValType::I32], &[ValType::I32]),
    ("hlp_gc_write_barrier", &[ValType::I32], &[]),
    ("hl_to_virtual", &[ValType::I32, ValType::I32], &[ValType::I32]),
    ("hlp_alloc_dynamic", &[ValType::I32], &[ValType::I32]),
    ("hlp_dyn_toint", &[ValType::I32], &[ValType::I32]),
    ("hlp_dyn_tofloat", &[ValType::I32], &[ValType::F32]),
    ("hlp_dyn_todouble", &[ValType::I32], &[ValType::F64]),
    ("hlp_dyn_toi64", &[ValType::I32], &[ValType::I64]),
    ("hlp_alloc_closure_void", &[ValType::I32, ValType::I32], &[ValType::I32]),
    (
        "hlp_alloc_closure_ptr",
        &[ValType::I32, ValType::I32, ValType::I32],
        &[ValType::I32],
    ),
    ("hlp_aot_native_missing", &[ValType::I32, ValType::I32], &[]),
    ("hlp_error", &[ValType::I32, ValType::I32], &[]),
    ("fmod", &[ValType::F64, ValType::F64], &[ValType::F64]),
    ("fmodf", &[ValType::F32, ValType::F32], &[ValType::F32]),
    ("hlp_setup_trap_jit", &[], &[ValType::I32]),
    ("hlp_remove_trap_jit", &[], &[]),
    ("hlp_get_exc_value", &[], &[ValType::I32]),
    ("hlp_clear_exc_value", &[], &[]),
    ("hlp_throw", &[ValType::I32], &[]),
    ("hlp_rethrow", &[ValType::I32], &[]),
    ("__wasm_longjmp", &[ValType::I32, ValType::I32], &[]),
    ("hlp_alloc_enum", &[ValType::I32, ValType::I32], &[ValType::I32]),
    ("hlp_dyn_castd", &[ValType::I32, ValType::I32], &[ValType::F64]),
    ("hlp_dyn_castf", &[ValType::I32, ValType::I32], &[ValType::F32]),
    ("hlp_dyn_casti64", &[ValType::I32, ValType::I32], &[ValType::I64]),
    (
        "hlp_dyn_casti",
        &[ValType::I32, ValType::I32, ValType::I32],
        &[ValType::I32],
    ),
    ("hlp_same_type", &[ValType::I32, ValType::I32], &[ValType::I32]),
    ("hlp_make_dyn", &[ValType::I32, ValType::I32], &[ValType::I32]),
    (
        "hlp_dyn_call",
        &[ValType::I32, ValType::I32, ValType::I32],
        &[ValType::I32],
    ),
    (
        "hlp_dyn_castp",
        &[ValType::I32, ValType::I32, ValType::I32],
        &[ValType::I32],
    ),
    ("hlp_alloc_dyn_array", &[ValType::I32], &[ValType::I32]),
    (
        "hlp_vcall_dyn",
        &[ValType::I32, ValType::I32, ValType::I32],
        &[ValType::I32],
    ),
    ("hlp_dyn_getd", &[ValType::I32, ValType::I32], &[ValType::F64]),
    ("hlp_dyn_getf", &[ValType::I32, ValType::I32], &[ValType::F32]),
    ("hlp_dyn_geti64", &[ValType::I32, ValType::I32], &[ValType::I64]),
    ("hlp_dyn_geti", &[ValType::I32, ValType::I32, ValType::I32], &[ValType::I32]),
    ("hlp_dyn_getp", &[ValType::I32, ValType::I32, ValType::I32], &[ValType::I32]),
    ("hlp_dyn_setd", &[ValType::I32, ValType::I32, ValType::F64], &[]),
    ("hlp_dyn_setf", &[ValType::I32, ValType::I32, ValType::F32], &[]),
    ("hlp_dyn_seti64", &[ValType::I32, ValType::I32, ValType::I64], &[]),
    (
        "hlp_dyn_seti",
        &[ValType::I32, ValType::I32, ValType::I32, ValType::I32],
        &[],
    ),
    (
        "hlp_dyn_setp",
        &[ValType::I32, ValType::I32, ValType::I32, ValType::I32],
        &[],
    ),
];

/// Where a virtual keeps the object it stands for, and its table of field
/// pointers, as byte offsets on wasm32: `vvirtual` is `{ t, value, next }`
/// followed by one pointer per field.
const VVIRTUAL_VALUE: u64 = 4;
const VVIRTUAL_FIELDS: u64 = 12;
/// Where a `varray` keeps its element count and its first element on wasm32:
/// `{ t, at, size, pad }` and then the elements.
/// Where a `vdynamic` keeps the value it boxes: `{ t, <8-aligned union> }`.
const VDYNAMIC_VALUE: u64 = 8;
const VARRAY_SIZE: u64 = 8;
const VARRAY_DATA: u64 = 16;
/// Where a type descriptor keeps the table of its class's methods.
const TYPE_VOBJ_PROTO: u64 = 8;
/// A closure is `{ t, fun, hasValue, value }`; one made to adapt a signature
/// is followed by the closure it wraps, and has `hasValue` 2.
const VCLOSURE_FUN: u64 = 4;
const VCLOSURE_HAS_VALUE: u64 = 8;
const VCLOSURE_VALUE: u64 = 12;
const VCLOSURE_WRAPPED: u64 = 16;
const WRAPPER: i32 = 2;
/// The marker error that asks `emit_function` to try again with a frame.
const NEEDS_FRAME: &str = "\u{0}needs a frame in memory";
/// The runtime's `AtomicU64` that ticks when a safepoint is wanted.
pub const RT_EPOCH: &str = "ash_fiber_poll_epoch";

pub fn emit_object<P: Program>(program: &P, inputs: &[Input], opts: Options) -> ObjectEmitted {
    let mut obj = object::ObjectBuilder::new();
    let mut emitted = emit_into(&mut obj, program, inputs, opts);
    emitted.bytes = obj.finish();
    emitted
}

/// [`emit_object`] into a builder that already holds other definitions, or
/// will: the result's `bytes` is empty, and the caller finishes the object.
pub fn emit_into<P: Program>(
    obj: &mut object::ObjectBuilder,
    program: &P,
    inputs: &[Input],
    opts: Options,
) -> ObjectEmitted {
    // Pass one: which inputs emit at all, and whom the survivors call. Calls
    // to anything are allowed here, since the linker resolves them.
    let throwaway = Runtime {
        fma64: Ref::index(0),
        fma32: Ref::index(0),
        push: Ref::index(0),
        pop: Ref::index(0),
        poll: Ref::index(0),
        epoch: None,
        funcs: HashMap::new(),
        any_func: true,
        data: HashMap::new(),
        frames: true,
    };
    let known = |findex: usize| {
        program.signature(findex).map(|sig| Callee {
            to: Ref::index(0),
            sig,
        })
    };
    let mut live: Vec<usize> = Vec::new();
    let mut skipped: Vec<(usize, String)> = Vec::new();
    let mut called: Vec<usize> = Vec::new();
    let mut uses_fma = false;
    let mut rt_names: Vec<&'static str> = Vec::new();
    let mut data_names: Vec<String> = Vec::new();
    for (i, inp) in inputs.iter().enumerate() {
        match emit_function(program, inp, &known, &throwaway, opts) {
            Ok((_, c, used)) => {
                live.push(i);
                called.extend(c);
                uses_fma |= used.fma;
                rt_names.extend(used.funcs);
                data_names.extend(used.data);
            }
            Err(e) => skipped.push((inp.findex, e.0)),
        }
    }

    // Pass two: symbols first, so a body can call a function defined after it.
    // Field accesses and the frame and poll words all go through memory.
    obj.import_memory(1);
    let wasm_sig = |sig: &Sig| {
        (
            sig.params.iter().filter_map(|t| t.wasm()).collect::<Vec<_>>(),
            sig.ret.wasm().into_iter().collect::<Vec<_>>(),
        )
    };
    let mut syms: HashMap<usize, object::Sym> = HashMap::new();
    for &i in &live {
        let inp = &inputs[i];
        let (p, r) = wasm_sig(&inp.sig);
        syms.insert(
            inp.findex,
            obj.declare_function(&program.symbol(inp.findex), p, r, object::Linkage::Hidden),
        );
    }
    called.sort_unstable();
    called.dedup();
    let mut imports = Vec::new();
    for findex in called {
        if syms.contains_key(&findex) {
            continue;
        }
        let Some(sig) = program.signature(findex) else {
            continue;
        };
        let (p, r) = wasm_sig(&sig);
        syms.insert(findex, obj.import_function(&program.symbol(findex), p, r));
        imports.push(findex);
    }
    let rt_sym = |obj: &mut object::ObjectBuilder, name: &str, p: Vec<ValType>, r: Vec<ValType>| {
        Ref {
            index: 0,
            sym: Some(obj.import_function(name, p, r)),
        }
    };
    let none = Ref::index(0);
    let runtime = Runtime {
        fma64: if uses_fma {
            rt_sym(obj, "fma", vec![ValType::F64; 3], vec![ValType::F64])
        } else {
            none
        },
        fma32: if uses_fma {
            rt_sym(obj, "fmaf", vec![ValType::F32; 3], vec![ValType::F32])
        } else {
            none
        },
        push: if opts.shadow {
            rt_sym(obj, RT_SHADOW_PUSH, vec![ValType::I32], vec![ValType::I32])
        } else {
            none
        },
        pop: if opts.shadow {
            rt_sym(obj, RT_SHADOW_POP, vec![], vec![])
        } else {
            none
        },
        poll: if opts.polls {
            rt_sym(obj, RT_POLL, vec![], vec![])
        } else {
            none
        },
        epoch: opts.polls.then(|| obj.import_data(RT_EPOCH)),
        funcs: {
            rt_names.sort_unstable();
            rt_names.dedup();
            rt_names
                .iter()
                .map(|&name| {
                    let (_, p, r) = RT_FUNCS
                        .iter()
                        .find(|(n, _, _)| *n == name)
                        .map(|(n, p, r)| (n, p.to_vec(), r.to_vec()))
                        .expect("a runtime function the table does not list");
                    (name, rt_sym(obj, name, p, r))
                })
                .collect()
        },
        any_func: false,
        data: data_names
            .iter()
            .map(|n| {
                let sym = if let Some(text) = n.strip_prefix(CSTR) {
                    // The text of a C string is defined here.
                    let sym = obj.declare_data(n, text.len() as u32 + 1, 0, object::Linkage::Local);
                    obj.set_data(sym, text.as_bytes(), &[]);
                    sym
                } else if let Some(text) = n.strip_prefix(UTF16) {
                    let units: Vec<u8> = text
                        .encode_utf16()
                        .chain(std::iter::once(0))
                        .flat_map(u16::to_le_bytes)
                        .collect();
                    let sym = obj.declare_data(n, units.len() as u32, 1, object::Linkage::Local);
                    obj.set_data(sym, &units, &[]);
                    sym
                } else {
                    obj.import_data(n)
                };
                (n.clone(), sym)
            })
            .collect(),
        frames: true,
    };
    let resolve = |findex: usize| {
        let sym = *syms.get(&findex)?;
        let sig = program.signature(findex)?;
        Some(Callee {
            to: Ref {
                index: 0,
                sym: Some(sym),
            },
            sig,
        })
    };
    let mut functions = Vec::new();
    for &i in &live {
        let inp = &inputs[i];
        match emit_function(program, inp, &resolve, &runtime, opts) {
            Ok((body, _, _)) => {
                obj.set_body(syms[&inp.findex], body);
                functions.push(inp.findex);
            }
            Err(e) => panic!(
                "f{} emitted in the first pass and not the second: {}",
                inp.findex, e.0
            ),
        }
    }
    data_names.sort_unstable();
    data_names.dedup();
    // What the body defines itself is not for another object to provide.
    data_names.retain(|n| !n.starts_with(CSTR) && !n.starts_with(UTF16));
    ObjectEmitted {
        bytes: Vec::new(),
        functions,
        skipped,
        imports,
        data: data_names,
    }
}

/// The module's own helpers: `shadow_push`, `shadow_pop` and `poll_check`.
fn prelude(funcs: &mut we::FunctionSection, code: &mut we::CodeSection, push: u32, void: u32) {
    // shadow_push(findex) -> slot: frame `depth` lives at FRAMES + 16 * depth,
    // or in the scratch frame past capacity; the depth counts either way so
    // the pops balance. The position word sits 8 bytes into the frame.
    funcs.function(push);
    let mut f = we::Function::new([(2, ValType::I32)]);
    f.instruction(&I::GlobalGet(0));
    f.instruction(&I::LocalSet(1));
    f.instruction(&I::GlobalGet(0));
    f.instruction(&I::I32Const(1));
    f.instruction(&I::I32Add);
    f.instruction(&I::GlobalSet(0));
    f.instruction(&I::LocalGet(1));
    f.instruction(&I::I32Const(FRAME_CAP));
    f.instruction(&I::I32LtU);
    f.instruction(&I::If(BlockType::Result(ValType::I32)));
    f.instruction(&I::LocalGet(1));
    f.instruction(&I::I32Const(FRAME_BYTES));
    f.instruction(&I::I32Mul);
    f.instruction(&I::I32Const(FRAMES));
    f.instruction(&I::I32Add);
    f.instruction(&I::Else);
    f.instruction(&I::I32Const(SCRATCH_FRAME));
    f.instruction(&I::End);
    f.instruction(&I::LocalTee(2));
    f.instruction(&I::LocalGet(0));
    f.instruction(&I::I32Store(we::MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    f.instruction(&I::LocalGet(2));
    f.instruction(&I::I32Const(8));
    f.instruction(&I::I32Add);
    f.instruction(&I::End);
    code.function(&f);

    // shadow_pop()
    funcs.function(void);
    let mut f = we::Function::new([]);
    f.instruction(&I::GlobalGet(0));
    f.instruction(&I::I32Const(1));
    f.instruction(&I::I32Sub);
    f.instruction(&I::GlobalSet(0));
    f.instruction(&I::End);
    code.function(&f);

    // poll_check(): the host asks for a safepoint by writing the word at 0.
    funcs.function(void);
    let mut f = we::Function::new([]);
    f.instruction(&I::I32Const(0));
    f.instruction(&I::I32Load(we::MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    f.instruction(&I::If(BlockType::Empty));
    f.instruction(&I::Call(POLL));
    f.instruction(&I::End);
    f.instruction(&I::End);
    code.function(&f);
}

#[derive(Default)]
struct TypeTable {
    index: HashMap<(Vec<ValType>, Vec<ValType>), u32>,
    list: Vec<(Vec<ValType>, Vec<ValType>)>,
}

impl TypeTable {
    fn get(&mut self, params: Vec<ValType>, results: Vec<ValType>) -> u32 {
        let key = (params, results);
        if let Some(&t) = self.index.get(&key) {
            return t;
        }
        let t = self.list.len() as u32;
        self.list.push(key.clone());
        self.index.insert(key, t);
        t
    }
}

/// How a body names a function: by its index in a module, or by the symbol an
/// object leaves for the linker to resolve.
#[derive(Clone, Copy)]
struct Ref {
    index: u32,
    sym: Option<object::Sym>,
}

impl Ref {
    fn index(index: u32) -> Self {
        Ref { index, sym: None }
    }
}

#[derive(Clone)]
struct Callee {
    to: Ref,
    sig: Sig,
}

/// The helpers every body may call.
struct Runtime {
    fma64: Ref,
    fma32: Ref,
    push: Ref,
    pop: Ref,
    poll: Ref,
    /// The runtime's poll epoch, when the body tests it inline: a loop header
    /// loads it and calls `poll` only if it has moved since this body last
    /// looked. Without it every header calls `poll`.
    epoch: Option<object::Sym>,
    /// Runtime functions by name. Empty in a standalone module, which has no
    /// runtime to ask.
    funcs: HashMap<&'static str, Ref>,
    /// Answers every name, for the pass that only finds out what a body uses.
    any_func: bool,
    /// Data symbols an object may take the address of, by name. Empty in a
    /// standalone module, which has none.
    data: HashMap<String, object::Sym>,
    /// Whether a body may keep values in a frame in memory, which needs the
    /// linker's stack pointer.
    frames: bool,
}

#[derive(Clone, Copy, PartialEq)]
enum Frame {
    Loop(usize),
    Block(usize),
    If,
}

struct Fx<'a, P: Program> {
    program: &'a P,
    f: &'a Function,
    sig: &'a Sig,
    callees: &'a dyn Fn(usize) -> Option<Callee>,
    rt: &'a Runtime,
    opts: Options,
    frame_local: u32,
    handled_local: u32,
    cur_local: u32,
    tmp_local: u32,
    /// Whether this body opens a frame in memory, and the locals that hold
    /// the stack pointer on entry, the frame's base, and four scratch words.
    has_frame: bool,
    frame_size: u32,
    sp_local: u32,
    fp_local: u32,
    scratch: u32,
    /// Where in the frame each cell of the function is.
    cell_offsets: Vec<u32>,
    /// The trap regions of the body, by handler block.
    traps: HashMap<usize, traps::TrapInfo>,
    /// Stores that need no barrier, by (block, instruction).
    barrier_free: std::collections::HashSet<(usize, usize)>,
    /// Where `instr` is, for the analysis above.
    at: (usize, usize),
    called: Vec<usize>,
    used: Used,
    out: object::Body,
    ctx: Vec<Frame>,
    nparams: u32,
    /// The wasm local of each value.
    slot: Vec<u32>,
    tys: Vec<Ty>,
    rpo_num: Vec<usize>,
    loop_header: Vec<bool>,
    merge: Vec<bool>,
    kids: Vec<Vec<usize>>,
}

fn emit_function<P: Program>(
    program: &P,
    inp: &Input,
    callees: &dyn Fn(usize) -> Option<Callee>,
    rt: &Runtime,
    opts: Options,
) -> Res<(object::Body, Vec<usize>, Used)> {
    // A body finds out that it needs memory for values only as it goes; the
    // second attempt opens a frame for it.
    match emit_function_in(program, inp, callees, rt, opts, false) {
        Err(e) if e.0 == NEEDS_FRAME && rt.frames => {
            emit_function_in(program, inp, callees, rt, opts, true)
        }
        Err(e) if e.0 == NEEDS_FRAME => unsupported("a frame in memory, in a module with no stack"),
        other => other,
    }
}

fn emit_function_in<P: Program>(
    program: &P,
    inp: &Input,
    callees: &dyn Fn(usize) -> Option<Callee>,
    rt: &Runtime,
    opts: Options,
    with_frame: bool,
) -> Res<(object::Body, Vec<usize>, Used)> {
    let f = inp.body;
    if inp.sig.params.iter().any(|t| t.wasm().is_none())
        || !(inp.sig.ret == Ty::Void || inp.sig.ret.wasm().is_some())
    {
        return unsupported("signature has a type outside the numeric subset");
    }
    let tys: Vec<Ty> = f.values.iter().map(|v| program.ty(v.ty)).collect();
    if let Some(t) = tys.iter().find(|t| matches!(t, Ty::Other)) {
        return unsupported(format!("a value of type {t:?}"));
    }
    let cfg = CfgInfo::build(f);
    let assignment = locals::assign(f, &cfg, &tys);
    let n = f.blocks.len();
    let rpo = cfg.rpo();
    let mut rpo_num = vec![usize::MAX; n];
    for (i, b) in rpo.iter().enumerate() {
        rpo_num[b.idx()] = i;
    }
    let mut fwd_in = vec![0usize; n];
    let mut forced = vec![false; n];
    let mut loop_header = vec![false; n];
    for b in &rpo {
        let from = b.idx();
        let blk = &f.blocks[from];
        if let Terminator::Trap { handler, .. } = &blk.term {
            forced[handler.idx()] = true;
        }
        let mut seen: Vec<usize> = Vec::new();
        for s in blk.term.successors() {
            let to = s.idx();
            if seen.contains(&to) {
                continue;
            }
            seen.push(to);
            if rpo_num[to] <= rpo_num[from] {
                loop_header[to] = true;
            } else {
                fwd_in[to] += 1;
            }
        }
        if let Terminator::Switch {
            targets, default, ..
        } = &blk.term
        {
            for t in targets.iter().chain(std::iter::once(default)) {
                forced[t.idx()] = true;
            }
        }
    }
    let mut merge = vec![false; n];
    for b in &rpo {
        let i = b.idx();
        merge[i] = i != rpo[0].idx() && (fwd_in[i] >= 2 || forced[i]);
    }
    let mut kids: Vec<Vec<usize>> = vec![Vec::new(); n];
    for b in &rpo {
        let i = b.idx();
        for &c in &cfg.dom.dom_children[i] {
            if c != i && merge[c] {
                kids[i].push(c);
            }
        }
        kids[i].sort_by_key(|k| rpo_num[*k]);
    }
    let nparams = inp.sig.params.len() as u32;
    let mut trap_order: Vec<(usize, air::v2::ir::CellId, Option<usize>)> = Vec::new();
    for b in &rpo {
        let blk = &f.blocks[b.idx()];
        if let Terminator::Trap {
            exc_cell, handler, ..
        } = &blk.term
        {
            if !f.blocks[handler.idx()].phis.is_empty() {
                return unsupported("a handler that takes values");
            }
            trap_order.push((handler.idx(), *exc_cell, blk.handler.map(|h| h.idx())));
        }
    }
    if !trap_order.is_empty() && !with_frame {
        return unsupported(NEEDS_FRAME);
    }
    let mut locals: Vec<(u32, ValType)> = Vec::new();
    for &t in &assignment.types {
        match locals.last_mut() {
            Some((count, last)) if *last == t => *count += 1,
            _ => locals.push((1, t)),
        }
    }
    let frame_local = nparams + assignment.types.len() as u32;
    locals.push((1, ValType::I32));
    // The poll epoch this body last saw, and the one just read.
    let handled_local = frame_local + 1;
    let cur_local = frame_local + 2;
    locals.push((1, ValType::I64));
    locals.push((1, ValType::I64));
    // An i32 scratch, for a value read twice.
    let tmp_local = frame_local + 3;
    locals.push((1, ValType::I32));
    // The stack pointer on entry, the frame's base, and four scratch words.
    let (sp_local, fp_local, scratch) = (frame_local + 4, frame_local + 5, frame_local + 6);
    if with_frame {
        locals.push((6, ValType::I32));
    }
    // One local per trap region, for the buffer it is armed with.
    let trap_base = frame_local + 10;
    let trap_infos: HashMap<usize, traps::TrapInfo> = trap_order
        .iter()
        .enumerate()
        .map(|(i, &(handler, exc_cell, parent))| {
            (
                handler,
                traps::TrapInfo {
                    exc_cell,
                    slot: trap_base + i as u32,
                    parent,
                },
            )
        })
        .collect();
    if !trap_order.is_empty() {
        locals.push((trap_order.len() as u32, ValType::I32));
    }
    let has_loops = loop_header.iter().any(|h| *h);
    let mut fx = Fx {
        program,
        f,
        sig: &inp.sig,
        callees,
        rt,
        opts,
        frame_local,
        handled_local,
        cur_local,
        tmp_local,
        has_frame: with_frame,
        frame_size: 0,
        sp_local,
        fp_local,
        scratch,
        cell_offsets: Vec::new(),
        traps: trap_infos,
        barrier_free: air::v2::analysis::barrier_free_stores(f),
        at: (0, 0),
        called: Vec::new(),
        used: Used::default(),
        out: object::Body::new(locals),
        ctx: Vec::new(),
        nparams,
        slot: assignment.slot.iter().map(|s| nparams + s).collect(),
        tys,
        rpo_num,
        loop_header,
        merge,
        kids,
    };
    for (i, t) in inp.sig.params.iter().enumerate() {
        if let Some(m) = t.mask() {
            fx.emit(&I::LocalGet(i as u32));
            fx.emit(&I::I32Const(m));
            fx.emit(&I::I32And);
            fx.emit(&I::LocalSet(i as u32));
        }
    }
    if with_frame {
        // sp0 = sp; fp = sp - frame size; sp = fp
        fx.out.stack_pointer_get();
        fx.emit(&I::LocalTee(sp_local));
        fx.out.frame_size_const();
        fx.emit(&I::I32Sub);
        fx.emit(&I::LocalTee(fp_local));
        fx.out.stack_pointer_set();
        fx.open_cells()?;
    }
    if opts.shadow {
        let findex = inp.findex as i32;
        fx.emit(&I::I32Const(findex));
        fx.call(rt.push);
        let frame = fx.frame_local;
        fx.emit(&I::LocalSet(frame));
    }
    if opts.polls
        && has_loops
        && let Some(epoch) = rt.epoch
    {
        fx.out.address_of(epoch, 0);
        fx.emit(&I::I64Load(we::MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        fx.emit(&I::LocalSet(handled_local));
    }
    if !rpo.is_empty() {
        fx.do_tree(rpo[0].idx())?;
    }
    // Control never falls off the end; this satisfies the function's result.
    fx.emit(&I::Unreachable);
    fx.emit(&I::End);
    let frame = fx.frame_size.div_ceil(16) * 16;
    fx.out.set_frame_size(frame);
    Ok((fx.out, fx.called, fx.used))
}

impl<P: Program> Fx<'_, P> {
    fn emit(&mut self, i: &I) {
        self.out.instruction(i);
    }

    /// Reserve `size` bytes in the frame, aligned to `align`, and return their
    /// offset from the frame's base.
    fn frame_alloc(&mut self, size: u32, align: u32) -> Res<u32> {
        if !self.has_frame {
            return unsupported(NEEDS_FRAME);
        }
        let at = self.frame_size.div_ceil(align) * align;
        self.frame_size = at + size;
        Ok(at)
    }

    /// The address of the frame byte at `offset`.
    fn frame_address(&mut self, offset: u32) {
        self.emit(&I::LocalGet(self.fp_local));
        if offset != 0 {
            self.emit(&I::I32Const(offset as i32));
            self.emit(&I::I32Add);
        }
    }

    /// Give the frame back, on the way out.
    fn close_frame(&mut self) {
        if self.has_frame {
            self.emit(&I::LocalGet(self.sp_local));
            self.out.stack_pointer_set();
        }
    }

    /// A safepoint at a loop header.
    fn poll(&mut self) {
        let poll = self.rt.poll;
        let Some(epoch) = self.rt.epoch else {
            self.call(poll);
            return;
        };
        let (handled, cur) = (self.handled_local, self.cur_local);
        self.out.address_of(epoch, 0);
        self.emit(&I::I64Load(we::MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        self.emit(&I::LocalTee(cur));
        self.emit(&I::LocalGet(handled));
        self.emit(&I::I64Ne);
        self.emit(&I::If(BlockType::Empty));
        self.emit(&I::LocalGet(cur));
        self.emit(&I::LocalSet(handled));
        self.call(poll);
        self.emit(&I::End);
    }

    fn call(&mut self, r: Ref) {
        match r.sym {
            Some(sym) => self.out.call(sym),
            None => self.out.instruction(&I::Call(r.index)),
        }
    }

    fn get(&mut self, v: ValueId) {
        self.emit(&I::LocalGet(self.slot[v.0 as usize]));
    }

    fn set(&mut self, v: ValueId) {
        self.emit(&I::LocalSet(self.slot[v.0 as usize]));
    }

    fn ty(&self, v: ValueId) -> Ty {
        self.tys[v.0 as usize]
    }

    fn label(&self, want: Frame) -> u32 {
        self.ctx
            .iter()
            .rev()
            .position(|f| *f == want)
            .expect("a branch target has no enclosing label") as u32
    }

    fn do_tree(&mut self, x: usize) -> Res<()> {
        let ys = self.kids[x].clone();
        if self.loop_header[x] {
            self.emit(&I::Loop(BlockType::Empty));
            self.ctx.push(Frame::Loop(x));
            if self.opts.polls {
                self.poll();
            }
            self.node_within(x, &ys)?;
            self.ctx.pop();
            self.emit(&I::End);
        } else {
            self.node_within(x, &ys)?;
        }
        Ok(())
    }

    fn node_within(&mut self, x: usize, ys: &[usize]) -> Res<()> {
        match ys.split_last() {
            None => {
                self.code(x)?;
                self.terminator(x)
            }
            Some((&y, rest)) => {
                self.emit(&I::Block(BlockType::Empty));
                self.ctx.push(Frame::Block(y));
                self.node_within(x, rest)?;
                self.ctx.pop();
                self.emit(&I::End);
                self.do_tree(y)
            }
        }
    }

    /// The pairs `(phi, incoming)` that the edge `from -> to` copies.
    fn phi_pairs(&self, from: usize, to: usize) -> Vec<(ValueId, ValueId)> {
        self.f.blocks[to]
            .phis
            .iter()
            .filter_map(|p| {
                p.incoming
                    .iter()
                    .find(|(b, _)| b.idx() == from)
                    .map(|(_, v)| (p.dst, *v))
            })
            .collect()
    }

    /// A parallel copy: every source is read before any destination is written.
    fn phi_copies(&mut self, from: usize, to: usize) -> Res<()> {
        let pairs = self.phi_pairs(from, to);
        for (dst, src) in &pairs {
            if self.ty(*dst) != self.ty(*src) {
                return unsupported("a phi that joins values of different types");
            }
            self.get(*src);
        }
        for (dst, _) in pairs.iter().rev() {
            self.set(*dst);
        }
        Ok(())
    }

    fn is_back_edge(&self, from: usize, to: usize) -> bool {
        self.rpo_num[to] <= self.rpo_num[from]
    }

    fn branch(&mut self, from: usize, to: usize) -> Res<()> {
        self.phi_copies(from, to)?;
        if self.is_back_edge(from, to) {
            let l = self.label(Frame::Loop(to));
            self.emit(&I::Br(l));
        } else if self.merge[to] {
            let l = self.label(Frame::Block(to));
            self.emit(&I::Br(l));
        } else {
            self.do_tree(to)?;
        }
        Ok(())
    }

    fn zero(&mut self, t: Ty) {
        match t.wasm() {
            Some(ValType::I64) => self.emit(&I::I64Const(0)),
            Some(ValType::F32) => self.emit(&I::F32Const(0.0f32.into())),
            Some(ValType::F64) => self.emit(&I::F64Const(0.0f64.into())),
            _ => self.emit(&I::I32Const(0)),
        }
    }

    fn int_const(&self, idx: usize) -> Res<i32> {
        let base = self.f.int_pool_base;
        if idx >= base {
            return self
                .f
                .pending_ints
                .get(idx - base)
                .copied()
                .ok_or_else(|| Unsupported(format!("integer constant {idx} is not in the pool")));
        }
        self.program
            .int(idx)
            .ok_or_else(|| Unsupported(format!("integer constant {idx} is not in the pool")))
    }

    fn code(&mut self, x: usize) -> Res<()> {
        let f = self.f;
        let instrs = &f.blocks[x].instrs;
        let region = f.blocks[x].handler.map(|h| h.idx());
        if self.traps.contains_key(&x) {
            self.landing(x)?;
        }
        for (i, ins) in instrs.iter().enumerate() {
            self.at = (x, i);
            // Of consecutive positions only the last can be read back.
            if let Instr::Pos { .. } = ins
                && matches!(instrs.get(i + 1), Some(Instr::Pos { .. }))
            {
                continue;
            }
            if region.is_some() && traps::may_throw(ins) {
                self.guarded(region, &mut |fx| fx.instr(ins))?;
            } else {
                self.instr(ins)?;
            }
        }
        Ok(())
    }

    fn instr(&mut self, ins: &Instr) -> Res<()> {
        match ins {
            Instr::Pos { file, line, .. } => {
                if self.opts.shadow {
                    let frame = self.frame_local;
                    self.emit(&I::LocalGet(frame));
                    self.emit(&I::I64Const(((u64::from(*file) << 32) | u64::from(*line)) as i64));
                    self.emit(&I::I64Store(we::MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                }
            }
            Instr::Param { dst, reg } => {
                // Registers past the arguments are locals that start zeroed.
                if *reg < self.nparams {
                    if self.sig.params[*reg as usize] != self.ty(*dst) {
                        return unsupported("a parameter whose register type differs");
                    }
                    self.emit(&I::LocalGet(*reg));
                } else {
                    let t = self.ty(*dst);
                    self.zero(t);
                }
                self.set(*dst);
            }
            Instr::Copy { dst, src } => {
                if self.ty(*dst) != self.ty(*src) {
                    return unsupported("a copy between different types");
                }
                self.get(*src);
                self.set(*dst);
            }
            Instr::Null { dst } => {
                let t = self.ty(*dst);
                if t == Ty::Other {
                    return unsupported("a null constant into a value of no storable kind");
                }
                // A void holds nothing, so there is nothing to store.
                if t != Ty::Void {
                    self.zero(t);
                    self.set(*dst);
                }
            }
            Instr::EndTrap { .. } => self.end_trap(self.at.0)?,
            // A cache hint, which wasm has no way to give.
            Instr::Prefetch { .. } => {}
            Instr::Assert => {
                // The runtime's error, which raises; its second word is the
                // empty list of further arguments.
                self.data_address(&format!("{UTF16}assert"), 0)?;
                self.emit(&I::I32Const(0));
                let error = self.runtime("hlp_error")?;
                self.call(error);
                self.emit(&I::Unreachable);
            }
            Instr::NullCheck { value } => self.null_check(*value)?,
            Instr::String { dst, idx } => self.string_const(*dst, *idx)?,
            Instr::Bytes { dst, idx } => self.bytes_const(*dst, *idx)?,
            Instr::GetType { dst, src } => self.get_type(*dst, *src)?,
            Instr::GetTID { dst, src } => self.get_tid(*dst, *src)?,
            Instr::DynGet { dst, obj, field } => self.dyn_get(*dst, *obj, *field)?,
            Instr::DynSet { obj, field, src } => self.dyn_set(*obj, *field, *src)?,
            Instr::New { dst } => self.new_value(*dst)?,
            Instr::TypeConst { dst, ty } => {
                if self.ty(*dst) != Ty::Ptr {
                    return unsupported("a type constant into a non-reference");
                }
                let Some(name) = self.program.type_symbol(ty.0 as usize) else {
                    return unsupported("a type with no descriptor symbol");
                };
                self.data_address(&name, 0)?;
                self.set(*dst);
            }
            Instr::GetGlobal { dst, global } => {
                if self.ty(*dst) != Ty::Ptr {
                    return unsupported("a global read into a non-reference");
                }
                self.global_address(*global)?;
                self.emit(&I::I32Load(we::MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));
                self.set(*dst);
            }
            Instr::SetGlobal { global, src } => {
                if !matches!(self.ty(*src), Ty::Ptr | Ty::I32) {
                    return unsupported("a global slot written with a value wider than a word");
                }
                self.global_address(*global)?;
                self.get(*src);
                self.emit(&I::I32Store(we::MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));
            }
            Instr::FieldGet {
                dst,
                obj,
                obj_ty,
                field,
            } => self.field_get(*dst, *obj, *obj_ty, *field)?,
            Instr::FieldSet {
                obj,
                obj_ty,
                field,
                src,
            } => self.field_set(*obj, *obj_ty, *field, *src)?,
            Instr::Int { dst, idx } => {
                let v = self.int_const(*idx)?;
                match self.ty(*dst) {
                    Ty::I32 | Ty::Bool => self.emit(&I::I32Const(v)),
                    Ty::U8 | Ty::U16 => {
                        let m = self.ty(*dst).mask().unwrap_or(-1);
                        self.emit(&I::I32Const(v & m))
                    }
                    Ty::I64 => self.emit(&I::I64Const(i64::from(v))),
                    t => return unsupported(format!("an integer constant into {t:?}")),
                }
                self.set(*dst);
            }
            Instr::Float { dst, idx } => {
                let v = self
                    .program
                    .float(*idx)
                    .ok_or_else(|| Unsupported(format!("float constant {idx} is not in the pool")))?;
                match self.ty(*dst) {
                    Ty::F64 => self.emit(&I::F64Const(v.into())),
                    Ty::F32 => self.emit(&I::F32Const((v as f32).into())),
                    t => return unsupported(format!("a float constant into {t:?}")),
                }
                self.set(*dst);
            }
            Instr::Bool { dst, value } => {
                if self.ty(*dst) != Ty::Bool {
                    return unsupported("a boolean constant into a non-boolean");
                }
                self.emit(&I::I32Const(i32::from(*value)));
                self.set(*dst);
            }
            Instr::BinOp { op, dst, a, b } => self.bin_op(*op, *dst, *a, *b)?,
            Instr::UnOp { op, dst, src } => self.un_op(*op, *dst, *src)?,
            Instr::Cast { kind, dst, src } => self.cast(*kind, *dst, *src)?,
            Instr::Call { dst, fun, args } => self.call_instr(*dst, *fun, args)?,
            Instr::CellGet { dst, cell } => self.cell_get(*dst, *cell)?,
            Instr::CellSet { cell, src } => self.cell_set(*cell, *src)?,
            Instr::CellIncr { cell } => self.cell_step(*cell, true)?,
            Instr::CellDecr { cell } => self.cell_step(*cell, false)?,
            Instr::CellRef { dst, cell } => self.cell_ref(*dst, *cell)?,
            Instr::Unref { dst, src } => self.unref(*dst, *src)?,
            Instr::SetRef { r, value } => self.set_ref(*r, *value)?,
            Instr::RefData { dst, src } => self.ref_data(*dst, *src)?,
            Instr::RefOffset { dst, base, offset } => self.ref_offset(*dst, *base, *offset)?,
            Instr::EnumAlloc { dst, construct } => self.enum_alloc(*dst, *construct)?,
            Instr::MakeEnum {
                dst,
                construct,
                args,
            } => self.make_enum(*dst, *construct, args)?,
            Instr::EnumIndex { dst, value } => self.enum_index(*dst, *value)?,
            Instr::EnumField {
                dst,
                value,
                construct,
                field,
            } => self.enum_field(*dst, *value, *construct, *field)?,
            Instr::SetEnumField {
                value,
                construct,
                field,
                src,
            } => self.set_enum_field(*value, *construct, *field, *src)?,
            Instr::CallMethod { dst, field, args } => self.call_method(*dst, *field, args)?,
            Instr::CallClosure { dst, fun, args } => self.call_closure(*dst, *fun, args)?,
            Instr::Intrinsic { kind, dst, args, .. } => self.intrinsic(*kind, *dst, args)?,
            Instr::ArraySize { dst, array } => self.array_size(*dst, *array)?,
            Instr::StaticClosure { dst, fun } => self.closure(*dst, *fun, None)?,
            Instr::InstanceClosure { dst, fun, obj } => self.closure(*dst, *fun, Some(*obj))?,
            Instr::VirtualClosure { dst, obj, field } => self.virtual_closure(*dst, *obj, *field)?,
            Instr::MemGet {
                kind,
                dst,
                base,
                index,
            } => self.mem_get(*kind, *dst, *base, *index)?,
            Instr::MemSet {
                kind,
                base,
                index,
                src,
            } => self.mem_set(*kind, *base, *index, *src)?,
            Instr::Fma { dst, a, b, c } => {
                let t = self.ty(*dst);
                if !t.is_float() || self.ty(*a) != t || self.ty(*b) != t || self.ty(*c) != t {
                    return unsupported("a fused multiply-add on mismatched types");
                }
                self.get(*a);
                self.get(*b);
                self.get(*c);
                let fma = if t == Ty::F64 { self.rt.fma64 } else { self.rt.fma32 };
                self.call(fma);
                self.used.fma = true;
                self.set(*dst);
            }
            other => {
                let name: String = format!("{other:?}")
                    .chars()
                    .take_while(|c| c.is_ascii_alphanumeric())
                    .collect();
                return unsupported(format!("the {name} instruction"));
            }
        }
        Ok(())
    }

    /// A runtime function by name, noting that this body calls it.
    fn runtime(&mut self, name: &'static str) -> Res<Ref> {
        if !self.used.funcs.contains(&name) {
            self.used.funcs.push(name);
        }
        if self.rt.any_func {
            return Ok(Ref::index(0));
        }
        match self.rt.funcs.get(name) {
            Some(&r) => Ok(r),
            None => unsupported(format!("{name}, which needs the runtime")),
        }
    }

    /// Push the address of a named data symbol plus `addend`.
    fn data_address(&mut self, name: &str, addend: i32) -> Res<()> {
        if !self.used.data.iter().any(|d| d == name) {
            self.used.data.push(name.to_string());
        }
        // A standalone module has no data symbols; pass one of an object only
        // records the name.
        match self.rt.data.get(name) {
            Some(&sym) => self.out.address_of(sym, addend),
            None if self.rt.any_func => self.emit(&I::I32Const(0)),
            None => return unsupported("data from another object, in a standalone module"),
        }
        Ok(())
    }

    fn global_address(&mut self, global: usize) -> Res<()> {
        let Some(table) = self.program.globals_symbol() else {
            return unsupported("a program with no global table");
        };
        let offset = i32::try_from(global * 4).map_err(|_| Unsupported("a global past 2 GiB".into()))?;
        self.data_address(&table, offset)
    }

    fn new_value(&mut self, dst: ValueId) -> Res<()> {
        if self.ty(dst) != Ty::Ptr {
            return unsupported("an allocation into a non-reference");
        }
        let type_index = self.f.values[dst.0 as usize].ty.0 as usize;
        let Some(plan) = self.program.alloc(type_index) else {
            return unsupported("an allocation of a type with no plan");
        };
        match plan {
            Alloc::DynObj => {
                let alloc = self.runtime("hlp_alloc_dynobj")?;
                self.call(alloc);
            }
            Alloc::Virtual | Alloc::Object { .. } => {
                let Some(name) = self.program.type_symbol(type_index) else {
                    return unsupported("a type with no descriptor symbol");
                };
                self.data_address(&name, 0)?;
                match plan {
                    Alloc::Object {
                        sized: Some((size, flags)),
                    } => {
                        self.emit(&I::I32Const(size as i32));
                        self.emit(&I::I32Const(flags as i32));
                        let alloc = self.runtime("hlp_alloc_obj_sized_flags")?;
                        self.call(alloc);
                    }
                    Alloc::Object { sized: None } => {
                        let alloc = self.runtime("hlp_alloc_obj")?;
                        self.call(alloc);
                    }
                    Alloc::Virtual => {
                        let alloc = self.runtime("hlp_alloc_virtual")?;
                        self.call(alloc);
                    }
                    Alloc::DynObj => unreachable!("handled above"),
                }
            }
        }
        self.set(dst);
        Ok(())
    }

    fn null_check(&mut self, value: ValueId) -> Res<()> {
        if self.ty(value) != Ty::Ptr {
            // Nothing else can be null.
            return Ok(());
        }
        let raise = self.runtime("hlp_null_access")?;
        self.get(value);
        self.emit(&I::I32Eqz);
        self.emit(&I::If(BlockType::Empty));
        self.call(raise);
        self.emit(&I::Unreachable);
        self.emit(&I::End);
        Ok(())
    }

    /// The offset of a field and the kind of memory access it takes, checked
    /// against the type of the register that reads or writes it.
    fn field_slot(&self, obj_ty: TypeRef, field: usize, value: ValueId) -> Res<(u32, Ty)> {
        let Some((offset, declared)) = self.program.field(obj_ty, field) else {
            return unsupported("a field of a type without fixed offsets");
        };
        if declared != self.ty(value) {
            return unsupported("a field whose declared kind differs from its register");
        }
        Ok((offset, declared))
    }

    /// Load a value of `kind` from the address on the stack plus `offset`.
    fn load_kind(&mut self, kind: Ty, offset: u64) -> Res<()> {
        let mem = |align: u32| we::MemArg {
            offset,
            align,
            memory_index: 0,
        };
        match kind {
            // Any nonzero byte is true, as the runtime reads it.
            Ty::Bool => {
                self.emit(&I::I32Load8U(mem(0)));
                self.emit(&I::I32Const(0));
                self.emit(&I::I32Ne);
            }
            Ty::U8 => self.emit(&I::I32Load8U(mem(0))),
            Ty::U16 => self.emit(&I::I32Load16U(mem(1))),
            Ty::I32 | Ty::Ptr => self.emit(&I::I32Load(mem(2))),
            Ty::I64 => self.emit(&I::I64Load(mem(3))),
            Ty::F32 => self.emit(&I::F32Load(mem(2))),
            Ty::F64 => self.emit(&I::F64Load(mem(3))),
            Ty::Void | Ty::Other => return unsupported("a field of no storable kind"),
        }
        Ok(())
    }

    /// Store a value of `kind`, on the stack above its address, at `offset`.
    fn store_kind(&mut self, kind: Ty, offset: u64) -> Res<()> {
        let mem = |align: u32| we::MemArg {
            offset,
            align,
            memory_index: 0,
        };
        match kind {
            Ty::Bool | Ty::U8 => self.emit(&I::I32Store8(mem(0))),
            Ty::U16 => self.emit(&I::I32Store16(mem(1))),
            Ty::I32 | Ty::Ptr => self.emit(&I::I32Store(mem(2))),
            Ty::I64 => self.emit(&I::I64Store(mem(3))),
            Ty::F32 => self.emit(&I::F32Store(mem(2))),
            Ty::F64 => self.emit(&I::F64Store(mem(3))),
            Ty::Void | Ty::Other => return unsupported("a field of no storable kind"),
        }
        Ok(())
    }

    fn field_get(&mut self, dst: ValueId, obj: ValueId, obj_ty: TypeRef, field: usize) -> Res<()> {
        if self.ty(obj) != Ty::Ptr {
            return unsupported("a field read from a non-reference");
        }
        if let Some(hash) = self.program.virtual_field(obj_ty, field) {
            return self.virtual_get(dst, obj, field, hash);
        }
        let (offset, kind) = self.field_slot(obj_ty, field, dst)?;
        self.get(obj);
        self.load_kind(kind, u64::from(offset))?;
        self.set(dst);
        Ok(())
    }

    /// The descriptor of the type of `v`, pushed as an address.
    fn type_of_value(&mut self, v: ValueId) -> Res<()> {
        let type_index = self.f.values[v.0 as usize].ty.0 as usize;
        let Some(name) = self.program.type_symbol(type_index) else {
            return unsupported("a type with no descriptor symbol");
        };
        self.data_address(&name, 0)
    }

    /// A field of a virtual: through its pointer in the field table when the
    /// virtual has one, and by name on the object it stands for when not.
    fn virtual_get(&mut self, dst: ValueId, obj: ValueId, field: usize, hash: i32) -> Res<()> {
        let kind = self.ty(dst);
        if matches!(kind, Ty::Void | Ty::Other) {
            return unsupported("a virtual field of no storable kind");
        }
        let slot = VVIRTUAL_FIELDS + 4 * field as u64;
        let tmp = self.tmp_local;
        self.get(obj);
        self.emit(&I::I32Load(we::MemArg {
            offset: slot,
            align: 2,
            memory_index: 0,
        }));
        self.emit(&I::LocalTee(tmp));
        self.emit(&I::If(BlockType::Empty));
        self.emit(&I::LocalGet(tmp));
        self.load_kind(kind, 0)?;
        self.set(dst);
        self.emit(&I::Else);
        self.get(obj);
        self.emit(&I::I32Load(we::MemArg {
            offset: VVIRTUAL_VALUE,
            align: 2,
            memory_index: 0,
        }));
        self.emit(&I::I32Const(hash));
        let getter = match kind {
            Ty::F64 => "hlp_dyn_getd",
            Ty::F32 => "hlp_dyn_getf",
            Ty::I64 => "hlp_dyn_geti64",
            Ty::Bool | Ty::I32 | Ty::U8 | Ty::U16 => "hlp_dyn_geti",
            _ => "hlp_dyn_getp",
        };
        if !matches!(kind, Ty::F64 | Ty::F32 | Ty::I64) {
            self.type_of_value(dst)?;
        }
        let getter = self.runtime(getter)?;
        self.call(getter);
        if kind == Ty::Bool {
            self.emit(&I::I32Const(0));
            self.emit(&I::I32Ne);
        }
        self.narrow(kind);
        self.set(dst);
        self.emit(&I::End);
        Ok(())
    }

    /// After a store of `src`, mark the card of the address `addr_of` pushes
    /// when the collector needs it.
    fn write_barrier(&mut self, addr_of: impl FnOnce(&mut Self), src: ValueId) -> Res<()> {
        if self.ty(src) != Ty::Ptr
            || !self.program.write_barriers()
            || self.barrier_free.contains(&self.at)
        {
            return Ok(());
        }
        let barrier = self.runtime("hlp_gc_write_barrier")?;
        addr_of(self);
        self.call(barrier);
        Ok(())
    }

    fn field_set(&mut self, obj: ValueId, obj_ty: TypeRef, field: usize, src: ValueId) -> Res<()> {
        if self.ty(obj) != Ty::Ptr {
            return unsupported("a field write to a non-reference");
        }
        if let Some(hash) = self.program.virtual_field(obj_ty, field) {
            return self.virtual_set(obj, field, src, hash);
        }
        let (offset, kind) = self.field_slot(obj_ty, field, src)?;
        self.get(obj);
        self.get(src);
        self.store_kind(kind, u64::from(offset))?;
        self.write_barrier(
            |fx| {
                fx.get(obj);
                fx.emit(&I::I32Const(offset as i32));
                fx.emit(&I::I32Add);
            },
            src,
        )
    }

    fn virtual_set(&mut self, obj: ValueId, field: usize, src: ValueId, hash: i32) -> Res<()> {
        let kind = self.ty(src);
        if matches!(kind, Ty::Void | Ty::Other) {
            return unsupported("a virtual field of no storable kind");
        }
        let slot = VVIRTUAL_FIELDS + 4 * field as u64;
        let tmp = self.tmp_local;
        self.get(obj);
        self.emit(&I::I32Load(we::MemArg {
            offset: slot,
            align: 2,
            memory_index: 0,
        }));
        self.emit(&I::LocalTee(tmp));
        self.emit(&I::If(BlockType::Empty));
        self.emit(&I::LocalGet(tmp));
        self.get(src);
        self.store_kind(kind, 0)?;
        self.write_barrier(|fx| fx.emit(&I::LocalGet(tmp)), src)?;
        self.emit(&I::Else);
        self.get(obj);
        self.emit(&I::I32Load(we::MemArg {
            offset: VVIRTUAL_VALUE,
            align: 2,
            memory_index: 0,
        }));
        self.emit(&I::I32Const(hash));
        let setter = match kind {
            Ty::F64 => "hlp_dyn_setd",
            Ty::F32 => "hlp_dyn_setf",
            Ty::I64 => "hlp_dyn_seti64",
            Ty::Bool | Ty::I32 | Ty::U8 | Ty::U16 => "hlp_dyn_seti",
            _ => "hlp_dyn_setp",
        };
        if !matches!(kind, Ty::F64 | Ty::F32 | Ty::I64) {
            self.type_of_value(src)?;
        }
        self.get(src);
        let setter = self.runtime(setter)?;
        self.call(setter);
        self.emit(&I::End);
        Ok(())
    }

    fn bin_op(&mut self, op: BinOp, dst: ValueId, a: ValueId, b: ValueId) -> Res<()> {
        let (ta, tb, td) = (self.ty(a), self.ty(b), self.ty(dst));
        let shift = matches!(op, BinOp::Shl | BinOp::SShr | BinOp::UShr);
        if ta.is_int() && td == ta && (tb == ta || (shift && ta == Ty::I64 && tb == Ty::I32)) {
            let wide = ta == Ty::I64;
            // The reference wraps INT_MIN / -1; wasm traps on it.
            if matches!(op, BinOp::SDiv) {
                self.get(b);
                if wide {
                    self.emit(&I::I64Const(-1));
                    self.emit(&I::I64Eq);
                    self.emit(&I::If(BlockType::Result(ValType::I64)));
                    self.emit(&I::I64Const(0));
                    self.get(a);
                    self.emit(&I::I64Sub);
                    self.emit(&I::Else);
                    self.get(a);
                    self.get(b);
                    self.emit(&I::I64DivS);
                } else {
                    self.emit(&I::I32Const(-1));
                    self.emit(&I::I32Eq);
                    self.emit(&I::If(BlockType::Result(ValType::I32)));
                    self.emit(&I::I32Const(0));
                    self.get(a);
                    self.emit(&I::I32Sub);
                    self.emit(&I::Else);
                    self.get(a);
                    self.get(b);
                    self.emit(&I::I32DivS);
                }
                self.emit(&I::End);
                self.narrow(td);
                self.set(dst);
                return Ok(());
            }
            self.get(a);
            self.get(b);
            if wide && tb == Ty::I32 {
                self.emit(&I::I64ExtendI32U);
            }
            let ins = if wide {
                match op {
                    BinOp::Add => I::I64Add,
                    BinOp::Sub => I::I64Sub,
                    BinOp::Mul => I::I64Mul,
                    BinOp::UDiv => I::I64DivU,
                    BinOp::SMod => I::I64RemS,
                    BinOp::UMod => I::I64RemU,
                    BinOp::Shl => I::I64Shl,
                    BinOp::SShr => I::I64ShrS,
                    BinOp::UShr => I::I64ShrU,
                    BinOp::And => I::I64And,
                    BinOp::Or => I::I64Or,
                    BinOp::Xor => I::I64Xor,
                    BinOp::SDiv => unreachable!("handled above"),
                }
            } else {
                match op {
                    BinOp::Add => I::I32Add,
                    BinOp::Sub => I::I32Sub,
                    BinOp::Mul => I::I32Mul,
                    BinOp::UDiv => I::I32DivU,
                    BinOp::SMod => I::I32RemS,
                    BinOp::UMod => I::I32RemU,
                    BinOp::Shl => I::I32Shl,
                    BinOp::SShr => I::I32ShrS,
                    BinOp::UShr => I::I32ShrU,
                    BinOp::And => I::I32And,
                    BinOp::Or => I::I32Or,
                    BinOp::Xor => I::I32Xor,
                    BinOp::SDiv => unreachable!("handled above"),
                }
            };
            self.emit(&ins);
            self.narrow(td);
            self.set(dst);
            return Ok(());
        }
        if ta.is_float() && ta == tb && ta == td && matches!(op, BinOp::SMod) {
            self.get(a);
            self.get(b);
            let fmod = self.runtime(if ta == Ty::F64 { "fmod" } else { "fmodf" })?;
            self.call(fmod);
            self.set(dst);
            return Ok(());
        }
        if ta.is_float() && ta == tb && ta == td {
            let f64_ = ta == Ty::F64;
            let ins = match (op, f64_) {
                (BinOp::Add, true) => I::F64Add,
                (BinOp::Sub, true) => I::F64Sub,
                (BinOp::Mul, true) => I::F64Mul,
                (BinOp::SDiv, true) => I::F64Div,
                (BinOp::Add, false) => I::F32Add,
                (BinOp::Sub, false) => I::F32Sub,
                (BinOp::Mul, false) => I::F32Mul,
                (BinOp::SDiv, false) => I::F32Div,
                _ => return unsupported(format!("{op:?} on floats")),
            };
            self.get(a);
            self.get(b);
            self.emit(&ins);
            self.set(dst);
            return Ok(());
        }
        unsupported(format!("{op:?} on {ta:?} and {tb:?}"))
    }

    fn un_op(&mut self, op: UnOp, dst: ValueId, src: ValueId) -> Res<()> {
        let (ts, td) = (self.ty(src), self.ty(dst));
        if ts != td {
            return unsupported("a unary operation that changes type");
        }
        match (op, ts) {
            (UnOp::Neg, Ty::I32 | Ty::U8 | Ty::U16) => {
                self.emit(&I::I32Const(0));
                self.get(src);
                self.emit(&I::I32Sub);
            }
            (UnOp::Neg, Ty::I64) => {
                self.emit(&I::I64Const(0));
                self.get(src);
                self.emit(&I::I64Sub);
            }
            (UnOp::Neg, Ty::F64) => {
                self.get(src);
                self.emit(&I::F64Neg);
            }
            (UnOp::Neg, Ty::F32) => {
                self.get(src);
                self.emit(&I::F32Neg);
            }
            (UnOp::Not, Ty::Bool) => {
                self.get(src);
                self.emit(&I::I32Eqz);
            }
            (UnOp::Not, Ty::I32 | Ty::U8 | Ty::U16) => {
                self.get(src);
                self.emit(&I::I32Const(-1));
                self.emit(&I::I32Xor);
            }
            (UnOp::Not, Ty::I64) => {
                self.get(src);
                self.emit(&I::I64Const(-1));
                self.emit(&I::I64Xor);
            }
            (UnOp::Incr | UnOp::Decr, Ty::I32 | Ty::U8 | Ty::U16) => {
                self.get(src);
                self.emit(&I::I32Const(1));
                self.emit(&if matches!(op, UnOp::Incr) {
                    I::I32Add
                } else {
                    I::I32Sub
                });
            }
            (UnOp::Incr | UnOp::Decr, Ty::I64) => {
                self.get(src);
                self.emit(&I::I64Const(1));
                self.emit(&if matches!(op, UnOp::Incr) {
                    I::I64Add
                } else {
                    I::I64Sub
                });
            }
            (op, t) => return unsupported(format!("{op:?} on {t:?}")),
        }
        self.narrow(td);
        self.set(dst);
        Ok(())
    }

    /// Wrap the `i32` on the stack to `t`, when `t` is a narrow integer.
    fn narrow(&mut self, t: Ty) {
        if let Some(m) = t.mask() {
            self.emit(&I::I32Const(m));
            self.emit(&I::I32And);
        }
    }

    fn cast(&mut self, kind: CastKind, dst: ValueId, src: ValueId) -> Res<()> {
        let (ts, td) = (self.ty(src), self.ty(dst));
        match kind {
            CastKind::ToSFloat | CastKind::ToUFloat => {
                let signed = matches!(kind, CastKind::ToSFloat);
                self.get(src);
                match ts {
                    Ty::I32 | Ty::Bool | Ty::U8 | Ty::U16 => self.emit(&if signed {
                        I::F64ConvertI32S
                    } else {
                        I::F64ConvertI32U
                    }),
                    Ty::I64 => self.emit(&if signed {
                        I::F64ConvertI64S
                    } else {
                        I::F64ConvertI64U
                    }),
                    Ty::F32 => self.emit(&I::F64PromoteF32),
                    Ty::F64 => {}
                    t => return unsupported(format!("{kind:?} from {t:?}")),
                }
                match td {
                    Ty::F64 => {}
                    Ty::F32 => self.emit(&I::F32DemoteF64),
                    t => return unsupported(format!("{kind:?} into {t:?}")),
                }
            }
            CastKind::ToInt => {
                self.get(src);
                match (ts, td) {
                    // A float saturates at the destination's range.
                    (Ty::F64, Ty::I32) => self.emit(&I::I32TruncSatF64S),
                    (Ty::F64, Ty::I64) => self.emit(&I::I64TruncSatF64S),
                    (Ty::F32, Ty::I32) => self.emit(&I::I32TruncSatF32S),
                    (Ty::F32, Ty::I64) => self.emit(&I::I64TruncSatF32S),
                    (Ty::I32 | Ty::Bool, Ty::I64) => self.emit(&I::I64ExtendI32S),
                    (Ty::U8 | Ty::U16, Ty::I64) => self.emit(&I::I64ExtendI32U),
                    (Ty::I64, Ty::I32) => self.emit(&I::I32WrapI64),
                    (Ty::I32 | Ty::Bool | Ty::U8 | Ty::U16, Ty::I32) | (Ty::I64, Ty::I64) => {}
                    (Ty::I32 | Ty::Bool | Ty::U8 | Ty::U16, Ty::U8 | Ty::U16) => self.narrow(td),
                    (Ty::I64, Ty::U8 | Ty::U16) => {
                        self.emit(&I::I32WrapI64);
                        self.narrow(td);
                    }
                    (Ty::F64, Ty::U8 | Ty::U16) => {
                        self.emit(&I::I32TruncSatF64S);
                        self.narrow(td);
                    }
                    (Ty::F32, Ty::U8 | Ty::U16) => {
                        self.emit(&I::I32TruncSatF32S);
                        self.narrow(td);
                    }
                    (a, b) => return unsupported(format!("ToInt from {a:?} into {b:?}")),
                }
            }
            CastKind::UnsafeCast => {
                if ts != td {
                    return unsupported("an unchecked cast between different kinds");
                }
                self.get(src);
            }
            CastKind::ToVirtual => {
                let dst_type = self.f.values[dst.0 as usize].ty;
                if ts != td || ts != Ty::Ptr {
                    return unsupported("a wrap into a structural type from other than a reference");
                }
                if self.program.shape(dst_type) == Shape::Virtual {
                    self.type_of_value(dst)?;
                    self.get(src);
                    let wrap = self.runtime("hl_to_virtual")?;
                    self.call(wrap);
                } else {
                    self.get(src);
                }
            }
            CastKind::ToDyn => return self.box_dynamic(dst, src),
            CastKind::SafeCast => return self.safe_cast(dst, src),
        }
        self.set(dst);
        Ok(())
    }

    /// A box for the value `src`, of the type `ty`, in `dst`: the runtime
    /// allocates it and the value goes into the payload word.
    fn box_value(&mut self, dst: ValueId, src: ValueId, boxed: Ty, type_name: &str) -> Res<()> {
        let tmp = self.tmp_local;
        self.data_address(type_name, 0)?;
        let alloc = self.runtime("hlp_alloc_dynamic")?;
        self.call(alloc);
        self.emit(&I::LocalSet(tmp));
        self.emit(&I::LocalGet(tmp));
        self.get(src);
        self.store_kind(boxed, VDYNAMIC_VALUE)?;
        self.emit(&I::LocalGet(tmp));
        self.set(dst);
        Ok(())
    }

    fn box_dynamic(&mut self, dst: ValueId, src: ValueId) -> Res<()> {
        let src_type = self.f.values[src.0 as usize].ty;
        if self.ty(dst) != Ty::Ptr {
            return unsupported("a Dynamic held in other than a reference");
        }
        let kind = self.ty(src);
        match kind {
            Ty::Ptr if self.program.shape(src_type) != Shape::Boxed => {
                // A pointer with a type header is its own Dynamic.
                self.get(src);
                self.set(dst);
                Ok(())
            }
            Ty::Ptr => {
                let Some(name) = self.program.type_symbol(src_type.0 as usize) else {
                    return unsupported("a type with no descriptor symbol");
                };
                let tmp = self.tmp_local;
                self.get(src);
                self.emit(&I::I32Eqz);
                self.emit(&I::If(BlockType::Result(ValType::I32)));
                self.emit(&I::I32Const(0));
                self.emit(&I::Else);
                self.data_address(&name, 0)?;
                let alloc = self.runtime("hlp_alloc_dynamic")?;
                self.call(alloc);
                self.emit(&I::LocalSet(tmp));
                self.emit(&I::LocalGet(tmp));
                self.get(src);
                self.store_kind(Ty::Ptr, VDYNAMIC_VALUE)?;
                self.emit(&I::LocalGet(tmp));
                self.emit(&I::End);
                self.set(dst);
                Ok(())
            }
            Ty::Bool | Ty::I32 | Ty::U8 | Ty::U16 | Ty::I64 | Ty::F32 | Ty::F64 => {
                let Some(name) = self.program.type_symbol(src_type.0 as usize) else {
                    return unsupported("a type with no descriptor symbol");
                };
                self.box_value(dst, src, kind, &name)
            }
            Ty::Void | Ty::Other => unsupported("a Dynamic from a value of no storable kind"),
        }
    }

    /// The checked cast: unbox a Dynamic to a number, box a number, or copy.
    /// The forms that hand the runtime a pointer to the value are not done.
    fn safe_cast(&mut self, dst: ValueId, src: ValueId) -> Res<()> {
        let (ts, td) = (self.ty(src), self.ty(dst));
        let src_type = self.f.values[src.0 as usize].ty;
        let dst_type = self.f.values[dst.0 as usize].ty;
        let (src_shape, dst_shape) = (self.program.shape(src_type), self.program.shape(dst_type));
        let numeric =
            |t: Ty| matches!(t, Ty::Bool | Ty::I32 | Ty::U8 | Ty::U16 | Ty::I64 | Ty::F32 | Ty::F64);

        if ts == Ty::Ptr && matches!(src_shape, Shape::Null | Shape::Dyn) && numeric(td) {
            // Unbox through the runtime, which switches on the box's own type;
            // no box is the zero value.
            let (helper, zero) = match td {
                Ty::F64 => ("hlp_dyn_todouble", I::F64Const(0.0f64.into())),
                Ty::F32 => ("hlp_dyn_tofloat", I::F32Const(0.0f32.into())),
                Ty::I64 => ("hlp_dyn_toi64", I::I64Const(0)),
                _ => ("hlp_dyn_toint", I::I32Const(0)),
            };
            let wasm = td.wasm().expect("a numeric kind has a wasm type");
            self.get(src);
            self.emit(&I::I32Eqz);
            self.emit(&I::If(BlockType::Result(wasm)));
            self.emit(&zero);
            self.emit(&I::Else);
            self.get(src);
            let unbox = self.runtime(helper)?;
            self.call(unbox);
            if td == Ty::Bool {
                // Any nonzero byte is true.
                self.emit(&I::I32Const(0));
                self.emit(&I::I32Ne);
            }
            self.narrow(td);
            self.emit(&I::End);
            self.set(dst);
            return Ok(());
        }
        if matches!(dst_shape, Shape::Null | Shape::Dyn) && numeric(ts) {
            // A number into a box, of the type the box holds.
            let box_type = match dst_shape {
                Shape::Null => self.program.null_param(dst_type).unwrap_or(src_type),
                _ => src_type,
            };
            let boxed = self.program.ty(box_type);
            let Some(name) = self.program.type_symbol(box_type.0 as usize) else {
                return unsupported("a type with no descriptor symbol");
            };
            if boxed == ts {
                return self.box_value(dst, src, boxed, &name);
            }
            if boxed == Ty::I64 && matches!(ts, Ty::I32 | Ty::Bool) {
                // Null<Int64> from an Int widens first.
                let tmp = self.tmp_local;
                self.data_address(&name, 0)?;
                let alloc = self.runtime("hlp_alloc_dynamic")?;
                self.call(alloc);
                self.emit(&I::LocalSet(tmp));
                self.emit(&I::LocalGet(tmp));
                self.get(src);
                self.emit(&I::I64ExtendI32S);
                self.store_kind(Ty::I64, VDYNAMIC_VALUE)?;
                self.emit(&I::LocalGet(tmp));
                self.set(dst);
                return Ok(());
            }
            return unsupported("a number boxed as a different kind of number");
        }
        if ts == Ty::Ptr && numeric(td) {
            return self.cast_through_memory(dst, src);
        }
        if ts == Ty::Ptr && matches!(src_shape, Shape::Dyn | Shape::Null) {
            return self.cast_through_memory(dst, src);
        }
        let class_cast = src_type != dst_type
            && matches!(
                (src_shape, dst_shape),
                (Shape::Obj, Shape::Obj)
                    | (Shape::Struct, Shape::Struct)
                    | (Shape::Virtual, Shape::Obj | Shape::Struct | Shape::Virtual)
            );
        if class_cast {
            return self.cast_through_memory(dst, src);
        }
        if ts != td {
            return unsupported("a cast between different kinds");
        }
        self.get(src);
        self.set(dst);
        Ok(())
    }

    /// A closure over function `fun`, binding `obj` as its first argument when
    /// there is one. The function's address is read from the program's table,
    /// where the module that defines it put it.
    fn closure(&mut self, dst: ValueId, fun: usize, obj: Option<ValueId>) -> Res<()> {
        if self.ty(dst) != Ty::Ptr || obj.is_some_and(|o| self.ty(o) != Ty::Ptr) {
            return unsupported("a closure over other than references");
        }
        let Some(fun_type) = self.program.function_type(fun) else {
            return unsupported(format!("a closure over f{fun}, which has no type"));
        };
        let (Some(type_name), Some(table)) =
            (self.program.type_symbol(fun_type), self.program.functions_symbol())
        else {
            return unsupported("a closure in a program with no function table");
        };
        let slot = i32::try_from(fun * 4).map_err(|_| Unsupported("a function past 512 MiB".into()))?;
        self.data_address(&type_name, 0)?;
        self.data_address(&table, slot)?;
        self.emit(&I::I32Load(we::MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        let alloc = match obj {
            Some(o) => {
                self.get(o);
                self.runtime("hlp_alloc_closure_ptr")?
            }
            None => self.runtime("hlp_alloc_closure_void")?,
        };
        self.call(alloc);
        self.set(dst);
        Ok(())
    }

    /// A closure over a method of the object, as the object itself has it:
    /// the class's table names the override, and the function the type names
    /// is the fallback.
    fn virtual_closure(&mut self, dst: ValueId, obj: ValueId, field: usize) -> Res<()> {
        if self.ty(dst) != Ty::Ptr || self.ty(obj) != Ty::Ptr {
            return unsupported("a closure over other than references");
        }
        let obj_ty = self.f.values[obj.0 as usize].ty;
        let Some(fun) = self.program.method_function(obj_ty, field) else {
            return unsupported("a method slot whose function is not known");
        };
        let Some(fun_type) = self.program.function_type(fun) else {
            return unsupported(format!("a closure over f{fun}, which has no type"));
        };
        let (Some(type_name), Some(table)) =
            (self.program.type_symbol(fun_type), self.program.functions_symbol())
        else {
            return unsupported("a closure in a program with no function table");
        };
        let slot = i32::try_from(fun * 4).map_err(|_| Unsupported("a function past 512 MiB".into()))?;
        let tmp = self.tmp_local;
        self.data_address(&type_name, 0)?;
        self.get(obj);
        self.emit(&I::I32Load(we::MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        self.emit(&I::I32Load(we::MemArg {
            offset: TYPE_VOBJ_PROTO,
            align: 2,
            memory_index: 0,
        }));
        self.emit(&I::I32Load(we::MemArg {
            offset: 4 * field as u64,
            align: 2,
            memory_index: 0,
        }));
        self.emit(&I::LocalTee(tmp));
        self.emit(&I::I32Eqz);
        self.emit(&I::If(BlockType::Result(ValType::I32)));
        self.data_address(&table, slot)?;
        self.emit(&I::I32Load(we::MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        self.emit(&I::Else);
        self.emit(&I::LocalGet(tmp));
        self.emit(&I::End);
        self.get(obj);
        let alloc = self.runtime("hlp_alloc_closure_ptr")?;
        self.call(alloc);
        self.set(dst);
        Ok(())
    }

    /// `dst = array.size`, which is 0 for no array.
    fn array_size(&mut self, dst: ValueId, array: ValueId) -> Res<()> {
        if self.ty(array) != Ty::Ptr || self.ty(dst) != Ty::I32 {
            return unsupported("an array size on other than a reference");
        }
        self.get(array);
        self.emit(&I::I32Eqz);
        self.emit(&I::If(BlockType::Result(ValType::I32)));
        self.emit(&I::I32Const(0));
        self.emit(&I::Else);
        self.get(array);
        self.emit(&I::I32Load(we::MemArg {
            offset: VARRAY_SIZE,
            align: 2,
            memory_index: 0,
        }));
        self.emit(&I::End);
        self.set(dst);
        Ok(())
    }

    /// The address of element `index` of the array `base`, less the header.
    fn element_address(&mut self, base: ValueId, index: ValueId, size: u32) {
        self.get(base);
        self.get(index);
        if size != 1 {
            self.emit(&I::I32Const(size as i32));
            self.emit(&I::I32Mul);
        }
        self.emit(&I::I32Add);
    }

    /// Where the access `kind` of `base[index]` lands, as an address on the
    /// stack and the offset to add to it. `elem` is the element's type for an
    /// array access.
    fn access_address(
        &mut self,
        kind: MemAccess,
        base: ValueId,
        index: ValueId,
        elem: ValueId,
    ) -> Res<u64> {
        if self.ty(base) != Ty::Ptr || self.ty(index) != Ty::I32 {
            return unsupported("a memory access with the wrong operand types");
        }
        if kind == MemAccess::Array {
            let t = self.f.values[elem.0 as usize].ty;
            let Some(size) = self.program.array_elem_size(t) else {
                return unsupported("an array of elements with no known size");
            };
            self.element_address(base, index, size);
            Ok(VARRAY_DATA)
        } else {
            // A byte offset from the base.
            self.element_address(base, index, 1);
            Ok(0)
        }
    }

    fn mem_get(&mut self, kind: MemAccess, dst: ValueId, base: ValueId, index: ValueId) -> Res<()> {
        let want = self.ty(dst);
        let at = self.access_address(kind, base, index, dst)?;
        let mem = |align: u32| we::MemArg {
            offset: at,
            align,
            memory_index: 0,
        };
        match kind {
            MemAccess::I8 | MemAccess::I16 => {
                if !matches!(want, Ty::I32 | Ty::U8 | Ty::U16) {
                    return unsupported("a narrow read into other than an integer");
                }
                if kind == MemAccess::I8 {
                    self.emit(&I::I32Load8U(mem(0)));
                } else {
                    self.emit(&I::I32Load16U(mem(1)));
                }
            }
            MemAccess::Mem | MemAccess::Array => self.load_kind(want, at)?,
        }
        self.set(dst);
        Ok(())
    }

    fn mem_set(&mut self, kind: MemAccess, base: ValueId, index: ValueId, src: ValueId) -> Res<()> {
        let value = self.ty(src);
        let at = self.access_address(kind, base, index, src)?;
        match kind {
            MemAccess::I8 | MemAccess::I16 => {
                if !matches!(value, Ty::I32 | Ty::Bool | Ty::U8 | Ty::U16) {
                    return unsupported("a narrow write of other than an integer");
                }
                self.get(src);
                if kind == MemAccess::I8 {
                    self.emit(&I::I32Store8(we::MemArg {
                        offset: at,
                        align: 0,
                        memory_index: 0,
                    }));
                } else {
                    self.emit(&I::I32Store16(we::MemArg {
                        offset: at,
                        align: 1,
                        memory_index: 0,
                    }));
                }
                Ok(())
            }
            MemAccess::Mem | MemAccess::Array => {
                self.get(src);
                self.store_kind(value, at)?;
                self.write_barrier(
                    |fx| {
                        // The address was consumed by the store; build it again.
                        let size = if kind == MemAccess::Array {
                            let t = fx.f.values[src.0 as usize].ty;
                            fx.program.array_elem_size(t).unwrap_or(1)
                        } else {
                            1
                        };
                        fx.element_address(base, index, size);
                        if at != 0 {
                            fx.emit(&I::I32Const(at as i32));
                            fx.emit(&I::I32Add);
                        }
                    },
                    src,
                )
            }
        }
    }

    /// A recognized stdlib operation as the wasm instructions that compute it.
    fn intrinsic(&mut self, kind: IntrinsicKind, dst: ValueId, args: &[ValueId]) -> Res<()> {
        use IntrinsicKind as K;
        if args.len() != kind.arity() {
            return unsupported("an intrinsic with the wrong operand count");
        }
        if kind == K::PtrCompare {
            if args.iter().any(|a| self.ty(*a) != Ty::Ptr) || self.ty(dst) != Ty::I32 {
                return unsupported("a pointer comparison on other than references");
            }
            // (a > b) - (a < b), unsigned: the identity order of the addresses.
            self.get(args[0]);
            self.get(args[1]);
            self.emit(&I::I32GtU);
            self.get(args[0]);
            self.get(args[1]);
            self.emit(&I::I32LtU);
            self.emit(&I::I32Sub);
            self.set(dst);
            return Ok(());
        }
        let x = args[0];
        let result = match kind {
            K::IsNaN | K::IsFinite => Ty::Bool,
            K::FloorToI32 | K::CeilToI32 | K::RoundHalfUpToI32 => Ty::I32,
            _ => Ty::F64,
        };
        if self.ty(x) != Ty::F64 || self.ty(dst) != result {
            return unsupported("an intrinsic on other than doubles");
        }
        self.get(x);
        match kind {
            K::IsNaN => {
                self.get(x);
                self.emit(&I::F64Ne);
            }
            K::IsFinite => {
                self.emit(&I::F64Abs);
                self.emit(&I::F64Const(f64::INFINITY.into()));
                self.emit(&I::F64Lt);
            }
            K::Sqrt => self.emit(&I::F64Sqrt),
            K::Abs => self.emit(&I::F64Abs),
            K::Floor => self.emit(&I::F64Floor),
            K::Ceil => self.emit(&I::F64Ceil),
            K::FloorToI32 => {
                self.emit(&I::F64Floor);
                self.emit(&I::I32TruncSatF64S);
            }
            K::CeilToI32 => {
                self.emit(&I::F64Ceil);
                self.emit(&I::I32TruncSatF64S);
            }
            K::RoundHalfUp | K::RoundHalfUpToI32 => {
                self.emit(&I::F64Const(0.5f64.into()));
                self.emit(&I::F64Add);
                self.emit(&I::F64Floor);
                if kind == K::RoundHalfUpToI32 {
                    self.emit(&I::I32TruncSatF64S);
                }
            }
            K::PtrCompare => unreachable!("handled above"),
        }
        self.set(dst);
        Ok(())
    }

    fn call_instr(&mut self, dst: ValueId, fun: usize, args: &[ValueId]) -> Res<()> {
        if let Some(native) = self.program.native_slot(fun) {
            return self.call_native_slot(dst, fun, args, &native);
        }
        let Some(callee) = (self.callees)(fun) else {
            return unsupported(format!("a call to f{fun}, which has no known signature"));
        };
        if callee.sig.params.len() != args.len() {
            return unsupported(format!("a call to f{fun} with the wrong argument count"));
        }
        for (a, want) in args.iter().zip(&callee.sig.params) {
            if self.ty(*a) != *want {
                return unsupported(format!("a call to f{fun} with a mismatched argument"));
            }
        }
        let ret = callee.sig.ret;
        if ret != Ty::Void && self.ty(dst) != ret {
            return unsupported(format!("a call to f{fun} whose result type differs"));
        }
        for a in args {
            self.get(*a);
        }
        self.call(callee.to);
        self.called.push(fun);
        if ret != Ty::Void {
            // A callee compiled elsewhere need not extend what it returns.
            self.narrow(ret);
            self.set(dst);
        }
        Ok(())
    }

    fn condition(&mut self, c: CondKind, a: ValueId, b: Option<ValueId>) -> Res<()> {
        let t = self.ty(a);
        self.get(a);
        match b {
            Some(b) => {
                if self.ty(b) != t {
                    return unsupported("a comparison of different types");
                }
                self.get(b);
                if t == Ty::Ptr && self.program.dynamic_compare(self.f.values[a.0 as usize].ty) {
                    return self.dynamic_condition(c);
                }
                let (op, negate) = compare(t, c)?;
                self.emit(&op);
                if negate {
                    self.emit(&I::I32Eqz);
                }
            }
            None => match (t, c) {
                (Ty::I32 | Ty::Bool | Ty::Ptr | Ty::U8 | Ty::U16, CondKind::True | CondKind::NotNull) => {}
                (Ty::I32 | Ty::Bool | Ty::Ptr | Ty::U8 | Ty::U16, CondKind::False | CondKind::Null) => {
                    self.emit(&I::I32Eqz)
                }
                (Ty::I64, CondKind::True | CondKind::NotNull) => {
                    self.emit(&I::I64Eqz);
                    self.emit(&I::I32Eqz);
                }
                (Ty::I64, CondKind::False | CondKind::Null) => self.emit(&I::I64Eqz),
                (t, c) => return unsupported(format!("the condition {c:?} on {t:?}")),
            },
        }
        Ok(())
    }

    /// Both references are on the stack. The runtime orders them, and the
    /// condition is a test of that order against zero.
    fn dynamic_condition(&mut self, c: CondKind) -> Res<()> {
        let compare = self.runtime("hlp_dyn_compare")?;
        self.call(compare);
        let (op, ordered) = match c {
            CondKind::Eq => (I::I32Eq, false),
            CondKind::NotEq => (I::I32Ne, false),
            CondKind::SLt => (I::I32LtS, true),
            CondKind::SGte => (I::I32GeS, true),
            CondKind::SGt => (I::I32GtS, true),
            CondKind::SLte => (I::I32LeS, true),
            CondKind::ULt => (I::I32LtU, true),
            CondKind::UGte => (I::I32GeU, true),
            // The inverted tests of Haxe's float comparisons.
            CondKind::NotLt => (I::I32GeS, true),
            CondKind::NotGte => (I::I32LtS, true),
            other => return unsupported(format!("the condition {other:?} on references")),
        };
        if ordered {
            // A pair with no order answers a sentinel that must not pass an
            // ordering test.
            let tmp = self.tmp_local;
            self.emit(&I::LocalTee(tmp));
            self.emit(&I::I32Const(0));
            self.emit(&op);
            self.emit(&I::LocalGet(tmp));
            self.emit(&I::I32Const(self.program.invalid_comparison()));
            self.emit(&I::I32Ne);
            self.emit(&I::I32And);
        } else {
            self.emit(&I::I32Const(0));
            self.emit(&op);
        }
        Ok(())
    }

    fn terminator(&mut self, x: usize) -> Res<()> {
        let f = self.f;
        match &f.blocks[x].term {
            Terminator::Ret { value } => {
                if self.opts.shadow {
                    let pop = self.rt.pop;
                    self.call(pop);
                }
                self.close_frame();
                if self.sig.ret != Ty::Void {
                    if self.ty(*value) != self.sig.ret {
                        return unsupported("a return whose type differs from the signature");
                    }
                    self.get(*value);
                }
                self.emit(&I::Return);
            }
            Terminator::Jump { target } => self.branch(x, target.idx())?,
            Terminator::CondJump {
                cond,
                a,
                b,
                if_true,
                if_false,
            } => {
                self.condition(*cond, *a, *b)?;
                self.emit(&I::If(BlockType::Empty));
                self.ctx.push(Frame::If);
                self.branch(x, if_true.idx())?;
                self.emit(&I::Else);
                self.branch(x, if_false.idx())?;
                self.ctx.pop();
                self.emit(&I::End);
            }
            Terminator::Switch {
                value,
                targets,
                default,
            } => self.switch(x, *value, targets, *default)?,
            Terminator::Throw { exc } => self.throw(x, *exc, false)?,
            Terminator::Rethrow { exc } => self.throw(x, *exc, true)?,
            Terminator::Trap {
                handler, normal, ..
            } => self.arm_trap(x, handler.idx(), normal.idx())?,
        }
        Ok(())
    }

    fn switch(
        &mut self,
        x: usize,
        value: ValueId,
        targets: &[air::v2::ir::BlockId],
        default: air::v2::ir::BlockId,
    ) -> Res<()> {
        if self.ty(value) != Ty::I32 {
            return unsupported("a switch on a non-i32");
        }
        let carries_phis = targets
            .iter()
            .chain(std::iter::once(&default))
            .any(|t| !self.phi_pairs(x, t.idx()).is_empty());
        if !carries_phis {
            let label = |fx: &Self, to: usize| {
                if fx.is_back_edge(x, to) {
                    fx.label(Frame::Loop(to))
                } else {
                    fx.label(Frame::Block(to))
                }
            };
            let labels: Vec<u32> = targets.iter().map(|t| label(self, t.idx())).collect();
            let d = label(self, default.idx());
            self.get(value);
            self.emit(&I::BrTable(labels.into(), d));
            return Ok(());
        }
        // A phi-carrying edge needs its copies, which a br_table cannot run:
        // test the cases one by one instead.
        for (i, t) in targets.iter().enumerate() {
            self.get(value);
            self.emit(&I::I32Const(i as i32));
            self.emit(&I::I32Eq);
            self.emit(&I::If(BlockType::Empty));
            self.ctx.push(Frame::If);
            self.branch(x, t.idx())?;
            self.ctx.pop();
            self.emit(&I::End);
        }
        self.branch(x, default.idx())
    }
}

/// The wasm comparison for `c` on `t`, and whether its result is inverted.
fn compare(t: Ty, c: CondKind) -> Res<(I<'static>, bool)> {
    use CondKind as C;
    let r = match t {
        Ty::Ptr => match c {
            C::Eq => (I::I32Eq, false),
            C::NotEq => (I::I32Ne, false),
            _ => return unsupported(format!("the condition {c:?} on references")),
        },
        Ty::I32 | Ty::Bool | Ty::U8 | Ty::U16 => match c {
            C::Eq => (I::I32Eq, false),
            C::NotEq => (I::I32Ne, false),
            C::SLt => (I::I32LtS, false),
            C::SGte => (I::I32GeS, false),
            C::SGt => (I::I32GtS, false),
            C::SLte => (I::I32LeS, false),
            C::ULt => (I::I32LtU, false),
            C::UGte => (I::I32GeU, false),
            C::NotLt => (I::I32LtS, true),
            C::NotGte => (I::I32GeS, true),
            _ => return unsupported(format!("the condition {c:?} with two operands")),
        },
        Ty::I64 => match c {
            C::Eq => (I::I64Eq, false),
            C::NotEq => (I::I64Ne, false),
            C::SLt => (I::I64LtS, false),
            C::SGte => (I::I64GeS, false),
            C::SGt => (I::I64GtS, false),
            C::SLte => (I::I64LeS, false),
            C::ULt => (I::I64LtU, false),
            C::UGte => (I::I64GeU, false),
            C::NotLt => (I::I64LtS, true),
            C::NotGte => (I::I64GeS, true),
            _ => return unsupported(format!("the condition {c:?} with two operands")),
        },
        Ty::F64 => match c {
            C::Eq => (I::F64Eq, false),
            C::NotEq => (I::F64Ne, false),
            C::SLt | C::ULt => (I::F64Lt, false),
            C::SGte | C::UGte => (I::F64Ge, false),
            C::SGt => (I::F64Gt, false),
            C::SLte => (I::F64Le, false),
            C::NotLt => (I::F64Lt, true),
            C::NotGte => (I::F64Ge, true),
            _ => return unsupported(format!("the condition {c:?} with two operands")),
        },
        Ty::F32 => match c {
            C::Eq => (I::F32Eq, false),
            C::NotEq => (I::F32Ne, false),
            C::SLt | C::ULt => (I::F32Lt, false),
            C::SGte | C::UGte => (I::F32Ge, false),
            C::SGt => (I::F32Gt, false),
            C::SLte => (I::F32Le, false),
            C::NotLt => (I::F32Lt, true),
            C::NotGte => (I::F32Ge, true),
            _ => return unsupported(format!("the condition {c:?} with two operands")),
        },
        Ty::Void | Ty::Other => return unsupported("a comparison of non-numbers"),
    };
    Ok(r)
}
