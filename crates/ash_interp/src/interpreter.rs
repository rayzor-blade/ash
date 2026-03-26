use anyhow::{anyhow, Result};
use std::collections::{HashMap, HashSet};
use std::ffi::c_void;
use std::ffi::CStr;
use std::path::Path;
use std::sync::mpsc::{self, Receiver, Sender, TryRecvError};
use std::thread;

use ash::bytecode::DecodedBytecode;
use ash::c_types::CTypeFactory;
use ash::hl_bindings::{self as hl, _vclosure, hl_runtime_obj, hl_type, hl_type_kind_HSTRUCT};
use ash::jit::module::{CompiledFunctionMeta, JITModule, SharedRuntimeHandles};
use ash::native_lib::NativeFunctionResolver;
use ash::opcodes::{Opcode, Reg};
use ash::types::{HLFunction, ValueTypeKind};
use inkwell::context::Context;

use crate::frame::InterpreterFrame;
use crate::values::{CmpOp, FloatBinOp, IntBinOp, NanBoxedValue};

/// Function pointer types for stdlib functions resolved at runtime.
type FnAllocObj = unsafe extern "C" fn(*mut c_void) -> *mut c_void;
type FnAllocDynObj = unsafe extern "C" fn() -> *mut c_void;
type FnAllocVirtual = unsafe extern "C" fn(*mut c_void) -> *mut c_void;
type FnGetObjRt = unsafe extern "C" fn(*mut c_void) -> *mut c_void;
type FnAllocClosureVoid = unsafe extern "C" fn(*mut c_void, *mut c_void) -> *mut c_void;
type FnDynGetD = unsafe extern "C" fn(*mut c_void, i32) -> f64;
type FnDynGetF = unsafe extern "C" fn(*mut c_void, i32) -> f32;
type FnDynGetI64 = unsafe extern "C" fn(*mut c_void, i32) -> i64;
type FnDynGetI = unsafe extern "C" fn(*mut c_void, i32, *mut c_void) -> i32;
type FnDynGetP = unsafe extern "C" fn(*mut c_void, i32, *mut c_void) -> *mut c_void;
type FnDynSetD = unsafe extern "C" fn(*mut c_void, i32, f64);
type FnDynSetF = unsafe extern "C" fn(*mut c_void, i32, f32);
type FnDynSetI64 = unsafe extern "C" fn(*mut c_void, i32, i64);
type FnDynSetI = unsafe extern "C" fn(*mut c_void, i32, *mut c_void, i32);
type FnDynSetP = unsafe extern "C" fn(*mut c_void, i32, *mut c_void, *mut c_void);
type FnHashGen = unsafe extern "C" fn(*const u16, bool) -> i32;
type FnObjGetField = unsafe extern "C" fn(*mut hl::vdynamic, i32) -> *mut hl::vdynamic;
type FnValueToString = unsafe extern "C" fn(*mut hl::vdynamic, *mut i32) -> *const hl::vbyte;
type FnTypeName = unsafe extern "C" fn(*const hl::hl_type) -> *mut hl::vbyte;

/// Resolve/calculate a HashLink field hash from a bytecode string index.
/// Uses std's hlp_hash_gen when available so field names are cached for reflection/JSON.
fn hash_field_name(
    bytecode: &DecodedBytecode,
    str_idx: usize,
    fn_hash_gen: *mut c_void,
    utf16_cache: &mut HashMap<usize, Vec<u16>>,
    hash_cache: &mut HashMap<usize, i32>,
) -> Result<i32> {
    if let Some(&h) = hash_cache.get(&str_idx) {
        return Ok(h);
    }
    let utf16 = if let Some(cached) = utf16_cache.get(&str_idx) {
        cached.as_ptr()
    } else {
        let s = bytecode
            .strings
            .get(str_idx)
            .ok_or_else(|| anyhow!("Dyn field string out of bounds: {}", str_idx))?;
        let mut buf: Vec<u16> = s.encode_utf16().collect();
        buf.push(0);
        utf16_cache.insert(str_idx, buf);
        utf16_cache[&str_idx].as_ptr()
    };
    let h = if !fn_hash_gen.is_null() {
        let f: FnHashGen = unsafe { std::mem::transmute(fn_hash_gen) };
        unsafe { f(utf16, true) }
    } else {
        let slice = unsafe {
            let mut len = 0;
            while *utf16.add(len) != 0 {
                len += 1;
            }
            std::slice::from_raw_parts(utf16, len)
        };
        let mut h: i32 = 0;
        for c in slice {
            h = h.wrapping_mul(223).wrapping_add(*c as i32);
        }
        h.wrapping_rem(0x1FFFFF7B)
    };
    hash_cache.insert(str_idx, h);
    Ok(h)
}

#[inline]
unsafe fn call_setjmp_opaque(jmp_buf: *mut c_void) -> i32 {
    type SetJmpOpaque = unsafe extern "C" fn(*mut c_void) -> i32;
    let setjmp_fn: SetJmpOpaque = std::mem::transmute(hl::_setjmp as *const () as usize);
    setjmp_fn(jmp_buf)
}

/// Result of executing a single opcode.
enum StepResult {
    /// Continue to next opcode (pc already incremented)
    Continue,
    /// Jump to a relative offset from current pc
    Jump(i32),
    /// Jump to an absolute opcode index (used for exception handler entry)
    JumpAbs(usize),
    /// Return a value from the current function
    Return(NanBoxedValue),
    /// Call a function by findex, with arguments and destination register
    Call {
        findex: usize,
        args: Vec<NanBoxedValue>,
        dst: u32,
    },
}

/// Carries a thrown HL exception value up through the Rust call stack.
/// Distinguishable from other errors so callers can catch it via downcast.
#[derive(Debug, Clone)]
struct HLExceptionPropagation {
    value: NanBoxedValue,
    message: Option<String>,
}

impl std::fmt::Display for HLExceptionPropagation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(msg) = &self.message {
            write!(f, "HL exception: {}", msg)
        } else {
            write!(f, "HL exception: {:?}", self.value)
        }
    }
}

impl std::error::Error for HLExceptionPropagation {}

#[derive(Debug, Clone)]
pub struct TieredConfig {
    pub enabled: bool,
    pub jit_threshold: u64,
    pub max_jit_args: usize,
    pub min_ops_for_promotion: usize,
    pub log_promotions: bool,
    pub strict_mode: bool,
}

impl Default for TieredConfig {
    fn default() -> Self {
        Self {
            enabled: false,
            jit_threshold: 100,
            max_jit_args: 8,
            // 0 disables the static opcode-size gate; promotion hotness is call-count based.
            min_ops_for_promotion: 0,
            log_promotions: false,
            strict_mode: true,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct TieredStats {
    pub attempted_promotions: u64,
    pub successful_promotions: u64,
    pub failed_promotions: u64,
    pub compiled_calls: u64,
    pub fallback_calls: u64,
}

#[derive(Debug, Clone)]
struct CompiledFunctionEntry {
    fn_addr: usize,
    arg_kinds: Vec<u32>,
    ret_kind: u32,
}

#[derive(Debug)]
struct PromotionResult {
    findex: usize,
    result: std::result::Result<CompiledFunctionMeta, String>,
}

struct TieredRuntime {
    config: TieredConfig,
    compiled: HashMap<usize, CompiledFunctionEntry>,
    blacklist: HashMap<usize, String>,
    queued: HashSet<usize>,
    worker_tx: Sender<usize>,
    worker_rx: Receiver<PromotionResult>,
    stats: TieredStats,
}

/// Hybrid HashLink bytecode interpreter with JIT promotion support.
///
/// Executes HL bytecode directly using a register-based architecture
/// with NaN-boxed values. Tracks per-function call counts and signals
/// when a function should be promoted to JIT compilation.
pub struct HLInterpreter {
    /// Global variable store (indexed by global index)
    pub globals: Vec<NanBoxedValue>,
    /// Call stack
    stack: Vec<InterpreterFrame>,
    /// Maximum call stack depth
    max_stack_depth: usize,
    /// Per-function call counts for JIT promotion
    call_counts: HashMap<usize, u64>,
    /// Call count threshold before signaling JIT promotion
    pub jit_threshold: u64,
    /// Functions that have been flagged for JIT promotion
    pub jit_candidates: Vec<usize>,
    /// Map from findex → function array index (for bytecode functions)
    findex_to_func: HashMap<usize, usize>,
    /// Map from findex → native array index
    findex_to_native: HashMap<usize, usize>,
    /// C-level type structures for native function interop
    c_type_factory: CTypeFactory,
    /// Resolved stdlib function pointer: hlp_alloc_obj
    fn_alloc_obj: *mut c_void,
    /// Resolved stdlib function pointer: hlp_get_obj_rt
    fn_get_obj_rt: *mut c_void,
    /// Resolved stdlib function pointer: hlp_make_dyn
    fn_make_dyn: *mut c_void,
    /// Resolved stdlib function pointer: hlp_alloc_enum
    fn_alloc_enum: *mut c_void,
    /// Resolved stdlib function pointer: hlp_alloc_dynobj
    fn_alloc_dynobj: *mut c_void,
    /// Resolved stdlib function pointer: hlp_alloc_virtual
    fn_alloc_virtual: *mut c_void,
    /// Resolved stdlib function pointer: hlp_alloc_closure_void
    fn_alloc_closure_void: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_getd
    fn_dyn_getd: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_getf
    fn_dyn_getf: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_geti64
    fn_dyn_geti64: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_geti
    fn_dyn_geti: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_getp
    fn_dyn_getp: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_setd
    fn_dyn_setd: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_setf
    fn_dyn_setf: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_seti64
    fn_dyn_seti64: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_seti
    fn_dyn_seti: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_setp
    fn_dyn_setp: *mut c_void,
    /// Resolved stdlib function pointer: hlp_hash_gen
    fn_hash_gen: *mut c_void,
    /// Resolved stdlib function pointer: hlp_setup_trap_jit (setjmp trap for native throws)
    fn_setup_trap_jit: *mut c_void,
    /// Resolved stdlib function pointer: hlp_remove_trap_jit (pop native trap after success)
    fn_remove_trap_jit: *mut c_void,
    /// Resolved stdlib function pointer: hlp_get_exc_value (interpreter exception recovery)
    fn_get_exc_value: *mut c_void,
    /// Resolved stdlib function pointer: hlp_clear_exc_value (clears exc_value after recovery)
    fn_clear_exc_value: *mut c_void,
    /// Resolved stdlib function pointer: hlp_obj_get_field (reads dynamic object fields)
    fn_obj_get_field: *mut c_void,
    /// Resolved stdlib function pointer: hlp_value_to_string (for readable exception messages)
    fn_value_to_string: *mut c_void,
    /// Resolved stdlib function pointer: hlp_type_name (runtime type name lookup)
    fn_type_name: *mut c_void,
    /// Resolved stdlib function pointer: hlp_gc_clear_scan_roots
    fn_gc_clear_scan_roots: *mut c_void,
    /// Resolved stdlib function pointer: hlp_gc_add_scan_root
    fn_gc_add_scan_root: *mut c_void,
    /// Resolved stdlib function pointer: hlp_gc_set_stack_top
    fn_gc_set_stack_top: *mut c_void,
    /// Resolved stdlib function pointer: hlp_gc_set_globals
    fn_gc_set_globals: *mut c_void,
    /// Whether GC globals/stack top were initialized for this interpreter.
    gc_runtime_initialized: bool,
    /// Scratch space for decoded raw pointer roots (from NaN-boxed registers).
    gc_root_ptrs: Vec<usize>,
    /// Cache of UTF-16 null-terminated strings (string index → owned buffer).
    /// HashLink uses UTF-16 internally; bytecode strings are stored as UTF-8 in Rust.
    utf16_strings: HashMap<usize, Vec<u16>>,
    /// Cache of field name hashes (string index → hash value).
    field_hash_cache: HashMap<usize, i32>,
    /// Fallback storage for HVIRTUAL fields when runtime virtual indexes are unavailable.
    virtual_fields: HashMap<(usize, usize), NanBoxedValue>,
    /// Hash-keyed fallback storage for HVIRTUAL dynamic field access via hl.Api/Reflect.
    virtual_hash_fields: HashMap<(usize, i32), NanBoxedValue>,
    /// Optional tiered runtime (hybrid mode).
    tiered_runtime: Option<TieredRuntime>,
}

impl HLInterpreter {
    pub fn new(bytecode: &DecodedBytecode, native_resolver: &NativeFunctionResolver) -> Self {
        // Build findex lookup tables
        let mut findex_to_func = HashMap::new();
        for (i, f) in bytecode.functions.iter().enumerate() {
            findex_to_func.insert(f.findex as usize, i);
        }
        let mut findex_to_native = HashMap::new();
        for (i, n) in bytecode.natives.iter().enumerate() {
            findex_to_native.insert(n.findex as usize, i);
        }

        // Initialize globals
        let globals = vec![NanBoxedValue::null(); bytecode.globals.len()];

        // Create C-level type structures for native interop
        let c_type_factory = CTypeFactory::new(bytecode);

        // Resolve internal stdlib function pointers for object operations
        let fn_alloc_obj = native_resolver
            .resolve_function("std", "hlp_alloc_obj")
            .unwrap_or(std::ptr::null_mut());
        let fn_get_obj_rt = native_resolver
            .resolve_function("std", "hlp_get_obj_rt")
            .unwrap_or(std::ptr::null_mut());
        let fn_make_dyn = native_resolver
            .resolve_function("std", "hlp_make_dyn")
            .unwrap_or(std::ptr::null_mut());
        let fn_alloc_enum = native_resolver
            .resolve_function("std", "hlp_alloc_enum")
            .unwrap_or(std::ptr::null_mut());
        let fn_alloc_dynobj = native_resolver
            .resolve_function("std", "hlp_alloc_dynobj")
            .unwrap_or(std::ptr::null_mut());
        let fn_alloc_virtual = native_resolver
            .resolve_function("std", "hlp_alloc_virtual")
            .unwrap_or(std::ptr::null_mut());
        let fn_alloc_closure_void = native_resolver
            .resolve_function("std", "hlp_alloc_closure_void")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_getd = native_resolver
            .resolve_function("std", "hlp_dyn_getd")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_getf = native_resolver
            .resolve_function("std", "hlp_dyn_getf")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_geti64 = native_resolver
            .resolve_function("std", "hlp_dyn_geti64")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_geti = native_resolver
            .resolve_function("std", "hlp_dyn_geti")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_getp = native_resolver
            .resolve_function("std", "hlp_dyn_getp")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_setd = native_resolver
            .resolve_function("std", "hlp_dyn_setd")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_setf = native_resolver
            .resolve_function("std", "hlp_dyn_setf")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_seti64 = native_resolver
            .resolve_function("std", "hlp_dyn_seti64")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_seti = native_resolver
            .resolve_function("std", "hlp_dyn_seti")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_setp = native_resolver
            .resolve_function("std", "hlp_dyn_setp")
            .unwrap_or(std::ptr::null_mut());
        let fn_hash_gen = native_resolver
            .resolve_function("std", "hlp_hash_gen")
            .unwrap_or(std::ptr::null_mut());
        let fn_setup_trap_jit = native_resolver
            .resolve_function("std", "hlp_setup_trap_jit")
            .unwrap_or(std::ptr::null_mut());
        let fn_remove_trap_jit = native_resolver
            .resolve_function("std", "hlp_remove_trap_jit")
            .unwrap_or(std::ptr::null_mut());
        let fn_get_exc_value = native_resolver
            .resolve_function("std", "hlp_get_exc_value")
            .unwrap_or(std::ptr::null_mut());
        let fn_clear_exc_value = native_resolver
            .resolve_function("std", "hlp_clear_exc_value")
            .unwrap_or(std::ptr::null_mut());
        let fn_obj_get_field = native_resolver
            .resolve_function("std", "hlp_obj_get_field")
            .unwrap_or(std::ptr::null_mut());
        let fn_value_to_string = native_resolver
            .resolve_function("std", "hlp_value_to_string")
            .unwrap_or(std::ptr::null_mut());
        let fn_type_name = native_resolver
            .resolve_function("std", "hlp_type_name")
            .unwrap_or(std::ptr::null_mut());
        let fn_gc_clear_scan_roots = native_resolver
            .resolve_function("std", "hlp_gc_clear_scan_roots")
            .unwrap_or(std::ptr::null_mut());
        let fn_gc_add_scan_root = native_resolver
            .resolve_function("std", "hlp_gc_add_scan_root")
            .unwrap_or(std::ptr::null_mut());
        let fn_gc_set_stack_top = native_resolver
            .resolve_function("std", "hlp_gc_set_stack_top")
            .unwrap_or(std::ptr::null_mut());
        let fn_gc_set_globals = native_resolver
            .resolve_function("std", "hlp_gc_set_globals")
            .unwrap_or(std::ptr::null_mut());
        HLInterpreter {
            globals,
            stack: Vec::with_capacity(64),
            max_stack_depth: 1000,
            call_counts: HashMap::new(),
            jit_threshold: 100,
            jit_candidates: Vec::new(),
            findex_to_func,
            findex_to_native,
            c_type_factory,
            fn_alloc_obj,
            fn_get_obj_rt,
            fn_make_dyn,
            fn_alloc_enum,
            fn_alloc_dynobj,
            fn_alloc_virtual,
            fn_alloc_closure_void,
            fn_dyn_getd,
            fn_dyn_getf,
            fn_dyn_geti64,
            fn_dyn_geti,
            fn_dyn_getp,
            fn_dyn_setd,
            fn_dyn_setf,
            fn_dyn_seti64,
            fn_dyn_seti,
            fn_dyn_setp,
            fn_hash_gen,
            fn_setup_trap_jit,
            fn_remove_trap_jit,
            fn_get_exc_value,
            fn_clear_exc_value,
            fn_obj_get_field,
            fn_value_to_string,
            fn_type_name,
            fn_gc_clear_scan_roots,
            fn_gc_add_scan_root,
            fn_gc_set_stack_top,
            fn_gc_set_globals,
            gc_runtime_initialized: false,
            gc_root_ptrs: Vec::new(),
            utf16_strings: HashMap::new(),
            field_hash_cache: HashMap::new(),
            virtual_fields: HashMap::new(),
            virtual_hash_fields: HashMap::new(),
            tiered_runtime: None,
        }
    }

    pub fn enable_tiered(
        &mut self,
        hl_path: &Path,
        _native_resolver: &NativeFunctionResolver,
        mut config: TieredConfig,
    ) -> Result<()> {
        config.enabled = true;
        self.jit_threshold = config.jit_threshold;

        let log_promotions = config.log_promotions;
        let hl_path = hl_path.to_path_buf();
        let (globals_data_ptr, nglobals) = self.c_type_factory.globals_data();
        let shared = SharedRuntimeHandles {
            globals_data_ptr,
            nglobals,
            c_types: self.c_type_factory.as_slice().to_vec(),
            module_ctx: self.c_type_factory.module_ctx(),
        };
        let (worker_tx, worker_req_rx) = mpsc::channel::<usize>();
        let (worker_res_tx, worker_rx) = mpsc::channel::<PromotionResult>();

        let _ = thread::Builder::new()
            .name("ash-jit-worker".to_string())
            .spawn(move || {
                let mut module: Option<JITModule<'static>> = None;
                for findex in worker_req_rx {
                    if module.is_none() {
                        let init_result =
                            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                                let context = Box::leak(Box::new(Context::create()));
                                JITModule::new_with_shared_runtime(
                                    context,
                                    &hl_path,
                                    shared.clone(),
                                )
                            }));
                        match init_result {
                            Ok(m) => {
                                if log_promotions {
                                    eprintln!("[tiered] initialized shared JIT module");
                                }
                                module = Some(m);
                            }
                            Err(_) => {
                                let _ = worker_res_tx.send(PromotionResult {
                                    findex,
                                    result: Err("tiered worker init panicked".to_string()),
                                });
                                continue;
                            }
                        }
                    }

                    let compile_result =
                        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                            module
                                .as_mut()
                                .expect("JIT module should be initialized")
                                .promote_function_strict(findex)
                        }));
                    let result = match compile_result {
                        Ok(Ok(meta)) => Ok(meta),
                        Ok(Err(e)) => Err(e.to_string()),
                        Err(_) => Err("promotion panicked".to_string()),
                    };
                    let _ = worker_res_tx.send(PromotionResult { findex, result });
                }
                // LLVM objects may throw foreign exceptions during drop on some platforms.
                // Leak the JIT module on worker shutdown to keep process termination stable.
                if let Some(m) = module.take() {
                    std::mem::forget(m);
                }
            });

        self.tiered_runtime = Some(TieredRuntime {
            config,
            compiled: HashMap::new(),
            blacklist: HashMap::new(),
            queued: HashSet::new(),
            worker_tx,
            worker_rx,
            stats: TieredStats::default(),
        });
        Ok(())
    }

    pub fn tiered_stats(&self) -> Option<&TieredStats> {
        self.tiered_runtime.as_ref().map(|t| &t.stats)
    }

    #[inline(always)]
    fn current_stack_addr() -> usize {
        // Portable stack probe: address of a local variable approximates current SP.
        let marker = 0u8;
        (&marker as *const u8) as usize
    }

    fn ensure_gc_runtime_initialized(&mut self) {
        if self.gc_runtime_initialized {
            return;
        }
        if self.fn_gc_set_globals.is_null() || self.fn_gc_set_stack_top.is_null() {
            return;
        }
        let (globals_ptr, globals_len) = self.c_type_factory.globals_data();
        if globals_ptr.is_null() {
            return;
        }
        type FnSetGlobals = unsafe extern "C" fn(*const *mut c_void, usize);
        type FnSetStackTop = unsafe extern "C" fn(usize);
        let set_globals: FnSetGlobals = unsafe { std::mem::transmute(self.fn_gc_set_globals) };
        let set_stack_top: FnSetStackTop = unsafe { std::mem::transmute(self.fn_gc_set_stack_top) };
        unsafe {
            set_globals(globals_ptr as *const *mut c_void, globals_len);
            set_stack_top(Self::current_stack_addr());
        }
        self.gc_runtime_initialized = true;
    }

    /// Publish interpreter register memory as conservative GC scan ranges.
    /// This keeps live values held in bytecode registers visible to the std GC.
    fn sync_gc_scan_roots(&mut self) {
        if self.fn_gc_clear_scan_roots.is_null() || self.fn_gc_add_scan_root.is_null() {
            return;
        }
        type FnClear = unsafe extern "C" fn();
        type FnAdd = unsafe extern "C" fn(*const c_void, usize);
        let clear: FnClear = unsafe { std::mem::transmute(self.fn_gc_clear_scan_roots) };
        let add: FnAdd = unsafe { std::mem::transmute(self.fn_gc_add_scan_root) };
        unsafe { clear() };
        self.gc_root_ptrs.clear();
        for frame in &self.stack {
            for v in frame.registers.as_slice() {
                if v.is_ptr() && !v.is_null() {
                    self.gc_root_ptrs.push(v.as_ptr());
                }
            }
        }
        if !self.gc_root_ptrs.is_empty() {
            let ptr = self.gc_root_ptrs.as_ptr() as *const c_void;
            let size = self.gc_root_ptrs.len() * std::mem::size_of::<usize>();
            unsafe { add(ptr, size) };
        }
    }

    fn decode_utf16_chars(ptr: *const u16, len: i32) -> String {
        if ptr.is_null() || len <= 0 {
            return String::new();
        }
        let slice = unsafe { std::slice::from_raw_parts(ptr, len as usize) };
        String::from_utf16_lossy(slice)
    }

    fn value_to_string(&self, dyn_ptr: *mut hl::vdynamic) -> Option<String> {
        if self.fn_value_to_string.is_null() || dyn_ptr.is_null() {
            return None;
        }
        let f: FnValueToString = unsafe { std::mem::transmute(self.fn_value_to_string) };
        let mut len: i32 = 0;
        let out = unsafe { f(dyn_ptr, &mut len as *mut i32) };
        if out.is_null() || len <= 0 {
            None
        } else {
            Some(Self::decode_utf16_chars(out as *const u16, len))
        }
    }

    fn hash_literal_name(&self, name: &str) -> i32 {
        let mut utf16: Vec<u16> = name.encode_utf16().collect();
        utf16.push(0);
        if !self.fn_hash_gen.is_null() {
            let f: FnHashGen = unsafe { std::mem::transmute(self.fn_hash_gen) };
            return unsafe { f(utf16.as_ptr(), true) };
        }
        let mut h: i32 = 0;
        for c in &utf16[..utf16.len() - 1] {
            h = h.wrapping_mul(223).wrapping_add(*c as i32);
        }
        h.wrapping_rem(0x1FFFFF7B)
    }

    fn resolve_typed_field_hash(
        bytecode: &DecodedBytecode,
        obj_type_idx: usize,
        field_idx: usize,
    ) -> Option<i32> {
        let ty = bytecode.types.get(obj_type_idx)?;
        if let Some(obj) = ty.obj.as_ref() {
            if let Some(f) = obj.fields.get(field_idx) {
                if f.hashed_name != 0 {
                    return Some(f.hashed_name);
                }
            }
        }
        if let Some(virt) = ty.virt.as_ref() {
            if let Some(f) = virt.fields.get(field_idx) {
                if f.hashed_name != 0 {
                    return Some(f.hashed_name);
                }
            }
        }
        None
    }

    unsafe fn resolve_virtual_field_offset(
        c_type_ptr: *mut c_void,
        field_idx: usize,
    ) -> Option<usize> {
        if c_type_ptr.is_null() {
            return None;
        }
        let t = c_type_ptr as *mut hl_type;
        if t.is_null() || (*t).kind != hl::hl_type_kind_HVIRTUAL {
            return None;
        }
        let virt = (*t).__bindgen_anon_1.virt;
        if virt.is_null() || (*virt).indexes.is_null() || field_idx >= (*virt).nfields as usize {
            return None;
        }
        let off = *(*virt).indexes.add(field_idx);
        if off < 0 {
            None
        } else {
            Some(off as usize)
        }
    }

    unsafe fn resolve_virtual_field_index_and_type(
        obj_ptr: *mut c_void,
        hfield: i32,
    ) -> Option<(usize, *mut hl_type)> {
        if obj_ptr.is_null() {
            return None;
        }
        let t = *(obj_ptr as *const *mut hl_type);
        if t.is_null() || (*t).kind != hl::hl_type_kind_HVIRTUAL {
            return None;
        }
        let virt = (*t).__bindgen_anon_1.virt;
        if virt.is_null() || (*virt).fields.is_null() {
            return None;
        }
        for i in 0..(*virt).nfields as usize {
            let f = &*(*virt).fields.add(i);
            if f.hashed_name == hfield {
                return Some((i, f.t));
            }
        }
        None
    }

    #[inline]
    fn is_primitive_or_bytes_kind(kind: u32) -> bool {
        matches!(
            kind,
            hl::hl_type_kind_HI32
                | hl::hl_type_kind_HUI8
                | hl::hl_type_kind_HUI16
                | hl::hl_type_kind_HI64
                | hl::hl_type_kind_HF32
                | hl::hl_type_kind_HF64
                | hl::hl_type_kind_HBOOL
                | hl::hl_type_kind_HBYTES
        )
    }

    #[inline]
    fn is_numeric_or_bool_kind(kind: u32) -> bool {
        matches!(
            kind,
            hl::hl_type_kind_HI32
                | hl::hl_type_kind_HUI8
                | hl::hl_type_kind_HUI16
                | hl::hl_type_kind_HI64
                | hl::hl_type_kind_HF32
                | hl::hl_type_kind_HF64
                | hl::hl_type_kind_HBOOL
        )
    }

    fn box_value_as_dynamic_with_type(
        &self,
        val: NanBoxedValue,
        field_t: *mut hl_type,
    ) -> NanBoxedValue {
        if val.is_null() || val.is_void() {
            return NanBoxedValue::null();
        }
        if field_t.is_null() {
            return if val.is_ptr() {
                NanBoxedValue::from_ptr(val.as_ptr())
            } else {
                NanBoxedValue::null()
            };
        }
        let kind = unsafe { (*field_t).kind };
        if !Self::is_primitive_or_bytes_kind(kind) {
            return NanBoxedValue::from_ptr(val.as_ptr());
        }
        if self.fn_make_dyn.is_null() {
            return val;
        }
        let mut data: i64 = match kind {
            hl::hl_type_kind_HI32 | hl::hl_type_kind_HUI8 | hl::hl_type_kind_HUI16 => {
                val.as_i32() as i64
            }
            hl::hl_type_kind_HI64 => val.as_i64_lossy(),
            hl::hl_type_kind_HF32 | hl::hl_type_kind_HF64 => val.as_f64().to_bits() as i64,
            hl::hl_type_kind_HBOOL => {
                if val.as_bool() {
                    1
                } else {
                    0
                }
            }
            hl::hl_type_kind_HBYTES => val.as_ptr() as i64,
            _ => val.as_ptr() as i64,
        };
        let make_dyn: unsafe extern "C" fn(*mut c_void, *mut c_void) -> *mut c_void =
            unsafe { std::mem::transmute(self.fn_make_dyn) };
        let dyn_ptr =
            unsafe { make_dyn(&mut data as *mut i64 as *mut c_void, field_t as *mut c_void) };
        if dyn_ptr.is_null() {
            NanBoxedValue::null()
        } else {
            NanBoxedValue::from_ptr(dyn_ptr as usize)
        }
    }

    #[inline]
    fn coerce_value_for_static_kind(&self, val: NanBoxedValue, dst_kind: u32) -> NanBoxedValue {
        if val.is_ptr()
            && !val.is_null()
            && val.as_ptr() != 0
            && Self::is_primitive_or_bytes_kind(dst_kind)
        {
            return unsafe {
                Self::unbox_dynamic_to_kind(val.as_ptr() as *mut hl::vdynamic, dst_kind)
                    .unwrap_or(val)
            };
        }
        if val.is_null() {
            return match dst_kind {
                hl::hl_type_kind_HI32 | hl::hl_type_kind_HUI8 | hl::hl_type_kind_HUI16 => {
                    NanBoxedValue::from_i32(0)
                }
                hl::hl_type_kind_HI64 => NanBoxedValue::from_i64(0),
                hl::hl_type_kind_HF32 | hl::hl_type_kind_HF64 => NanBoxedValue::from_f64(0.0),
                hl::hl_type_kind_HBOOL => NanBoxedValue::from_bool(false),
                _ => val,
            };
        }
        val
    }

    fn try_handle_virtual_obj_get_field(
        &mut self,
        args: &[NanBoxedValue],
    ) -> Option<NanBoxedValue> {
        if args.len() < 2 {
            return Some(NanBoxedValue::null());
        }
        let obj = args[0];
        if obj.is_null() || obj.is_void() {
            return Some(NanBoxedValue::null());
        }
        let obj_ptr = obj.as_ptr();
        if obj_ptr == 0 {
            return Some(NanBoxedValue::null());
        }
        let hfield = args[1].as_i32();
        let meta =
            unsafe { Self::resolve_virtual_field_index_and_type(obj_ptr as *mut c_void, hfield) };
        if meta.is_none()
            && unsafe {
                let t = *(obj_ptr as *const *mut hl_type);
                t.is_null() || (*t).kind != hl::hl_type_kind_HVIRTUAL
            }
        {
            return None;
        }
        let found = self
            .virtual_fields
            .get(&(obj_ptr, meta.map_or(usize::MAX, |(idx, _)| idx)))
            .copied()
            .or_else(|| self.virtual_hash_fields.get(&(obj_ptr, hfield)).copied());
        match found {
            Some(val) if !val.is_null() && !val.is_void() => {
                if val.is_ptr() {
                    return Some(NanBoxedValue::from_ptr(val.as_ptr()));
                }
                let field_t = meta.map_or(std::ptr::null_mut(), |(_, ft)| ft);
                Some(self.box_value_as_dynamic_with_type(val, field_t))
            }
            _ => {
                // Field not in interpreter maps — fall through to native hlp_obj_get_field
                // which can read from the virtual object's backing memory.
                None
            }
        }
    }

    fn try_handle_virtual_obj_set_field(
        &mut self,
        args: &[NanBoxedValue],
    ) -> Option<NanBoxedValue> {
        if args.len() < 3 {
            return Some(NanBoxedValue::void());
        }
        let obj = args[0];
        if obj.is_null() || obj.is_void() {
            return Some(NanBoxedValue::void());
        }
        let obj_ptr = obj.as_ptr();
        if obj_ptr == 0 {
            return Some(NanBoxedValue::void());
        }
        let hfield = args[1].as_i32();
        let src_val = args[2];
        let meta =
            unsafe { Self::resolve_virtual_field_index_and_type(obj_ptr as *mut c_void, hfield) };
        if meta.is_none()
            && unsafe {
                let t = *(obj_ptr as *const *mut hl_type);
                t.is_null() || (*t).kind != hl::hl_type_kind_HVIRTUAL
            }
        {
            return None;
        }

        match meta {
            Some((idx, field_t)) => {
                let stored = if src_val.is_null() || src_val.is_void() {
                    NanBoxedValue::null()
                } else {
                    let kind = unsafe { (*field_t).kind };
                    if Self::is_primitive_or_bytes_kind(kind) && src_val.is_ptr() {
                        unsafe {
                            Self::unbox_dynamic_to_kind(src_val.as_ptr() as *mut hl::vdynamic, kind)
                                .unwrap_or(src_val)
                        }
                    } else {
                        src_val
                    }
                };
                self.virtual_fields.insert((obj_ptr, idx), stored);
                self.virtual_hash_fields.insert((obj_ptr, hfield), stored);
            }
            None => {
                self.virtual_hash_fields.insert((obj_ptr, hfield), src_val);
            }
        }
        Some(NanBoxedValue::void())
    }

    fn try_handle_virtual_obj_has_field(
        &mut self,
        args: &[NanBoxedValue],
    ) -> Option<NanBoxedValue> {
        if args.len() < 2 {
            return Some(NanBoxedValue::from_bool(false));
        }
        let obj = args[0];
        if obj.is_null() || obj.is_void() {
            return Some(NanBoxedValue::from_bool(false));
        }
        let obj_ptr = obj.as_ptr();
        if obj_ptr == 0 {
            return Some(NanBoxedValue::from_bool(false));
        }
        let hfield = args[1].as_i32();
        let meta =
            unsafe { Self::resolve_virtual_field_index_and_type(obj_ptr as *mut c_void, hfield) };
        if meta.is_none()
            && unsafe {
                let t = *(obj_ptr as *const *mut hl_type);
                t.is_null() || (*t).kind != hl::hl_type_kind_HVIRTUAL
            }
        {
            return None;
        }
        let found = match meta {
            Some((idx, _)) => {
                self.virtual_fields.contains_key(&(obj_ptr, idx))
                    || self.virtual_hash_fields.contains_key(&(obj_ptr, hfield))
            }
            None => self.virtual_hash_fields.contains_key(&(obj_ptr, hfield)),
        };
        Some(NanBoxedValue::from_bool(found))
    }

    fn try_handle_virtual_obj_delete_field(
        &mut self,
        args: &[NanBoxedValue],
    ) -> Option<NanBoxedValue> {
        if args.len() < 2 {
            return Some(NanBoxedValue::from_bool(false));
        }
        let obj = args[0];
        if obj.is_null() || obj.is_void() {
            return Some(NanBoxedValue::from_bool(false));
        }
        let obj_ptr = obj.as_ptr();
        if obj_ptr == 0 {
            return Some(NanBoxedValue::from_bool(false));
        }
        let hfield = args[1].as_i32();
        let meta =
            unsafe { Self::resolve_virtual_field_index_and_type(obj_ptr as *mut c_void, hfield) };
        if meta.is_none()
            && unsafe {
                let t = *(obj_ptr as *const *mut hl_type);
                t.is_null() || (*t).kind != hl::hl_type_kind_HVIRTUAL
            }
        {
            return None;
        }
        let removed = match meta {
            Some((idx, _)) => {
                self.virtual_fields.remove(&(obj_ptr, idx)).is_some()
                    || self
                        .virtual_hash_fields
                        .remove(&(obj_ptr, hfield))
                        .is_some()
            }
            None => self
                .virtual_hash_fields
                .remove(&(obj_ptr, hfield))
                .is_some(),
        };
        Some(NanBoxedValue::from_bool(removed))
    }

    #[allow(clippy::too_many_arguments)]
    fn dyn_get_field_by_hash(
        obj_ptr: *mut c_void,
        hfield: i32,
        dst_kind: u32,
        dst_type_ptr: *mut c_void,
        fn_dyn_getd: *mut c_void,
        fn_dyn_getf: *mut c_void,
        fn_dyn_geti64: *mut c_void,
        fn_dyn_geti: *mut c_void,
        fn_dyn_getp: *mut c_void,
    ) -> NanBoxedValue {
        match dst_kind {
            hl::hl_type_kind_HF64 => {
                if fn_dyn_getd.is_null() {
                    NanBoxedValue::null()
                } else {
                    let f: FnDynGetD = unsafe { std::mem::transmute(fn_dyn_getd) };
                    NanBoxedValue::from_f64(unsafe { f(obj_ptr, hfield) })
                }
            }
            hl::hl_type_kind_HF32 => {
                if fn_dyn_getf.is_null() {
                    NanBoxedValue::null()
                } else {
                    let f: FnDynGetF = unsafe { std::mem::transmute(fn_dyn_getf) };
                    NanBoxedValue::from_f64(unsafe { f(obj_ptr, hfield) as f64 })
                }
            }
            hl::hl_type_kind_HI64 => {
                if fn_dyn_geti64.is_null() {
                    NanBoxedValue::null()
                } else {
                    let f: FnDynGetI64 = unsafe { std::mem::transmute(fn_dyn_geti64) };
                    NanBoxedValue::from_i64(unsafe { f(obj_ptr, hfield) })
                }
            }
            hl::hl_type_kind_HI32
            | hl::hl_type_kind_HBOOL
            | hl::hl_type_kind_HUI8
            | hl::hl_type_kind_HUI16 => {
                if fn_dyn_geti.is_null() {
                    NanBoxedValue::null()
                } else {
                    let f: FnDynGetI = unsafe { std::mem::transmute(fn_dyn_geti) };
                    let i = unsafe { f(obj_ptr, hfield, dst_type_ptr) };
                    if dst_kind == hl::hl_type_kind_HBOOL {
                        NanBoxedValue::from_bool(i != 0)
                    } else {
                        NanBoxedValue::from_i32(i)
                    }
                }
            }
            _ => {
                if fn_dyn_getp.is_null() {
                    NanBoxedValue::null()
                } else {
                    let f: FnDynGetP = unsafe { std::mem::transmute(fn_dyn_getp) };
                    let p = unsafe { f(obj_ptr, hfield, dst_type_ptr) };
                    if p.is_null() {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(p as usize)
                    }
                }
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn dyn_set_field_by_hash(
        obj_ptr: *mut c_void,
        hfield: i32,
        src_val: NanBoxedValue,
        src_kind: u32,
        src_type_ptr: *mut c_void,
        fn_dyn_setd: *mut c_void,
        fn_dyn_setf: *mut c_void,
        fn_dyn_seti64: *mut c_void,
        fn_dyn_seti: *mut c_void,
        fn_dyn_setp: *mut c_void,
    ) {
        match src_kind {
            hl::hl_type_kind_HF64 => {
                if !fn_dyn_setd.is_null() {
                    let f: FnDynSetD = unsafe { std::mem::transmute(fn_dyn_setd) };
                    unsafe { f(obj_ptr, hfield, src_val.as_f64()) };
                }
            }
            hl::hl_type_kind_HF32 => {
                if !fn_dyn_setf.is_null() {
                    let f: FnDynSetF = unsafe { std::mem::transmute(fn_dyn_setf) };
                    unsafe { f(obj_ptr, hfield, src_val.as_f64() as f32) };
                }
            }
            hl::hl_type_kind_HI64 => {
                if !fn_dyn_seti64.is_null() {
                    let f: FnDynSetI64 = unsafe { std::mem::transmute(fn_dyn_seti64) };
                    unsafe { f(obj_ptr, hfield, src_val.as_i64_lossy()) };
                }
            }
            hl::hl_type_kind_HI32
            | hl::hl_type_kind_HBOOL
            | hl::hl_type_kind_HUI8
            | hl::hl_type_kind_HUI16 => {
                if !fn_dyn_seti.is_null() {
                    let f: FnDynSetI = unsafe { std::mem::transmute(fn_dyn_seti) };
                    let i = if src_kind == hl::hl_type_kind_HBOOL {
                        if src_val.as_bool() {
                            1
                        } else {
                            0
                        }
                    } else {
                        src_val.as_i32()
                    };
                    unsafe { f(obj_ptr, hfield, src_type_ptr, i) };
                }
            }
            _ => {
                if !fn_dyn_setp.is_null() {
                    let f: FnDynSetP = unsafe { std::mem::transmute(fn_dyn_setp) };
                    let p = if src_val.is_null() || src_val.is_void() {
                        std::ptr::null_mut()
                    } else {
                        src_val.as_ptr() as *mut c_void
                    };
                    unsafe { f(obj_ptr, hfield, src_type_ptr, p) };
                }
            }
        }
    }

    fn dynamic_type_name(&self, d: *mut hl::vdynamic) -> Option<String> {
        if d.is_null() || self.fn_type_name.is_null() {
            return None;
        }
        let t = unsafe { (*d).t };
        if t.is_null() {
            return None;
        }
        let f: FnTypeName = unsafe { std::mem::transmute(self.fn_type_name) };
        let name_ptr = unsafe { f(t as *const hl::hl_type) };
        if name_ptr.is_null() {
            return None;
        }
        let s = unsafe { CStr::from_ptr(name_ptr as *const i8) };
        Some(s.to_string_lossy().into_owned())
    }

    fn format_hl_exception(&self, val: NanBoxedValue) -> HLExceptionPropagation {
        let msg = if val.is_null() || val.is_void() {
            None
        } else {
            let dyn_ptr = val.as_ptr() as *mut hl::vdynamic;
            let base = self.value_to_string(dyn_ptr);
            if !self.fn_obj_get_field.is_null() {
                let get_field: FnObjGetField =
                    unsafe { std::mem::transmute(self.fn_obj_get_field) };
                let mut extracted: Option<String> = None;
                for field_name in ["__exceptionMessage", "message"] {
                    let h = self.hash_literal_name(field_name);
                    let msg_dyn = unsafe { get_field(dyn_ptr, h) };
                    if let Some(inner) = self.value_to_string(msg_dyn) {
                        if !inner.is_empty() && inner != "null" {
                            extracted = Some(inner);
                            break;
                        }
                    }
                }
                if let Some(inner) = extracted {
                    if let Some(base) = base.as_ref() {
                        if !inner.is_empty() && inner != *base {
                            Some(format!("{}: {}", base, inner))
                        } else {
                            Some(base.clone())
                        }
                    } else if !inner.is_empty() {
                        Some(inner)
                    } else {
                        base
                    }
                } else {
                    base
                }
            } else {
                base
            }
        };
        HLExceptionPropagation {
            value: val,
            message: msg,
        }
    }

    /// Intern a bytecode string as null-terminated UTF-16 and return a stable pointer.
    fn intern_utf16_string(
        &mut self,
        bytecode: &DecodedBytecode,
        str_idx: usize,
    ) -> Option<*const u16> {
        if let Some(cached) = self.utf16_strings.get(&str_idx) {
            return Some(cached.as_ptr());
        }
        let s = bytecode.strings.get(str_idx)?;
        let mut utf16: Vec<u16> = s.encode_utf16().collect();
        utf16.push(0);
        self.utf16_strings.insert(str_idx, utf16);
        Some(self.utf16_strings[&str_idx].as_ptr())
    }

    /// Execute starting from the bytecode entrypoint.
    pub fn execute_entrypoint(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
    ) -> Result<NanBoxedValue> {
        self.ensure_gc_runtime_initialized();
        // Initialize constants (pre-populated globals) before running
        self.init_constants(bytecode, native_resolver)?;

        let entry_findex = bytecode.entrypoint as usize;
        self.call_function(bytecode, native_resolver, entry_findex, &[])
    }

    /// Initialize bytecode constants into the globals array.
    /// Constants are pre-allocated global type singletons that must be
    /// initialized before the entrypoint runs.
    fn init_constants(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
    ) -> Result<()> {
        if bytecode.constants.is_empty() {
            return Ok(());
        }

        // Resolve hlp_alloc_closure_void for function-typed constant fields
        let fn_alloc_closure_void = native_resolver
            .resolve_function("std", "hlp_alloc_closure_void")
            .unwrap_or(std::ptr::null_mut());

        for constant in &bytecode.constants {
            let global_idx = constant.global as usize;
            if global_idx >= bytecode.globals.len() {
                continue;
            }

            let type_idx = bytecode.globals[global_idx].0;
            let hl_type_rust = &bytecode.types[type_idx];
            let c_type_ptr = self.c_type_factory.get(type_idx);

            if c_type_ptr.is_null() {
                continue;
            }

            let kind = hl_type_rust.kind;

            if kind == hl::hl_type_kind_HOBJ || kind == hl::hl_type_kind_HSTRUCT {
                let obj_data = match hl_type_rust.obj.as_ref() {
                    Some(o) => o,
                    None => continue,
                };

                // Allocate the object via hlp_alloc_obj
                let alloc_fn = self.fn_alloc_obj;
                if alloc_fn.is_null() {
                    continue;
                }

                let f: FnAllocObj = unsafe { std::mem::transmute(alloc_fn) };
                let obj_ptr = unsafe { f(c_type_ptr as *mut c_void) };
                if obj_ptr.is_null() {
                    continue;
                }

                // Store in globals
                self.globals[global_idx] = NanBoxedValue::from_ptr(obj_ptr as usize);

                // Update the global_value slot ONLY when this constant IS the class descriptor
                // for its type (i.e., global_idx == type.obj.global_value - 1).
                // Do NOT update for regular instances (e.g., String constants), which would
                // overwrite the class descriptor slot with a plain data object.
                unsafe {
                    let obj = (*c_type_ptr).__bindgen_anon_1.obj;
                    if !obj.is_null() && !(*obj).global_value.is_null() {
                        let (gd, _) = self.c_type_factory.globals_data();
                        let slot_offset = (*obj).global_value.offset_from(gd as *const *mut c_void);
                        if slot_offset >= 0 && slot_offset as usize == global_idx {
                            *(*obj).global_value = obj_ptr;
                        }
                    }
                }

                // Get runtime object to compute field offsets
                let get_rt = self.fn_get_obj_rt;
                if get_rt.is_null() || constant.fields.is_empty() {
                    continue;
                }

                let rt = unsafe {
                    let get_rt_fn: FnGetObjRt = std::mem::transmute(get_rt);
                    get_rt_fn(c_type_ptr as *mut c_void) as *const hl_runtime_obj
                };

                if rt.is_null() {
                    continue;
                }

                // Calculate field start offset (skip parent fields)
                let start = unsafe { (*rt).nfields as usize - obj_data.fields.len() };

                // Get module context for function pointer stubs
                let module_ctx = self.c_type_factory.module_ctx();

                // Fill in constant fields
                for (j, &field_value) in constant.fields.iter().enumerate() {
                    if j >= obj_data.fields.len() {
                        break;
                    }

                    let field_type_idx = obj_data.fields[j].type_.0;
                    let field_kind = bytecode.types[field_type_idx].kind;
                    let field_c_type = self.c_type_factory.get(field_type_idx);

                    let field_offset = unsafe { *(*rt).fields_indexes.add(j + start) };
                    let field_addr = unsafe { (obj_ptr as *mut u8).add(field_offset as usize) };

                    match field_kind {
                        hl::hl_type_kind_HFUN | hl::hl_type_kind_HMETHOD => {
                            // field_value is a findex - create closure
                            if !fn_alloc_closure_void.is_null() {
                                let findex = field_value as usize;
                                let func_ptr = unsafe {
                                    if !module_ctx.is_null()
                                        && !(*module_ctx).functions_ptrs.is_null()
                                    {
                                        *(*module_ctx).functions_ptrs.add(findex)
                                    } else {
                                        (findex + 1) as *mut c_void
                                    }
                                };

                                let alloc_cl: FnAllocClosureVoid =
                                    unsafe { std::mem::transmute(fn_alloc_closure_void) };
                                let closure =
                                    unsafe { alloc_cl(field_c_type as *mut c_void, func_ptr) };
                                if !closure.is_null() {
                                    unsafe {
                                        *(field_addr as *mut *mut c_void) = closure as *mut c_void;
                                    }
                                }
                            }
                        }
                        k if k == hl::hl_type_kind_HOBJ || k == hl::hl_type_kind_HSTRUCT => {
                            // field_value is a global index reference
                            let ref_global = field_value as usize;
                            if ref_global < self.globals.len() {
                                let ref_val = self.globals[ref_global];
                                unsafe {
                                    *(field_addr as *mut usize) = if ref_val.is_null() {
                                        0
                                    } else {
                                        ref_val.as_ptr()
                                    };
                                }
                            }
                        }
                        hl::hl_type_kind_HBYTES => {
                            // field_value is a string index → convert to UTF-16 pointer
                            let str_idx = field_value as usize;
                            if let Some(ptr) = self.intern_utf16_string(bytecode, str_idx) {
                                unsafe {
                                    *(field_addr as *mut *const u16) = ptr;
                                }
                            }
                        }
                        hl::hl_type_kind_HTYPE => {
                            // field_value is a type index → store c_type pointer (8 bytes)
                            let type_ptr = self.c_type_factory.get(field_value as usize);
                            unsafe {
                                *(field_addr as *mut usize) = type_ptr as usize;
                            }
                        }
                        _ => {
                            // field_value is an index into ints table for HI32/HBOOL
                            let int_val = bytecode
                                .ints
                                .get(field_value as usize)
                                .copied()
                                .unwrap_or(field_value);
                            unsafe {
                                *(field_addr as *mut i32) = int_val;
                            }
                        }
                    }
                }
            }
        }

        // Sync globals_data → self.globals for any entries set by native code
        // (e.g., hlp_alloc_obj may write to global_value slots during binding setup)
        let (gd, nglobals) = self.c_type_factory.globals_data();
        if !gd.is_null() {
            for i in 0..nglobals.min(self.globals.len()) {
                let raw = unsafe { *gd.add(i) };
                if !raw.is_null() && self.globals[i].is_null() {
                    self.globals[i] = NanBoxedValue::from_ptr(raw as usize);
                }
            }
        }

        Ok(())
    }

    /// Call a function by its findex.
    pub fn call_function(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        findex: usize,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        self.ensure_gc_runtime_initialized();

        // Track call count for JIT promotion
        let call_count = {
            let count = self.call_counts.entry(findex).or_insert(0);
            *count += 1;
            *count
        };
        let threshold = self
            .tiered_runtime
            .as_ref()
            .map(|t| t.config.jit_threshold)
            .unwrap_or(self.jit_threshold);
        if call_count == threshold {
            self.jit_candidates.push(findex);
        }
        self.poll_tiered_results();

        // Hybrid tiered call path for bytecode functions.
        if self.findex_to_func.contains_key(&findex) {
            let mut compiled_entry = self
                .tiered_runtime
                .as_ref()
                .and_then(|t| t.compiled.get(&findex).cloned());

            if compiled_entry.is_none()
                && self.should_attempt_tiered_promotion(bytecode, findex, args, call_count)
            {
                self.queue_tiered_promotion(findex);
                compiled_entry = self
                    .tiered_runtime
                    .as_ref()
                    .and_then(|t| t.compiled.get(&findex).cloned());
            }

            if let Some(entry) = compiled_entry {
                match self.call_compiled_function(findex, &entry, args) {
                    Ok(v) => {
                        if let Some(tiered) = self.tiered_runtime.as_mut() {
                            tiered.stats.compiled_calls += 1;
                        }
                        return Ok(v);
                    }
                    Err(e) => {
                        self.record_tiered_fallback(
                            findex,
                            format!("compiled invoke failed: {}", e),
                        );
                    }
                }
            }
        }

        // Check if it's a bytecode function or native
        if let Some(&func_idx) = self.findex_to_func.get(&findex) {
            self.execute_hl_function(bytecode, native_resolver, func_idx, args)
        } else if let Some(&native_idx) = self.findex_to_native.get(&findex) {
            self.call_native(bytecode, native_resolver, native_idx, args)
        } else {
            Err(anyhow!("Function findex {} not found", findex))
        }
    }

    fn should_attempt_tiered_promotion(
        &self,
        bytecode: &DecodedBytecode,
        findex: usize,
        args: &[NanBoxedValue],
        call_count: u64,
    ) -> bool {
        let Some(tiered) = self.tiered_runtime.as_ref() else {
            return false;
        };
        let log_promotions = tiered.config.log_promotions;
        let log_this_check = log_promotions && call_count == tiered.config.jit_threshold;
        let log_skip = |reason: &str| {
            if log_this_check {
                eprintln!("[tiered] skip findex={} reason={}", findex, reason);
            }
        };
        if !tiered.config.enabled || !tiered.config.strict_mode {
            log_skip("disabled_or_non_strict");
            return false;
        }
        if tiered.blacklist.contains_key(&findex) || tiered.compiled.contains_key(&findex) {
            log_skip("already_blacklisted_or_compiled");
            return false;
        }
        if tiered.queued.contains(&findex) {
            log_skip("already_queued");
            return false;
        }
        if args.len() > tiered.config.max_jit_args {
            log_skip("arg_count_over_limit");
            return false;
        }
        let Some(&func_idx) = self.findex_to_func.get(&findex) else {
            log_skip("not_bytecode_function");
            return false;
        };
        let func = &bytecode.functions[func_idx];
        let func_name = func.name();
        if func_name == "init"
            || func_name == "main"
            || func_name == "__constructor__"
            || func_name.starts_with("__")
        {
            log_skip("name_blacklisted");
            return false;
        }
        if tiered.config.min_ops_for_promotion > 0
            && func.ops.len() < tiered.config.min_ops_for_promotion
        {
            log_skip("op_count_below_min");
            return false;
        }
        if let Some(bad) = func.ops.iter().find(|op| !Self::is_v1_tierable_opcode(op)) {
            if log_this_check {
                eprintln!(
                    "[tiered] skip findex={} reason=unsupported_opcode op={:?}",
                    findex, bad
                );
            }
            return false;
        }
        if call_count < tiered.config.jit_threshold {
            log_skip("below_threshold");
            return false;
        }
        true
    }

    fn is_v1_tierable_opcode(op: &Opcode) -> bool {
        matches!(
            op,
            Opcode::Nop
                | Opcode::Label
                | Opcode::Mov { .. }
                | Opcode::Int { .. }
                | Opcode::Float { .. }
                | Opcode::Bool { .. }
                | Opcode::Null { .. }
                | Opcode::Add { .. }
                | Opcode::Sub { .. }
                | Opcode::Mul { .. }
                | Opcode::SDiv { .. }
                | Opcode::UDiv { .. }
                | Opcode::SMod { .. }
                | Opcode::UMod { .. }
                | Opcode::Shl { .. }
                | Opcode::SShr { .. }
                | Opcode::UShr { .. }
                | Opcode::And { .. }
                | Opcode::Or { .. }
                | Opcode::Xor { .. }
                | Opcode::Neg { .. }
                | Opcode::Not { .. }
                | Opcode::Incr { .. }
                | Opcode::Decr { .. }
                | Opcode::JEq { .. }
                | Opcode::JNotEq { .. }
                | Opcode::JSGte { .. }
                | Opcode::JSGt { .. }
                | Opcode::JSLte { .. }
                | Opcode::JSLt { .. }
                | Opcode::JUGte { .. }
                | Opcode::JULt { .. }
                | Opcode::JNotLt { .. }
                | Opcode::JNotGte { .. }
                | Opcode::JTrue { .. }
                | Opcode::JFalse { .. }
                | Opcode::JNull { .. }
                | Opcode::JNotNull { .. }
                | Opcode::JAlways { .. }
                | Opcode::Ret { .. }
        )
    }

    fn queue_tiered_promotion(&mut self, findex: usize) {
        let Some(tiered) = self.tiered_runtime.as_mut() else {
            return;
        };
        if tiered.blacklist.contains_key(&findex)
            || tiered.compiled.contains_key(&findex)
            || tiered.queued.contains(&findex)
        {
            return;
        }
        tiered.stats.attempted_promotions += 1;
        if tiered.worker_tx.send(findex).is_ok() {
            tiered.queued.insert(findex);
            if tiered.config.log_promotions {
                eprintln!("[tiered] queued findex={}", findex);
            }
        } else {
            tiered.stats.failed_promotions += 1;
            tiered.config.enabled = false;
            let reason = "tiered worker channel closed".to_string();
            tiered.blacklist.insert(findex, reason.clone());
            if tiered.config.log_promotions {
                eprintln!("[tiered] disabling tiered runtime: {}", reason);
            }
        }
    }

    fn poll_tiered_results(&mut self) {
        let mut disconnected = false;
        loop {
            let msg = {
                let Some(tiered) = self.tiered_runtime.as_mut() else {
                    return;
                };
                match tiered.worker_rx.try_recv() {
                    Ok(msg) => Some(msg),
                    Err(TryRecvError::Empty) => None,
                    Err(TryRecvError::Disconnected) => {
                        disconnected = true;
                        None
                    }
                }
            };

            let Some(PromotionResult { findex, result }) = msg else {
                break;
            };

            if let Some(tiered) = self.tiered_runtime.as_mut() {
                tiered.queued.remove(&findex);
                match result {
                    Ok(CompiledFunctionMeta {
                        findex: _,
                        fn_addr,
                        arg_kinds,
                        ret_kind,
                    }) => {
                        let entry = CompiledFunctionEntry {
                            fn_addr,
                            arg_kinds,
                            ret_kind,
                        };
                        tiered.compiled.insert(findex, entry);
                        tiered.stats.successful_promotions += 1;
                        if tiered.config.log_promotions {
                            eprintln!("[tiered] promoted findex={} addr=0x{:x}", findex, fn_addr);
                        }
                    }
                    Err(reason) => {
                        tiered.stats.failed_promotions += 1;
                        tiered.blacklist.insert(findex, reason.clone());
                        if tiered.config.log_promotions {
                            eprintln!("[tiered] blacklist findex={} reason={}", findex, reason);
                        }
                    }
                }
            }
        }

        if disconnected {
            if let Some(tiered) = self.tiered_runtime.as_mut() {
                tiered.config.enabled = false;
                if tiered.config.log_promotions {
                    eprintln!("[tiered] worker disconnected; disabling tiered runtime");
                }
            }
        }
    }

    fn record_tiered_fallback(&mut self, findex: usize, reason: String) {
        if let Some(tiered) = self.tiered_runtime.as_mut() {
            tiered.stats.fallback_calls += 1;
            tiered.compiled.remove(&findex);
            tiered.blacklist.insert(findex, reason.clone());
            if tiered.config.log_promotions {
                eprintln!("[tiered] fallback findex={} reason={}", findex, reason);
            }
        }
    }

    fn call_compiled_function(
        &mut self,
        findex: usize,
        entry: &CompiledFunctionEntry,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        if args.len() > 8 {
            return Err(anyhow!(
                "Compiled call {} has {} args (max 8)",
                findex,
                args.len()
            ));
        }

        self.sync_gc_scan_roots();

        let func_ptr = entry.fn_addr as *mut c_void;
        let arg_kinds = &entry.arg_kinds;
        let ret_kind = entry.ret_kind;

        let is_float_kind = |k: u32| k == hl::hl_type_kind_HF32 || k == hl::hl_type_kind_HF64;
        let ret_is_float = is_float_kind(ret_kind);
        let float_mask: u32 = arg_kinds.iter().enumerate().fold(0u32, |acc, (i, &k)| {
            if is_float_kind(k) {
                acc | (1 << i)
            } else {
                acc
            }
        });

        let extract_arg = |idx: usize| -> i64 {
            let kind = if idx < arg_kinds.len() {
                arg_kinds[idx]
            } else {
                0
            };
            self.value_to_i64(args[idx], kind)
        };

        let fn_setup_trap = self.fn_setup_trap_jit;
        let fn_remove_trap = self.fn_remove_trap_jit;
        let fn_get_exc = self.fn_get_exc_value;
        let fn_clear_exc = self.fn_clear_exc_value;
        let mut trap_installed = false;
        if !fn_setup_trap.is_null() {
            type FnSetupTrap = unsafe extern "C" fn() -> *mut c_void;
            let setup: FnSetupTrap = unsafe { std::mem::transmute(fn_setup_trap) };
            let jmp_buf = unsafe { setup() };
            if !jmp_buf.is_null() {
                trap_installed = true;
                let jumped = unsafe { call_setjmp_opaque(jmp_buf) };
                if jumped != 0 {
                    if !fn_get_exc.is_null() {
                        type FnGetExc = unsafe extern "C" fn() -> *mut c_void;
                        let exc_ptr =
                            unsafe { (std::mem::transmute::<*mut c_void, FnGetExc>(fn_get_exc))() };
                        if !exc_ptr.is_null() {
                            if !fn_clear_exc.is_null() {
                                type FnClearExc = unsafe extern "C" fn();
                                unsafe {
                                    (std::mem::transmute::<*mut c_void, FnClearExc>(fn_clear_exc))()
                                };
                            }
                            return Err(anyhow::Error::new(
                                self.format_hl_exception(NanBoxedValue::from_ptr(exc_ptr as usize)),
                            ));
                        }
                    }
                    return Err(anyhow!(
                        "Compiled call longjmp without exception: findex {}",
                        findex
                    ));
                }
            }
        }

        let dispatch_res: Result<i64> = if ret_is_float || float_mask != 0 {
            self.dispatch_float_native(func_ptr, args, arg_kinds, float_mask, ret_is_float)
        } else {
            Ok(unsafe {
                match args.len() {
                    0 => {
                        let f: unsafe extern "C" fn() -> i64 = std::mem::transmute(func_ptr);
                        f()
                    }
                    1 => {
                        let f: unsafe extern "C" fn(i64) -> i64 = std::mem::transmute(func_ptr);
                        f(extract_arg(0))
                    }
                    2 => {
                        let f: unsafe extern "C" fn(i64, i64) -> i64 =
                            std::mem::transmute(func_ptr);
                        f(extract_arg(0), extract_arg(1))
                    }
                    3 => {
                        let f: unsafe extern "C" fn(i64, i64, i64) -> i64 =
                            std::mem::transmute(func_ptr);
                        f(extract_arg(0), extract_arg(1), extract_arg(2))
                    }
                    4 => {
                        let f: unsafe extern "C" fn(i64, i64, i64, i64) -> i64 =
                            std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                        )
                    }
                    5 => {
                        let f: unsafe extern "C" fn(i64, i64, i64, i64, i64) -> i64 =
                            std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                            extract_arg(4),
                        )
                    }
                    6 => {
                        let f: unsafe extern "C" fn(i64, i64, i64, i64, i64, i64) -> i64 =
                            std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                            extract_arg(4),
                            extract_arg(5),
                        )
                    }
                    7 => {
                        let f: unsafe extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64 =
                            std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                            extract_arg(4),
                            extract_arg(5),
                            extract_arg(6),
                        )
                    }
                    8 => {
                        let f: unsafe extern "C" fn(i64, i64, i64, i64, i64, i64, i64, i64) -> i64 =
                            std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                            extract_arg(4),
                            extract_arg(5),
                            extract_arg(6),
                            extract_arg(7),
                        )
                    }
                    _ => 0i64,
                }
            })
        };

        if trap_installed && !fn_remove_trap.is_null() {
            type FnRemoveTrap = unsafe extern "C" fn();
            unsafe { (std::mem::transmute::<*mut c_void, FnRemoveTrap>(fn_remove_trap))() };
        }

        let raw_result = dispatch_res?;
        Ok(self.wrap_native_result(raw_result, ret_kind))
    }

    /// Execute a HashLink bytecode function.
    fn execute_hl_function(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        func_idx: usize,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        let func = &bytecode.functions[func_idx];

        if self.stack.len() >= self.max_stack_depth {
            return Err(anyhow!("Stack overflow (depth {})", self.stack.len()));
        }

        // Create frame with registers
        let reg_count = func.regs.len();
        let mut frame = InterpreterFrame::new(func_idx, reg_count);

        // Bind arguments to first N registers
        let type_fun = bytecode.types[func.type_.0]
            .fun
            .as_ref()
            .expect("function should have fun type");
        for (i, arg) in args.iter().enumerate() {
            if i < type_fun.args.len() {
                frame.registers.set(i as u32, *arg);
            }
        }

        self.stack.push(frame);
        self.sync_gc_scan_roots();

        // Main interpretation loop — always pop the frame even on error
        let result = self.interpret_loop(bytecode, native_resolver, func_idx);
        self.stack.pop();
        self.sync_gc_scan_roots();
        result
    }

    /// Main opcode dispatch loop for a function.
    fn interpret_loop(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        func_idx: usize,
    ) -> Result<NanBoxedValue> {
        loop {
            let func = &bytecode.functions[func_idx];
            let frame = self.stack.last().unwrap();
            let pc = frame.pc;

            if pc >= func.ops.len() {
                return Ok(NanBoxedValue::void());
            }

            let op = func.ops[pc].clone();
            if std::env::var("ASH_TRACE_ASSERT").is_ok() {
                eprintln!(
                    "[TRACE] f{} {} pc={} op={:?}",
                    func_idx,
                    func.name(),
                    pc,
                    op
                );
            }
            let result = self.execute_opcode(bytecode, &op, func_idx)?;

            match result {
                StepResult::Continue => {
                    self.stack.last_mut().unwrap().pc += 1;
                }
                StepResult::Jump(offset) => {
                    let frame = self.stack.last_mut().unwrap();
                    // offset is relative to the NEXT instruction
                    let next_pc = (frame.pc as i64) + 1 + (offset as i64);
                    frame.pc = next_pc as usize;
                }
                StepResult::JumpAbs(target_pc) => {
                    self.stack.last_mut().unwrap().pc = target_pc;
                }
                StepResult::Return(value) => {
                    return Ok(value);
                }
                StepResult::Call { findex, args, dst } => {
                    match self.call_function(bytecode, native_resolver, findex, &args) {
                        Ok(ret) => {
                            let dst_kind = bytecode.types[func.regs[dst as usize].0].kind;
                            let coerced = self.coerce_value_for_static_kind(ret, dst_kind);
                            self.stack.last_mut().unwrap().registers.set(dst, coerced);
                            self.stack.last_mut().unwrap().pc += 1;
                        }
                        Err(e) => {
                            if let Some(hl_exc) = e.downcast_ref::<HLExceptionPropagation>() {
                                let exc_val = hl_exc.value;
                                let frame = self.stack.last_mut().unwrap();
                                if let Some((target_pc, exc_reg)) = frame.trap_stack.pop() {
                                    frame.registers.set(exc_reg, exc_val);
                                    frame.pc = target_pc;
                                    // Continue from catch block (no pc increment)
                                } else {
                                    return Err(e);
                                }
                            } else {
                                return Err(e);
                            }
                        }
                    }
                }
            }
        }
    }

    /// Execute a single opcode. Returns what action the loop should take.
    fn execute_opcode(
        &mut self,
        bytecode: &DecodedBytecode,
        op: &Opcode,
        func_idx: usize,
    ) -> Result<StepResult> {
        let func = &bytecode.functions[func_idx];
        let fn_hash_gen = self.fn_hash_gen;
        let frame = self.stack.last_mut().unwrap();

        match op {
            // ===== Movement / Constants =====
            Opcode::Mov { dst, src } => {
                let val = frame.registers.get(src.0);
                frame.registers.set(dst.0, val);
            }
            Opcode::Int { dst, ptr } => {
                let val = bytecode.ints[ptr.0];
                frame.registers.set(dst.0, NanBoxedValue::from_i32(val));
            }
            Opcode::Float { dst, ptr } => {
                let val = bytecode.floats[ptr.0];
                frame.registers.set(dst.0, NanBoxedValue::from_f64(val));
            }
            Opcode::Bool { dst, value } => {
                frame.registers.set(dst.0, NanBoxedValue::from_bool(*value));
            }
            Opcode::Bytes { dst, ptr } => {
                let pos = bytecode.bytes_pos[ptr.0];
                frame
                    .registers
                    .set(dst.0, NanBoxedValue::from_bytes_ptr(pos));
            }
            Opcode::String { dst, ptr } => {
                // HashLink uses UTF-16 strings internally.
                // Get or create a cached null-terminated UTF-16 version of the string.
                let utf16_ptr = if let Some(cached) = self.utf16_strings.get(&ptr.0) {
                    cached.as_ptr()
                } else {
                    let s = bytecode
                        .strings
                        .get(ptr.0)
                        .ok_or_else(|| anyhow!("String constant out of bounds: {}", ptr.0))?;
                    let mut buf: Vec<u16> = s.encode_utf16().collect();
                    buf.push(0);
                    self.utf16_strings.insert(ptr.0, buf);
                    self.utf16_strings[&ptr.0].as_ptr()
                };
                frame
                    .registers
                    .set(dst.0, NanBoxedValue::from_bytes_ptr(utf16_ptr as usize));
            }
            Opcode::Null { dst } => {
                frame.registers.set(dst.0, NanBoxedValue::null());
            }

            // ===== Arithmetic =====
            Opcode::Add { dst, a, b } => {
                let va = frame.registers.get(a.0);
                let vb = frame.registers.get(b.0);
                let result = if let Some(r) = va.binary_int_op(vb, IntBinOp::Add) {
                    r
                } else if let Some(r) = va.binary_float_op(vb, FloatBinOp::Add) {
                    r
                } else {
                    return Err(anyhow!(
                        "Add: incompatible types {:?} + {:?} in {} at pc={} (dst=r{}, a=r{}, b=r{})",
                        va,
                        vb,
                        func.name(),
                        frame.pc,
                        dst.0,
                        a.0,
                        b.0
                    ));
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::Sub { dst, a, b } => {
                let va = frame.registers.get(a.0);
                let vb = frame.registers.get(b.0);
                let result = if let Some(r) = va.binary_int_op(vb, IntBinOp::Sub) {
                    r
                } else if let Some(r) = va.binary_float_op(vb, FloatBinOp::Sub) {
                    r
                } else {
                    return Err(anyhow!("Sub: incompatible types"));
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::Mul { dst, a, b } => {
                let va = frame.registers.get(a.0);
                let vb = frame.registers.get(b.0);
                let result = if let Some(r) = va.binary_int_op(vb, IntBinOp::Mul) {
                    r
                } else if let Some(r) = va.binary_float_op(vb, FloatBinOp::Mul) {
                    r
                } else {
                    return Err(anyhow!("Mul: incompatible types"));
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::SDiv { dst, a, b } => {
                let va = frame.registers.get(a.0);
                let vb = frame.registers.get(b.0);
                let result = if let Some(r) = va.binary_int_op(vb, IntBinOp::SDiv) {
                    r
                } else if let Some(r) = va.binary_float_op(vb, FloatBinOp::SDiv) {
                    r
                } else {
                    return Err(anyhow!("SDiv: incompatible types or div by zero"));
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::UDiv { dst, a, b } => {
                let va = frame.registers.get(a.0);
                let vb = frame.registers.get(b.0);
                let result = va
                    .binary_int_op(vb, IntBinOp::UDiv)
                    .ok_or_else(|| anyhow!("UDiv: incompatible types or div by zero"))?;
                frame.registers.set(dst.0, result);
            }
            Opcode::SMod { dst, a, b } => {
                let va = frame.registers.get(a.0);
                let vb = frame.registers.get(b.0);
                let result = if let Some(r) = va.binary_int_op(vb, IntBinOp::SMod) {
                    r
                } else if let Some(r) = va.binary_float_op(vb, FloatBinOp::SMod) {
                    r
                } else {
                    return Err(anyhow!("SMod: incompatible types or div by zero"));
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::UMod { dst, a, b } => {
                let va = frame.registers.get(a.0);
                let vb = frame.registers.get(b.0);
                // UMod only on integers
                let l = va.as_i32() as u32;
                let r = vb.as_i32() as u32;
                if r == 0 {
                    return Err(anyhow!("UMod: division by zero"));
                }
                frame
                    .registers
                    .set(dst.0, NanBoxedValue::from_i32((l % r) as i32));
            }
            Opcode::Shl { dst, a, b } => {
                self.int_binop(func, IntBinOp::Shl, dst.0, a.0, b.0)?;
            }
            Opcode::SShr { dst, a, b } => {
                self.int_binop(func, IntBinOp::SShr, dst.0, a.0, b.0)?;
            }
            Opcode::UShr { dst, a, b } => {
                self.int_binop(func, IntBinOp::UShr, dst.0, a.0, b.0)?;
            }
            Opcode::And { dst, a, b } => {
                self.int_binop(func, IntBinOp::And, dst.0, a.0, b.0)?;
            }
            Opcode::Or { dst, a, b } => {
                self.int_binop(func, IntBinOp::Or, dst.0, a.0, b.0)?;
            }
            Opcode::Xor { dst, a, b } => {
                self.int_binop(func, IntBinOp::Xor, dst.0, a.0, b.0)?;
            }
            Opcode::Neg { dst, src } => {
                let val = frame.registers.get(src.0);
                let result = if val.is_i32() {
                    NanBoxedValue::from_i32(val.as_i32().wrapping_neg())
                } else if val.is_f64() {
                    NanBoxedValue::from_f64(-val.as_f64())
                } else {
                    return Err(anyhow!("Neg: unsupported type {:?}", val));
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::Not { dst, src } => {
                let val = frame.registers.get(src.0);
                let result = if val.is_i32() {
                    NanBoxedValue::from_i32(!val.as_i32())
                } else if val.is_bool() {
                    NanBoxedValue::from_bool(!val.as_bool())
                } else {
                    return Err(anyhow!("Not: unsupported type {:?}", val));
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::Incr { dst } => {
                let val = frame.registers.get(dst.0);
                if val.is_i32() {
                    frame
                        .registers
                        .set(dst.0, NanBoxedValue::from_i32(val.as_i32().wrapping_add(1)));
                } else if val.is_f64() {
                    frame
                        .registers
                        .set(dst.0, NanBoxedValue::from_f64(val.as_f64() + 1.0));
                }
            }
            Opcode::Decr { dst } => {
                let val = frame.registers.get(dst.0);
                if val.is_i32() {
                    frame
                        .registers
                        .set(dst.0, NanBoxedValue::from_i32(val.as_i32().wrapping_sub(1)));
                } else if val.is_f64() {
                    frame
                        .registers
                        .set(dst.0, NanBoxedValue::from_f64(val.as_f64() - 1.0));
                }
            }

            // ===== Function Calls =====
            Opcode::Call0 { dst, fun } => {
                return Ok(StepResult::Call {
                    findex: fun.0,
                    args: vec![],
                    dst: dst.0,
                });
            }
            Opcode::Call1 { dst, fun, arg0 } => {
                let a0 = frame.registers.get(arg0.0);
                return Ok(StepResult::Call {
                    findex: fun.0,
                    args: vec![a0],
                    dst: dst.0,
                });
            }
            Opcode::Call2 {
                dst,
                fun,
                arg0,
                arg1,
            } => {
                let a0 = frame.registers.get(arg0.0);
                let a1 = frame.registers.get(arg1.0);
                return Ok(StepResult::Call {
                    findex: fun.0,
                    args: vec![a0, a1],
                    dst: dst.0,
                });
            }
            Opcode::Call3 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
            } => {
                let a0 = frame.registers.get(arg0.0);
                let a1 = frame.registers.get(arg1.0);
                let a2 = frame.registers.get(arg2.0);
                return Ok(StepResult::Call {
                    findex: fun.0,
                    args: vec![a0, a1, a2],
                    dst: dst.0,
                });
            }
            Opcode::Call4 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
                arg3,
            } => {
                let a0 = frame.registers.get(arg0.0);
                let a1 = frame.registers.get(arg1.0);
                let a2 = frame.registers.get(arg2.0);
                let a3 = frame.registers.get(arg3.0);
                return Ok(StepResult::Call {
                    findex: fun.0,
                    args: vec![a0, a1, a2, a3],
                    dst: dst.0,
                });
            }
            Opcode::CallN { dst, fun, args } => {
                let arg_vals: Vec<NanBoxedValue> =
                    args.iter().map(|r| frame.registers.get(r.0)).collect();
                return Ok(StepResult::Call {
                    findex: fun.0,
                    args: arg_vals,
                    dst: dst.0,
                });
            }
            Opcode::CallMethod { dst, field, args } | Opcode::CallThis { dst, field, args } => {
                // args[0] is 'this', remaining are actual arguments
                let arg_vals: Vec<NanBoxedValue> =
                    args.iter().map(|r| frame.registers.get(r.0)).collect();
                let this_val = arg_vals[0];

                if this_val.is_null() || this_val.is_void() {
                    return Err(anyhow!(
                        "CallMethod on null object (field={}, pc={})",
                        field.0,
                        frame.pc
                    ));
                }

                // HVIRTUAL dispatch: ToVirtual is a no-op in the interpreter,
                // so `this_val` holds the raw HOBJ pointer directly.
                // Resolve the findex by matching the virtual field's hashed_name
                // against the runtime object's proto chain.
                let this_reg_type_idx = func.regs[args[0].0 as usize].0;
                if this_reg_type_idx < bytecode.types.len()
                    && bytecode.types[this_reg_type_idx].kind == hl::hl_type_kind_HVIRTUAL
                {
                    let virt_type = self.c_type_factory.get(this_reg_type_idx);
                    let obj_ptr = this_val.as_ptr() as *const u8;
                    let findex_opt = unsafe {
                        // Get hashed_name of the virtual field
                        let virt = (*virt_type).__bindgen_anon_1.virt.as_ref();
                        if let Some(virt_data) = virt {
                            if (field.0 as i32) < virt_data.nfields {
                                let virt_field = &*virt_data.fields.add(field.0);
                                let hname = virt_field.hashed_name;
                                // Walk the runtime obj's proto chain for hname
                                let mut obj_hl_type = *(obj_ptr as *const *mut hl_type);
                                let mut found = None;
                                'search: while !obj_hl_type.is_null()
                                    && ((*obj_hl_type).kind == hl::hl_type_kind_HOBJ
                                        || (*obj_hl_type).kind == hl::hl_type_kind_HSTRUCT)
                                {
                                    let obj = (*obj_hl_type).__bindgen_anon_1.obj;
                                    for i in 0..(*obj).nproto as usize {
                                        let pr = &*(*obj).proto.add(i);
                                        if pr.hashed_name == hname {
                                            found = Some(pr.findex as usize);
                                            break 'search;
                                        }
                                    }
                                    // Try super class
                                    obj_hl_type = (*obj).super_ as *mut hl_type;
                                }
                                found
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    };
                    if let Some(findex) = findex_opt {
                        return Ok(StepResult::Call {
                            findex,
                            args: arg_vals,
                            dst: dst.0,
                        });
                    }
                }

                // Try to resolve via vobj_proto (set up by hlp_get_obj_proto)
                let obj_ptr = this_val.as_ptr() as *const u8;
                let findex = unsafe {
                    let type_ptr = *(obj_ptr as *const *mut hl_type);
                    if !type_ptr.is_null() {
                        let vobj_proto = (*type_ptr).vobj_proto;
                        if !vobj_proto.is_null() && vobj_proto as usize > 1 {
                            let method_ptr = *vobj_proto.add(field.0);
                            // Extract findex from stub pointer (findex+1)
                            (method_ptr as usize).wrapping_sub(1)
                        } else {
                            // Fallback: resolve from bytecode type proto
                            self.resolve_method_findex_from_bytecode(
                                bytecode, func, &args[0], field.0,
                            )
                            .ok_or_else(|| {
                                anyhow!("Cannot resolve method field={} on type", field.0)
                            })?
                        }
                    } else {
                        self.resolve_method_findex_from_bytecode(bytecode, func, &args[0], field.0)
                            .ok_or_else(|| {
                                anyhow!(
                                    "Cannot resolve method field={} (null type header)",
                                    field.0
                                )
                            })?
                    }
                };

                return Ok(StepResult::Call {
                    findex,
                    args: arg_vals,
                    dst: dst.0,
                });
            }
            Opcode::CallClosure { dst, fun, args } => {
                let closure_val = frame.registers.get(fun.0);
                let mut arg_vals: Vec<NanBoxedValue> =
                    args.iter().map(|r| frame.registers.get(r.0)).collect();

                if closure_val.is_null() || closure_val.is_void() {
                    return Err(anyhow!("CallClosure on null closure (pc={})", frame.pc));
                }

                // The closure value might be:
                // 1. A TAG_FUNC: raw function index (from StaticClosure with no capture)
                // 2. A TAG_PTR to a _vclosure struct (InstanceClosure with bound value)
                let findex = if closure_val.is_func() {
                    closure_val.as_func_index()
                } else {
                    let raw = closure_val.as_ptr();
                    if self.findex_to_func.contains_key(&raw)
                        || self.findex_to_native.contains_key(&raw)
                    {
                        raw
                    } else {
                        // It's a pointer to a _vclosure struct
                        let cl_ptr = raw as *const _vclosure;
                        if cl_ptr.is_null()
                            || !(cl_ptr as usize).is_multiple_of(std::mem::align_of::<_vclosure>())
                        {
                            return Err(anyhow!(
                                "CallClosure invalid closure value: {:?}",
                                closure_val
                            ));
                        }
                        unsafe {
                            let fun_ptr = (*cl_ptr).fun;
                            // Extract findex from stub pointer (findex+1)
                            let fi = (fun_ptr as usize).wrapping_sub(1);
                            // If the closure has a bound value, prepend it as the first arg
                            if (*cl_ptr).hasValue != 0 && !(*cl_ptr).value.is_null() {
                                let bound = NanBoxedValue::from_ptr((*cl_ptr).value as usize);
                                arg_vals.insert(0, bound);
                            }
                            fi
                        }
                    }
                };

                return Ok(StepResult::Call {
                    findex,
                    args: arg_vals,
                    dst: dst.0,
                });
            }

            // ===== Closures =====
            Opcode::StaticClosure { dst, fun } => {
                // Materialize a real vclosure* so std natives such as
                // hl.Api.noClosure / Reflect.callMethod can consume it.
                let findex = fun.0;
                let type_idx = if let Some(&fidx) = self.findex_to_func.get(&findex) {
                    bytecode.functions[fidx].type_.0
                } else if let Some(&nidx) = self.findex_to_native.get(&findex) {
                    bytecode.natives[nidx].type_.0
                } else {
                    usize::MAX
                };

                if type_idx != usize::MAX && !self.fn_alloc_closure_void.is_null() {
                    type FnAllocClosureVoid =
                        unsafe extern "C" fn(*mut c_void, *mut c_void) -> *mut _vclosure;
                    let f: FnAllocClosureVoid =
                        unsafe { std::mem::transmute(self.fn_alloc_closure_void) };
                    let tptr = self.c_type_factory.get(type_idx) as *mut c_void;
                    let closure = unsafe { f(tptr, (findex + 1) as *mut c_void) };
                    if !closure.is_null() {
                        if std::env::var("ASH_DBG_CLOSURE").is_ok() {
                            eprintln!(
                                "[STATICCLOSURE] findex={} type_idx={} -> {:p}",
                                findex, type_idx, closure
                            );
                        }
                        frame
                            .registers
                            .set(dst.0, NanBoxedValue::from_ptr(closure as usize));
                        return Ok(StepResult::Continue);
                    }
                }

                // Fallback to interpreter-local representation.
                if std::env::var("ASH_DBG_CLOSURE").is_ok() {
                    eprintln!(
                        "[STATICCLOSURE-FALLBACK] findex={} type_idx={} alloc_fn={:p}",
                        findex, type_idx, self.fn_alloc_closure_void
                    );
                }
                frame
                    .registers
                    .set(dst.0, NanBoxedValue::from_func_index(findex));
            }
            Opcode::InstanceClosure { dst, fun, obj } => {
                // Create a heap-allocated _vclosure with the bound object.
                // The closure's fun pointer is the stub sentinel (findex+1) so that
                // CallClosure can extract the findex. The bound object is stored in
                // vclosure.value and prepended as the first argument on CallClosure.
                let obj_val = frame.registers.get(obj.0);
                let obj_ptr = if obj_val.is_null() || obj_val.is_void() {
                    std::ptr::null_mut()
                } else {
                    obj_val.as_ptr() as *mut std::ffi::c_void
                };
                let findex = fun.0;
                let closure = Box::new(_vclosure {
                    t: std::ptr::null_mut(),
                    fun: (findex + 1) as *mut std::ffi::c_void,
                    hasValue: 1,
                    stackCount: 0,
                    value: obj_ptr,
                });
                let addr = Box::into_raw(closure) as usize;
                frame.registers.set(dst.0, NanBoxedValue::from_ptr(addr));
            }
            Opcode::VirtualClosure { dst, obj, field } => {
                // Resolve the virtual method findex from the object's proto chain,
                // then create a vclosure with the object as bound value.
                let obj_val = frame.registers.get(obj.0);
                if obj_val.is_null() || obj_val.is_void() {
                    frame.registers.set(dst.0, NanBoxedValue::null());
                } else {
                    let obj_ptr = obj_val.as_ptr() as *const u8;
                    // The virtual field index into the interface's field table
                    // We need to look up the method findex from the object's runtime type.
                    // For now, look up via the object's proto chain by field index.
                    let findex_opt: Option<usize> = unsafe {
                        let obj_hl_type = *(obj_ptr as *const *mut hl::hl_type);
                        if !obj_hl_type.is_null()
                            && ((*obj_hl_type).kind == hl::hl_type_kind_HOBJ
                                || (*obj_hl_type).kind == hl::hl_type_kind_HSTRUCT)
                        {
                            let obj_data = (*obj_hl_type).__bindgen_anon_1.obj;
                            let fi = field.0 as usize;
                            if fi < (*obj_data).nproto as usize {
                                Some((*(*obj_data).proto.add(fi)).findex as usize)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    };
                    if let Some(findex) = findex_opt {
                        let closure = Box::new(_vclosure {
                            t: std::ptr::null_mut(),
                            fun: (findex + 1) as *mut std::ffi::c_void,
                            hasValue: 1,
                            stackCount: 0,
                            value: obj_val.as_ptr() as *mut std::ffi::c_void,
                        });
                        let addr = Box::into_raw(closure) as usize;
                        frame.registers.set(dst.0, NanBoxedValue::from_ptr(addr));
                    } else {
                        frame.registers.set(dst.0, NanBoxedValue::null());
                    }
                }
            }

            // ===== Globals =====
            Opcode::GetGlobal { dst, global } => {
                let mut val = if global.0 < self.globals.len() {
                    self.globals[global.0]
                } else {
                    NanBoxedValue::null()
                };
                // If our NanBoxed store is null, check the shared globals_data array.
                // Native stdlib may have written to global_value slots (which point into
                // globals_data) without going through the interpreter's SetGlobal.
                if val.is_null() {
                    let (gd, nglobals) = self.c_type_factory.globals_data();
                    if !gd.is_null() && global.0 < nglobals {
                        let raw = unsafe { *gd.add(global.0) };
                        if !raw.is_null() {
                            val = NanBoxedValue::from_ptr(raw as usize);
                            // Cache it in our globals for future reads
                            self.globals[global.0] = val;
                        }
                    }
                }
                frame.registers.set(dst.0, val);
            }
            Opcode::SetGlobal { global, src } => {
                let val = frame.registers.get(src.0);
                if global.0 >= self.globals.len() {
                    self.globals.resize(global.0 + 1, NanBoxedValue::null());
                }
                self.globals[global.0] = val;
                // Also update globals_data so native code sees the new value
                let (gd, nglobals) = self.c_type_factory.globals_data();
                if !gd.is_null() && global.0 < nglobals {
                    unsafe {
                        *gd.add(global.0) = if val.is_null() || val.is_void() {
                            std::ptr::null_mut()
                        } else {
                            val.as_ptr() as *mut c_void
                        };
                    }
                }
            }

            // ===== Fields =====
            Opcode::Field { dst, obj, field } => {
                // Extract c_type info before borrowing frame mutably
                let obj_type_idx = func.regs[obj.0 as usize].0;
                let obj_kind = bytecode.types[obj_type_idx].kind;
                let obj_c_type = self.c_type_factory.get(obj_type_idx) as *mut c_void;
                let dst_kind = bytecode.types[func.regs[dst.0 as usize].0].kind;
                let get_rt = self.fn_get_obj_rt;
                let obj_val = frame.registers.get(obj.0);
                if std::env::var("ASH_DBG_FIELD").is_ok() {
                    eprintln!(
                        "[FIELD] f{} pc={} obj_ty={} obj_kind={} field={} dst_kind={} obj={:?}",
                        func_idx, frame.pc, obj_type_idx, obj_kind, field.0, dst_kind, obj_val
                    );
                }
                if obj_val.is_null() || obj_val.is_void() {
                    frame.registers.set(dst.0, NanBoxedValue::null());
                } else if obj_kind == hl::hl_type_kind_HOBJ || obj_kind == hl::hl_type_kind_HSTRUCT
                {
                    let obj_ptr = obj_val.as_ptr() as *mut u8;
                    let val = unsafe {
                        Self::read_obj_field(
                            obj_ptr, field.0, dst_kind, obj_c_type, obj_kind, get_rt,
                        )
                    };
                    if std::env::var("ASH_DBG_FIELD").is_ok() {
                        eprintln!(
                            "[GETFIELD-OBJ] f{} pc={} obj_ty={} obj_kind={} field={} dst_kind={} -> {:?}",
                            func_idx, frame.pc, obj_type_idx, obj_kind, field.0, dst_kind, val
                        );
                    }
                    frame.registers.set(dst.0, val);
                } else if obj_kind == hl::hl_type_kind_HVIRTUAL {
                    if let Some(offset) =
                        unsafe { Self::resolve_virtual_field_offset(obj_c_type, field.0) }
                    {
                        let obj_ptr = obj_val.as_ptr() as *mut u8;
                        let addr = unsafe { obj_ptr.add(offset) };
                        let val = unsafe { Self::read_value_at(addr, dst_kind) };
                        if std::env::var("ASH_DBG_FIELD").is_ok() {
                            eprintln!(
                                "[GETFIELD-VIRT] f{} pc={} obj_ty={} field={} off={} dst_kind={} -> {:?}",
                                func_idx, frame.pc, obj_type_idx, field.0, offset, dst_kind, val
                            );
                        }
                        frame.registers.set(dst.0, val);
                    } else {
                        let key = (obj_val.as_ptr(), field.0);
                        let val = self
                            .virtual_fields
                            .get(&key)
                            .copied()
                            .unwrap_or_else(NanBoxedValue::null);
                        if std::env::var("ASH_DBG_FIELD").is_ok() {
                            eprintln!(
                                "[GETFIELD-VIRT-FALLBACK] f{} pc={} obj_ty={} field={} -> {:?}",
                                func_idx, frame.pc, obj_type_idx, field.0, val
                            );
                        }
                        frame.registers.set(dst.0, val);
                    }
                } else if let Some(hfield) =
                    Self::resolve_typed_field_hash(bytecode, obj_type_idx, field.0)
                {
                    let obj_ptr = obj_val.as_ptr() as *mut c_void;
                    let dst_type_idx = func.regs[dst.0 as usize].0;
                    let dst_type_ptr = self.c_type_factory.get(dst_type_idx) as *mut c_void;
                    let out = Self::dyn_get_field_by_hash(
                        obj_ptr,
                        hfield,
                        dst_kind,
                        dst_type_ptr,
                        self.fn_dyn_getd,
                        self.fn_dyn_getf,
                        self.fn_dyn_geti64,
                        self.fn_dyn_geti,
                        self.fn_dyn_getp,
                    );
                    frame.registers.set(dst.0, out);
                } else {
                    frame.registers.set(dst.0, NanBoxedValue::null());
                }
            }
            Opcode::GetThis { dst, field } => {
                let obj_type_idx = func.regs[0].0;
                let obj_kind = bytecode.types[obj_type_idx].kind;
                let obj_c_type = self.c_type_factory.get(obj_type_idx) as *mut c_void;
                let dst_kind = bytecode.types[func.regs[dst.0 as usize].0].kind;
                let get_rt = self.fn_get_obj_rt;
                let obj_val = frame.registers.get(0); // reg 0 is 'this'
                if obj_val.is_null() || obj_val.is_void() {
                    frame.registers.set(dst.0, NanBoxedValue::null());
                } else if obj_kind == hl::hl_type_kind_HOBJ || obj_kind == hl::hl_type_kind_HSTRUCT
                {
                    let obj_ptr = obj_val.as_ptr() as *mut u8;
                    let val = unsafe {
                        Self::read_obj_field(
                            obj_ptr, field.0, dst_kind, obj_c_type, obj_kind, get_rt,
                        )
                    };
                    if std::env::var("ASH_DBG_FIELD").is_ok() {
                        eprintln!(
                            "[GETTHIS-OBJ] f{} pc={} obj_ty={} obj_kind={} field={} dst_kind={} -> {:?}",
                            func_idx, frame.pc, obj_type_idx, obj_kind, field.0, dst_kind, val
                        );
                    }
                    frame.registers.set(dst.0, val);
                } else if obj_kind == hl::hl_type_kind_HVIRTUAL {
                    if let Some(offset) =
                        unsafe { Self::resolve_virtual_field_offset(obj_c_type, field.0) }
                    {
                        let obj_ptr = obj_val.as_ptr() as *mut u8;
                        let addr = unsafe { obj_ptr.add(offset) };
                        let val = unsafe { Self::read_value_at(addr, dst_kind) };
                        if std::env::var("ASH_DBG_FIELD").is_ok() {
                            eprintln!(
                                "[GETTHIS-VIRT] f{} pc={} obj_ty={} field={} off={} dst_kind={} -> {:?}",
                                func_idx, frame.pc, obj_type_idx, field.0, offset, dst_kind, val
                            );
                        }
                        frame.registers.set(dst.0, val);
                    } else {
                        let key = (obj_val.as_ptr(), field.0);
                        let val = self
                            .virtual_fields
                            .get(&key)
                            .copied()
                            .unwrap_or_else(NanBoxedValue::null);
                        if std::env::var("ASH_DBG_FIELD").is_ok() {
                            eprintln!(
                                "[GETTHIS-VIRT-FALLBACK] f{} pc={} obj_ty={} field={} -> {:?}",
                                func_idx, frame.pc, obj_type_idx, field.0, val
                            );
                        }
                        frame.registers.set(dst.0, val);
                    }
                } else if let Some(hfield) =
                    Self::resolve_typed_field_hash(bytecode, obj_type_idx, field.0)
                {
                    let obj_ptr = obj_val.as_ptr() as *mut c_void;
                    let dst_type_idx = func.regs[dst.0 as usize].0;
                    let dst_type_ptr = self.c_type_factory.get(dst_type_idx) as *mut c_void;
                    let out = Self::dyn_get_field_by_hash(
                        obj_ptr,
                        hfield,
                        dst_kind,
                        dst_type_ptr,
                        self.fn_dyn_getd,
                        self.fn_dyn_getf,
                        self.fn_dyn_geti64,
                        self.fn_dyn_geti,
                        self.fn_dyn_getp,
                    );
                    frame.registers.set(dst.0, out);
                } else {
                    frame.registers.set(dst.0, NanBoxedValue::null());
                }
            }
            Opcode::SetField { obj, field, src } => {
                let obj_type_idx = func.regs[obj.0 as usize].0;
                let obj_kind = bytecode.types[obj_type_idx].kind;
                let obj_c_type = self.c_type_factory.get(obj_type_idx) as *mut c_void;
                let src_type_idx = func.regs[src.0 as usize].0;
                let src_kind = bytecode.types[src_type_idx].kind;
                let get_rt = self.fn_get_obj_rt;
                let obj_val = frame.registers.get(obj.0);
                if std::env::var("ASH_DBG_FIELD").is_ok() {
                    eprintln!(
                        "[SETFIELD] f{} pc={} obj_ty={} obj_kind={} field={} src_ty={} src_kind={} obj={:?} src={:?}",
                        func_idx,
                        frame.pc,
                        obj_type_idx,
                        obj_kind,
                        field.0,
                        src_type_idx,
                        src_kind,
                        obj_val,
                        frame.registers.get(src.0)
                    );
                }
                if !obj_val.is_null() && !obj_val.is_void() {
                    let src_val = frame.registers.get(src.0);
                    if obj_kind == hl::hl_type_kind_HOBJ || obj_kind == hl::hl_type_kind_HSTRUCT {
                        let obj_ptr = obj_val.as_ptr() as *mut u8;
                        if std::env::var("ASH_DBG_FIELD").is_ok() {
                            eprintln!(
                                "[SETFIELD-OBJ] f{} pc={} obj_ty={} obj_kind={} field={} src_kind={} src={:?}",
                                func_idx, frame.pc, obj_type_idx, obj_kind, field.0, src_kind, src_val
                            );
                        }
                        unsafe {
                            Self::write_obj_field(
                                obj_ptr, field.0, src_kind, src_val, obj_c_type, obj_kind, get_rt,
                            );
                        }
                    } else if obj_kind == hl::hl_type_kind_HVIRTUAL {
                        if let Some(offset) =
                            unsafe { Self::resolve_virtual_field_offset(obj_c_type, field.0) }
                        {
                            let obj_ptr = obj_val.as_ptr() as *mut u8;
                            let addr = unsafe { obj_ptr.add(offset) };
                            if std::env::var("ASH_DBG_FIELD").is_ok() {
                                eprintln!(
                                    "[SETFIELD-VIRT] f{} pc={} obj_ty={} field={} off={} src_kind={} src={:?}",
                                    func_idx, frame.pc, obj_type_idx, field.0, offset, src_kind, src_val
                                );
                            }
                            unsafe { Self::write_value_at(addr, src_kind, src_val) };
                        } else {
                            self.virtual_fields
                                .insert((obj_val.as_ptr(), field.0), src_val);
                            if std::env::var("ASH_DBG_FIELD").is_ok() {
                                eprintln!(
                                    "[SETFIELD-VIRT-FALLBACK] f{} pc={} obj_ty={} field={} src={:?}",
                                    func_idx, frame.pc, obj_type_idx, field.0, src_val
                                );
                            }
                        }
                    } else if let Some(hfield) =
                        Self::resolve_typed_field_hash(bytecode, obj_type_idx, field.0)
                    {
                        let obj_ptr = obj_val.as_ptr() as *mut c_void;
                        let src_type_ptr = self.c_type_factory.get(src_type_idx) as *mut c_void;
                        Self::dyn_set_field_by_hash(
                            obj_ptr,
                            hfield,
                            src_val,
                            src_kind,
                            src_type_ptr,
                            self.fn_dyn_setd,
                            self.fn_dyn_setf,
                            self.fn_dyn_seti64,
                            self.fn_dyn_seti,
                            self.fn_dyn_setp,
                        );
                    }
                }
            }
            Opcode::SetThis { field, src } => {
                let obj_type_idx = func.regs[0].0;
                let obj_kind = bytecode.types[obj_type_idx].kind;
                let obj_c_type = self.c_type_factory.get(obj_type_idx) as *mut c_void;
                let src_type_idx = func.regs[src.0 as usize].0;
                let src_kind = bytecode.types[src_type_idx].kind;
                let get_rt = self.fn_get_obj_rt;
                let obj_val = frame.registers.get(0); // reg 0 is 'this'
                if !obj_val.is_null() && !obj_val.is_void() {
                    let src_val = frame.registers.get(src.0);
                    if obj_kind == hl::hl_type_kind_HOBJ || obj_kind == hl::hl_type_kind_HSTRUCT {
                        let obj_ptr = obj_val.as_ptr() as *mut u8;
                        if std::env::var("ASH_DBG_FIELD").is_ok() {
                            eprintln!(
                                "[SETTHIS-OBJ] f{} pc={} obj_ty={} obj_kind={} field={} src_kind={} src={:?}",
                                func_idx, frame.pc, obj_type_idx, obj_kind, field.0, src_kind, src_val
                            );
                        }
                        unsafe {
                            Self::write_obj_field(
                                obj_ptr, field.0, src_kind, src_val, obj_c_type, obj_kind, get_rt,
                            );
                        }
                    } else if obj_kind == hl::hl_type_kind_HVIRTUAL {
                        if let Some(offset) =
                            unsafe { Self::resolve_virtual_field_offset(obj_c_type, field.0) }
                        {
                            let obj_ptr = obj_val.as_ptr() as *mut u8;
                            let addr = unsafe { obj_ptr.add(offset) };
                            if std::env::var("ASH_DBG_FIELD").is_ok() {
                                eprintln!(
                                    "[SETTHIS-VIRT] f{} pc={} obj_ty={} field={} off={} src_kind={} src={:?}",
                                    func_idx, frame.pc, obj_type_idx, field.0, offset, src_kind, src_val
                                );
                            }
                            unsafe { Self::write_value_at(addr, src_kind, src_val) };
                        } else {
                            self.virtual_fields
                                .insert((obj_val.as_ptr(), field.0), src_val);
                            if std::env::var("ASH_DBG_FIELD").is_ok() {
                                eprintln!(
                                    "[SETTHIS-VIRT-FALLBACK] f{} pc={} obj_ty={} field={} src={:?}",
                                    func_idx, frame.pc, obj_type_idx, field.0, src_val
                                );
                            }
                        }
                    } else if let Some(hfield) =
                        Self::resolve_typed_field_hash(bytecode, obj_type_idx, field.0)
                    {
                        let obj_ptr = obj_val.as_ptr() as *mut c_void;
                        let src_type_ptr = self.c_type_factory.get(src_type_idx) as *mut c_void;
                        Self::dyn_set_field_by_hash(
                            obj_ptr,
                            hfield,
                            src_val,
                            src_kind,
                            src_type_ptr,
                            self.fn_dyn_setd,
                            self.fn_dyn_setf,
                            self.fn_dyn_seti64,
                            self.fn_dyn_seti,
                            self.fn_dyn_setp,
                        );
                    }
                }
            }
            Opcode::DynGet { dst, obj, field } => {
                let obj_val = frame.registers.get(obj.0);
                if obj_val.is_null() || obj_val.is_void() {
                    frame.registers.set(dst.0, NanBoxedValue::null());
                } else {
                    let hfield = hash_field_name(
                        bytecode,
                        field.0,
                        fn_hash_gen,
                        &mut self.utf16_strings,
                        &mut self.field_hash_cache,
                    )?;
                    if std::env::var("ASH_DBG_DYN").is_ok() {
                        let fname = bytecode
                            .strings
                            .get(field.0)
                            .map(String::as_str)
                            .unwrap_or("<oob>");
                        eprintln!(
                            "[DYNGET] f{} pc={} obj={:?} field={} name={} hash={}",
                            func_idx, frame.pc, obj_val, field.0, fname, hfield
                        );
                    }
                    let obj_ptr = obj_val.as_ptr() as *mut c_void;
                    let dst_type_idx = func.regs[dst.0 as usize].0;
                    let dst_kind = bytecode.types[dst_type_idx].kind;
                    let dst_type_ptr = self.c_type_factory.get(dst_type_idx) as *mut c_void;
                    let out = Self::dyn_get_field_by_hash(
                        obj_ptr,
                        hfield,
                        dst_kind,
                        dst_type_ptr,
                        self.fn_dyn_getd,
                        self.fn_dyn_getf,
                        self.fn_dyn_geti64,
                        self.fn_dyn_geti,
                        self.fn_dyn_getp,
                    );
                    if std::env::var("ASH_DBG_DYN").is_ok() {
                        eprintln!(
                            "[DYNGET] f{} pc={} dst_kind={} -> {:?}",
                            func_idx, frame.pc, dst_kind, out
                        );
                    }
                    frame.registers.set(dst.0, out);
                }
            }
            Opcode::DynSet { obj, field, src } => {
                let obj_val = frame.registers.get(obj.0);
                if obj_val.is_null() || obj_val.is_void() {
                    // no-op
                } else {
                    let hfield = hash_field_name(
                        bytecode,
                        field.0,
                        fn_hash_gen,
                        &mut self.utf16_strings,
                        &mut self.field_hash_cache,
                    )?;
                    let obj_ptr = obj_val.as_ptr() as *mut c_void;
                    let src_val = frame.registers.get(src.0);
                    let src_type_idx = func.regs[src.0 as usize].0;
                    let src_kind = bytecode.types[src_type_idx].kind;
                    if std::env::var("ASH_DBG_DYN").is_ok() {
                        let fname = bytecode
                            .strings
                            .get(field.0)
                            .map(String::as_str)
                            .unwrap_or("<oob>");
                        eprintln!(
                            "[DYNSET] f{} pc={} obj={:?} field={} name={} hash={} src_ty={} src_kind={} src={:?}",
                            func_idx, frame.pc, obj_val, field.0, fname, hfield, src_type_idx, src_kind, src_val
                        );
                    }
                    let src_type_ptr = self.c_type_factory.get(src_type_idx) as *mut c_void;
                    Self::dyn_set_field_by_hash(
                        obj_ptr,
                        hfield,
                        src_val,
                        src_kind,
                        src_type_ptr,
                        self.fn_dyn_setd,
                        self.fn_dyn_setf,
                        self.fn_dyn_seti64,
                        self.fn_dyn_seti,
                        self.fn_dyn_setp,
                    );
                }
            }

            // ===== Conditional Jumps =====
            Opcode::JTrue { cond, offset } => {
                let val = frame.registers.get(cond.0);
                if val.to_bool() {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JFalse { cond, offset } => {
                let val = frame.registers.get(cond.0);
                if !val.to_bool() {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JNull { reg, offset } => {
                let val = frame.registers.get(reg.0);
                if val.is_null() {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JNotNull { reg, offset } => {
                let val = frame.registers.get(reg.0);
                if !val.is_null() {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JSLt { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::SLt) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JSGte { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::SGte) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JSGt { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::SGt) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JSLte { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::SLte) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JULt { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::ULt) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JUGte { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::UGte) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JNotLt { a, b, offset } => {
                // JNotLt is equivalent to JGte (signed)
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::SGte) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JNotGte { a, b, offset } => {
                // JNotGte is equivalent to JLt (signed)
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::SLt) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JEq { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::Eq) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JNotEq { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::NotEq) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JAlways { offset } => {
                return Ok(StepResult::Jump(*offset));
            }

            // ===== Control Flow =====
            Opcode::Ret { ret } => {
                let val = frame.registers.get(ret.0);
                return Ok(StepResult::Return(val));
            }
            Opcode::Switch { reg, offsets, end } => {
                let val = frame.registers.get(reg.0);
                let index = val.as_i32();
                if index >= 0 && (index as usize) < offsets.len() {
                    return Ok(StepResult::Jump(offsets[index as usize]));
                } else {
                    // Out-of-range switch values fall through to the default arm,
                    // which is encoded as the next instruction in HL bytecode.
                    let _ = end;
                    return Ok(StepResult::Continue);
                }
            }
            Opcode::Label => {
                // No-op marker
            }
            Opcode::Nop => {}
            Opcode::Assert => {
                return Err(anyhow!("Assert hit at pc {}", frame.pc));
            }

            // ===== Type Operations =====
            Opcode::Type { dst, ty } => {
                // Store C-level hl_type pointer for native interop
                let c_type_ptr = self.c_type_factory.get(ty.0);
                frame
                    .registers
                    .set(dst.0, NanBoxedValue::from_ptr(c_type_ptr as usize));
            }
            Opcode::GetType { dst, src } => {
                // GetType returns the RUNTIME hl_type* of the value.
                // For reference types (HDYN, HOBJ, HSTRUCT, etc.) the hl_type*
                // is stored in the first 8 bytes of the pointed-to object.
                // For primitives, return the static C type pointer.
                let type_ref = &func.regs[src.0 as usize];
                let src_kind = bytecode.types[type_ref.0].kind;
                let val = frame.registers.get(src.0);

                let type_ptr: usize = if val.is_ptr() && !val.is_null() && val.as_ptr() != 0 {
                    match src_kind {
                        hl::hl_type_kind_HDYN
                        | hl::hl_type_kind_HOBJ
                        | hl::hl_type_kind_HSTRUCT
                        | hl::hl_type_kind_HVIRTUAL
                        | hl::hl_type_kind_HENUM
                        | hl::hl_type_kind_HDYNOBJ
                        | hl::hl_type_kind_HNULL => {
                            // First 8 bytes of object is the hl_type*
                            unsafe { *(val.as_ptr() as *const usize) }
                        }
                        _ => self.c_type_factory.get(type_ref.0) as usize,
                    }
                } else {
                    self.c_type_factory.get(type_ref.0) as usize
                };

                frame
                    .registers
                    .set(dst.0, NanBoxedValue::from_ptr(type_ptr));
            }
            Opcode::GetTID { dst, src } => {
                // GetTID returns the kind field of the hl_type* in src.
                // src should hold an hl_type* (result of GetType).
                // hl_type.kind is a u32 at offset 0.
                let val = frame.registers.get(src.0);
                let kind = if val.is_ptr() && !val.is_null() && val.as_ptr() != 0 {
                    unsafe { *(val.as_ptr() as *const u32) as i32 }
                } else {
                    // Fallback to static type kind
                    let type_ref = &func.regs[src.0 as usize];
                    bytecode.types[type_ref.0].kind as i32
                };
                frame.registers.set(dst.0, NanBoxedValue::from_i32(kind));
            }

            // ===== Casting =====
            Opcode::ToDyn { dst, src } => {
                // Box a value into a vdynamic* for native code consumption.
                // Pointer types (HOBJ, HDYN, etc.) already have a vdynamic header - pass through.
                // Primitive types (HI32, HF64, HBOOL, HBYTES) need hlp_make_dyn wrapping.
                let src_type_ref = &func.regs[src.0 as usize];
                let src_kind = bytecode.types[src_type_ref.0].kind;
                let val = frame.registers.get(src.0);

                let needs_boxing = matches!(
                    src_kind,
                    hl::hl_type_kind_HI32
                        | hl::hl_type_kind_HI64
                        | hl::hl_type_kind_HF32
                        | hl::hl_type_kind_HF64
                        | hl::hl_type_kind_HBOOL
                        | hl::hl_type_kind_HBYTES
                        | hl::hl_type_kind_HUI8
                        | hl::hl_type_kind_HUI16
                );

                if needs_boxing && !self.fn_make_dyn.is_null() {
                    let c_type_ptr = self.c_type_factory.get(src_type_ref.0);
                    // Create a stack slot holding the raw value for hlp_make_dyn
                    let mut data: i64 = if val.is_i32() {
                        val.as_i32() as i64
                    } else if val.is_f64() {
                        val.as_f64().to_bits() as i64
                    } else if val.is_bool() {
                        val.as_bool() as i64
                    } else {
                        // Pointer-like (HBYTES, etc.)
                        val.as_ptr() as i64
                    };
                    let make_dyn: unsafe extern "C" fn(*mut c_void, *mut c_void) -> *mut c_void =
                        unsafe { std::mem::transmute(self.fn_make_dyn) };
                    let dyn_ptr = unsafe {
                        make_dyn(
                            &mut data as *mut i64 as *mut c_void,
                            c_type_ptr as *mut c_void,
                        )
                    };
                    frame
                        .registers
                        .set(dst.0, NanBoxedValue::from_ptr(dyn_ptr as usize));
                } else {
                    // Already a pointer type with vdynamic header, or no make_dyn available
                    frame.registers.set(dst.0, val);
                }
            }
            Opcode::ToSFloat { dst, src } => {
                let val = frame.registers.get(src.0);
                let f = if val.is_i32() {
                    val.as_i32() as f64
                } else {
                    val.as_f64()
                };
                frame.registers.set(dst.0, NanBoxedValue::from_f64(f));
            }
            Opcode::ToUFloat { dst, src } => {
                let val = frame.registers.get(src.0);
                let f = if val.is_i32() {
                    (val.as_i32() as u32) as f64
                } else {
                    val.as_f64()
                };
                frame.registers.set(dst.0, NanBoxedValue::from_f64(f));
            }
            Opcode::ToInt { dst, src } => {
                let val = frame.registers.get(src.0);
                let i = if val.is_f64() {
                    val.as_f64() as i32
                } else {
                    val.as_i32()
                };
                frame.registers.set(dst.0, NanBoxedValue::from_i32(i));
            }
            Opcode::SafeCast { dst, src } => {
                let val = frame.registers.get(src.0);
                let dst_kind = bytecode.types[func.regs[dst.0 as usize].0].kind;
                // When casting a vdynamic* to a primitive type, extract the value
                // from the vdynamic (layout: t=0..7, v=8..15).
                let result = if val.is_ptr() && !val.is_null() && val.as_ptr() != 0 {
                    unsafe {
                        Self::unbox_dynamic_to_kind(val.as_ptr() as *mut hl::vdynamic, dst_kind)
                            .unwrap_or(val)
                    }
                } else if val.is_null() {
                    match dst_kind {
                        hl::hl_type_kind_HI32 | hl::hl_type_kind_HUI8 | hl::hl_type_kind_HUI16 => {
                            NanBoxedValue::from_i32(0)
                        }
                        hl::hl_type_kind_HI64 => NanBoxedValue::from_i64(0),
                        hl::hl_type_kind_HF32 | hl::hl_type_kind_HF64 => {
                            NanBoxedValue::from_f64(0.0)
                        }
                        hl::hl_type_kind_HBOOL => NanBoxedValue::from_bool(false),
                        _ => val,
                    }
                } else {
                    val
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::UnsafeCast { dst, src } => {
                let val = frame.registers.get(src.0);
                frame.registers.set(dst.0, val);
            }
            Opcode::ToVirtual { dst, src } => {
                // TODO: Convert object to virtual interface (Phase 3)
                let val = frame.registers.get(src.0);
                frame.registers.set(dst.0, val);
            }

            // ===== Object Creation =====
            Opcode::New { dst } => {
                let type_idx = func.regs[dst.0 as usize].0;
                let type_kind = bytecode.types[type_idx].kind;
                let c_type_ptr = self.c_type_factory.get(type_idx);

                let obj = match type_kind {
                    hl::hl_type_kind_HOBJ | hl::hl_type_kind_HSTRUCT => {
                        if c_type_ptr.is_null() || self.fn_alloc_obj.is_null() {
                            std::ptr::null_mut()
                        } else {
                            let f: FnAllocObj = unsafe { std::mem::transmute(self.fn_alloc_obj) };
                            unsafe { f(c_type_ptr as *mut c_void) }
                        }
                    }
                    hl::hl_type_kind_HDYNOBJ => {
                        if self.fn_alloc_dynobj.is_null() {
                            std::ptr::null_mut()
                        } else {
                            let f: FnAllocDynObj =
                                unsafe { std::mem::transmute(self.fn_alloc_dynobj) };
                            unsafe { f() }
                        }
                    }
                    hl::hl_type_kind_HVIRTUAL => {
                        if c_type_ptr.is_null() || self.fn_alloc_virtual.is_null() {
                            std::ptr::null_mut()
                        } else {
                            let f: FnAllocVirtual =
                                unsafe { std::mem::transmute(self.fn_alloc_virtual) };
                            unsafe { f(c_type_ptr as *mut c_void) }
                        }
                    }
                    _ => std::ptr::null_mut(),
                };

                if obj.is_null() {
                    frame.registers.set(dst.0, NanBoxedValue::null());
                } else {
                    frame
                        .registers
                        .set(dst.0, NanBoxedValue::from_ptr(obj as usize));
                }
            }

            // ===== Array Operations =====
            Opcode::GetArray { dst, array, index } => {
                let arr_val = frame.registers.get(array.0);
                let idx = frame.registers.get(index.0).as_i32().max(0) as usize;
                let dst_kind = bytecode.types[func.regs[dst.0 as usize].0].kind;
                let val = if arr_val.is_null() || arr_val.is_void() {
                    NanBoxedValue::null()
                } else if !arr_val.is_ptr() {
                    return Err(anyhow!(
                        "GetArray: array reg r{} is not pointer in {} at pc={} (val={:?}, type_kind={})",
                        array.0,
                        func.name(),
                        frame.pc,
                        arr_val,
                        bytecode.types[func.regs[array.0 as usize].0].kind
                    ));
                } else {
                    // varray: t@0, at@8, size@16, data@24
                    let arr_ptr = arr_val.as_ptr() as *const u8;
                    unsafe {
                        let size = *(arr_ptr.add(16) as *const i32);
                        if idx >= size.max(0) as usize {
                            return Err(anyhow!(
                                "GetArray: index {} out of bounds (size={}) in {} at pc={} arr=r{} val={:?}",
                                idx,
                                size,
                                func.name(),
                                frame.pc,
                                array.0,
                                arr_val
                            ));
                        }
                        let at = *(arr_ptr.add(8) as *const *mut hl_type);
                        if !at.is_null()
                            && !(at as usize).is_multiple_of(std::mem::align_of::<hl_type>())
                        {
                            return Err(anyhow!(
                                "GetArray: invalid at pointer {:p} in {} at pc={} (arr=r{} val={:?} idx={} r4={:?} r6={:?} r16={:?})",
                                at,
                                func.name(),
                                frame.pc,
                                array.0,
                                arr_val,
                                idx,
                                frame.registers.get(4),
                                frame.registers.get(6),
                                frame.registers.get(16)
                            ));
                        }
                        let at_kind = if at.is_null() {
                            hl::hl_type_kind_HDYN
                        } else {
                            (*at).kind
                        };
                        let data = arr_ptr.add(24);
                        match at_kind {
                            k if k == hl::hl_type_kind_HUI8 => {
                                NanBoxedValue::from_i32(*data.add(idx) as i32)
                            }
                            k if k == hl::hl_type_kind_HUI16 => {
                                NanBoxedValue::from_i32(*(data.add(idx * 2) as *const u16) as i32)
                            }
                            k if k == hl::hl_type_kind_HBOOL => {
                                NanBoxedValue::from_bool(*(data.add(idx * 2) as *const u16) != 0)
                            }
                            k if k == hl::hl_type_kind_HI32 => {
                                NanBoxedValue::from_i32(*(data.add(idx * 4) as *const i32))
                            }
                            k if k == hl::hl_type_kind_HI64 => {
                                NanBoxedValue::from_i64(*(data.add(idx * 8) as *const i64))
                            }
                            k if k == hl::hl_type_kind_HF32 => {
                                NanBoxedValue::from_f64(*(data.add(idx * 4) as *const f32) as f64)
                            }
                            k if k == hl::hl_type_kind_HF64 => {
                                NanBoxedValue::from_f64(*(data.add(idx * 8) as *const f64))
                            }
                            k => {
                                let ptr_val = *(data.add(idx * 8) as *const usize);
                                if ptr_val == 0 {
                                    match dst_kind {
                                        hl::hl_type_kind_HI32
                                        | hl::hl_type_kind_HUI8
                                        | hl::hl_type_kind_HUI16 => NanBoxedValue::from_i32(0),
                                        hl::hl_type_kind_HI64 => NanBoxedValue::from_i64(0),
                                        hl::hl_type_kind_HF32 | hl::hl_type_kind_HF64 => {
                                            NanBoxedValue::from_f64(0.0)
                                        }
                                        hl::hl_type_kind_HBOOL => NanBoxedValue::from_bool(false),
                                        _ => NanBoxedValue::null(),
                                    }
                                } else if (k == hl::hl_type_kind_HDYN
                                    || k == hl::hl_type_kind_HNULL)
                                    && Self::is_primitive_or_bytes_kind(dst_kind)
                                {
                                    Self::unbox_dynamic_to_kind(
                                        ptr_val as *mut hl::vdynamic,
                                        dst_kind,
                                    )
                                    .unwrap_or_else(|| NanBoxedValue::from_ptr(ptr_val))
                                } else {
                                    NanBoxedValue::from_ptr(ptr_val)
                                }
                            }
                        }
                    }
                };
                frame.registers.set(dst.0, val);
            }
            Opcode::SetArray { array, index, src } => {
                let arr_val = frame.registers.get(array.0);
                let idx = frame.registers.get(index.0).as_i32().max(0) as usize;
                let src_val = frame.registers.get(src.0);
                if !arr_val.is_null() && !arr_val.is_void() {
                    if !arr_val.is_ptr() {
                        return Err(anyhow!(
                            "SetArray: array reg r{} is not pointer in {} at pc={} (val={:?}, type_kind={})",
                            array.0,
                            func.name(),
                            frame.pc,
                            arr_val,
                            bytecode.types[func.regs[array.0 as usize].0].kind
                        ));
                    }
                    let arr_ptr = arr_val.as_ptr() as *mut u8;
                    unsafe {
                        let size = *(arr_ptr.add(16) as *const i32);
                        if idx >= size.max(0) as usize {
                            return Err(anyhow!(
                                "SetArray: index {} out of bounds (size={}) in {} at pc={} (arr=r{} val={:?} src={:?})",
                                idx,
                                size,
                                func.name(),
                                frame.pc,
                                array.0,
                                arr_val,
                                src_val
                            ));
                        }
                        let at = *(arr_ptr.add(8) as *const *mut hl_type);
                        if !at.is_null()
                            && !(at as usize).is_multiple_of(std::mem::align_of::<hl_type>())
                        {
                            return Err(anyhow!(
                                "SetArray: invalid at pointer {:p} in {} at pc={} (arr=r{} val={:?} idx={} src={:?} r4={:?} r6={:?} r16={:?})",
                                at,
                                func.name(),
                                frame.pc,
                                array.0,
                                arr_val,
                                idx,
                                src_val,
                                frame.registers.get(4),
                                frame.registers.get(6),
                                frame.registers.get(16)
                            ));
                        }
                        let at_kind = if at.is_null() {
                            hl::hl_type_kind_HDYN
                        } else {
                            (*at).kind
                        };
                        let data = arr_ptr.add(24);
                        match at_kind {
                            k if k == hl::hl_type_kind_HUI8 => {
                                *data.add(idx) = src_val.as_i32() as u8
                            }
                            k if k == hl::hl_type_kind_HUI16 => {
                                *(data.add(idx * 2) as *mut u16) = src_val.as_i32() as u16
                            }
                            k if k == hl::hl_type_kind_HBOOL => {
                                *(data.add(idx * 2) as *mut u16) = src_val.as_bool() as u16
                            }
                            k if k == hl::hl_type_kind_HI32 => {
                                *(data.add(idx * 4) as *mut i32) = src_val.as_i32()
                            }
                            k if k == hl::hl_type_kind_HI64 => {
                                *(data.add(idx * 8) as *mut i64) = src_val.as_i64_lossy()
                            }
                            k if k == hl::hl_type_kind_HF32 => {
                                *(data.add(idx * 4) as *mut f32) = src_val.as_f64() as f32
                            }
                            k if k == hl::hl_type_kind_HF64 => {
                                *(data.add(idx * 8) as *mut f64) = src_val.as_f64()
                            }
                            k => {
                                let ptr_val = if src_val.is_null() || src_val.is_void() {
                                    0usize
                                } else if (k == hl::hl_type_kind_HDYN
                                    || k == hl::hl_type_kind_HNULL)
                                    && !src_val.is_ptr()
                                {
                                    // Arrays of dyn/null store vdynamic*. Box primitives before write.
                                    let src_type_idx = func.regs[src.0 as usize].0;
                                    let src_t = self.c_type_factory.get(src_type_idx);
                                    let boxed = self.box_value_as_dynamic_with_type(src_val, src_t);
                                    if boxed.is_null() || boxed.is_void() {
                                        0usize
                                    } else {
                                        boxed.as_ptr()
                                    }
                                } else {
                                    src_val.as_ptr()
                                };
                                *(data.add(idx * 8) as *mut usize) = ptr_val;
                            }
                        }
                    }
                }
            }
            Opcode::ArraySize { dst, array } => {
                // Read array size: varray layout has size (i32) at offset 16.
                // Only read if the register static type is HARRAY and value is non-null.
                let arr_type_kind = bytecode.types[func.regs[array.0 as usize].0].kind;
                let arr_val = frame.registers.get(array.0);
                let size = if arr_type_kind == hl::hl_type_kind_HARRAY
                    && !arr_val.is_null()
                    && !arr_val.is_void()
                {
                    let arr_ptr = arr_val.as_ptr() as *const u8;
                    // varray: t@0, at@8, size@16 (i32)
                    unsafe { *(arr_ptr.add(16) as *const i32) }
                } else {
                    0i32
                };
                frame.registers.set(dst.0, NanBoxedValue::from_i32(size));
            }

            // ===== Memory Access =====
            // bytes = base pointer, index = byte offset
            Opcode::GetI8 { dst, bytes, index } => {
                let base = frame.registers.get(bytes.0);
                let idx = frame.registers.get(index.0).as_i32();
                let val = if base.is_null() || base.is_void() || idx < 0 {
                    NanBoxedValue::from_i32(0)
                } else {
                    let addr = (base.as_ptr() as *const u8).wrapping_add(idx as usize);
                    NanBoxedValue::from_i32(unsafe { *(addr as *const i8) as i32 })
                };
                frame.registers.set(dst.0, val);
            }
            Opcode::GetI16 { dst, bytes, index } => {
                let base = frame.registers.get(bytes.0);
                let idx = frame.registers.get(index.0).as_i32();
                let val = if base.is_null() || base.is_void() || idx < 0 {
                    NanBoxedValue::from_i32(0)
                } else {
                    let addr = (base.as_ptr() as *const u8).wrapping_add(idx as usize);
                    NanBoxedValue::from_i32(unsafe { *(addr as *const i16) as i32 })
                };
                frame.registers.set(dst.0, val);
            }
            Opcode::GetMem { dst, bytes, index } => {
                let base = frame.registers.get(bytes.0);
                let idx = frame.registers.get(index.0).as_i32();
                let dst_kind = bytecode.types[func.regs[dst.0 as usize].0].kind;
                let val = if base.is_null() || base.is_void() || idx < 0 {
                    NanBoxedValue::from_i32(0)
                } else {
                    let addr = (base.as_ptr() as *const u8).wrapping_add(idx as usize);
                    Self::read_value_from_ptr(addr, dst_kind)
                };
                frame.registers.set(dst.0, val);
            }
            Opcode::SetI8 { bytes, index, src } => {
                let base = frame.registers.get(bytes.0);
                let idx = frame.registers.get(index.0).as_i32();
                let src_val = frame.registers.get(src.0);
                if !base.is_null() && !base.is_void() && idx >= 0 {
                    let addr = (base.as_ptr() as *mut u8).wrapping_add(idx as usize);
                    unsafe { *addr = src_val.as_i32() as u8 };
                }
            }
            Opcode::SetI16 { bytes, index, src } => {
                let base = frame.registers.get(bytes.0);
                let idx = frame.registers.get(index.0).as_i32();
                let src_val = frame.registers.get(src.0);
                if !base.is_null() && !base.is_void() && idx >= 0 {
                    let addr = (base.as_ptr() as *mut u8).wrapping_add(idx as usize);
                    unsafe { *(addr as *mut u16) = src_val.as_i32() as u16 };
                }
            }
            Opcode::SetMem { bytes, index, src } => {
                let base = frame.registers.get(bytes.0);
                let idx = frame.registers.get(index.0).as_i32();
                let src_val = frame.registers.get(src.0);
                let src_kind = bytecode.types[func.regs[src.0 as usize].0].kind;
                if !base.is_null() && !base.is_void() && idx >= 0 {
                    let addr = (base.as_ptr() as *mut u8).wrapping_add(idx as usize);
                    Self::write_value_to_ptr(addr, src_val, src_kind);
                }
            }

            // ===== References =====
            Opcode::Ref { dst, src } => {
                // Store a pointer DIRECTLY to the src register's NanBoxedValue storage.
                //
                // HashLink semantics: native code writes through the ref pointer and the
                // source register is updated in-place (no Unref needed by bytecode).
                //
                // This works because NanBoxedValue is 8 bytes and, on little-endian,
                // the i32 payload occupies the low 4 bytes. A native c_int write of N
                // lands in bytes 0-3 → `reg.as_i32()` returns N immediately after return.
                //
                // The pointer is stable for the duration of the native call: pushing new
                // frames may move Vec<InterpreterFrame> but the inner Vec<NanBoxedValue>
                // data (separate heap allocation) does not move.
                let slot = frame.registers.slot_ptr(src.0) as usize;
                let ref_ptr = NanBoxedValue::from_ptr(slot);
                frame.registers.set(dst.0, ref_ptr);
            }
            Opcode::Unref { dst, src } => {
                // Dereference a Ref pointer to read back the value.
                let ptr_val = frame.registers.get(src.0);
                let ptr = ptr_val.as_ptr() as *const i64;
                if !ptr.is_null() {
                    let val = unsafe { *ptr };
                    let dst_kind = bytecode.types[func.regs[dst.0 as usize].0].kind;
                    let result = match dst_kind {
                        hl::hl_type_kind_HI32 | hl::hl_type_kind_HUI8 | hl::hl_type_kind_HUI16 => {
                            NanBoxedValue::from_i32(val as i32)
                        }
                        hl::hl_type_kind_HF64 | hl::hl_type_kind_HF32 => {
                            NanBoxedValue::from_f64(f64::from_bits(val as u64))
                        }
                        // Read low 32 bits: native writes a c_int (0/1) at byte offset 0.
                        // Must NOT check the full i64 because NAN_TAG bits are always nonzero.
                        hl::hl_type_kind_HBOOL => NanBoxedValue::from_bool((val as i32) != 0),
                        _ => NanBoxedValue::from_ptr(val as usize),
                    };
                    frame.registers.set(dst.0, result);
                } else {
                    frame.registers.set(dst.0, NanBoxedValue::null());
                }
            }
            Opcode::Setref { dst, value } => {
                // Write through a Ref pointer: *dst = value.
                // The pointer is the address of another register's NanBoxedValue storage.
                // Write the full NanBoxedValue so the tag bits are preserved.
                let ptr_val = frame.registers.get(dst.0);
                let ptr = ptr_val.as_ptr() as *mut NanBoxedValue;
                if !ptr.is_null() {
                    let val = frame.registers.get(value.0);
                    unsafe { *ptr = val };
                }
            }

            // ===== Enums =====
            Opcode::MakeEnum {
                dst,
                construct,
                args,
            } => {
                let type_idx = func.regs[dst.0 as usize].0;
                let c_type_ptr = self.c_type_factory.get(type_idx);
                let fn_alloc_enum = self.fn_alloc_enum;
                let val = Self::alloc_enum_value(fn_alloc_enum, c_type_ptr, construct.0 as i32);
                if !val.is_null() {
                    // Write each argument at its construct offset
                    unsafe {
                        let tenum = (*c_type_ptr).__bindgen_anon_1.tenum;
                        let c = &*(*tenum).constructs.add(construct.0);
                        let base = val as *mut u8;
                        for (i, arg_reg) in args.iter().enumerate() {
                            if i >= c.nparams as usize {
                                break;
                            }
                            let offset = *c.offsets.add(i) as usize;
                            let arg_val = frame.registers.get(arg_reg.0);
                            let param_kind = (*(*c.params.add(i))).kind;
                            Self::write_value_to_ptr(base.add(offset), arg_val, param_kind);
                        }
                    }
                }
                frame.registers.set(
                    dst.0,
                    if val.is_null() {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(val as usize)
                    },
                );
            }
            Opcode::EnumAlloc { dst, construct } => {
                let type_idx = func.regs[dst.0 as usize].0;
                let c_type_ptr = self.c_type_factory.get(type_idx);
                let fn_alloc_enum = self.fn_alloc_enum;
                let val = Self::alloc_enum_value(fn_alloc_enum, c_type_ptr, construct.0 as i32);
                frame.registers.set(
                    dst.0,
                    if val.is_null() {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(val as usize)
                    },
                );
            }
            Opcode::EnumIndex { dst, value } => {
                let val = frame.registers.get(value.0);
                let index = if val.is_null() || val.is_void() {
                    0i32
                } else {
                    // venum layout: t@0 (8 bytes), index@8 (i32)
                    unsafe { *(val.as_ptr() as *const u8).add(8).cast::<i32>() }
                };
                frame.registers.set(dst.0, NanBoxedValue::from_i32(index));
            }
            Opcode::EnumField {
                dst,
                value,
                construct,
                field,
            } => {
                let val = frame.registers.get(value.0);
                let type_idx = func.regs[value.0 as usize].0;
                let c_type_ptr = self.c_type_factory.get(type_idx);
                let result = if val.is_null() || val.is_void() || c_type_ptr.is_null() {
                    NanBoxedValue::null()
                } else {
                    unsafe {
                        let tenum = (*c_type_ptr).__bindgen_anon_1.tenum;
                        if tenum.is_null() || construct.0 >= (*tenum).nconstructs as usize {
                            NanBoxedValue::null()
                        } else {
                            let c = &*(*tenum).constructs.add(construct.0);
                            if field.0 >= c.nparams as usize {
                                NanBoxedValue::null()
                            } else {
                                let offset = *c.offsets.add(field.0) as usize;
                                let param_kind = (*(*c.params.add(field.0))).kind;
                                let base = val.as_ptr() as *const u8;
                                Self::read_value_from_ptr(base.add(offset), param_kind)
                            }
                        }
                    }
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::SetEnumField { value, field, src } => {
                let val = frame.registers.get(value.0);
                let src_val = frame.registers.get(src.0);
                let type_idx = func.regs[value.0 as usize].0;
                let c_type_ptr = self.c_type_factory.get(type_idx);
                if !val.is_null() && !val.is_void() && !c_type_ptr.is_null() {
                    unsafe {
                        let tenum = (*c_type_ptr).__bindgen_anon_1.tenum;
                        if !tenum.is_null() {
                            // Get construct index from the actual venum value
                            let construct_idx =
                                *(val.as_ptr() as *const u8).add(8).cast::<i32>() as usize;
                            if construct_idx < (*tenum).nconstructs as usize {
                                let c = &*(*tenum).constructs.add(construct_idx);
                                if field.0 < c.nparams as usize {
                                    let offset = *c.offsets.add(field.0) as usize;
                                    let param_kind = (*(*c.params.add(field.0))).kind;
                                    let base = val.as_ptr() as *mut u8;
                                    Self::write_value_to_ptr(base.add(offset), src_val, param_kind);
                                }
                            }
                        }
                    }
                }
            }

            // ===== Exception Handling =====
            Opcode::Trap { exc, offset } => {
                let target_pc = (frame.pc as i64 + 1 + *offset as i64) as usize;
                frame.trap_stack.push((target_pc, exc.0));
            }
            Opcode::EndTrap { exc } => {
                frame.trap_stack.pop();
                frame.registers.set(exc.0, NanBoxedValue::null());
            }
            Opcode::Throw { exc } => {
                let val = frame.registers.get(exc.0);
                if let Some((target_pc, exc_reg)) = frame.trap_stack.pop() {
                    frame.registers.set(exc_reg, val);
                    return Ok(StepResult::JumpAbs(target_pc));
                } else {
                    return Err(anyhow::Error::new(self.format_hl_exception(val)));
                }
            }
            Opcode::Rethrow { exc } => {
                let val = frame.registers.get(exc.0);
                if let Some((target_pc, exc_reg)) = frame.trap_stack.pop() {
                    frame.registers.set(exc_reg, val);
                    return Ok(StepResult::JumpAbs(target_pc));
                } else {
                    return Err(anyhow::Error::new(self.format_hl_exception(val)));
                }
            }
            Opcode::NullCheck { reg } => {
                let val = frame.registers.get(reg.0);
                if val.is_null() {
                    let pc = frame.pc;
                    let fname = func.name();
                    // Show surrounding opcodes for debugging context
                    let start = 0;
                    let end = func.ops.len().min(120);
                    let context: Vec<String> = (start..end)
                        .map(|i| {
                            let marker = if i == pc { ">>>" } else { "   " };
                            format!("{} pc={}: {:?}", marker, i, func.ops[i])
                        })
                        .collect();
                    return Err(anyhow!(
                        "Null pointer access in {} at pc={} reg=r{} (type_kind={})\nOpcodes:\n{}",
                        fname,
                        pc,
                        reg.0,
                        bytecode.types[func.regs[reg.0 as usize].0].kind,
                        context.join("\n")
                    ));
                }
            }

            // ===== Misc =====
            Opcode::RefData { dst, src } => {
                let val = frame.registers.get(src.0);
                frame.registers.set(dst.0, val);
            }
            Opcode::RefOffset { dst, reg, offset } => {
                let base = frame.registers.get(reg.0);
                let off = frame.registers.get(offset.0);
                let result = NanBoxedValue::from_ptr(base.as_ptr() + off.as_i32() as usize);
                frame.registers.set(dst.0, result);
            }
            Opcode::Prefetch { .. } => {
                // No-op on interpreter
            }
            Opcode::Asm { .. } => {
                // No-op on interpreter (x86 specific)
            }
        }

        Ok(StepResult::Continue)
    }

    /// Helper: perform integer binary op on two registers.
    fn int_binop(
        &mut self,
        func: &HLFunction,
        op: IntBinOp,
        dst: u32,
        a: u32,
        b: u32,
    ) -> Result<()> {
        let frame = self.stack.last_mut().unwrap();
        let va = frame.registers.get(a);
        let vb = frame.registers.get(b);
        let result = va.binary_int_op(vb, op).ok_or_else(|| {
            anyhow!(
                "{:?}: incompatible types {:?}, {:?} in {} at pc={} (dst=r{}, a=r{}, b=r{})",
                op,
                va,
                vb,
                func.name(),
                frame.pc,
                dst,
                a,
                b
            )
        })?;
        frame.registers.set(dst, result);
        Ok(())
    }

    /// Helper: compare two register values.
    fn compare_regs(
        &self,
        bytecode: &DecodedBytecode,
        func_idx: usize,
        a: u32,
        b: u32,
        op: CmpOp,
    ) -> bool {
        let frame = self.stack.last().unwrap();
        let va = frame.registers.get(a);
        let vb = frame.registers.get(b);
        let func = &bytecode.functions[func_idx];
        let ak = bytecode.types[func.regs[a as usize].0].kind;
        let bk = bytecode.types[func.regs[b as usize].0].kind;
        if let Some(result) = unsafe {
            self.try_compare_nullable_operands(
                bytecode, func, a as usize, va, ak, b as usize, vb, bk, op,
            )
        } {
            if std::env::var("ASH_TRACE_EQ").is_ok() {
                eprintln!(
                    "[CMP-HNULL] f{} op={:?} ak={} bk={} va={:?} vb={:?} -> {}",
                    func_idx, op, ak, bk, va, vb, result
                );
            }
            return result;
        }
        if op == CmpOp::Eq || op == CmpOp::NotEq {
            if ak == hl::hl_type_kind_HBYTES && bk == hl::hl_type_kind_HBYTES {
                let pa = if va.is_null() || va.is_void() {
                    std::ptr::null()
                } else {
                    va.as_ptr() as *const u16
                };
                let pb = if vb.is_null() || vb.is_void() {
                    std::ptr::null()
                } else {
                    vb.as_ptr() as *const u16
                };
                let eq = unsafe { Self::utf16z_eq(pa, pb) };
                if std::env::var("ASH_TRACE_EQ").is_ok() {
                    eprintln!(
                        "[CMP] f{} op={:?} ak={} bk={} (bytes) -> {}",
                        func_idx, op, ak, bk, eq
                    );
                }
                return if op == CmpOp::Eq { eq } else { !eq };
            }
            if ak == hl::hl_type_kind_HOBJ && bk == hl::hl_type_kind_HOBJ {
                let pa = if va.is_null() || va.is_void() {
                    std::ptr::null_mut()
                } else {
                    va.as_ptr() as *mut hl::vdynamic
                };
                let pb = if vb.is_null() || vb.is_void() {
                    std::ptr::null_mut()
                } else {
                    vb.as_ptr() as *mut hl::vdynamic
                };
                if !pa.is_null() && !pb.is_null() {
                    let ta_name = self.dynamic_type_name(pa);
                    let tb_name = self.dynamic_type_name(pb);
                    if std::env::var("ASH_TRACE_EQ").is_ok() {
                        eprintln!(
                            "[CMP-OBJ] f{} op={:?} ta={:?} tb={:?} pa={:#x} pb={:#x}",
                            func_idx,
                            op,
                            ta_name,
                            tb_name,
                            va.as_ptr(),
                            vb.as_ptr()
                        );
                    }
                    if ta_name == tb_name
                        && matches!(ta_name.as_deref(), Some("String") | Some("S"))
                    {
                        let sa = unsafe {
                            self.try_extract_string_object_raw(va.as_ptr() as *mut c_void)
                        };
                        let sb = unsafe {
                            self.try_extract_string_object_raw(vb.as_ptr() as *mut c_void)
                        };
                        if std::env::var("ASH_TRACE_EQ").is_ok() {
                            eprintln!(
                                "[CMP-OBJ] f{} string-extract sa={} sb={}",
                                func_idx,
                                sa.is_some(),
                                sb.is_some()
                            );
                        }
                        if let (Some((ab, al)), Some((bb, bl))) = (sa, sb) {
                            let eq = al == bl && unsafe { Self::utf16_len_eq(ab, bb, al as usize) };
                            if std::env::var("ASH_TRACE_EQ").is_ok() {
                                eprintln!(
                                    "[CMP] f{} op={:?} ak={} bk={} (string-obj) -> {}",
                                    func_idx, op, ak, bk, eq
                                );
                            }
                            return if op == CmpOp::Eq { eq } else { !eq };
                        }
                    }
                }
            }
            if ak == hl::hl_type_kind_HDYN && bk == hl::hl_type_kind_HDYN {
                let pa = if va.is_null() || va.is_void() {
                    std::ptr::null_mut()
                } else {
                    va.as_ptr() as *mut hl::vdynamic
                };
                let pb = if vb.is_null() || vb.is_void() {
                    std::ptr::null_mut()
                } else {
                    vb.as_ptr() as *mut hl::vdynamic
                };
                let eq = unsafe { self.dynamic_eq(pa, pb) };
                if std::env::var("ASH_TRACE_EQ").is_ok() {
                    eprintln!(
                        "[CMP] f{} op={:?} ak={} bk={} (dyn) -> {}",
                        func_idx, op, ak, bk, eq
                    );
                    if !eq {
                        let ka_dyn = if pa.is_null() || unsafe { (*pa).t.is_null() } {
                            0
                        } else {
                            unsafe { (*(*pa).t).kind }
                        };
                        let kb_dyn = if pb.is_null() || unsafe { (*pb).t.is_null() } {
                            0
                        } else {
                            unsafe { (*(*pb).t).kind }
                        };
                        eprintln!(
                            "[CMP_DYN] ka_dyn={} kb_dyn={} ta={:?} tb={:?} sa={:?} sb={:?}",
                            ka_dyn,
                            kb_dyn,
                            self.dynamic_type_name(pa),
                            self.dynamic_type_name(pb),
                            self.value_to_string(pa),
                            self.value_to_string(pb)
                        );
                    }
                }
                return if op == CmpOp::Eq { eq } else { !eq };
            }
        }
        let result = va.compare(vb, op).unwrap_or(false);
        if std::env::var("ASH_TRACE_EQ").is_ok() && (op == CmpOp::Eq || op == CmpOp::NotEq) {
            eprintln!(
                "[CMP] f{} op={:?} ak={} bk={} va={:?} vb={:?} -> {}",
                func_idx, op, ak, bk, va, vb, result
            );
        }
        result
    }

    #[allow(clippy::too_many_arguments)]
    unsafe fn try_compare_nullable_operands(
        &self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        a_idx: usize,
        va: NanBoxedValue,
        ak: u32,
        b_idx: usize,
        vb: NanBoxedValue,
        bk: u32,
        op: CmpOp,
    ) -> Option<bool> {
        if ak != hl::hl_type_kind_HNULL && bk != hl::hl_type_kind_HNULL {
            return None;
        }

        let (av, ak_eff) =
            self.normalize_nullable_compare_operand(bytecode, func, a_idx, ak, va)?;
        let (bv, bk_eff) =
            self.normalize_nullable_compare_operand(bytecode, func, b_idx, bk, vb)?;

        if av.is_none() || bv.is_none() {
            let eq = av.is_none() && bv.is_none();
            return Some(match op {
                CmpOp::Eq => eq,
                CmpOp::NotEq => !eq,
                _ => false,
            });
        }

        let av = av.unwrap();
        let bv = bv.unwrap();
        if let Some(result) = Self::compare_numeric_values(av, ak_eff, bv, bk_eff, op) {
            return Some(result);
        }

        if op == CmpOp::Eq || op == CmpOp::NotEq {
            if let Some(result) = av.compare(bv, op) {
                return Some(result);
            }
        }

        None
    }

    unsafe fn normalize_nullable_compare_operand(
        &self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        reg_idx: usize,
        reg_kind: u32,
        val: NanBoxedValue,
    ) -> Option<(Option<NanBoxedValue>, u32)> {
        if reg_kind != hl::hl_type_kind_HNULL {
            return Some((Some(val), reg_kind));
        }
        if val.is_null() || val.is_void() || (val.is_ptr() && val.as_ptr() == 0) {
            return Some((None, reg_kind));
        }
        if !val.is_ptr() {
            return Some((Some(val), reg_kind));
        }

        let reg_type_idx = match func.regs.get(reg_idx) {
            Some(r) => r.0,
            None => return Some((Some(val), reg_kind)),
        };
        let reg_type = match bytecode.types.get(reg_type_idx) {
            Some(t) => t,
            None => return Some((Some(val), reg_kind)),
        };
        let tparam_idx = match reg_type.tparam.as_ref() {
            Some(tp) => tp.0,
            None => return Some((Some(val), reg_kind)),
        };
        let inner_kind = match bytecode.types.get(tparam_idx) {
            Some(t) => t.kind,
            None => return Some((Some(val), reg_kind)),
        };

        if !Self::is_primitive_or_bytes_kind(inner_kind) {
            return Some((Some(val), reg_kind));
        }

        let d = val.as_ptr() as *mut hl::vdynamic;
        if d.is_null() {
            return Some((None, inner_kind));
        }
        if let Some(unboxed) = Self::unbox_dynamic_to_kind(d, inner_kind) {
            if unboxed.is_null() || unboxed.is_void() {
                return Some((None, inner_kind));
            }
            return Some((Some(unboxed), inner_kind));
        }

        Some((Some(val), reg_kind))
    }

    fn compare_numeric_values(
        av: NanBoxedValue,
        ak: u32,
        bv: NanBoxedValue,
        bk: u32,
        op: CmpOp,
    ) -> Option<bool> {
        if !Self::is_numeric_or_bool_kind(ak) || !Self::is_numeric_or_bool_kind(bk) {
            return None;
        }

        let has_float = ak == hl::hl_type_kind_HF32
            || ak == hl::hl_type_kind_HF64
            || bk == hl::hl_type_kind_HF32
            || bk == hl::hl_type_kind_HF64;

        if has_float {
            let l = Self::numeric_as_f64(av, ak)?;
            let r = Self::numeric_as_f64(bv, bk)?;
            return Some(match op {
                CmpOp::SLt | CmpOp::ULt => l < r,
                CmpOp::SGte | CmpOp::UGte => l >= r,
                CmpOp::SGt => l > r,
                CmpOp::SLte => l <= r,
                CmpOp::Eq => l == r,
                CmpOp::NotEq => l != r,
            });
        }

        match op {
            CmpOp::ULt | CmpOp::UGte => {
                let l = Self::numeric_as_u64(av, ak)?;
                let r = Self::numeric_as_u64(bv, bk)?;
                Some(match op {
                    CmpOp::ULt => l < r,
                    CmpOp::UGte => l >= r,
                    _ => unreachable!(),
                })
            }
            CmpOp::SLt | CmpOp::SGte | CmpOp::SGt | CmpOp::SLte | CmpOp::Eq | CmpOp::NotEq => {
                let l = Self::numeric_as_i64(av, ak)?;
                let r = Self::numeric_as_i64(bv, bk)?;
                Some(match op {
                    CmpOp::SLt => l < r,
                    CmpOp::SGte => l >= r,
                    CmpOp::SGt => l > r,
                    CmpOp::SLte => l <= r,
                    CmpOp::Eq => l == r,
                    CmpOp::NotEq => l != r,
                    _ => unreachable!(),
                })
            }
        }
    }

    fn numeric_as_f64(v: NanBoxedValue, kind: u32) -> Option<f64> {
        match kind {
            k if k == hl::hl_type_kind_HI32 => Some(v.as_i32() as f64),
            k if k == hl::hl_type_kind_HUI8 => Some((v.as_i32() as u8) as f64),
            k if k == hl::hl_type_kind_HUI16 => Some((v.as_i32() as u16) as f64),
            k if k == hl::hl_type_kind_HI64 => Some(v.as_i64_lossy() as f64),
            k if k == hl::hl_type_kind_HF32 || k == hl::hl_type_kind_HF64 => Some(v.as_f64()),
            k if k == hl::hl_type_kind_HBOOL => Some(if v.as_bool() { 1.0 } else { 0.0 }),
            _ => None,
        }
    }

    fn numeric_as_i64(v: NanBoxedValue, kind: u32) -> Option<i64> {
        match kind {
            k if k == hl::hl_type_kind_HI32 => Some(v.as_i32() as i64),
            k if k == hl::hl_type_kind_HUI8 => Some((v.as_i32() as u8) as i64),
            k if k == hl::hl_type_kind_HUI16 => Some((v.as_i32() as u16) as i64),
            k if k == hl::hl_type_kind_HI64 => Some(v.as_i64_lossy()),
            k if k == hl::hl_type_kind_HBOOL => Some(if v.as_bool() { 1 } else { 0 }),
            _ => None,
        }
    }

    fn numeric_as_u64(v: NanBoxedValue, kind: u32) -> Option<u64> {
        match kind {
            k if k == hl::hl_type_kind_HI32 => Some((v.as_i32() as u32) as u64),
            k if k == hl::hl_type_kind_HUI8 => Some((v.as_i32() as u8) as u64),
            k if k == hl::hl_type_kind_HUI16 => Some((v.as_i32() as u16) as u64),
            k if k == hl::hl_type_kind_HI64 => Some(v.as_i64_lossy() as u64),
            k if k == hl::hl_type_kind_HBOOL => Some(if v.as_bool() { 1 } else { 0 }),
            _ => None,
        }
    }

    unsafe fn utf16z_eq(a: *const u16, b: *const u16) -> bool {
        if a == b {
            return true;
        }
        if a.is_null() || b.is_null() {
            return false;
        }
        let mut i = 0usize;
        loop {
            let ca = *a.add(i);
            let cb = *b.add(i);
            if ca != cb {
                return false;
            }
            if ca == 0 {
                return true;
            }
            i += 1;
        }
    }

    unsafe fn utf16_len_eq(a: *const u16, b: *const u16, len: usize) -> bool {
        if a.is_null() || b.is_null() {
            return false;
        }
        for i in 0..len {
            if *a.add(i) != *b.add(i) {
                return false;
            }
        }
        true
    }

    unsafe fn try_extract_string_object(&self, d: *mut hl::vdynamic) -> Option<(*const u16, i32)> {
        if d.is_null() || self.fn_obj_get_field.is_null() {
            return None;
        }
        let get_field: FnObjGetField = std::mem::transmute(self.fn_obj_get_field);
        let h_len = self.hash_literal_name("length");
        let h_bytes = self.hash_literal_name("bytes");
        let len_dyn = get_field(d, h_len);
        let bytes_dyn = get_field(d, h_bytes);
        if len_dyn.is_null() || bytes_dyn.is_null() {
            return None;
        }
        if (*len_dyn).t.is_null() || (*bytes_dyn).t.is_null() {
            return None;
        }
        if (*(*len_dyn).t).kind != hl::hl_type_kind_HI32 {
            return None;
        }
        if (*(*bytes_dyn).t).kind != hl::hl_type_kind_HBYTES {
            return None;
        }
        let len = (*len_dyn).v.i;
        let bytes = (*bytes_dyn).v.bytes as *const u16;
        if len < 0 || bytes.is_null() {
            return None;
        }
        Some((bytes, len))
    }

    unsafe fn try_extract_string_object_raw(
        &self,
        obj_ptr: *mut c_void,
    ) -> Option<(*const u16, i32)> {
        if obj_ptr.is_null() || self.fn_get_obj_rt.is_null() {
            return None;
        }
        let type_ptr = *(obj_ptr as *const *mut hl::hl_type);
        if type_ptr.is_null() || (*type_ptr).kind != hl::hl_type_kind_HOBJ {
            return None;
        }
        let bytes_val = Self::read_obj_field(
            obj_ptr as *mut u8,
            0,
            hl::hl_type_kind_HBYTES,
            type_ptr as *mut c_void,
            hl::hl_type_kind_HOBJ,
            self.fn_get_obj_rt,
        );
        let len_val = Self::read_obj_field(
            obj_ptr as *mut u8,
            1,
            hl::hl_type_kind_HI32,
            type_ptr as *mut c_void,
            hl::hl_type_kind_HOBJ,
            self.fn_get_obj_rt,
        );
        if bytes_val.is_null() || len_val.is_null() || len_val.is_void() {
            return None;
        }
        let bytes = bytes_val.as_ptr() as *const u16;
        let len = len_val.as_i32();
        if bytes.is_null() || len < 0 {
            return None;
        }
        Some((bytes, len))
    }

    unsafe fn dynamic_eq(&self, a: *mut hl::vdynamic, b: *mut hl::vdynamic) -> bool {
        if a == b {
            return true;
        }
        if a.is_null() || b.is_null() {
            return false;
        }
        let ta = (*a).t;
        let tb = (*b).t;
        if ta.is_null() || tb.is_null() {
            return false;
        }
        let ka = (*ta).kind;
        let kb = (*tb).kind;
        if ka == kb {
            return match ka {
                k if k == hl::hl_type_kind_HI32 => (*a).v.i == (*b).v.i,
                k if k == hl::hl_type_kind_HUI8 => (*a).v.ui8 == (*b).v.ui8,
                k if k == hl::hl_type_kind_HUI16 => (*a).v.ui16 == (*b).v.ui16,
                k if k == hl::hl_type_kind_HI64 => (*a).v.i64_ == (*b).v.i64_,
                k if k == hl::hl_type_kind_HF32 => (*a).v.f == (*b).v.f,
                k if k == hl::hl_type_kind_HF64 => (*a).v.d == (*b).v.d,
                k if k == hl::hl_type_kind_HBOOL => (*a).v.b == (*b).v.b,
                k if k == hl::hl_type_kind_HBYTES => {
                    Self::utf16z_eq((*a).v.bytes as *const u16, (*b).v.bytes as *const u16)
                }
                _ => {
                    if ka == hl::hl_type_kind_HOBJ {
                        let ta_name = self.dynamic_type_name(a);
                        let tb_name = self.dynamic_type_name(b);
                        if ta_name == tb_name
                            && matches!(ta_name.as_deref(), Some("String") | Some("S"))
                        {
                            if let (Some(sa), Some(sb)) =
                                (self.value_to_string(a), self.value_to_string(b))
                            {
                                return sa == sb;
                            }
                        }
                        let sa = self.try_extract_string_object(a);
                        let sb = self.try_extract_string_object(b);
                        if let (Some((ab, al)), Some((bb, bl))) = (sa, sb) {
                            return al == bl && Self::utf16_len_eq(ab, bb, al as usize);
                        }
                    }
                    (*a).v.ptr == (*b).v.ptr
                }
            };
        }
        // Cross-kind numeric equality (e.g. Int dynamic vs Float dynamic)
        let a_num = match ka {
            k if k == hl::hl_type_kind_HI32 => Some((*a).v.i as f64),
            k if k == hl::hl_type_kind_HUI8 => Some((*a).v.ui8 as f64),
            k if k == hl::hl_type_kind_HUI16 => Some((*a).v.ui16 as f64),
            k if k == hl::hl_type_kind_HI64 => Some((*a).v.i64_ as f64),
            k if k == hl::hl_type_kind_HF32 => Some((*a).v.f as f64),
            k if k == hl::hl_type_kind_HF64 => Some((*a).v.d),
            _ => None,
        };
        let b_num = match kb {
            k if k == hl::hl_type_kind_HI32 => Some((*b).v.i as f64),
            k if k == hl::hl_type_kind_HUI8 => Some((*b).v.ui8 as f64),
            k if k == hl::hl_type_kind_HUI16 => Some((*b).v.ui16 as f64),
            k if k == hl::hl_type_kind_HI64 => Some((*b).v.i64_ as f64),
            k if k == hl::hl_type_kind_HF32 => Some((*b).v.f as f64),
            k if k == hl::hl_type_kind_HF64 => Some((*b).v.d),
            _ => None,
        };
        match (a_num, b_num) {
            (Some(x), Some(y)) => x == y,
            _ => false,
        }
    }

    unsafe fn unbox_dynamic_to_kind(d: *mut hl::vdynamic, dst_kind: u32) -> Option<NanBoxedValue> {
        if d.is_null() || (*d).t.is_null() {
            return None;
        }
        let sk = (*(*d).t).kind;
        let as_i64 = match sk {
            hl::hl_type_kind_HI32 => Some((*d).v.i as i64),
            hl::hl_type_kind_HUI8 => Some((*d).v.ui8 as i64),
            hl::hl_type_kind_HUI16 => Some((*d).v.ui16 as i64),
            hl::hl_type_kind_HI64 => Some((*d).v.i64_),
            hl::hl_type_kind_HF32 => Some((*d).v.f as i64),
            hl::hl_type_kind_HF64 => Some((*d).v.d as i64),
            hl::hl_type_kind_HBOOL => Some(if (*d).v.b { 1 } else { 0 }),
            _ => None,
        };
        let as_f64 = match sk {
            hl::hl_type_kind_HI32 => Some((*d).v.i as f64),
            hl::hl_type_kind_HUI8 => Some((*d).v.ui8 as f64),
            hl::hl_type_kind_HUI16 => Some((*d).v.ui16 as f64),
            hl::hl_type_kind_HI64 => Some((*d).v.i64_ as f64),
            hl::hl_type_kind_HF32 => Some((*d).v.f as f64),
            hl::hl_type_kind_HF64 => Some((*d).v.d),
            hl::hl_type_kind_HBOOL => Some(if (*d).v.b { 1.0 } else { 0.0 }),
            _ => None,
        };
        match dst_kind {
            hl::hl_type_kind_HI32 => as_i64.map(|v| NanBoxedValue::from_i32(v as i32)),
            hl::hl_type_kind_HUI8 => as_i64.map(|v| NanBoxedValue::from_i32((v as u8) as i32)),
            hl::hl_type_kind_HUI16 => as_i64.map(|v| NanBoxedValue::from_i32((v as u16) as i32)),
            hl::hl_type_kind_HI64 => as_i64.map(NanBoxedValue::from_i64),
            hl::hl_type_kind_HF32 | hl::hl_type_kind_HF64 => as_f64.map(NanBoxedValue::from_f64),
            hl::hl_type_kind_HBOOL => as_i64.map(|v| NanBoxedValue::from_bool(v != 0)),
            hl::hl_type_kind_HBYTES => {
                if sk == hl::hl_type_kind_HBYTES {
                    Some(NanBoxedValue::from_bytes_ptr((*d).v.bytes as usize))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Call a native function via FFI.
    fn call_native(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        native_idx: usize,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        let native = &bytecode.natives[native_idx];
        let func_name = format!("hlp_{}", native.name);
        let debug_native = std::env::var("ASH_DBG_NATIVE").is_ok();
        if debug_native
            && (native.name.contains("compare")
                || native.name.contains("eq")
                || native.name.contains("trim")
                || native.name.contains("date"))
        {
            eprintln!("[NATIVE] {} args={} vals={:?}", func_name, args.len(), args);
        }
        let debug_dyn = std::env::var("ASH_DBG_DYN").is_ok();
        if debug_dyn
            && (native.name == "hash"
                || native.name == "obj_get_field"
                || native.name == "obj_set_field"
                || native.name == "obj_has_field"
                || native.name == "obj_delete_field"
                || native.name == "no_closure"
                || native.name == "get_closure_value"
                || native.name == "call_method")
        {
            eprintln!(
                "[NATIVE-DYN] {} args={} vals={:?}",
                func_name,
                args.len(),
                args
            );
            if (native.name == "obj_get_field"
                || native.name == "obj_set_field"
                || native.name == "obj_has_field"
                || native.name == "obj_delete_field"
                || native.name == "no_closure"
                || native.name == "get_closure_value"
                || native.name == "call_method")
                && !args.is_empty()
                && args[0].is_ptr()
                && args[0].as_ptr() != 0
            {
                let d = args[0].as_ptr() as *mut hl::vdynamic;
                unsafe {
                    if !d.is_null() && !(*d).t.is_null() {
                        eprintln!(
                            "[NATIVE-DYN] {} obj_kind={} obj_t={:p}",
                            func_name,
                            (*(*d).t).kind,
                            (*d).t
                        );
                    }
                    if native.name == "obj_set_field"
                        && args.len() >= 3
                        && args[2].is_ptr()
                        && args[2].as_ptr() != 0
                    {
                        let v = args[2].as_ptr() as *mut hl::vdynamic;
                        if !v.is_null() && !(*v).t.is_null() {
                            eprintln!(
                                "[NATIVE-DYN] {} val_kind={} val_t={:p}",
                                func_name,
                                (*(*v).t).kind,
                                (*v).t
                            );
                        }
                    }
                }
            }
        }

        // Intercept sort natives: they call back into bytecode closures via C function pointers,
        // which doesn't work in interpreter mode. Implement sorting here instead.
        match native.name.as_str() {
            "bsort_i32" => return self.sort_bytes_i32(bytecode, native_resolver, args),
            "bsort_f64" => return self.sort_bytes_f64(bytecode, native_resolver, args),
            "bsort_i64" => return self.sort_bytes_i64(bytecode, native_resolver, args),
            "call_method" => {
                if let Some(v) =
                    self.try_handle_call_method_native(bytecode, native_resolver, args)?
                {
                    return Ok(v);
                }
            }
            // Virtual object field operations are intercepted so interpreter-side
            // HVIRTUAL fallback storage stays consistent with Reflect/hl.Api calls.
            "obj_get_field" => {
                if let Some(v) = self.try_handle_virtual_obj_get_field(args) {
                    return Ok(v);
                }
            }
            "obj_set_field" => {
                if let Some(v) = self.try_handle_virtual_obj_set_field(args) {
                    return Ok(v);
                }
            }
            "obj_has_field" => {
                if let Some(v) = self.try_handle_virtual_obj_has_field(args) {
                    return Ok(v);
                }
            }
            "obj_delete_field" => {
                if let Some(v) = self.try_handle_virtual_obj_delete_field(args) {
                    return Ok(v);
                }
            }
            _ => {}
        }

        // Resolve the native function pointer
        let func_ptr = native_resolver.resolve_function(&native.lib, &func_name)?;

        // Get the function type signature for type-aware marshaling
        let type_fun = bytecode.types[native.type_.0]
            .fun
            .as_ref()
            .ok_or_else(|| anyhow!("Native {} has no function type", func_name))?;

        // Get return type kind for wrapping the result
        let ret_kind = bytecode.types[type_fun.ret.0].kind;

        // Get argument type kinds for extraction
        let arg_kinds: Vec<u32> = type_fun
            .args
            .iter()
            .map(|a| bytecode.types[a.0].kind)
            .collect();
        if debug_dyn
            && (native.name == "obj_get_field"
                || native.name == "obj_set_field"
                || native.name == "obj_has_field"
                || native.name == "obj_delete_field"
                || native.name == "no_closure"
                || native.name == "get_closure_value"
                || native.name == "call_method")
        {
            eprintln!(
                "[NATIVE-DYN] {} arg_kinds={:?} ret_kind={}",
                func_name, arg_kinds, ret_kind
            );
        }

        // Check if any argument or return type involves floats.
        // On ARM64, floats use separate FP registers (d0-d7) vs integer registers (x0-x7),
        // so we must use typed dispatch with explicit f64 in the right positions.
        let is_float_kind = |k: u32| k == hl::hl_type_kind_HF32 || k == hl::hl_type_kind_HF64;
        let ret_is_float = is_float_kind(ret_kind);
        let float_mask: u32 = arg_kinds.iter().enumerate().fold(0u32, |acc, (i, &k)| {
            if is_float_kind(k) {
                acc | (1 << i)
            } else {
                acc
            }
        });

        // Set up a setjmp/longjmp trap so hlp_throw can propagate through native C ABI safely.
        // This covers BOTH float and integer dispatch paths.
        let fn_setup_trap = self.fn_setup_trap_jit;
        let fn_remove_trap = self.fn_remove_trap_jit;
        let fn_get_exc = self.fn_get_exc_value;
        let fn_clear_exc = self.fn_clear_exc_value;
        let mut trap_installed = false;
        if !fn_setup_trap.is_null() {
            type FnSetupTrap = unsafe extern "C" fn() -> *mut c_void;
            let setup: FnSetupTrap = unsafe { std::mem::transmute(fn_setup_trap) };
            let jmp_buf = unsafe { setup() };
            if !jmp_buf.is_null() {
                trap_installed = true;
                let jumped = unsafe { call_setjmp_opaque(jmp_buf) };
                if jumped != 0 {
                    if !fn_get_exc.is_null() {
                        type FnGetExc = unsafe extern "C" fn() -> *mut c_void;
                        let exc_ptr =
                            unsafe { (std::mem::transmute::<*mut c_void, FnGetExc>(fn_get_exc))() };
                        if !exc_ptr.is_null() {
                            if !fn_clear_exc.is_null() {
                                type FnClearExc = unsafe extern "C" fn();
                                unsafe {
                                    (std::mem::transmute::<*mut c_void, FnClearExc>(fn_clear_exc))()
                                };
                            }
                            return Err(anyhow::Error::new(
                                self.format_hl_exception(NanBoxedValue::from_ptr(exc_ptr as usize)),
                            ));
                        }
                    }
                    return Err(anyhow!(
                        "Native longjmp without exception value: {}",
                        func_name
                    ));
                }
            }
        }

        if ret_is_float || float_mask != 0 {
            let raw =
                self.dispatch_float_native(func_ptr, args, &arg_kinds, float_mask, ret_is_float);
            if trap_installed && !fn_remove_trap.is_null() {
                type FnRemoveTrap = unsafe extern "C" fn();
                unsafe { (std::mem::transmute::<*mut c_void, FnRemoveTrap>(fn_remove_trap))() };
            }
            return Ok(self.wrap_native_result(raw?, ret_kind));
        }

        // Type-aware argument extraction
        let extract_arg = |idx: usize| -> i64 {
            let kind = if idx < arg_kinds.len() {
                arg_kinds[idx]
            } else {
                0 // HVOID fallback
            };
            self.value_to_i64(args[idx], kind)
        };

        if args.len() > 8 {
            if trap_installed && !fn_remove_trap.is_null() {
                type FnRemoveTrap = unsafe extern "C" fn();
                unsafe { (std::mem::transmute::<*mut c_void, FnRemoveTrap>(fn_remove_trap))() };
            }
            return Err(anyhow!(
                "Native call with {} args not yet supported",
                args.len()
            ));
        }

        // Dispatch based on argument count, using type-aware extraction and wrapping.
        let raw_result: i64 = unsafe {
            match args.len() {
                0 => {
                    let f: unsafe extern "C" fn() -> i64 = std::mem::transmute(func_ptr);
                    f()
                }
                1 => {
                    let f: unsafe extern "C" fn(i64) -> i64 = std::mem::transmute(func_ptr);
                    f(extract_arg(0))
                }
                2 => {
                    let f: unsafe extern "C" fn(i64, i64) -> i64 = std::mem::transmute(func_ptr);
                    f(extract_arg(0), extract_arg(1))
                }
                3 => {
                    let f: unsafe extern "C" fn(i64, i64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(extract_arg(0), extract_arg(1), extract_arg(2))
                }
                4 => {
                    let f: unsafe extern "C" fn(i64, i64, i64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(
                        extract_arg(0),
                        extract_arg(1),
                        extract_arg(2),
                        extract_arg(3),
                    )
                }
                5 => {
                    let f: unsafe extern "C" fn(i64, i64, i64, i64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(
                        extract_arg(0),
                        extract_arg(1),
                        extract_arg(2),
                        extract_arg(3),
                        extract_arg(4),
                    )
                }
                6 => {
                    let f: unsafe extern "C" fn(i64, i64, i64, i64, i64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(
                        extract_arg(0),
                        extract_arg(1),
                        extract_arg(2),
                        extract_arg(3),
                        extract_arg(4),
                        extract_arg(5),
                    )
                }
                7 => {
                    let f: unsafe extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(
                        extract_arg(0),
                        extract_arg(1),
                        extract_arg(2),
                        extract_arg(3),
                        extract_arg(4),
                        extract_arg(5),
                        extract_arg(6),
                    )
                }
                8 => {
                    let f: unsafe extern "C" fn(i64, i64, i64, i64, i64, i64, i64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(
                        extract_arg(0),
                        extract_arg(1),
                        extract_arg(2),
                        extract_arg(3),
                        extract_arg(4),
                        extract_arg(5),
                        extract_arg(6),
                        extract_arg(7),
                    )
                }
                _ => 0i64, // arg count is pre-validated above
            }
        };

        if trap_installed && !fn_remove_trap.is_null() {
            type FnRemoveTrap = unsafe extern "C" fn();
            unsafe { (std::mem::transmute::<*mut c_void, FnRemoveTrap>(fn_remove_trap))() };
        }

        // Wrap return value using the correct NanBoxedValue type
        let wrapped = self.wrap_native_result(raw_result, ret_kind);
        if debug_native
            && (native.name.contains("compare")
                || native.name.contains("eq")
                || native.name.contains("trim")
                || native.name.contains("date"))
        {
            eprintln!(
                "[NATIVE] {} -> raw={} wrapped={:?}",
                func_name, raw_result, wrapped
            );
        }
        if debug_dyn
            && (native.name == "hash"
                || native.name == "obj_get_field"
                || native.name == "obj_set_field"
                || native.name == "obj_has_field"
                || native.name == "obj_delete_field"
                || native.name == "no_closure"
                || native.name == "get_closure_value"
                || native.name == "call_method")
        {
            eprintln!(
                "[NATIVE-DYN] {} -> raw={} wrapped={:?}",
                func_name, raw_result, wrapped
            );
            if wrapped.is_ptr() && wrapped.as_ptr() != 0 {
                let d = wrapped.as_ptr() as *mut hl::vdynamic;
                unsafe {
                    if !d.is_null() && !(*d).t.is_null() {
                        eprintln!(
                            "[NATIVE-DYN] {} result_kind={} result_t={:p}",
                            func_name,
                            (*(*d).t).kind,
                            (*d).t
                        );
                    }
                }
            }
        }
        Ok(wrapped)
    }

    /// Dispatch a native call that involves float arguments or float return value.
    ///
    /// Extract (findex, optional_bound_value) from a closure NanBoxedValue.
    ///
    /// Closures can be stored as:
    /// - TAG_FUNC: just a function index (StaticClosure with no capture)
    /// - TAG_PTR: pointer to a _vclosure struct (InstanceClosure or heap-allocated)
    fn closure_findex_and_value(&self, val: NanBoxedValue) -> (usize, Option<NanBoxedValue>) {
        if val.is_func() {
            (val.as_func_index(), None)
        } else if val.is_ptr() {
            let cl_ptr = val.as_ptr() as *const hl::_vclosure;
            unsafe {
                let stub = (*cl_ptr).fun as usize;
                let findex = stub.wrapping_sub(1);
                let bound = if (*cl_ptr).hasValue != 0 && !(*cl_ptr).value.is_null() {
                    Some(NanBoxedValue::from_ptr((*cl_ptr).value as usize))
                } else {
                    None
                };
                (findex, bound)
            }
        } else {
            // Fallback: treat raw i32 payload as findex
            (val.as_ptr(), None)
        }
    }

    /// Call a closure value (FUNC-tagged or PTR-to-vclosure) with the given arguments.
    /// Prepends the bound value if the closure has one (InstanceClosure pattern).
    fn call_closure_val(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        closure_val: NanBoxedValue,
        args: Vec<NanBoxedValue>,
    ) -> Result<NanBoxedValue> {
        let (findex, bound) = self.closure_findex_and_value(closure_val);
        let mut full_args = args;
        if let Some(v) = bound {
            full_args.insert(0, v);
        }
        self.call_function(bytecode, native_resolver, findex, &full_args)
    }

    fn dynamic_to_value_for_kind(&self, d: *mut hl::vdynamic, dst_kind: u32) -> NanBoxedValue {
        if d.is_null() {
            return NanBoxedValue::null();
        }
        if dst_kind == hl::hl_type_kind_HDYN {
            return NanBoxedValue::from_ptr(d as usize);
        }
        let sk = unsafe {
            if (*d).t.is_null() {
                return NanBoxedValue::null();
            }
            (*(*d).t).kind
        };
        if Self::is_primitive_or_bytes_kind(dst_kind) {
            return unsafe { Self::unbox_dynamic_to_kind(d, dst_kind) }
                .unwrap_or(NanBoxedValue::null());
        }
        if sk == dst_kind {
            match sk {
                hl::hl_type_kind_HOBJ
                | hl::hl_type_kind_HSTRUCT
                | hl::hl_type_kind_HARRAY
                | hl::hl_type_kind_HFUN
                | hl::hl_type_kind_HVIRTUAL
                | hl::hl_type_kind_HDYNOBJ
                | hl::hl_type_kind_HENUM => {
                    return NanBoxedValue::from_ptr(d as usize);
                }
                hl::hl_type_kind_HBYTES => {
                    let p = unsafe { (*d).v.bytes } as usize;
                    return if p == 0 {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(p)
                    };
                }
                _ => {
                    let p = unsafe { (*d).v.ptr } as usize;
                    return if p == 0 {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(p)
                    };
                }
            }
        }
        if sk == hl::hl_type_kind_HBYTES {
            let p = unsafe { (*d).v.bytes } as usize;
            return if p == 0 {
                NanBoxedValue::null()
            } else {
                NanBoxedValue::from_ptr(p)
            };
        }
        let p = unsafe { (*d).v.ptr } as usize;
        if p == 0 {
            NanBoxedValue::null()
        } else {
            NanBoxedValue::from_ptr(p)
        }
    }

    fn closure_arg_kinds_and_ret_type(
        &self,
        bytecode: &DecodedBytecode,
        findex: usize,
    ) -> Option<(Vec<u32>, usize)> {
        if let Some(&fidx) = self.findex_to_func.get(&findex) {
            let t_idx = bytecode.functions[fidx].type_.0;
            let tf = bytecode.types[t_idx].fun.as_ref()?;
            let arg_kinds = tf
                .args
                .iter()
                .map(|a| bytecode.types[a.0].kind)
                .collect::<Vec<_>>();
            return Some((arg_kinds, tf.ret.0));
        }
        if let Some(&nidx) = self.findex_to_native.get(&findex) {
            let t_idx = bytecode.natives[nidx].type_.0;
            let tf = bytecode.types[t_idx].fun.as_ref()?;
            let arg_kinds = tf
                .args
                .iter()
                .map(|a| bytecode.types[a.0].kind)
                .collect::<Vec<_>>();
            return Some((arg_kinds, tf.ret.0));
        }
        None
    }

    fn try_handle_call_method_native(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        args: &[NanBoxedValue],
    ) -> Result<Option<NanBoxedValue>> {
        let dbg = std::env::var("ASH_DBG_DYN").is_ok();
        if args.len() < 2
            || args[0].is_null()
            || args[0].is_void()
            || args[1].is_null()
            || args[1].is_void()
        {
            return Ok(Some(NanBoxedValue::null()));
        }

        let closure_val = args[0];
        let varray_ptr = args[1].as_ptr() as *const hl::varray;
        if varray_ptr.is_null() {
            return Ok(Some(NanBoxedValue::null()));
        }

        let (findex, bound) = self.closure_findex_and_value(closure_val);
        let (arg_kinds, ret_type_idx) = self
            .closure_arg_kinds_and_ret_type(bytecode, findex)
            .unwrap_or((Vec::new(), 0));
        let arg_shift = if bound.is_some() { 1usize } else { 0usize };
        if dbg {
            eprintln!(
                "[CALL_METHOD] findex={} bound={} arg_kinds={:?} ret_type_idx={}",
                findex,
                bound.is_some(),
                arg_kinds,
                ret_type_idx
            );
        }

        let argc = unsafe { (*varray_ptr).size.max(0) as usize };
        let data_ptr = unsafe {
            (varray_ptr as *const u8).add(std::mem::size_of::<hl::varray>())
                as *const *mut hl::vdynamic
        };

        let mut call_args = Vec::with_capacity(argc);
        for i in 0..argc {
            let dyn_arg = unsafe { *data_ptr.add(i) };
            let expected_kind = arg_kinds
                .get(i + arg_shift)
                .copied()
                .unwrap_or(hl::hl_type_kind_HDYN);
            let v = self.dynamic_to_value_for_kind(dyn_arg, expected_kind);
            if dbg {
                let sk = unsafe {
                    if dyn_arg.is_null() || (*dyn_arg).t.is_null() {
                        0
                    } else {
                        (*(*dyn_arg).t).kind
                    }
                };
                eprintln!(
                    "[CALL_METHOD] arg{} dyn={:p} sk={} expect={} -> {:?}",
                    i, dyn_arg, sk, expected_kind, v
                );
            }
            call_args.push(v);
        }

        let ret = self.call_closure_val(bytecode, native_resolver, closure_val, call_args)?;
        if dbg {
            eprintln!("[CALL_METHOD] raw_ret={:?}", ret);
        }
        let out = if ret.is_void() {
            NanBoxedValue::null()
        } else {
            let ret_t = self.c_type_factory.get(ret_type_idx) as *mut hl_type;
            self.box_value_as_dynamic_with_type(ret, ret_t)
        };
        if dbg {
            eprintln!("[CALL_METHOD] out={:?}", out);
        }
        Ok(Some(out))
    }

    /// Interpreter-side implementation of bsort_i32 that uses the interpreter's
    /// call mechanism for the comparator closure (bytecode closures can't be called
    /// as raw C functions in interpreter mode).
    fn sort_bytes_i32(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        if args.len() < 4 {
            return Ok(NanBoxedValue::void());
        }
        let bytes_ptr = args[0].as_ptr() as *mut i32;
        let pos = args[1].as_i32() as isize;
        let len = args[2].as_i32() as usize;
        let cmp_val = args[3];

        if len == 0 || bytes_ptr as usize == 0 {
            return Ok(NanBoxedValue::void());
        }

        let mut data: Vec<i32> =
            unsafe { std::slice::from_raw_parts(bytes_ptr.offset(pos), len) }.to_vec();

        // Use raw pointer to avoid borrow conflict inside sort_by closure
        let self_raw = self as *mut Self;
        let bytecode_raw = bytecode as *const DecodedBytecode;
        let resolver_raw = native_resolver as *const ash::native_lib::NativeFunctionResolver;
        let mut sort_err: Option<anyhow::Error> = None;

        data.sort_by(|&a, &b| {
            if sort_err.is_some() {
                return std::cmp::Ordering::Equal;
            }
            let interp = unsafe { &mut *self_raw };
            let bc = unsafe { &*bytecode_raw };
            let nr = unsafe { &*resolver_raw };
            let call_args = vec![NanBoxedValue::from_i32(a), NanBoxedValue::from_i32(b)];
            match interp.call_closure_val(bc, nr, cmp_val, call_args) {
                Ok(r) => r.as_i32().cmp(&0),
                Err(e) => {
                    sort_err = Some(e);
                    std::cmp::Ordering::Equal
                }
            }
        });

        if let Some(e) = sort_err {
            return Err(e);
        }

        unsafe {
            let slice = std::slice::from_raw_parts_mut(bytes_ptr.offset(pos), len);
            slice.copy_from_slice(&data);
        }
        Ok(NanBoxedValue::void())
    }

    /// Interpreter-side bsort_i64.
    fn sort_bytes_i64(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        if args.len() < 4 {
            return Ok(NanBoxedValue::void());
        }
        let bytes_ptr = args[0].as_ptr() as *mut i64;
        let pos = args[1].as_i32() as isize;
        let len = args[2].as_i32() as usize;
        let cmp_val = args[3];

        if len == 0 || bytes_ptr as usize == 0 {
            return Ok(NanBoxedValue::void());
        }

        let mut data: Vec<i64> =
            unsafe { std::slice::from_raw_parts(bytes_ptr.offset(pos), len) }.to_vec();

        let self_raw = self as *mut Self;
        let bytecode_raw = bytecode as *const DecodedBytecode;
        let resolver_raw = native_resolver as *const ash::native_lib::NativeFunctionResolver;
        let mut sort_err: Option<anyhow::Error> = None;

        data.sort_by(|&a, &b| {
            if sort_err.is_some() {
                return std::cmp::Ordering::Equal;
            }
            let interp = unsafe { &mut *self_raw };
            let bc = unsafe { &*bytecode_raw };
            let nr = unsafe { &*resolver_raw };
            let call_args = vec![NanBoxedValue::from_i64(a), NanBoxedValue::from_i64(b)];
            match interp.call_closure_val(bc, nr, cmp_val, call_args) {
                Ok(r) => r.as_i32().cmp(&0),
                Err(e) => {
                    sort_err = Some(e);
                    std::cmp::Ordering::Equal
                }
            }
        });

        if let Some(e) = sort_err {
            return Err(e);
        }

        unsafe {
            let slice = std::slice::from_raw_parts_mut(bytes_ptr.offset(pos), len);
            slice.copy_from_slice(&data);
        }
        Ok(NanBoxedValue::void())
    }

    /// Interpreter-side bsort_f64.
    fn sort_bytes_f64(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        if args.len() < 4 {
            return Ok(NanBoxedValue::void());
        }
        let bytes_ptr = args[0].as_ptr() as *mut f64;
        let pos = args[1].as_i32() as isize;
        let len = args[2].as_i32() as usize;
        let cmp_val = args[3];

        if len == 0 || bytes_ptr as usize == 0 {
            return Ok(NanBoxedValue::void());
        }

        let mut data: Vec<f64> =
            unsafe { std::slice::from_raw_parts(bytes_ptr.offset(pos), len) }.to_vec();

        let self_raw = self as *mut Self;
        let bytecode_raw = bytecode as *const DecodedBytecode;
        let resolver_raw = native_resolver as *const ash::native_lib::NativeFunctionResolver;
        let mut sort_err: Option<anyhow::Error> = None;

        data.sort_by(|&a, &b| {
            if sort_err.is_some() {
                return std::cmp::Ordering::Equal;
            }
            let interp = unsafe { &mut *self_raw };
            let bc = unsafe { &*bytecode_raw };
            let nr = unsafe { &*resolver_raw };
            let call_args = vec![NanBoxedValue::from_f64(a), NanBoxedValue::from_f64(b)];
            match interp.call_closure_val(bc, nr, cmp_val, call_args) {
                Ok(r) => r.as_i32().cmp(&0),
                Err(e) => {
                    sort_err = Some(e);
                    std::cmp::Ordering::Equal
                }
            }
        });

        if let Some(e) = sort_err {
            return Err(e);
        }

        unsafe {
            let slice = std::slice::from_raw_parts_mut(bytes_ptr.offset(pos), len);
            slice.copy_from_slice(&data);
        }
        Ok(NanBoxedValue::void())
    }

    /// On ARM64 (and x86-64), floating-point arguments go into FP registers (d0-d7 / xmm0-xmm7),
    /// separate from integer/pointer registers (x0-x7 / rdi-rdi). Using a generic
    /// `fn(i64,...)->i64` transmute would put float bits into the wrong registers.
    ///
    /// This function uses typed Rust fn signatures to ensure the compiler emits
    /// correct calling-convention instructions for each pattern.
    ///
    /// Returns the raw i64 result (float results are returned as their bit representation).
    fn dispatch_float_native(
        &self,
        func_ptr: *mut std::ffi::c_void,
        args: &[NanBoxedValue],
        arg_kinds: &[u32],
        float_mask: u32,
        ret_is_float: bool,
    ) -> Result<i64> {
        let gf = |i: usize| -> f64 { args[i].as_f64() };
        let gi = |i: usize| -> i64 { self.value_to_i64(args[i], arg_kinds[i]) };

        let raw: i64 = unsafe {
            match (args.len(), ret_is_float, float_mask) {
                // --- 0 args ---
                (0, true, 0b0) => {
                    // () -> f64
                    let f: unsafe extern "C" fn() -> f64 = std::mem::transmute(func_ptr);
                    f().to_bits() as i64
                }
                // --- 1 arg ---
                (1, true, 0b0) => {
                    // (i64) -> f64  e.g. date_get_time(t:Int)
                    let f: unsafe extern "C" fn(i64) -> f64 = std::mem::transmute(func_ptr);
                    f(gi(0)).to_bits() as i64
                }
                (1, true, 0b1) => {
                    // (f64) -> f64  e.g. math_sqrt, math_abs, math_floor, ...
                    let f: unsafe extern "C" fn(f64) -> f64 = std::mem::transmute(func_ptr);
                    f(gf(0)).to_bits() as i64
                }
                (1, false, 0b1) => {
                    // (f64) -> i64  e.g. math_ffloor, math_isnan, math_isfinite
                    let f: unsafe extern "C" fn(f64) -> i64 = std::mem::transmute(func_ptr);
                    f(gf(0))
                }
                // --- 2 args ---
                (2, false, 0b01) => {
                    // (f64, i64) -> i64  e.g. hlp_ftos(d, len)
                    let f: unsafe extern "C" fn(f64, i64) -> i64 = std::mem::transmute(func_ptr);
                    f(gf(0), gi(1))
                }
                (2, true, 0b01) => {
                    // (f64, i64) -> f64
                    let f: unsafe extern "C" fn(f64, i64) -> f64 = std::mem::transmute(func_ptr);
                    f(gf(0), gi(1)).to_bits() as i64
                }
                (2, false, 0b10) => {
                    // (i64, f64) -> i64
                    let f: unsafe extern "C" fn(i64, f64) -> i64 = std::mem::transmute(func_ptr);
                    f(gi(0), gf(1))
                }
                (2, true, 0b10) => {
                    // (i64, f64) -> f64
                    let f: unsafe extern "C" fn(i64, f64) -> f64 = std::mem::transmute(func_ptr);
                    f(gi(0), gf(1)).to_bits() as i64
                }
                (2, true, 0b11) => {
                    // (f64, f64) -> f64  e.g. math_pow, math_atan2
                    let f: unsafe extern "C" fn(f64, f64) -> f64 = std::mem::transmute(func_ptr);
                    f(gf(0), gf(1)).to_bits() as i64
                }
                (2, true, 0b00) => {
                    // (i64, i64) -> f64
                    let f: unsafe extern "C" fn(i64, i64) -> f64 = std::mem::transmute(func_ptr);
                    f(gi(0), gi(1)).to_bits() as i64
                }
                (2, false, 0b11) => {
                    // (f64, f64) -> i64
                    let f: unsafe extern "C" fn(f64, f64) -> i64 = std::mem::transmute(func_ptr);
                    f(gf(0), gf(1))
                }
                // --- 3 args ---
                (3, true, 0b000) => {
                    // (i64, i64, i64) -> f64  e.g. hlp_parse_float(bytes, pos, len)
                    let f: unsafe extern "C" fn(i64, i64, i64) -> f64 =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gi(1), gi(2)).to_bits() as i64
                }
                (3, false, 0b001) => {
                    // (f64, i64, i64) -> i64
                    let f: unsafe extern "C" fn(f64, i64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gf(0), gi(1), gi(2))
                }
                (3, true, 0b001) => {
                    // (f64, i64, i64) -> f64
                    let f: unsafe extern "C" fn(f64, i64, i64) -> f64 =
                        std::mem::transmute(func_ptr);
                    f(gf(0), gi(1), gi(2)).to_bits() as i64
                }
                (3, false, 0b111) => {
                    // (f64, f64, f64) -> i64
                    let f: unsafe extern "C" fn(f64, f64, f64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gf(0), gf(1), gf(2))
                }
                (3, true, 0b111) => {
                    // (f64, f64, f64) -> f64
                    let f: unsafe extern "C" fn(f64, f64, f64) -> f64 =
                        std::mem::transmute(func_ptr);
                    f(gf(0), gf(1), gf(2)).to_bits() as i64
                }
                _ => {
                    return Err(anyhow!(
                        "Float native dispatch: {} args, float_mask={:#b}, ret_float={} not yet supported",
                        args.len(),
                        float_mask,
                        ret_is_float
                    ));
                }
            }
        };
        Ok(raw)
    }

    /// Convert a NanBoxedValue to an i64 for FFI passing.
    /// Uses the HL type kind to correctly interpret the value.
    fn value_to_i64(&self, val: NanBoxedValue, type_kind: u32) -> i64 {
        use ValueTypeKind::*;
        match ValueTypeKind::try_from(type_kind).unwrap_or(HNULL) {
            HVOID => 0,
            HI32 | HUI8 | HUI16 => val.as_i32() as i64,
            HI64 => val.as_i64_lossy(),
            HF32 | HF64 => {
                // Floats passed through integer registers via transmute
                val.as_f64().to_bits() as i64
            }
            HBOOL => val.as_bool() as i64,
            _ => {
                // All other types are pointer-like (HOBJ, HDYN, HBYTES, HFUN, etc.)
                if val.is_null() || val.is_void() {
                    0
                } else if val.is_ptr() {
                    val.as_ptr() as i64
                } else if val.is_i32() {
                    // Sometimes an i32 is used where a pointer is expected (e.g., 0 for null)
                    val.as_i32() as i64
                } else {
                    // TAG_I64, TAG_BYTES, TAG_FUNC, or unknown - extract raw payload
                    val.as_ptr() as i64
                }
            }
        }
    }

    /// Wrap a raw i64 return value from a native function based on the HL return type.
    fn wrap_native_result(&self, raw: i64, ret_kind: u32) -> NanBoxedValue {
        use ValueTypeKind::*;
        match ValueTypeKind::try_from(ret_kind).unwrap_or(HNULL) {
            HVOID => NanBoxedValue::void(),
            HI32 | HUI8 | HUI16 => NanBoxedValue::from_i32(raw as i32),
            HI64 => NanBoxedValue::from_i64(raw),
            HF32 | HF64 => NanBoxedValue::from_f64(f64::from_bits(raw as u64)),
            HBOOL => NanBoxedValue::from_bool(raw != 0),
            HBYTES => NanBoxedValue::from_bytes_ptr(raw as usize),
            _ => {
                // All other types are pointer-like (HOBJ, HDYN, HFUN, HARRAY, etc.)
                if raw == 0 {
                    NanBoxedValue::null()
                } else {
                    NanBoxedValue::from_ptr(raw as usize)
                }
            }
        }
    }

    /// Read a field from an object at the given field index.
    /// Uses the runtime object's fields_indexes to compute the byte offset.
    unsafe fn read_obj_field(
        obj_ptr: *mut u8,
        field_idx: usize,
        dst_kind: u32,
        obj_c_type: *mut c_void,
        obj_kind: u32,
        fn_get_obj_rt: *mut c_void,
    ) -> NanBoxedValue {
        if fn_get_obj_rt.is_null() {
            return NanBoxedValue::null();
        }

        // Get the type pointer for runtime object lookup.
        // For HOBJ, prefer the object's own header type (supports polymorphism).
        // For HSTRUCT, use the register's declared type (structs have no header).
        let type_ptr = if obj_kind != hl_type_kind_HSTRUCT {
            let header = *(obj_ptr as *const *mut c_void);
            if !header.is_null() {
                header
            } else {
                obj_c_type
            }
        } else {
            obj_c_type
        };

        if type_ptr.is_null() {
            return NanBoxedValue::null();
        }

        let get_rt: FnGetObjRt = std::mem::transmute(fn_get_obj_rt);
        let rt = get_rt(type_ptr) as *const hl_runtime_obj;
        if rt.is_null() || (*rt).fields_indexes.is_null() {
            return NanBoxedValue::null();
        }

        if field_idx >= (*rt).nfields as usize {
            return NanBoxedValue::null();
        }

        let offset = *(*rt).fields_indexes.add(field_idx);
        let field_addr = obj_ptr.add(offset as usize);
        Self::read_value_at(field_addr, dst_kind)
    }

    /// Write a value to an object field at the given field index.
    unsafe fn write_obj_field(
        obj_ptr: *mut u8,
        field_idx: usize,
        src_kind: u32,
        val: NanBoxedValue,
        obj_c_type: *mut c_void,
        obj_kind: u32,
        fn_get_obj_rt: *mut c_void,
    ) {
        if fn_get_obj_rt.is_null() {
            return;
        }

        let type_ptr = if obj_kind != hl_type_kind_HSTRUCT {
            let header = *(obj_ptr as *const *mut c_void);
            if !header.is_null() {
                header
            } else {
                obj_c_type
            }
        } else {
            obj_c_type
        };

        if type_ptr.is_null() {
            return;
        }

        let get_rt: FnGetObjRt = std::mem::transmute(fn_get_obj_rt);
        let rt = get_rt(type_ptr) as *const hl_runtime_obj;
        if rt.is_null() || (*rt).fields_indexes.is_null() {
            return;
        }

        if field_idx >= (*rt).nfields as usize {
            return;
        }

        let offset = *(*rt).fields_indexes.add(field_idx);
        let field_addr = obj_ptr.add(offset as usize) as *mut u8;
        Self::write_value_at(field_addr, src_kind, val);
    }

    /// Read a value from a raw memory address based on the HL type kind.
    unsafe fn read_value_at(addr: *const u8, kind: u32) -> NanBoxedValue {
        use ValueTypeKind::*;
        match ValueTypeKind::try_from(kind).unwrap_or(HDYN) {
            HVOID => NanBoxedValue::void(),
            HUI8 => NanBoxedValue::from_i32(*(addr as *const u8) as i32),
            HUI16 => NanBoxedValue::from_i32(*(addr as *const u16) as i32),
            HI32 => NanBoxedValue::from_i32(*(addr as *const i32)),
            HI64 => NanBoxedValue::from_i64(*(addr as *const i64)),
            HF32 => NanBoxedValue::from_f64(*(addr as *const f32) as f64),
            HF64 => NanBoxedValue::from_f64(*(addr as *const f64)),
            HBOOL => NanBoxedValue::from_bool(*(addr as *const u8) != 0),
            _ => {
                // Pointer types (OBJ, DYN, FUN, ARRAY, BYTES, ENUM, etc.)
                let ptr = *(addr as *const usize);
                if ptr == 0 {
                    NanBoxedValue::null()
                } else {
                    NanBoxedValue::from_ptr(ptr)
                }
            }
        }
    }

    /// Resolve a method findex from bytecode type proto (fallback when vobj_proto unavailable).
    fn resolve_method_findex_from_bytecode(
        &self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        this_reg: &Reg,
        proto_index: usize,
    ) -> Option<usize> {
        let type_idx = func.regs[this_reg.0 as usize].0;
        self.find_proto_findex(bytecode, type_idx, proto_index)
    }

    /// Recursively search type and its supers for a proto with matching pindex.
    fn find_proto_findex(
        &self,
        bytecode: &DecodedBytecode,
        type_idx: usize,
        proto_index: usize,
    ) -> Option<usize> {
        let hl_type_rust = &bytecode.types[type_idx];
        if let Some(ref obj) = hl_type_rust.obj {
            for proto in &obj.proto {
                if proto.pindex as usize == proto_index {
                    return Some(proto.findex as usize);
                }
            }
            // Check super type
            if let Some(ref super_) = obj.super_ {
                return self.find_proto_findex(bytecode, super_.0, proto_index);
            }
        }
        None
    }

    /// Write a value to a raw memory address based on the HL type kind.
    unsafe fn write_value_at(addr: *mut u8, kind: u32, val: NanBoxedValue) {
        use ValueTypeKind::*;
        match ValueTypeKind::try_from(kind).unwrap_or(HDYN) {
            HVOID => {}
            HUI8 => *(addr as *mut u8) = val.as_i32() as u8,
            HUI16 => *(addr as *mut u16) = val.as_i32() as u16,
            HI32 => *(addr as *mut i32) = val.as_i32(),
            HI64 => *(addr as *mut i64) = val.as_i64_lossy(),
            HF32 => *(addr as *mut f32) = val.as_f64() as f32,
            HF64 => *(addr as *mut f64) = val.as_f64(),
            HBOOL => *(addr as *mut u8) = val.as_bool() as u8,
            _ => {
                // Pointer types
                let ptr = if val.is_null() || val.is_void() {
                    0usize
                } else {
                    val.as_ptr()
                };
                *(addr as *mut usize) = ptr;
            }
        }
    }

    /// Allocate a venum value for the given type and construct index using the GC allocator.
    /// Takes fn_alloc_enum as a parameter to avoid conflicting with the frame mutable borrow.
    fn alloc_enum_value(
        fn_alloc_enum: *mut c_void,
        c_type_ptr: *mut hl_type,
        construct_idx: i32,
    ) -> *mut u8 {
        if fn_alloc_enum.is_null() || c_type_ptr.is_null() {
            return std::ptr::null_mut();
        }
        unsafe {
            let f: unsafe extern "C" fn(*mut hl_type, i32) -> *mut u8 =
                std::mem::transmute(fn_alloc_enum);
            f(c_type_ptr, construct_idx)
        }
    }

    /// Read a NanBoxedValue from a raw memory pointer using the given type kind.
    fn read_value_from_ptr(ptr: *const u8, kind: u32) -> NanBoxedValue {
        unsafe { Self::read_value_at(ptr, kind) }
    }

    /// Write a NanBoxedValue to a raw memory pointer using the given type kind.
    fn write_value_to_ptr(ptr: *mut u8, val: NanBoxedValue, kind: u32) {
        unsafe { Self::write_value_at(ptr, kind, val) }
    }
}
