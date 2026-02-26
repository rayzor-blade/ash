use std::collections::HashMap;
use std::ffi::c_void;
use anyhow::{anyhow, Result};

use ash::bytecode::DecodedBytecode;
use ash::c_types::CTypeFactory;
use ash::hl_bindings::{self as hl, hl_runtime_obj, hl_type, hl_type_kind_HSTRUCT, _vclosure};
use ash::native_lib::NativeFunctionResolver;
use ash::opcodes::{Opcode, Reg};
use ash::types::{HLFunction, ValueTypeKind};

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

/// Resolve/calculate a HashLink field hash from a bytecode string index.
/// Uses std's hlp_hash_gen when available so field names are cached for reflection/JSON.
fn hash_field_name(
    bytecode: &DecodedBytecode,
    str_idx: usize,
    fn_hash_gen: *mut c_void,
) -> Result<i32> {
    let s = bytecode
        .strings
        .get(str_idx)
        .ok_or_else(|| anyhow!("Dyn field string out of bounds: {}", str_idx))?;
    let mut utf16: Vec<u16> = s.encode_utf16().collect();
    utf16.push(0);
    if !fn_hash_gen.is_null() {
        let f: FnHashGen = unsafe { std::mem::transmute(fn_hash_gen) };
        return Ok(unsafe { f(utf16.as_ptr(), true) });
    }
    let mut h: i32 = 0;
    for c in &utf16[..utf16.len() - 1] {
        h = h.wrapping_mul(223).wrapping_add(*c as i32);
    }
    Ok(h.wrapping_rem(0x1FFFFF7B))
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
#[derive(Debug, Clone, Copy)]
struct HLExceptionPropagation(NanBoxedValue);

impl std::fmt::Display for HLExceptionPropagation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "HL exception: {:?}", self.0)
    }
}

impl std::error::Error for HLExceptionPropagation {}

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
    /// Resolved stdlib function pointer: hlp_gc_clear_scan_roots
    fn_gc_clear_scan_roots: *mut c_void,
    /// Resolved stdlib function pointer: hlp_gc_add_scan_root
    fn_gc_add_scan_root: *mut c_void,
    /// Scratch space for decoded raw pointer roots (from NaN-boxed registers).
    gc_root_ptrs: Vec<usize>,
    /// Cache of UTF-16 null-terminated strings (string index → leaked pointer)
    /// HashLink uses UTF-16 internally; bytecode strings are stored as UTF-8 in Rust.
    utf16_strings: HashMap<usize, *const u16>,
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
        let fn_gc_clear_scan_roots = native_resolver
            .resolve_function("std", "hlp_gc_clear_scan_roots")
            .unwrap_or(std::ptr::null_mut());
        let fn_gc_add_scan_root = native_resolver
            .resolve_function("std", "hlp_gc_add_scan_root")
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
            fn_gc_clear_scan_roots,
            fn_gc_add_scan_root,
            gc_root_ptrs: Vec::new(),
            utf16_strings: HashMap::new(),
        }
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

    /// Intern a bytecode string as null-terminated UTF-16 and return a stable pointer.
    fn intern_utf16_string(&mut self, bytecode: &DecodedBytecode, str_idx: usize) -> Option<*const u16> {
        if let Some(&cached) = self.utf16_strings.get(&str_idx) {
            return Some(cached);
        }
        let s = bytecode.strings.get(str_idx)?;
        let mut utf16: Vec<u16> = s.encode_utf16().collect();
        utf16.push(0);
        let ptr = utf16.as_ptr();
        std::mem::forget(utf16);
        self.utf16_strings.insert(str_idx, ptr);
        Some(ptr)
    }

    /// Execute starting from the bytecode entrypoint.
    pub fn execute_entrypoint(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
    ) -> Result<NanBoxedValue> {
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
                            let int_val = bytecode.ints.get(field_value as usize)
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
        // Track call count for JIT promotion
        let call_count = {
            let count = self.call_counts.entry(findex).or_insert(0);
            *count += 1;
            *count
        };
        if call_count == self.jit_threshold {
            self.jit_candidates.push(findex);
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
                            self.stack.last_mut().unwrap().registers.set(dst, ret);
                            self.stack.last_mut().unwrap().pc += 1;
                        }
                        Err(e) => {
                            if let Some(hl_exc) = e.downcast_ref::<HLExceptionPropagation>() {
                                let exc_val = hl_exc.0;
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
                frame.registers.set(dst.0, NanBoxedValue::from_bytes_ptr(pos));
            }
            Opcode::String { dst, ptr } => {
                // HashLink uses UTF-16 strings internally.
                // Get or create a cached null-terminated UTF-16 version of the string.
                let utf16_ptr = if let Some(&cached) = self.utf16_strings.get(&ptr.0) {
                    cached
                } else {
                    let s = bytecode
                        .strings
                        .get(ptr.0)
                        .ok_or_else(|| anyhow!("String constant out of bounds: {}", ptr.0))?;
                    let mut utf16: Vec<u16> = s.encode_utf16().collect();
                    utf16.push(0); // null terminator
                    let ptr_val = utf16.as_ptr();
                    std::mem::forget(utf16); // leak - lives for program duration
                    self.utf16_strings.insert(ptr.0, ptr_val);
                    ptr_val
                };
                frame.registers.set(dst.0, NanBoxedValue::from_bytes_ptr(utf16_ptr as usize));
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
                                let mut obj_hl_type =
                                    *(obj_ptr as *const *mut hl_type);
                                let mut found = None;
                                'search: while !obj_hl_type.is_null()
                                    && ((*obj_hl_type).kind == hl::hl_type_kind_HOBJ
                                        || (*obj_hl_type).kind
                                            == hl::hl_type_kind_HSTRUCT)
                                {
                                    let obj = (*obj_hl_type)
                                        .__bindgen_anon_1
                                        .obj;
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
                                anyhow!(
                                    "Cannot resolve method field={} on type",
                                    field.0
                                )
                            })?
                        }
                    } else {
                        self.resolve_method_findex_from_bytecode(
                            bytecode, func, &args[0], field.0,
                        )
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
                    // It's a pointer to a _vclosure struct
                    let cl_ptr = closure_val.as_ptr() as *const _vclosure;
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
                };

                return Ok(StepResult::Call {
                    findex,
                    args: arg_vals,
                    dst: dst.0,
                });
            }

            // ===== Closures =====
            Opcode::StaticClosure { dst, fun } => {
                // Store the function index as a TAG_FUNC value (no capture)
                frame
                    .registers
                    .set(dst.0, NanBoxedValue::from_func_index(fun.0));
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
                let obj_kind = bytecode.types[func.regs[obj.0 as usize].0].kind;
                let obj_c_type = self.c_type_factory.get(func.regs[obj.0 as usize].0) as *mut c_void;
                let dst_kind = bytecode.types[func.regs[dst.0 as usize].0].kind;
                let get_rt = self.fn_get_obj_rt;
                let obj_val = frame.registers.get(obj.0);
                if obj_val.is_null() || obj_val.is_void() {
                    frame.registers.set(dst.0, NanBoxedValue::null());
                } else {
                    let obj_ptr = obj_val.as_ptr() as *mut u8;
                    let val = unsafe {
                        Self::read_obj_field(obj_ptr, field.0, dst_kind, obj_c_type, obj_kind, get_rt)
                    };
                    frame.registers.set(dst.0, val);
                }
            }
            Opcode::GetThis { dst, field } => {
                let obj_kind = bytecode.types[func.regs[0].0].kind;
                let obj_c_type = self.c_type_factory.get(func.regs[0].0) as *mut c_void;
                let dst_kind = bytecode.types[func.regs[dst.0 as usize].0].kind;
                let get_rt = self.fn_get_obj_rt;
                let obj_val = frame.registers.get(0); // reg 0 is 'this'
                if obj_val.is_null() || obj_val.is_void() {
                    frame.registers.set(dst.0, NanBoxedValue::null());
                } else {
                    let obj_ptr = obj_val.as_ptr() as *mut u8;
                    let val = unsafe {
                        Self::read_obj_field(obj_ptr, field.0, dst_kind, obj_c_type, obj_kind, get_rt)
                    };
                    frame.registers.set(dst.0, val);
                }
            }
            Opcode::SetField { obj, field, src } => {
                let obj_kind = bytecode.types[func.regs[obj.0 as usize].0].kind;
                let obj_c_type = self.c_type_factory.get(func.regs[obj.0 as usize].0) as *mut c_void;
                let src_kind = bytecode.types[func.regs[src.0 as usize].0].kind;
                let get_rt = self.fn_get_obj_rt;
                let obj_val = frame.registers.get(obj.0);
                if !obj_val.is_null() && !obj_val.is_void() {
                    let obj_ptr = obj_val.as_ptr() as *mut u8;
                    let src_val = frame.registers.get(src.0);
                    unsafe {
                        Self::write_obj_field(obj_ptr, field.0, src_kind, src_val, obj_c_type, obj_kind, get_rt);
                    }
                }
            }
            Opcode::SetThis { field, src } => {
                let obj_kind = bytecode.types[func.regs[0].0].kind;
                let obj_c_type = self.c_type_factory.get(func.regs[0].0) as *mut c_void;
                let src_kind = bytecode.types[func.regs[src.0 as usize].0].kind;
                let get_rt = self.fn_get_obj_rt;
                let obj_val = frame.registers.get(0); // reg 0 is 'this'
                if !obj_val.is_null() && !obj_val.is_void() {
                    let obj_ptr = obj_val.as_ptr() as *mut u8;
                    let src_val = frame.registers.get(src.0);
                    unsafe {
                        Self::write_obj_field(obj_ptr, field.0, src_kind, src_val, obj_c_type, obj_kind, get_rt);
                    }
                }
            }
            Opcode::DynGet { dst, obj, field } => {
                let obj_val = frame.registers.get(obj.0);
                if obj_val.is_null() || obj_val.is_void() {
                    frame.registers.set(dst.0, NanBoxedValue::null());
                } else {
                    let hfield = hash_field_name(bytecode, field.0, fn_hash_gen)?;
                    let obj_ptr = obj_val.as_ptr() as *mut c_void;
                    let dst_type_idx = func.regs[dst.0 as usize].0;
                    let dst_kind = bytecode.types[dst_type_idx].kind;
                    let dst_type_ptr = self.c_type_factory.get(dst_type_idx) as *mut c_void;

                    let out = match dst_kind {
                        hl::hl_type_kind_HF64 => {
                            if self.fn_dyn_getd.is_null() {
                                NanBoxedValue::null()
                            } else {
                                let f: FnDynGetD = unsafe { std::mem::transmute(self.fn_dyn_getd) };
                                NanBoxedValue::from_f64(unsafe { f(obj_ptr, hfield) })
                            }
                        }
                        hl::hl_type_kind_HF32 => {
                            if self.fn_dyn_getf.is_null() {
                                NanBoxedValue::null()
                            } else {
                                let f: FnDynGetF = unsafe { std::mem::transmute(self.fn_dyn_getf) };
                                NanBoxedValue::from_f64(unsafe { f(obj_ptr, hfield) as f64 })
                            }
                        }
                        hl::hl_type_kind_HI64 => {
                            if self.fn_dyn_geti64.is_null() {
                                NanBoxedValue::null()
                            } else {
                                let f: FnDynGetI64 =
                                    unsafe { std::mem::transmute(self.fn_dyn_geti64) };
                                NanBoxedValue::from_i64(unsafe { f(obj_ptr, hfield) })
                            }
                        }
                        hl::hl_type_kind_HI32
                        | hl::hl_type_kind_HBOOL
                        | hl::hl_type_kind_HUI8
                        | hl::hl_type_kind_HUI16 => {
                            if self.fn_dyn_geti.is_null() {
                                NanBoxedValue::null()
                            } else {
                                let f: FnDynGetI = unsafe { std::mem::transmute(self.fn_dyn_geti) };
                                let i = unsafe { f(obj_ptr, hfield, dst_type_ptr) };
                                if dst_kind == hl::hl_type_kind_HBOOL {
                                    NanBoxedValue::from_bool(i != 0)
                                } else {
                                    NanBoxedValue::from_i32(i)
                                }
                            }
                        }
                        _ => {
                            if self.fn_dyn_getp.is_null() {
                                NanBoxedValue::null()
                            } else {
                                let f: FnDynGetP = unsafe { std::mem::transmute(self.fn_dyn_getp) };
                                let p = unsafe { f(obj_ptr, hfield, dst_type_ptr) };
                                if p.is_null() {
                                    NanBoxedValue::null()
                                } else {
                                    NanBoxedValue::from_ptr(p as usize)
                                }
                            }
                        }
                    };
                    frame.registers.set(dst.0, out);
                }
            }
            Opcode::DynSet { obj, field, src } => {
                let obj_val = frame.registers.get(obj.0);
                if obj_val.is_null() || obj_val.is_void() {
                    // no-op
                } else {
                    let hfield = hash_field_name(bytecode, field.0, fn_hash_gen)?;
                    let obj_ptr = obj_val.as_ptr() as *mut c_void;
                    let src_val = frame.registers.get(src.0);
                    let src_type_idx = func.regs[src.0 as usize].0;
                    let src_kind = bytecode.types[src_type_idx].kind;
                    let src_type_ptr = self.c_type_factory.get(src_type_idx) as *mut c_void;

                    match src_kind {
                        hl::hl_type_kind_HF64 => {
                            if !self.fn_dyn_setd.is_null() {
                                let f: FnDynSetD =
                                    unsafe { std::mem::transmute(self.fn_dyn_setd) };
                                unsafe { f(obj_ptr, hfield, src_val.as_f64()) };
                            }
                        }
                        hl::hl_type_kind_HF32 => {
                            if !self.fn_dyn_setf.is_null() {
                                let f: FnDynSetF =
                                    unsafe { std::mem::transmute(self.fn_dyn_setf) };
                                unsafe { f(obj_ptr, hfield, src_val.as_f64() as f32) };
                            }
                        }
                        hl::hl_type_kind_HI64 => {
                            if !self.fn_dyn_seti64.is_null() {
                                let f: FnDynSetI64 =
                                    unsafe { std::mem::transmute(self.fn_dyn_seti64) };
                                unsafe { f(obj_ptr, hfield, src_val.as_i64_lossy()) };
                            }
                        }
                        hl::hl_type_kind_HI32
                        | hl::hl_type_kind_HBOOL
                        | hl::hl_type_kind_HUI8
                        | hl::hl_type_kind_HUI16 => {
                            if !self.fn_dyn_seti.is_null() {
                                let f: FnDynSetI =
                                    unsafe { std::mem::transmute(self.fn_dyn_seti) };
                                let i = if src_kind == hl::hl_type_kind_HBOOL {
                                    if src_val.as_bool() { 1 } else { 0 }
                                } else {
                                    src_val.as_i32()
                                };
                                unsafe { f(obj_ptr, hfield, src_type_ptr, i) };
                            }
                        }
                        _ => {
                            if !self.fn_dyn_setp.is_null() {
                                let f: FnDynSetP =
                                    unsafe { std::mem::transmute(self.fn_dyn_setp) };
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
                if self.compare_regs(a.0, b.0, CmpOp::SLt) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JSGte { a, b, offset } => {
                if self.compare_regs(a.0, b.0, CmpOp::SGte) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JSGt { a, b, offset } => {
                if self.compare_regs(a.0, b.0, CmpOp::SGt) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JSLte { a, b, offset } => {
                if self.compare_regs(a.0, b.0, CmpOp::SLte) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JULt { a, b, offset } => {
                if self.compare_regs(a.0, b.0, CmpOp::ULt) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JUGte { a, b, offset } => {
                if self.compare_regs(a.0, b.0, CmpOp::UGte) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JNotLt { a, b, offset } => {
                // JNotLt is equivalent to JGte (signed)
                if self.compare_regs(a.0, b.0, CmpOp::SGte) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JNotGte { a, b, offset } => {
                // JNotGte is equivalent to JLt (signed)
                if self.compare_regs(a.0, b.0, CmpOp::SLt) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JEq { a, b, offset } => {
                if self.compare_regs(a.0, b.0, CmpOp::Eq) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JNotEq { a, b, offset } => {
                if self.compare_regs(a.0, b.0, CmpOp::NotEq) {
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

                let type_ptr: usize = if val.is_ptr() && !val.is_null() {
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

                frame.registers.set(dst.0, NanBoxedValue::from_ptr(type_ptr));
            }
            Opcode::GetTID { dst, src } => {
                // GetTID returns the kind field of the hl_type* in src.
                // src should hold an hl_type* (result of GetType).
                // hl_type.kind is a u32 at offset 0.
                let val = frame.registers.get(src.0);
                let kind = if val.is_ptr() && !val.is_null() {
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
                let result = if val.is_ptr() && !val.is_null() {
                    let base = val.as_ptr() as *const u8;
                    unsafe {
                        match dst_kind {
                            hl::hl_type_kind_HI32
                            | hl::hl_type_kind_HUI8
                            | hl::hl_type_kind_HUI16 => {
                                let i = base.add(8).cast::<i32>().read_unaligned();
                                NanBoxedValue::from_i32(i)
                            }
                            hl::hl_type_kind_HI64 => {
                                let i = base.add(8).cast::<i64>().read_unaligned();
                                NanBoxedValue::from_i64(i)
                            }
                            hl::hl_type_kind_HF32 => {
                                let f = base.add(8).cast::<f32>().read_unaligned();
                                NanBoxedValue::from_f64(f as f64)
                            }
                            hl::hl_type_kind_HF64 => {
                                let f = base.add(8).cast::<f64>().read_unaligned();
                                NanBoxedValue::from_f64(f)
                            }
                            hl::hl_type_kind_HBOOL => {
                                let b = base.add(8).cast::<u8>().read();
                                NanBoxedValue::from_bool(b != 0)
                            }
                            _ => val, // pointer/object types: pass through
                        }
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
                    frame.registers.set(dst.0, NanBoxedValue::from_ptr(obj as usize));
                }
            }

            // ===== Array Operations =====
            Opcode::GetArray { dst, array, index } => {
                let arr_val = frame.registers.get(array.0);
                let idx = frame.registers.get(index.0).as_i32().max(0) as usize;
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
                            && (at as usize) % std::mem::align_of::<hl_type>() != 0
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
                        let at_kind = if at.is_null() { hl::hl_type_kind_HDYN } else { (*at).kind };
                        let data = arr_ptr.add(24);
                        match at_kind {
                            k if k == hl::hl_type_kind_HUI8 => NanBoxedValue::from_i32(*data.add(idx) as i32),
                            k if k == hl::hl_type_kind_HUI16 => NanBoxedValue::from_i32(*(data.add(idx * 2) as *const u16) as i32),
                            k if k == hl::hl_type_kind_HBOOL => NanBoxedValue::from_bool(*(data.add(idx * 2) as *const u16) != 0),
                            k if k == hl::hl_type_kind_HI32 => NanBoxedValue::from_i32(*(data.add(idx * 4) as *const i32)),
                            k if k == hl::hl_type_kind_HI64 => NanBoxedValue::from_i64(*(data.add(idx * 8) as *const i64)),
                            k if k == hl::hl_type_kind_HF32 => NanBoxedValue::from_f64(*(data.add(idx * 4) as *const f32) as f64),
                            k if k == hl::hl_type_kind_HF64 => NanBoxedValue::from_f64(*(data.add(idx * 8) as *const f64)),
                            _ => {
                                let ptr_val = *(data.add(idx * 8) as *const usize);
                                if ptr_val == 0 { NanBoxedValue::null() } else { NanBoxedValue::from_ptr(ptr_val) }
                            }
                        }
                    }
                };
                frame.registers.set(dst.0, val);
            }
            Opcode::SetArray {
                array,
                index,
                src,
            } => {
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
                            && (at as usize) % std::mem::align_of::<hl_type>() != 0
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
                        let at_kind = if at.is_null() { hl::hl_type_kind_HDYN } else { (*at).kind };
                        let data = arr_ptr.add(24);
                        match at_kind {
                            k if k == hl::hl_type_kind_HUI8 => *data.add(idx) = src_val.as_i32() as u8,
                            k if k == hl::hl_type_kind_HUI16 => *(data.add(idx * 2) as *mut u16) = src_val.as_i32() as u16,
                            k if k == hl::hl_type_kind_HBOOL => *(data.add(idx * 2) as *mut u16) = src_val.as_bool() as u16,
                            k if k == hl::hl_type_kind_HI32 => *(data.add(idx * 4) as *mut i32) = src_val.as_i32(),
                            k if k == hl::hl_type_kind_HI64 => *(data.add(idx * 8) as *mut i64) = src_val.as_i64_lossy(),
                            k if k == hl::hl_type_kind_HF32 => *(data.add(idx * 4) as *mut f32) = src_val.as_f64() as f32,
                            k if k == hl::hl_type_kind_HF64 => *(data.add(idx * 8) as *mut f64) = src_val.as_f64(),
                            _ => {
                                let ptr_val = if src_val.is_null() || src_val.is_void() { 0usize } else { src_val.as_ptr() };
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
                            if i >= c.nparams as usize { break; }
                            let offset = *c.offsets.add(i) as usize;
                            let arg_val = frame.registers.get(arg_reg.0);
                            let param_kind = (*(*c.params.add(i))).kind;
                            Self::write_value_to_ptr(base.add(offset), arg_val, param_kind);
                        }
                    }
                }
                frame.registers.set(dst.0, if val.is_null() {
                    NanBoxedValue::null()
                } else {
                    NanBoxedValue::from_ptr(val as usize)
                });
            }
            Opcode::EnumAlloc { dst, construct } => {
                let type_idx = func.regs[dst.0 as usize].0;
                let c_type_ptr = self.c_type_factory.get(type_idx);
                let fn_alloc_enum = self.fn_alloc_enum;
                let val = Self::alloc_enum_value(fn_alloc_enum, c_type_ptr, construct.0 as i32);
                frame.registers.set(dst.0, if val.is_null() {
                    NanBoxedValue::null()
                } else {
                    NanBoxedValue::from_ptr(val as usize)
                });
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
                            let construct_idx = *(val.as_ptr() as *const u8).add(8).cast::<i32>()
                                as usize;
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
                    return Err(anyhow::Error::new(HLExceptionPropagation(val)));
                }
            }
            Opcode::Rethrow { exc } => {
                let val = frame.registers.get(exc.0);
                if let Some((target_pc, exc_reg)) = frame.trap_stack.pop() {
                    frame.registers.set(exc_reg, val);
                    return Ok(StepResult::JumpAbs(target_pc));
                } else {
                    return Err(anyhow::Error::new(HLExceptionPropagation(val)));
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
        let result = va
            .binary_int_op(vb, op)
            .ok_or_else(|| {
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
    fn compare_regs(&self, a: u32, b: u32, op: CmpOp) -> bool {
        let frame = self.stack.last().unwrap();
        let va = frame.registers.get(a);
        let vb = frame.registers.get(b);
        va.compare(vb, op).unwrap_or(false)
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

        // Intercept sort natives: they call back into bytecode closures via C function pointers,
        // which doesn't work in interpreter mode. Implement sorting here instead.
        match native.name.as_str() {
            "bsort_i32" => return self.sort_bytes_i32(bytecode, native_resolver, args),
            "bsort_f64" => return self.sort_bytes_f64(bytecode, native_resolver, args),
            "bsort_i64" => return self.sort_bytes_i64(bytecode, native_resolver, args),
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

        // Check if any argument or return type involves floats.
        // On ARM64, floats use separate FP registers (d0-d7) vs integer registers (x0-x7),
        // so we must use typed dispatch with explicit f64 in the right positions.
        let is_float_kind = |k: u32| {
            k == hl::hl_type_kind_HF32 || k == hl::hl_type_kind_HF64
        };
        let ret_is_float = is_float_kind(ret_kind);
        let float_mask: u32 = arg_kinds
            .iter()
            .enumerate()
            .fold(0u32, |acc, (i, &k)| {
                if is_float_kind(k) { acc | (1 << i) } else { acc }
            });

        if ret_is_float || float_mask != 0 {
            let raw = self.dispatch_float_native(func_ptr, args, &arg_kinds, float_mask, ret_is_float)?;
            return Ok(self.wrap_native_result(raw, ret_kind));
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


        if args.len() > 7 {
            return Err(anyhow!("Native call with {} args not yet supported", args.len()));
        }

        // Set up a setjmp/longjmp trap so hlp_throw can propagate through native C ABI safely.
        let fn_setup_trap = self.fn_setup_trap_jit;
        let fn_remove_trap = self.fn_remove_trap_jit;
        let fn_get_exc = self.fn_get_exc_value;
        let fn_clear_exc = self.fn_clear_exc_value;
        let mut trap_installed = false;
        if !fn_setup_trap.is_null() {
            type FnSetupTrap = unsafe extern "C" fn() -> *mut i32;
            let setup: FnSetupTrap = unsafe { std::mem::transmute(fn_setup_trap) };
            let jmp_buf = unsafe { setup() };
            if !jmp_buf.is_null() {
                trap_installed = true;
                let jumped = unsafe { hl::_setjmp(jmp_buf) };
                if jumped != 0 {
                    if !fn_get_exc.is_null() {
                        type FnGetExc = unsafe extern "C" fn() -> *mut c_void;
                        let exc_ptr = unsafe {
                            (std::mem::transmute::<*mut c_void, FnGetExc>(fn_get_exc))()
                        };
                        if !exc_ptr.is_null() {
                            if !fn_clear_exc.is_null() {
                                type FnClearExc = unsafe extern "C" fn();
                                unsafe {
                                    (std::mem::transmute::<*mut c_void, FnClearExc>(fn_clear_exc))()
                                };
                            }
                            return Err(anyhow::Error::new(HLExceptionPropagation(
                                NanBoxedValue::from_ptr(exc_ptr as usize),
                            )));
                        }
                    }
                    return Err(anyhow!("Native longjmp without exception value: {}", func_name));
                }
            }
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
                    f(extract_arg(0), extract_arg(1), extract_arg(2), extract_arg(3))
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
                _ => 0i64, // arg count is pre-validated above
            }
        };

        if trap_installed && !fn_remove_trap.is_null() {
            type FnRemoveTrap = unsafe extern "C" fn();
            unsafe { (std::mem::transmute::<*mut c_void, FnRemoveTrap>(fn_remove_trap))() };
        }

        // Wrap return value using the correct NanBoxedValue type
        Ok(self.wrap_native_result(raw_result, ret_kind))
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

        let mut data: Vec<i32> = unsafe {
            std::slice::from_raw_parts(bytes_ptr.offset(pos), len)
        }.to_vec();

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
            let call_args = vec![
                NanBoxedValue::from_i32(a),
                NanBoxedValue::from_i32(b),
            ];
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

        let mut data: Vec<i64> = unsafe {
            std::slice::from_raw_parts(bytes_ptr.offset(pos), len)
        }.to_vec();

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
            let call_args = vec![
                NanBoxedValue::from_i64(a),
                NanBoxedValue::from_i64(b),
            ];
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

        let mut data: Vec<f64> = unsafe {
            std::slice::from_raw_parts(bytes_ptr.offset(pos), len)
        }.to_vec();

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
            let call_args = vec![
                NanBoxedValue::from_f64(a),
                NanBoxedValue::from_f64(b),
            ];
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
                // --- 1 arg ---
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
    fn alloc_enum_value(fn_alloc_enum: *mut c_void, c_type_ptr: *mut hl_type, construct_idx: i32) -> *mut u8 {
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
