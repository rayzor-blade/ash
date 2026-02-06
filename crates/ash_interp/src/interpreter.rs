use std::collections::HashMap;
use std::ffi::c_void;

use anyhow::{anyhow, Result};

use ash::bytecode::DecodedBytecode;
use ash::c_types::CTypeFactory;
use ash::hl_bindings::{self as hl, hl_runtime_obj, hl_type, hl_type_kind_HSTRUCT, _vclosure};
use ash::native_lib::NativeFunctionResolver;
use ash::opcodes::{Opcode, Reg};
use ash::types::{HLFunction, HLNative, ValueTypeKind};

use crate::frame::InterpreterFrame;
use crate::values::{CmpOp, FloatBinOp, IntBinOp, NanBoxedValue};

/// Function pointer types for stdlib functions resolved at runtime.
type FnAllocObj = unsafe extern "C" fn(*mut c_void) -> *mut c_void;
type FnGetObjRt = unsafe extern "C" fn(*mut c_void) -> *mut c_void;
type FnAllocClosureVoid = unsafe extern "C" fn(*mut c_void, *mut c_void) -> *mut c_void;

/// Result of executing a single opcode.
enum StepResult {
    /// Continue to next opcode (pc already incremented)
    Continue,
    /// Jump to a relative offset from current pc
    Jump(i32),
    /// Return a value from the current function
    Return(NanBoxedValue),
    /// Call a function by findex, with arguments and destination register
    Call {
        findex: usize,
        args: Vec<NanBoxedValue>,
        dst: u32,
    },
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
            utf16_strings: HashMap::new(),
        }
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

                // Also update the global_value slot in hl_type_obj so stdlib can find it
                unsafe {
                    let obj = (*c_type_ptr).__bindgen_anon_1.obj;
                    if !obj.is_null() && !(*obj).global_value.is_null() {
                        *(*obj).global_value = obj_ptr;
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
                            if str_idx < bytecode.strings.len() {
                                let s = &bytecode.strings[str_idx];
                                // Convert to null-terminated UTF-16 (what HashLink expects)
                                let mut utf16: Vec<u16> = s.encode_utf16().collect();
                                utf16.push(0); // null terminator
                                let ptr = utf16.as_ptr();
                                std::mem::forget(utf16);
                                unsafe {
                                    *(field_addr as *mut *const u16) = ptr;
                                }
                            }
                        }
                        _ => {
                            // Primitive value - store directly as i32
                            unsafe {
                                *(field_addr as *mut i32) = field_value;
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
        let count = self.call_counts.entry(findex).or_insert(0);
        *count += 1;
        if *count == self.jit_threshold {
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

        // Main interpretation loop
        let result = self.interpret_loop(bytecode, native_resolver, func_idx)?;

        self.stack.pop();
        Ok(result)
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
                StepResult::Return(value) => {
                    return Ok(value);
                }
                StepResult::Call { findex, args, dst } => {
                    let ret = self.call_function(bytecode, native_resolver, findex, &args)?;
                    self.stack.last_mut().unwrap().registers.set(dst, ret);
                    self.stack.last_mut().unwrap().pc += 1;
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
                    let s = &bytecode.strings[ptr.0];
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
                    return Err(anyhow!("Add: incompatible types {:?} + {:?}", va, vb));
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
                let arg_vals: Vec<NanBoxedValue> =
                    args.iter().map(|r| frame.registers.get(r.0)).collect();

                if closure_val.is_null() || closure_val.is_void() {
                    return Err(anyhow!("CallClosure on null closure (pc={})", frame.pc));
                }

                // The closure value might be:
                // 1. A raw function index (from StaticClosure)
                // 2. A pointer to a _vclosure struct
                let findex = if closure_val.is_func() {
                    closure_val.as_func_index()
                } else {
                    // It's a pointer to a _vclosure struct
                    let cl_ptr = closure_val.as_ptr() as *const _vclosure;
                    unsafe {
                        let fun_ptr = (*cl_ptr).fun;
                        // Extract findex from stub pointer (findex+1)
                        (fun_ptr as usize).wrapping_sub(1)
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
                // Store the function index as a closure value
                frame
                    .registers
                    .set(dst.0, NanBoxedValue::from_func_index(fun.0));
            }
            Opcode::InstanceClosure { dst, fun, obj } => {
                // TODO: Allocate closure with bound object (Phase 2)
                frame
                    .registers
                    .set(dst.0, NanBoxedValue::from_func_index(fun.0));
            }
            Opcode::VirtualClosure { dst, obj, field } => {
                // TODO: Resolve virtual method and create closure (Phase 2)
                frame.registers.set(dst.0, NanBoxedValue::null());
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
                // TODO: Dynamic field access (Phase 2)
                frame.registers.set(dst.0, NanBoxedValue::null());
            }
            Opcode::DynSet { obj, field, src } => {
                // TODO: Dynamic field write (Phase 2)
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
                let index = val.as_i32() as usize;
                if index < offsets.len() {
                    return Ok(StepResult::Jump(offsets[index]));
                } else {
                    return Ok(StepResult::Jump(*end));
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
                // Return the C-level hl_type pointer for the register's type
                let type_ref = &func.regs[src.0 as usize];
                let c_type_ptr = self.c_type_factory.get(type_ref.0);
                frame
                    .registers
                    .set(dst.0, NanBoxedValue::from_ptr(c_type_ptr as usize));
            }
            Opcode::GetTID { dst, src } => {
                let type_ref = &func.regs[src.0 as usize];
                let kind = bytecode.types[type_ref.0].kind as i32;
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
                // TODO: Runtime type check (Phase 3)
                let val = frame.registers.get(src.0);
                frame.registers.set(dst.0, val);
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
                let c_type_ptr = self.c_type_factory.get(func.regs[dst.0 as usize].0);
                let alloc_fn = self.fn_alloc_obj;
                if c_type_ptr.is_null() || alloc_fn.is_null() {
                    frame.registers.set(dst.0, NanBoxedValue::null());
                } else {
                    let f: FnAllocObj = unsafe { std::mem::transmute(alloc_fn) };
                    let obj = unsafe { f(c_type_ptr as *mut c_void) };
                    if obj.is_null() {
                        frame.registers.set(dst.0, NanBoxedValue::null());
                    } else {
                        frame.registers.set(dst.0, NanBoxedValue::from_ptr(obj as usize));
                    }
                }
            }

            // ===== Array Operations =====
            Opcode::GetArray { dst, array, index } => {
                // TODO: Proper array element access (Phase 5)
                frame.registers.set(dst.0, NanBoxedValue::null());
            }
            Opcode::SetArray {
                array,
                index,
                src,
            } => {
                // TODO: Proper array element write (Phase 5)
            }
            Opcode::ArraySize { dst, array } => {
                // TODO: Read array size field (Phase 5)
                frame.registers.set(dst.0, NanBoxedValue::from_i32(0));
            }

            // ===== Memory Access =====
            Opcode::GetI8 { dst, bytes, index } => {
                // TODO: Raw memory read (Phase 5)
                frame.registers.set(dst.0, NanBoxedValue::from_i32(0));
            }
            Opcode::GetI16 { dst, bytes, index } => {
                frame.registers.set(dst.0, NanBoxedValue::from_i32(0));
            }
            Opcode::GetMem { dst, bytes, index } => {
                frame.registers.set(dst.0, NanBoxedValue::from_i32(0));
            }
            Opcode::SetI8 {
                bytes,
                index,
                src,
            } => {
                // TODO: Raw memory write (Phase 5)
            }
            Opcode::SetI16 {
                bytes,
                index,
                src,
            } => {}
            Opcode::SetMem {
                bytes,
                index,
                src,
            } => {}

            // ===== References =====
            Opcode::Ref { dst, src } => {
                // Create a pointer to a heap-allocated slot holding the value.
                // Native code may read/write through this pointer (e.g., output params).
                let val = frame.registers.get(src.0);
                let boxed = Box::into_raw(Box::new(val.as_i64_lossy()));
                frame.registers.set(dst.0, NanBoxedValue::from_ptr(boxed as usize));
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
                        hl::hl_type_kind_HBOOL => NanBoxedValue::from_bool(val != 0),
                        _ => NanBoxedValue::from_ptr(val as usize),
                    };
                    frame.registers.set(dst.0, result);
                } else {
                    frame.registers.set(dst.0, NanBoxedValue::null());
                }
            }
            Opcode::Setref { dst, value } => {
                // Write through a Ref pointer: *dst = value
                let ptr_val = frame.registers.get(dst.0);
                let ptr = ptr_val.as_ptr() as *mut i64;
                if !ptr.is_null() {
                    let val = frame.registers.get(value.0);
                    unsafe { *ptr = val.as_i64_lossy() };
                }
            }

            // ===== Enums =====
            Opcode::MakeEnum {
                dst,
                construct,
                args,
            } => {
                // TODO: Allocate enum via hlp_alloc_enum (Phase 5)
                frame.registers.set(dst.0, NanBoxedValue::null());
            }
            Opcode::EnumAlloc { dst, construct } => {
                frame.registers.set(dst.0, NanBoxedValue::null());
            }
            Opcode::EnumIndex { dst, value } => {
                // TODO: Read enum construct index (Phase 5)
                frame.registers.set(dst.0, NanBoxedValue::from_i32(0));
            }
            Opcode::EnumField {
                dst,
                value,
                construct,
                field,
            } => {
                frame.registers.set(dst.0, NanBoxedValue::null());
            }
            Opcode::SetEnumField { value, field, src } => {}

            // ===== Exception Handling =====
            Opcode::Trap { exc, offset } => {
                // TODO: Set up trap context (Phase 4)
                // For now, just continue - exceptions will crash
            }
            Opcode::EndTrap { exc } => {
                // TODO: Remove trap context (Phase 4)
            }
            Opcode::Throw { exc } => {
                let val = frame.registers.get(exc.0);
                return Err(anyhow!("Uncaught exception: {:?}", val));
            }
            Opcode::Rethrow { exc } => {
                let val = frame.registers.get(exc.0);
                return Err(anyhow!("Uncaught rethrown exception: {:?}", val));
            }
            Opcode::NullCheck { reg } => {
                let val = frame.registers.get(reg.0);
                if val.is_null() {
                    let pc = frame.pc;
                    let fname = func.name();
                    // Show surrounding opcodes for debugging context
                    let start = pc.saturating_sub(4);
                    let end = (pc + 2).min(func.ops.len());
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
        _func: &HLFunction,
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
            .ok_or_else(|| anyhow!("{:?}: incompatible types {:?}, {:?}", op, va, vb))?;
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

        // Type-aware argument extraction
        let extract_arg = |idx: usize| -> i64 {
            let kind = if idx < arg_kinds.len() {
                arg_kinds[idx]
            } else {
                0 // HVOID fallback
            };
            self.value_to_i64(args[idx], kind)
        };

        // Dispatch based on argument count, using type-aware extraction and wrapping
        let raw_result = unsafe {
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
                _ => {
                    return Err(anyhow!(
                        "Native call with {} args not yet supported",
                        args.len()
                    ));
                }
            }
        };

        // Wrap return value using the correct NanBoxedValue type
        Ok(self.wrap_native_result(raw_result, ret_kind))
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
}
