//! Opcode handlers extracted from `execute_opcode`.
//!
//! These carry the semantics the SSA dispatcher shares with the flat
//! interpreter, so both run the same code rather than two copies of it. A
//! child module of `interpreter` so it reaches `HLInterpreter`'s private
//! fields without widening them.

use anyhow::{anyhow, Result};
use std::ffi::c_void;

use ash_core::hl_bindings as hl;
use ash_core::opcodes::Reg;
use ash_core::types::HLFunction;

use crate::values::NanBoxedValue;

use super::HLExceptionPropagation;

use ash_core::bytecode::DecodedBytecode;
use ash_core::hl_bindings::{_vclosure, hl_type};

use crate::tiering::env_flag;

use super::instrument::{stride_probe, stride_probe_enabled};
use super::{
    func_of, hash_field_name, native_of, run_with_hl_trap, FnAllocDynObj, FnAllocObj,
    FnAllocVirtual, HLInterpreter, StepResult,
};

impl HLInterpreter {
    /// Helper: perform integer binary op on two registers.
    /// Write an array element.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    pub(super) fn op_set_array(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        array: u32,
        index: u32,
        src: u32,
    ) -> Result<StepResult> {
        let frame = self.stack.last_mut().unwrap();
        let arr_val = frame.registers.get(array);
        let idx = frame.registers.get(index).as_i32().max(0) as usize;
        let src_val = frame.registers.get(src);
        if !arr_val.is_null() && !arr_val.is_void() {
            if !arr_val.is_ptr() {
                return Err(anyhow!(
                        "SetArray: array reg r{} is not pointer in {} at pc={} (val={:?}, type_kind={})",
                        array,
                        func.name(),
                        frame.pc,
                        arr_val,
                        bytecode.types[func.regs[array as usize].0].kind
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
                            array,
                            arr_val,
                            src_val
                        ));
                }
                let at = *(arr_ptr.add(8) as *const *mut hl_type);
                if !at.is_null() && !(at as usize).is_multiple_of(std::mem::align_of::<hl_type>()) {
                    return Err(anyhow!(
                            "SetArray: invalid at pointer {:p} in {} at pc={} (arr=r{} val={:?} idx={} src={:?} r4={:?} r6={:?} r16={:?})",
                            at,
                            func.name(),
                            frame.pc,
                            array,
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
                    k if k == hl::hl_type_kind_HUI8 => *data.add(idx) = src_val.as_i32() as u8,
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
                        } else if (k == hl::hl_type_kind_HDYN || k == hl::hl_type_kind_HNULL)
                            && !src_val.is_ptr()
                        {
                            // Arrays of dyn/null store vdynamic*. Box primitives before write.
                            let src_type_idx = func.regs[src as usize].0;
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

        Ok(StepResult::Continue)
    }

    /// Read an array element.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    pub(super) fn op_get_array(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        dst: u32,
        array: u32,
        index: u32,
    ) -> Result<StepResult> {
        let frame = self.stack.last_mut().unwrap();
        let arr_val = frame.registers.get(array);
        let idx = frame.registers.get(index).as_i32().max(0) as usize;
        let dst_kind = bytecode.types[func.regs[dst as usize].0].kind;
        let val = if arr_val.is_null() || arr_val.is_void() {
            NanBoxedValue::null()
        } else if !arr_val.is_ptr() {
            return Err(anyhow!(
                "GetArray: array reg r{} is not pointer in {} at pc={} (val={:?}, type_kind={})",
                array,
                func.name(),
                frame.pc,
                arr_val,
                bytecode.types[func.regs[array as usize].0].kind
            ));
        } else {
            // varray: t@0, at@8, size@16, data@24
            let arr_ptr = arr_val.as_ptr() as *const u8;
            if (arr_ptr as usize) < 0x1000
                || !(arr_ptr as usize).is_multiple_of(std::mem::align_of::<usize>())
            {
                static BAD_ARR_COUNT: std::sync::atomic::AtomicU32 =
                    std::sync::atomic::AtomicU32::new(0);
                let c = BAD_ARR_COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                if c == 0 || c == 100 || c == 10000 {
                    eprintln!(
                        "[WARN] GetArray invalid ptr={:#x} count={} in {} pc={}",
                        arr_ptr as usize,
                        c + 1,
                        func.name(),
                        frame.pc
                    );
                }
                frame.registers.set(dst, NanBoxedValue::null());
                self.stack.last_mut().unwrap().pc += 1;
                return Ok(StepResult::Continue);
            }
            // `ASH_STRIDE_PROBE=1`: is an array of object references laid out
            // so that the objects themselves are at a constant stride? If they
            // are, a loop over `a[i].field` is a strided access rather than a
            // gather, which is the difference between vectorizable and not on
            // a target with no gather instruction. Reports the first array it
            // sees, once.
            if stride_probe_enabled() {
                unsafe { stride_probe(arr_ptr, &func.name()) };
            }
            unsafe {
                let size = *(arr_ptr.add(16) as *const i32);
                if idx >= size.max(0) as usize {
                    return Err(anyhow!(
                            "GetArray: index {} out of bounds (size={}) in {} at pc={} arr=r{} val={:?}",
                            idx,
                            size,
                            func.name(),
                            frame.pc,
                            array,
                            arr_val
                        ));
                }
                let at = *(arr_ptr.add(8) as *const *mut hl_type);
                if !at.is_null() && !(at as usize).is_multiple_of(std::mem::align_of::<hl_type>()) {
                    return Err(anyhow!(
                            "GetArray: invalid at pointer {:p} in {} at pc={} (arr=r{} val={:?} idx={} r4={:?} r6={:?} r16={:?})",
                            at,
                            func.name(),
                            frame.pc,
                            array,
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
                        } else if (k == hl::hl_type_kind_HDYN || k == hl::hl_type_kind_HNULL)
                            && Self::is_primitive_or_bytes_kind(dst_kind)
                        {
                            Self::unbox_dynamic_to_kind(ptr_val as *mut hl::vdynamic, dst_kind)
                                .unwrap_or_else(|| NanBoxedValue::from_ptr(ptr_val))
                        } else {
                            NanBoxedValue::from_ptr(ptr_val)
                        }
                    }
                }
            }
        };
        frame.registers.set(dst, val);

        Ok(StepResult::Continue)
    }

    /// Allocate a value of the destination's type.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    pub(super) fn op_new(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        dst: u32,
    ) -> Result<StepResult> {
        let frame = self.stack.last_mut().unwrap();
        let type_idx = func.regs[dst as usize].0;
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
                    let f: FnAllocDynObj = unsafe { std::mem::transmute(self.fn_alloc_dynobj) };
                    unsafe { f() }
                }
            }
            hl::hl_type_kind_HVIRTUAL => {
                if c_type_ptr.is_null() || self.fn_alloc_virtual.is_null() {
                    std::ptr::null_mut()
                } else {
                    let f: FnAllocVirtual = unsafe { std::mem::transmute(self.fn_alloc_virtual) };
                    unsafe { f(c_type_ptr as *mut c_void) }
                }
            }
            _ => std::ptr::null_mut(),
        };

        if obj.is_null() {
            frame.registers.set(dst, NanBoxedValue::null());
        } else {
            frame
                .registers
                .set(dst, NanBoxedValue::from_ptr(obj as usize));
        }

        Ok(StepResult::Continue)
    }

    /// Raise a failed checked cast through the current Haxe trap, if any.
    ///
    /// SafeCast runs as an interpreter opcode rather than a native call, so
    /// calling `hlp_dyn_cast*` for its failure path would longjmp without the
    /// native-call setjmp boundary. Represent the same catchable failure in
    /// the interpreter's trap stack instead, carrying the value compiled code
    /// throws: the runtime's `invalid_cast` message as a bytes exception. This
    /// used to hand the catch a null, so `catch (e:Dynamic)` printed "null"
    /// where every other engine printed "Can't cast String to i32"; Assert and
    /// NullCheck already mint their value through `internal_exception_value`.
    pub(super) fn invalid_cast_step(
        &mut self,
        bytecode: &DecodedBytecode,
        src_type_idx: usize,
        dst_type_idx: usize,
    ) -> Result<StepResult> {
        let message = format!(
            "Can't cast {} to {}",
            self.type_str(bytecode, src_type_idx),
            self.type_str(bytecode, dst_type_idx)
        );
        let value = self.internal_exception_value(&message);
        let frame = self.stack.last_mut().unwrap();
        if let Some((target, exc_reg)) = frame.trap_stack.pop() {
            frame.registers.set(exc_reg, value);
            Ok(StepResult::JumpAbs(target))
        } else {
            let stack = self.capture_call_stack(bytecode);
            Err(anyhow::Error::new(HLExceptionPropagation {
                value,
                message: Some(message),
                stack,
            }))
        }
    }

    /// Checked cast, unboxing nullables and validating object hierarchies.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    pub(super) fn op_safe_cast(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        dst: u32,
        src: u32,
    ) -> Result<StepResult> {
        let mut val = self.stack.last().unwrap().registers.get(src);
        let dst_type_idx = func.regs[dst as usize].0;
        let dst_kind = bytecode.types[dst_type_idx].kind;
        let src_type_idx = func.regs[src as usize].0;
        let mut src_kind = bytecode.types[src_type_idx].kind;

        // Virtual registers retain raw objects for interpreter dispatch. At a
        // checked object cast, recover the concrete backing object so the
        // normal HOBJ path below can select its most-derived `__cast` proto
        // (ArrayDyn's override, rather than ArrayBase's inherited fallback).
        if src_kind == hl::hl_type_kind_HVIRTUAL
            && dst_kind == hl::hl_type_kind_HOBJ
            && val.is_ptr()
            && !val.is_null()
        {
            let header = unsafe { *(val.as_ptr() as *const *mut hl_type) };
            if !header.is_null() {
                let runtime_kind = unsafe { (*header).kind };
                if runtime_kind == hl::hl_type_kind_HVIRTUAL {
                    let view = val.as_ptr() as *mut hl::vvirtual;
                    let backing = unsafe { (*view).value };
                    if !backing.is_null() {
                        val = NanBoxedValue::from_ptr(backing as usize);
                        src_kind = unsafe { (*(*backing).t).kind };
                    }
                } else {
                    // AIR V2 intentionally retains the concrete object in an
                    // HVIRTUAL register. It is already the value this cast is
                    // trying to recover; asking hl_to_virtual for a view here
                    // can also touch an uninitialised inline interface cache.
                    src_kind = runtime_kind;
                }
            }
        }

        let result = if val.is_null() || val.is_void() {
            match dst_kind {
                hl::hl_type_kind_HI32 | hl::hl_type_kind_HUI8 | hl::hl_type_kind_HUI16 => {
                    NanBoxedValue::from_i32(0)
                }
                hl::hl_type_kind_HI64 => NanBoxedValue::from_i64(0),
                hl::hl_type_kind_HF32 | hl::hl_type_kind_HF64 => NanBoxedValue::from_f64(0.0),
                hl::hl_type_kind_HBOOL => NanBoxedValue::from_bool(false),
                _ => val,
            }
        } else if val.is_ptr() && val.as_ptr() != 0 {
            if Self::is_unboxable_primitive_kind(dst_kind) {
                // Primitive destination: unbox from vdynamic
                match unsafe {
                    Self::unbox_dynamic_to_kind(val.as_ptr() as *mut hl::vdynamic, dst_kind)
                } {
                    Some(value) => value,
                    None => {
                        // Calls through erased Dynamic signatures can return a
                        // scalar in the machine word. The generic result slot
                        // records that word as a pointer-shaped NanBox value;
                        // tiny values therefore are immediate payloads, not a
                        // vdynamic to dereference. Preserve that representation
                        // boundary while still rejecting real object-to-number
                        // casts below.
                        if matches!(src_kind, hl::hl_type_kind_HDYN | hl::hl_type_kind_HNULL)
                            && val.as_ptr() < 0x10000
                        {
                            let raw = val.as_ptr() as i64;
                            match dst_kind {
                                hl::hl_type_kind_HI32 => NanBoxedValue::from_i32(raw as i32),
                                hl::hl_type_kind_HUI8 => NanBoxedValue::from_i32(raw as u8 as i32),
                                hl::hl_type_kind_HUI16 => {
                                    NanBoxedValue::from_i32(raw as u16 as i32)
                                }
                                hl::hl_type_kind_HI64 => NanBoxedValue::from_i64(raw),
                                hl::hl_type_kind_HF32 | hl::hl_type_kind_HF64 => {
                                    NanBoxedValue::from_f64(raw as f64)
                                }
                                hl::hl_type_kind_HBOOL => NanBoxedValue::from_bool(raw != 0),
                                _ => {
                                    return self.invalid_cast_step(
                                        bytecode,
                                        src_type_idx,
                                        dst_type_idx,
                                    );
                                }
                            }
                        } else {
                            return self.invalid_cast_step(bytecode, src_type_idx, dst_type_idx);
                        }
                    }
                }
            } else {
                // Closure destination: pass the closure through unchanged.
                // Upstream adapts signatures with hl_make_fun_wrapper (a
                // marshalling trampoline); the interpreter needs none — its
                // closures are stub sentinels and EVERY invocation already
                // marshals per the callee's own type, so the declared
                // signature never touches an ABI. Routing this through
                // hlp_dyn_castp instead hit invalid_cast ("Can't cast
                // (fun...) to (fun...)", unit suite Issue5082) and aborted.
                // Guarded on the runtime value actually being a closure so a
                // genuine bad cast still fails.
                if dst_kind == hl::hl_type_kind_HFUN {
                    let rt_kind = unsafe {
                        let d = val.as_ptr() as *mut hl::vdynamic;
                        if !d.is_null() && !(*d).t.is_null() {
                            (*(*d).t).kind
                        } else {
                            hl::hl_type_kind_HVOID
                        }
                    };
                    if rt_kind == hl::hl_type_kind_HFUN || rt_kind == hl::hl_type_kind_HMETHOD {
                        self.stack.last_mut().unwrap().registers.set(dst, val);
                        return Ok(StepResult::Continue);
                    }
                }

                if (src_kind == hl::hl_type_kind_HDYN || src_kind == hl::hl_type_kind_HNULL)
                    && !self.fn_dyn_castp.is_null()
                {
                    // HDYN/HNULL → concrete type: use hlp_dyn_castp
                    let src_c_type = self.c_type_factory.get(src_type_idx) as *mut c_void;
                    let dst_c_type = self.c_type_factory.get(dst_type_idx) as *mut c_void;
                    type FnCastp =
                        unsafe extern "C" fn(*mut c_void, *mut c_void, *mut c_void) -> *mut c_void;
                    let castp: FnCastp = unsafe { std::mem::transmute(self.fn_dyn_castp) };
                    let mut data = val.as_ptr() as *mut c_void;
                    let result_ptr = unsafe {
                        castp(&mut data as *mut _ as *mut c_void, src_c_type, dst_c_type)
                    };
                    if result_ptr.is_null() {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(result_ptr as usize)
                    }
                } else {
                    // For HOBJ→HOBJ: call hlp_dyn_castp for type-safe cast
                    // (validates supertype chain, returns null on mismatch).
                    // For other pointer casts: plain copy.
                    {
                        // Debug: trace HOBJ→HOBJ super chain
                        if src_kind == hl::hl_type_kind_HOBJ
                            && dst_kind == hl::hl_type_kind_HOBJ
                            && val.as_ptr() > 0x10000
                            && env_flag!("ASH_DBG_CAST")
                        {
                            static CAST_COUNT: std::sync::atomic::AtomicU32 =
                                std::sync::atomic::AtomicU32::new(0);
                            let c = CAST_COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                            if (9..12).contains(&c) {
                                // trace casts #9+
                                let obj_ptr = val.as_ptr() as *const hl::vdynamic;
                                let header_t = unsafe { (*obj_ptr).t };
                                let dst_c = self.c_type_factory.get(dst_type_idx);
                                eprintln!(
                                        "[SafeCast-HOBJ#{}] src_tidx={} dst_tidx={} header={:p} dst_c={:p}",
                                        c, src_type_idx, dst_type_idx, header_t, dst_c
                                    );
                                if !header_t.is_null() && (header_t as usize) >= 0x10000 {
                                    unsafe {
                                        let mut cur = header_t;
                                        for d in 0..8 {
                                            if cur.is_null() || (cur as usize) < 0x10000 {
                                                break;
                                            }
                                            let k = (*cur).kind;
                                            if k != hl::hl_type_kind_HOBJ {
                                                eprintln!("  [{d}] kind={k} (not HOBJ)");
                                                break;
                                            }
                                            let obj = (*cur).__bindgen_anon_1.obj;
                                            if obj.is_null() || (obj as usize) < 0x10000 {
                                                eprintln!("  [{d}] obj={obj:p} (invalid)");
                                                break;
                                            }
                                            let name_ptr = (*obj).name;
                                            let name = if !name_ptr.is_null()
                                                && (name_ptr as usize) > 0x10000
                                            {
                                                let mut len = 0;
                                                while *name_ptr.add(len) != 0 && len < 100 {
                                                    len += 1;
                                                }
                                                String::from_utf16_lossy(
                                                    std::slice::from_raw_parts(name_ptr, len),
                                                )
                                            } else {
                                                "?".into()
                                            };
                                            let sup = (*obj).super_;
                                            eprintln!("  [{d}] type={cur:p} obj={obj:p} name={name} super={sup:p}");
                                            if sup.is_null() || (sup as usize) < 0x10000 {
                                                break;
                                            }
                                            cur = sup;
                                        }
                                    }
                                }
                            }
                        }
                        // For HOBJ→HOBJ SafeCast: check if source has __cast proto.
                        // In the interpreter, castFun can't be called (it's a stub
                        // pointer), so we call the __cast bytecode function directly.
                        if src_kind == hl::hl_type_kind_HOBJ && dst_kind == hl::hl_type_kind_HOBJ {
                            // Look up __cast proto findex from the object's runtime type
                            let obj_ptr = val.as_ptr() as *const hl::vdynamic;
                            let header_t = unsafe { (*obj_ptr).t };
                            let (cast_findex, upcast) = if !header_t.is_null()
                                && (header_t as usize) >= 0x10000
                                && unsafe { (*header_t).kind } == hl::hl_type_kind_HOBJ
                            {
                                unsafe {
                                    let obj_t = (*header_t).__bindgen_anon_1.obj;
                                    if !obj_t.is_null() && (obj_t as usize) >= 0x10000 {
                                        // Hash "__cast" using same algorithm as hlp_hash_gen
                                        let cast_hash = {
                                            let chars: &[u16] =
                                                &[0x5F, 0x5F, 0x63, 0x61, 0x73, 0x74]; // __cast
                                            let mut h: i32 = 0;
                                            for &c in chars {
                                                h = h.wrapping_mul(223).wrapping_add(c as i32);
                                            }
                                            h.wrapping_rem(0x1FFFFF7B)
                                        };
                                        // Walk the runtime super chain: __cast is
                                        // inherited (ArrayObj relies on ArrayBase's).
                                        let dst_c0 = self.c_type_factory.get(dst_type_idx);
                                        let dst_obj0 = if !dst_c0.is_null()
                                            && (dst_c0 as usize) >= 0x10000
                                            && (*dst_c0).kind == hl::hl_type_kind_HOBJ
                                        {
                                            (*dst_c0).__bindgen_anon_1.obj
                                        } else {
                                            std::ptr::null_mut()
                                        };
                                        let mut found: Option<usize> = None;
                                        let mut curo = obj_t;
                                        let mut upcast = false;
                                        let mut depth = 0;
                                        while !curo.is_null()
                                            && (curo as usize) >= 0x10000
                                            && depth < 64
                                        {
                                            if !dst_obj0.is_null() && curo == dst_obj0 {
                                                upcast = true;
                                                break;
                                            }
                                            if found.is_none() {
                                                let nproto = (*curo).nproto;
                                                let proto_ptr = (*curo).proto;
                                                if !proto_ptr.is_null()
                                                    && (proto_ptr as usize) >= 0x10000
                                                {
                                                    for i in 0..nproto as usize {
                                                        let proto = &*proto_ptr.add(i);
                                                        if proto.hashed_name == cast_hash {
                                                            found = Some(proto.findex as usize);
                                                            break;
                                                        }
                                                    }
                                                }
                                            }
                                            let sup = (*curo).super_;
                                            if sup.is_null() || (sup as usize) < 0x10000 {
                                                break;
                                            }
                                            if (*sup).kind != hl::hl_type_kind_HOBJ {
                                                break;
                                            }
                                            curo = (*sup).__bindgen_anon_1.obj;
                                            depth += 1;
                                        }
                                        if upcast {
                                            (None, true)
                                        } else {
                                            (found, false)
                                        }
                                    } else {
                                        (None, false)
                                    }
                                }
                            } else {
                                (None, false)
                            };

                            if let Some(findex) = cast_findex {
                                // Call __cast(obj, dst_type) via StepResult::Call
                                let dst_c_type = self.c_type_factory.get(dst_type_idx);
                                let type_val = NanBoxedValue::from_ptr(dst_c_type as usize);
                                // Store args in registers and dispatch as a call
                                self.stack.last_mut().unwrap().registers.set(dst, val);
                                return Ok(StepResult::Call {
                                    findex,
                                    args: vec![val, type_val],
                                    dst,
                                });
                            } else if upcast {
                                val
                            } else {
                                return self.invalid_cast_step(
                                    bytecode,
                                    src_type_idx,
                                    dst_type_idx,
                                );
                            }
                        } else {
                            val // non-HOBJ cast, just copy
                        }
                    }
                }
            }
        } else {
            // An unboxed primitive can inhabit HDYN/HNULL registers in the
            // interpreter. SafeCast is the point where it re-enters a concrete
            // register, so normalize its representation to that static kind.
            Self::coerce_value_for_static_kind(val, dst_kind)
        };
        self.stack.last_mut().unwrap().registers.set(dst, result);

        Ok(StepResult::Continue)
    }

    /// Materialize HashLink's canonical structural-interface view.
    ///
    /// Keeping the source HOBJ pointer in an HVIRTUAL register looks harmless
    /// while the value stays in the interpreter, because its field helpers can
    /// resolve by hash. It is not ABI-compatible once that value is cached in
    /// an object field or passed to compiled AIR V2: generated code correctly
    /// reads the `vvirtual` header and its field-address table. A raw object in
    /// that slot therefore turns ordinary object fields into bogus virtual
    /// entries. Upstream's OToVirtual calls `hl_to_virtual`, and every Ash
    /// execution tier must preserve that representation boundary as well.
    pub(super) fn op_to_virtual(
        &mut self,
        func: &HLFunction,
        dst: u32,
        src: u32,
    ) -> Result<StepResult> {
        let value = self.stack.last().unwrap().registers.get(src);
        if value.is_null() || value.is_void() {
            self.stack.last_mut().unwrap().registers.set(dst, value);
            return Ok(StepResult::Continue);
        }
        if !value.is_ptr() || self.fn_to_virtual.is_null() {
            return Err(anyhow!("ToVirtual cannot materialize value {value:?}"));
        }

        let dst_type = self.c_type_factory.get(func.regs[dst as usize].0);
        if dst_type.is_null() {
            return Err(anyhow!("ToVirtual destination type is unavailable"));
        }

        // The helper allocates. Publish the backing object before entering it;
        // on return there is no allocation point before the view is installed
        // in the destination register and becomes part of the live root set.
        self.sync_gc_scan_roots();
        type FnToVirtual =
            unsafe extern "C" fn(*mut hl_type, *mut hl::vdynamic) -> *mut hl::vvirtual;
        let to_virtual: FnToVirtual = unsafe { std::mem::transmute(self.fn_to_virtual) };
        // Through the trap boundary: materializing a view over a dynobj
        // recasts mismatched fields, and a failed recast throws. Without an
        // installed trap that longjmp aborts the process instead of
        // surfacing as a catchable HL exception.
        let stack_depth = self.stack.len();
        let mut view: *mut hl::vvirtual = std::ptr::null_mut();
        let jumped = run_with_hl_trap(self.fn_setup_trap_jit, self.fn_remove_trap_jit, || {
            view = unsafe { to_virtual(dst_type, value.as_ptr() as *mut hl::vdynamic) };
        });
        if jumped != 0 {
            return Err(self.longjmp_error(
                None,
                stack_depth,
                "exception while materializing a virtual view".to_string(),
            ));
        }
        if view.is_null() {
            return Err(anyhow!("ToVirtual returned null for a non-null object"));
        }
        self.stack
            .last_mut()
            .unwrap()
            .registers
            .set(dst, NanBoxedValue::from_ptr(view as usize));
        Ok(StepResult::Continue)
    }

    /// Box a value into a vdynamic for native consumption.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    pub(super) fn op_to_dyn(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        dst: u32,
        src: u32,
    ) -> Result<StepResult> {
        let frame = self.stack.last_mut().unwrap();
        // Box a value into a vdynamic* for native code consumption.
        // Pointer types (HOBJ, HDYN, etc.) already have a vdynamic header - pass through.
        // Primitive types (HI32, HF64, HBOOL, HBYTES) need hlp_make_dyn wrapping.
        let src_type_ref = &func.regs[src as usize];
        let src_kind = bytecode.types[src_type_ref.0].kind;
        let val = frame.registers.get(src);

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
                // HABSTRACT is a pointer but not a dynamic kind, so a raw
                // copy leaves a Dynamic whose first word is the abstract's
                // own payload, not an hl_type. hl_dyn_castp reads that word
                // on the way back out. Upstream's OToDyn boxes every
                // non-dynamic kind for exactly this reason.
                | hl::hl_type_kind_HABSTRACT
        );

        if needs_boxing && !self.fn_make_dyn.is_null() {
            let c_type_ptr = self.c_type_factory.get(src_type_ref.0);
            // Create a stack slot holding the raw value for hlp_make_dyn
            let mut data: i64 = if val.is_i32() {
                val.as_i32() as i64
            } else if val.is_i64() {
                val.as_i64_lossy()
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
                .set(dst, NanBoxedValue::from_ptr(dyn_ptr as usize));
        } else {
            // Already a pointer type with vdynamic header, or no make_dyn available
            frame.registers.set(dst, val);
        }

        Ok(StepResult::Continue)
    }

    /// Write a field by name hash on a dynamic value.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    pub(super) fn op_dyn_set(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        obj: u32,
        field: usize,
        src: u32,
    ) -> Result<StepResult> {
        let fn_hash_gen = self.fn_hash_gen;
        let (mk_dyn, pt_i32, pt_f64, pt_bool) = (
            self.fn_make_dyn,
            self.prim_t_i32,
            self.prim_t_f64,
            self.prim_t_bool,
        );
        let frame = self.stack.last_mut().unwrap();
        let obj_val = frame.registers.get(obj);
        if obj_val.is_null() || obj_val.is_void() {
            // no-op
        } else {
            let hfield = hash_field_name(
                bytecode,
                field,
                fn_hash_gen,
                &mut self.utf16_strings,
                &mut self.field_hash_cache,
            )?;
            let obj_ptr = obj_val.as_ptr() as *mut c_void;
            let src_type_idx = func.regs[src as usize].0;
            let src_kind = bytecode.types[src_type_idx].kind;
            // Dynamic values may be unboxed while they live in registers,
            // but a named field with dynamic type stores a vdynamic*. In
            // particular, the raw bits of 0.0 are a null pointer if written
            // without this boundary box.
            let src_val = Self::box_for_dynamic_slot(
                mk_dyn,
                pt_i32,
                pt_f64,
                pt_bool,
                src_kind,
                frame.registers.get(src),
            );
            if env_flag!("ASH_DBG_DYN") {
                let fname = bytecode
                    .strings
                    .get(field)
                    .map(String::as_str)
                    .unwrap_or("<oob>");
                eprintln!(
                        "[DYNSET] f{} pc={} obj={:?} field={} name={} hash={} src_ty={} src_kind={} src={:?}",
                        func_idx, frame.pc, obj_val, field, fname, hfield, src_type_idx, src_kind, src_val
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

        Ok(StepResult::Continue)
    }

    /// Read a field by name hash off a dynamic value.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    pub(super) fn op_dyn_get(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        dst: u32,
        obj: u32,
        field: usize,
    ) -> Result<StepResult> {
        let fn_hash_gen = self.fn_hash_gen;
        let frame = self.stack.last_mut().unwrap();
        let obj_val = frame.registers.get(obj);
        if obj_val.is_null() || obj_val.is_void() {
            frame.registers.set(dst, NanBoxedValue::null());
        } else {
            let hfield = hash_field_name(
                bytecode,
                field,
                fn_hash_gen,
                &mut self.utf16_strings,
                &mut self.field_hash_cache,
            )?;
            if env_flag!("ASH_DBG_DYN") {
                let fname = bytecode
                    .strings
                    .get(field)
                    .map(String::as_str)
                    .unwrap_or("<oob>");
                eprintln!(
                    "[DYNGET] f{} pc={} obj={:?} field={} name={} hash={}",
                    func_idx, frame.pc, obj_val, field, fname, hfield
                );
            }
            let obj_ptr = obj_val.as_ptr() as *mut c_void;
            let dst_type_idx = func.regs[dst as usize].0;
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
            if env_flag!("ASH_DBG_DYN") {
                eprintln!(
                    "[DYNGET] f{} pc={} dst_kind={} -> {:?}",
                    func_idx, frame.pc, dst_kind, out
                );
            }
            frame.registers.set(dst, out);
        }

        Ok(StepResult::Continue)
    }

    /// Write `obj.field` for any object representation.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    pub(super) fn op_field_set(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        obj: u32,
        field: usize,
        src: u32,
    ) -> Result<StepResult> {
        let (mk_dyn, pt_i32, pt_f64, pt_bool) = (
            self.fn_make_dyn,
            self.prim_t_i32,
            self.prim_t_f64,
            self.prim_t_bool,
        );
        let frame = self.stack.last_mut().unwrap();
        let obj_type_idx = func.regs[obj as usize].0;
        let obj_kind = bytecode.types[obj_type_idx].kind;
        let obj_c_type = self.c_type_factory.get(obj_type_idx) as *mut c_void;
        let src_type_idx = func.regs[src as usize].0;
        let src_kind = bytecode.types[src_type_idx].kind;
        let get_rt = self.fn_get_obj_rt;
        let obj_val = frame.registers.get(obj);
        if env_flag!("ASH_DBG_FIELD") {
            eprintln!(
                    "[SETFIELD] f{} pc={} obj_ty={} obj_kind={} field={} src_ty={} src_kind={} obj={:?} src={:?}",
                    func_idx,
                    frame.pc,
                    obj_type_idx,
                    obj_kind,
                    field,
                    src_type_idx,
                    src_kind,
                    obj_val,
                    frame.registers.get(src)
                );
        }
        if !obj_val.is_null() && !obj_val.is_void() {
            // A Dynamic slot takes a box, not a raw payload. See
            // `box_for_dynamic_slot`.
            let src_val = Self::box_for_dynamic_slot(
                mk_dyn,
                pt_i32,
                pt_f64,
                pt_bool,
                src_kind,
                frame.registers.get(src),
            );
            if obj_kind == hl::hl_type_kind_HOBJ || obj_kind == hl::hl_type_kind_HSTRUCT {
                let obj_ptr = obj_val.as_ptr() as *mut u8;
                if env_flag!("ASH_DBG_FIELD") {
                    eprintln!(
                            "[SETFIELD-OBJ] f{} pc={} obj_ty={} obj_kind={} field={} src_kind={} src={:?}",
                            func_idx, frame.pc, obj_type_idx, obj_kind, field, src_kind, src_val
                        );
                }
                unsafe {
                    Self::write_obj_field(
                        obj_ptr, field, src_kind, src_val, obj_c_type, obj_kind, get_rt,
                    );
                }
            } else if obj_kind == hl::hl_type_kind_HVIRTUAL {
                if let Some(offset) = unsafe {
                    Self::resolve_virtual_field_offset(
                        obj_val.as_ptr() as *mut u8,
                        obj_c_type,
                        field,
                    )
                } {
                    let obj_ptr = obj_val.as_ptr() as *mut u8;
                    let addr = unsafe { obj_ptr.add(offset) };
                    if env_flag!("ASH_DBG_FIELD") {
                        eprintln!(
                                "[SETFIELD-VIRT] f{} pc={} obj_ty={} field={} off={} src_kind={} src={:?}",
                                func_idx, frame.pc, obj_type_idx, field, offset, src_kind, src_val
                            );
                    }
                    unsafe { Self::write_value_at(addr, src_kind, src_val) };
                } else {
                    self.virtual_fields
                        .insert((obj_val.as_ptr(), field), src_val);
                    if let Some(hfield) =
                        Self::resolve_typed_field_hash(bytecode, obj_type_idx, field)
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
                    if env_flag!("ASH_DBG_FIELD") {
                        eprintln!(
                            "[SETFIELD-VIRT-FALLBACK] f{} pc={} obj_ty={} field={} src={:?}",
                            func_idx, frame.pc, obj_type_idx, field, src_val
                        );
                    }
                }
            } else if let Some(hfield) =
                Self::resolve_typed_field_hash(bytecode, obj_type_idx, field)
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

        Ok(StepResult::Continue)
    }

    /// Read `obj.field` for any object representation.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    pub(super) fn op_field_get(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        dst: u32,
        obj: u32,
        field: usize,
    ) -> Result<StepResult> {
        let frame = self.stack.last_mut().unwrap();
        // Extract c_type info before borrowing frame mutably
        let obj_type_idx = func.regs[obj as usize].0;
        let obj_kind = bytecode.types[obj_type_idx].kind;
        let obj_c_type = self.c_type_factory.get(obj_type_idx) as *mut c_void;
        let dst_kind = bytecode.types[func.regs[dst as usize].0].kind;
        let get_rt = self.fn_get_obj_rt;
        let obj_val = frame.registers.get(obj);
        if env_flag!("ASH_DBG_FIELD") {
            eprintln!(
                "[FIELD] f{} pc={} obj_ty={} obj_kind={} field={} dst_kind={} obj={:?}",
                func_idx, frame.pc, obj_type_idx, obj_kind, field, dst_kind, obj_val
            );
        }
        if obj_val.is_null() || obj_val.is_void() {
            frame.registers.set(dst, NanBoxedValue::null());
        } else if obj_kind == hl::hl_type_kind_HOBJ || obj_kind == hl::hl_type_kind_HSTRUCT {
            let obj_ptr = obj_val.as_ptr() as *mut u8;
            let val = unsafe {
                Self::read_obj_field(obj_ptr, field, dst_kind, obj_c_type, obj_kind, get_rt)
            };
            if env_flag!("ASH_DBG_FIELD") {
                eprintln!(
                    "[GETFIELD-OBJ] f{} pc={} obj_ty={} obj_kind={} field={} dst_kind={} -> {:?}",
                    func_idx, frame.pc, obj_type_idx, obj_kind, field, dst_kind, val
                );
            }
            frame.registers.set(dst, val);
        } else if obj_kind == hl::hl_type_kind_HVIRTUAL {
            if let Some(offset) = unsafe {
                Self::resolve_virtual_field_offset(obj_val.as_ptr() as *mut u8, obj_c_type, field)
            } {
                let obj_ptr = obj_val.as_ptr() as *mut u8;
                let addr = unsafe { obj_ptr.add(offset) };
                let val = unsafe { Self::read_value_at(addr, dst_kind) };
                if env_flag!("ASH_DBG_FIELD") {
                    eprintln!(
                        "[GETFIELD-VIRT] f{} pc={} obj_ty={} field={} off={} dst_kind={} -> {:?}",
                        func_idx, frame.pc, obj_type_idx, field, offset, dst_kind, val
                    );
                }
                frame.registers.set(dst, val);
            } else {
                let key = (obj_val.as_ptr(), field);
                let val = if let Some(v) = self.virtual_fields.get(&key).copied() {
                    v
                } else if let Some(hfield) =
                    Self::resolve_typed_field_hash(bytecode, obj_type_idx, field)
                {
                    let dst_type_idx = func.regs[dst as usize].0;
                    let dst_type_ptr = self.c_type_factory.get(dst_type_idx) as *mut c_void;
                    Self::dyn_get_field_by_hash(
                        obj_val.as_ptr() as *mut c_void,
                        hfield,
                        dst_kind,
                        dst_type_ptr,
                        self.fn_dyn_getd,
                        self.fn_dyn_getf,
                        self.fn_dyn_geti64,
                        self.fn_dyn_geti,
                        self.fn_dyn_getp,
                    )
                } else {
                    NanBoxedValue::null()
                };
                if env_flag!("ASH_DBG_FIELD") {
                    eprintln!(
                        "[GETFIELD-VIRT-FALLBACK] f{} pc={} obj_ty={} field={} -> {:?}",
                        func_idx, frame.pc, obj_type_idx, field, val
                    );
                }
                frame.registers.set(dst, val);
            }
        } else if let Some(hfield) = Self::resolve_typed_field_hash(bytecode, obj_type_idx, field) {
            let obj_ptr = obj_val.as_ptr() as *mut c_void;
            let dst_type_idx = func.regs[dst as usize].0;
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
            frame.registers.set(dst, out);
        } else {
            frame.registers.set(dst, NanBoxedValue::null());
        }

        Ok(StepResult::Continue)
    }

    /// Materialize a vclosure for a virtual method of an object.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    pub(super) fn op_virtual_closure(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        dst: u32,
        obj: u32,
        field: u32,
    ) -> Result<StepResult> {
        let frame = self.stack.last_mut().unwrap();
        // Resolve the virtual method findex from the object's proto chain,
        // then create a vclosure with the object as bound value.
        let obj_val = frame.registers.get(obj);
        if obj_val.is_null() || obj_val.is_void() {
            frame.registers.set(dst, NanBoxedValue::null());
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
                    let fi = field as usize;
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
                // The METHOD's full type, for the same reason
                // `op_instance_closure` passes it: the destination register
                // carries the already-stripped signature with a null parent,
                // and a bound closure's dynamic callers read that parent to
                // learn they must marshal the receiver.
                let closure_type = func_of(&self.targets, findex)
                    .map(|fi| self.c_type_factory.get(bytecode.functions[fi].type_.0))
                    .unwrap_or_else(|| self.c_type_factory.get(func.regs[dst as usize].0));
                let value = unsafe {
                    Self::alloc_bound_closure(
                        self.fn_alloc_closure_ptr,
                        closure_type,
                        findex,
                        obj_val.as_ptr() as *mut std::ffi::c_void,
                    )
                };
                frame.registers.set(dst, value);
            } else {
                frame.registers.set(dst, NanBoxedValue::null());
            }
        }

        Ok(StepResult::Continue)
    }

    /// Materialize a vclosure bound to an object.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    pub(super) fn op_instance_closure(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        dst: u32,
        fun: usize,
        obj: u32,
    ) -> Result<StepResult> {
        let frame = self.stack.last_mut().unwrap();
        // Create a _vclosure with the bound object. The closure's fun
        // pointer is the stub sentinel (findex+1) so that CallClosure
        // can extract the findex. The bound object is stored in
        // vclosure.value and prepended as the first argument on CallClosure.
        let obj_val = frame.registers.get(obj);
        let obj_ptr = if obj_val.is_null() || obj_val.is_void() {
            std::ptr::null_mut()
        } else {
            obj_val.as_ptr() as *mut std::ffi::c_void
        };
        // Hand the allocator the METHOD's full type -- the one whose first
        // argument is the receiver -- exactly as HashLink's OInstanceClosure
        // does. `hlp_alloc_closure_ptr` strips it down to the closure's own
        // signature and, in doing so, sets `fun->parent` back to the full
        // type it was given.
        //
        // The destination register's type is the ALREADY-stripped signature,
        // and the bytecode reader hard-codes `parent: None` for every HFUN it
        // builds (crates/ash/src/bytecode.rs, `read_type_fun`), so passing it
        // hands the allocator a type whose parent is null and leaves it null.
        // Five places in std/src/fun.rs read `cl->t->fun->parent` for a bound
        // closure, and the fiber's dynamic runner reads it to learn that it
        // must marshal the receiver: with it null, the runner built a 1-value
        // argument array against an arity-0 signature, the receiver was never
        // passed, and the callee read `this` out of a register nobody set --
        // SIGSEGV at offset 0x20 inside the compiled method.
        let closure_type = func_of(&self.targets, fun)
            .map(|fi| self.c_type_factory.get(bytecode.functions[fi].type_.0))
            .unwrap_or_else(|| self.c_type_factory.get(func.regs[dst as usize].0));
        let value = unsafe {
            Self::alloc_bound_closure(self.fn_alloc_closure_ptr, closure_type, fun, obj_ptr)
        };
        frame.registers.set(dst, value);

        Ok(StepResult::Continue)
    }

    /// Materialize a vclosure for a bare function index.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    pub(super) fn op_static_closure(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        dst: u32,
        fun: usize,
    ) -> Result<StepResult> {
        let frame = self.stack.last_mut().unwrap();
        // Materialize a real vclosure* so std natives such as
        // hl.Api.noClosure / Reflect.callMethod can consume it.
        let findex = fun;
        let type_idx = if let Some(fidx) = func_of(&self.targets, findex) {
            bytecode.functions[fidx].type_.0
        } else if let Some(nidx) = native_of(&self.targets, findex) {
            bytecode.natives[nidx].type_.0
        } else {
            usize::MAX
        };

        if type_idx != usize::MAX && !self.fn_alloc_closure_void.is_null() {
            type FnAllocClosureVoid =
                unsafe extern "C" fn(*mut c_void, *mut c_void) -> *mut _vclosure;
            let f: FnAllocClosureVoid = unsafe { std::mem::transmute(self.fn_alloc_closure_void) };
            let tptr = self.c_type_factory.get(type_idx) as *mut c_void;
            let closure = unsafe { f(tptr, (findex + 1) as *mut c_void) };
            if !closure.is_null() {
                if env_flag!("ASH_DBG_CLOSURE") {
                    eprintln!(
                        "[STATICCLOSURE] findex={} type_idx={} -> {:p}",
                        findex, type_idx, closure
                    );
                }
                frame
                    .registers
                    .set(dst, NanBoxedValue::from_ptr(closure as usize));
                return Ok(StepResult::Continue);
            }
        }

        // Fallback to interpreter-local representation.
        if env_flag!("ASH_DBG_CLOSURE") {
            eprintln!(
                "[STATICCLOSURE-FALLBACK] findex={} type_idx={} alloc_fn={:p}",
                findex, type_idx, self.fn_alloc_closure_void
            );
        }
        frame
            .registers
            .set(dst, NanBoxedValue::from_func_index(findex));

        Ok(StepResult::Continue)
    }

    /// Build the `Array<Dynamic>` passed to the closure wrapped by
    /// `Reflect.makeVarArgs`.
    pub(super) fn pack_varargs_array(
        &mut self,
        func: &HLFunction,
        args: &[Reg],
        values: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        if self.fn_alloc_array.is_null() || self.prim_t_dyn.is_null() {
            return Err(anyhow!("HashLink varargs array allocator is unavailable"));
        }
        type FnAllocArray = unsafe extern "C" fn(*mut hl_type, i32) -> *mut hl::varray;
        let alloc: FnAllocArray = unsafe { std::mem::transmute(self.fn_alloc_array) };
        let array = unsafe { alloc(self.prim_t_dyn as *mut hl_type, values.len() as i32) };
        if array.is_null() {
            return Err(anyhow!("HashLink varargs array allocation failed"));
        }

        let data = unsafe {
            (array as *mut u8).add(std::mem::size_of::<hl::varray>()) as *mut *mut hl::vdynamic
        };
        for (i, (&reg, &value)) in args.iter().zip(values).enumerate() {
            let type_idx = func.regs[reg.0 as usize].0;
            let c_type = self.c_type_factory.get(type_idx);
            let boxed = self.box_value_as_dynamic_with_type(value, c_type);
            let ptr = if boxed.is_null() || boxed.is_void() {
                std::ptr::null_mut()
            } else if boxed.is_ptr() {
                boxed.as_ptr() as *mut hl::vdynamic
            } else {
                return Err(anyhow!(
                    "could not box vararg {} with type index {}",
                    i,
                    type_idx
                ));
            };
            unsafe { *data.add(i) = ptr };
        }
        Ok(NanBoxedValue::from_ptr(array as usize))
    }

    /// Resolve and stage a call through a closure value.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    pub(super) fn op_call_closure(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        dst: u32,
        fun: u32,
        args: &[Reg],
    ) -> Result<StepResult> {
        let (closure_val, mut arg_vals, call_pc) = {
            let frame = self.stack.last_mut().unwrap();
            (
                frame.registers.get(fun),
                args.iter()
                    .map(|r| frame.registers.get(r.0))
                    .collect::<Vec<_>>(),
                frame.pc,
            )
        };

        if closure_val.is_null() || closure_val.is_void() {
            return Err(anyhow!("CallClosure on null closure (pc={call_pc})"));
        }

        // The closure value might be:
        // 1. A TAG_FUNC: raw function index (from StaticClosure with no capture)
        // 2. A TAG_PTR to a _vclosure struct (InstanceClosure with bound value)
        let findex = if closure_val.is_func() {
            closure_val.as_func_index()
        } else {
            let raw = closure_val.as_ptr();
            if func_of(&self.targets, raw).is_some() || native_of(&self.targets, raw).is_some() {
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

                    // `hlp_make_var_args` does not store an interpreter stub
                    // in `fun`: it stores HashLink's real `fun_var_args`
                    // sentinel and keeps the original Haxe closure in
                    // `value`. Native HashLink recognizes that sentinel,
                    // packs the typed arguments into Array<Dynamic>, then
                    // invokes the wrapped closure. Treating the native
                    // address as `findex + 1` produced enormous bogus
                    // findexes on both arm64 and x86-64.
                    if !self.fn_fun_var_args.is_null() && fun_ptr == self.fn_fun_var_args {
                        let wrapped = (*cl_ptr).value as *const _vclosure;
                        if wrapped.is_null() {
                            return Err(anyhow!("varargs closure has no wrapped closure"));
                        }
                        let packed = self.pack_varargs_array(func, args, &arg_vals)?;
                        let wrapped_fun = (*wrapped).fun as usize;
                        let fi = if (wrapped_fun as u64)
                            < ash_core::llvm::stub_bridge::STUB_SENTINEL_LIMIT
                        {
                            wrapped_fun.wrapping_sub(1)
                        } else {
                            self.findex_for_code_addr(wrapped_fun).unwrap_or(usize::MAX)
                        };
                        if func_of(&self.targets, fi).is_none()
                            && native_of(&self.targets, fi).is_none()
                        {
                            return Err(anyhow!("varargs wrapped closure has invalid findex {fi}"));
                        }
                        arg_vals.clear();
                        if (*wrapped).hasValue != 0 && !(*wrapped).value.is_null() {
                            arg_vals.push(NanBoxedValue::from_ptr((*wrapped).value as usize));
                        }
                        arg_vals.push(packed);
                        return Ok(StepResult::Call {
                            findex: fi,
                            args: arg_vals,
                            dst,
                        });
                    }

                    // `fun` holds either the interpreter's `findex + 1` stub
                    // sentinel or, when compiled code allocated this closure
                    // from `functions_ptrs`, a real entry address.
                    let fi = if (fun_ptr as u64) < ash_core::llvm::stub_bridge::STUB_SENTINEL_LIMIT
                    {
                        (fun_ptr as usize).wrapping_sub(1)
                    } else {
                        self.findex_for_code_addr(fun_ptr as usize).ok_or_else(|| {
                            anyhow!("CallClosure on unknown compiled closure {fun_ptr:?}")
                        })?
                    };
                    let bound_value = (*cl_ptr).hasValue != 0 && !(*cl_ptr).value.is_null();
                    // What this site called, for the LLVM tier's guarded
                    // devirtualisation. Only sentinel-form targets are worth
                    // recording: the emitted guard compares the fun field
                    // against `findex + 1`, which is what that form holds.
                    if self.tiered_runtime.is_some()
                        && (fun_ptr as u64) < ash_core::llvm::stub_bridge::STUB_SENTINEL_LIMIT
                    {
                        ash_core::callsite_profile::record_closure(
                            bytecode.functions[func_idx].findex as u32,
                            call_pc as u32,
                            fi as u32,
                            bound_value,
                        );
                    }
                    // If the closure has a bound value, prepend it as the first arg
                    if bound_value {
                        let bound = NanBoxedValue::from_ptr((*cl_ptr).value as usize);
                        arg_vals.insert(0, bound);
                    }
                    fi
                }
            }
        };

        Ok(StepResult::Call {
            findex,
            args: arg_vals,
            dst,
        })
    }

    /// Resolve and stage a method call through the receiver's vtable slot.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    pub(super) fn op_call_method(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        op_is_this: bool,
        dst: u32,
        field: usize,
        args: &[Reg],
    ) -> Result<StepResult> {
        // CallMethod: args[0] is 'this'. CallThis: the receiver is
        // IMPLICITLY register 0 (HashLink OCallThis semantics) and
        // args hold only the real arguments — prepend Reg(0), else
        // method resolution runs against the first argument's type.
        let args_with_this: Vec<Reg> = if op_is_this {
            let mut v = Vec::with_capacity(args.len() + 1);
            v.push(Reg(0));
            v.extend(args.iter().copied());
            v
        } else {
            args.to_vec()
        };
        let args = &args_with_this;
        let (arg_vals, call_pc) = {
            let frame = self.stack.last().unwrap();
            (
                args.iter()
                    .map(|r| frame.registers.get(r.0))
                    .collect::<Vec<_>>(),
                frame.pc,
            )
        };
        let this_val = arg_vals[0];

        if this_val.is_null() || this_val.is_void() {
            return Err(anyhow!(
                "CallMethod on null object (field={}, pc={})",
                field,
                call_pc
            ));
        }

        // HVIRTUAL dispatch. Canonical values are vvirtual views; tolerate a
        // raw HOBJ as well for values arriving from older/external producers.
        // Resolve the findex by matching the virtual field's hashed_name
        // against the runtime object's proto chain.
        let this_reg_type_idx = func.regs[args[0].0 as usize].0;
        if this_reg_type_idx < bytecode.types.len()
            && bytecode.types[this_reg_type_idx].kind == hl::hl_type_kind_HVIRTUAL
        {
            let virt_type = self.c_type_factory.get(this_reg_type_idx);
            let obj_ptr = this_val.as_ptr() as *const u8;
            let (findex_opt, receiver, hfield, needs_boxed_dispatch) = unsafe {
                // Get hashed_name of the virtual field
                let virt = (*virt_type).__bindgen_anon_1.virt.as_ref();
                if let Some(virt_data) = virt {
                    if (field as i32) < virt_data.nfields {
                        let virt_field = &*virt_data.fields.add(field);
                        let hname = virt_field.hashed_name;
                        // ToVirtual can leave a raw object in the register, or
                        // native field access can materialize a real vvirtual
                        // view. A view dispatches against its wrapped object
                        // and passes that object as `this`; looking for an
                        // object proto on the HVIRTUAL header itself finds
                        // nothing and made every iterator-style interface call
                        // fail at field zero.
                        let header = *(obj_ptr as *const *mut hl_type);
                        let dispatch_obj =
                            if !header.is_null() && (*header).kind == hl::hl_type_kind_HVIRTUAL {
                                let value = (*(obj_ptr as *const hl::vvirtual)).value;
                                if value.is_null() {
                                    std::ptr::null()
                                } else {
                                    value as *const u8
                                }
                            } else {
                                obj_ptr
                            };
                        if !header.is_null() && (*header).kind == hl::hl_type_kind_HVIRTUAL {
                            let dispatch_type = if dispatch_obj.is_null() {
                                std::ptr::null_mut()
                            } else {
                                *(dispatch_obj as *const *mut hl_type)
                            };
                            if dispatch_obj.is_null()
                                || (!dispatch_type.is_null()
                                    && (*dispatch_type).kind == hl::hl_type_kind_HDYNOBJ)
                            {
                                // A virtual over an anonymous object stores an
                                // address for each matching field immediately
                                // after the vvirtual header. Function fields
                                // are closure slots, not object protos. A
                                // self-backed virtual uses the same layout with
                                // `value == null`; invoke that closure and omit
                                // the structural wrapper from the argument list.
                                let fields = obj_ptr.add(std::mem::size_of::<hl::vvirtual>())
                                    as *const *mut c_void;
                                let slot = *fields.add(field);
                                if !slot.is_null() {
                                    let closure = *(slot as *const *const _vclosure);
                                    if !closure.is_null() {
                                        let cfun = (*closure).fun as usize;
                                        let fi = if (cfun as u64)
                                            < ash_core::llvm::stub_bridge::STUB_SENTINEL_LIMIT
                                        {
                                            cfun.wrapping_sub(1)
                                        } else {
                                            self.findex_for_code_addr(cfun).unwrap_or(usize::MAX)
                                        };
                                        if func_of(&self.targets, fi).is_some()
                                            || native_of(&self.targets, fi).is_some()
                                        {
                                            let mut call_args = arg_vals[1..].to_vec();
                                            if (*closure).hasValue != 0
                                                && !(*closure).value.is_null()
                                            {
                                                call_args.insert(
                                                    0,
                                                    NanBoxedValue::from_ptr(
                                                        (*closure).value as usize,
                                                    ),
                                                );
                                            }
                                            return Ok(StepResult::Call {
                                                findex: fi,
                                                args: call_args,
                                                dst,
                                            });
                                        }
                                    }
                                }
                            }
                        }
                        // Upstream's OCallMethod fast path for a real view
                        // over an object: `hl_to_virtual` already resolved
                        // each METHOD field to the target's entry from
                        // `rt->methods`, so `vfields[field]` holds a function
                        // address — a `findex + 1` stub sentinel or compiled
                        // code — and the call receiver is the wrapped value.
                        // Re-resolving by hashed name here is both slower and
                        // weaker: it cannot see what the view already bound.
                        if !header.is_null()
                            && (*header).kind == hl::hl_type_kind_HVIRTUAL
                            && !dispatch_obj.is_null()
                            && {
                                let dk = *(dispatch_obj as *const *mut hl_type);
                                !dk.is_null()
                                    && ((*dk).kind == hl::hl_type_kind_HOBJ
                                        || (*dk).kind == hl::hl_type_kind_HSTRUCT)
                            }
                        {
                            let fields = obj_ptr.add(std::mem::size_of::<hl::vvirtual>())
                                as *const *mut c_void;
                            let entry = *fields.add(field) as usize;
                            if entry != 0 {
                                let fi = if (entry as u64)
                                    < ash_core::llvm::stub_bridge::STUB_SENTINEL_LIMIT
                                {
                                    entry.wrapping_sub(1)
                                } else {
                                    self.findex_for_code_addr(entry).unwrap_or(usize::MAX)
                                };
                                // Direct only when the callee's declared
                                // return and the call site's destination agree
                                // on representation. The view may be typed
                                // Iterator<Int> while the call site reads it
                                // as Iterator<Dynamic> (type-parameter
                                // erasure): a raw i32 return stored into a
                                // Dynamic register is a pointer-shaped lie.
                                // Upstream's fast path calls through
                                // emit_dyn_call, which coerces the return;
                                // ours falls back to the boxed dispatch below,
                                // which marshals both directions.
                                let ret_compatible = func_of(&self.targets, fi)
                                    .and_then(|f_idx| {
                                        let ft = &bytecode.functions[f_idx];
                                        bytecode.types[ft.type_.0]
                                            .fun
                                            .as_ref()
                                            .map(|f| bytecode.types[f.ret.0].kind)
                                    })
                                    .map(|ret_kind| {
                                        let dst_kind =
                                            bytecode.types[func.regs[dst as usize].0].kind;
                                        Self::is_ptr_kind(ret_kind) == Self::is_ptr_kind(dst_kind)
                                    });
                                if ret_compatible == Some(true)
                                    && (func_of(&self.targets, fi).is_some()
                                        || native_of(&self.targets, fi).is_some())
                                {
                                    let mut call_args = arg_vals;
                                    call_args[0] = NanBoxedValue::from_ptr(dispatch_obj as usize);
                                    return Ok(StepResult::Call {
                                        findex: fi,
                                        args: call_args,
                                        dst,
                                    });
                                }
                            }
                        }
                        if dispatch_obj.is_null() {
                            (None, this_val, hname, false)
                        } else {
                            // Walk the runtime obj's proto chain for hname.
                            let mut obj_hl_type = *(dispatch_obj as *const *mut hl_type);
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
                                // Try super class.
                                obj_hl_type = (*obj).super_;
                            }

                            // A class value can satisfy a structural function
                            // field with one of its static closures. Such a
                            // field is object data, not an instance proto, so
                            // resolve it by hash and invoke the closure without
                            // passing the structural receiver as `this`.
                            if found.is_none()
                                && !header.is_null()
                                && (*header).kind == hl::hl_type_kind_HOBJ
                            {
                                let closure_value = Self::dyn_get_field_by_hash(
                                    dispatch_obj as *mut c_void,
                                    hname,
                                    (*virt_field.t).kind,
                                    virt_field.t as *mut c_void,
                                    self.fn_dyn_getd,
                                    self.fn_dyn_getf,
                                    self.fn_dyn_geti64,
                                    self.fn_dyn_geti,
                                    self.fn_dyn_getp,
                                );
                                if closure_value.is_ptr() {
                                    let closure = closure_value.as_ptr() as *const _vclosure;
                                    if !closure.is_null() {
                                        let cfun = (*closure).fun as usize;
                                        let fi = if (cfun as u64)
                                            < ash_core::llvm::stub_bridge::STUB_SENTINEL_LIMIT
                                        {
                                            cfun.wrapping_sub(1)
                                        } else {
                                            self.findex_for_code_addr(cfun).unwrap_or(usize::MAX)
                                        };
                                        if func_of(&self.targets, fi).is_some()
                                            || native_of(&self.targets, fi).is_some()
                                        {
                                            let mut call_args = arg_vals[1..].to_vec();
                                            if (*closure).hasValue != 0
                                                && !(*closure).value.is_null()
                                            {
                                                call_args.insert(
                                                    0,
                                                    NanBoxedValue::from_ptr(
                                                        (*closure).value as usize,
                                                    ),
                                                );
                                            }
                                            return Ok(StepResult::Call {
                                                findex: fi,
                                                args: call_args,
                                                dst,
                                            });
                                        }
                                    }
                                }
                            }
                            let receiver = NanBoxedValue::from_ptr(dispatch_obj as usize);
                            let needs_boxed_dispatch =
                                if found.is_some() && !self.fn_to_virtual.is_null() {
                                    type FnToVirtual = unsafe extern "C" fn(
                                        *mut hl_type,
                                        *mut hl::vdynamic,
                                    )
                                        -> *mut hl::vvirtual;
                                    let to_virtual: FnToVirtual =
                                        std::mem::transmute(self.fn_to_virtual);
                                    let view = if !header.is_null()
                                        && (*header).kind == hl::hl_type_kind_HVIRTUAL
                                    {
                                        obj_ptr as *mut hl::vvirtual
                                    } else {
                                        to_virtual(virt_type, dispatch_obj as *mut hl::vdynamic)
                                    };
                                    if view.is_null() {
                                        true
                                    } else {
                                        let fields = (view as *const u8)
                                            .add(std::mem::size_of::<hl::vvirtual>())
                                            as *const *mut c_void;
                                        (*fields.add(field)).is_null()
                                    }
                                } else {
                                    false
                                };
                            // A resolved target whose declared return does
                            // not share the destination's representation must
                            // ALSO go boxed: a direct call would store a raw
                            // scalar into a pointer-typed register (or vice
                            // versa). Same erasure hazard as the vfields fast
                            // path above.
                            let needs_boxed_dispatch = needs_boxed_dispatch
                                || found.is_some_and(|fi| {
                                    func_of(&self.targets, fi).is_some_and(|f_idx| {
                                        let ft = &bytecode.functions[f_idx];
                                        bytecode.types[ft.type_.0].fun.as_ref().is_some_and(|f| {
                                            let ret_kind = bytecode.types[f.ret.0].kind;
                                            let dst_kind =
                                                bytecode.types[func.regs[dst as usize].0].kind;
                                            dst_kind != hl::hl_type_kind_HVOID
                                                && Self::is_ptr_kind(ret_kind)
                                                    != Self::is_ptr_kind(dst_kind)
                                        })
                                    })
                                });
                            (found, receiver, hname, needs_boxed_dispatch)
                        }
                    } else {
                        (None, this_val, 0, false)
                    }
                } else {
                    (None, this_val, 0, false)
                }
            };
            if let Some(findex) = findex_opt {
                let dst_type_idx = func.regs[dst as usize].0;
                let dst_kind = bytecode.types[dst_type_idx].kind;
                if needs_boxed_dispatch && dst_kind != hl::hl_type_kind_HVOID {
                    if self.fn_vcall_dyn.is_null() {
                        return Err(anyhow!("hlp_vcall_dyn is unavailable"));
                    }
                    let packed = self.pack_varargs_array(func, &args[1..], &arg_vals[1..])?;
                    type FnVCallDyn = unsafe extern "C" fn(
                        *mut hl::vdynamic,
                        i32,
                        *mut hl::varray,
                    )
                        -> *mut hl::vdynamic;
                    let vcall: FnVCallDyn = unsafe { std::mem::transmute(self.fn_vcall_dyn) };
                    let result = unsafe {
                        vcall(
                            receiver.as_ptr() as *mut hl::vdynamic,
                            hfield,
                            packed.as_ptr() as *mut hl::varray,
                        )
                    };
                    let value = if result.is_null() {
                        Self::coerce_value_for_static_kind(NanBoxedValue::null(), dst_kind)
                    } else if Self::is_unboxable_primitive_kind(dst_kind) {
                        self.dynamic_to_value_for_kind(result, dst_kind)
                    } else if matches!(dst_kind, hl::hl_type_kind_HDYN | hl::hl_type_kind_HNULL)
                        || self.fn_dyn_castp.is_null()
                    {
                        NanBoxedValue::from_ptr(result as usize)
                    } else {
                        type FnDynCastP = unsafe extern "C" fn(
                            *mut c_void,
                            *mut c_void,
                            *mut c_void,
                        )
                            -> *mut c_void;
                        let cast: FnDynCastP = unsafe { std::mem::transmute(self.fn_dyn_castp) };
                        let mut slot = result;
                        let target = self.c_type_factory.get(dst_type_idx) as *mut c_void;
                        let casted = unsafe {
                            cast(
                                &mut slot as *mut *mut hl::vdynamic as *mut c_void,
                                (*result).t as *mut c_void,
                                target,
                            )
                        };
                        if casted.is_null() {
                            NanBoxedValue::null()
                        } else {
                            NanBoxedValue::from_ptr(casted as usize)
                        }
                    };
                    self.stack.last_mut().unwrap().registers.set(dst, value);
                    return Ok(StepResult::Continue);
                }
                let mut call_args = arg_vals;
                call_args[0] = receiver;
                return Ok(StepResult::Call {
                    findex,
                    args: call_args,
                    dst,
                });
            }

            // Upstream ends OCallMethod-on-virtual with an unconditional
            // `hl_dyn_call_obj(v->value, ...)`: whatever static resolution
            // missed is resolved dynamically by the field's hashed name. A
            // live view that reaches this point with nothing resolved gets
            // that dispatch — falling through to the object proto path below
            // would look for a vtable on the HVIRTUAL header and fail.
            let runtime_is_view = unsafe {
                let hdr = *(this_val.as_ptr() as *const *mut hl_type);
                !hdr.is_null() && (*hdr).kind == hl::hl_type_kind_HVIRTUAL
            };
            if runtime_is_view {
                if self.fn_vcall_dyn.is_null() {
                    return Err(anyhow!("hlp_vcall_dyn is unavailable"));
                }
                let packed = self.pack_varargs_array(func, &args[1..], &arg_vals[1..])?;
                type FnVCallDyn = unsafe extern "C" fn(
                    *mut hl::vdynamic,
                    i32,
                    *mut hl::varray,
                ) -> *mut hl::vdynamic;
                let vcall: FnVCallDyn = unsafe { std::mem::transmute(self.fn_vcall_dyn) };
                // Through the trap boundary: the dispatched method can throw
                // (a failed dyn cast in its marshalling included), and a
                // longjmp with no HL trap installed aborts the process.
                let stack_depth = self.stack.len();
                let mut result: *mut hl::vdynamic = std::ptr::null_mut();
                let jumped =
                    run_with_hl_trap(self.fn_setup_trap_jit, self.fn_remove_trap_jit, || {
                        result = unsafe {
                            vcall(
                                this_val.as_ptr() as *mut hl::vdynamic,
                                hfield,
                                packed.as_ptr() as *mut hl::varray,
                            )
                        };
                    });
                if jumped != 0 {
                    return Err(self.longjmp_error(
                        Some(bytecode),
                        stack_depth,
                        format!("exception in virtual dispatch (field={field})"),
                    ));
                }
                let dst_type_idx = func.regs[dst as usize].0;
                let dst_kind = bytecode.types[dst_type_idx].kind;
                let value = if dst_kind == hl::hl_type_kind_HVOID || result.is_null() {
                    Self::coerce_value_for_static_kind(NanBoxedValue::null(), dst_kind)
                } else if Self::is_unboxable_primitive_kind(dst_kind) {
                    self.dynamic_to_value_for_kind(result, dst_kind)
                } else {
                    NanBoxedValue::from_ptr(result as usize)
                };
                self.stack.last_mut().unwrap().registers.set(dst, value);
                return Ok(StepResult::Continue);
            }
        }

        // Try to resolve via vobj_proto (set up by hlp_get_obj_proto)
        let obj_ptr = this_val.as_ptr() as *const u8;
        // The receiver's type header, captured for the call-site profile; a
        // null type resolves through the bytecode fallback and records nothing.
        let recv_type_ptr: u64 = unsafe { *(obj_ptr as *const *mut hl_type) as u64 };
        let findex = unsafe {
            let type_ptr = *(obj_ptr as *const *mut hl_type);
            if !type_ptr.is_null() {
                let vobj_proto = (*type_ptr).vobj_proto;
                if !vobj_proto.is_null() && vobj_proto as usize > 1 {
                    let method_ptr = *vobj_proto.add(field);
                    if (method_ptr as u64) < ash_core::llvm::stub_bridge::STUB_SENTINEL_LIMIT {
                        // Interpreter stub: the slot encodes findex+1.
                        (method_ptr as usize).wrapping_sub(1)
                    } else {
                        // A real code pointer — `patch_vtable_slots` wrote the
                        // compiled address into this row on promotion, so the
                        // findex has to be re-derived. It MUST come from the
                        // RUNTIME type's proto chain: this branch only ever
                        // runs after a promotion, and resolving from the
                        // register's declared type silently dispatched the
                        // base class's code for every overridden method — the
                        // whole hybrid-DeltaBlue corruption (Null access,
                        // "Projection 4 failed", checksum 10940085) was this
                        // line, triggered by whichever rows happened to be
                        // patched. The static resolver stays as last resort
                        // for a malformed chain.
                        Self::find_runtime_proto_findex(type_ptr, field)
                            .or_else(|| {
                                self.resolve_method_findex_from_bytecode(
                                    bytecode, func, &args[0], field,
                                )
                            })
                            .ok_or_else(|| {
                                anyhow!("Cannot resolve method field={} on type", field)
                            })?
                    }
                } else {
                    // vtable not materialized; the runtime type header is
                    // still the dispatch truth for overridden methods.
                    Self::find_runtime_proto_findex(type_ptr, field)
                        .or_else(|| {
                            self.resolve_method_findex_from_bytecode(
                                bytecode, func, &args[0], field,
                            )
                        })
                        .ok_or_else(|| anyhow!("Cannot resolve method field={} on type", field))?
                }
            } else {
                self.resolve_method_findex_from_bytecode(bytecode, func, &args[0], field)
                    .ok_or_else(|| {
                        anyhow!("Cannot resolve method field={} (null type header)", field)
                    })?
            }
        };

        // What this site dispatched on, for the LLVM tier's guarded
        // devirtualisation: the receiver's type header is the guard anchor
        // (vtable SLOTS get patched on promotion; type pointers never move).
        if self.tiered_runtime.is_some() && recv_type_ptr != 0 {
            let pc = self.stack.last().map(|fr| fr.pc).unwrap_or(0);
            ash_core::callsite_profile::record_method(
                bytecode.functions[func_idx].findex as u32,
                pc as u32,
                recv_type_ptr,
                findex as u32,
            );
        }

        Ok(StepResult::Call {
            findex,
            args: arg_vals,
            dst,
        })
    }

    // =====================================================================
    // AIR v2 SSA dispatch
    //
    // See `crate::ssa` for the design. In short: the frame is
    // `values.len() + cells.len()` slots, a `ValueId` indexes it directly, and
    // the shared `op_*` methods above run the per-instruction semantics so this
    // dispatcher never holds a second copy of them.
    // =====================================================================
}
