//! Calling native code, and marshalling values across that boundary.
//!
//! Registers hold NaN-boxed values; native code expects the platform ABI's
//! raw scalars and pointers. Everything that converts between the two lives
//! here, alongside the dispatchers that hand control to a native function.
//! A child module of `interpreter` so it reaches `HLInterpreter`'s private
//! fields without widening them.

use anyhow::{anyhow, Result};
use std::ffi::c_void;

use ash_core::bytecode::DecodedBytecode;
use ash_core::hl_bindings as hl;
use ash_core::hl_bindings::{hl_runtime_obj, hl_type, hl_type_kind_HSTRUCT};
use ash_core::native_lib::NativeFunctionResolver;
use ash_core::opcodes::Reg;
use ash_core::types::{HLFunction, ValueTypeKind};

use crate::tiering::env_flag;
use crate::values::NanBoxedValue;

use super::{
    func_of, kind_u32, native_of, FnGetObjRt, run_with_hl_trap, HLInterpreter, HlpName,
};

impl HLInterpreter {
    /// Call a native function via FFI.
    /// Time each native and report the slow ones, when `ASH_SLOW_NATIVE_MS`
    /// asks.
    ///
    /// A native runs with no safepoint poll in it, so one that takes long
    /// enough holds up every world stop for its whole duration. The collector
    /// can say a thread reached a safepoint late but not what it was doing
    /// before it got there, and a sampler catches only what it happens to
    /// land on. This names the call and its cost directly.
    pub(super) fn call_native(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        native_idx: usize,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        static LIMIT: std::sync::OnceLock<Option<u128>> = std::sync::OnceLock::new();
        let limit = *LIMIT.get_or_init(|| {
            std::env::var("ASH_SLOW_NATIVE_MS")
                .ok()
                .and_then(|v| v.parse::<u128>().ok())
        });
        let Some(limit) = limit else {
            return self.call_native_inner(bytecode, native_resolver, native_idx, args);
        };
        let started = std::time::Instant::now();
        let out = self.call_native_inner(bytecode, native_resolver, native_idx, args);
        let took = started.elapsed();
        if took.as_millis() >= limit {
            let native = &bytecode.natives[native_idx];
            eprintln!(
                "[slow-native] hlp_{} lib={} took {:.1}ms on {}",
                native.name,
                native.lib,
                took.as_secs_f64() * 1e3,
                std::thread::current().name().unwrap_or("main"),
            );
        }
        out
    }

    fn call_native_inner(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        native_idx: usize,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        let native = &bytecode.natives[native_idx];

        // Trace every native call when ASH_TRACE_NATIVE is set
        if env_flag!("ASH_TRACE_NATIVE") {
            eprintln!(
                "[trace] native hlp_{} lib={} args={}",
                native.name,
                native.lib,
                args.len()
            );
        }

        let debug_native = env_flag!("ASH_DBG_NATIVE");
        let func_name = HlpName(&native.name);
        if debug_native
            && (native.name.contains("compare")
                || native.name.contains("eq")
                || native.name.contains("trim")
                || native.name.contains("date"))
        {
            eprintln!("[NATIVE] {} args={} vals={:?}", func_name, args.len(), args);
        }
        let debug_dyn = env_flag!("ASH_DBG_DYN");
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
            "call_stack_raw" => return self.stack_raw_native(bytecode, args, false),
            "exception_stack_raw" => return self.stack_raw_native(bytecode, args, true),
            "resolve_symbol" => {
                let symbol = args.first().copied().unwrap_or_else(NanBoxedValue::null);
                return Ok(if symbol.is_null() || symbol.is_void() {
                    NanBoxedValue::null()
                } else {
                    NanBoxedValue::from_bytes_ptr(symbol.as_ptr())
                });
            }
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
            // Reflect/hl.Api field operations go to the ash_std natives like
            // every other caller. They used to be intercepted into
            // interpreter-PRIVATE shadow HashMaps ("HVIRTUAL fallback
            // storage") that the real object never learned about — state in
            // one world again, and the reason a Reflect.setField was lost
            // the moment any tier compiled a Reflect wrapper: the compiled
            // wrapper calls hlp_obj_get_field directly and reads the actual
            // object, while the interpreted setField had only fed the shadow.
            // (Traced end-to-end on test_feature_typedef_anon at
            // --jit-threshold 1: [VSET] stored shadow maps 1/1, the read
            // never re-entered the interpreter, hlp_obj_get_field returned
            // the stale field.) The natives handle virtuals correctly since
            // the Phase-11/12 fixes; the crutch now only creates divergence.
            // No thread/event/lock interceptions needed — the stdlib's
            // non-blocking lock_wait handles single-threaded mode correctly.
            _ => {}
        }

        // Resolve the native function pointer: per-native cache first, then
        // the process-global symbol table (falls back to lazy dlsym once).
        let mut func_ptr = self
            .native_fn_cache
            .get(native_idx)
            .copied()
            .unwrap_or(std::ptr::null_mut());
        if func_ptr.is_null() {
            func_ptr =
                native_resolver.resolve_function(&native.lib, &format!("hlp_{}", native.name))?;
            if let Some(slot) = self.native_fn_cache.get_mut(native_idx) {
                *slot = func_ptr;
            }
        }

        // Get the function type signature for type-aware marshaling
        let type_fun = bytecode.types[native.type_.0]
            .fun
            .as_ref()
            .ok_or_else(|| anyhow!("Native {} has no function type", func_name))?;

        // Get return type kind for wrapping the result
        let ret_kind = bytecode.types[type_fun.ret.0].kind;

        // Get argument type kinds for extraction
        let arg_kinds: Vec<hl::hl_type_kind> = type_fun
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
        let is_float_kind =
            |k: hl::hl_type_kind| k == hl::hl_type_kind_HF32 || k == hl::hl_type_kind_HF64;
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
        // Same frame-stack invariant as `call_compiled_function`: a native that
        // re-enters the interpreter (closure runner, dynamic dispatch) and then
        // throws longjmps straight back here, leaving the frames it pushed
        // behind.
        let stack_depth = self.stack.len();

        if ret_is_float || float_mask != 0 {
            let mut raw = None;
            let mut recovered_signal = false;
            let jumped = run_with_hl_trap(fn_setup_trap, fn_remove_trap, || {
                // Arm recovery for float-dispatch native calls too.
                let recovered = unsafe { crate::native_recovery::arm_native_recovery() };
                if recovered != 0 {
                    crate::native_recovery::disarm_native_recovery();
                    recovered_signal = true;
                    return;
                }
                raw = Some(self.dispatch_float_native(
                    func_ptr,
                    args,
                    &arg_kinds,
                    float_mask,
                    ret_is_float,
                    ret_kind == hl::hl_type_kind_HF32,
                ));
                crate::native_recovery::disarm_native_recovery();
            });
            if jumped != 0 {
                crate::native_recovery::disarm_native_recovery();
                return Err(self.longjmp_error(
                    Some(bytecode),
                    stack_depth,
                    format!("Native longjmp without exception value: {func_name}"),
                ));
            }
            if recovered_signal {
                let sig = crate::native_recovery::last_recovery_signal();
                let addr = crate::native_recovery::last_recovery_fault_addr();
                eprintln!(
                    "[ash] Recovered from signal {} (fault_addr={:#x}) in native float call: {}",
                    sig, addr, func_name
                );
                return Ok(self.wrap_native_result(0i64, ret_kind));
            }
            let raw =
                raw.ok_or_else(|| anyhow!("Native trap boundary did not run: {func_name}"))??;
            return Ok(self.wrap_native_result(raw, ret_kind));
        }

        // Type-aware argument extraction.
        // For HNULL parameters: if the value is a primitive (I32/F64/Bool),
        // box it into a vdynamic via hlp_make_dyn so the native gets a pointer.
        let extract_arg = |idx: usize| -> i64 {
            let kind = if idx < arg_kinds.len() {
                arg_kinds[idx]
            } else {
                0 // HVOID fallback
            };

            // HNULL(T) parameters expect a vdynamic* pointer, not raw values
            if env_flag!("ASH_DBG_ALLOC") && kind == hl::hl_type_kind_HNULL {
                eprintln!(
                    "[extract_arg] idx={} kind=HNULL val={:?} is_i32={} is_ptr={}",
                    idx,
                    args[idx],
                    args[idx].is_i32(),
                    args[idx].is_ptr()
                );
            }
            if kind == hl::hl_type_kind_HNULL && !self.fn_make_dyn.is_null() {
                let val = args[idx];
                if val.is_null() || val.is_void() {
                    return 0; // null pointer
                }
                if val.is_i32()
                    || val.is_f64()
                    || val.is_bool()
                    || (val.is_ptr() && val.as_ptr() < 0x10000)
                {
                    // Box the primitive into a vdynamic
                    // Determine the inner type from the type signature
                    let inner_type_idx = if idx < type_fun.args.len() {
                        let arg_type = &bytecode.types[type_fun.args[idx].0];
                        arg_type.tparam.as_ref().map(|t| t.0).unwrap_or(0)
                    } else {
                        0
                    };
                    let inner_c_type = self.c_type_factory.get(inner_type_idx) as *mut c_void;
                    let mut data: i64 = if val.is_i32() {
                        val.as_i32() as i64
                    } else if val.is_f64() {
                        val.as_f64().to_bits() as i64
                    } else {
                        val.as_bool() as i64
                    };
                    let make_dyn: unsafe extern "C" fn(*mut c_void, *mut c_void) -> *mut c_void =
                        unsafe { std::mem::transmute(self.fn_make_dyn) };
                    let boxed =
                        unsafe { make_dyn(&mut data as *mut i64 as *mut c_void, inner_c_type) };
                    return boxed as i64;
                }
            }

            self.value_to_i64(args[idx], kind)
        };

        if args.len() > 12 {
            return Err(anyhow!(
                "Native call with {} args not yet supported",
                args.len()
            ));
        }

        // Arm the native call recovery point so SIGSEGV/SIGBUS from native code
        // (e.g., macOS GL driver bugs) is caught and turned into a recoverable error.
        let mut raw_result = None;
        let mut recovered_signal = false;
        let jumped = run_with_hl_trap(fn_setup_trap, fn_remove_trap, || {
            let recovered = unsafe { crate::native_recovery::arm_native_recovery() };
            if recovered != 0 {
                crate::native_recovery::disarm_native_recovery();
                recovered_signal = true;
                return;
            }

            // Dispatch based on argument count, using type-aware extraction and wrapping.
            raw_result = Some(unsafe {
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
                    9 => {
                        let f: unsafe extern "C" fn(
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                        ) -> i64 = std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                            extract_arg(4),
                            extract_arg(5),
                            extract_arg(6),
                            extract_arg(7),
                            extract_arg(8),
                        )
                    }
                    10 => {
                        let f: unsafe extern "C" fn(
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                        ) -> i64 = std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                            extract_arg(4),
                            extract_arg(5),
                            extract_arg(6),
                            extract_arg(7),
                            extract_arg(8),
                            extract_arg(9),
                        )
                    }
                    11 => {
                        let f: unsafe extern "C" fn(
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                        ) -> i64 = std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                            extract_arg(4),
                            extract_arg(5),
                            extract_arg(6),
                            extract_arg(7),
                            extract_arg(8),
                            extract_arg(9),
                            extract_arg(10),
                        )
                    }
                    12 => {
                        let f: unsafe extern "C" fn(
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                        ) -> i64 = std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                            extract_arg(4),
                            extract_arg(5),
                            extract_arg(6),
                            extract_arg(7),
                            extract_arg(8),
                            extract_arg(9),
                            extract_arg(10),
                            extract_arg(11),
                        )
                    }
                    _ => 0i64, // arg count is pre-validated above
                }
            });
            crate::native_recovery::disarm_native_recovery();
        });
        if jumped != 0 {
            crate::native_recovery::disarm_native_recovery();
            return Err(self.longjmp_error(
                Some(bytecode),
                stack_depth,
                format!("Native longjmp without exception value: {func_name}"),
            ));
        }
        if recovered_signal {
            let sig = crate::native_recovery::last_recovery_signal();
            let addr = crate::native_recovery::last_recovery_fault_addr();
            let sig_name = match sig {
                11 => "SIGSEGV",
                10 => "SIGBUS",
                _ => "SIGNAL",
            };
            eprintln!(
                "[ash] Recovered from {} (fault_addr={:#x}) in native call: {}",
                sig_name, addr, func_name
            );
            return Ok(self.wrap_native_result(0i64, ret_kind));
        }
        let raw_result =
            raw_result.ok_or_else(|| anyhow!("Native trap boundary did not run: {func_name}"))?;

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
    pub(super) fn closure_findex_and_value(
        &mut self,
        val: NanBoxedValue,
    ) -> (usize, Option<NanBoxedValue>) {
        if val.is_func() {
            (val.as_func_index(), None)
        } else if val.is_ptr() {
            let cl_ptr = val.as_ptr() as *const hl::_vclosure;
            unsafe {
                // `fun` is a `findex + 1` stub only when the interpreter
                // built the closure; compiled code stores the real entry it
                // loaded from `functions_ptrs`.
                let stub = (*cl_ptr).fun as usize;
                let findex = if (stub as u64) < ash_core::llvm::stub_bridge::STUB_SENTINEL_LIMIT {
                    stub.wrapping_sub(1)
                } else {
                    self.findex_for_code_addr(stub).unwrap_or(usize::MAX)
                };
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
    pub(super) fn call_closure_val(
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

    pub(super) fn dynamic_to_value_for_kind(
        &self,
        d: *mut hl::vdynamic,
        dst_kind: hl::hl_type_kind,
    ) -> NanBoxedValue {
        if d.is_null() {
            return NanBoxedValue::null();
        }
        // Not every word arriving here is a box — see is_derefable_dynamic.
        if !Self::is_derefable_dynamic(d) {
            return NanBoxedValue::from_ptr(d as usize);
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

    /// The callee's argument TYPE INDICES (not just kinds) and return type
    /// index. HREF marshalling needs the full type — the ref's tparam decides
    /// the cell the value is coerced into, and a kind alone has lost it.
    pub(super) fn closure_arg_type_idxs_and_ret(
        &self,
        bytecode: &DecodedBytecode,
        findex: usize,
    ) -> Option<(Vec<usize>, usize)> {
        let t_idx = match func_of(&self.targets, findex) {
            Some(fidx) => bytecode.functions[fidx].type_.0,
            None => bytecode.natives[native_of(&self.targets, findex)?].type_.0,
        };
        let tf = bytecode.types[t_idx].fun.as_ref()?;
        Some((tf.args.iter().map(|a| a.0).collect(), tf.ret.0))
    }

    pub(super) fn try_handle_call_method_native(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        args: &[NanBoxedValue],
    ) -> Result<Option<NanBoxedValue>> {
        let dbg = env_flag!("ASH_DBG_DYN");
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
        let (arg_type_idxs, ret_type_idx) = self
            .closure_arg_type_idxs_and_ret(bytecode, findex)
            .unwrap_or((Vec::new(), 0));
        let arg_kinds: Vec<hl::hl_type_kind> = arg_type_idxs
            .iter()
            .map(|&ti| bytecode.types[ti].kind)
            .collect();
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
        // Storage backing HREF arguments for the duration of the synchronous
        // call below. Box keeps each cell stable if this Vec grows.
        let mut ref_cells: Vec<Box<u64>> = Vec::new();
        for i in 0..argc {
            let dyn_arg = unsafe { *data_ptr.add(i) };
            let expected_type_idx = arg_type_idxs.get(i + arg_shift).copied();
            let expected_kind = arg_kinds
                .get(i + arg_shift)
                .copied()
                .unwrap_or(hl::hl_type_kind_HDYN);
            // A byref parameter: upstream hl_dyn_castp coerces the boxed
            // value into a fresh GC cell and passes the CELL — passing the
            // box's payload gave the callee an "address" of 0x2 for
            // Type.createInstance(ClassWithCtorDefaultValues, [2, "bar"]).
            // Null stays null: that is the callee's use-the-default signal.
            let v = if expected_kind == hl::hl_type_kind_HNULL && !dyn_arg.is_null() {
                // A provided nullable argument remains boxed. Extracting its
                // primitive payload turns `2 : Null<Int>` into pointer 0x2;
                // the callee expects the vdynamic* so its nullable prologue
                // can distinguish it from an omitted/null argument.
                NanBoxedValue::from_ptr(dyn_arg as usize)
            } else if expected_kind == hl::hl_type_kind_HREF && !dyn_arg.is_null() {
                if unsafe {
                    !(*dyn_arg).t.is_null() && (*(*dyn_arg).t).kind == hl::hl_type_kind_HREF
                } {
                    // `hlp_make_dyn` boxes HREF by preserving its cell
                    // pointer in `v.ptr`. The wrapper itself is non-null even
                    // when that pointer is null (an omitted optional
                    // argument), so testing only `dyn_arg` manufactured a
                    // non-null cell containing zero and suppressed defaults.
                    let cell = unsafe { (*dyn_arg).v.ptr } as usize;
                    if cell == 0 {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(cell)
                    }
                } else {
                    let href_type = &bytecode.types[arg_type_idxs[i + arg_shift]];
                    let inner_kind = href_type
                        .tparam
                        .as_ref()
                        .and_then(|t| bytecode.types.get(t.0))
                        .map(|t| t.kind)
                        .unwrap_or(hl::hl_type_kind_HDYN);
                    let inner = self.dynamic_to_value_for_kind(dyn_arg, inner_kind);
                    let mut cell = Box::new(0u64);
                    Self::write_value_to_ptr(
                        (&mut *cell as *mut u64).cast::<u8>(),
                        inner,
                        inner_kind,
                    );
                    let cell_ptr = (&mut *cell as *mut u64) as usize;
                    ref_cells.push(cell);
                    NanBoxedValue::from_ptr(cell_ptr)
                }
            } else if expected_kind == hl::hl_type_kind_HOBJ && !self.fn_dyn_castp.is_null() {
                // A dynamic HOBJ still needs an exact-type cast. Kind-only
                // conversion passes ArrayDyn to a method expecting
                // ArrayBytes<Int>, while HashLink's hl_dyn_call routes that
                // through hl_dyn_castp so ArrayDyn.__cast can materialize the
                // specialized representation.
                if let Some(expected_type_idx) = expected_type_idx {
                    let target_type = self.c_type_factory.get(expected_type_idx);
                    let source_type = unsafe { (*dyn_arg).t };
                    if source_type == target_type || target_type.is_null() || source_type.is_null()
                    {
                        NanBoxedValue::from_ptr(dyn_arg as usize)
                    } else if unsafe { (*source_type).kind } != hl::hl_type_kind_HOBJ {
                        // Default-argument method shims can present their
                        // receiver through HREF. Preserve the established
                        // wrapper unboxing for those non-object sources.
                        self.dynamic_to_value_for_kind(dyn_arg, expected_kind)
                    } else {
                        type FnCastp = unsafe extern "C" fn(
                            *mut c_void,
                            *mut c_void,
                            *mut c_void,
                        ) -> *mut c_void;
                        let castp: FnCastp = unsafe { std::mem::transmute(self.fn_dyn_castp) };
                        let mut data = dyn_arg as *mut c_void;
                        let casted = unsafe {
                            castp(
                                &mut data as *mut _ as *mut c_void,
                                source_type.cast(),
                                target_type.cast(),
                            )
                        };
                        if casted.is_null() {
                            NanBoxedValue::null()
                        } else {
                            NanBoxedValue::from_ptr(casted as usize)
                        }
                    }
                } else {
                    self.dynamic_to_value_for_kind(dyn_arg, expected_kind)
                }
            } else {
                self.dynamic_to_value_for_kind(dyn_arg, expected_kind)
            };
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

        // Reflect.callMethod sizes its NativeArray to the arguments it needs
        // to materialize; trailing optional parameters are omitted.  The
        // interpreter register file starts as Void, whereas HashLink presents
        // an omitted optional (HREF) parameter as null so the callee's default
        // prologue runs.  Pad to the declared signature before dispatch.
        let explicit_params = arg_kinds.len().saturating_sub(arg_shift);
        call_args.resize(explicit_params, NanBoxedValue::null());

        let ret = self.call_closure_val(bytecode, native_resolver, closure_val, call_args)?;
        if dbg {
            eprintln!("[CALL_METHOD] raw_ret={:?}", ret);
        }
        let out = if ret.is_void() {
            NanBoxedValue::null()
        } else {
            let ret_t = self.c_type_factory.get(ret_type_idx);
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
    pub(super) fn sort_bytes_i32(
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
        let resolver_raw = native_resolver as *const ash_core::native_lib::NativeFunctionResolver;
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
    pub(super) fn sort_bytes_i64(
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
        let resolver_raw = native_resolver as *const ash_core::native_lib::NativeFunctionResolver;
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
    pub(super) fn sort_bytes_f64(
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
        let resolver_raw = native_resolver as *const ash_core::native_lib::NativeFunctionResolver;
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
    pub(super) fn dispatch_float_native(
        &self,
        func_ptr: *mut std::ffi::c_void,
        args: &[NanBoxedValue],
        arg_kinds: &[hl::hl_type_kind],
        float_mask: u32,
        ret_is_float: bool,
        ret_is_f32: bool,
    ) -> Result<i64> {
        let gf = |i: usize| -> f64 { args[i].as_f64() };
        let gf32 = |i: usize| -> f32 { args[i].as_f64() as f32 };
        let gi = |i: usize| -> i64 { self.value_to_i64(args[i], arg_kinds[i]) };

        let raw: i64 = unsafe {
            match (args.len(), ret_is_float, float_mask) {
                // --- 0 args ---
                (0, true, 0b0) if ret_is_f32 => {
                    let f: unsafe extern "C" fn() -> f32 = std::mem::transmute(func_ptr);
                    (f() as f64).to_bits() as i64
                }
                (0, true, 0b0) => {
                    // () -> f64
                    let f: unsafe extern "C" fn() -> f64 = std::mem::transmute(func_ptr);
                    f().to_bits() as i64
                }
                // --- 1 arg ---
                (1, true, 0b0) if ret_is_f32 => {
                    let f: unsafe extern "C" fn(i64) -> f32 = std::mem::transmute(func_ptr);
                    (f(gi(0)) as f64).to_bits() as i64
                }
                (1, true, 0b0) => {
                    // (i64) -> f64  e.g. date_get_time(t:Int)
                    let f: unsafe extern "C" fn(i64) -> f64 = std::mem::transmute(func_ptr);
                    f(gi(0)).to_bits() as i64
                }
                (1, true, 0b1) if ret_is_f32 && arg_kinds[0] == hl::hl_type_kind_HF32 => {
                    let f: unsafe extern "C" fn(f32) -> f32 = std::mem::transmute(func_ptr);
                    (f(gf32(0)) as f64).to_bits() as i64
                }
                (1, true, 0b1) => {
                    // (f64) -> f64  e.g. math_sqrt, math_abs, math_floor, ...
                    let f: unsafe extern "C" fn(f64) -> f64 = std::mem::transmute(func_ptr);
                    f(gf(0)).to_bits() as i64
                }
                (1, false, 0b1) if arg_kinds[0] == hl::hl_type_kind_HF32 => {
                    let f: unsafe extern "C" fn(f32) = std::mem::transmute(func_ptr);
                    f(gf32(0));
                    0
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
                (2, false, 0b10) if arg_kinds[1] == hl::hl_type_kind_HF32 => {
                    let f: unsafe extern "C" fn(i64, f32) = std::mem::transmute(func_ptr);
                    f(gi(0), gf32(1));
                    0
                }
                (2, false, 0b10) => {
                    // (i64, f64) -> i64
                    let f: unsafe extern "C" fn(i64, f64) -> i64 = std::mem::transmute(func_ptr);
                    f(gi(0), gf(1))
                }
                (2, true, 0b10) if ret_is_f32 && arg_kinds[1] == hl::hl_type_kind_HF32 => {
                    let f: unsafe extern "C" fn(i64, f32) -> f32 = std::mem::transmute(func_ptr);
                    (f(gi(0), gf32(1)) as f64).to_bits() as i64
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
                (2, true, 0b00) if ret_is_f32 => {
                    let f: unsafe extern "C" fn(i64, i64) -> f32 = std::mem::transmute(func_ptr);
                    (f(gi(0), gi(1)) as f64).to_bits() as i64
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
                (3, false, 0b011) => {
                    // Two scalar values followed by comparison context.
                    let f: unsafe extern "C" fn(f64, f64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gf(0), gf(1), gi(2))
                }
                (3, false, 0b100) if arg_kinds[2] == hl::hl_type_kind_HF32 => {
                    // (i64, i64, f32) -> void, used by hlsdl's
                    // gl_tex_parameterf(target, parameter, value).
                    let f: unsafe extern "C" fn(i64, i64, f32) = std::mem::transmute(func_ptr);
                    f(gi(0), gi(1), gf32(2));
                    0
                }
                (3, false, 0b100) => {
                    // (i64, i64, f64) -> i64
                    let f: unsafe extern "C" fn(i64, i64, f64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gi(1), gf(2))
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
                // --- 4 args ---
                (4, false, 0b1110)
                    if arg_kinds[1..].iter().all(|&k| k == hl::hl_type_kind_HF32) =>
                {
                    // OpenAL listener3f(parameter, x, y, z).
                    let f: unsafe extern "C" fn(i64, f32, f32, f32) = std::mem::transmute(func_ptr);
                    f(gi(0), gf32(1), gf32(2), gf32(3));
                    0
                }
                (4, false, 0b1110) => {
                    // Compiled AIR functions and native vector helpers with
                    // a receiver followed by three doubles.
                    let f: unsafe extern "C" fn(i64, f64, f64, f64) = std::mem::transmute(func_ptr);
                    f(gi(0), gf(1), gf(2), gf(3));
                    0
                }
                (4, false, 0b1000) if arg_kinds[3] == hl::hl_type_kind_HF32 => {
                    let f: unsafe extern "C" fn(i64, i64, i64, f32) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gi(1), gi(2), gf32(3))
                }
                (4, false, 0b1000) => {
                    // AIR functions such as structural equality carry the
                    // comparison epsilon after three pointer-like operands.
                    let f: unsafe extern "C" fn(i64, i64, i64, f64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gi(1), gi(2), gf(3))
                }
                (4, false, 0b0110) => {
                    let f: unsafe extern "C" fn(i64, f64, f64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gf(1), gf(2), gi(3))
                }
                (4, true, 0b0000) if ret_is_f32 => {
                    let f: unsafe extern "C" fn(i64, i64, i64, i64) -> f32 =
                        std::mem::transmute(func_ptr);
                    (f(gi(0), gi(1), gi(2), gi(3)) as f64).to_bits() as i64
                }
                (4, true, 0b0000) => {
                    let f: unsafe extern "C" fn(i64, i64, i64, i64) -> f64 =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gi(1), gi(2), gi(3)).to_bits() as i64
                }
                (4, false, 0b1111) => {
                    // (f64, f64, f64, f64) -> i64  e.g. gl_clear_color(r, g, b, a)
                    let f: unsafe extern "C" fn(f64, f64, f64, f64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gf(0), gf(1), gf(2), gf(3))
                }
                // --- 5 args ---
                (5, false, 0b11100)
                    if arg_kinds[2..].iter().all(|&k| k == hl::hl_type_kind_HF32) =>
                {
                    // OpenAL source3f/buffer3f(object, parameter, x, y, z).
                    let f: unsafe extern "C" fn(i64, i64, f32, f32, f32) =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gi(1), gf32(2), gf32(3), gf32(4));
                    0
                }
                (5, false, 0b11100) => {
                    let f: unsafe extern "C" fn(i64, i64, f64, f64, f64) =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gi(1), gf(2), gf(3), gf(4));
                    0
                }
                (5, false, 0b00011) => {
                    let f: unsafe extern "C" fn(f64, f64, i64, i64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gf(0), gf(1), gi(2), gi(3), gi(4))
                }
                (5, false, 0b11110) => {
                    let f: unsafe extern "C" fn(i64, f64, f64, f64, f64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gf(1), gf(2), gf(3), gf(4))
                }
                // --- 6 args ---
                (6, false, 0b100000) => {
                    // (i64, i64, i64, i64, i64, f64) -> i64
                    // e.g. socket_select(read, write, other, tmp, size, timeout)
                    let f: unsafe extern "C" fn(i64, i64, i64, i64, i64, f64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gi(1), gi(2), gi(3), gi(4), gf(5))
                }
                // --- 8 args ---
                (8, false, 0b0011_1100) => {
                    // Haxe graphics helpers commonly carry an object and
                    // flags around four scalar coordinates:
                    // (i64, i64, f64, f64, f64, f64, i64, i64) -> word.
                    let f: unsafe extern "C" fn(i64, i64, f64, f64, f64, f64, i64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gi(1), gf(2), gf(3), gf(4), gf(5), gi(6), gi(7))
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
    pub(super) fn value_to_i64(&self, val: NanBoxedValue, type_kind: hl::hl_type_kind) -> i64 {
        use ValueTypeKind::*;
        match ValueTypeKind::try_from(kind_u32(type_kind)).unwrap_or(HNULL) {
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
    pub(super) fn wrap_native_result(&self, raw: i64, ret_kind: hl::hl_type_kind) -> NanBoxedValue {
        use ValueTypeKind::*;
        match ValueTypeKind::try_from(kind_u32(ret_kind)).unwrap_or(HNULL) {
            HVOID => NanBoxedValue::void(),
            HI32 => NanBoxedValue::from_i32(raw as i32),
            // A callee returning bool/u8/u16 only defines the low bits of the
            // return register — SysV x86-64 leaves the rest undefined, and the
            // dispatch reads the full register. Truncate to the ABI width or
            // Linux garbage bits turn a returned `false` into `true`.
            HUI8 => NanBoxedValue::from_i32(raw as u8 as i32),
            HUI16 => NanBoxedValue::from_i32(raw as u16 as i32),
            HI64 => NanBoxedValue::from_i64(raw),
            HF32 | HF64 => NanBoxedValue::from_f64(f64::from_bits(raw as u64)),
            HBOOL => NanBoxedValue::from_bool((raw as u8) != 0),
            HBYTES => {
                if raw == 0 {
                    NanBoxedValue::null()
                } else {
                    NanBoxedValue::from_bytes_ptr(raw as usize)
                }
            }
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
    pub(super) unsafe fn read_obj_field(
        obj_ptr: *mut u8,
        field_idx: usize,
        dst_kind: hl::hl_type_kind,
        obj_c_type: *mut c_void,
        obj_kind: hl::hl_type_kind,
        fn_get_obj_rt: *mut c_void,
    ) -> NanBoxedValue {
        if fn_get_obj_rt.is_null() {
            return NanBoxedValue::null();
        }

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

        // Corruption tripwire: a type pointer must be 8-aligned; a NaN-boxed
        // double here means the object's memory was reclaimed and reused.
        // Print the evidence (cross-reference with ASH_GC_TRACE_FREED) before
        // the misaligned deref aborts without it.
        {
            let bad_align = (type_ptr as usize) & 7 != 0;
            // An aligned-but-garbage header (a reused line of doubles) passes
            // the alignment check; the type's kind field gives it away.
            let bad_kind = !bad_align && {
                let k = *(type_ptr as *const i32);
                !(0..=22).contains(&k)
            };
            if bad_align || bad_kind {
                eprintln!(
                    "[gc-corrupt] FieldGet obj={:#x} header={:#x} field={field_idx}",
                    obj_ptr as usize, type_ptr as usize
                );
            }
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

        // Use dst_kind (register type) for reading — the compiler knows the correct
        // read width. The field's declared type is only used for WRITING to prevent
        // 8-byte NanBox spill into adjacent fields.
        Self::read_value_at(field_addr, dst_kind)
    }

    /// Write a value to an object field at the given field index.
    pub(super) unsafe fn write_obj_field(
        obj_ptr: *mut u8,
        field_idx: usize,
        src_kind: hl::hl_type_kind,
        val: NanBoxedValue,
        obj_c_type: *mut c_void,
        obj_kind: hl::hl_type_kind,
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
        let field_addr = obj_ptr.add(offset as usize);

        Self::write_value_at(field_addr, src_kind, val);
    }

    /// Read a value from a raw memory address based on the HL type kind.
    pub(super) unsafe fn read_value_at(addr: *const u8, kind: hl::hl_type_kind) -> NanBoxedValue {
        use ValueTypeKind::*;
        match ValueTypeKind::try_from(kind_u32(kind)).unwrap_or(HDYN) {
            HVOID => NanBoxedValue::void(),
            HUI8 => NanBoxedValue::from_i32(*addr as i32),
            HUI16 => NanBoxedValue::from_i32(*(addr as *const u16) as i32),
            HI32 => NanBoxedValue::from_i32(*(addr as *const i32)),
            HI64 => NanBoxedValue::from_i64(*(addr as *const i64)),
            HF32 => NanBoxedValue::from_f64(*(addr as *const f32) as f64),
            HF64 => NanBoxedValue::from_f64(*(addr as *const f64)),
            HBOOL => NanBoxedValue::from_bool(*addr != 0),
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

    /// Upstream's `hl_is_ptr`: kinds at or above HBYTES live in a machine
    /// word that holds a pointer; kinds below it are value scalars.
    #[inline(always)]
    pub(super) fn is_ptr_kind(kind: hl::hl_type_kind) -> bool {
        kind >= hl::hl_type_kind_HBYTES
    }

    /// Resolve a real compiled entry address back to the findex it belongs to.
    ///
    /// Compiled code allocates closures from `functions_ptrs[findex]`, so the
    /// `fun` field of a closure that crossed the compiled→interpreter boundary
    /// may hold an entry address where the interpreter expects a `findex + 1`
    /// stub sentinel. `functions_ptrs` is the table that address came from, so
    /// it is also the map back.
    ///
    /// The scan is amortised: a miss indexes the whole table at once, and
    /// promotion only ever adds addresses, so a cached entry stays true.
    pub(super) fn findex_for_code_addr(&mut self, addr: usize) -> Option<usize> {
        if let Some(&fi) = self.code_addr_findex.get(&addr) {
            return Some(fi);
        }
        // Every install registers its entry, and that registry accumulates
        // rather than overwriting, so it answers for superseded tiers too.
        if let Some(fi) = ash_core::profile::findex_at_entry(addr) {
            let fi = fi as usize;
            self.code_addr_findex.insert(addr, fi);
            return Some(fi);
        }
        let module_ctx = self.c_type_factory.module_ctx();
        if module_ctx.is_null() {
            return None;
        }
        // SAFETY: `module_ctx` is the process-lifetime context the type
        // factory owns; `functions_ptrs` is its findex-indexed slot table,
        // sized to hold every findex in `targets`.
        let ptrs = unsafe { (*module_ctx).functions_ptrs };
        if ptrs.is_null() {
            return None;
        }
        for findex in 0..self.targets.len() {
            let slot = unsafe { *ptrs.add(findex) } as usize;
            if slot as u64 >= ash_core::llvm::stub_bridge::STUB_SENTINEL_LIMIT {
                self.code_addr_findex.entry(slot).or_insert(findex);
            }
        }
        self.code_addr_findex.get(&addr).copied()
    }

    /// Resolve a vtable slot to its findex from the object's RUNTIME type:
    /// walk the C proto chain child-first for the entry with this absolute
    /// `pindex`, so an override shadows its ancestor. This is the same truth
    /// `vobj_proto` itself is built from.
    ///
    /// # Safety contract
    /// `type_ptr` must be a live `hl_type` (it came from an object header).
    pub(super) unsafe fn find_runtime_proto_findex(type_ptr: *mut hl_type, pindex: usize) -> Option<usize> {
        let mut t = type_ptr;
        while !t.is_null()
            && ((*t).kind == hl::hl_type_kind_HOBJ || (*t).kind == hl::hl_type_kind_HSTRUCT)
        {
            let obj = (*t).__bindgen_anon_1.obj;
            if obj.is_null() {
                break;
            }
            for i in 0..(*obj).nproto as usize {
                let pr = &*(*obj).proto.add(i);
                if pr.pindex >= 0 && pr.pindex as usize == pindex {
                    return Some(pr.findex as usize);
                }
            }
            t = (*obj).super_;
        }
        None
    }

    /// Resolve a method findex from bytecode type proto (fallback when vobj_proto unavailable).
    pub(super) fn resolve_method_findex_from_bytecode(
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
    pub(super) fn find_proto_findex(
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
    pub(super) unsafe fn write_value_at(addr: *mut u8, kind: hl::hl_type_kind, val: NanBoxedValue) {
        use ValueTypeKind::*;
        match ValueTypeKind::try_from(kind_u32(kind)).unwrap_or(HDYN) {
            HVOID => {}
            HUI8 => *addr = val.as_i32() as u8,
            HUI16 => *(addr as *mut u16) = val.as_i32() as u16,
            HI32 => *(addr as *mut i32) = val.as_i32(),
            HI64 => *(addr as *mut i64) = val.as_i64_lossy(),
            HF32 => *(addr as *mut f32) = val.as_f64() as f32,
            HF64 => *(addr as *mut f64) = val.as_f64(),
            HBOOL => *addr = val.as_bool() as u8,
            _ => {
                // Pointer types — but the NanBoxed value might actually
                // be a primitive (e.g., HDYN register holding an I32).
                if val.is_null() || val.is_void() {
                    *(addr as *mut usize) = 0;
                } else if val.is_i32() {
                    *(addr as *mut i32) = val.as_i32();
                } else if val.is_f64() {
                    *(addr as *mut f64) = val.as_f64();
                } else {
                    *(addr as *mut usize) = val.as_ptr();
                }
            }
        }
    }

    /// Allocate a venum value for the given type and construct index using the GC allocator.
    /// Takes fn_alloc_enum as a parameter to avoid conflicting with the frame mutable borrow.
    pub(super) fn alloc_enum_value(
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
    pub(super) fn read_value_from_ptr(ptr: *const u8, kind: hl::hl_type_kind) -> NanBoxedValue {
        unsafe { Self::read_value_at(ptr, kind) }
    }

    /// Write a NanBoxedValue to a raw memory pointer using the given type kind.
    pub(super) fn write_value_to_ptr(ptr: *mut u8, val: NanBoxedValue, kind: hl::hl_type_kind) {
        unsafe { Self::write_value_at(ptr, kind, val) }
    }
}
