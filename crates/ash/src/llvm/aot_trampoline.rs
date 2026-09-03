//! Dynamic calls on a target that checks every indirect call's signature.
//!
//! `ash_static_call` takes a function pointer, the HL type describing it, and
//! an array of argument slots, and makes the call. On a native target it does
//! that by placing values in the registers the C ABI names and jumping, which
//! is a shape a run-time value can take. WebAssembly has no registers to
//! place anything in, and its `call_indirect` names a signature the validator
//! checks against the callee's, so a call whose shape is only known at run
//! time cannot be assembled at all.
//!
//! What can be assembled is every shape the program actually contains. The
//! compiler sees each function type, so it emits one trampoline per distinct
//! signature -- a function of fixed shape `(fun, args, out) -> ptr` that
//! unpacks the arguments, makes one statically-typed call, and stores the
//! result -- and registers them by a key the runtime can compute from an
//! `hl_type` it holds. `ash_static_call` then looks the shape up instead of
//! constructing it.
//!
//! # The argument convention is copied, not designed
//!
//! Each `args[i]` is read exactly as the aarch64 implementation in
//! `std/src/fun.rs` reads it, including where that is odd: an integer slot
//! holds its value as an `f64` and is converted, while a 64-bit integer slot
//! holds an `i64` and a 32-bit float slot holds an `f32`. Two callers built
//! those slots differently long before this file existed, and matching the
//! native reader exactly means wasm behaves as the other targets do --
//! including when they are wrong, which is the property worth having until
//! someone fixes both.

use std::collections::BTreeMap;

use anyhow::{anyhow, Result};
use inkwell::module::Linkage;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, FunctionValue};
use inkwell::AddressSpace;

use crate::hl::{
    hl_type_kind, hl_type_kind_HBOOL, hl_type_kind_HF32, hl_type_kind_HF64, hl_type_kind_HI32,
    hl_type_kind_HI64, hl_type_kind_HUI16, hl_type_kind_HUI8, hl_type_kind_HVOID,
};
use crate::llvm::module::JITModule;

/// Where a `vdynamic`'s value union starts.
///
/// The header is a type pointer, plus four bytes of padding on a 32-bit
/// target so the union is aligned for a double. Eight bytes either way, which
/// is why this is a constant rather than a question for `TargetAbi`.
const VDYNAMIC_VALUE_OFFSET: u64 = 8;

/// How a kind crosses the boundary.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Slot {
    /// `args[i]` points at an `f64` holding the value; the callee wants i32.
    IntViaDouble,
    /// `args[i]` points at an `i64`.
    Int64,
    /// `args[i]` points at an `f32`.
    Float32,
    /// `args[i]` points at an `f64`.
    Float64,
    /// `args[i]` IS the value.
    Pointer,
}

fn slot_of(kind: hl_type_kind) -> Slot {
    match kind {
        hl_type_kind_HF32 => Slot::Float32,
        hl_type_kind_HF64 => Slot::Float64,
        hl_type_kind_HI64 => Slot::Int64,
        hl_type_kind_HBOOL | hl_type_kind_HUI8 | hl_type_kind_HUI16 | hl_type_kind_HI32 => {
            Slot::IntViaDouble
        }
        _ => Slot::Pointer,
    }
}

/// The key both sides compute for a signature.
///
/// FNV-1a over the return kind, the argument count and each argument kind, in
/// that order. `std/src/fun.rs` computes the same value from the `hl_type` it
/// is handed, and the two must not drift: a mismatch is a call that finds no
/// trampoline, which reports itself rather than misbehaving.
pub fn signature_key(ret_kind: u32, arg_kinds: &[u32]) -> u64 {
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    let mut mix = |value: u32| {
        for byte in value.to_le_bytes() {
            hash ^= u64::from(byte);
            hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
        }
    };
    mix(ret_kind);
    mix(arg_kinds.len() as u32);
    for kind in arg_kinds {
        mix(*kind);
    }
    hash
}

impl<'ctx> JITModule<'ctx> {
    /// Emit one trampoline per distinct signature and register them all.
    ///
    /// Returns the number emitted. Zero is a legitimate answer for a program
    /// with no function types at all.
    pub fn emit_call_trampolines(&mut self) -> Result<usize> {
        // Signature -> (ret kind, arg kinds). Ordered so a build is
        // reproducible: two runs over the same program emit the same module.
        let mut wanted: BTreeMap<u64, (u32, Vec<u32>)> = BTreeMap::new();
        let mut record = |wanted: &mut BTreeMap<u64, (u32, Vec<u32>)>,
                          fun: &crate::types::HLTypeFun,
                          types: &[crate::types::HLType]| {
            let ret = types[fun.ret.0].kind as u32;
            let args: Vec<u32> = fun.args.iter().map(|a| types[a.0].kind as u32).collect();
            wanted.insert(signature_key(ret, &args), (ret, args));
        };
        for ty in &self.types_ {
            let Some(fun) = ty.fun.as_ref() else { continue };
            record(&mut wanted, fun, &self.types_);
            // A method's closure form -- the same function with `this`
            // removed -- is a signature the program calls but does not
            // necessarily name as a type of its own, so it has to be walked
            // rather than waited for. Missing one is a call that finds no
            // trampoline at run time, which is exactly how this was found.
            if let Some(closure) = fun.closure.as_ref() {
                record(&mut wanted, closure, &self.types_);
            }
            // And the closure form of any method: `hlp_get_closure_type`
            // builds one at run time by dropping `this`, for any method a
            // program takes a reference to, and the result need not appear in
            // the type table at all. `(HOBJ) -> HBOOL` reached the runtime
            // this way and found nothing, which is how this was found.
            if !fun.args.is_empty() {
                let ret = self.types_[fun.ret.0].kind as u32;
                let args: Vec<u32> = fun.args[1..]
                    .iter()
                    .map(|a| self.types_[a.0].kind as u32)
                    .collect();
                wanted.insert(signature_key(ret, &args), (ret, args));
            }
        }
        if wanted.is_empty() {
            return Ok(0);
        }

        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i64_type = self.context.i64_type();
        let mut keys: Vec<inkwell::values::IntValue<'ctx>> = Vec::new();
        let mut fns: Vec<inkwell::values::PointerValue<'ctx>> = Vec::new();

        for (key, (ret, args)) in &wanted {
            let f = self.emit_one_trampoline(*key, *ret, args)?;
            keys.push(i64_type.const_int(*key, false));
            fns.push(f.as_global_value().as_pointer_value());
        }

        let key_array = i64_type.const_array(&keys);
        let key_global = self
            .module
            .add_global(key_array.get_type(), None, "ash_tramp_keys");
        key_global.set_initializer(&key_array);
        key_global.set_linkage(Linkage::Internal);

        let fn_array = ptr_type.const_array(&fns);
        let fn_global = self
            .module
            .add_global(fn_array.get_type(), None, "ash_tramp_fns");
        fn_global.set_initializer(&fn_array);
        fn_global.set_linkage(Linkage::Internal);

        self.trampoline_registry = Some((
            key_global.as_pointer_value(),
            fn_global.as_pointer_value(),
            wanted.len(),
        ));
        Ok(wanted.len())
    }

    /// One trampoline: `ptr (ptr fun, ptr args, ptr out)`.
    fn emit_one_trampoline(
        &mut self,
        key: u64,
        ret: u32,
        args: &[u32],
    ) -> Result<FunctionValue<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i32_type = self.context.i32_type();
        let i64_type = self.context.i64_type();
        let f32_type = self.context.f32_type();
        let f64_type = self.context.f64_type();

        let name = format!("ash_tramp_{key:016x}");
        if let Some(existing) = self.module.get_function(&name) {
            return Ok(existing);
        }
        let tramp_ty =
            ptr_type.fn_type(&[ptr_type.into(), ptr_type.into(), ptr_type.into()], false);
        let tramp = self
            .module
            .add_function(&name, tramp_ty, Some(Linkage::Internal));
        let entry = self.context.append_basic_block(tramp, "entry");
        let saved = self.builder.get_insert_block();
        self.builder.position_at_end(entry);

        let fun = tramp.get_nth_param(0).unwrap().into_pointer_value();
        let arg_slots = tramp.get_nth_param(1).unwrap().into_pointer_value();
        let out = tramp.get_nth_param(2).unwrap().into_pointer_value();

        // Unpack each argument from its slot.
        let mut call_args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::new();
        let mut param_types: Vec<BasicMetadataTypeEnum<'ctx>> = Vec::new();
        for (i, kind) in args.iter().enumerate() {
            let slot_ptr = unsafe {
                self.builder.build_gep(
                    ptr_type,
                    arg_slots,
                    &[i32_type.const_int(i as u64, false)],
                    &format!("slot{i}"),
                )?
            };
            let slot = self
                .builder
                .build_load(ptr_type, slot_ptr, &format!("arg{i}"))?
                .into_pointer_value();
            match slot_of(*kind as hl_type_kind) {
                Slot::Pointer => {
                    call_args.push(slot.into());
                    param_types.push(ptr_type.into());
                }
                Slot::Float32 => {
                    let v = self.builder.build_load(f32_type, slot, "f32")?;
                    call_args.push(v.into());
                    param_types.push(f32_type.into());
                }
                Slot::Float64 => {
                    let v = self.builder.build_load(f64_type, slot, "f64")?;
                    call_args.push(v.into());
                    param_types.push(f64_type.into());
                }
                Slot::Int64 => {
                    let v = self.builder.build_load(i64_type, slot, "i64")?;
                    call_args.push(v.into());
                    param_types.push(i64_type.into());
                }
                Slot::IntViaDouble => {
                    let v = self.builder.build_load(f64_type, slot, "as_double")?;
                    let v = self.builder.build_float_to_signed_int(
                        v.into_float_value(),
                        i32_type,
                        "as_i32",
                    )?;
                    call_args.push(v.into());
                    param_types.push(i32_type.into());
                }
            }
        }

        // Call the target with the one signature this trampoline exists for.
        let ret_kind = ret as hl_type_kind;
        let ret_basic: Option<BasicTypeEnum<'ctx>> = match ret_kind {
            hl_type_kind_HVOID => None,
            hl_type_kind_HF32 => Some(f32_type.into()),
            hl_type_kind_HF64 => Some(f64_type.into()),
            hl_type_kind_HI64 => Some(i64_type.into()),
            hl_type_kind_HBOOL | hl_type_kind_HUI8 | hl_type_kind_HUI16 | hl_type_kind_HI32 => {
                Some(i32_type.into())
            }
            _ => Some(ptr_type.into()),
        };
        let call_ty = match ret_basic {
            None => self.context.void_type().fn_type(&param_types, false),
            Some(t) => t.fn_type(&param_types, false),
        };
        let call = self
            .builder
            .build_indirect_call(call_ty, fun, &call_args, "dyncall")?;

        // Store the result where `ash_static_call`'s caller looks for it.
        let value_ptr = unsafe {
            self.builder.build_gep(
                self.context.i8_type(),
                out,
                &[i32_type.const_int(VDYNAMIC_VALUE_OFFSET, false)],
                "outv",
            )?
        };
        let null = ptr_type.const_null();
        match ret_kind {
            hl_type_kind_HVOID => {
                self.builder.build_return(Some(&null))?;
            }
            hl_type_kind_HF32 | hl_type_kind_HF64 | hl_type_kind_HI64 | hl_type_kind_HBOOL
            | hl_type_kind_HUI8 | hl_type_kind_HUI16 | hl_type_kind_HI32 => {
                let value = call
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| anyhow!("trampoline {name}: call returned void"))?;
                self.builder.build_store(value_ptr, value)?;
                self.builder.build_return(Some(&null))?;
            }
            _ => {
                let value = call
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| anyhow!("trampoline {name}: call returned void"))?;
                self.builder.build_return(Some(&value))?;
            }
        }

        if let Some(block) = saved {
            self.builder.position_at_end(block);
        }
        Ok(tramp)
    }
}
