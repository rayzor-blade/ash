//! Traps on wasm, as exception handlers instead of `setjmp`.
//!
//! A throw reaches a trap the same way on every target: the runtime finds the
//! innermost armed trap and `longjmp`s to its buffer. On wasm that `longjmp`
//! is a throw of the `__c_longjmp` tag carrying the buffer, so a function can
//! catch it directly. Entering a trap here arms it and records its buffer in a
//! slot; every call inside the region is an `invoke` that unwinds to the
//! trap's catch block; that block runs the handler when the thrown buffer is
//! the trap's own, or passes the exception to the enclosing trap's catch
//! block, or to the caller. Nothing is paid on entry beyond arming the trap,
//! and because the exceptional edges are real CFG edges the function is
//! optimized like any other.

use std::collections::HashMap;

use anyhow::{Result, anyhow};
use inkwell::AddressSpace;
use inkwell::basic_block::BasicBlock;
use inkwell::llvm_sys;
use inkwell::values::{AsValueRef, FunctionValue, InstructionOpcode, PointerValue};

use crate::llvm::module::JITModule;

unsafe extern "C" {
    fn ash_call_to_invoke(
        call: llvm_sys::prelude::LLVMValueRef,
        unwind: llvm_sys::prelude::LLVMBasicBlockRef,
    ) -> llvm_sys::prelude::LLVMBasicBlockRef;
}

/// The personality every function with a catch block names.
pub(crate) const PERSONALITY: &str = "__gxx_wasm_personality_v0";

/// The `__c_longjmp` tag's index in `llvm.wasm.catch`.
const C_LONGJMP_TAG: u64 = 1;

/// One function's trap regions while it is lowered. A trap is named by its
/// handler block.
#[derive(Default)]
pub(crate) struct WasmTraps<'ctx> {
    /// The slot holding each trap's armed buffer, null while it is not armed.
    pub slots: HashMap<u32, PointerValue<'ctx>>,
    /// Where each trap's caught exception continues.
    pub landings: Vec<(u32, BasicBlock<'ctx>)>,
    /// The trap each trap is armed inside, if any.
    pub parents: HashMap<u32, Option<u32>>,
    /// Lowered blocks inside a region, with the region's trap: their calls
    /// unwind to its catch block.
    pub covered: Vec<(BasicBlock<'ctx>, u32)>,
    /// The trap of the AIR block being lowered, if it is in a region.
    pub current: Option<u32>,
}

impl<'ctx> JITModule<'ctx> {
    /// Whether traps are exception handlers rather than `setjmp`.
    pub(crate) fn traps_are_wasm_handlers(&self) -> bool {
        self.target_abi.triple().starts_with("wasm")
    }

    /// Arm the trap whose handler is `handler`: the runtime links it into the
    /// chain and answers its buffer, which the slot keeps for the catch block.
    ///
    /// A throw jumps with a copy of the buffer, not the buffer itself, so the
    /// buffer carries its own address in its first word for the catch block
    /// to compare. Those two words are where `setjmp` keeps its invocation
    /// and label; a label that is not zero, under an invocation no
    /// `setjmp`-lowered frame holds, sends the exception past any such frame
    /// it meets on the way.
    pub(super) fn arm_wasm_trap(&mut self, handler: u32) -> Result<()> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let setup = self.declare_native("hlp_setup_trap_jit", &[], Some(ptr_type.into()));
        let buf = self
            .builder
            .build_call(setup, &[], "air_trap_buf")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| anyhow!("hlp_setup_trap_jit returned void"))?
            .into_pointer_value();
        self.builder.build_store(buf, buf)?;
        let label_at = unsafe {
            self.builder.build_gep(
                self.context.i8_type(),
                buf,
                &[self
                    .context
                    .i32_type()
                    .const_int(self.target_abi.pointer_bytes() as u64, false)],
                "air_trap_label",
            )?
        };
        self.builder
            .build_store(label_at, self.context.i32_type().const_all_ones())?;
        let slot = self.wasm_trap_slot(handler)?;
        self.builder.build_store(slot, buf)?;
        if let Some(traps) = self.wasm_traps.as_mut() {
            let parent = traps.current;
            traps.parents.insert(handler, parent);
        }
        Ok(())
    }

    /// Mark the trap whose handler is `handler` as no longer armed.
    pub(super) fn disarm_wasm_trap(&mut self, handler: u32) -> Result<()> {
        let slot = self.wasm_trap_slot(handler)?;
        let null = self.context.ptr_type(AddressSpace::default()).const_null();
        self.builder.build_store(slot, null)?;
        Ok(())
    }

    fn wasm_trap_slot(&self, handler: u32) -> Result<PointerValue<'ctx>> {
        self.wasm_traps
            .as_ref()
            .and_then(|t| t.slots.get(&handler).copied())
            .ok_or_else(|| anyhow!("no trap slot for handler b{handler}"))
    }

    /// Build each trap's catch block and send every call in its region to it.
    ///
    /// One catch block per trap rather than one per function: a handler's
    /// own calls then unwind to the enclosing trap's block, never back into
    /// the block that caught, which the backend does not lower correctly.
    pub(super) fn finish_wasm_traps(&mut self, function: FunctionValue<'ctx>) -> Result<()> {
        let Some(traps) = self.wasm_traps.take() else {
            return Ok(());
        };
        let mut landings: Vec<(u32, BasicBlock<'ctx>)> = Vec::new();
        for &(handler, landing) in &traps.landings {
            if !landings.iter().any(|(h, _)| *h == handler) {
                landings.push((handler, landing));
            }
        }
        if landings.is_empty() {
            return Ok(());
        }
        let saved = self.builder.get_insert_block();
        let dispatches: HashMap<u32, BasicBlock<'ctx>> = landings
            .iter()
            .map(|&(h, _)| {
                (
                    h,
                    self.context
                        .append_basic_block(function, &format!("air_eh_dispatch_b{h}")),
                )
            })
            .collect();
        for &(handler, landing) in &landings {
            let outer = traps
                .parents
                .get(&handler)
                .copied()
                .flatten()
                .and_then(|p| dispatches.get(&p).copied());
            self.build_wasm_catch(
                function,
                dispatches[&handler],
                outer,
                traps.slots[&handler],
                landing,
            )?;
        }

        // Every call a region makes unwinds to its trap's catch block.
        let mut calls = Vec::new();
        let mut visited = std::collections::HashSet::new();
        for &(block, handler) in &traps.covered {
            if !visited.insert(block.as_mut_ptr()) {
                continue;
            }
            let Some(&dispatch) = dispatches.get(&handler) else {
                continue;
            };
            let mut at = block.get_first_instruction();
            while let Some(i) = at {
                if i.get_opcode() == InstructionOpcode::Call && !calls_intrinsic(i) {
                    calls.push((i, dispatch));
                }
                at = i.get_next_instruction();
            }
        }
        for (call, dispatch) in calls {
            unsafe {
                ash_call_to_invoke(call.as_value_ref(), dispatch.as_mut_ptr());
            }
        }

        let personality = self.wasm_personality();
        unsafe {
            llvm_sys::core::LLVMSetPersonalityFn(
                function.as_value_ref(),
                personality.as_value_ref(),
            );
        }
        if let Some(block) = saved {
            self.builder.position_at_end(block);
        }
        Ok(())
    }

    /// One trap's catch block at `dispatch`: the exception goes to `landing`
    /// when its buffer is the one in `slot`, else on to `outer`, or to the
    /// caller when there is none.
    fn build_wasm_catch(
        &self,
        function: FunctionValue<'ctx>,
        dispatch: BasicBlock<'ctx>,
        outer: Option<BasicBlock<'ctx>>,
        slot: PointerValue<'ctx>,
        landing: BasicBlock<'ctx>,
    ) -> Result<()> {
        use llvm_sys::core::*;
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i32_type = self.context.i32_type();
        let pad = self.context.append_basic_block(function, "air_eh_pad");
        let take = self.context.append_basic_block(function, "air_eh_take");
        let pass = self.context.append_basic_block(function, "air_eh_pass");
        let builder = self.builder.as_mut_ptr();

        self.builder.position_at_end(dispatch);
        let catchpad = unsafe {
            let switch = LLVMBuildCatchSwitch(
                builder,
                std::ptr::null_mut(),
                outer.map_or(std::ptr::null_mut(), |b| b.as_mut_ptr()),
                1,
                c"air_eh_switch".as_ptr(),
            );
            LLVMAddHandler(switch, pad.as_mut_ptr());
            self.builder.position_at_end(pad);
            LLVMBuildCatchPad(
                builder,
                switch,
                std::ptr::null_mut(),
                0,
                c"air_eh_catch".as_ptr(),
            )
        };
        let catch = self
            .module
            .get_function("llvm.wasm.catch")
            .unwrap_or_else(|| {
                self.module.add_function(
                    "llvm.wasm.catch",
                    ptr_type.fn_type(&[i32_type.into()], false),
                    None,
                )
            });
        let thrown = self
            .builder
            .build_call(
                catch,
                &[i32_type.const_int(C_LONGJMP_TAG, false).into()],
                "air_eh_thrown",
            )?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| anyhow!("llvm.wasm.catch returned nothing"))?
            .into_pointer_value();
        // `{ env, val }`, as `__wasm_longjmp` throws it; `env` is a copy of
        // the trap's buffer, whose first word names the buffer.
        let env = self
            .builder
            .build_load(ptr_type, thrown, "air_eh_env")?
            .into_pointer_value();
        let val_at = unsafe {
            self.builder.build_gep(
                self.context.i8_type(),
                thrown,
                &[i32_type.const_int(self.target_abi.pointer_bytes() as u64, false)],
                "air_eh_val_at",
            )?
        };
        let val = self
            .builder
            .build_load(i32_type, val_at, "air_eh_val")?
            .into_int_value();
        let named = self
            .builder
            .build_load(ptr_type, env, "air_eh_trap")?
            .into_pointer_value();
        let armed = self
            .builder
            .build_load(ptr_type, slot, "air_eh_armed")?
            .into_pointer_value();
        let ours = self.builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            named,
            armed,
            "air_eh_ours",
        )?;
        self.builder.build_conditional_branch(ours, take, pass)?;

        self.builder.position_at_end(take);
        unsafe {
            LLVMBuildCatchRet(builder, catchpad, landing.as_mut_ptr());
        }

        // Someone else's: the same jump goes on, thrown anew rather than
        // rethrown. A rethrow keeps the caught exception in an `exnref`
        // local, and the fiber transform cannot resume a call while one
        // might be live.
        self.builder.position_at_end(pass);
        let jump = self.wasm_trap_pass();
        unsafe {
            let mut funclet_args = [catchpad];
            let bundle =
                LLVMCreateOperandBundle(c"funclet".as_ptr(), 7, funclet_args.as_mut_ptr(), 1);
            let mut bundles = [bundle];
            let mut args = [env.as_value_ref(), val.as_value_ref()];
            let ty = LLVMGlobalGetValueType(jump.as_value_ref());
            match outer {
                // A call in a catch block unwinds where its catch switch
                // does, so inside another trap it is an invoke to that one.
                Some(outer) => {
                    let never = self.context.append_basic_block(function, "air_eh_passed");
                    LLVMBuildInvokeWithOperandBundles(
                        builder,
                        ty,
                        jump.as_value_ref(),
                        args.as_mut_ptr(),
                        2,
                        never.as_mut_ptr(),
                        outer.as_mut_ptr(),
                        bundles.as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    self.builder.position_at_end(never);
                }
                None => {
                    LLVMBuildCallWithOperandBundles(
                        builder,
                        ty,
                        jump.as_value_ref(),
                        args.as_mut_ptr(),
                        2,
                        bundles.as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                }
            }
            LLVMDisposeOperandBundle(bundle);
            LLVMBuildUnreachable(builder);
        }
        Ok(())
    }

    /// `ash_trap_pass(env, val)`: `longjmp(env, val)` again, for a jump a
    /// catch block does not take. A function of its own, never inlined: the
    /// backend rewrites a `longjmp` call without the funclet bundle a call in
    /// a catch block needs, so the call there must be to something else.
    fn wasm_trap_pass(&self) -> FunctionValue<'ctx> {
        const NAME: &str = "ash_trap_pass";
        if let Some(f) = self.module.get_function(NAME) {
            return f;
        }
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i32_type = self.context.i32_type();
        let void = self.context.void_type();
        let f = self.module.add_function(
            NAME,
            void.fn_type(&[ptr_type.into(), i32_type.into()], false),
            Some(inkwell::module::Linkage::Internal),
        );
        for name in ["noinline", "noreturn"] {
            f.add_attribute(
                inkwell::attributes::AttributeLoc::Function,
                self.context.create_enum_attribute(
                    inkwell::attributes::Attribute::get_named_enum_kind_id(name),
                    0,
                ),
            );
        }
        let longjmp = self.module.get_function("longjmp").unwrap_or_else(|| {
            self.module.add_function(
                "longjmp",
                void.fn_type(&[ptr_type.into(), i32_type.into()], false),
                None,
            )
        });
        let saved = self.builder.get_insert_block();
        let entry = self.context.append_basic_block(f, "entry");
        self.builder.position_at_end(entry);
        let env = f.get_nth_param(0).expect("env").into();
        let val = f.get_nth_param(1).expect("val").into();
        let _ = self.builder.build_call(longjmp, &[env, val], "");
        let _ = self.builder.build_unreachable();
        if let Some(block) = saved {
            self.builder.position_at_end(block);
        }
        f
    }

    /// The personality a function with exception pads must name. The catch
    /// blocks filter by tag alone, so nothing calls it; it is defined here,
    /// weakly, so no runtime has to supply it.
    fn wasm_personality(&self) -> FunctionValue<'ctx> {
        if let Some(f) = self.module.get_function(PERSONALITY) {
            return f;
        }
        let i32_type = self.context.i32_type();
        let f = self.module.add_function(
            PERSONALITY,
            i32_type.fn_type(&[], true),
            Some(inkwell::module::Linkage::WeakODR),
        );
        f.as_global_value()
            .set_visibility(inkwell::GlobalVisibility::Hidden);
        let saved = self.builder.get_insert_block();
        let entry = self.context.append_basic_block(f, "entry");
        self.builder.position_at_end(entry);
        let _ = self.builder.build_return(Some(&i32_type.const_zero()));
        if let Some(block) = saved {
            self.builder.position_at_end(block);
        }
        f
    }
}

/// Whether `call` is to an LLVM intrinsic, which never unwinds.
fn calls_intrinsic(call: inkwell::values::InstructionValue<'_>) -> bool {
    unsafe {
        let callee = llvm_sys::core::LLVMGetCalledValue(call.as_value_ref());
        !callee.is_null() && llvm_sys::core::LLVMGetIntrinsicID(callee) != 0
    }
}
