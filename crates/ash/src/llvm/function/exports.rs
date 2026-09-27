//! The C functions a host calls program members by; see `host_export`.

use anyhow::{Result, anyhow};
use inkwell::AddressSpace;
use inkwell::IntPredicate;
use inkwell::module::Linkage;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue};

use crate::hl;
use crate::host_export::ResolvedExport;
use crate::llvm::module::JITModule;

impl<'ctx> JITModule<'ctx> {
    /// Define every export's function. Runs after the bodies are lowered:
    /// each calls the function its export resolved to.
    pub(crate) fn emit_host_exports(&mut self) -> Result<()> {
        let exports = std::mem::take(&mut self.aot_exports);
        let module = crate::air_pipeline::AshModule::new(&self.bytecode);
        let guards: Vec<Guard> = exports
            .iter()
            .map(|e| {
                let x = &e.export;
                let casts_nothrow = x.casts_nothrow
                    || x.arg_casts
                        .iter()
                        .chain(std::iter::once(&x.ret_cast))
                        .flatten()
                        .all(|c| c.starts_with("ash:"));
                if !casts_nothrow {
                    Guard::Trap
                } else if module.frameless(e.findex) {
                    Guard::None
                } else if e.receiver && module.frameless_given_receiver(e.findex) {
                    Guard::NullReceiver
                } else {
                    Guard::Trap
                }
            })
            .collect();
        drop(module);
        for (e, guard) in exports.iter().zip(guards) {
            let symbol = e.export.symbol.as_str();
            let body = self.export_body(e, &format!("ash_export_body_{symbol}"), None)?;
            match guard {
                Guard::None => self.export_unguarded(e, body)?,
                Guard::Trap => self.export_trapped(e, body, symbol, Linkage::External)?,
                // A null receiver takes the trapped path, where the member's
                // own null check raises; any other runs the body bare.
                Guard::NullReceiver => {
                    let trapped = format!("ash_export_trapped_{symbol}");
                    self.export_trapped(e, body, &trapped, Linkage::Internal)?;
                    let trapped = self
                        .module
                        .get_function(&trapped)
                        .ok_or_else(|| anyhow!("export `{symbol}`: no trapped path"))?;
                    let fast =
                        self.export_body(e, &format!("ash_export_fast_{symbol}"), Some(trapped))?;
                    self.export_unguarded(e, fast)?;
                }
            }
        }
        self.aot_exports = exports;
        Ok(())
    }

    fn export_trapped(
        &mut self,
        e: &ResolvedExport,
        body: FunctionValue<'ctx>,
        name: &str,
        linkage: Linkage,
    ) -> Result<()> {
        if self.traps_are_wasm_handlers() {
            self.export_boundary_wasm(e, body, name, linkage)
        } else {
            self.export_boundary(e, body, name, linkage)
        }
    }

    /// `symbol` for a member that cannot throw, whose casts cannot either:
    /// the body alone, with no trap, which the optimizer folds into it.
    fn export_unguarded(&self, e: &ResolvedExport, body: FunctionValue<'ctx>) -> Result<()> {
        let x = &e.export;
        if self.module.get_function(&x.symbol).is_some() {
            return Err(anyhow!(
                "export `{}`: the symbol is already defined",
                x.symbol
            ));
        }
        let saved_block = self.builder.get_insert_block();
        let wrapper = self
            .module
            .add_function(&x.symbol, body.get_type(), Some(Linkage::External));
        self.stamp_host_cpu(wrapper);
        let entry = self.context.append_basic_block(wrapper, "entry");
        self.builder.position_at_end(entry);
        let args: Vec<BasicMetadataValueEnum> =
            wrapper.get_param_iter().map(|p| p.into()).collect();
        let result = self
            .builder
            .build_call(body, &args, "result")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| anyhow!("export body returned nothing"))?;
        self.builder.build_return(Some(&result))?;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        Ok(())
    }

    /// `symbol`: the body inside a trap, so a throw reaches `raise` rather
    /// than the caller. Kept out of the optimizer, as every function that
    /// calls `setjmp` is.
    fn export_boundary(
        &self,
        e: &ResolvedExport,
        body: FunctionValue<'ctx>,
        name: &str,
        linkage: Linkage,
    ) -> Result<()> {
        let x = &e.export;
        let i64_type = self.context.i64_type();
        let i32_type = self.context.i32_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let fn_type = body.get_type();
        if self.module.get_function(name).is_some() {
            return Err(anyhow!(
                "export `{}`: `{name}` is already defined",
                x.symbol
            ));
        }
        let saved_block = self.builder.get_insert_block();
        let wrapper = self.module.add_function(name, fn_type, Some(linkage));
        self.stamp_host_cpu(wrapper);
        for name in ["noinline", "optnone"] {
            let attr = self.context.create_enum_attribute(
                inkwell::attributes::Attribute::get_named_enum_kind_id(name),
                0,
            );
            wrapper.add_attribute(inkwell::attributes::AttributeLoc::Function, attr);
        }

        let start = self.context.append_basic_block(wrapper, "start");
        let normal = self.context.append_basic_block(wrapper, "normal");
        let thrown = self.context.append_basic_block(wrapper, "thrown");
        self.builder.position_at_end(start);
        let setup = self.declare_native("hlp_setup_trap_jit", &[], Some(ptr_type.into()));
        let buf = self
            .builder
            .build_call(setup, &[], "export_trap")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| anyhow!("hlp_setup_trap_jit returned nothing"))?
            .into_pointer_value();
        let jumped = self.build_setjmp_call(buf, "export_setjmp")?;
        let is_throw = self.builder.build_int_compare(
            IntPredicate::NE,
            jumped,
            i32_type.const_zero(),
            "thrown_p",
        )?;
        self.builder
            .build_conditional_branch(is_throw, thrown, normal)?;

        self.builder.position_at_end(normal);
        let args: Vec<BasicMetadataValueEnum> =
            wrapper.get_param_iter().map(|p| p.into()).collect();
        let result = self
            .builder
            .build_call(body, &args, "result")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| anyhow!("export body returned nothing"))?;
        let remove = self.declare_native("hlp_remove_trap_jit", &[], None);
        self.builder.build_call(remove, &[], "")?;
        self.builder.build_return(Some(&result))?;

        self.builder.position_at_end(thrown);
        let get_exc = self.declare_native("hlp_get_exc_value", &[], Some(ptr_type.into()));
        let exc = self
            .builder
            .build_call(get_exc, &[], "exception")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| anyhow!("hlp_get_exc_value returned nothing"))?;
        let clear = self.declare_native("hlp_clear_exc_value", &[], None);
        self.builder.build_call(clear, &[], "")?;
        let raise = self.aot_runtime_fn(
            &x.raise,
            self.context.void_type().fn_type(&[ptr_type.into()], false),
        );
        self.builder.build_call(raise, &[exc.into()], "")?;
        self.builder
            .build_return(Some(&i64_type.const_int(x.unit, false)))?;

        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        if !wrapper.verify(true) {
            return Err(anyhow!("export `{}`: invalid function", x.symbol));
        }
        Ok(())
    }

    /// `symbol` on wasm: the body called under a trap that is an exception
    /// handler (see `wasm_traps`), so entering it costs no `setjmp` and the
    /// function is optimized.
    fn export_boundary_wasm(
        &mut self,
        e: &ResolvedExport,
        body: FunctionValue<'ctx>,
        name: &str,
        linkage: Linkage,
    ) -> Result<()> {
        let x = &e.export;
        let i64_type = self.context.i64_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        if self.module.get_function(name).is_some() {
            return Err(anyhow!(
                "export `{}`: `{name}` is already defined",
                x.symbol
            ));
        }
        let saved_block = self.builder.get_insert_block();
        let wrapper = self
            .module
            .add_function(name, body.get_type(), Some(linkage));
        self.stamp_host_cpu(wrapper);
        let start = self.context.append_basic_block(wrapper, "start");
        let thrown = self.context.append_basic_block(wrapper, "thrown");
        self.builder.position_at_end(start);

        const HANDLER: u32 = 0;
        let slot = self.builder.build_alloca(ptr_type, "export_trap_slot")?;
        self.builder.build_store(slot, ptr_type.const_null())?;
        let mut traps = super::wasm_traps::WasmTraps::default();
        traps.slots.insert(HANDLER, slot);
        traps.landings.push((HANDLER, thrown));
        traps.covered.push((start, HANDLER));
        self.wasm_traps = Some(traps);

        self.arm_wasm_trap(HANDLER)?;
        let args: Vec<BasicMetadataValueEnum> =
            wrapper.get_param_iter().map(|p| p.into()).collect();
        let result = self
            .builder
            .build_call(body, &args, "result")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| anyhow!("export body returned nothing"))?;
        let remove = self.declare_native("hlp_remove_trap_jit", &[], None);
        self.builder.build_call(remove, &[], "")?;
        self.builder.build_return(Some(&result))?;

        self.builder.position_at_end(thrown);
        let get_exc = self.declare_native("hlp_get_exc_value", &[], Some(ptr_type.into()));
        let exc = self
            .builder
            .build_call(get_exc, &[], "exception")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| anyhow!("hlp_get_exc_value returned nothing"))?;
        let clear = self.declare_native("hlp_clear_exc_value", &[], None);
        self.builder.build_call(clear, &[], "")?;
        let raise = self.aot_runtime_fn(
            &x.raise,
            self.context.void_type().fn_type(&[ptr_type.into()], false),
        );
        self.builder.build_call(raise, &[exc.into()], "")?;
        self.builder
            .build_return(Some(&i64_type.const_int(x.unit, false)))?;

        self.finish_wasm_traps(wrapper)?;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        if !wrapper.verify(true) {
            return Err(anyhow!("export `{}`: invalid function", x.symbol));
        }
        Ok(())
    }

    /// Words in, word out: the casts around one call of the member, as an
    /// internal function `name`. With `null_exit`, a null receiver is
    /// answered by calling it with the same words instead.
    fn export_body(
        &mut self,
        e: &ResolvedExport,
        name: &str,
        null_exit: Option<FunctionValue<'ctx>>,
    ) -> Result<FunctionValue<'ctx>> {
        let x = &e.export;
        let i64_type = self.context.i64_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let (target, _) = self.get_or_create_function_value(e.findex)?;
        let target_params = target.get_type().get_param_types();
        if target_params.len() != e.params.len() {
            return Err(anyhow!(
                "export `{}`: the member takes {} arguments, its function {}",
                x.symbol,
                e.params.len(),
                target_params.len()
            ));
        }
        let mut param_descs = Vec::with_capacity(e.params.len());
        for &t in &e.params {
            param_descs.push(self.get_initialized_type(t)?);
        }
        let ret_desc = self.get_initialized_type(e.ret)?;
        let ret_kind = self.types_[e.ret].kind;

        let words: Vec<BasicMetadataTypeEnum> = vec![i64_type.into(); e.params.len()];
        let saved_block = self.builder.get_insert_block();
        let body = self.module.add_function(
            name,
            i64_type.fn_type(&words, false),
            Some(Linkage::Internal),
        );
        let entry = self.context.append_basic_block(body, "entry");
        self.builder.position_at_end(entry);

        let mut args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(e.params.len());
        for (i, word) in body.get_param_iter().enumerate() {
            let word = word.into_int_value();
            let target_ty = BasicTypeEnum::try_from(target_params[i])
                .map_err(|_| anyhow!("export `{}`: parameter {i} has no value type", x.symbol))?;
            let value = match &x.arg_casts[i] {
                Some(cast) => self.emit_host_cast(cast, word.into(), param_descs[i], target_ty)?,
                None => self.word_to(word, target_ty, self.types_[e.params[i]].kind)?,
            };
            args.push(value.into());
        }
        if let Some(exit) = null_exit {
            let receiver = match args.first() {
                Some(BasicMetadataValueEnum::PointerValue(p)) => *p,
                _ => {
                    return Err(anyhow!(
                        "export `{}`: the receiver is not an object",
                        x.symbol
                    ));
                }
            };
            let is_null = self.builder.build_is_null(receiver, "receiver_null")?;
            let exit_block = self.context.append_basic_block(body, "null_receiver");
            let call_block = self.context.append_basic_block(body, "call");
            self.builder
                .build_conditional_branch(is_null, exit_block, call_block)?;
            self.builder.position_at_end(exit_block);
            let words: Vec<BasicMetadataValueEnum> =
                body.get_param_iter().map(|p| p.into()).collect();
            let answer = self
                .builder
                .build_call(exit, &words, "trapped")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    anyhow!("export `{}`: the trapped path returned nothing", x.symbol)
                })?;
            self.builder.build_return(Some(&answer))?;
            self.builder.position_at_end(call_block);
        }
        let call = self.builder.build_call(target, &args, "member")?;
        let result = match (call.try_as_basic_value().basic(), ret_kind) {
            (_, hl::hl_type_kind_HVOID) | (None, _) => i64_type.const_int(x.unit, false),
            (Some(value), _) => match &x.ret_cast {
                Some(cast) => self
                    .emit_host_cast(cast, value, ret_desc, i64_type.into())?
                    .into_int_value(),
                None => self.word_from(value, ret_kind)?,
            },
        };
        self.builder.build_return(Some(&result))?;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        Ok(body)
    }

    /// `value` through the host's cast named `cast`, answering `target`.
    ///
    /// A name in the `ash:` namespace is a conversion emitted inline rather
    /// than a function called, for the NaN-boxed words many interpreters
    /// use, where a number is its own `f64` bits and every other value is a
    /// quiet NaN with bits set under `0x7ffc_0000_0000_0000`:
    /// `ash:unbox_f64` reads a word as a float, NaN when it is not a number;
    /// `ash:box_f64` writes a float as a word, a NaN as the canonical
    /// `0x7ff8_0000_0000_0000` so it cannot read as a boxed value.
    /// Any other name is called as `cast(value, t) -> target`, `t` being
    /// `desc`, the program's type for the value.
    pub(super) fn emit_host_cast(
        &mut self,
        cast: &str,
        value: BasicValueEnum<'ctx>,
        desc: BasicValueEnum<'ctx>,
        target: BasicTypeEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>> {
        const BOXED: u64 = 0x7ffc_0000_0000_0000;
        const CANONICAL_NAN: u64 = 0x7ff8_0000_0000_0000;
        let b = &self.builder;
        let i64_type = self.context.i64_type();
        let f64_type = self.context.f64_type();
        match cast {
            "ash:unbox_f64" => {
                let word = match value {
                    BasicValueEnum::IntValue(v) if v.get_type().get_bit_width() == 64 => v,
                    BasicValueEnum::IntValue(v) => b.build_int_z_extend(v, i64_type, "word")?,
                    other => {
                        return Err(anyhow!("{cast} takes a word, not {:?}", other.get_type()));
                    }
                };
                let tag = b.build_and(word, i64_type.const_int(BOXED, false), "boxed_bits")?;
                let is_num = b.build_int_compare(
                    IntPredicate::NE,
                    tag,
                    i64_type.const_int(BOXED, false),
                    "is_num",
                )?;
                let bits = b.build_bit_cast(word, f64_type, "num")?.into_float_value();
                let nan = f64_type.const_float(f64::NAN);
                let num = b
                    .build_select(is_num, bits, nan, "unboxed")?
                    .into_float_value();
                Ok(match target {
                    BasicTypeEnum::FloatType(t) if t == f64_type => num.into(),
                    BasicTypeEnum::FloatType(t) => {
                        b.build_float_trunc(num, t, "unboxed_f32")?.into()
                    }
                    other => return Err(anyhow!("{cast} answers a float, not {other:?}")),
                })
            }
            "ash:box_f64" => {
                let num = match value {
                    BasicValueEnum::FloatValue(v) if v.get_type() == f64_type => v,
                    BasicValueEnum::FloatValue(v) => b.build_float_ext(v, f64_type, "num")?,
                    other => {
                        return Err(anyhow!("{cast} takes a float, not {:?}", other.get_type()));
                    }
                };
                let is_nan =
                    b.build_float_compare(inkwell::FloatPredicate::UNO, num, num, "is_nan")?;
                let bits = b.build_bit_cast(num, i64_type, "bits")?.into_int_value();
                let canonical = i64_type.const_int(CANONICAL_NAN, false);
                let word = b
                    .build_select(is_nan, canonical, bits, "boxed")?
                    .into_int_value();
                Ok(match target {
                    BasicTypeEnum::IntType(t) if t == i64_type => word.into(),
                    other => return Err(anyhow!("{cast} answers a word, not {other:?}")),
                })
            }
            _ if cast.starts_with("ash:") => Err(anyhow!("no built-in cast `{cast}`")),
            _ => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let cast_fn = self.aot_runtime_fn(
                    cast,
                    target.fn_type(&[value.get_type().into(), ptr_type.into()], false),
                );
                self.builder
                    .build_call(cast_fn, &[value.into(), desc.into()], "cast")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| anyhow!("{cast} returned nothing"))
            }
        }
    }

    /// A word as a value of `ty`: an integer narrowed, a float by its bits,
    /// a pointer by its address.
    fn word_to(
        &self,
        word: IntValue<'ctx>,
        ty: BasicTypeEnum<'ctx>,
        kind: hl::hl_type_kind,
    ) -> Result<BasicValueEnum<'ctx>> {
        let b = &self.builder;
        Ok(match ty {
            BasicTypeEnum::IntType(t) if kind == hl::hl_type_kind_HBOOL => {
                let truth = b.build_int_compare(
                    IntPredicate::NE,
                    word,
                    word.get_type().const_zero(),
                    "word_bool",
                )?;
                b.build_int_z_extend_or_bit_cast(truth, t, "word_bool_slot")?
                    .into()
            }
            BasicTypeEnum::IntType(t) if t.get_bit_width() < 64 => {
                b.build_int_truncate(word, t, "word_int")?.into()
            }
            BasicTypeEnum::IntType(_) => word.into(),
            BasicTypeEnum::FloatType(t) if t == self.context.f32_type() => {
                let low = b.build_int_truncate(word, self.context.i32_type(), "word_low")?;
                b.build_bit_cast(low, t, "word_f32")?
            }
            BasicTypeEnum::FloatType(t) => b.build_bit_cast(word, t, "word_f64")?,
            BasicTypeEnum::PointerType(t) => b.build_int_to_ptr(word, t, "word_ptr")?.into(),
            other => return Err(anyhow!("no word form for {other:?}")),
        })
    }

    /// `value` as a word: a signed integer sign-extended, any other
    /// zero-extended, a float by its bits, a pointer by its address.
    fn word_from(
        &self,
        value: BasicValueEnum<'ctx>,
        kind: hl::hl_type_kind,
    ) -> Result<IntValue<'ctx>> {
        let b = &self.builder;
        let i64_type = self.context.i64_type();
        Ok(match value {
            BasicValueEnum::IntValue(v) if v.get_type().get_bit_width() == 64 => v,
            BasicValueEnum::IntValue(v)
                if kind == hl::hl_type_kind_HI32 || kind == hl::hl_type_kind_HI64 =>
            {
                b.build_int_s_extend(v, i64_type, "word")?
            }
            BasicValueEnum::IntValue(v) => b.build_int_z_extend(v, i64_type, "word")?,
            BasicValueEnum::FloatValue(v) if v.get_type() == self.context.f32_type() => {
                let bits = b
                    .build_bit_cast(v, self.context.i32_type(), "f32_bits")?
                    .into_int_value();
                b.build_int_z_extend(bits, i64_type, "word")?
            }
            BasicValueEnum::FloatValue(v) => {
                b.build_bit_cast(v, i64_type, "word")?.into_int_value()
            }
            BasicValueEnum::PointerValue(v) => b.build_ptr_to_int(v, i64_type, "word")?,
            other => return Err(anyhow!("no word form for {other:?}")),
        })
    }
}

/// What stands between a host's call and the member.
enum Guard {
    /// Nothing: the member and its casts cannot throw.
    None,
    /// A trap, so a throw reaches `raise`.
    Trap,
    /// A test of the receiver: only a null one can make the member throw.
    NullReceiver,
}
