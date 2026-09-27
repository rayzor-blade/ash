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
        for e in &exports {
            let body = self.export_body(e)?;
            self.export_boundary(e, body)?;
        }
        self.aot_exports = exports;
        Ok(())
    }

    /// `symbol`: the body inside a trap, so a throw reaches `raise` rather
    /// than the caller. Kept out of the optimizer, as every function that
    /// calls `setjmp` is.
    fn export_boundary(&self, e: &ResolvedExport, body: FunctionValue<'ctx>) -> Result<()> {
        let x = &e.export;
        let i64_type = self.context.i64_type();
        let i32_type = self.context.i32_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let fn_type = body.get_type();
        if self.module.get_function(&x.symbol).is_some() {
            return Err(anyhow!(
                "export `{}`: the symbol is already defined",
                x.symbol
            ));
        }
        let saved_block = self.builder.get_insert_block();
        let wrapper = self
            .module
            .add_function(&x.symbol, fn_type, Some(Linkage::External));
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

    /// Words in, word out: the casts around one call of the member.
    fn export_body(&mut self, e: &ResolvedExport) -> Result<FunctionValue<'ctx>> {
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
            &format!("ash_export_body_{}", x.symbol),
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
                Some(cast) => {
                    let cast_fn = self.aot_runtime_fn(
                        cast,
                        target_ty.fn_type(&[i64_type.into(), ptr_type.into()], false),
                    );
                    self.builder
                        .build_call(cast_fn, &[word.into(), param_descs[i].into()], "cast")?
                        .try_as_basic_value()
                        .basic()
                        .ok_or_else(|| anyhow!("{cast} returned nothing"))?
                }
                None => self.word_to(word, target_ty, self.types_[e.params[i]].kind)?,
            };
            args.push(value.into());
        }
        let call = self.builder.build_call(target, &args, "member")?;
        let result = match (call.try_as_basic_value().basic(), ret_kind) {
            (_, hl::hl_type_kind_HVOID) | (None, _) => i64_type.const_int(x.unit, false),
            (Some(value), _) => match &x.ret_cast {
                Some(cast) => {
                    let cast_fn = self.aot_runtime_fn(
                        cast,
                        i64_type.fn_type(&[value.get_type().into(), ptr_type.into()], false),
                    );
                    self.builder
                        .build_call(cast_fn, &[value.into(), ret_desc.into()], "ret_cast")?
                        .try_as_basic_value()
                        .basic()
                        .ok_or_else(|| anyhow!("{cast} returned nothing"))?
                        .into_int_value()
                }
                None => self.word_from(value, ret_kind)?,
            },
        };
        self.builder.build_return(Some(&result))?;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        Ok(body)
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
