//! Instructions that go through the runtime by name or by type: fields of a
//! Dynamic, the type of a value, constants from the pools, and the casts that
//! hand the runtime a pointer to the value.
use super::*;

fn mem(offset: u64, align: u32) -> we::MemArg {
    we::MemArg {
        offset,
        align,
        memory_index: 0,
    }
}

impl<P: Program> Fx<'_, P> {
    /// `dst = obj.<name>`, found by name at run time.
    pub(crate) fn dyn_get(&mut self, dst: ValueId, obj: ValueId, field: usize) -> Res<()> {
        let kind = self.ty(dst);
        let Some(hash) = self.program.string_hash(field) else {
            return unsupported("a field name that is not in the string pool");
        };
        let getter = match kind {
            Ty::F64 => "hlp_dyn_getd",
            Ty::F32 => "hlp_dyn_getf",
            Ty::I64 => "hlp_dyn_geti64",
            Ty::Bool | Ty::I32 | Ty::U8 | Ty::U16 => "hlp_dyn_geti",
            Ty::Ptr => "hlp_dyn_getp",
            Ty::Void | Ty::Other | Ty::V128 => return unsupported("a dynamic field of no storable kind"),
        };
        self.get(obj);
        self.emit(&I::I32Const(hash));
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
        Ok(())
    }

    /// `obj.<name> = src`, found by name at run time.
    pub(crate) fn dyn_set(&mut self, obj: ValueId, field: usize, src: ValueId) -> Res<()> {
        let kind = self.ty(src);
        let Some(hash) = self.program.string_hash(field) else {
            return unsupported("a field name that is not in the string pool");
        };
        let setter = match kind {
            Ty::F64 => "hlp_dyn_setd",
            Ty::F32 => "hlp_dyn_setf",
            Ty::I64 => "hlp_dyn_seti64",
            Ty::Bool | Ty::I32 | Ty::U8 | Ty::U16 => "hlp_dyn_seti",
            Ty::Ptr => "hlp_dyn_setp",
            Ty::Void | Ty::Other | Ty::V128 => return unsupported("a dynamic field of no storable kind"),
        };
        self.get(obj);
        self.emit(&I::I32Const(hash));
        if !matches!(kind, Ty::F64 | Ty::F32 | Ty::I64) {
            self.type_of_value(src)?;
        }
        self.get(src);
        let setter = self.runtime(setter)?;
        self.call(setter);
        Ok(())
    }

    /// `dst = src->t`; the type of no value is the void type.
    pub(crate) fn get_type(&mut self, dst: ValueId, src: ValueId) -> Res<()> {
        let Some(void) = self.program.void_type() else {
            return unsupported("a program with no void type");
        };
        let Some(name) = self.program.type_symbol(void.0 as usize) else {
            return unsupported("a type with no descriptor symbol");
        };
        self.get(src);
        self.emit(&I::If(BlockType::Result(ValType::I32)));
        self.get(src);
        self.emit(&I::I32Load(mem(0, 2)));
        self.emit(&I::Else);
        self.data_address(&name, 0)?;
        self.emit(&I::End);
        self.set(dst);
        Ok(())
    }

    /// `dst = kind of src`: read through the value's header, or the static
    /// kind for a null.
    pub(crate) fn get_tid(&mut self, dst: ValueId, src: ValueId) -> Res<()> {
        let src_ty = self.f.values[src.0 as usize].ty;
        if self.ty(src) != Ty::Ptr {
            let Some(kind) = self.program.type_kind(src_ty) else {
                return unsupported("a kind that is not known");
            };
            self.emit(&I::I32Const(kind));
            self.set(dst);
            return Ok(());
        }
        let Some(kind) = self.program.type_kind(src_ty) else {
            return unsupported("a kind that is not known");
        };
        let is_type = self.program.is_type(src_ty);
        self.get(src);
        self.emit(&I::If(BlockType::Result(ValType::I32)));
        self.get(src);
        if !is_type {
            // The header's type, and then its kind.
            self.emit(&I::I32Load(mem(0, 2)));
        }
        self.emit(&I::I32Load(mem(0, 2)));
        self.emit(&I::Else);
        self.emit(&I::I32Const(kind));
        self.emit(&I::End);
        self.set(dst);
        Ok(())
    }

    pub(crate) fn string_const(&mut self, dst: ValueId, idx: usize) -> Res<()> {
        let Some(name) = self.program.string_symbol(idx) else {
            return unsupported("a string that is not in the pool");
        };
        self.data_address(&name, 0)?;
        self.set(dst);
        Ok(())
    }

    /// The address of a bytes constant; null when the index names none.
    pub(crate) fn bytes_const(&mut self, dst: ValueId, idx: usize) -> Res<()> {
        match self.program.bytes_symbol(idx) {
            Some(name) => self.data_address(&name, 0)?,
            None => self.emit(&I::I32Const(0)),
        }
        self.set(dst);
        Ok(())
    }

    /// A cast whose runtime helper takes a pointer to the value: the value is
    /// put in the frame and its address passed, with the type it has and the
    /// type it is to have.
    pub(crate) fn cast_through_memory(&mut self, dst: ValueId, src: ValueId) -> Res<()> {
        let kind = self.ty(dst);
        let helper = match kind {
            Ty::F64 => "hlp_dyn_castd",
            Ty::F32 => "hlp_dyn_castf",
            Ty::I64 => "hlp_dyn_casti64",
            Ty::Bool | Ty::I32 | Ty::U8 | Ty::U16 => "hlp_dyn_casti",
            Ty::Ptr => "hlp_dyn_castp",
            Ty::Void | Ty::Other | Ty::V128 => return unsupported("a cast into a value of no storable kind"),
        };
        let slot = self.frame_alloc(4, 4)?;
        self.frame_address(slot);
        self.get(src);
        self.emit(&I::I32Store(mem(0, 2)));
        self.frame_address(slot);
        self.type_of_value(src)?;
        // The integer and the pointer forms say what they are to become.
        if matches!(helper, "hlp_dyn_casti" | "hlp_dyn_castp") {
            self.type_of_value(dst)?;
        }
        let helper = self.runtime(helper)?;
        self.call(helper);
        if kind == Ty::Bool {
            self.emit(&I::I32Const(0));
            self.emit(&I::I32Ne);
        }
        self.narrow(kind);
        self.set(dst);
        Ok(())
    }
}
