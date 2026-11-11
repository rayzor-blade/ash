//! Calls that go through a pointer: a method of a class, a method of a
//! structural type, and a closure.
use super::*;

/// The wasm parameter and result types of a signature.
pub(crate) fn wasm_signature(sig: &Sig) -> (Vec<ValType>, Vec<ValType>) {
    (
        sig.params.iter().filter_map(|t| t.wasm()).collect(),
        sig.ret.wasm().into_iter().collect(),
    )
}

fn mem(offset: u64, align: u32) -> we::MemArg {
    we::MemArg {
        offset,
        align,
        memory_index: 0,
    }
}

impl<P: Program> Fx<'_, P> {
    /// The scratch word `n` (0 to 3) of the frame.
    pub(crate) fn scratch_word(&self, n: u32) -> u32 {
        self.scratch + n
    }

    /// The scratch words are locals a body has only when it has a frame.
    pub(crate) fn need_scratch(&self) -> Res<()> {
        if self.has_frame {
            Ok(())
        } else {
            unsupported(NEEDS_FRAME)
        }
    }

    /// The result of a call that left it on the stack: into `dst`, or nowhere.
    fn call_result(&mut self, dst: ValueId, ret: Ty) -> Res<()> {
        if ret == Ty::Void {
            return Ok(());
        }
        if self.ty(dst) != ret {
            return unsupported("a call whose result type differs from its destination");
        }
        // A callee compiled elsewhere need not extend what it returns.
        self.narrow(ret);
        self.set(dst);
        Ok(())
    }

    /// The arguments of a call match the signature they are passed through.
    fn check_arguments(&self, sig: &Sig, args: &[ValueId]) -> Res<()> {
        if sig.params.len() != args.len() {
            return unsupported("a call with the wrong argument count");
        }
        for (a, want) in args.iter().zip(&sig.params) {
            if self.ty(*a) != *want {
                return unsupported("a call with a mismatched argument");
            }
        }
        Ok(())
    }

    /// `receiver.method(args)`: the method in the receiver's class table.
    pub(crate) fn call_method(&mut self, dst: ValueId, field: usize, args: &[ValueId]) -> Res<()> {
        let Some(&this) = args.first() else {
            return unsupported("a method call with no receiver");
        };
        let obj_ty = self.f.values[this.0 as usize].ty;
        match self.program.shape(obj_ty) {
            Shape::Obj => {}
            Shape::Virtual => return self.call_virtual(dst, obj_ty, field, args),
            other => return unsupported(format!("a method call on {other:?}")),
        }
        let Some(sig) = self.program.method(obj_ty, field) else {
            return unsupported("a method slot whose function is not known");
        };
        self.check_arguments(&sig, args)?;
        for a in args {
            self.get(*a);
        }
        // object -> type -> method table -> slot
        self.get(this);
        self.emit(&I::I32Load(mem(0, 2)));
        self.emit(&I::I32Load(mem(TYPE_VOBJ_PROTO, 2)));
        self.emit(&I::I32Load(mem(4 * field as u64, 2)));
        let (p, r) = wasm_signature(&sig);
        self.out.call_indirect(p, r);
        self.call_result(dst, sig.ret)
    }

    /// A method of a structural type. The value it views carries the real
    /// method in a slot of the view; a view whose slot is empty, or a value
    /// that is not a view at all, is called by name through the runtime.
    fn call_virtual(
        &mut self,
        dst: ValueId,
        obj_ty: TypeRef,
        field: usize,
        args: &[ValueId],
    ) -> Res<()> {
        self.need_scratch()?;
        let Some(declared) = self.program.virtual_method(obj_ty, field) else {
            return unsupported("a method of a structural type that is not a function");
        };
        let Some(hash) = self.program.virtual_field(obj_ty, field) else {
            return unsupported("a method of a structural type with no name");
        };
        // The receiver is passed as the object the view stands for, a plain
        // reference; the rest as declared.
        self.check_arguments(&declared, args)?;
        let (view, value, target) = (
            self.scratch_word(0),
            self.scratch_word(1),
            self.scratch_word(2),
        );
        self.get(args[0]);
        self.emit(&I::LocalSet(view));

        // direct: a real view, with its slot filled.
        self.emit(&I::LocalGet(view));
        self.emit(&I::If(BlockType::Result(ValType::I32)));
        self.emit(&I::LocalGet(view));
        self.emit(&I::I32Load(mem(0, 2)));
        self.emit(&I::I32Load(mem(0, 2)));
        self.emit(&I::I32Const(self.program.virtual_kind()));
        self.emit(&I::I32Eq);
        self.emit(&I::Else);
        self.emit(&I::I32Const(0));
        self.emit(&I::End);
        self.emit(&I::If(BlockType::Result(ValType::I32)));
        self.emit(&I::LocalGet(view));
        self.emit(&I::I32Load(mem(VVIRTUAL_VALUE, 2)));
        self.emit(&I::LocalSet(value));
        self.emit(&I::LocalGet(view));
        self.emit(&I::I32Load(mem(VVIRTUAL_FIELDS + 4 * field as u64, 2)));
        self.emit(&I::LocalTee(target));
        self.emit(&I::Else);
        self.emit(&I::I32Const(0));
        self.emit(&I::End);
        self.emit(&I::If(BlockType::Empty));
        self.emit(&I::LocalGet(value));
        for a in &args[1..] {
            self.get(*a);
        }
        self.emit(&I::LocalGet(target));
        let (p, r) = wasm_signature(&declared);
        self.out.call_indirect(p, r);
        if declared.ret != Ty::Void {
            self.call_result(dst, declared.ret)?;
        }
        self.emit(&I::Else);
        self.dynamic_method_call(dst, obj_ty, args, hash, view)?;
        self.emit(&I::End);
        Ok(())
    }

    /// Call a method by name: box the arguments, let the runtime find the
    /// method on the real object, and unbox what comes back.
    fn dynamic_method_call(
        &mut self,
        dst: ValueId,
        _obj_ty: TypeRef,
        args: &[ValueId],
        hash: i32,
        view: u32,
    ) -> Res<()> {
        let tail = &args[1..];
        let array = self.scratch_word(3);
        if tail.is_empty() {
            self.emit(&I::I32Const(0));
        } else {
            self.emit(&I::I32Const(tail.len() as i32));
            let alloc = self.runtime("hlp_alloc_dyn_array")?;
            self.call(alloc);
        }
        self.emit(&I::LocalSet(array));
        for (i, a) in tail.iter().enumerate() {
            self.emit(&I::LocalGet(array));
            self.boxed_argument(*a)?;
            self.emit(&I::I32Store(mem(VARRAY_DATA + 4 * i as u64, 2)));
        }
        self.emit(&I::LocalGet(view));
        self.emit(&I::I32Const(hash));
        self.emit(&I::LocalGet(array));
        let call = self.runtime("hlp_vcall_dyn")?;
        self.call(call);
        self.dynamic_result(dst)
    }

    /// Leave on the stack a Dynamic for argument `a`: the value itself if it
    /// carries its own type, a box if it does not.
    fn boxed_argument(&mut self, a: ValueId) -> Res<()> {
        let ty = self.f.values[a.0 as usize].ty;
        if self.program.self_describing(ty) {
            self.get(a);
            return Ok(());
        }
        let kind = self.ty(a);
        let Some(name) = self.program.type_symbol(ty.0 as usize) else {
            return unsupported("a type with no descriptor symbol");
        };
        let slot = self.frame_alloc(8, 8)?;
        self.frame_address(slot);
        self.get(a);
        self.store_kind(kind, 0)?;
        self.frame_address(slot);
        self.data_address(&name, 0)?;
        let make = self.runtime("hlp_make_dyn")?;
        self.call(make);
        Ok(())
    }

    /// The Dynamic a by-name call returned is on the stack; put it in `dst`
    /// as the type `dst` has.
    fn dynamic_result(&mut self, dst: ValueId) -> Res<()> {
        let dst_ty = self.f.values[dst.0 as usize].ty;
        let kind = self.ty(dst);
        match kind {
            Ty::Void => {
                self.emit(&I::Drop);
                return Ok(());
            }
            Ty::Ptr => match self.program.shape(dst_ty) {
                Shape::Dyn => {}
                Shape::Virtual => {
                    // The callee returns its own view; the caller's type is
                    // what the rest of this body expects.
                    self.emit(&I::LocalSet(self.tmp_local));
                    self.type_of_value(dst)?;
                    self.emit(&I::LocalGet(self.tmp_local));
                    let wrap = self.runtime("hl_to_virtual")?;
                    self.call(wrap);
                }
                _ => {
                    let Some(dyn_ty) = self.program.dyn_type() else {
                        return unsupported("a program with no Dynamic type");
                    };
                    let Some(dyn_name) = self.program.type_symbol(dyn_ty.0 as usize) else {
                        return unsupported("a type with no descriptor symbol");
                    };
                    let slot = self.frame_alloc(4, 4)?;
                    self.emit(&I::LocalSet(self.tmp_local));
                    self.frame_address(slot);
                    self.emit(&I::LocalGet(self.tmp_local));
                    self.emit(&I::I32Store(mem(0, 2)));
                    self.frame_address(slot);
                    self.data_address(&dyn_name, 0)?;
                    self.type_of_value(dst)?;
                    let cast = self.runtime("hlp_dyn_castp")?;
                    self.call(cast);
                }
            },
            Ty::F64 => {
                let r = self.runtime("hlp_dyn_todouble")?;
                self.call(r);
            }
            Ty::F32 => {
                let r = self.runtime("hlp_dyn_tofloat")?;
                self.call(r);
            }
            Ty::I64 => {
                let r = self.runtime("hlp_dyn_toi64")?;
                self.call(r);
            }
            Ty::Bool | Ty::I32 | Ty::U8 | Ty::U16 => {
                let r = self.runtime("hlp_dyn_toint")?;
                self.call(r);
                if kind == Ty::Bool {
                    self.emit(&I::I32Const(0));
                    self.emit(&I::I32Ne);
                }
                self.narrow(kind);
            }
            Ty::Other => return unsupported("a dynamic result of no storable kind"),
        }
        self.set(dst);
        Ok(())
    }

    /// Call the closure `fun`. A closure made to adapt a signature wraps the
    /// one that does the work. When the closure's own type is the type the
    /// call site expects, its function is called directly, with the value it
    /// carries as a leading argument if it carries one; any other closure is
    /// called through the runtime, which boxes the arguments.
    pub(crate) fn call_closure(&mut self, dst: ValueId, fun: ValueId, args: &[ValueId]) -> Res<()> {
        self.need_scratch()?;
        let fun_ty = self.f.values[fun.0 as usize].ty;
        let Some(type_name) = self.program.type_symbol(fun_ty.0 as usize) else {
            return unsupported("a closure type with no descriptor symbol");
        };
        let ret = self.ty(dst);
        let params: Vec<Ty> = args.iter().map(|a| self.ty(*a)).collect();
        if params.iter().any(|t| t.wasm().is_none()) || ret == Ty::Other {
            return unsupported("a closure call with a value of no wasm kind");
        }
        let (raw, callee_closure, runtime_type) = (
            self.scratch_word(0),
            self.scratch_word(1),
            self.scratch_word(2),
        );

        self.get(fun);
        self.emit(&I::LocalSet(raw));
        // The closure that does the work.
        self.emit(&I::LocalGet(raw));
        self.emit(&I::I32Load(mem(VCLOSURE_HAS_VALUE, 2)));
        self.emit(&I::I32Const(WRAPPER));
        self.emit(&I::I32Eq);
        self.emit(&I::If(BlockType::Result(ValType::I32)));
        self.emit(&I::LocalGet(raw));
        self.emit(&I::I32Load(mem(VCLOSURE_WRAPPED, 2)));
        self.emit(&I::Else);
        self.emit(&I::LocalGet(raw));
        self.emit(&I::End);
        self.emit(&I::LocalSet(callee_closure));

        // Is the closure's own type the one this call site was written for?
        self.emit(&I::LocalGet(raw));
        self.emit(&I::I32Load(mem(0, 2)));
        self.emit(&I::LocalTee(runtime_type));
        self.data_address(&type_name, 0)?;
        self.emit(&I::I32Eq);
        self.emit(&I::If(BlockType::Result(ValType::I32)));
        self.emit(&I::I32Const(1));
        self.emit(&I::Else);
        self.emit(&I::LocalGet(runtime_type));
        self.data_address(&type_name, 0)?;
        let same = self.runtime("hlp_same_type")?;
        self.call(same);
        self.emit(&I::End);
        self.emit(&I::If(BlockType::Empty));

        // Typed: with the bound value, or without.
        let plain = Sig {
            params: params.clone(),
            ret,
        };
        let mut bound = plain.clone();
        bound.params.insert(0, Ty::Ptr);
        self.emit(&I::LocalGet(callee_closure));
        self.emit(&I::I32Load(mem(VCLOSURE_HAS_VALUE, 2)));
        self.emit(&I::If(BlockType::Empty));
        self.emit(&I::LocalGet(callee_closure));
        self.emit(&I::I32Load(mem(VCLOSURE_VALUE, 2)));
        for a in args {
            self.get(*a);
        }
        self.emit(&I::LocalGet(callee_closure));
        self.emit(&I::I32Load(mem(VCLOSURE_FUN, 2)));
        let (p, r) = wasm_signature(&bound);
        self.out.call_indirect(p, r);
        self.call_result(dst, ret)?;
        self.emit(&I::Else);
        for a in args {
            self.get(*a);
        }
        self.emit(&I::LocalGet(callee_closure));
        self.emit(&I::I32Load(mem(VCLOSURE_FUN, 2)));
        let (p, r) = wasm_signature(&plain);
        self.out.call_indirect(p, r);
        self.call_result(dst, ret)?;
        self.emit(&I::End);

        self.emit(&I::Else);
        // Dynamic: the runtime marshals through the closure's own type.
        let argv = self.frame_alloc(4 * args.len().max(1) as u32, 4)?;
        for (i, a) in args.iter().enumerate() {
            self.frame_address(argv + 4 * i as u32);
            self.boxed_argument(*a)?;
            self.emit(&I::I32Store(mem(0, 2)));
        }
        self.emit(&I::LocalGet(raw));
        self.frame_address(argv);
        self.emit(&I::I32Const(args.len() as i32));
        let call = self.runtime("hlp_dyn_call")?;
        self.call(call);
        self.dynamic_result(dst)?;
        self.emit(&I::End);
        Ok(())
    }

    /// A call to a native whose address the host puts in a slot: through the
    /// slot, or the runtime's report that the native is missing when the slot
    /// is empty.
    pub(crate) fn call_native_slot(
        &mut self,
        dst: ValueId,
        fun: usize,
        args: &[ValueId],
        native: &NativeSlot,
    ) -> Res<()> {
        self.need_scratch()?;
        let Some(sig) = self.program.signature(fun) else {
            return unsupported("a native whose signature is not known");
        };
        self.check_arguments(&sig, args)?;
        for a in args {
            self.get(*a);
        }
        let target = self.scratch_word(0);
        self.data_address(&native.slot, 0)?;
        self.emit(&I::I32Load(mem(0, 2)));
        self.emit(&I::LocalTee(target));
        self.emit(&I::I32Eqz);
        self.emit(&I::If(BlockType::Empty));
        self.data_address(&format!("{CSTR}{}", native.lib), 0)?;
        self.data_address(&format!("{CSTR}{}", native.prim), 0)?;
        let missing = self.runtime("hlp_aot_native_missing")?;
        self.call(missing);
        self.emit(&I::Unreachable);
        self.emit(&I::End);
        self.emit(&I::LocalGet(target));
        let (p, r) = wasm_signature(&sig);
        self.out.call_indirect(p, r);
        self.call_result(dst, sig.ret)
    }
}
