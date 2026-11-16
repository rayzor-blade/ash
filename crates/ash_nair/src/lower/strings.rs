//! String constants. A string literal is not an allocation: it reads a global that holds one
//! shared `String` object, which the module builds when it starts, as genhl does with its
//! constants table.

use air::v2::ir::*;
use ash_tast::format::*;

use super::{FnLower, Result};
use crate::hl::{HlType, TypeIx};

impl<'m, 'a> FnLower<'m, 'a> {
    pub(super) fn string_constant(&mut self, n: &NodeRec) -> Result<ValueId> {
        let text = self.tast.string(n.a.get()).to_owned();
        let ty = self.node_ty(n)?;
        if self.m.types.slot_in(ty, "bytes").is_none() || self.m.types.slot_in(ty, "length").is_none()
        {
            return Err(format!(
                "a string constant of {:?}, which has no bytes and length",
                self.m.types.table.types[ty]
            ));
        }
        let global = self.m.string_global(&text, ty);
        let dst = self.value(ty);
        self.emit(Instr::GetGlobal {
            dst,
            global: global as usize,
        });
        Ok(dst)
    }

    /// Whether `ty` is the `String` class.
    pub(super) fn is_string(&self, ty: TypeIx) -> bool {
        matches!(self.m.types.table.types[ty],
            HlType::Obj(o) if self.m.types.table.objs[o].name == "String")
    }

    /// The type of a `String`.
    fn string_type(&mut self) -> Result<TypeIx> {
        let class = self
            .m
            .types
            .class_id("String")
            .ok_or("the program has no String class")?;
        self.m.types.class_type(class, &[], false, None)
    }

    /// A function the std's `String` class declares, which the program lowers with the caller.
    fn string_function(&mut self, name: &str) -> Result<usize> {
        self.class_function("String", name)
    }

    /// The function `name` that the class `class` declares, which the program lowers with the
    /// caller.
    pub(super) fn class_function(&mut self, class: &str, name: &str) -> Result<usize> {
        let id = self
            .m
            .types
            .class_id(class)
            .ok_or_else(|| format!("the program has no {class} class"))?;
        let field = self
            .m
            .types
            .declared_field(id, name)
            .ok_or_else(|| format!("{class}.{name} is not in the program"))?;
        let fun = self.m.types.fun_id(class, name) as usize;
        self.m.called.insert(fun as u32);
        self.m.request(field);
        Ok(fun)
    }

    /// A value as a `String`, as genhl's `to_string` makes it: an integer or a float is written by
    /// the runtime into a buffer whose length comes back through a reference.
    pub(super) fn stringify(&mut self, v: ValueId) -> Result<ValueId> {
        let ty = self.val_ty(v);
        if self.is_string(ty) {
            return Ok(v);
        }
        let int = self.ty_of(HlType::I32);
        let native = match self.m.types.table.types[ty] {
            HlType::I32 => "itos",
            HlType::F64 => "ftos",
            ref other => return Err(format!("{other:?} as a string is not lowered yet")),
        };
        let bytes_ty = self.ty_of(HlType::Bytes);
        let ref_ty = self.ty_of(HlType::Ref(int));
        let length = self.new_cell(int);
        let at = self.value(ref_ty);
        self.emit(Instr::CellRef {
            dst: at,
            cell: length,
        });
        let fun = self.std_native(native, &[ty, ref_ty], bytes_ty);
        let bytes = self.value(bytes_ty);
        self.emit(Instr::Call {
            dst: bytes,
            fun,
            args: vec![v, at],
        });
        let len = self.value(int);
        self.emit(Instr::CellGet {
            dst: len,
            cell: length,
        });
        let string = self.string_type()?;
        let alloc = self.string_function("__alloc__")?;
        let out = self.value(string);
        self.emit(Instr::Call {
            dst: out,
            fun: alloc,
            args: vec![bytes, len],
        });
        Ok(out)
    }

    /// `a + b` on strings, either of which may be a number.
    pub(super) fn concat(&mut self, a: ValueId, b: ValueId) -> Result<ValueId> {
        let (a, b) = (self.stringify(a)?, self.stringify(b)?);
        let string = self.string_type()?;
        let add = self.string_function("__add__")?;
        let out = self.value(string);
        self.emit(Instr::Call {
            dst: out,
            fun: add,
            args: vec![a, b],
        });
        Ok(out)
    }

    /// A register whose address is taken, which lives in memory for as long as the function runs.
    fn new_cell(&mut self, ty: TypeIx) -> CellId {
        let t = TypeRef(ty as u32);
        let reg = self.f.new_reg(t);
        let id = CellId(self.f.cells.len() as u32);
        self.f.cells.push(CellData {
            reg,
            ty: t,
            reason: PinReason::RefTaken,
        });
        id
    }
}
