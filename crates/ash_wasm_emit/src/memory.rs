//! Values that live in memory: the cells that registers whose address is
//! taken are pinned to, and the references made from them.
use super::*;

fn mem(offset: u64, align: u32) -> we::MemArg {
    we::MemArg {
        offset,
        align,
        memory_index: 0,
    }
}

fn size_of(t: Ty) -> Option<u32> {
    match t {
        Ty::Bool | Ty::U8 => Some(1),
        Ty::U16 => Some(2),
        Ty::I32 | Ty::F32 | Ty::Ptr => Some(4),
        Ty::I64 | Ty::F64 => Some(8),
        Ty::Void | Ty::Other => None,
    }
}

impl<P: Program> Fx<'_, P> {
    /// Give every cell its place in the frame, zeroed, as a register is at
    /// the start of a function.
    pub(crate) fn open_cells(&mut self) -> Res<()> {
        let kinds: Vec<Ty> = self.f.cells.iter().map(|c| self.program.ty(c.ty)).collect();
        for kind in kinds {
            // A void cell holds nothing, and nothing reads it.
            if kind == Ty::Void {
                self.cell_offsets.push(0);
                continue;
            }
            let Some(size) = size_of(kind) else {
                return unsupported("a cell of no storable kind");
            };
            let at = self.frame_alloc(size, size)?;
            self.cell_offsets.push(at);
            self.frame_address(at);
            self.zero(kind);
            self.store_kind(kind, 0)?;
        }
        Ok(())
    }

    pub(crate) fn cell_place(&self, cell: air::v2::ir::CellId) -> Res<(u32, Ty)> {
        let at = *self
            .cell_offsets
            .get(cell.idx())
            .ok_or_else(|| Unsupported(NEEDS_FRAME.to_string()))?;
        Ok((at, self.program.ty(self.f.cells[cell.idx()].ty)))
    }

    pub(crate) fn cell_get(&mut self, dst: ValueId, cell: air::v2::ir::CellId) -> Res<()> {
        let (at, kind) = self.cell_place(cell)?;
        if kind == Ty::Void && self.ty(dst) == Ty::Void {
            return Ok(());
        }
        if self.ty(dst) != kind {
            return unsupported("a cell read into a value of another kind");
        }
        self.frame_address(0);
        self.load_kind(kind, u64::from(at))?;
        self.set(dst);
        Ok(())
    }

    pub(crate) fn cell_set(&mut self, cell: air::v2::ir::CellId, src: ValueId) -> Res<()> {
        let (at, kind) = self.cell_place(cell)?;
        if kind == Ty::Void && self.ty(src) == Ty::Void {
            return Ok(());
        }
        if self.ty(src) != kind {
            return unsupported("a cell written from a value of another kind");
        }
        self.frame_address(0);
        self.get(src);
        self.store_kind(kind, u64::from(at))
    }

    pub(crate) fn cell_step(&mut self, cell: air::v2::ir::CellId, up: bool) -> Res<()> {
        let (at, kind) = self.cell_place(cell)?;
        self.frame_address(0);
        self.frame_address(0);
        self.load_kind(kind, u64::from(at))?;
        match kind {
            Ty::I32 | Ty::U8 | Ty::U16 => {
                self.emit(&I::I32Const(1));
                self.emit(&if up { I::I32Add } else { I::I32Sub });
                self.narrow(kind);
            }
            Ty::I64 => {
                self.emit(&I::I64Const(1));
                self.emit(&if up { I::I64Add } else { I::I64Sub });
            }
            other => return unsupported(format!("a step of a cell of {other:?}")),
        }
        self.store_kind(kind, u64::from(at))
    }

    pub(crate) fn cell_ref(&mut self, dst: ValueId, cell: air::v2::ir::CellId) -> Res<()> {
        let (at, _) = self.cell_place(cell)?;
        if self.ty(dst) != Ty::Ptr {
            return unsupported("a reference held in other than a pointer");
        }
        self.frame_address(at);
        self.set(dst);
        Ok(())
    }

    /// `dst = *src`; the value behind no reference is zero.
    pub(crate) fn unref(&mut self, dst: ValueId, src: ValueId) -> Res<()> {
        let kind = self.ty(dst);
        let Some(wasm) = kind.wasm() else {
            return unsupported("a dereference into a value of no storable kind");
        };
        self.get(src);
        self.emit(&I::I32Eqz);
        self.emit(&I::If(BlockType::Result(wasm)));
        self.zero(kind);
        self.emit(&I::Else);
        self.get(src);
        self.load_kind(kind, 0)?;
        self.emit(&I::End);
        self.set(dst);
        Ok(())
    }

    /// `*r = value`; a store through no reference does nothing.
    pub(crate) fn set_ref(&mut self, r: ValueId, value: ValueId) -> Res<()> {
        let kind = self.ty(value);
        if size_of(kind).is_none() {
            return unsupported("a store through a reference of a value of no storable kind");
        }
        self.get(r);
        self.emit(&I::If(BlockType::Empty));
        self.get(r);
        self.get(value);
        self.store_kind(kind, 0)?;
        self.write_barrier(|fx| fx.get(r), value)?;
        self.emit(&I::End);
        Ok(())
    }

    /// `dst = &src->data`: the address of an array's first element.
    pub(crate) fn ref_data(&mut self, dst: ValueId, src: ValueId) -> Res<()> {
        self.get(src);
        self.emit(&I::I32Const(VARRAY_DATA as i32));
        self.emit(&I::I32Add);
        self.set(dst);
        Ok(())
    }

    /// `dst = base + offset * size`: the offset counts elements of the type
    /// the reference refers to.
    pub(crate) fn ref_offset(&mut self, dst: ValueId, base: ValueId, offset: ValueId) -> Res<()> {
        let dst_ty = self.f.values[dst.0 as usize].ty;
        let Some(inner) = self.program.null_param(dst_ty) else {
            return unsupported("a reference offset whose destination is not a reference");
        };
        let Some(stride) = self.program.array_elem_size(inner) else {
            return unsupported("a reference to elements of unknown size");
        };
        self.get(base);
        self.get(offset);
        if stride != 1 {
            self.emit(&I::I32Const(stride as i32));
            self.emit(&I::I32Mul);
        }
        self.emit(&I::I32Add);
        self.set(dst);
        Ok(())
    }

    // Enums: `{ t, index, fields... }`, each field at the offset the program
    // gives for its construct.

    pub(crate) fn enum_alloc(&mut self, dst: ValueId, construct: usize) -> Res<()> {
        self.type_of_value(dst)?;
        self.emit(&I::I32Const(construct as i32));
        let alloc = self.runtime("hlp_alloc_enum")?;
        self.call(alloc);
        self.set(dst);
        Ok(())
    }

    pub(crate) fn make_enum(&mut self, dst: ValueId, construct: usize, args: &[ValueId]) -> Res<()> {
        let enum_ty = self.f.values[dst.0 as usize].ty;
        let tmp = self.tmp_local;
        self.type_of_value(dst)?;
        self.emit(&I::I32Const(construct as i32));
        let alloc = self.runtime("hlp_alloc_enum")?;
        self.call(alloc);
        self.emit(&I::LocalSet(tmp));
        for (j, a) in args.iter().enumerate() {
            let Some((offset, kind)) = self.program.enum_field(enum_ty, construct, j) else {
                return unsupported("an enum field whose place is not known");
            };
            if self.ty(*a) != kind {
                return unsupported("an enum field written from a value of another kind");
            }
            self.emit(&I::LocalGet(tmp));
            self.get(*a);
            self.store_kind(kind, u64::from(offset))?;
        }
        self.emit(&I::LocalGet(tmp));
        self.set(dst);
        Ok(())
    }

    /// The constructor index of no enum value is 0.
    pub(crate) fn enum_index(&mut self, dst: ValueId, value: ValueId) -> Res<()> {
        self.get(value);
        self.emit(&I::I32Eqz);
        self.emit(&I::If(BlockType::Result(ValType::I32)));
        self.emit(&I::I32Const(0));
        self.emit(&I::Else);
        self.get(value);
        self.emit(&I::I32Load(mem(4, 2)));
        self.emit(&I::End);
        self.set(dst);
        Ok(())
    }

    /// A field of no enum value is zero.
    pub(crate) fn enum_field(
        &mut self,
        dst: ValueId,
        value: ValueId,
        construct: usize,
        field: usize,
    ) -> Res<()> {
        let enum_ty = self.f.values[value.0 as usize].ty;
        let Some((offset, kind)) = self.program.enum_field(enum_ty, construct, field) else {
            return unsupported("an enum field whose place is not known");
        };
        let Some(wasm) = kind.wasm() else {
            return unsupported("an enum field of no storable kind");
        };
        if self.ty(dst) != kind {
            return unsupported("an enum field read into a value of another kind");
        }
        self.get(value);
        self.emit(&I::I32Eqz);
        self.emit(&I::If(BlockType::Result(wasm)));
        self.zero(kind);
        self.emit(&I::Else);
        self.get(value);
        self.load_kind(kind, u64::from(offset))?;
        self.emit(&I::End);
        self.set(dst);
        Ok(())
    }

    /// Setting a field of no enum value does nothing.
    pub(crate) fn set_enum_field(
        &mut self,
        value: ValueId,
        construct: usize,
        field: usize,
        src: ValueId,
    ) -> Res<()> {
        let enum_ty = self.f.values[value.0 as usize].ty;
        let Some((offset, kind)) = self.program.enum_field(enum_ty, construct, field) else {
            return unsupported("an enum field whose place is not known");
        };
        if self.ty(src) != kind {
            return unsupported("an enum field written from a value of another kind");
        }
        self.get(value);
        self.emit(&I::If(BlockType::Empty));
        self.get(value);
        self.get(src);
        self.store_kind(kind, u64::from(offset))?;
        self.write_barrier(
            |fx| {
                fx.get(value);
                if offset != 0 {
                    fx.emit(&I::I32Const(offset as i32));
                    fx.emit(&I::I32Add);
                }
            },
            src,
        )?;
        self.emit(&I::End);
        Ok(())
    }
}
