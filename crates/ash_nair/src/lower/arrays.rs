//! `Array<T>` indexing, as genhl writes it: inline, over the fields of the class that stands for
//! the array. An `Array<Int>` is an object with its `length` and a `bytes` buffer; any other array
//! holds its elements as Dynamic in a native array in `array`. A read compares the index with the
//! length and either reads the elements or gives the element type's default; a write past the
//! length first calls the class's `__expand`.

use air::v2::ir::*;
use ash_tast::format::*;

use super::{FnLower, Result};
use crate::build::FieldSlot;
use crate::hl::{HlType, TypeIx};

/// Where an array class keeps its elements.
#[derive(Clone, Copy)]
enum Elements {
    /// A byte buffer, with log2 of the element's size in bytes.
    Bytes(i32),
    /// A native array of Dynamic, holding objects.
    Refs,
    /// An `Array<Dynamic>`, which wraps another array: its elements are read and written through
    /// the `getDyn` and `setDyn` of the class.
    Dynamic,
}

impl<'m, 'a> FnLower<'m, 'a> {
    /// How an array of `elem` is laid out.
    fn elements(&self, elem: TypeIx) -> Result<Elements> {
        match self.m.types.table.types[elem] {
            HlType::Dyn => Ok(Elements::Dynamic),
            HlType::I32 => Ok(Elements::Bytes(2)),
            HlType::F64 => Ok(Elements::Bytes(3)),
            HlType::Obj(_)
            | HlType::Struct(_)
            | HlType::Virtual(_)
            | HlType::Array
            | HlType::Fun(..)
            | HlType::Enum(_)
            | HlType::DynObj => Ok(Elements::Refs),
            ref other => Err(format!("an array element of {other:?} is not lowered yet")),
        }
    }

    /// The `length` of an array object and the field that holds its elements.
    fn array_slots(&self, arr: ValueId, elems: Elements) -> Result<(FieldSlot, FieldSlot)> {
        let arr_ty = self.val_ty(arr);
        let held = match elems {
            Elements::Bytes(_) => "bytes",
            Elements::Refs => "array",
            Elements::Dynamic => return Err("an Array<Dynamic> has no elements of its own".into()),
        };
        let slots = (
            self.m.types.slot_in(arr_ty, "length"),
            self.m.types.slot_in(arr_ty, held),
        );
        let (Some(length), Some(held)) = slots else {
            let what = match self.m.types.table.types[arr_ty] {
                HlType::Obj(o) | HlType::Struct(o) => {
                    let obj = &self.m.types.table.objs[o];
                    let mut names: Vec<_> = obj.index.keys().cloned().collect();
                    names.sort();
                    let own: Vec<_> = obj.fields.iter().map(|(n, _)| n.as_str()).collect();
                    format!("{} with {names:?} (own fields {own:?})", obj.name)
                }
                ref other => format!("{other:?}"),
            };
            return Err(format!(
                "indexing {what}: it has no length and {held} to read"
            ));
        };
        Ok((length, held))
    }

    /// The array and the index of `a[i]`, evaluated in that order, the index as an integer.
    fn array_operands(&mut self, n: &NodeRec) -> Result<Option<(ValueId, ValueId)>> {
        let Some(arr) = self.expr(self.kid(n, 0))? else {
            return Ok(None);
        };
        let Some(idx) = self.expr(self.kid(n, 1))? else {
            return Ok(None);
        };
        let i32_ty = self.ty_of(HlType::I32);
        let idx = self.coerce(idx, i32_ty)?;
        self.emit(Instr::NullCheck { value: arr });
        Ok(Some((arr, idx)))
    }

    /// `a[i]`. An index past the length reads the default of the element type, as Haxe says.
    pub(super) fn array_get(&mut self, n: &NodeRec) -> Result<Option<ValueId>> {
        let elem = self.node_ty(n)?;
        let elems = self.elements(elem)?;
        let Some((arr, idx)) = self.array_operands(n)? else {
            return Ok(None);
        };
        self.array_read(arr, idx, elem, elems).map(Some)
    }

    fn array_read(
        &mut self,
        arr: ValueId,
        idx: ValueId,
        elem: TypeIx,
        elems: Elements,
    ) -> Result<ValueId> {
        if matches!(elems, Elements::Dynamic) {
            let got = self.call_known_method(arr, "hl.types.ArrayDyn", "getDyn", &[idx])?;
            return self.coerce_forced(got, elem);
        }
        let (length, held) = self.array_slots(arr, elems)?;
        let i32_ty = self.ty_of(HlType::I32);
        let len = self.load_field(arr, length);

        let result = self.temp(elem);
        let (inside, outside, join) = (self.new_block(), self.new_block(), self.new_block());
        self.terminate(Terminator::CondJump {
            cond: CondKind::ULt,
            a: idx,
            b: Some(len),
            if_true: inside,
            if_false: outside,
        });
        self.seal(inside);
        self.seal(outside);

        self.cur = Some(outside);
        let default = self.default_value(elem)?;
        self.write_var(result, outside, default);
        self.jump(join);

        self.cur = Some(inside);
        let buffer = self.load_field(arr, held);
        let read = match elems {
            Elements::Bytes(shift) => {
                let by = self.int_const(shift);
                let at = self.value(i32_ty);
                self.emit(Instr::BinOp {
                    op: BinOp::Shl,
                    dst: at,
                    a: idx,
                    b: by,
                });
                let dst = self.value(elem);
                self.emit(Instr::MemGet {
                    kind: MemAccess::Mem,
                    dst,
                    base: buffer,
                    index: at,
                });
                dst
            }
            Elements::Dynamic => unreachable!("an Array<Dynamic> is read through getDyn"),
            Elements::Refs => {
                // the native array holds Dynamic, which is the object itself
                let dynamic = self.ty_of(HlType::Dyn);
                let held = self.value(dynamic);
                self.emit(Instr::MemGet {
                    kind: MemAccess::Array,
                    dst: held,
                    base: buffer,
                    index: idx,
                });
                let dst = self.value(elem);
                self.emit(Instr::Cast {
                    kind: CastKind::UnsafeCast,
                    dst,
                    src: held,
                });
                dst
            }
        };
        self.write_var(result, inside, read);
        self.jump(join);

        self.seal(join);
        self.cur = Some(join);
        self.read_var(result, join)
    }

    /// `a[i] = v`, growing the array first when `i` is past its length.
    fn array_write(
        &mut self,
        arr: ValueId,
        idx: ValueId,
        value: ValueId,
        elems: Elements,
    ) -> Result<()> {
        if matches!(elems, Elements::Dynamic) {
            let dynamic = self.ty_of(HlType::Dyn);
            let boxed = self.coerce(value, dynamic)?;
            self.call_known_method(arr, "hl.types.ArrayDyn", "setDyn", &[idx, boxed])?;
            return Ok(());
        }
        let (length, held) = self.array_slots(arr, elems)?;
        let i32_ty = self.ty_of(HlType::I32);
        let len = self.load_field(arr, length);
        let (grow, store) = (self.new_block(), self.new_block());
        self.terminate(Terminator::CondJump {
            cond: CondKind::ULt,
            a: idx,
            b: Some(len),
            if_true: store,
            if_false: grow,
        });
        self.seal(grow);

        self.cur = Some(grow);
        let expand = self.array_expand(arr)?;
        let void = self.ty_of(HlType::Void);
        let dst = self.value(void);
        self.emit(Instr::Call {
            dst,
            fun: expand,
            args: vec![arr, idx],
        });
        self.jump(store);
        self.seal(store);

        // the elements are read after the growth: it may have replaced them
        self.cur = Some(store);
        let buffer = self.load_field(arr, held);
        match elems {
            Elements::Bytes(shift) => {
                let by = self.int_const(shift);
                let at = self.value(i32_ty);
                self.emit(Instr::BinOp {
                    op: BinOp::Shl,
                    dst: at,
                    a: idx,
                    b: by,
                });
                self.emit(Instr::MemSet {
                    kind: MemAccess::Mem,
                    base: buffer,
                    index: at,
                    src: value,
                });
            }
            Elements::Dynamic => unreachable!("an Array<Dynamic> is written through setDyn"),
            Elements::Refs => {
                let dynamic = self.ty_of(HlType::Dyn);
                let boxed = self.coerce(value, dynamic)?;
                self.emit(Instr::MemSet {
                    kind: MemAccess::Array,
                    base: buffer,
                    index: idx,
                    src: boxed,
                });
            }
        }
        Ok(())
    }

    /// The function index of the `__expand` of `arr`'s class, which is lowered with the program.
    fn array_expand(&mut self, arr: ValueId) -> Result<usize> {
        let ty = self.val_ty(arr);
        let mut class = self.m.types.class_of_object(ty);
        while let Some(c) = class {
            if let Some(field) = self.m.types.declared_field(c, "__expand") {
                let cname = self.m.types.class_name(c).to_owned();
                let fun = self.m.types.fun_id(&cname, "__expand") as usize;
                self.m.called.insert(fun as u32);
                self.m.request(field);
                return Ok(fun);
            }
            class = self.m.types.superclass(c);
        }
        Err("an array class without __expand".into())
    }

    /// `a[i] = v` and `a[i] op= v`.
    pub(super) fn assign_element(
        &mut self,
        target: &NodeRec,
        value: u32,
        op: Option<&str>,
    ) -> Result<Option<ValueId>> {
        let elem = self.node_ty(target)?;
        let elems = self.elements(elem)?;
        let Some((arr, idx)) = self.array_operands(target)? else {
            return Ok(None);
        };
        let Some(rhs) = self.expr(value)? else {
            return Ok(None);
        };
        let new = match op {
            None => self.coerce(rhs, elem)?,
            Some(op) => {
                let old = self.array_read(arr, idx, elem, elems)?;
                self.arith(op, old, rhs, elem)?
            }
        };
        self.array_write(arr, idx, new, elems)?;
        Ok(Some(new))
    }

    /// `[a, b, c]`: the elements are written into a buffer, or a native array of Dynamic, which
    /// the std's `alloc` functions wrap in the array class.
    pub(super) fn array_literal(&mut self, n: &NodeRec) -> Result<Option<ValueId>> {
        let ty = self.node_ty(n)?;
        let t = self.tast.types()[n.ty.get() as usize];
        let arg = self
            .tast
            .type_args(t.args_first, t.args_count)
            .first()
            .map(|a| a.get())
            .ok_or("an array literal with no element type")?;
        let elem = self.m.types.to_type(arg)?;
        let elems = self.elements(elem)?;
        let ids = self.kids(n);
        let int = self.ty_of(HlType::I32);
        let count = self.int_const(ids.len() as i32);
        match elems {
            Elements::Dynamic => Err("an array literal of Dynamic is not lowered yet".into()),
            Elements::Bytes(bits) => {
                let make = match self.m.types.table.types[elem] {
                    HlType::I32 => "allocI32",
                    _ => "allocF64",
                };
                let bytes_ty = self.ty_of(HlType::Bytes);
                let size = self.int_const((ids.len() as i32) << bits);
                let alloc = self.std_native("alloc_bytes", &[int], bytes_ty);
                let buffer = self.value(bytes_ty);
                self.emit(Instr::Call {
                    dst: buffer,
                    fun: alloc,
                    args: vec![size],
                });
                for (i, id) in ids.iter().enumerate() {
                    let Some(v) = self.expr(*id)? else {
                        return Ok(None);
                    };
                    let v = self.coerce(v, elem)?;
                    let at = self.int_const((i as i32) << bits);
                    self.emit(Instr::MemSet {
                        kind: MemAccess::Mem,
                        base: buffer,
                        index: at,
                        src: v,
                    });
                }
                let wrap = self.class_function("hl.types.ArrayBase", make)?;
                let out = self.value(ty);
                self.emit(Instr::Call {
                    dst: out,
                    fun: wrap,
                    args: vec![buffer, count],
                });
                Ok(Some(out))
            }
            Elements::Refs => {
                let dynamic = self.ty_of(HlType::Dyn);
                let type_ty = self.ty_of(HlType::Type);
                let array_ty = self.ty_of(HlType::Array);
                let of = self.value(type_ty);
                self.emit(Instr::TypeConst {
                    dst: of,
                    ty: TypeRef(dynamic as u32),
                });
                let alloc = self.std_native("alloc_array", &[type_ty, int], array_ty);
                let held = self.value(array_ty);
                self.emit(Instr::Call {
                    dst: held,
                    fun: alloc,
                    args: vec![of, count],
                });
                for (i, id) in ids.iter().enumerate() {
                    let Some(v) = self.expr(*id)? else {
                        return Ok(None);
                    };
                    let v = self.coerce(v, dynamic)?;
                    let at = self.int_const(i as i32);
                    self.emit(Instr::MemSet {
                        kind: MemAccess::Array,
                        base: held,
                        index: at,
                        src: v,
                    });
                }
                let wrap = self.class_function("hl.types.ArrayObj", "alloc")?;
                let out = self.value(ty);
                self.emit(Instr::Call {
                    dst: out,
                    fun: wrap,
                    args: vec![held],
                });
                Ok(Some(out))
            }
        }
    }
}
