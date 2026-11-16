//! `Array<T>` indexing, as genhl writes it: inline, over the fields of the class that stands for
//! the array. An `Array<Int>` is an object with its `length` and a `bytes` buffer, so a read
//! compares the index with the length, and either reads the buffer or gives the element type's
//! default.

use air::v2::ir::*;
use ash_tast::format::*;

use super::{FnLower, Result};
use crate::hl::HlType;

impl<'m, 'a> FnLower<'m, 'a> {
    /// `a[i]`. An index past the length reads the default of the element type, as Haxe says.
    pub(super) fn array_get(&mut self, n: &NodeRec) -> Result<Option<ValueId>> {
        // log2 of the element's size in bytes
        let elem = self.node_ty(n)?;
        let shift = match self.m.types.table.types[elem] {
            HlType::I32 => 2,
            HlType::F64 => 3,
            ref other => {
                return Err(format!("an array element of {other:?} is not lowered yet"));
            }
        };
        let Some(arr) = self.expr(self.kid(n, 0))? else {
            return Ok(None);
        };
        let arr_ty = self.val_ty(arr);
        let slots = (
            self.m.types.slot_in(arr_ty, "length"),
            self.m.types.slot_in(arr_ty, "bytes"),
        );
        let (Some(length), Some(bytes)) = slots else {
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
            return Err(format!("indexing {what}: it has no length and bytes to read"));
        };
        let Some(idx) = self.expr(self.kid(n, 1))? else {
            return Ok(None);
        };
        let i32_ty = self.ty_of(HlType::I32);
        let idx = self.coerce(idx, i32_ty)?;
        self.emit(Instr::NullCheck { value: arr });
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
        let buffer = self.load_field(arr, bytes);
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
        self.write_var(result, inside, dst);
        self.jump(join);

        self.seal(join);
        self.cur = Some(join);
        self.read_var(result, join).map(Some)
    }
}
