//! The compiler's own operations, which the std writes as `$name(arguments)`: reading and writing
//! a byte buffer, and how wide its elements are. genhl turns each into one or two opcodes; the
//! element type is the type argument of the `hl.BytesAccess<T>` the buffer is typed as.

use air::v2::ir::*;
use ash_tast::format::*;

use super::{FnLower, Result};
use crate::hl::{HlType, TypeIx};

impl<'m, 'a> FnLower<'m, 'a> {
    pub(super) fn intrinsic(&mut self, name: &str, args: &[u32]) -> Result<Option<ValueId>> {
        match (name, args) {
            ("$bytes_sizebits", [b]) => {
                let (_, bits) = self.byte_element(*b)?;
                Ok(Some(self.int_const(bits)))
            }
            ("$bget", [b, pos]) => {
                let (elem, bits) = self.byte_element(*b)?;
                let Some(buffer) = self.expr(*b)? else {
                    return Ok(None);
                };
                let Some(pos) = self.expr(*pos)? else {
                    return Ok(None);
                };
                let at = self.byte_offset(pos, bits)?;
                let dst = self.value(elem);
                self.emit(Instr::MemGet {
                    kind: MemAccess::Mem,
                    dst,
                    base: buffer,
                    index: at,
                });
                Ok(Some(dst))
            }
            ("$bset", [b, pos, value]) => {
                let (elem, bits) = self.byte_element(*b)?;
                let Some(buffer) = self.expr(*b)? else {
                    return Ok(None);
                };
                let Some(pos) = self.expr(*pos)? else {
                    return Ok(None);
                };
                let Some(value) = self.expr(*value)? else {
                    return Ok(None);
                };
                let value = self.coerce(value, elem)?;
                let at = self.byte_offset(pos, bits)?;
                self.emit(Instr::MemSet {
                    kind: MemAccess::Mem,
                    base: buffer,
                    index: at,
                    src: value,
                });
                Ok(Some(value))
            }
            _ => Err(format!(
                "the intrinsic {name} with {} arguments is not lowered yet",
                args.len()
            )),
        }
    }

    /// The element type of the `hl.BytesAccess<T>` that `buffer` is typed as, and log2 of its
    /// size in bytes. Only 32-bit integers and 64-bit floats so far.
    fn byte_element(&mut self, buffer: u32) -> Result<(TypeIx, i32)> {
        let n = *self.tast.node(buffer);
        let t = self.tast.types()[n.ty.get() as usize];
        let is_access = t.kind == type_kind::ABSTRACT
            && self
                .tast
                .opt_string(self.tast.abstracts()[t.def.get() as usize].name)
                == Some("hl.BytesAccess");
        let arg = self.tast.type_args(t.args_first, t.args_count).first().copied();
        let (true, Some(arg)) = (is_access, arg) else {
            let what = match t.kind {
                type_kind::ABSTRACT => format!(
                    "abstract {:?}",
                    self.tast
                        .opt_string(self.tast.abstracts()[t.def.get() as usize].name)
                ),
                type_kind::CLASS => format!(
                    "class {:?}",
                    self.tast.opt_string(self.tast.classes()[t.def.get() as usize].name)
                ),
                other => format!("type kind {other}"),
            };
            return Err(format!(
                "a byte intrinsic on {what}, which is not an hl.BytesAccess"
            ));
        };
        let elem = self.m.types.to_type(arg.get())?;
        match self.m.types.table.types[elem] {
            HlType::I32 => Ok((elem, 2)),
            HlType::F64 => Ok((elem, 3)),
            ref other => Err(format!("a byte buffer of {other:?} is not lowered yet")),
        }
    }

    /// `pos << bits`: the byte offset of element `pos`.
    fn byte_offset(&mut self, pos: ValueId, bits: i32) -> Result<ValueId> {
        let i32_ty = self.ty_of(HlType::I32);
        let pos = self.coerce(pos, i32_ty)?;
        let by = self.int_const(bits);
        let at = self.value(i32_ty);
        self.emit(Instr::BinOp {
            op: BinOp::Shl,
            dst: at,
            a: pos,
            b: by,
        });
        Ok(at)
    }

    /// `throw value`: the value as a Dynamic, then the end of the block.
    pub(super) fn throw(&mut self, n: &NodeRec) -> Result<Option<ValueId>> {
        let Some(v) = self.expr(self.kid(n, 0))? else {
            return Ok(None);
        };
        let dynamic = self.ty_of(HlType::Dyn);
        let exc = self.coerce(v, dynamic)?;
        self.terminate(Terminator::Throw { exc });
        Ok(None)
    }
}
