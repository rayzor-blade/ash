//! String constants. A string literal is not an allocation: it reads a global that holds one
//! shared `String` object, which the module builds when it starts, as genhl does with its
//! constants table.

use air::v2::ir::*;
use ash_tast::format::*;

use super::{FnLower, Result};

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
}
