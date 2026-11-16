//! Static variables. A class's statics live in one object that a global holds, so reading
//! `Class.x` is reading that global and then a field of it, and writing it is a field write.

use air::v2::ir::*;
use ash_tast::format::*;

use super::{FnLower, Result};
use crate::build::FieldSlot;

impl<'m, 'a> FnLower<'m, 'a> {
    /// The global that holds the statics object of `field`'s class, and where `field` sits in it.
    pub(super) fn static_slot(&mut self, field: u32) -> Result<(u32, FieldSlot)> {
        let rec = self.tast.fields()[field as usize];
        let flags = rec.flags.get();
        if flags & field_flag::METHOD != 0 {
            return Err("a static method used as a value is not lowered yet".into());
        }
        if flags & (field_flag::NOT_PHYSICAL | field_flag::EXTERN | field_flag::DYNAMIC) != 0 {
            let name = self.m.types.field_name(field);
            return Err(format!("{name} has no storage of its own to read or write"));
        }
        let (global, ty) = self.m.types.class_global(rec.owner.get(), true)?;
        let name = self.m.types.field_name(field);
        let slot = self.m.types.slot_in(ty, name).ok_or_else(|| {
            let class = self.m.types.class_name(rec.owner.get());
            format!("{class}.{name} has no slot in the statics layout")
        })?;
        Ok((global, slot))
    }

    /// `Class.x`: the class's statics object, read from its global, and where `x` sits in it.
    pub(super) fn static_target(&mut self, n: &NodeRec) -> Result<Option<(ValueId, FieldSlot)>> {
        let field = n.b.get();
        if field == NONE {
            return Err("a field the file does not name".into());
        }
        let (global, slot) = self.static_slot(field)?;
        let obj = self.value(slot.owner);
        self.emit(Instr::GetGlobal {
            dst: obj,
            global: global as usize,
        });
        Ok(Some((obj, slot)))
    }
}
