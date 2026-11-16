//! Object literals, `{ a: 1, b: x }`. A literal has a structure type of its own, made from the
//! fields it writes. When that type can stand for the type the program expects, the object is
//! that structure with its fields set by position; otherwise it is a dynamic object whose fields
//! are set by name, which is then converted to the expected structure, as genhl does.

use air::v2::ir::*;
use ash_tast::format::*;

use super::{FnLower, Result};
use crate::hl::{HlType, TypeIx};

impl<'m, 'a> FnLower<'m, 'a> {
    pub(super) fn object_literal(&mut self, n: &NodeRec) -> Result<Option<ValueId>> {
        let want = self.node_ty(n)?;
        let mut fields = Vec::new();
        for k in self.kids(n) {
            let f = *self.tast.node(k);
            fields.push((self.tast.string(f.a.get()).to_owned(), self.kid(&f, 0)));
        }
        if fields.iter().any(|(name, _)| name == "toString") {
            return Err("an object literal with a toString field is not lowered yet".into());
        }
        let mut own = Vec::new();
        for (name, e) in &fields {
            let en = *self.tast.node(*e);
            own.push((name.clone(), self.node_ty(&en)?));
        }
        let literal = self.m.types.structure(own, want);
        if !fields.is_empty() && self.m.types.structure_fits(literal, want) {
            self.structure_object(literal, want, &fields)
        } else {
            self.dynamic_object(want, &fields)
        }
    }

    /// The object as the structure the literal has, its fields written by position.
    fn structure_object(
        &mut self,
        literal: TypeIx,
        want: TypeIx,
        fields: &[(String, u32)],
    ) -> Result<Option<ValueId>> {
        let HlType::Virtual(v) = self.m.types.table.types[literal] else {
            return Err("an object literal that is not a structure".into());
        };
        let obj = self.value(literal);
        self.emit(Instr::New { dst: obj });
        for (name, e) in fields {
            let at = self.m.types.table.virts[v]
                .iter()
                .position(|(n, _)| n == name)
                .ok_or("a literal field outside its own structure")?;
            let held = self.m.types.table.virts[v][at].1;
            let Some(value) = self.expr(*e)? else {
                return Ok(None);
            };
            let value = self.coerce(value, held)?;
            self.emit(Instr::FieldSet {
                obj,
                obj_ty: TypeRef(literal as u32),
                field: at,
                src: value,
            });
        }
        self.coerce(obj, want).map(Some)
    }

    /// A dynamic object, each field set by name at the type the expected structure gives it.
    fn dynamic_object(&mut self, want: TypeIx, fields: &[(String, u32)]) -> Result<Option<ValueId>> {
        let dynobj = self.ty_of(HlType::DynObj);
        let obj = self.value(dynobj);
        self.emit(Instr::New { dst: obj });
        let declared = match self.m.types.table.types[want] {
            HlType::Virtual(v) => self.m.types.table.virts[v].clone(),
            _ => Vec::new(),
        };
        for (name, e) in fields {
            let Some(value) = self.expr(*e)? else {
                return Ok(None);
            };
            let value = match declared.iter().find(|(n, _)| n == name) {
                Some((_, held)) => self.coerce(value, *held)?,
                None => value,
            };
            let field = self.m.string_index(name);
            self.emit(Instr::DynSet {
                obj,
                field,
                src: value,
            });
        }
        self.coerce(obj, want).map(Some)
    }
}
