//! Objects: reading and writing instance variables, and `new`.

use air::v2::ir::*;
use ash_tast::format::*;

use super::{FnLower, Result};
use crate::build::FieldSlot;
use crate::hl::HlType;

impl<'m, 'a> FnLower<'m, 'a> {
    /// An instance variable access: the object, evaluated, and where the variable lives.
    fn field_target(&mut self, n: &NodeRec) -> Result<Option<(ValueId, FieldSlot)>> {
        if n.a.get() == field_access::STATIC {
            return self.static_target(n);
        }
        if n.a.get() == field_access::ANON {
            return self.structure_target(n);
        }
        if n.a.get() != field_access::INSTANCE {
            return Err("only instance, static and structure variables are lowered yet".into());
        }
        let field = n.b.get();
        if field == NONE {
            return Err("a field the file does not name".into());
        }
        let flags = self.tast.fields()[field as usize].flags.get();
        if flags & field_flag::METHOD != 0 {
            return Err("a method used as a value is not lowered yet".into());
        }
        if flags & (field_flag::NOT_PHYSICAL | field_flag::EXTERN | field_flag::DYNAMIC) != 0 {
            let name = self.m.types.field_name(field);
            return Err(format!("{name} has no storage of its own to read or write"));
        }
        let Some(obj) = self.expr(self.kid(n, 0))? else {
            return Ok(None);
        };
        let held = self.val_ty(obj);
        if !matches!(
            self.m.types.table.types[held],
            HlType::Obj(_) | HlType::Struct(_)
        ) {
            return Err(format!(
                "a field of {:?} is not lowered yet",
                self.m.types.table.types[held]
            ));
        }
        let owner = self.tast.fields()[field as usize].owner.get();
        let slot = if self.m.types.is_extern_class(owner) {
            // an extern class stands for the class its receiver's type names
            let name = self.m.types.field_name(field);
            self.m.types.slot_in(held, name).ok_or_else(|| {
                format!("{name} has no slot in the layout of the receiver's class")
            })?
        } else {
            self.m.types.field_slot(field)?
        };
        Ok(Some((obj, slot)))
    }

    /// A field of a structure, found by its name in the structure's fields, which are in the
    /// order of their names.
    fn structure_target(&mut self, n: &NodeRec) -> Result<Option<(ValueId, FieldSlot)>> {
        let name = self.tast.string(n.b.get()).to_owned();
        let Some(obj) = self.expr(self.kid(n, 0))? else {
            return Ok(None);
        };
        let held = self.val_ty(obj);
        let HlType::Virtual(v) = self.m.types.table.types[held] else {
            return Err(format!(
                "the field {name} of {:?} is not lowered yet",
                self.m.types.table.types[held]
            ));
        };
        let fields = &self.m.types.table.virts[v];
        let Some(at) = fields.iter().position(|(f, _)| *f == name) else {
            return Err(format!("{name} is not a field of the structure"));
        };
        let ty = fields[at].1;
        self.emit(Instr::NullCheck { value: obj });
        Ok(Some((
            obj,
            FieldSlot {
                owner: held,
                index: at as u32,
                ty,
            },
        )))
    }

    pub(super) fn field_get(&mut self, n: &NodeRec) -> Result<Option<ValueId>> {
        let Some((obj, slot)) = self.field_target(n)? else {
            return Ok(None);
        };
        Ok(Some(self.load_field(obj, slot)))
    }

    pub(super) fn load_field(&mut self, obj: ValueId, slot: FieldSlot) -> ValueId {
        let dst = self.value(slot.ty);
        self.emit(Instr::FieldGet {
            dst,
            obj,
            obj_ty: TypeRef(slot.owner as u32),
            field: slot.index as usize,
        });
        dst
    }

    /// `++` and `--` on a variable of an object or a class: read, step, write back.
    pub(super) fn step_field(
        &mut self,
        target: &NodeRec,
        up: bool,
        postfix: bool,
    ) -> Result<Option<ValueId>> {
        let Some((obj, slot)) = self.field_target(target)? else {
            return Ok(None);
        };
        let old = self.load_field(obj, slot);
        let new = self.stepped(old, up)?;
        self.emit(Instr::FieldSet {
            obj,
            obj_ty: TypeRef(slot.owner as u32),
            field: slot.index as usize,
            src: new,
        });
        Ok(Some(if postfix { old } else { new }))
    }

    pub(super) fn assign_field(
        &mut self,
        target: &NodeRec,
        value: u32,
        op: Option<&str>,
    ) -> Result<Option<ValueId>> {
        let Some((obj, slot)) = self.field_target(target)? else {
            return Ok(None);
        };
        let Some(rhs) = self.expr(value)? else {
            return Ok(None);
        };
        let new = match op {
            None => self.coerce(rhs, slot.ty)?,
            Some(op) => {
                let old = self.load_field(obj, slot);
                self.arith(op, old, rhs, slot.ty)?
            }
        };
        self.emit(Instr::FieldSet {
            obj,
            obj_ty: TypeRef(slot.owner as u32),
            field: slot.index as usize,
            src: new,
        });
        Ok(Some(new))
    }

    /// `new Class(arguments)`: allocate, then run the constructor on the object.
    pub(super) fn new_object(&mut self, n: &NodeRec) -> Result<Option<ValueId>> {
        let mut class = n.a.get();
        let mut cname = self.m.types.class_name(class).to_owned();
        if self.m.types.is_extern_class(class) {
            if cname != "Array" {
                return Err(format!(
                    "new {cname}: the class is declared outside the program"
                ));
            }
            // `Array<T>` is the class for T's representation, as genhl's `resolve_class` has it
            let t = self.tast.types()[n.ty.get() as usize];
            let args: Vec<u32> = self
                .tast
                .type_args(t.args_first, t.args_count)
                .iter()
                .map(|a| a.get())
                .collect();
            class = self.m.types.resolve_class(class, &args, false)?;
            cname = self.m.types.class_name(class).to_owned();
        }
        let ty = self.m.types.class_type(class, &[], false, None)?;
        if !matches!(self.m.types.table.types[ty], HlType::Obj(_)) {
            return Err(format!("new {cname}: only objects are lowered yet"));
        }
        let ids = self.kids(n);
        let obj = self.value(ty);
        self.emit(Instr::New { dst: obj });
        match self.m.types.constructor(class) {
            Some(ctor) => {
                if self.tast.fields()[ctor as usize].func.get() == NONE {
                    return Err(format!("new {cname}: the constructor has no body to lower"));
                }
                let fun = self.m.types.fun_id(&cname, "new") as usize;
                self.m.called.insert(fun as u32);
                self.m.request(ctor);
                let Some((args, ret)) = self.call_args(ctor, Some(obj), &ids)? else {
                    return Ok(None);
                };
                let dst = self.value(ret);
                self.emit(Instr::Call { dst, fun, args });
            }
            None if ids.is_empty() => {}
            None => {
                return Err(format!(
                    "new {cname}: arguments for a class with no constructor"
                ));
            }
        }
        Ok(Some(obj))
    }
}
