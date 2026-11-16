//! Calls of methods: the arguments, and which function an `object.method(...)` reaches.

use air::v2::ir::*;
use ash_tast::format::*;

use super::{FnLower, Result, THIS};
use crate::build::Resolution;
use crate::hl::{HlType, TypeIx};

impl<'m, 'a> FnLower<'m, 'a> {
    /// The arguments of a call of `field`, converted to its parameters, behind `lead`.
    pub(super) fn call_args(
        &mut self,
        field: u32,
        lead: Option<ValueId>,
        ids: &[u32],
    ) -> Result<Option<(Vec<ValueId>, TypeIx)>> {
        let rec = self.tast.fields()[field as usize];
        let name = self.m.types.field_name(field).to_owned();
        let sig = self.m.types.to_type(rec.ty.get())?;
        let HlType::Fun(params, ret) = self.m.types.table.types[sig].clone() else {
            return Err(format!("{name} is not a function"));
        };
        if params.len() != ids.len() {
            return Err(format!(
                "{name}: {} arguments for {} parameters",
                ids.len(),
                params.len()
            ));
        }
        let mut args: Vec<ValueId> = lead.into_iter().collect();
        for (k, p) in ids.iter().zip(&params) {
            let Some(v) = self.expr(*k)? else {
                return Ok(None);
            };
            args.push(self.coerce(v, *p)?);
        }
        Ok(Some((args, ret)))
    }

    /// The value `this` holds in the method being lowered.
    fn this_value(&mut self) -> Result<ValueId> {
        if self.this_ty.is_none() {
            return Err("`this` outside a method".into());
        }
        let b = self.cur.ok_or("a call in unreachable code")?;
        self.read_var(THIS, b)
    }

    /// A call of the function `field` holds, on `obj`.
    fn direct_call(&mut self, field: u32, obj: ValueId, ids: &[u32]) -> Result<Option<ValueId>> {
        let rec = self.tast.fields()[field as usize];
        let class = self.m.types.class_name(rec.owner.get()).to_owned();
        let name = self.m.types.field_name(field).to_owned();
        if rec.func.get() == NONE {
            return Err(format!("{class}.{name} has no body to lower"));
        }
        let fun = self.m.types.fun_id(&class, &name) as usize;
        self.m.called.insert(fun as u32);
        self.m.request(field);
        let Some((args, ret)) = self.call_args(field, Some(obj), ids)? else {
            return Ok(None);
        };
        let dst = self.value(ret);
        self.emit(Instr::Call { dst, fun, args });
        Ok((!self.is_void(ret)).then_some(dst))
    }

    /// `object.method(arguments)`. The class hierarchy says which function that is: one, when no
    /// class below the object's overrides the method, so it is a direct call even where
    /// HashLink would go through the method table; otherwise the method table, with every
    /// function it may name lowered.
    pub(super) fn method_call(&mut self, ids: &[u32], callee: &NodeRec) -> Result<Option<ValueId>> {
        let field = callee.b.get();
        if field == NONE {
            return Err("a call of a field the file does not name".into());
        }
        let rec = self.tast.fields()[field as usize];
        let class = self.m.types.class_name(rec.owner.get()).to_owned();
        let name = self.m.types.field_name(field).to_owned();
        let flags = rec.flags.get();
        if flags & field_flag::METHOD == 0
            || flags & (field_flag::DYNAMIC | field_flag::EXTERN) != 0
        {
            return Err(format!("{class}.{name} is not a method with a body"));
        }
        let receiver = *self.tast.node(self.kid(callee, 0));
        // `super.method(...)` is the superclass's own function, whatever the object's class
        if receiver.kind == node_kind::CONST && receiver.flags == const_kind::SUPER {
            let this = self.this_value()?;
            return self.direct_call(field, this, &ids[1..]);
        }
        let Some(obj) = self.expr(self.kid(callee, 0))? else {
            return Ok(None);
        };
        let held = self.val_ty(obj);
        let Some(receiver_class) = self.m.types.class_of_object(held) else {
            return Err(format!(
                "{class}.{name}: a call on {:?} is not lowered yet",
                self.m.types.table.types[held]
            ));
        };
        match self.m.types.resolve_method(receiver_class, field)? {
            Resolution::One(target) => self.direct_call(target, obj, &ids[1..]),
            Resolution::Many { slot, targets } => {
                let mut funs = Vec::new();
                for target in targets {
                    let t = self.tast.fields()[target as usize];
                    if t.func.get() == NONE {
                        return Err(format!("{class}.{name}: an implementation has no body"));
                    }
                    let owner = self.m.types.class_name(t.owner.get()).to_owned();
                    let fun = self.m.types.fun_id(&owner, &name);
                    self.m.called.insert(fun);
                    self.m.request(target);
                    funs.push(fun as usize);
                }
                self.m
                    .table_calls
                    .insert((TypeRef(held as u32), slot as usize), funs);
                let Some((args, ret)) = self.call_args(field, Some(obj), &ids[1..])? else {
                    return Ok(None);
                };
                let dst = self.value(ret);
                self.emit(Instr::CallMethod {
                    dst,
                    field: slot as usize,
                    args,
                });
                Ok((!self.is_void(ret)).then_some(dst))
            }
        }
    }

    /// A call of the method `name` of the class `class`, on `obj` and values already evaluated: the
    /// function the class hierarchy says, or the method table.
    pub(super) fn call_known_method(
        &mut self,
        obj: ValueId,
        class: &str,
        name: &str,
        args: &[ValueId],
    ) -> Result<ValueId> {
        let c = self
            .m
            .types
            .class_id(class)
            .ok_or_else(|| format!("the program has no {class} class"))?;
        let field = self
            .m
            .types
            .declared_field(c, name)
            .ok_or_else(|| format!("{class}.{name} is not in the program"))?;
        let rec = self.tast.fields()[field as usize];
        let sig = self.m.types.to_type(rec.ty.get())?;
        let HlType::Fun(params, ret) = self.m.types.table.types[sig].clone() else {
            return Err(format!("{class}.{name} is not a function"));
        };
        let mut all = vec![obj];
        for (a, p) in args.iter().zip(&params) {
            all.push(self.coerce(*a, *p)?);
        }
        let dst = self.value(ret);
        match self.m.types.resolve_method(c, field)? {
            Resolution::One(target) => {
                let t = self.tast.fields()[target as usize];
                let owner = self.m.types.class_name(t.owner.get()).to_owned();
                let fun = self.m.types.fun_id(&owner, name) as usize;
                self.m.called.insert(fun as u32);
                self.m.request(target);
                self.emit(Instr::Call {
                    dst,
                    fun,
                    args: all,
                });
            }
            Resolution::Many { slot, targets } => {
                let held = self.val_ty(obj);
                let mut funs = Vec::new();
                for target in targets {
                    let t = self.tast.fields()[target as usize];
                    let owner = self.m.types.class_name(t.owner.get()).to_owned();
                    let fun = self.m.types.fun_id(&owner, name);
                    self.m.called.insert(fun);
                    self.m.request(target);
                    funs.push(fun as usize);
                }
                self.m
                    .table_calls
                    .insert((TypeRef(held as u32), slot as usize), funs);
                self.emit(Instr::CallMethod {
                    dst,
                    field: slot as usize,
                    args: all,
                });
            }
        }
        Ok(dst)
    }

    /// `super(arguments)` in a constructor: the superclass's constructor, run on this object.
    pub(super) fn super_constructor(&mut self, ids: &[u32]) -> Result<Option<ValueId>> {
        let class = self.this_class.ok_or("`super` outside a method")?;
        let Some(sup) = self.m.types.superclass(class) else {
            return Err("`super` in a class with no superclass".into());
        };
        let sname = self.m.types.class_name(sup).to_owned();
        if self.m.types.is_extern_class(sup) {
            return Err(format!("super: {sname} is declared outside the program"));
        }
        let Some(ctor) = self.m.types.constructor(sup) else {
            return if ids.is_empty() {
                Ok(None)
            } else {
                Err(format!(
                    "super: {sname} has no constructor to take arguments"
                ))
            };
        };
        let this = self.this_value()?;
        self.direct_call(ctor, this, ids)
    }
}
