//! Where an object's variables live, and which function a method call reaches.
//!
//! The class hierarchy is closed: every class of the program is in the file, so the classes that
//! can override a method are known. That is what lets a call that HashLink would send through the
//! method table be one direct call, when no class below the receiver's overrides the method.

use std::collections::HashMap;

use ash_tast::format::*;

use super::{Result, TypeBuilder};
use crate::hl::*;

/// The place of an instance variable in an object: see [`TypeBuilder::field_slot`].
#[derive(Clone, Copy, Debug)]
pub struct FieldSlot {
    pub owner: TypeIx,
    pub index: u32,
    pub ty: TypeIx,
}

/// What a call of a method reaches, given the class of the object it is called on.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Resolution {
    /// One function, whatever class the object has at run time.
    One(u32),
    /// Several: the call goes through the method table at `slot`, which may name any of `targets`.
    Many { slot: u32, targets: Vec<u32> },
}

impl<'a> TypeBuilder<'a> {
    /// Where the instance variable `field` lives: the class that declares it, its index in the
    /// object layout (a subclass's layout continues after its superclass's, so the index is the
    /// same through every subclass) and its type.
    ///
    /// The declaring class names the slot, whatever type the object has where it is used, so
    /// that a write through a subclass and a read through the base are seen to be one location.
    pub fn field_slot(&mut self, field: u32) -> Result<FieldSlot> {
        let owner = self.tast.fields()[field as usize].owner.get();
        let name = self.field_name(field).to_owned();
        let class = self.class_name(owner).to_owned();
        let t = self.class_type(owner, &[], false, None)?;
        let (HlType::Obj(o) | HlType::Struct(o)) = self.table.types[t].clone() else {
            return Err(format!("{class} is not an object type"));
        };
        let &(index, ty) = self.table.objs[o]
            .index
            .get(&name)
            .ok_or_else(|| format!("{class}.{name} has no slot in the layout"))?;
        if index < 0 {
            return Err(format!("{class}.{name} is a method, not a variable"));
        }
        Ok(FieldSlot {
            owner: t,
            index: index as u32,
            ty,
        })
    }

    /// The field `name` that `class` itself declares, found in the file's field list.
    pub fn declared_field(&self, class: u32, name: &str) -> Option<u32> {
        let rec = self.tast.classes()[class as usize];
        (rec.fields_first.get()..rec.fields_first.get() + rec.fields_count.get())
            .find(|f| self.field_name(*f) == name)
    }

    /// The constructor of `class`, if it has one.
    pub fn constructor(&self, class: u32) -> Option<u32> {
        self.classes[class as usize].ctor
    }

    /// The slot of the instance variable `name` in objects of type `ty`, found through that
    /// type's layout and those of its superclasses: a class's index holds the names it declares.
    /// An extern class such as `Array<T>` stands for the class its receiver's type names, so its
    /// variables are found this way. The class that declares the name names the slot, as in
    /// [`Self::field_slot`].
    pub fn slot_in(&self, ty: TypeIx, name: &str) -> Option<FieldSlot> {
        let mut at = Some(ty);
        while let Some(t) = at {
            let (HlType::Obj(o) | HlType::Struct(o)) = self.table.types[t] else {
                return None;
            };
            if let Some(&(index, field_ty)) = self.table.objs[o].index.get(name) {
                return (index >= 0).then_some(FieldSlot {
                    owner: t,
                    index: index as u32,
                    ty: field_ty,
                });
            }
            at = self.table.objs[o].super_;
        }
        None
    }

    /// [`Self::field_slot`] for a class whose type is already built; `None` for one that is not,
    /// so that asking adds nothing to the table.
    pub fn built_field_slot(&self, field: u32) -> Option<FieldSlot> {
        let owner = self.tast.fields()[field as usize].owner.get();
        let t = *self.cached.get(self.class_name(owner))?;
        let (HlType::Obj(o) | HlType::Struct(o)) = self.table.types[t] else {
            return None;
        };
        let &(index, ty) = self.table.objs[o].index.get(self.field_name(field))?;
        (index >= 0).then_some(FieldSlot {
            owner: t,
            index: index as u32,
            ty,
        })
    }

    /// Whether the class is declared outside the program, so that it has no body to lower.
    pub fn is_extern_class(&self, class: u32) -> bool {
        self.is_extern(class)
    }

    /// The superclass of `class`, if it has one.
    pub fn superclass(&self, class: u32) -> Option<u32> {
        self.super_class(class)
    }

    /// The class an object type is the layout of.
    pub fn class_of_object(&self, t: TypeIx) -> Option<u32> {
        match &self.table.types[t] {
            HlType::Obj(o) => self.by_name.get(&self.table.objs[*o].name).copied(),
            _ => None,
        }
    }

    /// Whether an object of type `sub` can stand where one of type `sup` is expected.
    pub fn is_subclass_type(&self, sub: TypeIx, sup: TypeIx) -> bool {
        let mut at = Some(sub);
        while let Some(t) = at {
            if t == sup {
                return true;
            }
            at = match self.table.types[t] {
                HlType::Obj(o) | HlType::Struct(o) => self.table.objs[o].super_,
                _ => None,
            };
        }
        false
    }

    /// Records each class under its superclass.
    pub(super) fn find_children(&mut self) {
        let mut children: HashMap<u32, Vec<u32>> = HashMap::new();
        for c in 0..self.classes.len() as u32 {
            if let Some(sup) = self.super_class(c) {
                children.entry(sup).or_default().push(c);
            }
        }
        self.children = children;
    }

    /// The classes below `class`, however deep.
    fn descendants(&self, class: u32) -> Vec<u32> {
        let mut out = Vec::new();
        let mut todo = vec![class];
        while let Some(c) = todo.pop() {
            for &below in self.children.get(&c).into_iter().flatten() {
                out.push(below);
                todo.push(below);
            }
        }
        out
    }

    /// The method `name` that `class` itself declares.
    fn declared_method(&self, class: u32, name: &str) -> Option<u32> {
        self.classes[class as usize]
            .instance
            .iter()
            .copied()
            .find(|f| self.field_name(*f) == name && self.field_flags(*f) & field_flag::METHOD != 0)
    }

    /// Which function a call of `method` reaches on an object whose class is `receiver` or one
    /// below it: the nearest implementation at or above `receiver`, unless a class below
    /// `receiver` overrides it, when the call needs the method table and every implementation
    /// that table may name.
    ///
    /// This assumes the program is all there is: a class added after the file was made (loaded,
    /// or reloaded into a running program) would add an implementation this does not see.
    pub fn resolve_method(&mut self, receiver: u32, method: u32) -> Result<Resolution> {
        let name = self.field_name(method).to_owned();
        let class_name = self.classes[receiver as usize].name.clone();
        let mut at = Some(receiver);
        let mut implementation = None;
        while let Some(c) = at {
            if self.is_extern(c) {
                return Err(format!(
                    "{}: the class is declared outside the program",
                    self.classes[c as usize].name
                ));
            }
            if let Some(f) = self.declared_method(c, &name) {
                implementation = Some(f);
                break;
            }
            at = self.super_class(c);
        }
        let Some(implementation) = implementation else {
            return Err(format!("{class_name}.{name} has no implementation to call"));
        };
        let overriders: Vec<u32> = self
            .descendants(receiver)
            .into_iter()
            .filter_map(|d| self.declared_method(d, &name))
            .collect();
        for f in std::iter::once(&implementation).chain(&overriders) {
            if self.field_flags(*f) & (field_flag::DYNAMIC | field_flag::EXTERN) != 0 {
                return Err(format!("{class_name}.{name} is not a method with a body"));
            }
        }
        if overriders.is_empty() {
            return Ok(Resolution::One(implementation));
        }
        let t = self.class_type(receiver, &[], false, None)?;
        let (HlType::Obj(o) | HlType::Struct(o)) = self.table.types[t].clone() else {
            return Err(format!("{class_name} is not an object type"));
        };
        let (index, _) = self
            .get_index(o, &name)
            .ok_or_else(|| format!("{class_name}.{name} has no slot in the method table"))?;
        if index >= 0 {
            return Err(format!("{class_name}.{name} is a variable, not a method"));
        }
        let mut targets = vec![implementation];
        targets.extend(overriders);
        Ok(Resolution::Many {
            slot: (-index - 1) as u32,
            targets,
        })
    }
}
