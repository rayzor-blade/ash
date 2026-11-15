//! Compares the type table computed from a program with the one a `.hl` file carries.

use std::collections::HashSet;

use ash_tast::Tast;
use ash_tast::format::module_kind;

use crate::hl::{EnumType, HlType, Proto, TypeTable};

/// Names of the types the program defines: its classes with their statics objects, and its enums.
#[derive(Default)]
pub struct ProgramNames {
    pub objects: HashSet<String>,
    pub enums: HashSet<String>,
}

pub fn program_names(tast: &Tast) -> ProgramNames {
    let mut names = ProgramNames::default();
    for o in tast.order() {
        if o.kind.get() == u32::from(module_kind::CLASS) {
            if let Some(name) = tast.opt_string(tast.classes()[o.id.get() as usize].name) {
                names.objects.insert(match name.rsplit_once('.') {
                    Some((pack, last)) => format!("{pack}.${last}"),
                    None => format!("${name}"),
                });
                names.objects.insert(name.to_owned());
            }
        } else if o.kind.get() == u32::from(module_kind::ENUM)
            && let Some(name) = tast.opt_string(tast.enums()[o.id.get() as usize].name)
        {
            names.enums.insert(name.to_owned());
            // the object that holds an enum's statics
            names.objects.insert(match name.rsplit_once('.') {
                Some((pack, last)) => format!("{pack}.${last}"),
                None => format!("${name}"),
            });
        }
    }
    names
}

/// What differs between the table `genhl` wrote and the one computed from the program. Types
/// the generator creates only while compiling a function body, which this crate does not do
/// yet, are counted apart in the second number.
pub fn differences(
    ours: &TypeTable,
    theirs: &TypeTable,
    program: &ProgramNames,
) -> (usize, usize, Vec<String>) {
    let mut out = Vec::new();
    let mut compared = 0;
    let mut lazy = 0;
    let super_name = |t: &TypeTable, s: Option<usize>| -> Option<String> {
        s.map(|ix| match &t.types[ix] {
            HlType::Obj(o) | HlType::Struct(o) => t.objs[*o].name.clone(),
            other => format!("{other:?}"),
        })
    };
    for o in &theirs.objs {
        let Some(mi) = ours.obj_named(&o.name) else {
            if program.objects.contains(&o.name) {
                out.push(format!("{}: not computed", o.name));
            } else {
                lazy += 1;
            }
            continue;
        };
        compared += 1;
        let m = &ours.objs[mi];
        let n = &o.name;
        if m.is_struct != o.is_struct {
            out.push(format!(
                "{n}: struct {} against {}",
                m.is_struct, o.is_struct
            ));
        }
        if super_name(ours, m.super_) != super_name(theirs, o.super_) {
            out.push(format!(
                "{n}: super {:?} against {:?}",
                super_name(ours, m.super_),
                super_name(theirs, o.super_)
            ));
        }
        if m.global.is_some() != o.global.is_some() {
            out.push(format!("{n}: global {:?} against {:?}", m.global, o.global));
        }
        let fields = |t: &TypeTable, f: &[(String, usize)]| -> Vec<String> {
            f.iter()
                .map(|(name, ty)| format!("{name}:{}", t.describe(*ty)))
                .collect()
        };
        let (mf, tf) = (fields(ours, &m.fields), fields(theirs, &o.fields));
        if mf != tf {
            out.push(format!("{n}: fields\n    ours   {mf:?}\n    genhl  {tf:?}"));
        }
        let proto = |p: &[Proto]| -> Vec<(String, Option<u32>)> {
            p.iter().map(|x| (x.name.clone(), x.vslot)).collect()
        };
        if proto(&m.proto) != proto(&o.proto) {
            out.push(format!(
                "{n}: proto\n    ours   {:?}\n    genhl  {:?}",
                proto(&m.proto),
                proto(&o.proto)
            ));
        }
        let bound = |b: &[(u32, u32)]| -> Vec<u32> { b.iter().map(|x| x.0).collect() };
        if bound(&m.bindings) != bound(&o.bindings) {
            out.push(format!(
                "{n}: bindings {:?} against {:?}",
                bound(&m.bindings),
                bound(&o.bindings)
            ));
        }
    }
    for e in &theirs.enums {
        let Some(m) = ours.enums.iter().find(|x| x.name == e.name) else {
            if program.enums.contains(&e.name) {
                out.push(format!("enum {}: not computed", e.name));
            } else {
                lazy += 1;
            }
            continue;
        };
        compared += 1;
        let ctors = |t: &TypeTable, e: &EnumType| -> Vec<String> {
            e.fields
                .iter()
                .map(|(n, a)| {
                    let a: Vec<String> = a.iter().map(|x| t.describe(*x)).collect();
                    format!("{n}({})", a.join(","))
                })
                .collect()
        };
        if ctors(ours, m) != ctors(theirs, e) {
            out.push(format!(
                "enum {}: {:?} against {:?}",
                e.name,
                ctors(ours, m),
                ctors(theirs, e)
            ));
        }
        if m.global.is_some() != e.global.is_some() {
            out.push(format!("enum {}: global", e.name));
        }
    }
    (compared, lazy, out)
}
