//! Renumbering a reloaded program onto the types of the running one.
//!
//! A type is named by its index in a table that is built in the order the
//! compiler first needs each type, so a class added to a program moves every
//! type after it. The running program's table only grows instead: a type the
//! new program shares with it keeps its index, one the new program adds goes
//! after the running ones, and one it drops stays where it is, because
//! objects of it and bodies that name it may still be live.
//!
//! Types are matched by what survives an edit: a class, an enum and an
//! abstract by name, the rest by what they are made of.

use crate::bytecode::DecodedBytecode;
use crate::hl;
use crate::opcodes::Opcode;
use crate::types::{HLType, HLTypeFun, TypeRef};
use std::collections::{HashMap, VecDeque};
use std::hash::{Hash, Hasher};

/// How deep a type made of types is followed when two programs' types are
/// compared. A function type of function types is the deepest a program
/// goes in practice.
const DEPTH: usize = 8;

/// What renumbering the types of a program did.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct Retyped {
    /// Types the new program has and the running one did not.
    pub appended: usize,
    /// Positions of running types the new program no longer has; their slots
    /// hold placeholders until [`restore_dropped`].
    dropped: Vec<usize>,
}

impl Retyped {
    /// Positions of the running types the new program no longer has.
    pub fn dropped(&self) -> Vec<usize> {
        self.dropped.clone()
    }
}

/// The kind and name of a type, without anything it is made of.
fn label(t: &HLType) -> String {
    match t.kind {
        hl::hl_type_kind_HOBJ | hl::hl_type_kind_HSTRUCT => {
            let o = t.obj.as_ref();
            let name = o.map_or("", |o| o.name.as_str());
            if name.is_empty() {
                // An anonymous object is what its fields are called.
                let fields = o.map_or(String::new(), |o| {
                    o.fields
                        .iter()
                        .map(|f| f.name.as_str())
                        .collect::<Vec<_>>()
                        .join(",")
                });
                format!("{}:{{{fields}}}", t.kind)
            } else {
                format!("{}:{name}", t.kind)
            }
        }
        hl::hl_type_kind_HENUM => {
            format!("{}:{}", t.kind, t.tenum.as_ref().map_or("", |e| e.name.as_str()))
        }
        hl::hl_type_kind_HABSTRACT => {
            format!("{}:{}", t.kind, t.abs_name.as_deref().unwrap_or(""))
        }
        hl::hl_type_kind_HVIRTUAL => {
            let fields = t.virt.as_ref().map_or(String::new(), |v| {
                v.fields
                    .iter()
                    .map(|f| f.name.as_str())
                    .collect::<Vec<_>>()
                    .join(",")
            });
            format!("{}:{{{fields}}}", t.kind)
        }
        kind => kind.to_string(),
    }
}

/// The types a type is made of, which tell apart two types of one label:
/// those of a function, an anonymous object, a reference. A class, an enum
/// and an abstract are told apart by their names.
fn parts(t: &HLType) -> Vec<usize> {
    match t.kind {
        hl::hl_type_kind_HFUN | hl::hl_type_kind_HMETHOD => t.fun.as_ref().map_or(Vec::new(), |f| {
            f.args.iter().map(|a| a.0).chain([f.ret.0]).collect()
        }),
        hl::hl_type_kind_HVIRTUAL => t
            .virt
            .as_ref()
            .map_or(Vec::new(), |v| v.fields.iter().map(|f| f.type_.0).collect()),
        hl::hl_type_kind_HREF | hl::hl_type_kind_HNULL | hl::hl_type_kind_HPACKED => {
            t.tparam.iter().map(|p| p.0).collect()
        }
        hl::hl_type_kind_HOBJ | hl::hl_type_kind_HSTRUCT
            if t.obj.as_ref().is_some_and(|o| o.name.is_empty()) =>
        {
            t.obj
                .iter()
                .flat_map(|o| o.fields.iter().map(|f| f.type_.0))
                .collect()
        }
        _ => Vec::new(),
    }
}

fn hash_of(parts: impl Hash) -> u64 {
    let mut h = std::collections::hash_map::DefaultHasher::new();
    parts.hash(&mut h);
    h.finish()
}

/// A number for each type that is the same in two programs when the type is:
/// its label, and for the ones made of types, theirs to a depth of `DEPTH`.
fn keys(types: &[HLType]) -> Vec<u64> {
    let labels: Vec<u64> = types.iter().map(|t| hash_of(label(t))).collect();
    let parts: Vec<Vec<usize>> = types.iter().map(parts).collect();
    let mut keys = labels.clone();
    for _ in 0..DEPTH {
        keys = (0..types.len())
            .map(|i| {
                let mut h = std::collections::hash_map::DefaultHasher::new();
                labels[i].hash(&mut h);
                for &p in &parts[i] {
                    keys.get(p).hash(&mut h);
                }
                h.finish()
            })
            .collect();
    }
    keys
}

/// `t` with every type it names renumbered by `at`.
fn renumber(t: &mut HLType, at: &dyn Fn(usize) -> usize) {
    let map = |r: &mut TypeRef| r.0 = at(r.0);
    fn fun(f: &mut HLTypeFun, map: &dyn Fn(&mut TypeRef)) {
        f.args.iter_mut().for_each(map);
        map(&mut f.ret);
        f.parent.iter_mut().for_each(map);
        f.closure_type.iter_mut().for_each(map);
        if let Some(c) = f.closure.as_mut() {
            fun(c, map);
        }
    }
    t.tparam.iter_mut().for_each(map);
    if let Some(o) = t.obj.as_mut() {
        o.super_.iter_mut().for_each(map);
        o.fields.iter_mut().for_each(|f| map(&mut f.type_));
    }
    if let Some(f) = t.fun.as_mut() {
        fun(f, &map);
    }
    if let Some(e) = t.tenum.as_mut() {
        for c in &mut e.constructs {
            c.params.iter_mut().for_each(map);
        }
    }
    if let Some(v) = t.virt.as_mut() {
        v.fields.iter_mut().for_each(|f| map(&mut f.type_));
        map(&mut v.lookup.t);
    }
}

/// Why `new` cannot stand for `old`, two types matched by name that are laid
/// out differently.
fn incompatible(old: &HLType, new: &HLType) -> Option<String> {
    let same_types = |a: &[TypeRef], b: &[TypeRef]| {
        a.len() == b.len() && a.iter().zip(b).all(|(x, y)| x.0 == y.0)
    };
    if let (Some(o), Some(n)) = (&old.obj, &new.obj) {
        // The fields of a class are the field pass's to match.
        if o.super_.as_ref().map(|s| s.0) != n.super_.as_ref().map(|s| s.0) {
            return Some(format!(
                "the superclass of {} changed; live objects are laid out by the old one",
                o.name
            ));
        }
        // The methods of a class are the slot pass's to match; the statics a
        // class binds are fields of its `$` object, which is laid out like any.
        let statics = |o: &crate::types::HLTypeObj| -> Vec<i32> {
            o.bindings.iter().step_by(2).copied().collect()
        };
        if statics(o) != statics(n) {
            return Some(format!(
                "{} binds different statics; a static function was added or removed",
                o.name
            ));
        }
    }
    if let (Some(o), Some(n)) = (&old.tenum, &new.tenum) {
        let same = o.constructs.len() == n.constructs.len()
            && o.constructs
                .iter()
                .zip(&n.constructs)
                .all(|(a, b)| a.name == b.name && same_types(&a.params, &b.params));
        if !same {
            return Some(format!("the constructors of enum {} changed", o.name));
        }
    }
    None
}

/// Renumber the types `new` names onto the table of `old`, the running
/// program. A type `old` has stays where it is, and `new.types` has the
/// types it appends after them. A type of `old` that `new` no longer has
/// holds a placeholder until [`restore_dropped`] puts it back, which is
/// after the globals have been renumbered: a type that is not in `new` is
/// on the running program's numbering of them already.
///
/// `Err` says why `new` cannot stand on the running types: a class, an enum
/// or a method set that was laid out another way.
pub fn remap_types(old: &DecodedBytecode, new: &mut DecodedBytecode) -> Result<Retyped, String> {
    let (old_keys, new_keys) = (keys(&old.types), keys(&new.types));
    let mut running: HashMap<u64, VecDeque<usize>> = HashMap::new();
    for (i, k) in old_keys.iter().enumerate() {
        running.entry(*k).or_default().push_back(i);
    }
    let mut next = old.types.len();
    let map: Vec<usize> = new_keys
        .iter()
        .map(|k| match running.get_mut(k).and_then(VecDeque::pop_front) {
            Some(slot) => slot,
            None => {
                next += 1;
                next - 1
            }
        })
        .collect();
    if map.iter().enumerate().all(|(i, &m)| i == m) && new.types.len() == old.types.len() {
        // Nothing moves. Each type still has to be laid out as the running one.
        for (running, t) in old.types.iter().zip(&new.types) {
            if let Some(why) = incompatible(running, t) {
                return Err(why);
            }
        }
        return Ok(Retyped::default());
    }
    let at = |t: usize| map.get(t).copied().unwrap_or(t);

    // Constants name a type by index in their HTYPE fields, read against the
    // table they came with.
    let mut constants = std::mem::take(&mut new.constants);
    for c in &mut constants {
        let types: Vec<usize> = super::constant_fields(new, c)
            .filter(|(_, kind)| *kind == hl::hl_type_kind_HTYPE)
            .map(|(j, _)| j)
            .collect();
        for j in types {
            c.fields[j] = at(c.fields[j] as usize) as i32;
        }
    }
    new.constants = constants;

    let mut table: Vec<Option<HLType>> = (0..next).map(|_| None).collect();
    for (i, mut t) in std::mem::take(&mut new.types).into_iter().enumerate() {
        renumber(&mut t, &at);
        table[map[i]] = Some(t);
    }
    for (slot, t) in table.iter().enumerate().take(old.types.len()) {
        if let Some(t) = t {
            if let Some(why) = incompatible(&old.types[slot], t) {
                return Err(why);
            }
        }
    }
    let dropped: Vec<usize> = (0..old.types.len()).filter(|&i| table[i].is_none()).collect();
    new.types = table
        .into_iter()
        .map(|t| t.unwrap_or_default())
        .collect();

    for g in &mut new.globals {
        g.0 = at(g.0);
    }
    for n in &mut new.natives {
        n.type_.0 = at(n.type_.0);
    }
    for entry in &mut new.host_classes {
        entry.type_index = at(entry.type_index);
        entry.companion_index = at(entry.companion_index);
    }
    for f in &mut new.functions {
        f.type_.0 = at(f.type_.0);
        f.regs.iter_mut().for_each(|r| r.0 = at(r.0));
        if f.ops().iter().any(|op| matches!(op, Opcode::Type { ty, .. } if at(ty.0) != ty.0)) {
            for op in f.ops_mut() {
                if let Opcode::Type { ty, .. } = op {
                    ty.0 = at(ty.0);
                }
            }
        }
    }
    new.body_facts = std::sync::OnceLock::new();
    Ok(Retyped {
        appended: next - old.types.len(),
        dropped,
    })
}

/// Put back the running types the new program no longer has, in the slots
/// [`remap_types`] left for them.
pub fn restore_dropped(old: &DecodedBytecode, new: &mut DecodedBytecode, retyped: &Retyped) {
    for &i in &retyped.dropped {
        new.types[i] = old.types[i].clone();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::opcodes::{RefType, Reg};
    use crate::types::{HLFunction, HLObjField, HLTypeFun, HLTypeObj};

    const I32: usize = 0;

    fn prim(kind: hl::hl_type_kind) -> HLType {
        HLType {
            kind,
            ..Default::default()
        }
    }

    fn class(name: &str, fields: &[(&str, usize)]) -> HLType {
        HLType {
            kind: hl::hl_type_kind_HOBJ,
            obj: Some(HLTypeObj {
                name: name.into(),
                fields: fields
                    .iter()
                    .map(|(n, t)| HLObjField {
                        name: (*n).into(),
                        type_: TypeRef(*t),
                        hashed_name: 0,
                    })
                    .collect(),
                ..Default::default()
            }),
            ..Default::default()
        }
    }

    fn fun(args: &[usize], ret: usize) -> HLType {
        HLType {
            kind: hl::hl_type_kind_HFUN,
            fun: Some(HLTypeFun {
                args: args.iter().map(|a| TypeRef(*a)).collect(),
                ret: TypeRef(ret),
                ..Default::default()
            }),
            ..Default::default()
        }
    }

    /// A program of these types with one function whose registers are
    /// `regs`, the first of them loaded as a type by a `Type` op.
    fn program(types: Vec<HLType>, regs: &[usize], globals: &[usize]) -> DecodedBytecode {
        let mut f = HLFunction::with_body(
            vec![Opcode::Type {
                dst: Reg(0),
                ty: RefType(regs[0]),
            }],
            Vec::new(),
        );
        f.regs = regs.iter().map(|r| TypeRef(*r)).collect();
        f.type_ = TypeRef(regs[0]);
        DecodedBytecode {
            types,
            functions: vec![f],
            globals: globals.iter().map(|g| TypeRef(*g)).collect(),
            ..Default::default()
        }
    }

    /// A class added ahead of the others moves every type after it in the
    /// new program. They stay where they were, and it goes after them.
    #[test]
    fn a_class_added_in_the_middle_goes_after_the_running_types() {
        let old = program(
            vec![prim(hl::hl_type_kind_HI32), class("A", &[("n", I32)]), class("B", &[])],
            &[2, 1],
            &[1, 2],
        );
        let mut new = program(
            vec![
                prim(hl::hl_type_kind_HI32),
                class("A", &[("n", I32)]),
                class("New", &[("m", I32)]),
                class("B", &[]),
            ],
            &[3, 1],
            &[1, 2, 3],
        );
        let done = remap_types(&old, &mut new).unwrap();
        assert_eq!(done.appended, 1);
        let name = |i: usize| new.types[i].obj.as_ref().unwrap().name.clone();
        assert_eq!((name(1), name(2), name(3)), ("A".into(), "B".into(), "New".into()));
        // B was 3 and is 2 again; the new class is 3.
        assert_eq!(new.functions[0].regs.iter().map(|r| r.0).collect::<Vec<_>>(), vec![2, 1]);
        assert_eq!(new.functions[0].type_.0, 2);
        let Opcode::Type { ty, .. } = &new.functions[0].ops()[0] else {
            panic!()
        };
        assert_eq!(ty.0, 2);
        assert_eq!(new.globals.iter().map(|g| g.0).collect::<Vec<_>>(), vec![1, 3, 2]);
    }

    /// A function type is made of types, so it is matched by what they are
    /// after they are matched themselves, and one with a new signature is
    /// added.
    #[test]
    fn function_types_are_matched_by_what_they_are_made_of() {
        let old = program(
            vec![
                prim(hl::hl_type_kind_HI32),
                prim(hl::hl_type_kind_HF64),
                fun(&[0], 1),
                class("A", &[]),
                fun(&[3], 0),
            ],
            &[4, 2],
            &[],
        );
        // The new program puts a `(Float) -> Int` ahead of the others.
        let mut new = program(
            vec![
                prim(hl::hl_type_kind_HI32),
                prim(hl::hl_type_kind_HF64),
                fun(&[1], 0),
                fun(&[0], 1),
                class("A", &[]),
                fun(&[4], 0),
            ],
            &[5, 3, 2],
            &[],
        );
        let done = remap_types(&old, &mut new).unwrap();
        assert_eq!(done.appended, 1);
        // `(A) -> Int` and `(Int) -> Float` are where they were; the new
        // signature is last.
        assert_eq!(new.functions[0].regs.iter().map(|r| r.0).collect::<Vec<_>>(), vec![4, 2, 5]);
        let made = new.types[5].fun.as_ref().unwrap();
        assert_eq!((made.args[0].0, made.ret.0), (1, 0));
    }

    /// Which fields a class may have is for the field pass to say.
    #[test]
    fn a_class_that_gained_a_field_is_matched() {
        let old = program(vec![prim(hl::hl_type_kind_HI32), class("A", &[("n", I32)])], &[1], &[]);
        let mut new = program(
            vec![prim(hl::hl_type_kind_HI32), class("A", &[("n", I32), ("m", I32)])],
            &[1],
            &[],
        );
        assert_eq!(remap_types(&old, &mut new).unwrap(), Retyped::default());
        assert_eq!(new.types[1].obj.as_ref().unwrap().fields.len(), 2);
    }

    #[test]
    fn a_class_that_changed_its_superclass_is_refused() {
        let extends = |parent: usize| {
            let mut b = class("B", &[]);
            b.obj.as_mut().unwrap().super_ = Some(TypeRef(parent));
            b
        };
        let types = |parent: usize| {
            vec![prim(hl::hl_type_kind_HI32), class("A", &[]), class("C", &[]), extends(parent)]
        };
        let old = program(types(1), &[3], &[]);
        let mut new = program(types(2), &[3], &[]);
        let why = remap_types(&old, &mut new).unwrap_err();
        assert!(why.contains("superclass of B changed"), "{why}");
    }

    /// A type the new program no longer has stays, so a function that was
    /// dropped with it still has its registers' types.
    #[test]
    fn a_dropped_type_is_put_back() {
        let old = program(
            vec![prim(hl::hl_type_kind_HI32), class("Gone", &[("n", I32)]), class("A", &[])],
            &[2],
            &[],
        );
        let mut new = program(vec![prim(hl::hl_type_kind_HI32), class("A", &[])], &[1], &[]);
        let done = remap_types(&old, &mut new).unwrap();
        restore_dropped(&old, &mut new, &done);
        assert_eq!(new.types.len(), 3);
        assert_eq!(new.types[1].obj.as_ref().unwrap().name, "Gone");
        assert_eq!(new.types[2].obj.as_ref().unwrap().name, "A");
        assert_eq!(new.functions[0].regs[0].0, 2);
    }

    #[test]
    fn the_same_types_are_left_alone() {
        let types = || vec![prim(hl::hl_type_kind_HI32), class("A", &[]), fun(&[1], 0)];
        let old = program(types(), &[2], &[1]);
        let mut new = program(types(), &[2], &[1]);
        assert_eq!(remap_types(&old, &mut new).unwrap(), Retyped::default());
        assert_eq!(new.functions[0].regs[0].0, 2);
    }
}
