//! Method slots, kept across a reload.
//!
//! A call to a virtual method names a slot of the receiver's class, and the
//! compiler hands the slots out afresh for every program: a method is
//! virtual only when something overrides it, and slots follow declaration
//! order. A new program therefore numbers the slots of a class it shares with
//! the running one in its own way, while frames that began before the reload,
//! and objects made before it, go on using the running numbering.
//!
//! The running numbering wins. A method that has a slot keeps it, one that
//! gains a slot or is new and virtual takes the next slot above every slot of
//! its class and of the running classes that extend it, and an override takes
//! the slot of the method it overrides. The slots in the new program's
//! prototypes and in the bodies that call them are rewritten to match.
//!
//! A method the new program no longer has is dropped, with its function
//! staying behind as a dropped function does, except that one which
//! introduced a slot keeps its prototype entry too: a caller that began before
//! the reload may still call the slot.

use crate::bytecode::DecodedBytecode;
use crate::hl;
use crate::opcodes::Opcode;
use crate::types::{HLFunction, HLObjProto, HLTypeObj};
use std::collections::{HashMap, HashSet};

/// How far up a class's ancestors are followed, so that a table that names
/// itself as its own super cannot loop.
const DEPTH: usize = 64;

/// What the slots of the new program's classes come to in the running
/// program's numbering.
#[derive(Debug, Default)]
pub struct Plan {
    /// For each class, a slot as the new program numbers it and the slot it
    /// has in the running program. Inherited slots are in the subclass's map.
    slots: HashMap<usize, HashMap<i32, i32>>,
    /// The prototype list each class has, in the running numbering: the
    /// methods it had, in their order, then the ones it gains.
    protos: HashMap<usize, Vec<HLObjProto>>,
    /// The prototype entries of removed methods that introduced a slot.
    kept: HashMap<usize, Vec<HLObjProto>>,
    /// Functions of methods that a class both programs have gained, as the
    /// new program numbers them.
    pub added: HashSet<usize>,
    /// Functions of methods that such a class has lost, as the running
    /// program numbers them.
    pub removed: HashSet<usize>,
}

impl Plan {
    /// Whether the plan changes anything: a slot, or the methods of a class.
    pub fn is_empty(&self) -> bool {
        self.added.is_empty()
            && self.removed.is_empty()
            && self.kept.is_empty()
            && self.slots.values().all(|m| m.iter().all(|(a, b)| a == b))
    }
}

fn class(bc: &DecodedBytecode, t: usize) -> Option<&HLTypeObj> {
    let ty = bc.types.get(t)?;
    if ty.kind != hl::hl_type_kind_HOBJ && ty.kind != hl::hl_type_kind_HSTRUCT {
        return None;
    }
    ty.obj.as_ref()
}

/// `t` and its ancestors, nearest first.
fn chain(bc: &DecodedBytecode, t: usize) -> Vec<usize> {
    let mut out = Vec::new();
    let mut at = Some(t);
    while let Some(i) = at {
        if out.len() >= DEPTH || class(bc, i).is_none() {
            break;
        }
        out.push(i);
        at = class(bc, i).and_then(|o| o.super_.as_ref()).map(|s| s.0);
    }
    out
}

/// How many slots the vtable of `t` has: one past the largest of its own and
/// its ancestors'.
fn slot_count(bc: &DecodedBytecode, t: usize) -> i32 {
    chain(bc, t)
        .into_iter()
        .filter_map(|i| class(bc, i))
        .flat_map(|o| o.proto.iter().map(|p| p.pindex + 1))
        .max()
        .unwrap_or(0)
        .max(0)
}

/// Whether an ancestor of `t` has a method in `slot`, which makes a method of
/// `t` in that slot an override. A slot below the parent's count is not enough
/// to say so: a method given a slot above its subclasses' leaves their own
/// slots below it.
fn inherited_slot(bc: &DecodedBytecode, t: usize, slot: i32) -> bool {
    slot >= 0
        && chain(bc, t)
            .into_iter()
            .skip(1)
            .filter_map(|a| class(bc, a))
            .any(|o| o.proto.iter().any(|p| p.pindex == slot))
}

fn super_of(bc: &DecodedBytecode, t: usize) -> Option<usize> {
    class(bc, t)?.super_.as_ref().map(|s| s.0)
}

/// Work out the running slots of the classes of `new`, which has been put on
/// the types of `old` already. `dropped` are the positions of the running
/// types the new program no longer has.
pub fn plan(old: &DecodedBytecode, new: &DecodedBytecode, dropped: &[usize]) -> Result<Plan, String> {
    let dropped: HashSet<usize> = dropped.iter().copied().collect();
    let kept = |t: usize| t < old.types.len() && !dropped.contains(&t) && class(old, t).is_some();

    // The slots the running classes use, and the largest vtable among each
    // class and the classes that extend it.
    let running_count: Vec<i32> = (0..old.types.len()).map(|t| slot_count(old, t)).collect();
    let mut subtree = vec![0i32; old.types.len()];
    let mut subclasses: HashMap<usize, Vec<usize>> = HashMap::new();
    for t in 0..old.types.len() {
        if class(old, t).is_none() {
            continue;
        }
        for (depth, a) in chain(old, t).into_iter().enumerate() {
            subtree[a] = subtree[a].max(running_count[t]);
            if depth > 0 {
                subclasses.entry(a).or_default().push(t);
            }
        }
    }

    // Supers before the classes that extend them.
    let mut order: Vec<(usize, usize)> = (0..new.types.len())
        .filter(|&t| class(new, t).is_some())
        .map(|t| (chain(new, t).len(), t))
        .collect();
    order.sort_unstable();

    let mut plan = Plan::default();
    // One past the largest slot each class has so far, once its own are placed.
    let mut occupied: HashMap<usize, i32> = HashMap::new();
    for (_, t) in order {
        let n = class(new, t).expect("filtered above");
        let parent = super_of(new, t);
        let parent_map = parent.and_then(|p| plan.slots.get(&p)).cloned().unwrap_or_default();
        let parent_occupied = parent.and_then(|p| occupied.get(&p)).copied().unwrap_or(0);
        let parent_slots_new = parent.map_or(0, |p| slot_count(new, p));
        let is_kept = kept(t);
        let r = if is_kept { class(old, t) } else { None };
        let running_entry = |name: &str| r.and_then(|r| r.proto.iter().find(|p| p.name == name));

        let mut seen = HashSet::new();
        if let Some(why) = n.proto.iter().find_map(|p| (!seen.insert(p.name.as_str())).then(|| p.name.clone())) {
            return Err(format!("{} declares {why} twice", n.name));
        }

        let mut map = parent_map;
        let mut taken = parent_occupied.max(if is_kept { running_count[t] } else { 0 });
        let mut entries: HashMap<&str, i32> = HashMap::new();
        for e in &n.proto {
            let slot = if e.pindex < 0 {
                running_entry(&e.name).map_or(-1, |r| r.pindex.max(-1))
            } else if e.pindex < parent_slots_new {
                // An override takes the slot of what it overrides.
                let Some(&slot) = map.get(&e.pindex) else {
                    return Err(format!(
                        "{}.{} overrides slot {} that the class it extends does not have",
                        n.name, e.name, e.pindex
                    ));
                };
                if let Some(r) = running_entry(&e.name) {
                    if r.pindex >= 0 && r.pindex != slot {
                        return Err(format!(
                            "{}.{} moves from slot {} to {slot}, the slot of the method it now overrides",
                            n.name, e.name, r.pindex
                        ));
                    }
                }
                slot
            } else if let Some(r) = running_entry(&e.name).filter(|r| r.pindex >= 0) {
                r.pindex
            } else {
                // A new slot, above those of every class that extends this one.
                if let Some(clash) = subclasses.get(&t).into_iter().flatten().find(|&&s| {
                    class(old, s).is_some_and(|o| o.proto.iter().any(|p| p.name == e.name))
                }) {
                    return Err(format!(
                        "{}.{} is new and {} already declares a method of that name",
                        n.name,
                        e.name,
                        class(old, *clash).map_or("a subclass", |o| o.name.as_str())
                    ));
                }
                let slot = taken.max(if is_kept { subtree[t] } else { 0 });
                taken = slot + 1;
                slot
            };
            if e.pindex >= 0 {
                map.insert(e.pindex, slot);
            }
            entries.insert(e.name.as_str(), slot);
            taken = taken.max(slot + 1);
        }

        // The list: the methods it had, in their order, then the ones it gains.
        let mut list: Vec<HLObjProto> = Vec::new();
        let mut kept_back: Vec<HLObjProto> = Vec::new();
        let mut placed: HashSet<&str> = HashSet::new();
        if let Some(r) = r {
            for old_entry in &r.proto {
                match n.proto.iter().find(|p| p.name == old_entry.name) {
                    Some(e) => {
                        list.push(HLObjProto {
                            pindex: entries[e.name.as_str()],
                            ..e.clone()
                        });
                        placed.insert(e.name.as_str());
                    }
                    None => {
                        plan.removed.insert(old_entry.findex.max(0) as usize);
                        if old_entry.pindex >= 0 && !inherited_slot(old, t, old_entry.pindex) {
                            kept_back.push(old_entry.clone());
                        }
                    }
                }
            }
        }
        for e in &n.proto {
            if placed.contains(e.name.as_str()) {
                continue;
            }
            if is_kept && e.findex >= 0 {
                plan.added.insert(e.findex as usize);
            }
            list.push(HLObjProto {
                pindex: entries[e.name.as_str()],
                ..e.clone()
            });
        }

        // A slot a removed method held stays taken.
        for k in &kept_back {
            taken = taken.max(k.pindex + 1);
        }
        occupied.insert(t, taken);
        plan.slots.insert(t, map);
        if !kept_back.is_empty() {
            plan.kept.insert(t, kept_back);
        }
        plan.protos.insert(t, list);
    }
    Ok(plan)
}

/// The slot operand of an op that calls or binds a method of a class, with
/// the register that holds the receiver.
fn slot_operand(op: &mut Opcode) -> Option<(u32, &mut usize)> {
    match op {
        Opcode::CallMethod { field, args, .. } => {
            let receiver = args.first()?.0;
            Some((receiver, &mut field.0))
        }
        Opcode::CallThis { field, .. } => Some((0, &mut field.0)),
        _ => None,
    }
}

/// Put the new program's prototypes and calls on the running slots.
pub fn apply(new: &mut DecodedBytecode, plan: &Plan) {
    for (t, list) in &plan.protos {
        if let Some(o) = new.types.get_mut(*t).and_then(|t| t.obj.as_mut()) {
            o.proto = list.clone();
        }
    }
    let moved: HashSet<usize> = plan
        .slots
        .iter()
        .filter(|(_, m)| m.iter().any(|(a, b)| a != b))
        .map(|(t, _)| *t)
        .collect();
    if moved.is_empty() {
        return;
    }
    for f in &mut new.functions {
        remap_calls(f, plan, &moved);
    }
    new.body_facts = std::sync::OnceLock::new();
}

fn remap_calls(f: &mut HLFunction, plan: &Plan, moved: &HashSet<usize>) {
    let regs: Vec<usize> = f.regs.iter().map(|r| r.0).collect();
    let target = |receiver: u32, field: usize| -> Option<usize> {
        let class = *regs.get(receiver as usize)?;
        if !moved.contains(&class) {
            return None;
        }
        let to = *plan.slots.get(&class)?.get(&(field as i32))?;
        (to >= 0 && to as usize != field).then_some(to as usize)
    };
    let needs = |op: &Opcode| {
        let mut op = op.clone();
        slot_operand(&mut op).is_some_and(|(r, field)| target(r, *field).is_some())
            || matches!(&op, Opcode::VirtualClosure { obj, field, .. }
                if target(obj.0, field.0 as usize).is_some())
    };
    if !f.ops().iter().any(needs) {
        return;
    }
    for op in f.ops_mut() {
        match op {
            Opcode::VirtualClosure { obj, field, .. } => {
                if let Some(to) = target(obj.0, field.0 as usize) {
                    field.0 = to as u32;
                }
            }
            op => {
                if let Some((receiver, field)) = slot_operand(op) {
                    if let Some(to) = target(receiver, *field) {
                        *field = to;
                    }
                }
            }
        }
    }
}

/// Put back the prototype entries of the methods the new program lost that
/// introduced a slot. Their functions are the running program's, so this is
/// done after the functions have been renumbered.
pub fn restore(new: &mut DecodedBytecode, plan: &Plan) {
    for (t, entries) in &plan.kept {
        if let Some(o) = new.types.get_mut(*t).and_then(|t| t.obj.as_mut()) {
            o.proto.extend(entries.iter().cloned());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::opcodes::{RefField, Reg};
    use crate::types::{HLType, HLTypeObj, TypeRef};

    fn proto(name: &str, findex: i32, pindex: i32) -> HLObjProto {
        HLObjProto {
            name: name.into(),
            findex,
            pindex,
            hashed_name: 0,
        }
    }

    fn class_type(name: &str, parent: Option<usize>, methods: &[(&str, i32, i32)]) -> HLType {
        HLType {
            kind: hl::hl_type_kind_HOBJ,
            obj: Some(HLTypeObj {
                name: name.into(),
                super_: parent.map(TypeRef),
                proto: methods.iter().map(|(n, f, p)| proto(n, *f, *p)).collect(),
                ..Default::default()
            }),
            ..Default::default()
        }
    }

    /// A program of classes given as `(name, parent, methods)`, type i is
    /// the i-th class.
    fn program(classes: &[(&str, Option<usize>, &[(&str, i32, i32)])]) -> DecodedBytecode {
        DecodedBytecode {
            types: classes
                .iter()
                .map(|(n, p, m)| class_type(n, *p, m))
                .collect(),
            ..Default::default()
        }
    }

    fn slots_of(plan: &Plan, t: usize) -> Vec<(i32, i32)> {
        let mut v: Vec<(i32, i32)> = plan.slots[&t].iter().map(|(a, b)| (*a, *b)).collect();
        v.sort_unstable();
        v
    }

    fn list(plan: &Plan, t: usize) -> Vec<(String, i32)> {
        plan.protos[&t].iter().map(|p| (p.name.clone(), p.pindex)).collect()
    }

    #[test]
    fn a_program_that_is_the_same_changes_nothing() {
        let old = program(&[("A", None, &[("f", 1, 0), ("g", 2, -1)])]);
        let new = program(&[("A", None, &[("f", 1, 0), ("g", 2, -1)])]);
        let plan = plan(&old, &new, &[]).unwrap();
        assert!(plan.is_empty());
    }

    #[test]
    fn a_method_nothing_overrides_needs_no_slot() {
        let old = program(&[("A", None, &[("f", 1, -1)])]);
        let new = program(&[("A", None, &[("f", 1, -1), ("helper", 2, -1)])]);
        let plan = plan(&old, &new, &[]).unwrap();
        assert_eq!(plan.added, HashSet::from([2]));
        assert_eq!(list(&plan, 0), vec![("f".into(), -1), ("helper".into(), -1)]);
        assert!(!plan.is_empty());
    }

    #[test]
    fn a_new_virtual_method_takes_the_next_slot() {
        let old = program(&[("A", None, &[("f", 1, 0)]), ("B", Some(0), &[("f", 2, 0)])]);
        // The compiler lists the new method first, which moves f to slot 1.
        let new = program(&[
            ("A", None, &[("g", 3, 0), ("f", 1, 1)]),
            ("B", Some(0), &[("g", 4, 0), ("f", 2, 1)]),
        ]);
        let plan = plan(&old, &new, &[]).unwrap();
        // f stays at 0, g goes after it.
        assert_eq!(list(&plan, 0), vec![("f".into(), 0), ("g".into(), 1)]);
        assert_eq!(list(&plan, 1), vec![("f".into(), 0), ("g".into(), 1)]);
        assert_eq!(slots_of(&plan, 0), vec![(0, 1), (1, 0)]);
        assert_eq!(slots_of(&plan, 1), vec![(0, 1), (1, 0)]);
    }

    #[test]
    fn a_new_slot_goes_above_the_slots_of_a_subclass() {
        // B has a slot of its own (1) that A's new method must not take.
        let old = program(&[("A", None, &[("f", 1, 0)]), ("B", Some(0), &[("f", 2, 0), ("h", 3, 1)])]);
        let new = program(&[
            ("A", None, &[("f", 1, 0), ("g", 4, 1)]),
            ("B", Some(0), &[("f", 2, 0), ("g", 5, 1), ("h", 3, 2)]),
        ]);
        let plan = plan(&old, &new, &[]).unwrap();
        assert_eq!(list(&plan, 0), vec![("f".into(), 0), ("g".into(), 2)]);
        assert_eq!(list(&plan, 1), vec![("f".into(), 0), ("h".into(), 1), ("g".into(), 2)]);
    }

    #[test]
    fn a_method_that_becomes_virtual_gets_a_slot() {
        let old = program(&[("A", None, &[("f", 1, -1)]), ("B", Some(0), &[])]);
        let new = program(&[("A", None, &[("f", 1, 0)]), ("B", Some(0), &[("f", 2, 0)])]);
        let plan = plan(&old, &new, &[]).unwrap();
        assert_eq!(list(&plan, 0), vec![("f".into(), 0)]);
        assert_eq!(list(&plan, 1), vec![("f".into(), 0)]);
        // B's f is new, an override of A's.
        assert_eq!(plan.added, HashSet::from([2]));
    }

    #[test]
    fn a_method_that_stops_being_virtual_keeps_its_slot() {
        let old = program(&[("A", None, &[("f", 1, 0)])]);
        let new = program(&[("A", None, &[("f", 1, -1)])]);
        let plan = plan(&old, &new, &[]).unwrap();
        assert_eq!(list(&plan, 0), vec![("f".into(), 0)]);
    }

    #[test]
    fn a_removed_override_is_dropped_and_a_removed_slot_is_kept() {
        let old = program(&[
            ("A", None, &[("f", 1, 0)]),
            ("B", Some(0), &[("f", 2, 0), ("own", 3, 1)]),
        ]);
        let new = program(&[("A", None, &[("f", 1, 0)]), ("B", Some(0), &[])]);
        let plan = plan(&old, &new, &[]).unwrap();
        assert_eq!(plan.removed, HashSet::from([2, 3]));
        // The override is gone; the slot B introduced stays.
        assert_eq!(list(&plan, 1), Vec::<(String, i32)>::new());
        assert_eq!(plan.kept[&1].len(), 1);
        assert_eq!(plan.kept[&1][0].name, "own");
        let mut after = new.clone();
        apply(&mut after, &plan);
        restore(&mut after, &plan);
        assert_eq!(class(&after, 1).unwrap().proto[0].name, "own");
        assert_eq!(class(&after, 1).unwrap().proto[0].pindex, 1);
    }

    #[test]
    fn an_override_added_shares_the_slot_of_its_parent() {
        let old = program(&[("A", None, &[("f", 1, 0)]), ("B", Some(0), &[])]);
        let new = program(&[("A", None, &[("f", 1, 0)]), ("B", Some(0), &[("f", 2, 0)])]);
        let plan = plan(&old, &new, &[]).unwrap();
        assert_eq!(list(&plan, 1), vec![("f".into(), 0)]);
        assert_eq!(plan.added, HashSet::from([2]));
    }

    #[test]
    fn a_class_added_below_a_kept_class_starts_above_its_slots() {
        let old = program(&[("A", None, &[("f", 1, 0), ("g", 2, 1)])]);
        // The compiler numbers C's own slot 2; the running A has 0 and 1, and
        // A itself gains h.
        let new = program(&[
            ("A", None, &[("f", 1, 0), ("g", 2, 1), ("h", 4, 2)]),
            ("C", Some(0), &[("k", 3, 3)]),
        ]);
        let plan = plan(&old, &new, &[]).unwrap();
        assert_eq!(list(&plan, 0), vec![("f".into(), 0), ("g".into(), 1), ("h".into(), 2)]);
        assert_eq!(list(&plan, 1), vec![("k".into(), 3)]);
    }

    #[test]
    fn a_new_method_a_subclass_already_has_is_refused() {
        let old = program(&[("A", None, &[("f", 1, 0)]), ("B", Some(0), &[("f", 2, 0), ("g", 3, 1)])]);
        let new = program(&[
            ("A", None, &[("f", 1, 0), ("g", 4, 1)]),
            ("B", Some(0), &[("f", 2, 0), ("g", 3, 1)]),
        ]);
        let why = plan(&old, &new, &[]).unwrap_err();
        assert!(why.contains("already declares"), "{why}");
    }

    #[test]
    fn a_method_moving_onto_the_slot_of_an_override_is_refused() {
        let old = program(&[("A", None, &[("f", 1, 0)]), ("B", Some(0), &[("g", 2, 1)])]);
        // B.g now overrides a method A has just gained.
        let new = program(&[("A", None, &[("f", 1, 0), ("g", 3, 1)]), ("B", Some(0), &[("g", 2, 1)])]);
        let why = plan(&old, &new, &[]).unwrap_err();
        assert!(why.contains("already declares") || why.contains("moves"), "{why}");
    }

    /// A slot a method was given above those of a subclass leaves the
    /// subclass's own slots below the parent's count; a later removal of such
    /// a method must still keep its entry, as the slot it introduced.
    #[test]
    fn a_slot_below_the_parents_count_that_no_ancestor_has_is_the_classs_own() {
        let old = program(&[
            ("A", None, &[("f", 1, 0)]),
            ("B", Some(0), &[("f", 2, 0), ("h", 3, 1)]),
            ("C", Some(1), &[("h", 4, 1)]),
        ]);
        // A gains a virtual g that B overrides: it takes slot 2, above h.
        let mut grown = program(&[
            ("A", None, &[("f", 1, 0), ("g", 5, 1)]),
            ("B", Some(0), &[("f", 2, 0), ("g", 6, 1), ("h", 3, 2)]),
            ("C", Some(1), &[("h", 4, 2)]),
        ]);
        let first = plan(&old, &grown, &[]).unwrap();
        apply(&mut grown, &first);
        restore(&mut grown, &first);
        assert_eq!(slot(&grown, 0, "g"), Some(2));
        assert_eq!(slot(&grown, 1, "h"), Some(1));
        // Then B.h and C.h are removed. B.h introduced slot 1 though A now
        // has three slots, and no ancestor has a method in it.
        let gone = program(&[
            ("A", None, &[("f", 1, 0), ("g", 5, 1)]),
            ("B", Some(0), &[("f", 2, 0), ("g", 6, 1)]),
            ("C", Some(1), &[]),
        ]);
        let second = plan(&grown, &gone, &[]).unwrap();
        assert_eq!(second.kept[&1].len(), 1);
        assert_eq!(second.kept[&1][0].name, "h");
        // C.h overrode it, and the slot is B's to keep.
        assert!(!second.kept.contains_key(&2));
    }

    fn slot(bc: &DecodedBytecode, t: usize, name: &str) -> Option<i32> {
        class(bc, t)?.proto.iter().find(|p| p.name == name).map(|p| p.pindex)
    }

    fn call(field: usize, receiver: u32) -> Opcode {
        Opcode::CallMethod {
            dst: Reg(0),
            field: RefField(field),
            args: vec![Reg(receiver)],
        }
    }

    #[test]
    fn calls_are_rewritten_by_the_class_of_the_receiver() {
        let old = program(&[("A", None, &[("f", 1, 0)])]);
        let mut new = program(&[("A", None, &[("g", 3, 0), ("f", 1, 1)])]);
        let mut caller = HLFunction::with_body(vec![call(1, 0), call(0, 1), call(1, 2)], Vec::new());
        // r0 is an A, r1 is not a class (slot 0 must stay), r2 is an A.
        caller.regs = vec![TypeRef(0), TypeRef(1), TypeRef(0)];
        new.types.push(HLType::default());
        new.functions.push(caller);
        let plan = plan(&old, &new, &[]).unwrap();
        apply(&mut new, &plan);
        let fields: Vec<usize> = new.functions[0]
            .ops()
            .iter()
            .map(|op| match op {
                Opcode::CallMethod { field, .. } => field.0,
                _ => unreachable!(),
            })
            .collect();
        assert_eq!(fields, vec![0, 0, 0]);
    }

    #[test]
    fn this_calls_and_closures_are_rewritten_too() {
        let old = program(&[("A", None, &[("f", 1, 0)])]);
        let mut new = program(&[("A", None, &[("g", 3, 0), ("f", 1, 1)])]);
        let mut method = HLFunction::with_body(
            vec![
                Opcode::CallThis {
                    dst: Reg(1),
                    field: RefField(1),
                    args: vec![],
                },
                Opcode::VirtualClosure {
                    dst: Reg(1),
                    obj: Reg(0),
                    field: Reg(1),
                },
            ],
            Vec::new(),
        );
        method.regs = vec![TypeRef(0), TypeRef(0)];
        new.functions.push(method);
        let plan = plan(&old, &new, &[]).unwrap();
        apply(&mut new, &plan);
        let ops = new.functions[0].ops();
        assert!(matches!(&ops[0], Opcode::CallThis { field, .. } if field.0 == 0));
        assert!(matches!(&ops[1], Opcode::VirtualClosure { field, .. } if field.0 == 0));
    }
}
