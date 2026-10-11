//! Fields added to a class that is already running.
//!
//! An object keeps the layout it was made with, and compiled code reads a
//! field at a fixed offset from it, so a field can only be added where the
//! objects made before the reload have room for it. `--hot-reload` allocates
//! every object with spare bytes at its end (`hlp_set_object_room`). A field
//! the new program adds to a class is laid out after the fields the class
//! has, in that room, and reads as zero in the objects made before the
//! reload: the constructor's initialiser has not run for them.
//!
//! The running order of the fields wins, as the running slots do for
//! methods. The new program's field list for a class is put in the running
//! order with the added fields after it, and the bodies that name a field of
//! the class are rewritten to match. A field the new program drops stays in
//! its place, unused.
//!
//! Only a class nothing extends can gain a field: the fields of a subclass
//! follow its parent's, and would move.

use crate::bytecode::DecodedBytecode;
use crate::hl;
use crate::layout;
use crate::opcodes::Opcode;
use crate::types::{HLFunction, HLObjField, HLType, HLTypeObj};
use air::opcode_info::{successors, writes};
use std::collections::{HashMap, HashSet};

/// How far up a class's ancestors are followed, so that a table that names
/// itself as its own super cannot loop.
const DEPTH: usize = 64;

/// How far a value is followed back through copies to the op that made it.
const COPIES: usize = 8;

/// ashui keeps the signal of a `@:state` field in a field named
/// `<field>__state`. A program built with `-D ashui_hot_reload` makes the
/// signal on first use when the field is null, which is how an object built
/// before the reload that added the field reads it; that program has the
/// static `ashui.ui.HotReload.stateOnFirstUse`.
const LAZY_SUFFIX: &str = "__state";
const LAZY_MARKER: (&str, &str) = ("ashui.ui.HotReload", "stateOnFirstUse");

/// The name of the type that holds the statics of the class `name`, which has
/// the `$` before the last part: `pkg.$Name`.
fn statics_name(name: &str) -> String {
    match name.rsplit_once('.') {
        Some((package, last)) => format!("{package}.${last}"),
        None => format!("${name}"),
    }
}

/// The class whose statics the type `name` holds, if it holds any.
fn statics_of(name: &str) -> Option<String> {
    match name.rsplit_once('.') {
        Some((package, last)) => Some(format!("{package}.{}", last.strip_prefix('$')?)),
        None => name.strip_prefix('$').map(str::to_string),
    }
}

/// Whether the program makes its `__state` fields on first use.
fn makes_state_on_first_use(bc: &DecodedBytecode) -> bool {
    let (class, field) = LAZY_MARKER;
    let statics = statics_name(class);
    bc.types
        .iter()
        .filter_map(|t| t.obj.as_ref())
        .any(|o| o.name == statics && o.fields.iter().any(|f| f.name == field))
}

/// The object type at `t`, if it is one.
fn class(bc: &DecodedBytecode, t: usize) -> Option<&HLTypeObj> {
    let ty = bc.types.get(t)?;
    if ty.kind != hl::hl_type_kind_HOBJ && ty.kind != hl::hl_type_kind_HSTRUCT {
        return None;
    }
    ty.obj.as_ref()
}

/// The classes above `t`, nearest first.
fn ancestors(bc: &DecodedBytecode, t: usize) -> Vec<usize> {
    let mut out = Vec::new();
    let mut at = class(bc, t).and_then(|o| o.super_.as_ref()).map(|s| s.0);
    while let Some(i) = at {
        if out.len() >= DEPTH || class(bc, i).is_none() {
            break;
        }
        out.push(i);
        at = class(bc, i).and_then(|o| o.super_.as_ref()).map(|s| s.0);
    }
    out
}

/// How many fields the ancestors of `t` have between them, which is where
/// `t`'s own fields start in a field index.
fn inherited(bc: &DecodedBytecode, t: usize) -> usize {
    ancestors(bc, t)
        .into_iter()
        .filter_map(|a| class(bc, a))
        .map(|o| o.fields.len())
        .sum()
}

/// The name of field `index` of `t`, counting every ancestor's fields first.
fn field_name(bc: &DecodedBytecode, t: usize, index: usize) -> Option<&str> {
    let mut chain = ancestors(bc, t);
    chain.reverse();
    chain.push(t);
    let mut skip = index;
    for c in chain {
        let fields = &class(bc, c)?.fields;
        if skip < fields.len() {
            return Some(fields[skip].name.as_str());
        }
        skip -= fields.len();
    }
    None
}

/// Whether two field lists name the same fields of the same types in the same
/// order.
fn same(a: &[HLObjField], b: &[HLObjField]) -> bool {
    a.len() == b.len()
        && a.iter()
            .zip(b)
            .all(|(x, y)| x.name == y.name && x.type_.0 == y.type_.0)
}

/// A field as two programs name it: see [`identities`].
type Identity = (String, usize, usize);

/// What a reload leaves for the next one to know about the classes whose fields
/// it changed.
#[derive(Clone, Debug, Default)]
pub struct Kept {
    /// For each class that has grown, the smallest size an object of it was
    /// allocated at, which it cannot grow past.
    pub floors: HashMap<usize, i32>,
    /// For each class, the fields it dropped and has not got back. They stay in
    /// the class, and objects built since hold nothing in them.
    ghosts: HashMap<usize, HashSet<Identity>>,
}

/// What each field of a list is called across two programs: its name, or for a
/// field with none, which the cache of an interface a class implements is, its
/// type and which of that type's unnamed fields it is.
fn identities(fields: &[HLObjField]) -> Vec<Identity> {
    let mut unnamed: HashMap<usize, usize> = HashMap::new();
    fields
        .iter()
        .map(|f| {
            if f.name.is_empty() {
                let nth = unnamed.entry(f.type_.0).or_insert(0);
                *nth += 1;
                (String::new(), f.type_.0, *nth - 1)
            } else {
                (f.name.clone(), 0, 0)
            }
        })
        .collect()
}

/// A field as a message names it.
fn shown(identity: &Identity) -> String {
    if identity.0.is_empty() {
        "<interface>".to_string()
    } else {
        identity.0.clone()
    }
}

/// The constructor of the class `t`, which its statics object binds as
/// `__constructor__`.
fn constructor(bc: &DecodedBytecode, t: usize) -> Option<&HLFunction> {
    let name = statics_name(&class(bc, t)?.name);
    let statics = (0..bc.types.len()).find(|&s| class(bc, s).is_some_and(|o| o.name == name))?;
    let bindings = &class(bc, statics)?.bindings;
    let findex = bindings.chunks_exact(2).find_map(|pair| {
        let named = usize::try_from(pair[0])
            .ok()
            .and_then(|fid| field_name(bc, statics, fid));
        (named == Some("__constructor__")).then_some(pair[1])
    })?;
    bc.functions.iter().find(|f| f.findex == findex)
}

/// What a function's ops do to the straight run a value is followed along.
struct Flow {
    /// The ops that something jumps to.
    targets: HashSet<usize>,
    /// The registers whose address is taken.
    referenced: HashSet<u32>,
}

/// The ops that some jump, branch, switch or trap handler lands on.
fn jump_targets(ops: &[Opcode]) -> HashSet<usize> {
    let mut out = HashSet::new();
    for (i, op) in ops.iter().enumerate() {
        out.extend(successors(op, i, ops.len()).into_iter().filter(|&s| s != i + 1));
    }
    out
}

/// The registers whose address the ops take: a call can write them without
/// an op naming them.
fn referenced(ops: &[Opcode]) -> HashSet<u32> {
    ops.iter()
        .filter_map(|op| match op {
            Opcode::Ref { src, .. } => Some(src.0),
            _ => None,
        })
        .collect()
}

/// Whether `reg` holds a constant or null at op `at`, found by following it
/// back to the op that wrote it. Control must run straight from that op to
/// `at`: if anything jumps to an op between them, as the two ends of an
/// `if` do, the register may hold what another path wrote.
fn constant_value(
    ops: &[Opcode],
    flow: &Flow,
    constants: &HashSet<usize>,
    at: usize,
    reg: u32,
    copies: usize,
) -> bool {
    if flow.referenced.contains(&reg) {
        return false;
    }
    for i in (0..at).rev() {
        if !writes(&ops[i]).iter().any(|r| r.0 == reg) {
            continue;
        }
        if (i + 1..=at).any(|k| flow.targets.contains(&k)) {
            return false;
        }
        return match &ops[i] {
            Opcode::Int { .. }
            | Opcode::Float { .. }
            | Opcode::Bool { .. }
            | Opcode::Bytes { .. }
            | Opcode::String { .. }
            | Opcode::Null { .. } => true,
            Opcode::GetGlobal { global, .. } => constants.contains(&global.0),
            Opcode::Mov { src, .. } if copies > 0 => {
                constant_value(ops, flow, constants, i, src.0, copies - 1)
            }
            _ => false,
        };
    }
    false
}

/// Whether the constructor of `class` stores a value that is not a constant
/// into the field at `field`.
fn initialised_by_a_computation(
    class: usize,
    field: usize,
    constructor: &HLFunction,
    constants: &HashSet<usize>,
) -> bool {
    let ops = constructor.ops();
    let flow = Flow {
        targets: jump_targets(ops),
        referenced: referenced(ops),
    };
    ops.iter().enumerate().any(|(at, op)| {
        let (receiver, index, value) = match op {
            Opcode::SetField { obj, field, src } => (obj.0, field.0, src.0),
            Opcode::SetThis { field, src } => (0, field.0, src.0),
            _ => return false,
        };
        index == field
            && constructor.regs.get(receiver as usize).map(|r| r.0) == Some(class)
            && !constant_value(ops, &flow, constants, at, value, COPIES)
    })
}

/// Refuse the constructor of the class `t` if it stores something that is not
/// a constant into one of `fields`, which the objects built before the reload
/// lack: each is the field's name, its place in the new program's list of the
/// class's own fields, and whether the class had dropped it. The constructor
/// still names the fields as the new program numbers them.
fn check_constructor(
    new: &DecodedBytecode,
    t: usize,
    fields: &[(&String, usize, bool)],
    lazy_state: bool,
    constants: &HashSet<usize>,
) -> Result<(), String> {
    let Some(ctor) = constructor(new, t) else {
        return Ok(());
    };
    let start = inherited(new, t);
    let name = class(new, t).map_or("", |o| o.name.as_str());
    for &(field, at, back) in fields {
        let state = field.ends_with(LAZY_SUFFIX);
        if state && lazy_state {
            continue;
        }
        if initialised_by_a_computation(t, start + at, ctor, constants) {
            return Err(if state {
                format!(
                    "{name}.{field} is a state field the program does not make on first use: build with -D ashui_hot_reload so a new @:state field is made on first use"
                )
            } else if back {
                format!(
                    "{name}.{field} is set by a constructor with something that is not a constant, and an earlier reload dropped the field, so objects built since hold null in it"
                )
            } else {
                format!(
                    "{name}.{field} is set by a constructor with something that is not a constant, which does not run for objects built before the reload"
                )
            });
        }
    }
    Ok(())
}

/// Put the field lists of the classes of `new`, which is on the types of
/// `old` already, in the running order, and the bodies that name those fields
/// to match. `kept` holds what the reloads so far left about the classes, and
/// gets what this one does; `room` is the spare bytes an object has, `pinned` the classes whose
/// objects something lays out one after another at their size (`None` when
/// that is not known), and `dropped` the types of `old` that `new` has only as
/// placeholders.
///
/// `Err` says why a class cannot be laid out so: a field of another type, a
/// class something extends, or no room left. `kept` may have changed by then,
/// so it is a copy the caller drops.
pub fn keep_order(
    old: &DecodedBytecode,
    new: &mut DecodedBytecode,
    kept: &mut Kept,
    room: usize,
    pinned: Option<&HashSet<usize>>,
    dropped: &[usize],
) -> Result<(), String> {
    let mut moved: HashMap<usize, (usize, Vec<usize>)> = HashMap::new();
    let constants: HashSet<usize> = new.constants.iter().map(|c| c.global as usize).collect();
    let lazy_state = makes_state_on_first_use(new);
    for t in 0..old.types.len().min(new.types.len()) {
        let (Some(o), Some(n)) = (class(old, t), class(new, t)) else {
            continue;
        };
        let known: HashSet<Identity> = kept.ghosts.get(&t).cloned().unwrap_or_default();
        let unchanged = same(&o.fields, &n.fields);
        if unchanged && known.is_empty() {
            continue;
        }
        // A class that has dropped a field is looked at even when the lists
        // match: the new program declares it again, and nothing else moves.
        if unchanged {
            let wanted = identities(&n.fields);
            let revived: Vec<(&String, usize, bool)> = wanted
                .iter()
                .enumerate()
                .filter(|(_, id)| known.contains(*id))
                .map(|(at, _)| (&n.fields[at].name, at, true))
                .collect();
            check_constructor(new, t, &revived, lazy_state, &constants)?;
            kept.ghosts.remove(&t);
            continue;
        }
        let name = o.name.clone();
        if old.types[t].kind != hl::hl_type_kind_HOBJ {
            return Err(format!("{name} is a struct, whose layout cannot change"));
        }
        if let Some(class) = statics_of(&name) {
            return Err(format!(
                "the statics of {class} changed; a reload does not set up a static of a class that is running"
            ));
        }
        let (running, wanted) = (identities(&o.fields), identities(&n.fields));
        let unique = |ids: &[Identity]| ids.iter().collect::<HashSet<_>>().len() == ids.len();
        if !unique(&running) || !unique(&wanted) {
            return Err(format!("{name} has two fields of one name"));
        }
        // A field the new program no longer has stays where it is, unused: the
        // bodies that began before the reload may read it, and a later reload
        // that declares it again finds it with the value it had.
        let mut gone: Vec<Identity> = Vec::new();
        for (f, id) in o.fields.iter().zip(&running) {
            match wanted.iter().position(|w| w == id) {
                None => gone.push(id.clone()),
                Some(at) if n.fields[at].type_.0 != f.type_.0 => {
                    return Err(if known.contains(id) {
                        format!(
                            "field {} of {name} changed its type, and the class still has the field it dropped",
                            f.name
                        )
                    } else {
                        format!("field {} of {name} changed its type", f.name)
                    });
                }
                Some(_) => {}
            }
        }
        // The fields the class dropped that the new program declares again.
        let revived: Vec<(String, usize)> = wanted
            .iter()
            .enumerate()
            .filter(|(_, id)| known.contains(*id))
            .map(|(at, id)| (shown(id), at))
            .collect();
        // The running fields in their order, then the fields the new program
        // adds, in its order.
        let mut perm = Vec::with_capacity(n.fields.len());
        // The added fields, with the place the new program gives each.
        let mut added: Vec<(String, usize)> = Vec::new();
        let mut list = o.fields.clone();
        for (at, id) in wanted.iter().enumerate() {
            match running.iter().position(|r| r == id) {
                Some(at_running) => {
                    perm.push(at_running);
                    list[at_running] = n.fields[at].clone();
                }
                None => {
                    perm.push(o.fields.len() + added.len());
                    added.push((shown(id), at));
                    list.push(n.fields[at].clone());
                }
            }
        }
        let what = {
            let mut parts = Vec::new();
            if !gone.is_empty() {
                let names: Vec<String> = gone.iter().map(shown).collect();
                parts.push(format!("lost {}", names.join(", ")));
            }
            if !added.is_empty() {
                let names: Vec<&str> = added.iter().map(|(n, _)| n.as_str()).collect();
                parts.push(format!("gained {}", names.join(", ")));
            }
            if parts.is_empty() {
                "has its fields in another order".to_string()
            } else {
                parts.join(" and ")
            }
        };
        let extended_by = (0..old.types.len().max(new.types.len())).find_map(|x| {
            [old, &*new].into_iter().find_map(|bc| {
                let o = class(bc, x)?;
                (x != t && o.super_.as_ref().map(|s| s.0) == Some(t)).then(|| o.name.clone())
            })
        });
        if let Some(sub) = extended_by {
            return Err(format!(
                "{name} {what}, and {sub} extends it, so its fields would move"
            ));
        }
        if !o.bindings.is_empty() || !n.bindings.is_empty() {
            return Err(format!("{name} {what}, and binds closures into its fields"));
        }
        if new
            .host_classes
            .iter()
            .any(|h| h.type_index == t || h.companion_index == t)
        {
            return Err(format!("{name} {what}, and belongs to the host"));
        }
        if new
            .constants
            .iter()
            .any(|c| new.globals.get(c.global as usize).is_some_and(|g| g.0 == t))
        {
            return Err(format!("{name} {what}, and has constant objects"));
        }

        let before = layout::object_layout(&old.types, t)
            .ok_or_else(|| format!("{name} {what}, and its layout is not one a reload can extend"))?;
        let start = inherited(new, t);
        // A field the class keeps may be of a type the new program dropped,
        // which is a placeholder until the types are put back, and has no size.
        let mut kept_types: Vec<usize> = list
            .iter()
            .map(|f| f.type_.0)
            .filter(|ty| dropped.contains(ty))
            .collect();
        kept_types.sort_unstable();
        kept_types.dedup();
        let held: Vec<(usize, HLType)> = kept_types
            .into_iter()
            .map(|ty| (ty, std::mem::replace(&mut new.types[ty], old.types[ty].clone())))
            .collect();
        new.types[t].obj.as_mut().unwrap().fields = list;
        let after = layout::object_layout(&new.types, t);
        for (ty, placeholder) in held.into_iter().rev() {
            new.types[ty] = placeholder;
        }
        let after = after
            .ok_or_else(|| format!("{name} {what}, and its layout is not one a reload can extend"))?;
        let spare = i32::try_from(room).unwrap_or(i32::MAX);
        // Objects laid out in a row stay where they are while the class keeps
        // its size.
        if after.size != before.size {
            match pinned {
                Some(rows) if rows.contains(&t) => {
                    return Err(format!(
                        "{name} {what}, and a CArray of it lays its objects out one after another"
                    ));
                }
                None => {
                    return Err(format!(
                        "{name} {what}, and the runtime cannot say whether a CArray holds it"
                    ));
                }
                _ => {}
            }
        }
        let floor = *kept.floors.entry(t).or_insert(before.size.saturating_add(spare));
        if after.size > floor {
            return Err(format!(
                "{name} {what}, and an object made before the reload has {floor} bytes where the class now needs {}",
                after.size
            ));
        }
        // What an object built before the reload lacks cannot be made by a
        // computation in the constructor. A field the class had dropped is as
        // missing from the objects built since.
        let checked: Vec<(&String, usize, bool)> = added
            .iter()
            .map(|(field, at)| (field, *at, false))
            .chain(revived.iter().map(|(field, at)| (field, *at, true)))
            .collect();
        check_constructor(new, t, &checked, lazy_state, &constants)?;
        // The fields the class now lacks, and has dropped and not got back.
        let ghosts: HashSet<Identity> = gone.into_iter().collect();
        if ghosts.is_empty() {
            kept.ghosts.remove(&t);
        } else {
            kept.ghosts.insert(t, ghosts);
        }
        if perm.iter().enumerate().any(|(i, &p)| i != p) {
            moved.insert(t, (start, perm));
        }
    }
    if moved.is_empty() {
        return Ok(());
    }
    for f in &mut new.functions {
        renumber(f, &moved);
    }
    new.body_facts = std::sync::OnceLock::new();
    Ok(())
}

/// Rewrite the field indexes in `f` that name a field of a class in `moved`,
/// which maps a class to where its own fields start and the running position
/// of each, by the new program's.
fn renumber(f: &mut HLFunction, moved: &HashMap<usize, (usize, Vec<usize>)>) {
    let regs: Vec<usize> = f.regs.iter().map(|r| r.0).collect();
    let target = |receiver: u32, field: usize| -> Option<usize> {
        let (start, perm) = moved.get(regs.get(receiver as usize)?)?;
        let own = field.checked_sub(*start)?;
        let to = start + *perm.get(own)?;
        (to != field).then_some(to)
    };
    let needs = |op: &Opcode| match op {
        Opcode::Field { obj, field, .. } | Opcode::SetField { obj, field, .. } => {
            target(obj.0, field.0).is_some()
        }
        Opcode::GetThis { field, .. } | Opcode::SetThis { field, .. } => {
            target(0, field.0).is_some()
        }
        _ => false,
    };
    if !f.ops().iter().any(needs) {
        return;
    }
    for op in f.ops_mut() {
        let (receiver, field) = match op {
            Opcode::Field { obj, field, .. } | Opcode::SetField { obj, field, .. } => {
                (obj.0, field)
            }
            Opcode::GetThis { field, .. } | Opcode::SetThis { field, .. } => (0, field),
            _ => continue,
        };
        if let Some(to) = target(receiver, field.0) {
            field.0 = to;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::opcodes::{RefField, RefFun, RefInt, Reg};
    use crate::types::{HLType, TypeRef};

    const I32: usize = 0;
    const PTR: usize = 1;

    fn prim(kind: hl::hl_type_kind) -> HLType {
        HLType {
            kind,
            ..Default::default()
        }
    }

    fn class_type(name: &str, parent: Option<usize>, fields: &[(&str, usize)]) -> HLType {
        HLType {
            kind: hl::hl_type_kind_HOBJ,
            obj: Some(HLTypeObj {
                name: name.into(),
                super_: parent.map(TypeRef),
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

    /// A program of an int type, a pointer type and these classes, which are
    /// types 2 and on.
    fn program(classes: Vec<HLType>) -> DecodedBytecode {
        let mut types = vec![prim(hl::hl_type_kind_HI32), prim(hl::hl_type_kind_HBYTES)];
        types.extend(classes);
        DecodedBytecode {
            types,
            ..Default::default()
        }
    }

    fn names(bc: &DecodedBytecode, t: usize) -> Vec<&str> {
        class(bc, t).unwrap().fields.iter().map(|f| f.name.as_str()).collect()
    }

    fn fields_of(ops: &[Opcode]) -> Vec<usize> {
        ops.iter()
            .map(|op| match op {
                Opcode::GetThis { field, .. }
                | Opcode::SetThis { field, .. }
                | Opcode::Field { field, .. }
                | Opcode::SetField { field, .. } => field.0,
                _ => unreachable!(),
            })
            .collect()
    }

    fn keep(old: &DecodedBytecode, new: &mut DecodedBytecode, room: usize) -> Result<(), String> {
        keep_order(old, new, &mut Kept::default(), room, Some(&HashSet::new()), &[])
    }

    #[test]
    fn a_program_with_the_same_fields_is_left_alone() {
        let old = program(vec![class_type("A", None, &[("n", I32)])]);
        let mut new = program(vec![class_type("A", None, &[("n", I32)])]);
        let kept = &mut Kept::default();
        keep_order(&old, &mut new, kept, 64, Some(&HashSet::new()), &[]).unwrap();
        assert!(kept.floors.is_empty());
    }

    #[test]
    fn a_field_added_at_the_end_goes_there() {
        let old = program(vec![class_type("A", None, &[("n", I32)])]);
        let mut new = program(vec![class_type("A", None, &[("n", I32), ("p", PTR)])]);
        let kept = &mut Kept::default();
        keep_order(&old, &mut new, kept, 64, Some(&HashSet::new()), &[]).unwrap();
        assert_eq!(names(&new, 2), vec!["n", "p"]);
        // An instance is a header, an int and padding, 16 bytes, and 64 spare.
        assert_eq!(kept.floors[&2], 16 + 64);
    }

    #[test]
    fn a_field_declared_between_the_others_is_renumbered() {
        let old = program(vec![class_type("A", None, &[("a", I32), ("b", PTR)])]);
        let mut new = program(vec![class_type("A", None, &[("a", I32), ("x", I32), ("b", PTR)])]);
        let mut f = HLFunction::with_body(
            vec![
                Opcode::GetThis { dst: Reg(1), field: RefField(0) },
                Opcode::SetThis { field: RefField(1), src: Reg(1) },
                Opcode::GetThis { dst: Reg(1), field: RefField(2) },
                Opcode::Field { dst: Reg(1), obj: Reg(2), field: RefField(2) },
                Opcode::SetField { obj: Reg(3), field: RefField(2), src: Reg(1) },
            ],
            Vec::new(),
        );
        // r0 and r2 are As, r3 is not a class.
        f.regs = vec![TypeRef(2), TypeRef(I32), TypeRef(2), TypeRef(I32)];
        new.functions.push(f);
        keep(&old, &mut new, 64).unwrap();
        assert_eq!(names(&new, 2), vec!["a", "b", "x"]);
        // `x` was index 1 and `b` index 2; `b` is 1 and `x` is 2 now.
        assert_eq!(fields_of(new.functions[0].ops()), vec![0, 2, 1, 1, 2]);
    }

    #[test]
    fn the_fields_of_the_ancestors_come_first() {
        let old = program(vec![
            class_type("P", None, &[("p", I32)]),
            class_type("C", Some(2), &[("a", I32), ("b", I32)]),
        ]);
        let mut new = program(vec![
            class_type("P", None, &[("p", I32)]),
            class_type("C", Some(2), &[("a", I32), ("x", I32), ("b", I32)]),
        ]);
        let mut f = HLFunction::with_body(
            vec![
                // p, a, x and b of a C.
                Opcode::GetThis { dst: Reg(1), field: RefField(0) },
                Opcode::GetThis { dst: Reg(1), field: RefField(1) },
                Opcode::GetThis { dst: Reg(1), field: RefField(2) },
                Opcode::GetThis { dst: Reg(1), field: RefField(3) },
            ],
            Vec::new(),
        );
        f.regs = vec![TypeRef(3), TypeRef(I32)];
        new.functions.push(f);
        keep(&old, &mut new, 64).unwrap();
        assert_eq!(fields_of(new.functions[0].ops()), vec![0, 1, 3, 2]);
    }

    #[test]
    fn a_function_that_did_not_change_is_the_function_that_was_running() {
        let ops = |b: usize| {
            vec![
                Opcode::GetThis { dst: Reg(1), field: RefField(b) },
                Opcode::Ret { ret: Reg(1) },
            ]
        };
        let function = |b: usize| {
            let mut f = HLFunction::with_body(ops(b), Vec::new());
            f.regs = vec![TypeRef(2), TypeRef(I32)];
            f
        };
        let mut old = program(vec![class_type("A", None, &[("a", I32), ("b", I32)])]);
        old.functions.push(function(1));
        let mut new = program(vec![class_type("A", None, &[("a", I32), ("x", I32), ("b", I32)])]);
        new.functions.push(function(2));
        assert_ne!(old.functions[0].compute_hash(), new.functions[0].compute_hash());
        keep(&old, &mut new, 64).unwrap();
        assert_eq!(old.functions[0].compute_hash(), new.functions[0].compute_hash());
    }

    #[test]
    fn a_retyped_field_is_refused() {
        let old = program(vec![class_type("A", None, &[("a", I32), ("b", PTR)])]);
        let mut retyped = program(vec![class_type("A", None, &[("a", I32), ("b", I32)])]);
        let why = keep(&old, &mut retyped, 64).unwrap_err();
        assert!(why.contains("field b of A changed its type"), "{why}");
    }

    /// A field the new program drops stays where it is, and the fields after
    /// it keep their places in the bodies that began before the reload.
    #[test]
    fn a_field_that_is_gone_stays_unused() {
        let old = program(vec![class_type("A", None, &[("a", I32), ("b", PTR), ("c", I32)])]);
        let mut new = program(vec![class_type("A", None, &[("a", I32), ("c", I32)])]);
        // c is index 1 in the new program and 2 in the class.
        let mut f = HLFunction::with_body(
            vec![
                Opcode::GetThis { dst: Reg(1), field: RefField(0) },
                Opcode::GetThis { dst: Reg(1), field: RefField(1) },
            ],
            Vec::new(),
        );
        f.regs = vec![TypeRef(2), TypeRef(I32)];
        new.functions.push(f);
        keep(&old, &mut new, 64).unwrap();
        assert_eq!(names(&new, 2), vec!["a", "b", "c"]);
        assert_eq!(fields_of(new.functions[0].ops()), vec![0, 2]);
        // The next reload finds the field in the running class: dropping it
        // again changes nothing, and declaring it again brings it back where
        // it was.
        let mut again = program(vec![class_type("A", None, &[("a", I32), ("c", I32)])]);
        keep(&new, &mut again, 64).unwrap();
        assert_eq!(names(&again, 2), vec!["a", "b", "c"]);
        let mut back = program(vec![class_type("A", None, &[("a", I32), ("b", PTR), ("c", I32)])]);
        keep(&new, &mut back, 64).unwrap();
        assert_eq!(names(&back, 2), vec!["a", "b", "c"]);
    }

    /// The type of the field the class keeps is one the new program no longer
    /// has: the class is still as large as it was.
    #[test]
    fn a_kept_field_of_a_dropped_type_still_has_its_size() {
        let old = program(vec![
            class_type("Gone", None, &[]),
            class_type("A", None, &[("n", I32), ("g", 2)]),
        ]);
        // Only a placeholder is left at the dropped type's place.
        let mut new = program(vec![
            HLType::default(),
            class_type("A", None, &[("n", I32), ("m", PTR)]),
        ]);
        // Without the room the class's 24 bytes do not hold another reference.
        let why = keep_order(&old, &mut new, &mut Kept::default(), 0, Some(&HashSet::new()), &[2]).unwrap_err();
        assert!(why.contains("has 24 bytes where the class now needs 32"), "{why}");
        assert!(new.types[2].obj.is_none(), "the placeholder is put back");
        let mut new = program(vec![
            HLType::default(),
            class_type("A", None, &[("n", I32), ("m", PTR)]),
        ]);
        keep_order(&old, &mut new, &mut Kept::default(), 8, Some(&HashSet::new()), &[2]).unwrap();
    }

    #[test]
    fn a_field_renamed_is_one_dropped_and_one_added() {
        let old = program(vec![class_type("A", None, &[("a", I32), ("b", PTR)])]);
        let mut new = program(vec![class_type("A", None, &[("a", I32), ("c", PTR)])]);
        keep(&old, &mut new, 64).unwrap();
        assert_eq!(names(&new, 2), vec!["a", "b", "c"]);
    }

    #[test]
    fn a_dropped_field_needs_no_room_but_a_class_something_extends_is_refused() {
        let old = program(vec![
            class_type("A", None, &[("a", I32), ("b", I32)]),
            class_type("B", Some(2), &[]),
        ]);
        let mut new = program(vec![
            class_type("A", None, &[("a", I32)]),
            class_type("B", Some(2), &[]),
        ]);
        let why = keep(&old, &mut new, 0).unwrap_err();
        assert!(why.contains("A lost b, and B extends it"), "{why}");
        let old = program(vec![class_type("A", None, &[("a", I32), ("b", I32)])]);
        let mut new = program(vec![class_type("A", None, &[("a", I32)])]);
        keep(&old, &mut new, 0).unwrap();
    }

    #[test]
    fn a_class_something_extends_is_refused() {
        let old = program(vec![
            class_type("A", None, &[("a", I32)]),
            class_type("B", Some(2), &[("b", I32)]),
        ]);
        let mut new = program(vec![
            class_type("A", None, &[("a", I32), ("x", I32)]),
            class_type("B", Some(2), &[("b", I32)]),
        ]);
        let why = keep(&old, &mut new, 64).unwrap_err();
        assert!(why.contains("A gained x, and B extends it"), "{why}");
    }

    #[test]
    fn a_class_the_new_program_extends_is_refused_too() {
        let old = program(vec![class_type("A", None, &[("a", I32)])]);
        let mut new = program(vec![
            class_type("A", None, &[("a", I32), ("x", I32)]),
            class_type("B", Some(2), &[]),
        ]);
        let why = keep(&old, &mut new, 64).unwrap_err();
        assert!(why.contains("B extends it"), "{why}");
    }

    #[test]
    fn a_class_extended_by_a_dropped_class_is_refused() {
        let old = program(vec![
            class_type("A", None, &[("a", I32)]),
            class_type("Gone", Some(2), &[]),
        ]);
        // `Gone` is a placeholder until it is put back.
        let mut new = program(vec![class_type("A", None, &[("a", I32), ("x", I32)]), HLType::default()]);
        let why = keep(&old, &mut new, 64).unwrap_err();
        assert!(why.contains("Gone extends it"), "{why}");
    }

    #[test]
    fn a_class_cannot_grow_past_the_room_its_objects_have() {
        let old = program(vec![class_type("A", None, &[("n", I32)])]);
        // 16 bytes hold the header and two ints; a third int needs 24.
        let grown = |fields: &[(&'static str, usize)]| program(vec![class_type("A", None, fields)]);
        let mut two = grown(&[("n", I32), ("m", I32)]);
        keep(&old, &mut two, 0).unwrap();
        let mut three = grown(&[("n", I32), ("m", I32), ("k", I32)]);
        let why = keep(&old, &mut three, 0).unwrap_err();
        assert!(why.contains("has 16 bytes where the class now needs 24"), "{why}");
        let mut three = grown(&[("n", I32), ("m", I32), ("k", I32)]);
        keep(&old, &mut three, 8).unwrap();
    }

    #[test]
    fn the_first_size_is_the_floor_however_often_a_class_grows() {
        let fields = |n: usize| -> Vec<(&'static str, usize)> {
            ["a", "b", "c", "d", "e", "f", "g", "h"][..n].iter().map(|f| (*f, I32)).collect()
        };
        let mut kept = Kept::default();
        let first = program(vec![class_type("A", None, &fields(1))]);
        let second = program(vec![class_type("A", None, &fields(3))]);
        let third = program(vec![class_type("A", None, &fields(6))]);
        let fourth = program(vec![class_type("A", None, &fields(7))]);
        // 16 bytes and 16 spare: 32.
        let mut next = second.clone();
        keep_order(&first, &mut next, &mut kept, 16, Some(&HashSet::new()), &[]).unwrap();
        assert_eq!(kept.floors[&2], 32);
        // The running class is the 24 byte one now, but its objects are 32.
        let mut next = third.clone();
        keep_order(&second, &mut next, &mut kept, 16, Some(&HashSet::new()), &[]).unwrap();
        assert_eq!(kept.floors[&2], 32);
        // Seven ints end at 36, which is 40 with padding.
        let mut next = fourth.clone();
        let why = keep_order(&third, &mut next, &mut kept, 16, Some(&HashSet::new()), &[]).unwrap_err();
        assert!(why.contains("has 32 bytes where the class now needs 40"), "{why}");
    }

    #[test]
    fn a_struct_and_a_statics_object_cannot_gain_a_field() {
        let mut s = class_type("S", None, &[("a", I32)]);
        s.kind = hl::hl_type_kind_HSTRUCT;
        let old = program(vec![s.clone(), class_type("$A", None, &[])]);
        let mut grown = s;
        grown.obj.as_mut().unwrap().fields.push(HLObjField {
            name: "b".into(),
            type_: TypeRef(I32),
            hashed_name: 0,
        });
        let mut new = program(vec![grown, class_type("$A", None, &[])]);
        let why = keep(&old, &mut new, 64).unwrap_err();
        assert!(why.contains("S is a struct"), "{why}");
        let mut new = program(vec![
            class_type("S", None, &[("a", I32)]),
            class_type("$A", None, &[("count", I32)]),
        ]);
        let mut old = old;
        old.types[2].kind = hl::hl_type_kind_HOBJ;
        old.types[2].obj = Some(HLTypeObj { name: "S".into(), fields: vec![HLObjField { name: "a".into(), type_: TypeRef(I32), hashed_name: 0 }], ..Default::default() });
        new.types[2].kind = hl::hl_type_kind_HOBJ;
        let why = keep(&old, &mut new, 64).unwrap_err();
        assert!(why.contains("the statics of A changed"), "{why}");
    }

    /// `A` with its statics object, which binds `constructor` as
    /// `__constructor__`, and a constructor that runs `ops` on `this`.
    fn with_constructor(fields: &[(&str, usize)], ops: Vec<Opcode>) -> DecodedBytecode {
        in_package("A", "$A", fields, ops)
    }

    fn in_package(
        class: &str,
        statics: &str,
        fields: &[(&str, usize)],
        ops: Vec<Opcode>,
    ) -> DecodedBytecode {
        let mut bc = program(vec![
            class_type(class, None, fields),
            class_type(statics, None, &[("__constructor__", PTR)]),
        ]);
        bc.types[3].obj.as_mut().unwrap().bindings = vec![0, 7];
        let mut ctor = HLFunction::with_body(ops, Vec::new());
        ctor.findex = 7;
        ctor.regs = vec![TypeRef(2), TypeRef(I32), TypeRef(PTR)];
        bc.functions.push(ctor);
        bc
    }

    fn set_this(field: usize, src: u32) -> Opcode {
        Opcode::SetThis { field: RefField(field), src: Reg(src) }
    }

    #[test]
    fn a_constant_initialiser_is_accepted() {
        let old = with_constructor(&[("n", I32)], vec![]);
        for value in [
            Opcode::Int { dst: Reg(1), ptr: RefInt(0) },
            Opcode::Null { dst: Reg(1) },
        ] {
            let mut new = with_constructor(
                &[("n", I32), ("m", I32)],
                vec![value, set_this(1, 1), Opcode::Ret { ret: Reg(1) }],
            );
            keep(&old, &mut new, 64).unwrap();
        }
        // Through a copy.
        let mut new = with_constructor(
            &[("n", I32), ("m", I32)],
            vec![
                Opcode::Int { dst: Reg(1), ptr: RefInt(0) },
                Opcode::Mov { dst: Reg(2), src: Reg(1) },
                Opcode::SetField { obj: Reg(0), field: RefField(1), src: Reg(2) },
            ],
        );
        keep(&old, &mut new, 64).unwrap();
    }

    /// The compiler puts the `$` before the last part of the name.
    #[test]
    fn the_constructor_of_a_class_in_a_package_is_found() {
        let call = Opcode::Call1 { dst: Reg(1), fun: RefFun(9), arg0: Reg(2) };
        let old = in_package("p.q.A", "p.q.$A", &[("n", I32)], vec![]);
        let mut new = in_package(
            "p.q.A",
            "p.q.$A",
            &[("n", I32), ("m", I32)],
            vec![call, set_this(1, 1)],
        );
        let why = keep(&old, &mut new, 64).unwrap_err();
        assert!(why.contains("p.q.A.m is set by a constructor"), "{why}");
    }

    #[test]
    fn a_static_added_to_a_class_in_a_package_is_named() {
        let old = in_package("p.A", "p.$A", &[], vec![]);
        let mut new = in_package("p.A", "p.$A", &[], vec![]);
        new.types[3].obj.as_mut().unwrap().fields.push(HLObjField {
            name: "count".into(),
            type_: TypeRef(I32),
            hashed_name: 0,
        });
        let why = keep(&old, &mut new, 64).unwrap_err();
        assert!(why.contains("the statics of p.A changed"), "{why}");
    }

    /// The constructor of the new program names the fields by its own
    /// numbering, which is not where the running class puts the new one.
    #[test]
    fn an_initialiser_of_a_field_declared_between_the_others_is_found() {
        let old = with_constructor(&[("a", I32), ("b", I32)], vec![]);
        let call = Opcode::Call1 { dst: Reg(1), fun: RefFun(9), arg0: Reg(2) };
        // `x` is the second field of the new program and the third of the class.
        let mut new = with_constructor(
            &[("a", I32), ("x", I32), ("b", I32)],
            vec![call, set_this(1, 1)],
        );
        let why = keep(&old, &mut new, 64).unwrap_err();
        assert!(why.contains("A.x is set by a constructor"), "{why}");
        // `b` is set to a constant and is not the added field.
        let mut new = with_constructor(
            &[("a", I32), ("x", I32), ("b", I32)],
            vec![
                Opcode::Int { dst: Reg(1), ptr: RefInt(0) },
                set_this(2, 1),
            ],
        );
        keep(&old, &mut new, 64).unwrap();
    }

    #[test]
    fn an_initialiser_that_computes_is_refused() {
        let old = with_constructor(&[("n", I32)], vec![]);
        let call = Opcode::Call1 { dst: Reg(1), fun: RefFun(9), arg0: Reg(2) };
        let mut new = with_constructor(
            &[("n", I32), ("m", I32)],
            vec![call.clone(), set_this(1, 1)],
        );
        let why = keep(&old, &mut new, 64).unwrap_err();
        assert!(why.contains("A.m is set by a constructor with something that is not a constant"), "{why}");
        // A value the constructor was given.
        let mut new = with_constructor(&[("n", I32), ("m", I32)], vec![set_this(1, 2)]);
        assert!(keep(&old, &mut new, 64).is_err());
    }

    /// The two ends of an `if` meet at the store: the `null` is the last write
    /// before it in the ops, but the call may have been the one that ran.
    #[test]
    fn a_value_a_branch_may_have_set_is_not_a_constant() {
        let old = with_constructor(&[("n", I32)], vec![]);
        let call = Opcode::Call1 { dst: Reg(1), fun: RefFun(9), arg0: Reg(2) };
        let flag = Reg(2);
        // m = flag != null ? f(flag) : null
        let mut new = with_constructor(
            &[("n", I32), ("m", PTR)],
            vec![
                Opcode::JNull { reg: flag, offset: 2 },
                call.clone(),
                Opcode::JAlways { offset: 1 },
                Opcode::Null { dst: Reg(1) },
                set_this(1, 1),
            ],
        );
        let why = keep(&old, &mut new, 64).unwrap_err();
        assert!(why.contains("A.m is set by a constructor"), "{why}");
        // m = f(flag); if (flag == null) m = null;
        let mut new = with_constructor(
            &[("n", I32), ("m", PTR)],
            vec![
                call.clone(),
                Opcode::JNotNull { reg: flag, offset: 1 },
                Opcode::Null { dst: Reg(1) },
                set_this(1, 1),
            ],
        );
        assert!(keep(&old, &mut new, 64).is_err());
        // A jump that goes past the store, or lands before the write, leaves
        // the straight run from the write to the store alone.
        let mut new = with_constructor(
            &[("n", I32), ("m", PTR)],
            vec![
                Opcode::JNull { reg: flag, offset: 2 },
                Opcode::Null { dst: Reg(1) },
                set_this(1, 1),
                Opcode::Ret { ret: Reg(1) },
            ],
        );
        keep(&old, &mut new, 64).unwrap();
    }

    /// The static a program built with `-D ashui_hot_reload` has.
    fn mark_state_on_first_use(bc: &mut DecodedBytecode) {
        bc.types.push(class_type(
            "ashui.ui.$HotReload",
            None,
            &[("stateOnFirstUse", I32)],
        ));
    }

    #[test]
    fn a_state_field_needs_no_initialiser_when_the_program_makes_it_on_first_use() {
        let old = with_constructor(&[("n", I32)], vec![]);
        let call = Opcode::Call1 { dst: Reg(1), fun: RefFun(9), arg0: Reg(2) };
        let built = || {
            with_constructor(
                &[("n", I32), ("step__state", PTR)],
                vec![call.clone(), set_this(1, 1)],
            )
        };
        let mut new = built();
        mark_state_on_first_use(&mut new);
        keep(&old, &mut new, 64).unwrap();
        // Without the marker the accessors would read the null.
        let mut new = built();
        let why = keep(&old, &mut new, 64).unwrap_err();
        assert!(why.contains("A.step__state"), "{why}");
        assert!(why.contains("-D ashui_hot_reload"), "{why}");
        // The marker does not excuse a field that is not a state field.
        let mut new = with_constructor(
            &[("n", I32), ("m", PTR)],
            vec![call.clone(), set_this(1, 1)],
        );
        mark_state_on_first_use(&mut new);
        assert!(keep(&old, &mut new, 64).is_err());

        // Only a field the reload adds is checked: a method that computes one
        // the class has is nothing to do with it.
        let old = with_constructor(&[("n", I32)], vec![call.clone(), set_this(0, 1)]);
        let mut new = with_constructor(&[("n", I32), ("m", I32)], vec![call, set_this(0, 1)]);
        keep(&old, &mut new, 64).unwrap();
    }
    /// The interfaces a class implements leave a field in it each, with no
    /// name.
    #[test]
    fn unnamed_fields_do_not_stop_a_class_growing() {
        let interfaces = || vec![class_type("I1", None, &[]), class_type("I2", None, &[])];
        let with = |fields: &[(&str, usize)]| {
            let mut types = interfaces();
            types.push(class_type("A", None, fields));
            program(types)
        };
        let old = with(&[("n", I32), ("", 2), ("", 3), ("", 3)]);
        let mut new = with(&[("n", I32), ("", 2), ("", 3), ("", 3), ("m", I32)]);
        keep(&old, &mut new, 64).unwrap();
        assert_eq!(names(&new, 4), vec!["n", "", "", "", "m"]);
        // A new interface is a field the class gains.
        let mut new = with(&[("n", I32), ("", 2), ("", 3), ("", 3), ("", 2)]);
        keep(&old, &mut new, 64).unwrap();
        // One it drops is a field it loses.
        let mut new = with(&[("n", I32), ("", 2), ("", 3)]);
        keep(&old, &mut new, 64).unwrap();
        assert_eq!(names(&new, 4).len(), 4);
    }

    /// The running order is what the next reload finds, whichever order the
    /// source declares them in.
    #[test]
    fn a_reorder_leaves_the_running_order_for_the_next_reload() {
        let old = program(vec![class_type("A", None, &[("a", I32), ("b", PTR)])]);
        let mut first = program(vec![class_type("A", None, &[("b", PTR), ("a", I32)])]);
        keep(&old, &mut first, 64).unwrap();
        assert_eq!(names(&first, 2), vec!["a", "b"]);
        let mut again = program(vec![class_type("A", None, &[("b", PTR), ("a", I32)])]);
        keep(&first, &mut again, 64).unwrap();
        assert_eq!(names(&again, 2), vec!["a", "b"]);
    }

    #[test]
    fn a_class_whose_objects_something_lays_out_in_a_row_cannot_change_size() {
        let rows = HashSet::from([2]);
        let old = program(vec![class_type("A", None, &[("n", I32)])]);
        // A reference after an int needs 8 bytes more than the 16 it has.
        let mut new = program(vec![class_type("A", None, &[("n", I32), ("m", PTR)])]);
        let why = keep_order(&old, &mut new, &mut Kept::default(), 64, Some(&rows), &[]).unwrap_err();
        assert!(why.contains("A gained m, and a CArray of it"), "{why}");
        // An int fits the padding at the end of the class, and the size stays.
        let mut new = program(vec![class_type("A", None, &[("n", I32), ("m", I32)])]);
        keep_order(&old, &mut new, &mut Kept::default(), 64, Some(&rows), &[]).unwrap();
        // Reordering moves no object.
        let old = program(vec![class_type("A", None, &[("a", I32), ("b", I32)])]);
        let mut new = program(vec![class_type("A", None, &[("b", I32), ("a", I32)])]);
        keep_order(&old, &mut new, &mut Kept::default(), 64, Some(&rows), &[]).unwrap();
        // If the runtime cannot say, a change of size is refused.
        let old = program(vec![class_type("A", None, &[("n", I32)])]);
        let mut new = program(vec![class_type("A", None, &[("n", I32), ("m", PTR)])]);
        let why = keep_order(&old, &mut new, &mut Kept::default(), 64, None, &[]).unwrap_err();
        assert!(why.contains("cannot say whether a CArray holds it"), "{why}");
    }
    /// Whether a reload that adds `m` is accepted when the constructor of the
    /// new program is `ops`.
    fn accepts(ops: Vec<Opcode>) -> bool {
        let old = with_constructor(&[("n", I32)], vec![]);
        let mut new = with_constructor(&[("n", I32), ("m", PTR)], ops);
        keep(&old, &mut new, 64).is_ok()
    }

    #[test]
    fn where_the_jumps_land_decides_whether_a_value_is_a_constant() {
        let null = || Opcode::Null { dst: Reg(1) };
        let call = || Opcode::Call1 { dst: Reg(1), fun: RefFun(9), arg0: Reg(2) };
        let flag = Reg(2);
        // 0: jump to the write (a loop back to it); the write runs again before the store
        assert!(accepts(vec![
            null(),
            Opcode::JNotNull { reg: flag, offset: -2 },
            set_this(1, 1),
        ]));
        // 0 JNull -> 2, the op after the write: the call may be what reaches the store
        assert!(!accepts(vec![
            call(),
            Opcode::JNull { reg: flag, offset: 1 },
            null(),
            set_this(1, 1),
        ]));
        // a copy over a merge: r3 = null on one path and a call on the other, then r1 = r3
        assert!(!accepts(vec![
            Opcode::JNull { reg: flag, offset: 2 },
            Opcode::Call1 { dst: Reg(3), fun: RefFun(9), arg0: flag },
            Opcode::JAlways { offset: 1 },
            Opcode::Null { dst: Reg(3) },
            Opcode::Mov { dst: Reg(1), src: Reg(3) },
            set_this(1, 1),
        ]));
        // a copy with no merge in between
        assert!(accepts(vec![
            Opcode::Null { dst: Reg(3) },
            Opcode::Mov { dst: Reg(1), src: Reg(3) },
            set_this(1, 1),
        ]));
        // a trap whose handler is the store: the write may not have run
        assert!(!accepts(vec![
            Opcode::Trap { exc: Reg(4), offset: 2 },
            null(),
            Opcode::EndTrap { exc: Reg(4) },
            set_this(1, 1),
        ]));
        // a switch that lands between the write and the store
        assert!(!accepts(vec![
            null(),
            Opcode::Switch { reg: flag, offsets: vec![1], end: 1 },
            Opcode::Nop,
            set_this(1, 1),
        ]));
    }

    /// A call can write a register whose address was taken.
    #[test]
    fn a_register_whose_address_is_taken_is_not_a_constant() {
        assert!(!accepts(vec![
            Opcode::Int { dst: Reg(1), ptr: RefInt(0) },
            Opcode::Ref { dst: Reg(3), src: Reg(1) },
            Opcode::Call1 { dst: Reg(4), fun: RefFun(9), arg0: Reg(3) },
            set_this(1, 1),
        ]));
        assert!(accepts(vec![
            Opcode::Int { dst: Reg(1), ptr: RefInt(0) },
            Opcode::Call1 { dst: Reg(4), fun: RefFun(9), arg0: Reg(2) },
            set_this(1, 1),
        ]));
    }
    /// A field the class dropped comes back: the objects built since hold
    /// nothing in it, so the constructor is held to the rule for a new field.
    #[test]
    fn a_field_that_comes_back_is_checked_like_an_added_one() {
        let call = || Opcode::Call1 { dst: Reg(1), fun: RefFun(9), arg0: Reg(2) };
        let running = with_constructor(&[("n", I32), ("m", PTR)], vec![]);
        let mut kept = Kept::default();
        let mut dropped = with_constructor(&[("n", I32)], vec![]);
        keep_order(&running, &mut dropped, &mut kept, 64, Some(&HashSet::new()), &[]).unwrap();
        assert_eq!(names(&dropped, 2), vec!["n", "m"]);
        let after_drop = kept.clone();
        let try_back = |ctor: Vec<Opcode>, fields: &[(&str, usize)], kept: &mut Kept| {
            let mut back = with_constructor(fields, ctor);
            let outcome = keep_order(&dropped, &mut back, kept, 64, Some(&HashSet::new()), &[]);
            (outcome, back)
        };
        // A constructor that builds it is refused.
        let (outcome, _) = try_back(vec![call(), set_this(1, 1)], &[("n", I32), ("m", PTR)], &mut after_drop.clone());
        let why = outcome.unwrap_err();
        assert!(why.contains("A.m is set by a constructor"), "{why}");
        assert!(why.contains("an earlier reload dropped the field"), "{why}");
        // It cannot come back as something else.
        let (outcome, _) = try_back(vec![], &[("n", I32), ("m", I32)], &mut after_drop.clone());
        let why = outcome.unwrap_err();
        assert!(why.contains("changed its type, and the class still has the field it dropped"), "{why}");
        // A constant is fine, and then the field is an ordinary one again.
        let (outcome, sets) = try_back(
            vec![Opcode::Null { dst: Reg(1) }, set_this(1, 1)],
            &[("n", I32), ("m", PTR)],
            &mut kept,
        );
        outcome.unwrap();
        let mut later = with_constructor(&[("n", I32), ("m", PTR)], vec![call(), set_this(1, 1)]);
        keep_order(&sets, &mut later, &mut kept, 64, Some(&HashSet::new()), &[]).unwrap();
    }
    /// What the class dropped is remembered by reloads that leave it alone,
    /// and a field that comes back with nothing else changing does not hold up
    /// a new subclass.
    #[test]
    fn a_dropped_field_is_remembered_across_reloads_that_leave_it_alone() {
        let call = || Opcode::Call1 { dst: Reg(1), fun: RefFun(9), arg0: Reg(2) };
        let running = with_constructor(&[("n", I32), ("m", PTR)], vec![]);
        let mut kept = Kept::default();
        let mut dropped = with_constructor(&[("n", I32)], vec![]);
        keep_order(&running, &mut dropped, &mut kept, 64, Some(&HashSet::new()), &[]).unwrap();
        // A reload that does not mention the field again.
        let mut quiet = with_constructor(&[("n", I32)], vec![]);
        keep_order(&dropped, &mut quiet, &mut kept, 64, Some(&HashSet::new()), &[]).unwrap();
        assert_eq!(names(&quiet, 2), vec!["n", "m"]);
        // It comes back, set by a computation.
        let mut computes = with_constructor(&[("n", I32), ("m", PTR)], vec![call(), set_this(1, 1)]);
        let why = keep_order(&quiet, &mut computes, &mut kept.clone(), 64, Some(&HashSet::new()), &[])
            .unwrap_err();
        assert!(why.contains("an earlier reload dropped the field"), "{why}");
        // It comes back with nothing to run, and a class that extends the
        // class arrives in the same reload: the lists are the running ones.
        let mut back = with_constructor(&[("n", I32), ("m", PTR)], vec![]);
        back.types.push(class_type("B", Some(2), &[]));
        keep_order(&quiet, &mut back, &mut kept, 64, Some(&HashSet::new()), &[]).unwrap();
        assert!(kept.ghosts.is_empty());
    }
}
