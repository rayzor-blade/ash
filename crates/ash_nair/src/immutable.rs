//! Which instance variables nothing writes once their object is built.
//!
//! HashLink's types do not say it, and a pass cannot find it from one function: a load of such a
//! field is the same value wherever the object is visible, so no store and no call changes it.
//! That is worth knowing because it lets the passes move and reuse the load past anything.
//!
//! A field qualifies when, over the whole program:
//! - the only writes to it are `this.f = ...` in constructors of its class or a subclass,
//! - no constructor on the way from the root class to the class that declares it, or to a class
//!   that writes it, lets `this` out before it returns: no call on `this`, no `this` passed or
//!   stored, no closure made. Otherwise a callee could read the field before it is written,
//! - nothing writes a field of that name through an anonymous structure or a Dynamic, and the
//!   program never calls `Reflect.setField` or `Reflect.setProperty`.
//!
//! The answer holds for a closed program: no hot reload, no code the file does not contain.

use std::collections::{HashMap, HashSet};

use ash_tast::Tast;
use ash_tast::format::*;

/// The node under any parentheses.
fn bare(tast: Tast, mut id: u32) -> u32 {
    while tast.node(id).kind == node_kind::PARENTHESIS {
        id = tast.kid_ids(tast.node(id)).start;
    }
    id
}

fn is_const(tast: Tast, id: u32, kind: u8) -> bool {
    let n = tast.node(id);
    n.kind == node_kind::CONST && n.flags == kind
}

/// An instance variable access, with the field it names.
fn instance_var(tast: Tast, id: u32) -> Option<u32> {
    let n = tast.node(id);
    if n.kind != node_kind::FIELD || n.a.get() != field_access::INSTANCE || n.b.get() == NONE {
        return None;
    }
    let flags = tast.fields()[n.b.get() as usize].flags.get();
    (flags & field_flag::METHOD == 0).then(|| n.b.get())
}

/// A field that can be assigned, as the typed program names the target of a write.
enum Target {
    /// An instance variable, and whether the object is `this`.
    Field { field: u32, via_this: bool },
    /// A field of an anonymous structure or a Dynamic, by name.
    Named(u32),
}

fn write_target(tast: Tast, lhs: u32) -> Option<Target> {
    let lhs = bare(tast, lhs);
    let n = tast.node(lhs);
    if n.kind != node_kind::FIELD {
        return None;
    }
    if let Some(field) = instance_var(tast, lhs) {
        let obj = bare(tast, tast.kid_ids(n).start);
        return Some(Target::Field {
            field,
            via_this: is_const(tast, obj, const_kind::THIS),
        });
    }
    matches!(n.a.get(), field_access::ANON | field_access::DYNAMIC).then(|| Target::Named(n.b.get()))
}

/// What a walk over one function body found.
#[derive(Default)]
struct Body {
    writes: Vec<(u32, bool)>,
    named_writes: Vec<u32>,
    /// `this` used for anything but reaching an instance variable, or a closure made.
    leaks_this: bool,
    reflective_write: bool,
}

fn walk(tast: Tast, root: u32) -> Body {
    let mut body = Body::default();
    let mut this_uses = Vec::new();
    let mut safe = HashSet::new();
    let mut stack = vec![root];
    while let Some(id) = stack.pop() {
        let n = tast.node(id);
        let kids = tast.kid_ids(n);
        match n.kind {
            node_kind::BINOP => {
                let op = BINOPS[n.a.get() as usize];
                if matches!(op, "OpAssign" | "OpAssignOp") && kids.len() == 2 {
                    match write_target(tast, kids.start) {
                        Some(Target::Field { field, via_this }) => body.writes.push((field, via_this)),
                        Some(Target::Named(name)) => body.named_writes.push(name),
                        None => {}
                    }
                }
            }
            node_kind::UNOP => {
                let op = UNOPS[n.a.get() as usize];
                if matches!(op, "OpIncrement" | "OpDecrement") && kids.len() == 1 {
                    match write_target(tast, kids.start) {
                        Some(Target::Field { field, via_this }) => body.writes.push((field, via_this)),
                        Some(Target::Named(name)) => body.named_writes.push(name),
                        None => {}
                    }
                }
            }
            node_kind::FIELD => {
                if instance_var(tast, id).is_some() {
                    let obj = bare(tast, kids.start);
                    if is_const(tast, obj, const_kind::THIS) {
                        safe.insert(obj);
                    }
                }
            }
            node_kind::CALL => {
                let callee = bare(tast, kids.start);
                if is_const(tast, callee, const_kind::SUPER) {
                    safe.insert(callee);
                }
                let c = tast.node(callee);
                if c.kind == node_kind::FIELD && c.a.get() == field_access::STATIC && c.b.get() != NONE
                {
                    let field = tast.fields()[c.b.get() as usize];
                    let class = tast.classes()[field.owner.get() as usize];
                    if tast.opt_string(class.name) == Some("Reflect")
                        && matches!(
                            tast.opt_string(field.name),
                            Some("setField" | "setProperty")
                        )
                    {
                        body.reflective_write = true;
                    }
                }
            }
            node_kind::CONST if n.flags == const_kind::THIS || n.flags == const_kind::SUPER => {
                this_uses.push(id);
            }
            node_kind::FUNCTION => body.leaks_this = true,
            _ => {}
        }
        stack.extend(kids);
    }
    if this_uses.iter().any(|u| !safe.contains(u)) {
        body.leaks_this = true;
    }
    body
}

/// The instance variables nothing writes once their object is built, by field id.
pub fn immutable_fields(tast: Tast) -> HashSet<u32> {
    let classes = tast.classes();
    let parent: Vec<Option<u32>> = classes
        .iter()
        .map(|c| {
            let t = (c.super_type.get() != NONE).then(|| tast.types()[c.super_type.get() as usize])?;
            (t.kind == type_kind::CLASS).then(|| t.def.get())
        })
        .collect();

    // the class whose constructor a function is
    let mut constructor_of: HashMap<u32, u32> = HashMap::new();
    for (ci, c) in classes.iter().enumerate() {
        for f in c.fields_first.get()..c.fields_first.get() + c.fields_count.get() {
            let fr = tast.fields()[f as usize];
            if fr.func.get() != NONE
                && fr.flags.get() & field_flag::STATIC == 0
                && tast.opt_string(fr.name) == Some("new")
            {
                constructor_of.insert(fr.func.get(), ci as u32);
            }
        }
    }

    let mut disqualified: HashSet<u32> = HashSet::new();
    let mut named: HashSet<&str> = HashSet::new();
    let mut leaky: HashSet<u32> = HashSet::new();
    let mut constructor_writers: HashMap<u32, HashSet<u32>> = HashMap::new();
    for (fi, func) in tast.funcs().iter().enumerate() {
        if func.root.get() == NONE {
            continue;
        }
        let body = walk(tast, func.root.get());
        if body.reflective_write {
            return HashSet::new();
        }
        for name in body.named_writes {
            named.insert(tast.string(name));
        }
        let class = constructor_of.get(&(fi as u32)).copied();
        for (field, via_this) in body.writes {
            match class {
                Some(c) if via_this => {
                    constructor_writers.entry(field).or_default().insert(c);
                }
                _ => {
                    disqualified.insert(field);
                }
            }
        }
        if let Some(c) = class
            && body.leaks_this
        {
            leaky.insert(c);
        }
    }

    // no constructor from the root down to `class` lets `this` out
    let chain_is_quiet = |mut class: u32| {
        loop {
            if leaky.contains(&class) {
                return false;
            }
            match parent[class as usize] {
                Some(p) => class = p,
                None => return true,
            }
        }
    };

    let mut out = HashSet::new();
    for (ci, c) in classes.iter().enumerate() {
        let flags = c.flags.get();
        if flags & (class_flag::EXTERN | class_flag::INTERFACE | class_flag::TYPE_PARAM) != 0 {
            continue;
        }
        if !chain_is_quiet(ci as u32) {
            continue;
        }
        for f in c.fields_first.get()..c.fields_first.get() + c.fields_count.get() {
            let fr = tast.fields()[f as usize];
            let ff = fr.flags.get();
            let storage = field_flag::METHOD
                | field_flag::STATIC
                | field_flag::NOT_PHYSICAL
                | field_flag::EXTERN
                | field_flag::DYNAMIC;
            if ff & storage != 0
                || disqualified.contains(&f)
                || tast.opt_string(fr.name).is_some_and(|n| named.contains(n))
            {
                continue;
            }
            let writers_quiet = constructor_writers
                .get(&f)
                .is_none_or(|ws| ws.iter().all(|w| chain_is_quiet(*w)));
            if writers_quiet {
                out.insert(f);
            }
        }
    }
    out
}
