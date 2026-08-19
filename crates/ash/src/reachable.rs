//! # Which functions a program can actually run
//!
//! A `.hl` module carries the whole Haxe standard library that the compiler
//! linked, not the part the program calls. On this corpus that is most of it:
//! `test_mandelbrot_small.hl` declares 339 functions and executes 19.
//!
//! Nothing is *wrong* with compiling the other 320 — they are correct code that
//! never runs — but two things quietly pay for them:
//!
//! * **Whole-module compilation.** The full-JIT seeds from the entrypoint, so
//!   it is already demand-driven, but any pass that iterates `bc.functions`
//!   directly is doing ~17x the necessary work.
//! * **Reports.** [`crate::air_pipeline::osr_report`] and the AIR sweep both
//!   walk every declared function, so a denominator like "58 of 341 eligible"
//!   is mostly counting loops in code that cannot execute. That reads as a
//!   dismal eligibility rate when the real one may be fine.
//!
//! ## Soundness
//!
//! Direct calls and static/instance closures name a `findex` outright, so those
//! edges are exact. The rest of HashLink's dispatch is not:
//!
//! * `CallMethod`/`CallThis` select a vtable slot on the receiver's *static*
//!   type, and any subtype may override it.
//! * `VirtualClosure` takes its field from a register, so the name is not
//!   statically known at all.
//! * `Reflect`/`Type.createInstance` reach anything with a runtime name.
//!
//! So this over-approximates deliberately, but only where it must:
//!
//! * A `CallMethod` names an exact vtable slot, so that edge is resolved
//!   against `pindex`; only the *receiver* is widened, to every subtype.
//! * Over half of all protos carry `pindex == -1` — no vtable slot at all —
//!   and are reached by **name** through the virtual-field path, which is how
//!   a `for` loop finds `hasNext`/`next`. Rooting every slotless method of a
//!   live type was tried and put 86% of the module in the live set. Instead
//!   the walk collects the field names that live code actually looks up
//!   structurally, and roots a slotless method only when its name is among
//!   them. Since a name can be discovered after the type that defines it was
//!   already visited, the walk runs to a fixed point.
//!
//! The result is an upper bound on what can run — safe as a denominator or to
//! skip work, never as grounds for deleting a function. Anything reflection can
//! reach is explicitly **not** covered; see [`Reachability::reflection_risk`].

use crate::bytecode::DecodedBytecode;
use air::opcodes::Opcode;
use std::collections::{HashMap, HashSet};

/// What [`analyze`] concluded about a module.
#[derive(Debug, Default)]
pub struct Reachability {
    /// Findexes that can be entered, functions and natives alike.
    pub live: HashSet<i32>,
    /// Type indices whose vtable was pulled in.
    pub live_types: HashSet<usize>,
    pub total_functions: usize,
    pub total_natives: usize,
    /// Types carrying a `global_value`, which reflection can name at runtime.
    /// A caller that must be sound in the presence of `Type.createInstance`
    /// has to treat these as roots; a *report* can ignore them.
    pub reflection_risk: usize,
}

impl Reachability {
    /// Live functions (excluding natives), and the module's total.
    pub fn function_counts(&self, bc: &DecodedBytecode) -> (usize, usize) {
        let live = bc
            .functions
            .iter()
            .filter(|f| self.live.contains(&f.findex))
            .count();
        (live, self.total_functions)
    }

    pub fn is_live(&self, findex: i32) -> bool {
        self.live.contains(&findex)
    }
}

/// How a type came to be live, which decides what of it is pulled in.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Dispatch {
    /// `CallMethod`/`CallThis`: the vtable slot is known exactly.
    Slot(usize),
    /// `VirtualClosure`: the field is named by a register, so nothing is known.
    ByName,
    /// `New`: the type is instantiated but no method is named here. Its
    /// slotless methods still become reachable, since a `for` loop can find
    /// them by name later.
    Allocate,
}

/// Every type that transitively extends `root`, plus `root` itself.
fn subtree(children: &HashMap<usize, Vec<usize>>, root: usize) -> Vec<usize> {
    let mut out = vec![root];
    let mut i = 0;
    while i < out.len() {
        if let Some(kids) = children.get(&out[i]) {
            for &k in kids {
                if !out.contains(&k) {
                    out.push(k);
                }
            }
        }
        i += 1;
    }
    out
}

/// Upper bound on the functions a program can enter, from the entrypoint out.
/// Upper bound on the functions a program can enter, from the entrypoint out.
pub fn analyze(bc: &DecodedBytecode) -> Reachability {
    let mut r = Reachability {
        total_functions: bc.functions.len(),
        total_natives: bc.natives.len(),
        ..Default::default()
    };

    let by_findex: HashMap<i32, usize> = bc
        .functions
        .iter()
        .enumerate()
        .map(|(i, f)| (f.findex, i))
        .collect();

    // Subclass edges, so reaching a method on a base type also reaches every
    // override an actual receiver might dispatch to.
    let mut children: HashMap<usize, Vec<usize>> = HashMap::new();
    for (i, t) in bc.types.iter().enumerate() {
        if let Some(o) = &t.obj {
            if o.global_value > 0 {
                r.reflection_risk += 1;
            }
            if let Some(sup) = &o.super_ {
                children.entry(sup.0).or_default().push(i);
            }
        }
    }

    // Field names live code looks up structurally, on a virtual receiver.
    // A slotless proto is a root only if its name is in here.
    let mut structural: HashSet<String> = HashSet::new();
    // A `VirtualClosure` names its field with a register, so once one is
    // reached no name can be ruled out again.
    let mut any_name = false;

    // The name set only grows, and a bigger name set can only reach more code,
    // so re-running the walk converges. Two iterations is the normal case: one
    // to discover the names, one to use them.
    loop {
        let (before_live, before_names, before_any) = (r.live.len(), structural.len(), any_name);
        r.live.clear();
        r.live_types.clear();
        let mut expanded: HashSet<usize> = HashSet::new();
        let mut worklist: Vec<i32> = vec![bc.entrypoint as i32];

        while let Some(fx) = worklist.pop() {
            if !r.live.insert(fx) {
                continue;
            }
            let Some(&idx) = by_findex.get(&fx) else {
                continue; // a native: live, but has no body to walk
            };
            let f = &bc.functions[idx];
            let reg_type = |reg: u32| -> Option<usize> { f.regs.get(reg as usize).map(|t| t.0) };

            // Resolve a field index against a receiver's type, recording the
            // name when the receiver is virtual and returning the vtable slot
            // when it is a plain object.
            let mut note_field = |ty: usize, field: usize, structural: &mut HashSet<String>| {
                if let Some(v) = bc.types.get(ty).and_then(|t| t.virt.as_ref()) {
                    if let Some(fl) = v.fields.get(field) {
                        structural.insert(fl.name.clone());
                    }
                }
            };

            for op in &f.ops {
                match op {
                    Opcode::Call0 { fun, .. }
                    | Opcode::Call1 { fun, .. }
                    | Opcode::Call2 { fun, .. }
                    | Opcode::Call3 { fun, .. }
                    | Opcode::Call4 { fun, .. }
                    | Opcode::CallN { fun, .. }
                    | Opcode::StaticClosure { fun, .. }
                    | Opcode::InstanceClosure { fun, .. } => worklist.push(fun.0 as i32),

                    Opcode::CallMethod { field, args, .. } => {
                        if let Some(ty) = args.first().and_then(|a| reg_type(a.0)) {
                            note_field(ty, field.0, &mut structural);
                            dispatch(
                                bc,
                                &children,
                                ty,
                                Dispatch::Slot(field.0),
                                &structural,
                                any_name,
                                &mut expanded,
                                &mut r.live_types,
                                &mut worklist,
                            );
                        }
                    }
                    Opcode::CallThis { field, .. } => {
                        if let Some(ty) = reg_type(0) {
                            note_field(ty, field.0, &mut structural);
                            dispatch(
                                bc,
                                &children,
                                ty,
                                Dispatch::Slot(field.0),
                                &structural,
                                any_name,
                                &mut expanded,
                                &mut r.live_types,
                                &mut worklist,
                            );
                        }
                    }
                    // Reading a function-typed field off a virtual is how a
                    // structural call site is spelled when it is not a direct
                    // CallMethod. The name matters even though no call is here.
                    Opcode::Field { obj, field, .. } => {
                        if let Some(ty) = reg_type(obj.0) {
                            note_field(ty, field.0, &mut structural);
                        }
                    }
                    // Field named by a register: no name can be ruled out.
                    Opcode::VirtualClosure { obj, .. } => {
                        any_name = true;
                        if let Some(ty) = reg_type(obj.0) {
                            dispatch(
                                bc,
                                &children,
                                ty,
                                Dispatch::ByName,
                                &structural,
                                any_name,
                                &mut expanded,
                                &mut r.live_types,
                                &mut worklist,
                            );
                        }
                    }
                    // A name given as a literal string reaches whatever bears
                    // it, so it counts as a structural lookup.
                    Opcode::DynGet { field, .. } | Opcode::DynSet { field, .. } => {
                        if let Some(n) = bc.strings.get(field.0) {
                            structural.insert(n.clone());
                        }
                    }
                    // Instantiating a type is what makes its slotless methods
                    // findable by name later.
                    Opcode::New { dst } => {
                        if let Some(ty) = reg_type(dst.0) {
                            dispatch(
                                bc,
                                &children,
                                ty,
                                Dispatch::Allocate,
                                &structural,
                                any_name,
                                &mut expanded,
                                &mut r.live_types,
                                &mut worklist,
                            );
                        }
                    }
                    _ => {}
                }
            }
        }

        if r.live.len() == before_live && structural.len() == before_names && any_name == before_any
        {
            break;
        }
    }

    r
}

/// Pull one type (and every subtype, since any of them may be the receiver)
/// into the live set, and queue the methods this dispatch can select.
#[allow(clippy::too_many_arguments)]
fn dispatch(
    bc: &DecodedBytecode,
    children: &HashMap<usize, Vec<usize>>,
    ty: usize,
    how: Dispatch,
    structural: &HashSet<String>,
    any_name: bool,
    expanded: &mut HashSet<usize>,
    live_types: &mut HashSet<usize>,
    worklist: &mut Vec<i32>,
) {
    for t in subtree(children, ty) {
        live_types.insert(t);
        let Some(obj) = bc.types.get(t).and_then(|x| x.obj.as_ref()) else {
            continue;
        };
        // Slot-independent roots, taken once per type per fixed-point round.
        if expanded.insert(t) {
            // `pindex == -1` means no vtable slot, so no `CallMethod` can name
            // it — yet it still runs, found by name through the virtual-field
            // path. That is how a `for` loop reaches `hasNext`/`next`, and
            // filtering on the slot alone reported stdlib's iterator methods
            // unreachable while they were executing.
            for p in obj
                .proto
                .iter()
                .filter(|p| p.pindex < 0 && (any_name || structural.contains(&p.name)))
            {
                worklist.push(p.findex);
            }
            // `bindings` is flat (field, findex) pairs: a closure installed on
            // the vtable, which is how Haxe gives an object a function-valued
            // field.
            for pair in obj.bindings.chunks_exact(2) {
                worklist.push(pair[1]);
            }
        }
        match how {
            Dispatch::Slot(sl) => {
                for p in obj
                    .proto
                    .iter()
                    .filter(|p| p.pindex >= 0 && p.pindex as usize == sl)
                {
                    worklist.push(p.findex);
                }
            }
            Dispatch::ByName => {
                for p in &obj.proto {
                    worklist.push(p.findex);
                }
            }
            // Allocation names no method; the slotless roots above are its
            // whole contribution.
            Dispatch::Allocate => {}
        }
    }
}

/// Every static reference to `target`, for working out why a function that
/// demonstrably runs was not predicted to. Reports references the walk follows
/// *and* ones it does not, since the second kind is what a hole is made of.
pub fn why(bc: &DecodedBytecode, target: i32) -> Vec<String> {
    let mut out = Vec::new();
    for f in &bc.functions {
        for (i, op) in f.ops.iter().enumerate() {
            let named = match op {
                Opcode::Call0 { fun, .. }
                | Opcode::Call1 { fun, .. }
                | Opcode::Call2 { fun, .. }
                | Opcode::Call3 { fun, .. }
                | Opcode::Call4 { fun, .. }
                | Opcode::CallN { fun, .. }
                | Opcode::StaticClosure { fun, .. }
                | Opcode::InstanceClosure { fun, .. } => Some(fun.0 as i32),
                _ => None,
            };
            if named == Some(target) {
                out.push(format!(
                    "direct: findex={} {} op[{i}] {op:?}",
                    f.findex,
                    f.name()
                ));
            }
        }
    }
    for (ti, t) in bc.types.iter().enumerate() {
        let Some(o) = &t.obj else { continue };
        for p in &o.proto {
            if p.findex == target {
                out.push(format!(
                    "proto: type[{ti}] {} slot pindex={} name={}",
                    o.name, p.pindex, p.name
                ));
            }
        }
        for pair in o.bindings.chunks_exact(2) {
            if pair[1] == target {
                out.push(format!(
                    "binding: type[{ti}] {} field={} -> findex={}",
                    o.name, pair[0], pair[1]
                ));
            }
        }
    }
    if out.is_empty() {
        out.push(format!(
            "findex={target} has no static reference at all: entered only through \
             a runtime name (Reflect/Type.createInstance) or as the entrypoint"
        ));
    }
    out
}

/// One line per fact, ending in the summary — the shape the AIR and OSR
/// sweeps already use, so the three read the same way.
pub fn report(bc: &DecodedBytecode) -> Vec<String> {
    let r = analyze(bc);
    let (live_fns, total_fns) = r.function_counts(bc);
    let live_natives = bc
        .natives
        .iter()
        .filter(|n| r.live.contains(&n.findex))
        .count();
    let pct = if total_fns == 0 {
        0.0
    } else {
        100.0 * live_fns as f64 / total_fns as f64
    };
    let mut live_list: Vec<i32> = bc
        .functions
        .iter()
        .map(|f| f.findex)
        .filter(|fx| r.live.contains(fx))
        .collect();
    live_list.sort_unstable();
    vec![
        format!("functions {live_fns}/{total_fns} reachable ({pct:.1}%)"),
        format!(
            "findexes {}",
            live_list
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join(",")
        ),
        format!("natives   {live_natives}/{}", r.total_natives),
        format!("vtables   {} types pulled in", r.live_types.len()),
        {
            let (mut slotless, mut slotted) = (0usize, 0usize);
            for t in &bc.types {
                if let Some(o) = &t.obj {
                    for pr in &o.proto {
                        if pr.pindex < 0 {
                            slotless += 1
                        } else {
                            slotted += 1
                        }
                    }
                }
            }
            format!("protos    {slotless} slotless / {slotted} slotted")
        },
        format!(
            "reflection {} types carry a global_value and are NOT counted as roots",
            r.reflection_risk
        ),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    /// `subtree` has to terminate on a cycle. Bytecode should not contain one,
    /// but a malformed `super_` chain must not hang the compiler.
    #[test]
    fn subtree_terminates_on_a_cycle() {
        let mut children: HashMap<usize, Vec<usize>> = HashMap::new();
        children.insert(0, vec![1]);
        children.insert(1, vec![0]);
        let got = subtree(&children, 0);
        assert_eq!(got.len(), 2);
    }

    #[test]
    fn subtree_collects_grandchildren() {
        let mut children: HashMap<usize, Vec<usize>> = HashMap::new();
        children.insert(0, vec![1]);
        children.insert(1, vec![2]);
        assert_eq!(subtree(&children, 0), vec![0, 1, 2]);
    }
}
