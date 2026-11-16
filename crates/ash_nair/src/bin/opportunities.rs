//! Counts, over some classes of a TAST file, what the typed program says that HashLink's types
//! drop, and how often the code would use it.
//!
//!     cargo run -p ash_nair --bin opportunities -- program.tast Class [Class ...]
//!
//! The counts are static: one per place in the source, not per execution. They size the
//! opportunity; they do not measure a gain.

use std::collections::{BTreeMap, HashSet};

use ash_nair::lower::Module;
use ash_tast::Tast;
use ash_tast::format::*;

#[derive(Default)]
struct Use {
    reads: u32,
    writes_in_constructor: u32,
    writes_elsewhere: u32,
}

#[derive(Default)]
struct Totals {
    object_literals: u32,
    anon_reads: u32,
    dynamic_reads: u32,
    interface_calls: u32,
    method_calls: u32,
    new_sites: u32,
}

/// The node under any parentheses.
fn bare(tast: Tast, mut id: u32) -> u32 {
    while tast.node(id).kind == node_kind::PARENTHESIS {
        id = tast.kid_ids(tast.node(id)).start;
    }
    id
}

/// The field an instance access names, when the node is one.
fn instance_field(tast: Tast, id: u32) -> Option<u32> {
    let n = tast.node(id);
    (n.kind == node_kind::FIELD && n.a.get() == field_access::INSTANCE && n.b.get() != NONE)
        .then(|| n.b.get())
}

fn main() {
    let mut args = std::env::args().skip(1);
    let Some(path) = args.next() else {
        eprintln!("usage: opportunities program.tast Class [Class ...]");
        std::process::exit(2);
    };
    let wanted: Vec<String> = args.collect();
    let bytes = std::fs::read(path).expect("read the TAST file");
    let tast = Tast::parse(&bytes).expect("a valid TAST file");
    let names = Module::new(tast);

    // classes that something in the file extends
    let mut extended = HashSet::new();
    for c in tast.classes() {
        if c.super_type.get() != NONE {
            let t = tast.types()[c.super_type.get() as usize];
            if t.kind == type_kind::CLASS {
                extended.insert(t.def.get());
            }
        }
    }

    let mut classes = Vec::new();
    for (ci, c) in tast.classes().iter().enumerate() {
        if wanted.iter().any(|w| w == names.types.class_name(ci as u32)) {
            classes.push((ci as u32, *c));
        }
    }
    let (mut finals, mut interfaces, mut effectively_final) = (0, 0, 0);
    let mut uses: BTreeMap<u32, Use> = BTreeMap::new();
    let mut declared_final = HashSet::new();
    let mut totals = Totals::default();

    for (ci, c) in &classes {
        let flags = c.flags.get();
        if flags & class_flag::INTERFACE != 0 {
            interfaces += 1;
        } else if flags & class_flag::FINAL != 0 {
            finals += 1;
        } else if !extended.contains(ci) {
            effectively_final += 1;
        }
        for f in c.fields_first.get()..c.fields_first.get() + c.fields_count.get() {
            let fr = tast.fields()[f as usize];
            let ff = fr.flags.get();
            if ff & (field_flag::METHOD | field_flag::STATIC | field_flag::NOT_PHYSICAL) == 0 {
                uses.entry(f).or_default();
                if ff & field_flag::FINAL != 0 {
                    declared_final.insert(f);
                }
            }
        }
    }

    for (_, c) in &classes {
        for f in c.fields_first.get()..c.fields_first.get() + c.fields_count.get() {
            let fr = tast.fields()[f as usize];
            if fr.func.get() == NONE {
                continue;
            }
            let in_constructor = names.types.field_name(f) == "new";
            let root = tast.funcs()[fr.func.get() as usize].root.get();
            if root == NONE {
                continue;
            }
            let mut targets: HashSet<u32> = HashSet::new();
            let mut stack = vec![root];
            while let Some(id) = stack.pop() {
                let n = tast.node(id);
                let kids = tast.kid_ids(n);
                match n.kind {
                    node_kind::BINOP => {
                        let op = BINOPS[n.a.get() as usize];
                        if matches!(op, "OpAssign" | "OpAssignOp") && kids.len() == 2 {
                            let lhs = bare(tast, kids.start);
                            if let Some(field) = instance_field(tast, lhs) {
                                let u = uses.entry(field).or_default();
                                if in_constructor {
                                    u.writes_in_constructor += 1;
                                } else {
                                    u.writes_elsewhere += 1;
                                }
                                targets.insert(lhs);
                            }
                        }
                    }
                    node_kind::UNOP => {
                        let op = UNOPS[n.a.get() as usize];
                        if matches!(op, "OpIncrement" | "OpDecrement") && kids.len() == 1 {
                            let operand = bare(tast, kids.start);
                            if let Some(field) = instance_field(tast, operand) {
                                uses.entry(field).or_default().writes_elsewhere += 1;
                                targets.insert(operand);
                            }
                        }
                    }
                    node_kind::FIELD if !targets.contains(&id) => match n.a.get() {
                        field_access::INSTANCE if n.b.get() != NONE => {
                            let ff = tast.fields()[n.b.get() as usize].flags.get();
                            if ff & field_flag::METHOD != 0 {
                                totals.method_calls += 1;
                                let owner = tast.fields()[n.b.get() as usize].owner.get();
                                if tast.classes()[owner as usize].flags.get()
                                    & class_flag::INTERFACE
                                    != 0
                                {
                                    totals.interface_calls += 1;
                                }
                            } else if let Some(u) = uses.get_mut(&n.b.get()) {
                                u.reads += 1;
                            }
                        }
                        field_access::ANON => totals.anon_reads += 1,
                        field_access::DYNAMIC => totals.dynamic_reads += 1,
                        _ => {}
                    },
                    node_kind::OBJECT_DECL => totals.object_literals += 1,
                    node_kind::NEW => totals.new_sites += 1,
                    _ => {}
                }
                stack.extend(kids);
            }
        }
    }

    let total_fields = uses.len();
    let never_rewritten: Vec<_> = uses
        .iter()
        .filter(|(f, u)| declared_final.contains(f) || u.writes_elsewhere == 0)
        .collect();
    let reads_all: u32 = uses.values().map(|u| u.reads).sum();
    let reads_of_never_rewritten: u32 = never_rewritten.iter().map(|(_, u)| u.reads).sum();
    println!(
        "classes {}: {finals} final, {interfaces} interface, {effectively_final} not final but extended by nothing in the file",
        classes.len()
    );
    println!(
        "instance fields {total_fields}: {} declared final, {} never written outside a constructor (declared final included)",
        declared_final.len(),
        never_rewritten.len()
    );
    println!(
        "field reads {reads_all}, of which {reads_of_never_rewritten} are of a field never written outside a constructor"
    );
    println!(
        "new {}, object literals {}, field reads on an anonymous structure {}, on a Dynamic {}, method calls {}, through an interface {}",
        totals.new_sites,
        totals.object_literals,
        totals.anon_reads,
        totals.dynamic_reads,
        totals.method_calls,
        totals.interface_calls
    );
}
