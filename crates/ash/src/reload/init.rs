//! Setting up the classes and enums a reload adds.
//!
//! A program sets its classes up in its entry function, which runs once, at
//! the start: `Type.initClass` for each class, the initial value of each
//! static, the constructors of each enum. What the new program adds is never
//! reached by that. What it needs is the part of the new entry function that
//! touches what is new, so that part is made a function of its own, which
//! the interpreter runs once after the reload.

use crate::bytecode::DecodedBytecode;
use crate::opcodes::Opcode;
use crate::types::HLFunction;
use air::opcode_info::{is_pure, jump_offset, reads, successors, writes};
use std::collections::HashSet;

/// Whether `op` decides which op runs next, or ends the function.
fn is_control(op: &Opcode) -> bool {
    jump_offset(op).is_some()
        || matches!(
            op,
            Opcode::Label
                | Opcode::Switch { .. }
                | Opcode::Trap { .. }
                | Opcode::EndTrap { .. }
                | Opcode::Ret { .. }
                | Opcode::Throw { .. }
                | Opcode::Rethrow { .. }
        )
}

/// Which ops lie inside a jump: from a forward jump to the op before its
/// target, and from a backward jump's target to the jump. Branches and
/// loops, which pick what runs or how often, are made of these.
fn inside_a_jump(ops: &[Opcode]) -> Vec<bool> {
    let mut starts = vec![0i32; ops.len() + 1];
    for (i, op) in ops.iter().enumerate() {
        for target in successors(op, i, ops.len()) {
            if target == i + 1 {
                continue;
            }
            let (from, to) = if target > i {
                (i, (target - 1).min(ops.len() - 1))
            } else {
                (target, i)
            };
            starts[from] += 1;
            starts[to + 1] -= 1;
        }
    }
    let mut open = 0;
    starts[..ops.len()]
        .iter()
        .map(|d| {
            open += d;
            open > 0
        })
        .collect()
}

/// Make the function that sets up the types from `types_from` on, which the
/// new program adds, and name it in `new.reload_init`. It is the ops of the
/// entry function that touch those types, with the ops that compute what
/// they use: loading a type, naming it, calling `Type.initClass` on it,
/// giving a static its first value. `slots` is how many findexes the running
/// tables have room for. `Err` says why that part of the entry function
/// cannot run on its own.
pub fn attach_init(
    new: &mut DecodedBytecode,
    types_from: usize,
    slots: usize,
) -> Result<(), String> {
    if new.types.len() <= types_from {
        return Ok(());
    }
    let entry = new.entrypoint as usize;
    let Some(entry_fn) = new.functions.iter().find(|f| f.findex as usize == entry) else {
        return Ok(());
    };
    let ops = entry_fn.ops();
    let is_new_global = |g: usize| new.globals.get(g).is_some_and(|t| t.0 >= types_from);

    // Registers that hold something new, and the ops that do something with it.
    let mut tainted: HashSet<u32> = HashSet::new();
    let mut seeds: HashSet<usize> = HashSet::new();
    for (i, op) in ops.iter().enumerate() {
        let touches = reads(op).iter().any(|r| tainted.contains(&r.0));
        let seed = match op {
            Opcode::SetGlobal { global, .. } => is_new_global(global.0) || touches,
            _ => touches && !is_pure(op) && !is_control(op),
        };
        if seed {
            seeds.insert(i);
        }
        let new_value = match op {
            Opcode::Type { ty, .. } => ty.0 >= types_from,
            Opcode::GetGlobal { global, .. } => is_new_global(global.0),
            _ => touches,
        };
        for w in writes(op) {
            if new_value {
                tainted.insert(w.0);
            } else {
                tainted.remove(&w.0);
            }
        }
    }
    if seeds.is_empty() {
        return Ok(());
    }

    // Walk back from those ops, keeping the ones that compute what they read
    // and the ones that change what they read: an array is filled by stores
    // that define no register.
    let mut keep = vec![false; ops.len()];
    let mut need: HashSet<u32> = HashSet::new();
    for i in (0..ops.len()).rev() {
        let op = &ops[i];
        let changes_it =
            !is_pure(op) && !is_control(op) && reads(op).iter().any(|r| need.contains(&r.0));
        if seeds.contains(&i) || changes_it || writes(op).iter().any(|w| need.contains(&w.0)) {
            keep[i] = true;
            for w in writes(op) {
                need.remove(&w.0);
            }
            for r in reads(op) {
                need.insert(r.0);
            }
        }
    }
    // The slice is a straight run of ops: one inside a branch or a loop would
    // run without the test that guards it, and a value some branch picks
    // would be whichever write came last.
    let branched = inside_a_jump(ops);
    let mut setup: Vec<Opcode> = Vec::new();
    for (i, op) in ops.iter().enumerate() {
        if keep[i] {
            if is_control(op) || branched[i] {
                return Err("the setup of the new classes has control flow in it".into());
            }
            setup.push(op.clone());
        }
    }
    let ret = ops
        .iter()
        .rev()
        .find_map(|op| match op {
            Opcode::Ret { ret } => Some(*ret),
            _ => None,
        })
        .ok_or("the entry function never returns")?;
    setup.push(Opcode::Ret { ret });

    let findex = new
        .functions
        .iter()
        .map(|f| f.findex)
        .chain(new.natives.iter().map(|n| n.findex))
        .max()
        .map_or(0, |max| max as usize + 1);
    if findex >= slots {
        return Err(format!(
            "the new program needs {} function slots and the running one has {slots}; restart it",
            findex + 1
        ));
    }
    let mut init = HLFunction::with_body(setup, Vec::new());
    init.findex = findex as i32;
    init.type_ = entry_fn.type_.clone();
    init.regs = entry_fn.regs.clone();
    new.functions.push(init);
    new.reload_init = Some(findex);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hl;
    use crate::opcodes::{RefField, RefFun, RefGlobal, RefInt, RefString, RefType, Reg};
    use crate::types::{HLType, TypeRef};

    fn class(name: &str) -> HLType {
        HLType {
            kind: hl::hl_type_kind_HOBJ,
            obj: Some(crate::types::HLTypeObj {
                name: name.into(),
                ..Default::default()
            }),
            ..Default::default()
        }
    }

    /// `Type.initClass(statics, class, name)` for the class at type `t`.
    fn register(t: usize) -> Vec<Opcode> {
        vec![
            Opcode::Type {
                dst: Reg(1),
                ty: RefType(t),
            },
            Opcode::Type {
                dst: Reg(2),
                ty: RefType(t),
            },
            Opcode::String {
                dst: Reg(3),
                ptr: RefString(0),
            },
            Opcode::Call3 {
                dst: Reg(4),
                fun: RefFun(9),
                arg0: Reg(1),
                arg1: Reg(2),
                arg2: Reg(3),
            },
        ]
    }

    /// `statics_of_global.f = value`.
    fn set_static(global: usize, value: i32) -> Vec<Opcode> {
        vec![
            Opcode::Int {
                dst: Reg(5),
                ptr: RefInt(value as usize),
            },
            Opcode::GetGlobal {
                dst: Reg(6),
                global: RefGlobal(global),
            },
            Opcode::SetField {
                obj: Reg(6),
                field: RefField(0),
                src: Reg(5),
            },
        ]
    }

    /// An entry function that sets up the class at type 1 (running) and the
    /// class at type 2 (new), then returns. Globals 0 and 1 are their statics.
    fn program(entry: Vec<Opcode>) -> DecodedBytecode {
        let mut f = HLFunction::with_body(entry, Vec::new());
        f.regs = vec![TypeRef(0); 8];
        DecodedBytecode {
            types: vec![
                HLType {
                    kind: hl::hl_type_kind_HVOID,
                    ..Default::default()
                },
                class("Old"),
                class("New"),
            ],
            globals: vec![TypeRef(1), TypeRef(2)],
            functions: vec![f],
            entrypoint: 0,
            ..Default::default()
        }
    }

    fn ret() -> Opcode {
        Opcode::Ret { ret: Reg(7) }
    }

    /// Only what touches the new class is set up again: it is registered, and
    /// its statics are given their first values. The running class is left as
    /// it is.
    #[test]
    fn the_setup_of_a_new_class_is_taken_from_the_entry_function() {
        let mut ops = register(1);
        ops.extend(set_static(0, 7));
        ops.extend(register(2));
        ops.extend(set_static(1, 3));
        ops.push(ret());
        let mut bc = program(ops);
        attach_init(&mut bc, 2, 100).unwrap();
        let findex = bc.reload_init.expect("an init function");
        assert_eq!(findex, 1);
        let init = bc.functions.iter().find(|f| f.findex as usize == findex).unwrap();
        let mut expected = register(2);
        expected.extend(set_static(1, 3));
        expected.push(ret());
        assert_eq!(format!("{:?}", init.ops()), format!("{expected:?}"));
    }

    /// A static that is an array has its elements stored by ops that define
    /// no register, which the setup needs as much as the allocation.
    #[test]
    fn the_stores_that_fill_a_static_array_are_part_of_the_setup() {
        use crate::opcodes::RefFun;
        let mut ops = register(2);
        let alloc = Opcode::Call0 {
            dst: Reg(5),
            fun: RefFun(8),
        };
        let store = |index: usize| Opcode::SetArray {
            array: Reg(5),
            index: Reg(index as u32),
            src: Reg(4),
        };
        let publish = [
            Opcode::GetGlobal {
                dst: Reg(6),
                global: RefGlobal(1),
            },
            Opcode::SetField {
                obj: Reg(6),
                field: RefField(0),
                src: Reg(5),
            },
        ];
        ops.push(alloc.clone());
        ops.push(store(1));
        ops.push(store(2));
        ops.extend(publish.clone());
        ops.push(ret());
        let mut bc = program(ops);
        attach_init(&mut bc, 2, 100).unwrap();
        let init = bc.functions.last().unwrap();
        let stores = init
            .ops()
            .iter()
            .filter(|op| matches!(op, Opcode::SetArray { .. }))
            .count();
        assert_eq!(stores, 2);
    }

    #[test]
    fn a_program_with_no_new_type_needs_no_setup() {
        let mut ops = register(1);
        ops.push(ret());
        let mut bc = program(ops);
        bc.types.pop();
        attach_init(&mut bc, 2, 100).unwrap();
        assert_eq!(bc.reload_init, None);
    }

    /// A new type the entry function never touches has nothing to set up.
    #[test]
    fn a_new_type_the_entry_function_never_names_needs_no_setup() {
        let mut ops = register(1);
        ops.push(ret());
        let mut bc = program(ops);
        attach_init(&mut bc, 2, 100).unwrap();
        assert_eq!(bc.reload_init, None);
        assert_eq!(bc.functions.len(), 1);
    }

    /// What the setup computes may not come from a branch.
    #[test]
    fn setup_that_depends_on_control_flow_is_refused() {
        let mut ops = register(2);
        ops.push(Opcode::Trap {
            exc: Reg(5),
            offset: 1,
        });
        ops.push(Opcode::GetGlobal {
            dst: Reg(6),
            global: RefGlobal(1),
        });
        ops.push(Opcode::SetField {
            obj: Reg(6),
            field: RefField(0),
            src: Reg(5),
        });
        ops.push(ret());
        let mut bc = program(ops);
        let why = attach_init(&mut bc, 2, 100).unwrap_err();
        assert!(why.contains("control flow"), "{why}");
    }

    /// The ops that make a bool of a comparison, as the entry function has
    /// them: `r5 = (r1 == null)`.
    fn bool_of_comparison() -> Vec<Opcode> {
        vec![
            Opcode::Null { dst: Reg(0) },
            Opcode::JEq {
                a: Reg(1),
                b: Reg(0),
                offset: 2,
            },
            Opcode::Bool {
                dst: Reg(5),
                value: false,
            },
            Opcode::JAlways { offset: 1 },
            Opcode::Bool {
                dst: Reg(5),
                value: true,
            },
        ]
    }

    fn store_r5_in_static() -> Vec<Opcode> {
        vec![
            Opcode::GetGlobal {
                dst: Reg(6),
                global: RefGlobal(1),
            },
            Opcode::SetField {
                obj: Reg(6),
                field: RefField(0),
                src: Reg(5),
            },
        ]
    }

    /// A static that a comparison decides has two writes behind a jump, and
    /// a straight run of the ops that write it would keep one of them.
    #[test]
    fn a_static_chosen_by_a_branch_is_refused() {
        let mut ops = register(2);
        ops.extend(bool_of_comparison());
        ops.extend(store_r5_in_static());
        ops.push(ret());
        let mut bc = program(ops);
        let why = attach_init(&mut bc, 2, 100).unwrap_err();
        assert!(why.contains("control flow"), "{why}");
    }

    /// A store the entry function makes only when some test holds is not
    /// made without the test.
    #[test]
    fn a_store_under_a_test_is_refused() {
        let mut ops = register(2);
        let store = set_static(1, 3);
        ops.push(Opcode::JTrue {
            cond: Reg(0),
            offset: store.len() as i32,
        });
        ops.extend(store);
        ops.push(ret());
        let mut bc = program(ops);
        let why = attach_init(&mut bc, 2, 100).unwrap_err();
        assert!(why.contains("control flow"), "{why}");
    }

    /// A branch that belongs to something else, between the ops the setup
    /// takes, is left where it is.
    #[test]
    fn a_branch_between_the_ops_of_the_setup_is_left_out() {
        let mut ops = register(2);
        ops.extend(bool_of_comparison());
        ops.extend(set_static(1, 3));
        ops.push(ret());
        let mut bc = program(ops);
        attach_init(&mut bc, 2, 100).unwrap();
        let init = bc.functions.last().unwrap();
        let mut expected = register(2);
        expected.extend(set_static(1, 3));
        expected.push(ret());
        assert_eq!(format!("{:?}", init.ops()), format!("{expected:?}"));
    }

    /// A jump the setup does not depend on is left behind with the rest of
    /// the entry function.
    #[test]
    fn a_jump_the_setup_does_not_depend_on_is_left_out() {
        let mut ops = register(2);
        ops.push(Opcode::JAlways { offset: 0 });
        ops.push(ret());
        let mut bc = program(ops);
        attach_init(&mut bc, 2, 100).unwrap();
        let init = bc.functions.last().unwrap();
        assert!(!init.ops().iter().any(|op| matches!(op, Opcode::JAlways { .. })));
    }

    #[test]
    fn the_setup_needs_a_slot() {
        let mut ops = register(2);
        ops.push(ret());
        let mut bc = program(ops);
        let why = attach_init(&mut bc, 2, 1).unwrap_err();
        assert!(why.contains("function slots"), "{why}");
    }
}
