//! Escape analysis and scalar replacement of aggregates.

use super::{
    compact_values, feeds_handler_phi, handler_blocks, param_values, privatize, replace_all_uses,
    Pass, PassOptions, PassStats, RegClaims,
};
use crate::v2::analysis::CfgInfo;
use crate::v2::ir::*;
use anyhow::Result;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

/// Replaces allocations that never escape with SSA values for their fields.
///
/// An allocation qualifies when its pointer is only ever used to read or write
/// its own fields. Field reads then become uses of the field's current value
/// and field writes become new definitions of it, exactly as `mem2reg` treats
/// a stack slot: each field variable gets phis at the iterated dominance
/// frontier of its definitions and a renaming walk resolves every read. The
/// allocation, its field accesses and its null checks all disappear.
///
/// **Ordering.** In HL `new C(...)` hands the fresh object straight to the
/// constructor, so *every* allocation escapes into a call until that
/// constructor has been inlined. This pass is only worth running after
/// [`inline`](super::inline).
///
/// Guarantees:
/// * **All or nothing.** An allocation is either removed outright or left
///   completely alone. Partial scalarization — promoting some fields while the
///   object still exists — would have to keep memory and the promoted values
///   coherent, so it is refused rather than attempted. Removing an allocation
///   is always safe for the conservative GC: it strictly reduces what has to
///   be traced.
/// * **Escape is anything but a field access.** Being stored into memory,
///   passed to any call, returned, thrown, merged by a phi, written into a
///   cell, address-taken, cast (which boxes or re-types the pointer), branched
///   on (pointer identity is observable) or asked for its runtime type all
///   count as escaping. `Copy` is transparent: the copies of an allocation are
///   tracked as aliases of it and classified the same way, which is what lets
///   the pass see through the copies inlining leaves behind.
/// * **SROA does not fire inside `try`/`catch`.** This is a property of the
///   pass, not a defect: lowering pins every register written inside a trap
///   region to a cell, so an allocation made there is `CellSet` into one —
///   an escape. The pass additionally refuses when the allocation or any of
///   its uses sits in a block with a [`Block::handler`], which is also what
///   keeps the removal of a `NullCheck` from ever deleting a block's
///   exceptional edge.
/// * **`EnumAlloc`/`MakeEnum` payloads scalarize through the same
///   machinery**: `MakeEnum` arguments are the payload's initial definitions,
///   `SetEnumField` defines and `EnumField` reads. `EnumIndex` escapes,
///   because folding a construct tag would need an integer constant-pool index
///   the IR has no way to mint.
/// * **A field read before any write refuses the allocation.** HL zero-fills a
///   fresh object; the IR has no way to name that initial value.
/// * **Register-correct.** A read replaced by the field's current value
///   lengthens that value's live range, so it is privatized first
///   ([`privatize`]); when that is impossible the allocation is left alone.
///
/// An allocation made in a loop body is written to a register the loop header
/// merges, so the header carries a phi over the pointer — an escape — until
/// [`DeadCodeElim`](super::dce::DeadCodeElim) removes that phi as dead. The
/// per-iteration allocation of a loop is therefore scalarized on a later
/// [`PassManager`](super::PassManager) round rather than the first.
pub struct ScalarReplacement;

/// The allocation flavour, which decides what counts as a field access.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Shape {
    /// `New`: fields addressed by `(object type, slot)`, initially undefined.
    Object(TypeRef),
    /// `EnumAlloc`/`MakeEnum`: payload addressed by `(construct, slot)`.
    Enum(usize),
}

impl Pass for ScalarReplacement {
    fn name(&self) -> &'static str {
        "sroa"
    }

    fn run(&self, f: &mut Function, _opts: &PassOptions) -> Result<PassStats> {
        let mut stats = PassStats::default();
        // Each successful plan removes at least the allocation, so the total
        // instruction count bounds the loop.
        let bound: usize = f.blocks.iter().map(|b| b.instrs.len()).sum();
        for _ in 0..bound {
            let Some(plan) = first_candidate(f) else {
                break;
            };
            let fields = plan.fields;
            apply(f, plan)?;
            stats.allocs_removed += 1;
            stats.fields_scalarized += fields;
        }
        Ok(stats)
    }
}

/// What the rewrite has to do, decided before anything is mutated.
struct Plan {
    /// `(block, instruction)` positions to delete.
    remove: Vec<(usize, usize)>,
    /// `(field read, the value it becomes)`, in dominance order.
    rewrites: Vec<(ValueId, ValueId)>,
    /// Phis to create, in the order their value ids were reserved.
    phis: Vec<(BlockId, Phi)>,
    /// Distinct field slots promoted.
    fields: usize,
}

fn first_candidate(f: &Function) -> Option<Plan> {
    for (b, blk) in f.blocks.iter().enumerate() {
        for (k, ins) in blk.instrs.iter().enumerate() {
            let shape = match ins {
                Instr::New { dst } => Shape::Object(f.value_ty(*dst)),
                Instr::EnumAlloc { construct, .. } | Instr::MakeEnum { construct, .. } => {
                    Shape::Enum(*construct)
                }
                _ => continue,
            };
            let dst = ins.dst().expect("an allocation defines a value");
            if let Some(plan) = plan_for(f, BlockId(b as u32), k, dst, shape) {
                return Some(plan);
            }
        }
    }
    None
}

/// The classified uses of an allocation and of every copy of it.
struct Uses {
    /// Accessor and alias-copy instructions, as `(block, index)`.
    touch: HashSet<(usize, usize)>,
    /// Blocks holding a definition of each field.
    def_blocks: BTreeMap<usize, BTreeSet<usize>>,
    /// The type each field slot carries.
    field_ty: BTreeMap<usize, TypeRef>,
}

/// Classify every use of the allocation and of its copies, or `None` when the
/// pointer escapes.
fn classify(f: &Function, alloc: ValueId, shape: Shape) -> Option<Uses> {
    let mut aliases: HashSet<ValueId> = HashSet::from([alloc]);
    loop {
        let before = aliases.len();
        let mut uses = Uses {
            touch: HashSet::new(),
            def_blocks: BTreeMap::new(),
            field_ty: BTreeMap::new(),
        };
        let note = |uses: &mut Uses, field: usize, ty: TypeRef| -> bool {
            *uses.field_ty.entry(field).or_insert(ty) == ty
        };

        for (b, blk) in f.blocks.iter().enumerate() {
            // A phi merge and any terminator operand escape outright.
            for phi in &blk.phis {
                if phi.incoming.iter().any(|&(_, v)| aliases.contains(&v)) {
                    return None;
                }
            }
            if blk.term.uses().iter().any(|u| aliases.contains(u)) {
                return None;
            }
            for (k, ins) in blk.instrs.iter().enumerate() {
                if !ins.uses().iter().any(|u| aliases.contains(u)) {
                    continue;
                }
                if blk.handler.is_some() {
                    return None;
                }
                match (ins, shape) {
                    (Instr::Copy { dst, src }, _) if aliases.contains(src) => {
                        aliases.insert(*dst);
                    }
                    (Instr::NullCheck { .. }, _) => {}
                    (
                        Instr::FieldGet {
                            dst,
                            obj,
                            obj_ty,
                            field,
                        },
                        Shape::Object(ty),
                    ) if aliases.contains(obj) && *obj_ty == ty => {
                        if !note(&mut uses, *field, f.value_ty(*dst)) {
                            return None;
                        }
                    }
                    (
                        Instr::FieldSet {
                            obj,
                            obj_ty,
                            field,
                            src,
                        },
                        Shape::Object(ty),
                    ) if aliases.contains(obj) && *obj_ty == ty => {
                        if aliases.contains(src) || !note(&mut uses, *field, f.value_ty(*src)) {
                            return None;
                        }
                        uses.def_blocks.entry(*field).or_default().insert(b);
                    }
                    (
                        Instr::EnumField {
                            dst,
                            value,
                            construct,
                            field,
                        },
                        Shape::Enum(c),
                    ) if aliases.contains(value) && *construct == c => {
                        if !note(&mut uses, *field, f.value_ty(*dst)) {
                            return None;
                        }
                    }
                    (
                        Instr::SetEnumField {
                            value,
                            construct,
                            field,
                            src,
                        },
                        Shape::Enum(c),
                    ) if aliases.contains(value) && *construct == c => {
                        if aliases.contains(src) || !note(&mut uses, *field, f.value_ty(*src)) {
                            return None;
                        }
                        uses.def_blocks.entry(*field).or_default().insert(b);
                    }
                    _ => return None,
                }
                uses.touch.insert((b, k));
            }
        }
        // A copy discovered late may sit in an earlier block than its uses, so
        // the scan repeats until the alias set is stable.
        if aliases.len() == before {
            return Some(uses);
        }
    }
}

fn plan_for(
    f: &Function,
    ablock: BlockId,
    aidx: usize,
    alloc: ValueId,
    shape: Shape,
) -> Option<Plan> {
    if f.blocks[ablock.idx()].handler.is_some() {
        return None;
    }
    let mut uses = classify(f, alloc, shape)?;

    // `MakeEnum` initializes its payload at the allocation itself.
    let init: Vec<ValueId> = match &f.blocks[ablock.idx()].instrs[aidx] {
        Instr::MakeEnum { args, .. } => args.clone(),
        _ => Vec::new(),
    };
    for (j, &a) in init.iter().enumerate() {
        if *uses.field_ty.entry(j).or_insert(f.value_ty(a)) != f.value_ty(a) {
            return None;
        }
        uses.def_blocks.entry(j).or_default().insert(ablock.idx());
    }
    if uses.field_ty.is_empty() {
        return None; // nothing touches it; DCE reclaims a dead allocation
    }

    let cfg = CfgInfo::build(f);
    let handlers = handler_blocks(f);
    let nb = f.blocks.len();
    // The object exists exactly where its allocation dominates.
    let in_scope: Vec<bool> = (0..nb)
        .map(|b| cfg.dominates(ablock, BlockId(b as u32)))
        .collect();

    // ---- phi placement per field ------------------------------------------
    let fields: Vec<usize> = uses.field_ty.keys().copied().collect();
    let mut phi_fields: BTreeMap<usize, Vec<usize>> = BTreeMap::new(); // block -> fields
    for &fd in &fields {
        let defs = uses.def_blocks.get(&fd).cloned().unwrap_or_default();
        let mut work: Vec<usize> = defs.iter().copied().collect();
        let mut placed: HashSet<usize> = HashSet::new();
        let mut ever: HashSet<usize> = defs.iter().copied().collect();
        while let Some(x) = work.pop() {
            for &y in &cfg.dom.dom_frontier[x] {
                if !in_scope[y] {
                    continue; // the object does not exist there
                }
                if handlers[y] {
                    return None; // a handler block cannot carry a real phi
                }
                if placed.insert(y) {
                    phi_fields.entry(y).or_default().push(fd);
                    if ever.insert(y) {
                        work.push(y);
                    }
                }
            }
        }
    }
    // Reserve dense value ids for the phis, in the order `apply` mints them.
    let mut slots: Vec<(usize, usize)> = Vec::new(); // (block, field)
    for (&b, flds) in phi_fields.iter() {
        for &fd in flds {
            slots.push((b, fd));
        }
    }
    slots.sort_unstable();
    let base = f.values.len() as u32;
    let phi_dst: HashMap<(usize, usize), ValueId> = slots
        .iter()
        .enumerate()
        .map(|(i, &s)| (s, ValueId(base + i as u32)))
        .collect();

    // ---- renaming walk over the dominator tree ----------------------------
    let mut plan = Plan {
        remove: uses.touch.iter().copied().collect(),
        rewrites: Vec::new(),
        phis: Vec::new(),
        fields: fields.len(),
    };
    plan.remove.push((ablock.idx(), aidx));
    plan.remove.sort_unstable();
    plan.remove.dedup();

    let mut stacks: BTreeMap<usize, Vec<ValueId>> =
        fields.iter().map(|&fd| (fd, Vec::new())).collect();
    let mut incoming: HashMap<(usize, usize), Vec<(BlockId, ValueId)>> = HashMap::new();

    enum Item {
        Visit(usize),
        Restore(Vec<usize>),
    }
    let mut walk = vec![Item::Visit(ablock.idx())];
    while let Some(item) = walk.pop() {
        let b = match item {
            Item::Restore(saved) => {
                for (i, &fd) in fields.iter().enumerate() {
                    stacks.get_mut(&fd).expect("known field").truncate(saved[i]);
                }
                continue;
            }
            Item::Visit(b) => b,
        };
        let saved: Vec<usize> = fields.iter().map(|fd| stacks[fd].len()).collect();

        if let Some(flds) = phi_fields.get(&b) {
            for &fd in flds {
                let v = phi_dst[&(b, fd)];
                stacks.get_mut(&fd).expect("known field").push(v);
            }
        }
        for (k, ins) in f.blocks[b].instrs.iter().enumerate() {
            if b == ablock.idx() && k == aidx {
                for (j, &a) in init.iter().enumerate() {
                    stacks.get_mut(&j).expect("known field").push(a);
                }
                continue;
            }
            if !uses.touch.contains(&(b, k)) {
                continue;
            }
            match ins {
                Instr::FieldSet { field, src, .. } | Instr::SetEnumField { field, src, .. } => {
                    stacks.get_mut(field).expect("known field").push(*src);
                }
                Instr::FieldGet { dst, field, .. } | Instr::EnumField { dst, field, .. } => {
                    // A read with nothing on the stack is a read of the
                    // object's initial state, which cannot be named.
                    plan.rewrites.push((*dst, *stacks[field].last()?));
                }
                _ => {}
            }
        }
        for &s in &cfg.succs[b] {
            let Some(flds) = phi_fields.get(&s.idx()) else {
                continue;
            };
            for &fd in flds {
                let cur = *stacks[&fd].last()?;
                incoming
                    .entry((s.idx(), fd))
                    .or_default()
                    .push((BlockId(b as u32), cur));
            }
        }

        walk.push(Item::Restore(saved));
        for &child in cfg.dom.dom_children[b].iter().rev() {
            if in_scope[child] {
                walk.push(Item::Visit(child));
            }
        }
    }

    for &(b, fd) in &slots {
        let inc = incoming.remove(&(b, fd))?;
        if inc.len() != cfg.preds[b].len() {
            return None;
        }
        plan.phis.push((
            BlockId(b as u32),
            Phi {
                dst: phi_dst[&(b, fd)],
                incoming: inc,
            },
        ));
    }

    // Every replacement lengthens the live range of the value it forwards.
    let is_param = param_values(f);
    let claims = RegClaims::build(f);
    for &(_, src) in &plan.rewrites {
        // A value the plan mints is a phi with a register of its own.
        if src.idx() >= f.values.len() {
            continue;
        }
        if feeds_handler_phi(f, &handlers, src) {
            return None;
        }
        if !claims.is_exclusive(f, src) && is_param[src.idx()] {
            return None;
        }
    }
    Some(plan)
}

fn apply(f: &mut Function, plan: Plan) -> Result<()> {
    // Mint the phi values in the order their ids were reserved.
    for (b, phi) in &plan.phis {
        let ty = f.value_ty(phi.incoming[0].1);
        let reg = f.new_reg(ty);
        let v = f.new_value(ty, reg);
        if v != phi.dst {
            anyhow::bail!("sroa: phi value id {} was not reserved densely", phi.dst.0);
        }
        f.blocks[b.idx()].phis.push(phi.clone());
    }

    // `obj.a = obj.b` makes one read feed another field, so a replacement's
    // target may itself have been replaced.
    let mut subst: HashMap<ValueId, ValueId> = HashMap::new();
    let is_param = param_values(f);
    let mut claims = RegClaims::build(f);
    for &(from, to) in &plan.rewrites {
        let mut to = to;
        for _ in 0..plan.rewrites.len() + 1 {
            match subst.get(&to) {
                Some(&n) if n != to => to = n,
                _ => break,
            }
        }
        if !privatize(f, to, &mut claims, is_param[to.idx()]) {
            anyhow::bail!("sroa: v{} could not be given a private register", to.0);
        }
        subst.insert(from, to);
        replace_all_uses(f, from, to);
    }

    for &(b, k) in plan.remove.iter().rev() {
        f.blocks[b].instrs.remove(k);
    }
    compact_values(f)?;
    Ok(())
}
