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
/// **Phi webs.** A pointer phi does not escape by itself: when *every* value a
/// phi can carry is itself a qualifying allocation of the same shape, the whole
/// web — allocations, copies and phis — scalarizes together. This is the loop
/// shape `z = phi(initial, per-iteration)` that an escape-style benchmark lives
/// in. Each *root* (an allocation or an object phi) keeps private per-field
/// variables, so two objects live at once with interleaved writes stay
/// distinct; the object phi itself becomes one *stitch* phi per demanded field,
/// whose incoming on each edge is the field value of whichever root flowed in
/// on that edge. A pre-merge alias touched again after the merge would read a
/// stale variable, so it refuses the web (`alias touched after its merge`)
/// unless the alias's own definition sits strictly below the merge — the
/// fresh-per-iteration case, where the stitch has already re-read it.
///
/// **Ordering.** In HL `new C(...)` hands the fresh object straight to the
/// constructor, so *every* allocation escapes into a call until that
/// constructor has been inlined. This pass is only worth running after
/// [`inline`](super::inline).
///
/// Guarantees:
/// * **All or nothing.** A web is either removed outright or left completely
///   alone. Partial scalarization — promoting some fields while the object
///   still exists — would have to keep memory and the promoted values
///   coherent, so it is refused rather than attempted. Removing an allocation
///   is always safe for the conservative GC: it strictly reduces what has to
///   be traced.
/// * **Escape is anything but a field access.** Being stored into memory,
///   passed to any call, returned, thrown, written into a cell, address-taken,
///   cast (which boxes or re-types the pointer), branched on (pointer identity
///   is observable) or asked for its runtime type all count as escaping. A phi
///   merging the web with any value that is *not* one of its allocations — a
///   parameter, a call result, `Null` (a field read through null must trap) —
///   escapes too. `Copy` is transparent: the copies of an allocation are
///   tracked as aliases of it and classified the same way, which is what lets
///   the pass see through the copies inlining leaves behind.
/// * **SROA does not fire inside `try`/`catch`.** This is a property of the
///   pass, not a defect: lowering pins every register written inside a trap
///   region to a cell, so an allocation made there is `CellSet` into one —
///   an escape. The pass additionally refuses when any allocation, object phi
///   or use sits in a block with a [`Block::handler`], which is also what
///   keeps the removal of a `NullCheck` from ever deleting a block's
///   exceptional edge.
/// * **`EnumAlloc`/`MakeEnum` payloads scalarize through the same
///   machinery**: `MakeEnum` arguments are the payload's initial definitions,
///   `SetEnumField` defines and `EnumField` reads. `EnumIndex` escapes,
///   because folding a construct tag would need an integer constant-pool index
///   the IR has no way to mint.
/// * **A field read before any write refuses the web.** HL zero-fills a fresh
///   object; the IR has no way to name that initial value.
/// * **Register-correct.** A read replaced by the field's current value
///   lengthens that value's live range, so it is privatized first
///   ([`privatize`]); when that is impossible the web is left alone.
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
        // Each successful plan removes at least one allocation, so the total
        // instruction count bounds the loop.
        let bound: usize = f.blocks.iter().map(|b| b.instrs.len()).sum();
        for _ in 0..bound {
            let Some(plan) = first_candidate(f) else {
                break;
            };
            let fields = plan.fields;
            let allocs = plan.alloc_count;
            apply(f, plan)?;
            stats.allocs_removed += allocs;
            stats.fields_scalarized += fields;
        }
        Ok(stats)
    }
}

/// What the rewrite has to do, decided before anything is mutated.
struct Plan {
    /// `(block, instruction)` positions to delete: every use *and* every
    /// allocation of the web.
    remove: Vec<(usize, usize)>,
    /// Object-carrying phis to delete, as `(block, phi dst)`.
    remove_phis: Vec<(usize, ValueId)>,
    /// `(field read, the value it becomes)`, in dominance order.
    rewrites: Vec<(ValueId, ValueId)>,
    /// Phis to create, in the order their value ids were reserved, each with
    /// the type of the field it carries.
    ///
    /// The type is recorded here rather than read back from the phi's first
    /// incoming value: a field phi's incoming can name *another* field phi, and
    /// in a nested loop that one may not have been minted yet, so looking it up
    /// during `apply` indexes past the end of the value table.
    phis: Vec<(BlockId, Phi, TypeRef)>,
    /// Distinct field slots promoted.
    fields: usize,
    /// Allocation instructions removed.
    alloc_count: usize,
}

fn first_candidate(f: &Function) -> Option<Plan> {
    let mut visited: HashSet<ValueId> = HashSet::new();
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
            // A refused web refuses identically from each of its members.
            if !visited.insert(dst) {
                continue;
            }
            if let Some(plan) = plan_for(f, BlockId(b as u32), k, dst, shape, &mut visited) {
                return Some(plan);
            }
        }
    }
    None
}

/// `ASH_SROA_WHY=1` reports why each allocation was refused. A pass that fires
/// zero times is indistinguishable from one that is not running, and the
/// difference matters.
fn why(alloc: ValueId, reason: &str) {
    if std::env::var("ASH_SROA_WHY").is_ok() {
        eprintln!("[sroa] refused v{}: {}", alloc.idx(), reason);
    }
}

/// Where a value is defined.
#[derive(Clone, Copy)]
enum DefSite {
    Ins(usize, usize),
    Phi(usize, usize),
}

/// The web grown from one allocation: its roots, aliases and uses.
struct Web {
    /// Allocation sites as `(block, index, root, MakeEnum initial args)`.
    sites: Vec<(usize, usize, usize, Vec<ValueId>)>,
    /// Object phis as `(block, phi index, root)`.
    obj_phis: Vec<(usize, usize, usize)>,
    /// Root index for every alias.
    root_of: HashMap<ValueId, usize>,
    /// Defining block of each root.
    root_block: Vec<usize>,
    /// Accessor and alias-copy instructions, as `(block, index)`.
    touch: HashSet<(usize, usize)>,
    /// `(root, block)` of every touch, for the merge-shadow check.
    touch_at: Vec<(usize, usize)>,
    /// Blocks holding a definition of each `(root, field)` variable.
    def_blocks: BTreeMap<(usize, usize), BTreeSet<usize>>,
    /// The type each field slot carries.
    field_ty: BTreeMap<usize, TypeRef>,
}

/// Grow the alias web from `alloc`, classify every use, or `None` when any
/// part of it escapes.
fn classify(f: &Function, alloc: ValueId, shape: Shape) -> Option<Web> {
    // ---- definitions -------------------------------------------------------
    let mut defs: HashMap<ValueId, DefSite> = HashMap::new();
    for (b, blk) in f.blocks.iter().enumerate() {
        for (pi, phi) in blk.phis.iter().enumerate() {
            defs.insert(phi.dst, DefSite::Phi(b, pi));
        }
        for (k, ins) in blk.instrs.iter().enumerate() {
            if let Some(d) = ins.dst() {
                defs.insert(d, DefSite::Ins(b, k));
            }
        }
    }

    // ---- growth to a fixed point ------------------------------------------
    // Copies connect in both directions; a phi touching the web absorbs its
    // destination and every incoming.
    let mut aliases: BTreeSet<ValueId> = BTreeSet::from([alloc]);
    loop {
        let before = aliases.len();
        for blk in &f.blocks {
            for phi in &blk.phis {
                if aliases.contains(&phi.dst)
                    || phi.incoming.iter().any(|&(_, v)| aliases.contains(&v))
                {
                    aliases.insert(phi.dst);
                    for &(_, v) in &phi.incoming {
                        aliases.insert(v);
                    }
                }
            }
            for ins in &blk.instrs {
                if let Instr::Copy { dst, src } = ins {
                    if aliases.contains(dst) || aliases.contains(src) {
                        aliases.insert(*dst);
                        aliases.insert(*src);
                    }
                }
            }
        }
        if aliases.len() == before {
            break;
        }
    }

    // ---- validate definitions, collect roots ------------------------------
    let mut web = Web {
        sites: Vec::new(),
        obj_phis: Vec::new(),
        root_of: HashMap::new(),
        root_block: Vec::new(),
        touch: HashSet::new(),
        touch_at: Vec::new(),
        def_blocks: BTreeMap::new(),
        field_ty: BTreeMap::new(),
    };
    let mut root_ids: Vec<ValueId> = Vec::new();
    for &v in &aliases {
        match defs.get(&v) {
            Some(&DefSite::Ins(b, k)) => match &f.blocks[b].instrs[k] {
                Instr::New { dst } if shape == Shape::Object(f.value_ty(*dst)) => {
                    root_ids.push(v);
                    web.root_block.push(b);
                    web.sites.push((b, k, root_ids.len() - 1, Vec::new()));
                }
                Instr::EnumAlloc { construct, .. } if shape == Shape::Enum(*construct) => {
                    root_ids.push(v);
                    web.root_block.push(b);
                    web.sites.push((b, k, root_ids.len() - 1, Vec::new()));
                }
                Instr::MakeEnum {
                    construct, args, ..
                } if shape == Shape::Enum(*construct) => {
                    root_ids.push(v);
                    web.root_block.push(b);
                    web.sites.push((b, k, root_ids.len() - 1, args.clone()));
                }
                Instr::Copy { .. } => {} // resolved to a root below
                other => {
                    let mut d = format!("{:?}", other);
                    d.truncate(100);
                    why(alloc, &format!("phi merges a non-allocation value: {d}"));
                    return None;
                }
            },
            Some(&DefSite::Phi(b, pi)) => {
                root_ids.push(v);
                web.root_block.push(b);
                web.obj_phis.push((b, pi, root_ids.len() - 1));
            }
            None => {
                why(alloc, "phi merges a value with no definition (a parameter)");
                return None;
            }
        }
    }
    let root_idx: HashMap<ValueId, usize> =
        root_ids.iter().enumerate().map(|(i, &v)| (v, i)).collect();
    for &v in &aliases {
        // Chase copy chains to a root; SSA has no copy cycles that avoid phis.
        let mut cur = v;
        for _ in 0..aliases.len() + 1 {
            if let Some(&r) = root_idx.get(&cur) {
                web.root_of.insert(v, r);
                break;
            }
            match defs.get(&cur) {
                Some(&DefSite::Ins(b, k)) => match &f.blocks[b].instrs[k] {
                    Instr::Copy { src, .. } => cur = *src,
                    _ => unreachable!("validated above"),
                },
                _ => unreachable!("validated above"),
            }
        }
        if !web.root_of.contains_key(&v) {
            why(alloc, "copy chain does not reach a root");
            return None;
        }
    }

    // ---- classify every use -----------------------------------------------
    let note = |web: &mut Web, field: usize, ty: TypeRef| -> bool {
        *web.field_ty.entry(field).or_insert(ty) == ty
    };
    for (b, blk) in f.blocks.iter().enumerate() {
        for phi in &blk.phis {
            // Growth absorbed every phi that names an alias; one that carries
            // an alias but is not itself in the web cannot exist any more.
            if !aliases.contains(&phi.dst)
                && phi.incoming.iter().any(|&(_, v)| aliases.contains(&v))
            {
                why(alloc, "phi merge outside the web");
                return None;
            }
            if aliases.contains(&phi.dst) && blk.handler.is_some() {
                why(alloc, "inside a trap region");
                return None;
            }
        }
        if blk.term.uses().iter().any(|u| aliases.contains(u)) {
            why(alloc, "terminator operand");
            return None;
        }
        for (k, ins) in blk.instrs.iter().enumerate() {
            if !ins.uses().iter().any(|u| aliases.contains(u)) {
                continue;
            }
            if blk.handler.is_some() {
                why(alloc, "inside a trap region");
                return None;
            }
            let root = |web: &Web, v: &ValueId| web.root_of[v];
            match (ins, shape) {
                (Instr::Copy { src, .. }, _) if aliases.contains(src) => {
                    web.touch_at.push((root(&web, src), b));
                }
                (Instr::NullCheck { value, .. }, _) if aliases.contains(value) => {
                    web.touch_at.push((root(&web, value), b));
                }
                (
                    Instr::FieldGet {
                        dst,
                        obj,
                        obj_ty,
                        field,
                    },
                    Shape::Object(ty),
                ) if aliases.contains(obj) && *obj_ty == ty => {
                    if !note(&mut web, *field, f.value_ty(*dst)) {
                        return None;
                    }
                    web.touch_at.push((root(&web, obj), b));
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
                    if aliases.contains(src) || !note(&mut web, *field, f.value_ty(*src)) {
                        why(alloc, "stored into a field");
                        return None;
                    }
                    let r = root(&web, obj);
                    web.def_blocks.entry((r, *field)).or_default().insert(b);
                    web.touch_at.push((r, b));
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
                    if !note(&mut web, *field, f.value_ty(*dst)) {
                        return None;
                    }
                    web.touch_at.push((root(&web, value), b));
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
                    if aliases.contains(src) || !note(&mut web, *field, f.value_ty(*src)) {
                        why(alloc, "stored into a field");
                        return None;
                    }
                    let r = root(&web, value);
                    web.def_blocks.entry((r, *field)).or_default().insert(b);
                    web.touch_at.push((r, b));
                }
                _ => {
                    let mut d = format!("{:?}", ins);
                    d.truncate(100);
                    why(alloc, &format!("escapes into an instruction: {d}"));
                    return None;
                }
            }
            web.touch.insert((b, k));
        }
    }
    Some(web)
}

fn plan_for(
    f: &Function,
    ablock: BlockId,
    _aidx: usize,
    alloc: ValueId,
    shape: Shape,
    visited: &mut HashSet<ValueId>,
) -> Option<Plan> {
    if f.blocks[ablock.idx()].handler.is_some() {
        return None;
    }
    let mut web = classify(f, alloc, shape)?;
    // The web refuses or fires as a unit; never re-plan it from another member.
    for (b, k, _, _) in &web.sites {
        if let Some(d) = f.blocks[*b].instrs[*k].dst() {
            visited.insert(d);
        }
    }
    for (b, _, _) in &web.obj_phis {
        if f.blocks[*b].handler.is_some() {
            why(alloc, "inside a trap region");
            return None;
        }
    }

    // `MakeEnum` initializes its payload at the allocation itself.
    for (b, _, r, init) in &web.sites {
        for (j, &a) in init.iter().enumerate() {
            if *web.field_ty.entry(j).or_insert(f.value_ty(a)) != f.value_ty(a) {
                return None;
            }
            web.def_blocks.entry((*r, j)).or_default().insert(*b);
        }
    }
    if web.field_ty.is_empty() {
        return None; // nothing touches it; DCE reclaims a dead web
    }

    let cfg = CfgInfo::build(f);
    let handlers = handler_blocks(f);
    let nb = f.blocks.len();

    // ---- merge-shadow check -----------------------------------------------
    // After an object phi, its incomings are other names for the object the
    // phi now carries; a write through one name is invisible to the other's
    // variables. An incoming defined strictly *below* the merge is fresh on
    // every arrival — the stitch re-reads it on each edge — so only touches of
    // a pre-merge name inside the merge's dominance are unsound.
    for &(pb, pi, pr) in &web.obj_phis {
        for &(_, v) in &f.blocks[pb].phis[pi].incoming {
            let r = web.root_of[&v];
            if r == pr {
                continue;
            }
            let rb = web.root_block[r];
            if pb != rb && cfg.dominates(BlockId(pb as u32), BlockId(rb as u32)) {
                continue;
            }
            for &(tr, tb) in &web.touch_at {
                if tr == r && cfg.dominates(BlockId(pb as u32), BlockId(tb as u32)) {
                    why(alloc, "alias touched after its merge");
                    return None;
                }
            }
        }
    }

    // ---- which (phi root, field) variables the stitch must carry ----------
    // A field is demanded through a phi when something reads it via the phi's
    // own root, or a demanded phi downstream merges this one.
    let fields: Vec<usize> = web.field_ty.keys().copied().collect();
    let mut demanded: HashSet<(usize, usize)> = HashSet::new();
    for (b, blk) in f.blocks.iter().enumerate() {
        for (k, ins) in blk.instrs.iter().enumerate() {
            if !web.touch.contains(&(b, k)) {
                continue;
            }
            match ins {
                Instr::FieldGet { obj, field, .. } => {
                    demanded.insert((web.root_of[obj], *field));
                }
                Instr::EnumField { value, field, .. } => {
                    demanded.insert((web.root_of[value], *field));
                }
                _ => {}
            }
        }
    }
    loop {
        let before = demanded.len();
        for &(pb, pi, pr) in &web.obj_phis {
            for &fd in &fields {
                if demanded.contains(&(pr, fd)) {
                    for &(_, v) in &f.blocks[pb].phis[pi].incoming {
                        demanded.insert((web.root_of[&v], fd));
                    }
                }
            }
        }
        if demanded.len() == before {
            break;
        }
    }
    for &(pb, _, pr) in &web.obj_phis {
        for &fd in &fields {
            if demanded.contains(&(pr, fd)) {
                web.def_blocks.entry((pr, fd)).or_default().insert(pb);
            }
        }
    }

    // ---- phi placement per (root, field) variable -------------------------
    // A variable exists exactly where its root's definition dominates.
    let in_scope: Vec<Vec<bool>> = web
        .root_block
        .iter()
        .map(|&rb| {
            (0..nb)
                .map(|b| cfg.dominates(BlockId(rb as u32), BlockId(b as u32)))
                .collect()
        })
        .collect();

    /// A minted field phi: the object phi's per-edge stitch, or an ordinary
    /// `mem2reg` phi at the iterated dominance frontier of the writes.
    #[derive(Clone, Copy)]
    enum Var {
        Stitch(usize), // index into web.obj_phis
        Idf,
    }
    let mut phi_fields: BTreeMap<usize, Vec<(usize, usize, Var)>> = BTreeMap::new();
    let stitch_at: HashMap<(usize, usize, usize), usize> = web
        .obj_phis
        .iter()
        .enumerate()
        .flat_map(|(i, &(pb, _, pr))| fields.iter().map(move |&fd| ((pb, pr, fd), i)))
        .collect();
    for (&(r, fd), defs) in &web.def_blocks {
        let mut work: Vec<usize> = defs.iter().copied().collect();
        let mut placed: HashSet<usize> = HashSet::new();
        let mut ever: HashSet<usize> = defs.iter().copied().collect();
        // The stitch is a definition *at* the object phi's own block.
        if let Some(&i) = stitch_at.get(&(web.root_block[r], r, fd)) {
            if demanded.contains(&(r, fd)) {
                placed.insert(web.root_block[r]);
                phi_fields
                    .entry(web.root_block[r])
                    .or_default()
                    .push((r, fd, Var::Stitch(i)));
            }
        }
        while let Some(x) = work.pop() {
            for &y in &cfg.dom.dom_frontier[x] {
                if !in_scope[r][y] {
                    continue; // the variable does not exist there
                }
                if handlers[y] {
                    return None; // a handler block cannot carry a real phi
                }
                if placed.insert(y) {
                    phi_fields.entry(y).or_default().push((r, fd, Var::Idf));
                    if ever.insert(y) {
                        work.push(y);
                    }
                }
            }
        }
    }
    // Reserve dense value ids for the phis, in the order `apply` mints them.
    let mut slots: Vec<(usize, usize, usize, Var)> = Vec::new(); // (block, root, field)
    for (&b, vars) in phi_fields.iter() {
        for &(r, fd, kind) in vars {
            slots.push((b, r, fd, kind));
        }
    }
    slots.sort_unstable_by_key(|&(b, r, fd, _)| (b, r, fd));
    let base = f.values.len() as u32;
    let phi_dst: HashMap<(usize, usize, usize), ValueId> = slots
        .iter()
        .enumerate()
        .map(|(i, &(b, r, fd, _))| ((b, r, fd), ValueId(base + i as u32)))
        .collect();

    // ---- renaming walk over the dominator tree ----------------------------
    let mut plan = Plan {
        remove: web.touch.iter().copied().collect(),
        remove_phis: web
            .obj_phis
            .iter()
            .map(|&(b, pi, _)| (b, f.blocks[b].phis[pi].dst))
            .collect(),
        rewrites: Vec::new(),
        phis: Vec::new(),
        fields: fields.len(),
        alloc_count: web.sites.len(),
    };
    for (b, k, _, _) in &web.sites {
        plan.remove.push((*b, *k));
    }
    plan.remove.sort_unstable();
    plan.remove.dedup();

    let site_at: HashMap<(usize, usize), (usize, Vec<ValueId>)> = web
        .sites
        .iter()
        .map(|(b, k, r, init)| ((*b, *k), (*r, init.clone())))
        .collect();
    let vars: Vec<(usize, usize)> = {
        let mut v: BTreeSet<(usize, usize)> = web.def_blocks.keys().copied().collect();
        for &(_, r, fd, _) in &slots {
            v.insert((r, fd));
        }
        for r in 0..web.root_block.len() {
            for &fd in &fields {
                v.insert((r, fd));
            }
        }
        v.into_iter().collect()
    };
    let mut stacks: BTreeMap<(usize, usize), Vec<ValueId>> =
        vars.iter().map(|&k| (k, Vec::new())).collect();
    let mut incoming: HashMap<(usize, usize, usize), Vec<(BlockId, ValueId)>> = HashMap::new();

    enum Item {
        Visit(usize),
        Restore(Vec<usize>),
    }
    // The walk covers the whole function: different roots live in different
    // regions, and a variable is simply undefined outside its own.
    let mut walk = vec![Item::Visit(0)];
    while let Some(item) = walk.pop() {
        let b = match item {
            Item::Restore(saved) => {
                for (i, key) in vars.iter().enumerate() {
                    stacks.get_mut(key).expect("known var").truncate(saved[i]);
                }
                continue;
            }
            Item::Visit(b) => b,
        };
        let saved: Vec<usize> = vars.iter().map(|key| stacks[key].len()).collect();

        if let Some(flds) = phi_fields.get(&b) {
            for &(r, fd, _) in flds {
                let v = phi_dst[&(b, r, fd)];
                stacks.get_mut(&(r, fd)).expect("known var").push(v);
            }
        }
        for (k, ins) in f.blocks[b].instrs.iter().enumerate() {
            if let Some((r, init)) = site_at.get(&(b, k)) {
                for (j, &a) in init.iter().enumerate() {
                    stacks.get_mut(&(*r, j)).expect("known var").push(a);
                }
                continue;
            }
            if !web.touch.contains(&(b, k)) {
                continue;
            }
            match ins {
                Instr::FieldSet {
                    obj, field, src, ..
                } => {
                    let r = web.root_of[obj];
                    stacks.get_mut(&(r, *field)).expect("known var").push(*src);
                }
                Instr::SetEnumField {
                    value, field, src, ..
                } => {
                    let r = web.root_of[value];
                    stacks.get_mut(&(r, *field)).expect("known var").push(*src);
                }
                Instr::FieldGet {
                    dst, obj, field, ..
                } => {
                    // A read with nothing on the stack is a read of the
                    // object's initial state, which cannot be named.
                    let r = web.root_of[obj];
                    plan.rewrites.push((*dst, *stacks[&(r, *field)].last()?));
                }
                Instr::EnumField {
                    dst, value, field, ..
                } => {
                    let r = web.root_of[value];
                    plan.rewrites.push((*dst, *stacks[&(r, *field)].last()?));
                }
                _ => {}
            }
        }
        for &s in &cfg.succs[b] {
            let Some(flds) = phi_fields.get(&s.idx()) else {
                continue;
            };
            for &(r, fd, kind) in flds {
                let cur = match kind {
                    Var::Idf => *stacks[&(r, fd)].last()?,
                    Var::Stitch(i) => {
                        // The stitch reads whichever root flowed in on this
                        // edge, not the phi's own variable.
                        let (pb, pi, _) = web.obj_phis[i];
                        let inc = &f.blocks[pb].phis[pi].incoming;
                        let &(_, v) = inc.iter().find(|&&(p, _)| p.idx() == b)?;
                        *stacks[&(web.root_of[&v], fd)].last()?
                    }
                };
                incoming
                    .entry((s.idx(), r, fd))
                    .or_default()
                    .push((BlockId(b as u32), cur));
            }
        }

        walk.push(Item::Restore(saved));
        for &child in cfg.dom.dom_children[b].iter().rev() {
            walk.push(Item::Visit(child));
        }
    }

    for &(b, r, fd, _) in &slots {
        let inc = incoming.remove(&(b, r, fd))?;
        if inc.len() != cfg.preds[b].len() {
            return None;
        }
        plan.phis.push((
            BlockId(b as u32),
            Phi {
                dst: phi_dst[&(b, r, fd)],
                incoming: inc,
            },
            *web.field_ty.get(&fd)?,
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
    for (b, phi, ty) in &plan.phis {
        let ty = *ty;
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
    for &(b, dst) in &plan.remove_phis {
        f.blocks[b].phis.retain(|p| p.dst != dst);
    }
    compact_values(f)?;
    Ok(())
}
