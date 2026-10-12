//! Bounded analysis and opt-in construction for recursive producer/consumer fusion.
//!
//! The analysis is not a general purity analysis. It symbolically
//! checks both cases of an i32 depth guard, expanding straight-line constructors
//! and treating self-recursive calls inductively. Only `depth > 0` followed by
//! `depth - 1` is admitted; mutual recursion and CFG loops are refused. A fold
//! must visit every uniquely owned child exactly once on each producer shape.
//!
//! The result is conditional on an embedder's allocation-elision and code
//! lifetime contracts. Unknown contracts remain explicit blockers. In particular
//! bytecode alone cannot rule out host tails/drop hooks, replacement allocators,
//! or a later hot reload. The read-only survey grants no rewrite permission.
//! `build_helper` and `rewrite_caller` require both contracts and recheck the
//! structural proof. The embedder owns registration, rooting and safepoints.

use super::ir::*;
use super::module::ModuleInfo;
use std::collections::{BTreeMap, BTreeSet};

mod fuse;
pub use fuse::{build_helper, rewrite_caller};

/// Facts not encoded in a function's AIR. Default knowledge is conservative.
pub trait TreeModule: ModuleInfo {
    fn signature(&self, function: usize) -> Option<(Vec<TypeRef>, TypeRef)>;
    fn is_i32(&self, ty: TypeRef) -> bool;
    fn is_void(&self, ty: TypeRef) -> bool;
    /// Complete field layout, including inherited fields, or None. Bindings,
    /// packed fields, and other implicit initialization need separate modeling.
    fn plain_fields(&self, ty: TypeRef) -> Option<Vec<TypeRef>>;
    /// Vouches that removing these allocations cannot expose host allocation/
    /// drop hooks, finalizers, weak refs, identity, or recoverable OOM, and that
    /// GC scheduling/allocation statistics are not semantic barriers to elision.
    /// This is stronger than "currently no registered callback".
    fn allocation_elision(&self, _ty: TypeRef) -> AllocationElision {
        AllocationElision::Unknown
    }
    /// Assumptions remain valid through execution (or guarded invalidation).
    fn stable_code_and_layout(&self) -> bool {
        false
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AllocationElision {
    Unknown,
    Observable,
    Permitted,
}

#[derive(Clone, Copy, Debug)]
pub struct Limits {
    pub body_ops: usize,
    pub steps: usize,
    pub sites: usize,
}

impl Default for Limits {
    fn default() -> Self {
        Self {
            body_ops: 256,
            steps: 16_384,
            sites: 64,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Shape {
    pub tree_type: TypeRef,
    pub depth_arg: usize,
    pub child_fields: Vec<usize>,
    pub scalar_fields: Vec<usize>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Site {
    pub producer: usize,
    pub consumer: Option<usize>,
    pub block: BlockId,
    pub instruction: usize,
    pub shape: Option<Shape>,
    pub blockers: Vec<String>,
}

#[derive(Clone, Debug, Default)]
pub struct Survey {
    pub sites: Vec<Site>,
    /// A partial survey never means there were no other candidates.
    pub truncated: bool,
}

type Check<T> = Result<T, String>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Integer {
    Param(usize),
    Constant(i32),
    Decrement(usize),
    Computed,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Value {
    Int(Integer),
    Root,
    Child(usize),
    Null,
    Void,
}

#[derive(Default)]
struct Heap {
    allocated: bool,
    fields: BTreeMap<usize, Value>,
    children: usize,
    consumed: BTreeSet<usize>,
}

struct Checker<'a> {
    info: &'a dyn TreeModule,
    limits: Limits,
    remaining: usize,
}

impl Checker<'_> {
    /// Lowering can carry an unused register around a loop in a cycle of
    /// phis. Such a cycle does not expose the receiver. Follow phis backwards
    /// from non-phi uses, without removing any instruction or effect.
    fn live_phis(&mut self, caller: &Function) -> Check<BTreeSet<ValueId>> {
        let mut phis = BTreeMap::new();
        let mut work = Vec::new();
        for block in &caller.blocks {
            self.step()?;
            for phi in &block.phis {
                self.step()?;
                phis.insert(phi.dst, phi);
            }
            for instr in &block.instrs {
                self.step()?;
                work.extend(instr.uses());
            }
            work.extend(block.term.uses());
        }
        let mut live = BTreeSet::new();
        while let Some(value) = work.pop() {
            self.step()?;
            if let Some(phi) = phis.get(&value)
                && live.insert(value)
            {
                work.extend(phi.incoming.iter().map(|(_, v)| *v));
            }
        }
        Ok(live)
    }

    fn step(&mut self) -> Check<()> {
        self.remaining = self
            .remaining
            .checked_sub(1)
            .ok_or("analysis step budget exhausted")?;
        Ok(())
    }

    fn body(&mut self, index: usize) -> Check<Function> {
        self.step()?;
        let count = self
            .info
            .callee_ops(index)
            .ok_or("callee size/body unavailable")?;
        if count > self.limits.body_ops {
            return Err("callee body budget exceeded".into());
        }
        let f = self
            .info
            .callee(index)
            .ok_or("callee body unavailable")?
            .into_function(index, self.info)
            .map_err(|e| format!("lowering failed: {e}"))?;
        if f.blocks.len() + f.values.len() > self.limits.body_ops.saturating_mul(8)
            || f.blocks.iter().map(|b| b.instrs.len()).sum::<usize>()
                > self.limits.body_ops.saturating_mul(4)
            || f.allocated_bytes() > self.limits.body_ops.saturating_mul(4096)
        {
            return Err("lowered body budget exceeded".into());
        }
        super::verify::verify(&f).map_err(|e| format!("invalid AIR: {e}"))?;
        if !f.cells.is_empty() || f.blocks.iter().any(|b| b.handler.is_some()) {
            return Err("cells or exception regions are unsupported".into());
        }
        Ok(f)
    }

    /// One abstract execution. Calls to the producer/consumer are induction
    /// hypotheses, never unrolled. Constructors share only the fresh root.
    #[allow(clippy::too_many_arguments)]
    fn execute(
        &mut self,
        f: &Function,
        args: &[Value],
        tree: TypeRef,
        fields: &[TypeRef],
        role: Role,
        depth: usize,
        positive: bool,
        heap: &mut Heap,
    ) -> Check<Value> {
        let mut values = vec![None; f.values.len()];
        let mut visited = BTreeSet::new();
        let mut block = BlockId(0);
        let mut previous = None;
        loop {
            self.step()?;
            if !visited.insert(block) {
                return Err("CFG loop: termination unproven".into());
            }
            let b = &f.blocks[block.idx()];
            // Parallel phi semantics: read all incoming values before writing.
            let incoming: Check<Vec<_>> = b
                .phis
                .iter()
                .map(|phi| {
                    let src = phi
                        .incoming
                        .iter()
                        .find(|(pred, _)| Some(*pred) == previous)
                        .ok_or("phi predecessor unavailable")?
                        .1;
                    Ok((phi.dst, get(&values, src)?))
                })
                .collect();
            for (dst, value) in incoming? {
                values[dst.idx()] = Some(value);
            }
            for instr in &b.instrs {
                self.step()?;
                match instr {
                    Instr::Param { dst, reg } => {
                        values[dst.idx()] = args
                            .get(*reg as usize)
                            .copied()
                            .or_else(|| self.info.is_void(f.value_ty(*dst)).then_some(Value::Void));
                    }
                    Instr::Pos { .. } => {}
                    Instr::Copy { dst, src } => values[dst.idx()] = Some(get(&values, *src)?),
                    Instr::Int { dst, idx } if self.info.is_i32(f.value_ty(*dst)) => {
                        values[dst.idx()] = Some(Value::Int(Integer::Constant(
                            f.int_at(*idx, |i| self.info.int_value(i))
                                .ok_or("unknown integer constant")?,
                        )));
                    }
                    Instr::Null { dst } if f.value_ty(*dst) == tree => {
                        values[dst.idx()] = Some(Value::Null)
                    }
                    Instr::BinOp { dst, op, a, b }
                        if self.info.is_i32(f.value_ty(*dst))
                            && matches!(
                                op,
                                BinOp::Add
                                    | BinOp::Sub
                                    | BinOp::Mul
                                    | BinOp::And
                                    | BinOp::Or
                                    | BinOp::Xor
                            ) =>
                    {
                        let a = integer(get(&values, *a)?)?;
                        let b = integer(get(&values, *b)?)?;
                        let v = match (op, a, b) {
                            (BinOp::Sub, Integer::Param(p), Integer::Constant(1)) => {
                                Integer::Decrement(p)
                            }
                            _ => Integer::Computed,
                        };
                        values[dst.idx()] = Some(Value::Int(v));
                    }
                    Instr::UnOp {
                        dst,
                        op: UnOp::Neg,
                        src,
                    } if self.info.is_i32(f.value_ty(*dst)) => {
                        integer(get(&values, *src)?)?;
                        values[dst.idx()] = Some(Value::Int(Integer::Computed));
                    }
                    Instr::New { dst }
                        if matches!(role, Role::Producer(_)) && f.value_ty(*dst) == tree =>
                    {
                        if heap.allocated {
                            return Err("multiple local roots are unsupported".into());
                        }
                        heap.allocated = true;
                        values[dst.idx()] = Some(Value::Root);
                    }
                    Instr::FieldSet {
                        obj,
                        obj_ty,
                        field,
                        src,
                    } if !matches!(role, Role::Consumer(_)) => {
                        if *obj_ty != tree || get(&values, *obj)? != Value::Root || !heap.allocated
                        {
                            return Err("write outside the fresh root".into());
                        }
                        let value = get(&values, *src)?;
                        let ty = fields.get(*field).ok_or("unknown field layout")?;
                        if !matches!(
                            (self.info.is_i32(*ty), *ty == tree, value),
                            (true, _, Value::Int(_)) | (_, true, Value::Child(_) | Value::Null)
                        ) {
                            return Err("field stores an alias, cycle, or unsupported value".into());
                        }
                        if heap.fields.insert(*field, value).is_some() {
                            return Err("field is not initialized exactly once".into());
                        }
                    }
                    Instr::FieldGet {
                        dst,
                        obj,
                        obj_ty,
                        field,
                    } if matches!(role, Role::Consumer(_)) => {
                        if *obj_ty != tree || get(&values, *obj)? != Value::Root {
                            return Err("read outside the consumer root".into());
                        }
                        values[dst.idx()] = Some(
                            *heap
                                .fields
                                .get(field)
                                .ok_or("field read before initialization")?,
                        );
                    }
                    Instr::NullCheck { value } => {
                        if !matches!(get(&values, *value)?, Value::Root | Value::Child(_)) {
                            return Err("null check can throw".into());
                        }
                    }
                    Instr::Call {
                        dst,
                        fun,
                        args: call_args,
                    } => {
                        let actual: Check<Vec<_>> =
                            call_args.iter().map(|v| get(&values, *v)).collect();
                        let actual = actual?;
                        let result = match role {
                            Role::Producer(index) if *fun == index => {
                                if !positive
                                    || actual.get(depth)
                                        != Some(&Value::Int(Integer::Decrement(depth)))
                                {
                                    return Err(
                                        "recursive depth does not decrease under a positive guard"
                                            .into(),
                                    );
                                }
                                if actual.len() != args.len()
                                    || actual.iter().any(|v| !matches!(v, Value::Int(_)))
                                {
                                    return Err(
                                        "recursive producer arguments escape the scalar domain"
                                            .into(),
                                    );
                                }
                                let id = heap.children;
                                heap.children += 1;
                                Value::Child(id)
                            }
                            Role::Consumer(index) if *fun == index => {
                                let [Value::Child(id)] = actual.as_slice() else {
                                    return Err("consumer recursion is not on a fresh child".into());
                                };
                                if !heap.consumed.insert(*id) {
                                    return Err("consumer visits a child more than once".into());
                                }
                                Value::Int(Integer::Computed)
                            }
                            Role::Producer(_) => {
                                let (formal, ret) = self
                                    .info
                                    .signature(*fun)
                                    .ok_or("unresolved constructor call")?;
                                if !self.info.is_void(ret)
                                    || formal.len() != actual.len()
                                    || formal.first() != Some(&tree)
                                    || actual.first() != Some(&Value::Root)
                                {
                                    return Err(
                                        "non-constructor call or mutual recursion is unsupported"
                                            .into(),
                                    );
                                }
                                for (ty, value) in formal.iter().zip(&actual) {
                                    if !value_matches(self.info, *ty, tree, *value) {
                                        return Err("constructor argument type mismatch".into());
                                    }
                                }
                                let ctor = self.body(*fun)?;
                                let result = self.execute(
                                    &ctor,
                                    &actual,
                                    tree,
                                    fields,
                                    Role::Constructor,
                                    depth,
                                    positive,
                                    heap,
                                )?;
                                if result != Value::Void {
                                    return Err("constructor returns a value".into());
                                }
                                result
                            }
                            _ => return Err("unknown call, effects, or mutual recursion".into()),
                        };
                        values[dst.idx()] = Some(result);
                    }
                    _ => return Err(format!("unsupported operation/effect: {instr:?}")),
                }
            }
            let target = match &b.term {
                Terminator::Ret { value } => return get(&values, *value),
                Terminator::Jump { target } => *target,
                Terminator::CondJump {
                    cond,
                    a,
                    b,
                    if_true,
                    if_false,
                } if !matches!(role, Role::Constructor) => {
                    let a = get(&values, *a)?;
                    let b = b.map(|v| get(&values, v)).transpose()?;
                    let yes = branch(*cond, a, b, depth, positive)?;
                    if yes { *if_true } else { *if_false }
                }
                _ => {
                    return Err("unsupported control flow, exception, or constructor branch".into());
                }
            };
            previous = Some(block);
            block = target;
        }
    }

    fn pair(&mut self, producer: usize, consumer: usize) -> Check<Shape> {
        let (args, tree) = self
            .info
            .signature(producer)
            .ok_or("unknown producer signature")?;
        let (consumer_args, ret) = self
            .info
            .signature(consumer)
            .ok_or("unknown consumer signature")?;
        if args.is_empty()
            || args.len() > 8
            || args.iter().any(|ty| !self.info.is_i32(*ty))
            || consumer_args != [tree]
            || !self.info.is_i32(ret)
        {
            return Err("requires scalar i32 producer arguments and a single-root i32 fold".into());
        }
        let fields = self
            .info
            .plain_fields(tree)
            .ok_or("unknown/plain object layout unavailable")?;
        let child_fields: Vec<_> = fields
            .iter()
            .enumerate()
            .filter_map(|(i, t)| (*t == tree).then_some(i))
            .collect();
        if child_fields.is_empty()
            || fields.len() > 32
            || fields
                .iter()
                .any(|ty| *ty != tree && !self.info.is_i32(*ty))
        {
            return Err("requires recursive object and i32 fields".into());
        }
        let p = self.body(producer)?;
        let c = self.body(consumer)?;
        let actual: Vec<_> = (0..args.len())
            .map(|i| Value::Int(Integer::Param(i)))
            .collect();
        let mut reasons = Vec::new();
        for depth in 0..args.len() {
            let attempt = (|| {
                for positive in [false, true] {
                    let mut heap = Heap::default();
                    let value = self.execute(
                        &p,
                        &actual,
                        tree,
                        &fields,
                        Role::Producer(producer),
                        depth,
                        positive,
                        &mut heap,
                    )?;
                    if value != Value::Root || !heap.allocated || heap.fields.len() != fields.len()
                    {
                        return Err(
                            "producer does not return one completely initialized fresh root"
                                .to_string(),
                        );
                    }
                    let mut stored = BTreeSet::new();
                    for field in &child_fields {
                        match (positive, heap.fields.get(field)) {
                            (false, Some(Value::Null)) => {}
                            (true, Some(Value::Child(id))) if stored.insert(*id) => {}
                            _ => {
                                return Err(
                                    "child is shared, missing, or not null in the leaf".into()
                                );
                            }
                        }
                    }
                    if stored.len() != heap.children {
                        return Err("producer creates an unconsumed child".into());
                    }
                    let result = self.execute(
                        &c,
                        &[Value::Root],
                        tree,
                        &fields,
                        Role::Consumer(consumer),
                        depth,
                        positive,
                        &mut heap,
                    )?;
                    integer(result)?;
                    if stored != heap.consumed {
                        return Err(
                            "consumer does not visit every produced child exactly once".into()
                        );
                    }
                }
                Ok(())
            })();
            match attempt {
                Ok(()) => {
                    return Ok(Shape {
                        tree_type: tree,
                        depth_arg: depth,
                        scalar_fields: (0..fields.len())
                            .filter(|i| !child_fields.contains(i))
                            .collect(),
                        child_fields,
                    });
                }
                Err(e) => reasons.push(format!("arg {depth}: {e}")),
            }
        }
        Err(reasons.join("; "))
    }
}

#[derive(Clone, Copy)]
enum Role {
    Producer(usize),
    Consumer(usize),
    Constructor,
}

fn get(values: &[Option<Value>], id: ValueId) -> Check<Value> {
    values
        .get(id.idx())
        .copied()
        .flatten()
        .ok_or_else(|| "uninitialized/unknown value".into())
}

fn integer(value: Value) -> Check<Integer> {
    if let Value::Int(i) = value {
        Ok(i)
    } else {
        Err("non-integer used as a scalar".into())
    }
}

fn value_matches(info: &dyn TreeModule, ty: TypeRef, tree: TypeRef, value: Value) -> bool {
    (info.is_i32(ty) && matches!(value, Value::Int(_)))
        || (ty == tree && matches!(value, Value::Root | Value::Child(_) | Value::Null))
}

fn branch(cond: CondKind, a: Value, b: Option<Value>, depth: usize, positive: bool) -> Check<bool> {
    if b.is_none() && matches!(cond, CondKind::Null | CondKind::NotNull) {
        let null = match a {
            Value::Null => true,
            Value::Root | Value::Child(_) => false,
            _ => return Err("null comparison on an unknown value".into()),
        };
        return Ok(if cond == CondKind::Null { null } else { !null });
    }
    // Exactly d > 0 or its inverse/swapped spelling. At d > 0, d - 1
    // cannot wrap; all d <= 0 take the leaf, including i32::MIN.
    let d = Value::Int(Integer::Param(depth));
    let zero = Value::Int(Integer::Constant(0));
    match (cond, a, b) {
        (CondKind::SGt, x, Some(y)) if x == d && y == zero => Ok(positive),
        (CondKind::SLte, x, Some(y)) if x == d && y == zero => Ok(!positive),
        (CondKind::SLt, x, Some(y)) if x == zero && y == d => Ok(positive),
        (CondKind::SGte, x, Some(y)) if x == zero && y == d => Ok(!positive),
        _ => Err("branch is not a proven null test or positive depth guard".into()),
    }
}

/// Structural proof for a function pair, independent of caller escape and
/// embedder policy. Consumers must still inspect `survey`'s blockers.
pub fn analyze_pair(
    info: &dyn TreeModule,
    producer: usize,
    consumer: usize,
    limits: Limits,
) -> Check<Shape> {
    Checker {
        info,
        limits,
        remaining: limits.steps,
    }
    .pair(producer, consumer)
}

/// Survey raw (pre-inlining) AIR. No rewriting, module registration, or cache
/// mutation. Calls must be adjacent except for positions, copies and proven
/// receiver null checks. Every alias use is audited across the entire caller;
/// phi/cell escapes and delayed long-lived consumption are rejected.
pub fn survey(info: &dyn TreeModule, caller: &Function, limits: Limits) -> Survey {
    let mut reports = Vec::new();
    let mut truncated = false;
    let mut checker = Checker {
        info,
        limits,
        remaining: limits.steps,
    };
    let Ok(live_phis) = checker.live_phis(caller) else {
        return Survey {
            sites: reports,
            truncated: true,
        };
    };
    'blocks: for (bi, block) in caller.blocks.iter().enumerate() {
        for (ii, instr) in block.instrs.iter().enumerate() {
            if reports.len() >= limits.sites || checker.step().is_err() {
                truncated = true;
                break 'blocks;
            }
            let Instr::Call { dst, fun, .. } = instr else {
                continue;
            };
            let Some((_, tree)) = info.signature(*fun) else {
                continue;
            };
            if !info
                .plain_fields(tree)
                .is_some_and(|fields| fields.contains(&tree))
            {
                continue;
            }
            let mut report = Site {
                producer: *fun,
                consumer: None,
                block: BlockId(bi as u32),
                instruction: ii,
                shape: None,
                blockers: Vec::new(),
            };
            let mut aliases = BTreeSet::from([*dst]);
            let mut consumer_pos = None;
            for (offset, next) in block.instrs[ii + 1..].iter().enumerate() {
                if checker.step().is_err() {
                    report
                        .blockers
                        .push("analysis step budget exhausted".into());
                    break;
                }
                match next {
                    Instr::Pos { .. } => {}
                    Instr::Copy { dst, src } if aliases.contains(src) => {
                        aliases.insert(*dst);
                    }
                    Instr::NullCheck { value } if aliases.contains(value) => {}
                    Instr::Call { fun, args, .. }
                        if args.len() == 1 && aliases.contains(&args[0]) =>
                    {
                        report.consumer = Some(*fun);
                        consumer_pos = Some(ii + 1 + offset);
                        break;
                    }
                    _ => break,
                }
            }
            if let Some(consumer) = report.consumer {
                let mut escaped = false;
                for (bj, b) in caller.blocks.iter().enumerate() {
                    for phi in &b.phis {
                        if checker.step().is_err() {
                            escaped = true;
                            break;
                        }
                        if live_phis.contains(&phi.dst)
                            && phi.incoming.iter().any(|(_, v)| aliases.contains(v))
                        {
                            escaped = true;
                        }
                    }
                    for (ij, op) in b.instrs.iter().enumerate() {
                        if checker.step().is_err() {
                            escaped = true;
                            break;
                        }
                        if op.uses().iter().any(|v| aliases.contains(v)) {
                            let allowed = bj == bi
                                && ij > ii
                                && (Some(ij) == consumer_pos
                                    || (ij < consumer_pos.unwrap()
                                        && matches!(op,
                                    Instr::Copy { dst, src } if aliases.contains(src) && aliases.contains(dst)))
                                    || (ij < consumer_pos.unwrap()
                                        && matches!(op, Instr::NullCheck { .. })));
                            if !allowed {
                                escaped = true;
                            }
                        }
                    }
                    if b.term.uses().iter().any(|v| aliases.contains(v)) {
                        escaped = true;
                    }
                }
                if escaped || block.handler.is_some() {
                    report.blockers.push(
                        "receiver escapes, has additional uses, or crosses an exception region"
                            .into(),
                    );
                } else {
                    match checker.pair(*fun, consumer) {
                        Ok(shape) => report.shape = Some(shape),
                        Err(reason) => report.blockers.push(reason),
                    }
                }
            } else {
                report.blockers.push(
                    "no adjacent single-use direct consumer (delayed use or intervening effect)"
                        .into(),
                );
            }
            if report.shape.is_some() {
                match info.allocation_elision(tree) {
                    AllocationElision::Unknown => report.blockers.push(
                        "allocation-elision contract unknown (host hooks/GC observability)".into(),
                    ),
                    AllocationElision::Observable => report
                        .blockers
                        .push("type has observable allocation behavior".into()),
                    AllocationElision::Permitted => {}
                }
                if !info.stable_code_and_layout() {
                    report
                        .blockers
                        .push("code/layout lifetime or invalidation contract unknown".into());
                }
            }
            reports.push(report);
        }
    }
    Survey {
        sites: reports,
        truncated: truncated || checker.remaining == 0,
    }
}

#[cfg(test)]
mod tests;
