//! Materialize a proven producer/fold pair as scalar AIR. Symbolic execution
//! selects the two proven shapes; emitted scalar operations retain their
//! order within each shape. Recursive calls compute child folds in producer
//! order, which is legal only under TreeModule's explicit elision contract.
use super::*;
use crate::v2::passes::{DeadCodeElim, Pass, PassOptions};

#[derive(Clone, Copy)]
struct Traced {
    abstract_value: Value,
    scalar: Option<ValueId>,
}
impl Traced {
    fn plain(v: Value) -> Self {
        Self {
            abstract_value: v,
            scalar: None,
        }
    }
    fn scalar(v: Integer, id: ValueId) -> Self {
        Self {
            abstract_value: Value::Int(v),
            scalar: Some(id),
        }
    }
    fn id(self) -> Check<ValueId> {
        self.scalar.ok_or_else(|| "expected scalar result".into())
    }
}

struct Emitter<'a> {
    checker: Checker<'a>,
    out: Function,
    target: usize,
    scalar_type: TypeRef,
    fields: BTreeMap<usize, Traced>,
    children: Vec<ValueId>,
    shape: Shape,
    producer: usize,
    consumer: usize,
}

impl Emitter<'_> {
    fn fresh(&mut self) -> ValueId {
        let reg = self.out.new_reg(self.scalar_type);
        self.out.new_value(self.scalar_type, reg)
    }
    fn emit(&mut self, op: Instr) {
        self.out.blocks[self.target].instrs.push(op);
    }
    fn run(&mut self, f: &Function, args: &[Traced], role: Role, positive: bool) -> Check<Traced> {
        let mut values = vec![None; f.values.len()];
        let read = |values: &[Option<Traced>], v: ValueId| {
            values
                .get(v.idx())
                .copied()
                .flatten()
                .ok_or_else(|| "unavailable trace value".to_string())
        };
        let mut previous = None;
        let mut block = BlockId(0);
        let mut visited = BTreeSet::new();
        loop {
            self.checker.step()?;
            if !visited.insert(block) {
                return Err("unexpected cyclic trace".into());
            }
            let b = &f.blocks[block.idx()];
            let phis: Check<Vec<_>> = b
                .phis
                .iter()
                .map(|p| {
                    let v = p
                        .incoming
                        .iter()
                        .find(|(pred, _)| Some(*pred) == previous)
                        .ok_or("missing trace predecessor")?
                        .1;
                    Ok((p.dst, read(&values, v)?))
                })
                .collect();
            for (dst, v) in phis? {
                values[dst.idx()] = Some(v);
            }
            for op in &b.instrs {
                self.checker.step()?;
                let result = match op {
                    Instr::Param { reg, dst } => args.get(*reg as usize).copied().or_else(|| {
                        self.checker
                            .info
                            .is_void(f.value_ty(*dst))
                            .then_some(Traced::plain(Value::Void))
                    }),
                    Instr::Pos { .. } => {
                        self.emit(op.clone());
                        None
                    }
                    Instr::Copy { src, .. } => Some(read(&values, *src)?),
                    Instr::Int { idx, .. } => {
                        let n = f
                            .int_at(*idx, |i| self.checker.info.int_value(i))
                            .ok_or("unknown trace integer")?;
                        let idx = self.out.intern_int(n, |i| self.checker.info.int_value(i));
                        let dst = self.fresh();
                        self.emit(Instr::Int { dst, idx });
                        Some(Traced::scalar(Integer::Constant(n), dst))
                    }
                    Instr::BinOp { op, a, b, .. } => {
                        let a = read(&values, *a)?;
                        let b = read(&values, *b)?;
                        let abstract_value =
                            match (op, integer(a.abstract_value)?, integer(b.abstract_value)?) {
                                (BinOp::Sub, Integer::Param(p), Integer::Constant(1)) => {
                                    Integer::Decrement(p)
                                }
                                _ => Integer::Computed,
                            };
                        let dst = self.fresh();
                        self.emit(Instr::BinOp {
                            dst,
                            op: *op,
                            a: a.id()?,
                            b: b.id()?,
                        });
                        Some(Traced::scalar(abstract_value, dst))
                    }
                    Instr::UnOp { op, src, .. } => {
                        let src = read(&values, *src)?.id()?;
                        let dst = self.fresh();
                        self.emit(Instr::UnOp { dst, op: *op, src });
                        Some(Traced::scalar(Integer::Computed, dst))
                    }
                    Instr::New { .. } => Some(Traced::plain(Value::Root)),
                    Instr::Null { .. } => Some(Traced::plain(Value::Null)),
                    Instr::NullCheck { .. } => None, // Checker proved non-null on this path.
                    Instr::FieldSet { field, src, .. } => {
                        self.fields.insert(*field, read(&values, *src)?);
                        None
                    }
                    Instr::FieldGet { field, .. } => {
                        Some(*self.fields.get(field).ok_or("uninitialized trace field")?)
                    }
                    Instr::Call {
                        fun, args: actual, ..
                    } => {
                        let actual: Check<Vec<_>> =
                            actual.iter().map(|v| read(&values, *v)).collect();
                        let actual = actual?;
                        Some(match role {
                            Role::Producer(_) if *fun == self.producer => {
                                let args: Check<Vec<_>> = actual.iter().map(|v| v.id()).collect();
                                let dst = self.fresh();
                                self.emit(Instr::Call {
                                    dst,
                                    fun: self.out.findex.unwrap(),
                                    args: args?,
                                });
                                let child = self.children.len();
                                self.children.push(dst);
                                Traced::plain(Value::Child(child))
                            }
                            Role::Consumer(_) if *fun == self.consumer => {
                                let [
                                    Traced {
                                        abstract_value: Value::Child(id),
                                        ..
                                    },
                                ] = actual.as_slice()
                                else {
                                    return Err("unexpected child trace".into());
                                };
                                Traced::scalar(Integer::Computed, self.children[*id])
                            }
                            Role::Producer(_) => {
                                let ctor = self.checker.body(*fun)?;
                                self.run(&ctor, &actual, Role::Constructor, positive)?
                            }
                            _ => return Err("unexpected trace call".into()),
                        })
                    }
                    _ => return Err("unsupported trace instruction".into()),
                };
                if let Some(dst) = op.dst() {
                    values[dst.idx()] = result;
                }
            }
            let next = match &b.term {
                Terminator::Ret { value } => return read(&values, *value),
                Terminator::Jump { target } => *target,
                Terminator::CondJump {
                    cond,
                    a,
                    b,
                    if_true,
                    if_false,
                } => {
                    let a = read(&values, *a)?.abstract_value;
                    let b = b
                        .map(|v| read(&values, v).map(|v| v.abstract_value))
                        .transpose()?;
                    if branch(*cond, a, b, self.shape.depth_arg, positive)? {
                        *if_true
                    } else {
                        *if_false
                    }
                }
                _ => return Err("unsupported trace terminator".into()),
            };
            previous = Some(block);
            block = next;
        }
    }
}

/// Build a scalar recursive helper only after rechecking the pair and both
/// runtime contracts. The embedder reserves `helper` before calling this.
pub fn build_helper(
    info: &dyn TreeModule,
    producer: usize,
    consumer: usize,
    helper: usize,
    limits: Limits,
) -> Check<Function> {
    let mut checker = Checker {
        info,
        limits,
        remaining: limits.steps,
    };
    let shape = checker.pair(producer, consumer)?;
    if info.allocation_elision(shape.tree_type) != AllocationElision::Permitted
        || !info.stable_code_and_layout()
    {
        return Err("fusion runtime contracts are not established".into());
    }
    let (arg_types, _) = info
        .signature(producer)
        .ok_or("producer signature unavailable")?;
    let (_, ret) = info
        .signature(consumer)
        .ok_or("consumer signature unavailable")?;
    // The initial emitter uses one i32 type ID for fresh scalar values.
    if arg_types.iter().any(|t| *t != ret) {
        return Err("mixed scalar type IDs unsupported".into());
    }
    let p = checker.body(producer)?;
    let c = checker.body(consumer)?;
    let mut out = Function::new(arg_types.clone()).with_findex(helper);
    out.int_pool_base = info.int_pool_len();
    let mut params = Vec::new();
    let mut instrs = Vec::new();
    for (i, t) in arg_types.iter().enumerate() {
        let dst = out.new_value(*t, i as u32);
        instrs.push(Instr::Param { dst, reg: i as u32 });
        params.push(Traced::scalar(Integer::Param(i), dst));
    }
    let zero_reg = out.new_reg(ret);
    let zero = out.new_value(ret, zero_reg);
    let idx = out.intern_int(0, |i| info.int_value(i));
    instrs.push(Instr::Int { dst: zero, idx });
    out.blocks.push(Block {
        phis: vec![],
        instrs,
        term: Terminator::CondJump {
            cond: CondKind::SGt,
            a: params[shape.depth_arg].id()?,
            b: Some(zero),
            if_true: BlockId(1),
            if_false: BlockId(2),
        },
        handler: None,
    });
    for _ in 0..2 {
        out.blocks.push(Block {
            phis: vec![],
            instrs: vec![],
            term: Terminator::Ret { value: zero },
            handler: None,
        });
    }
    let mut e = Emitter {
        checker,
        out,
        target: 1,
        scalar_type: ret,
        fields: BTreeMap::new(),
        children: vec![],
        shape,
        producer,
        consumer,
    };
    for positive in [true, false] {
        e.target = if positive { 1 } else { 2 };
        e.fields.clear();
        e.children.clear();
        e.run(&p, &params, Role::Producer(producer), positive)?;
        let result = e.run(
            &c,
            &[Traced::plain(Value::Root)],
            Role::Consumer(consumer),
            positive,
        )?;
        e.out.blocks[e.target].term = Terminator::Ret {
            value: result.id()?,
        };
    }
    DeadCodeElim
        .run(&mut e.out, &PassOptions::default())
        .map_err(|e| e.to_string())?;
    super::super::verify::verify(&e.out).map_err(|e| e.to_string())?;
    Ok(e.out)
}

/// Re-survey the caller under the same module contract; stale Site objects
/// never authorize a rewrite. Each key names an already registered helper.
/// All work happens on a clone, and refusal leaves the original body intact.
pub fn rewrite_caller(
    info: &dyn TreeModule,
    caller: &Function,
    helpers: &BTreeMap<(usize, usize), usize>,
    limits: Limits,
) -> Check<(Function, usize)> {
    let sites = survey(info, caller, limits);
    if sites.truncated {
        return Err("caller survey exhausted its budget".into());
    }
    let mut out = caller.clone();
    let mut count = 0;
    // Replacements keep instruction positions stable until all sites are done.
    let mut remove = BTreeSet::new();
    for site in sites.sites {
        let Some(consumer) = site.consumer else {
            continue;
        };
        let Some(&helper) = helpers.get(&(site.producer, consumer)) else {
            continue;
        };
        if site.shape.is_none() || !site.blockers.is_empty() {
            continue;
        }
        let block = &mut out.blocks[site.block.idx()];
        let Instr::Call {
            dst: root, args, ..
        } = &block.instrs[site.instruction]
        else {
            return Err("producer moved".into());
        };
        let root = *root;
        let args = args.clone();
        let mut aliases = BTreeSet::from([root]);
        let mut found = false;
        for j in site.instruction + 1..block.instrs.len() {
            match &block.instrs[j] {
                Instr::Pos { .. } => {}
                Instr::Copy { dst, src } if aliases.contains(src) => {
                    aliases.insert(*dst);
                }
                Instr::NullCheck { value } if aliases.contains(value) => {
                    remove.insert((site.block.idx(), j));
                }
                Instr::Call {
                    dst,
                    fun,
                    args: actual,
                } if *fun == consumer && actual.len() == 1 && aliases.contains(&actual[0]) => {
                    block.instrs[j] = Instr::Call {
                        dst: *dst,
                        fun: helper,
                        args,
                    };
                    found = true;
                    break;
                }
                _ => return Err("consumer moved".into()),
            }
        }
        if !found {
            return Err("consumer unavailable".into());
        }
        // Unobserved phi cycles may still mention the old reference. Keep its
        // SSA definition until DCE removes those cycles, without an allocation.
        block.instrs[site.instruction] = Instr::Null { dst: root };
        count += 1;
    }
    if count == 0 {
        return Ok((out, 0));
    }
    for (bi, block) in out.blocks.iter_mut().enumerate() {
        let mut ii = 0;
        block.instrs.retain(|_| {
            let keep = !remove.contains(&(bi, ii));
            ii += 1;
            keep
        });
    }
    DeadCodeElim
        .run(&mut out, &PassOptions::default())
        .map_err(|e| e.to_string())?;
    super::super::verify::verify(&out).map_err(|e| e.to_string())?;
    Ok((out, count))
}
