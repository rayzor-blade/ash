//! Module preparation for generated scalar helpers. This runs before any
//! runtime, AIR cache, dispatch table or tier owns the module. Helpers become
//! ordinary bytecode functions; no backend invents its own copy.
use std::collections::{BTreeMap, BTreeSet};

use air::v2::ir::{Function, Instr, Terminator};
use anyhow::{Result, bail, ensure};

use crate::air_pipeline::AshModule;
use crate::bytecode::DecodedBytecode;
use crate::hl::{hl_type_kind_HFUN, hl_type_kind_HI32};
use crate::opcodes::{Opcode, RefFun, Reg};
use crate::types::{HLFunction, HLType, HLTypeFun, TypeRef};

/// Deduplication is local to one immutable input module snapshot. The pass
/// version and every specialization choice belong here. There is deliberately
/// no process-global helper/proof cache or hash-based reuse across modules.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Key {
    pub pass: String,
    pub version: u32,
    pub sources: Vec<usize>,
    pub specialization: Vec<i64>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Declaration {
    pub key: Key,
    pub args: Vec<usize>,
    pub ret: usize,
}

/// Only portable scalar helpers are admitted at this checkpoint. Registering
/// a body grants no allocation-elision or host-metadata specialization rights.
#[derive(Clone, Debug)]
pub struct Registered {
    pub declaration: Declaration,
    pub findex: usize,
    pub(crate) generation: u64,
}

/// A new preparation never shares a canonical AIR cache entry with an old
/// snapshot, even if its address and reserved IDs are reused by the allocator.
pub(crate) fn generation(bc: &DecodedBytecode) -> u64 {
    bc.generated_helpers.first().map_or(0, |h| h.generation)
}

#[derive(Clone, Copy, Debug)]
pub struct Limits {
    pub helpers: usize,
    pub body_ops: usize,
    pub total_ops: usize,
    pub air_bytes: usize,
}

impl Default for Limits {
    fn default() -> Self {
        Self {
            helpers: 32,
            body_ops: 2048,
            total_ops: 16_384,
            air_bytes: 16 * 1024 * 1024,
        }
    }
}

/// Prepare a fresh module atomically. All IDs are reserved in key order before
/// calling `build`, permitting self/mutual references. Identical declarations
/// invoke `build` once; conflicting signatures refuse the entire transaction.
/// Failure leaves `input` untouched, so the optimizer can retain its original
/// callers. Reprepare from original input after any code/type/host change.
///
/// Callers must prepare before creating an Arc or runtime tables, and must not
/// mutate public module fields afterwards. Host registration and hot reload
/// explicitly refuse a prepared module until invalidation is implemented.
pub fn prepare(
    input: &DecodedBytecode,
    declarations: impl IntoIterator<Item = Declaration>,
    limits: Limits,
    mut build: impl FnMut(&Declaration, &BTreeMap<Key, usize>) -> Result<Function>,
) -> Result<DecodedBytecode> {
    ensure!(
        input.generated_helpers.is_empty(),
        "module already prepared; rebuild from original input"
    );
    let mut declarations_by_key = BTreeMap::new();
    let scalar = |t: usize| {
        input
            .types
            .get(t)
            .is_some_and(|t| t.kind == hl_type_kind_HI32)
    };
    for d in declarations {
        ensure!(
            d.key.pass.len() <= 128
                && d.key.sources.len() <= 32
                && d.key.specialization.len() <= 32,
            "helper key budget exceeded"
        );
        ensure!(
            d.args.len() <= 16 && scalar(d.ret) && d.args.iter().all(|t| scalar(*t)),
            "generated helpers currently require i32 arguments and return"
        );
        ensure!(
            d.key
                .sources
                .iter()
                .all(|id| input.functions.iter().any(|f| f.findex as usize == *id)),
            "helper source is unavailable"
        );
        if let Some(old) = declarations_by_key.get(&d.key) {
            ensure!(old == &d, "conflicting helper declaration");
        } else {
            ensure!(
                declarations_by_key.len() < limits.helpers,
                "helper count budget exceeded"
            );
            declarations_by_key.insert(d.key.clone(), d);
        }
    }
    if declarations_by_key.is_empty() {
        return Ok(input.clone());
    }
    let n = input.functions.len() + input.natives.len();
    let existing: BTreeSet<_> = input
        .functions
        .iter()
        .map(|f| f.findex)
        .chain(input.natives.iter().map(|f| f.findex))
        .collect();
    ensure!(
        existing.len() == n
            && existing
                .iter()
                .enumerate()
                .all(|(i, id)| *id >= 0 && *id as usize == i),
        "generated registration requires a dense unique function table"
    );
    ensure!(
        n.checked_add(declarations_by_key.len())
            .is_some_and(|v| v < 0x2000_0000),
        "function ID overflow"
    );
    let ids: BTreeMap<_, _> = declarations_by_key
        .keys()
        .cloned()
        .enumerate()
        .map(|(i, k)| (k, n + i))
        .collect();
    let signatures: BTreeMap<_, _> = declarations_by_key
        .values()
        .map(|d| (ids[&d.key], (&d.args, d.ret)))
        .collect();
    let mut result = input.clone();
    static NEXT_GENERATION: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);
    let generation = NEXT_GENERATION
        .fetch_update(
            std::sync::atomic::Ordering::Relaxed,
            std::sync::atomic::Ordering::Relaxed,
            |v| v.checked_add(1),
        )
        .map_err(|_| anyhow::anyhow!("helper generation exhausted"))?;
    let mut total_ops = 0usize;
    let mut total_bytes = 0usize;
    for d in declarations_by_key.values() {
        let id = ids[&d.key];
        let body = build(d, &ids)?;
        total_bytes = total_bytes
            .checked_add(body.allocated_bytes())
            .ok_or_else(|| anyhow::anyhow!("AIR size overflow"))?;
        ensure!(
            total_bytes <= limits.air_bytes,
            "helper AIR budget exceeded"
        );
        let ops: usize = body.blocks.iter().map(|b| b.instrs.len() + 1).sum();
        ensure!(
            ops <= limits.body_ops && body.values.len() <= limits.body_ops.saturating_mul(8),
            "helper body budget exceeded"
        );
        ensure!(
            body.findex == Some(id),
            "helper body identity differs from its reservation"
        );
        air::v2::verify(&body)?;
        ensure!(
            body.reg_types.len() >= d.args.len()
                && body.reg_types.iter().all(|t| scalar(t.0 as usize))
                && body
                    .reg_types
                    .iter()
                    .take(d.args.len())
                    .map(|t| t.0 as usize)
                    .eq(d.args.iter().copied()),
            "helper register signature mismatch"
        );
        ensure!(
            body.values.iter().all(|v| scalar(v.ty.0 as usize)
                && v.lanes == 1
                && body.reg_types.get(v.reg as usize) == Some(&v.ty)),
            "non-scalar helper value or invalid register assignment"
        );
        ensure!(
            body.cells.is_empty() && body.inline_sites.is_empty() && body.simd_scratch.is_none(),
            "generated helper has unsupported cells, inline frames or SIMD scratch"
        );
        ensure!(
            body.int_pool_base == input.ints.len(),
            "helper belongs to another constant pool"
        );
        for block in &body.blocks {
            ensure!(
                block.handler.is_none(),
                "generated helper exception regions unsupported"
            );
            for instr in &block.instrs {
                match instr {
                    Instr::Param { dst, reg } => ensure!(
                        body.value_reg(*dst) == *reg,
                        "helper parameter register mismatch"
                    ),
                    Instr::Copy { .. } | Instr::BinOp { .. } | Instr::UnOp { .. } => {}
                    Instr::Int { idx, .. } => ensure!(
                        *idx < input.ints.len() + body.pending_ints.len(),
                        "invalid helper constant"
                    ),
                    Instr::Pos { file, line, site } => ensure!(
                        (*file as usize) < input.debug_files.len()
                            && *line <= i32::MAX as u32
                            && site.is_none(),
                        "invalid helper source position"
                    ),
                    Instr::Call { dst, fun, args } => {
                        let (expected, ret) = if let Some((args, ret)) = signatures.get(fun) {
                            ((*args).clone(), *ret)
                        } else {
                            let f = input
                                .functions
                                .iter()
                                .find(|f| f.findex as usize == *fun)
                                .ok_or_else(|| anyhow::anyhow!("unresolved helper call {fun}"))?;
                            let signature = input
                                .types
                                .get(f.type_.0)
                                .and_then(|t| t.fun.as_ref())
                                .ok_or_else(|| anyhow::anyhow!("missing call signature"))?;
                            (
                                signature.args.iter().map(|t| t.0).collect(),
                                signature.ret.0,
                            )
                        };
                        ensure!(
                            args.len() == expected.len()
                                && args
                                    .iter()
                                    .zip(expected)
                                    .all(|(v, t)| body.value_ty(*v).0 as usize == t)
                                && body.value_ty(*dst).0 as usize == ret,
                            "helper call signature mismatch"
                        );
                    }
                    _ => bail!("unsupported generated helper instruction: {instr:?}"),
                }
            }
            match &block.term {
                Terminator::Ret { value } => ensure!(
                    body.value_ty(*value).0 as usize == d.ret,
                    "helper return signature mismatch"
                ),
                Terminator::Jump { .. }
                | Terminator::CondJump { .. }
                | Terminator::Switch { .. } => {}
                _ => bail!("unsupported generated helper terminator"),
            }
        }
        let mut ser = air::v2::serialize::serialize_with_int_base(&body, input.ints.len())?;
        ensure!(
            ser.ops.len() <= limits.body_ops,
            "serialized helper body budget exceeded"
        );
        total_ops = total_ops
            .checked_add(ser.ops.len())
            .ok_or_else(|| anyhow::anyhow!("helper size overflow"))?;
        ensure!(
            total_ops <= limits.total_ops,
            "total helper code budget exceeded"
        );
        // Every builder sees the original pool. Rebase its private constants
        // into the module extension, once, before any backend sees the body.
        let int_offset = result.ints.len() - input.ints.len();
        for op in &mut ser.ops {
            if let Opcode::Int { ptr, .. } = op
                && ptr.0 >= input.ints.len()
            {
                ptr.0 += int_offset;
            }
        }
        let debug = source_positions(&body, &ser);
        result.ints.extend(ser.new_ints);
        let ty = result
            .types
            .iter()
            .position(|t| {
                t.kind == hl_type_kind_HFUN
                    && t.fun.as_ref().is_some_and(|f| {
                        f.args.iter().map(|t| t.0).eq(d.args.iter().copied())
                            && f.ret.0 == d.ret
                            && f.parent.is_none()
                            && f.closure.is_none()
                            && f.closure_type.is_none()
                    })
            })
            .unwrap_or_else(|| {
                let idx = result.types.len();
                result.types.push(HLType {
                    kind: hl_type_kind_HFUN,
                    fun: Some(HLTypeFun {
                        args: d.args.iter().copied().map(TypeRef).collect(),
                        ret: TypeRef(d.ret),
                        ..Default::default()
                    }),
                    ..Default::default()
                });
                idx
            });
        let mut f = HLFunction::with_body(ser.ops, debug);
        f.findex = id as i32;
        f.type_ = TypeRef(ty);
        f.regs = ser
            .reg_types
            .iter()
            .map(|t| TypeRef(t.0 as usize))
            .collect();
        f.field_name = Some(format!("$generated.{}.{}", d.key.pass, id));
        result.push_function(f);
        result.generated_helpers.push(Registered {
            declaration: d.clone(),
            findex: id,
            generation,
        });
    }
    Ok(result)
}

pub(crate) fn source_positions(body: &Function, ser: &air::v2::Serialized) -> Vec<i32> {
    let mut debug = vec![-1; ser.ops.len() * 2];
    for (bi, block) in body.blocks.iter().enumerate() {
        let mut position = [-1, 0];
        for (ii, op) in block.instrs.iter().enumerate() {
            if let Instr::Pos { file, line, .. } = op {
                position = [*file as i32, *line as i32];
            }
            let start = ser.instr_pcs[bi][ii];
            let end = ser.instr_pcs[bi]
                .get(ii + 1)
                .copied()
                .unwrap_or(ser.term_pcs[bi]);
            for pc in start..end {
                debug[pc * 2..pc * 2 + 2].copy_from_slice(&position);
            }
        }
        let pc = ser.term_pcs[bi];
        if pc < ser.ops.len() {
            debug[pc * 2..pc * 2 + 2].copy_from_slice(&position);
        }
    }
    debug
}

/// Development probe: copy one scalar function into a registered helper,
/// redirect its self calls, and replace its old body with a forwarding call.
/// This tests registration/dispatch, not tree fusion or allocation elision.
pub fn probe(input: &DecodedBytecode, source: usize) -> Result<DecodedBytecode> {
    let original = input
        .functions
        .iter()
        .find(|f| f.findex as usize == source)
        .ok_or_else(|| anyhow::anyhow!("probe source {source} is unavailable"))?;
    let signature = input
        .types
        .get(original.type_.0)
        .and_then(|t| t.fun.as_ref())
        .ok_or_else(|| anyhow::anyhow!("probe source has no signature"))?;
    let declaration = Declaration {
        key: Key {
            pass: "probe".into(),
            version: 1,
            sources: vec![source],
            specialization: vec![],
        },
        args: signature.args.iter().map(|t| t.0).collect(),
        ret: signature.ret.0,
    };
    let mut result = prepare(input, [declaration], Limits::default(), |d, ids| {
        let mut body = air::v2::lower_with_positions(
            original.ops(),
            &original
                .regs
                .iter()
                .map(|t| air::v2::TypeRef(t.0 as u32))
                .collect::<Vec<_>>(),
            &AshModule::new(input),
            Some(original.debug()),
        )?;
        let id = ids[&d.key];
        body.findex = Some(id);
        for b in &mut body.blocks {
            for op in &mut b.instrs {
                if let Instr::Call { fun, .. } = op
                    && *fun == source
                {
                    *fun = id;
                }
            }
        }
        Ok(body)
    })?;
    let id = result.generated_helpers[0].findex;
    let f = result
        .functions
        .iter_mut()
        .find(|f| f.findex as usize == source)
        .unwrap();
    f.regs = signature.args.clone();
    let dst = Reg(f.regs.len() as u32);
    f.regs.push(signature.ret.clone());
    f.set_ops(vec![
        Opcode::CallN {
            dst,
            fun: RefFun(id),
            args: (0..dst.0).map(Reg).collect(),
        },
        Opcode::Ret { ret: dst },
    ]);
    let position = original
        .debug()
        .chunks_exact(2)
        .find(|p| p[0] >= 0)
        .unwrap_or(&[-1, 0]);
    f.set_debug([position, position].concat());
    // The wrapper also changed a body that the decoder may have summarized.
    result.body_facts.take();
    Ok(result)
}

#[cfg(test)]
mod tests;
