//! Self-recursive tail-call elimination.

use super::{compact_values, replace_all_uses, Pass, PassOptions, PassStats};
use crate::v2::ir::*;
use anyhow::Result;
use std::collections::HashMap;

/// Turns self-recursive tail calls into a back edge to a loop header.
///
/// A `Call` to the enclosing function whose result reaches the block's `Ret`
/// — through any chain of `Copy`s — becomes a parallel copy of the argument
/// values into the entry parameters followed by a jump to a loop header. The
/// header is introduced by splitting the entry block: the entry keeps the
/// `Param` definitions, the header takes one phi per argument register, and
/// every use of a parameter is redirected to its phi. De-SSA then materializes
/// the parallel copy on the back edge, breaking argument-permutation cycles
/// with temporaries exactly as it does for any other phi.
///
/// This pays off on every engine, not only the compiled ones: the interpreter
/// burns a host stack frame per HL call, so the serialized output gets the
/// same iterative shape.
///
/// Guarantees:
/// * **self-recursion only** — the callee must be [`Function::findex`], so a
///   function lowered without its identity is left alone and mutual recursion
///   is out of scope;
/// * **never inside a trap region** — a call covered by a handler is not a
///   tail call, because the handler has to stay live for the duration of the
///   callee; the rewrite is refused when the returning block has a
///   [`Block::handler`];
/// * **only trailing copies may follow the call** — everything between the
///   call and the `Ret` is dropped, so anything with an effect refuses the
///   rewrite;
/// * **cell parameters refuse the rewrite** — a pinned argument register is a
///   memory slot whose address may have escaped, and each real activation gets
///   a fresh one, so it cannot be silently re-bound to the next iteration's
///   value;
/// * **live parameters of non-argument registers refuse the rewrite** — a
///   local register read before it is written observes its frame's initial
///   value, which a back edge would replace with the previous iteration's;
/// * **register-correct by construction** — each header phi is assigned the
///   argument register it merges, which is the register its `Param` already
///   held, so no live range is lengthened and nothing needs privatizing.
pub struct TailRecursionElim;

/// One self-recursive tail call.
struct Site {
    block: BlockId,
    /// Index of the `Call` instruction inside `block`.
    call: usize,
}

impl Pass for TailRecursionElim {
    fn name(&self) -> &'static str {
        "tre"
    }

    fn run(&self, f: &mut Function, _opts: &PassOptions) -> Result<PassStats> {
        let mut stats = PassStats::default();
        let Some(self_findex) = f.findex else {
            return Ok(stats);
        };
        // The entry block is `Param`s plus a jump; the header splits that edge.
        let Terminator::Jump { target: first } = f.blocks[0].term else {
            return Ok(stats);
        };

        let sites = find_sites(f, self_findex);
        if sites.is_empty() {
            return Ok(stats);
        }

        // Every site calls the same function, so they share an arity.
        let arity = call_args(f, &sites[0]).len();
        if arity > f.reg_types.len() || sites.iter().any(|s| call_args(f, s).len() != arity) {
            return Ok(stats);
        }
        // A pinned argument register is a memory slot, not a bindable name.
        if (0..arity as u32).any(|r| f.cells.iter().any(|c| c.reg == r)) {
            return Ok(stats);
        }

        let mut param_of: HashMap<u32, ValueId> = HashMap::new();
        for ins in &f.blocks[0].instrs {
            if let Instr::Param { dst, reg } = ins {
                param_of.insert(*reg, *dst);
            }
        }
        if (0..arity as u32).any(|r| !param_of.contains_key(&r)) {
            return Ok(stats);
        }
        // A local whose entry value is still read would see the previous
        // iteration's register instead of a fresh frame's initial value.
        let counts = f.use_counts();
        if param_of
            .iter()
            .any(|(&r, &v)| (r as usize) >= arity && counts[v.idx()] > 0)
        {
            return Ok(stats);
        }
        // Phi type consistency: each argument must have its parameter's type.
        for s in &sites {
            for (j, &a) in call_args(f, s).iter().enumerate() {
                if f.value_ty(a) != f.value_ty(param_of[&(j as u32)]) {
                    return Ok(stats);
                }
            }
        }

        // ---- rewrite --------------------------------------------------------
        let header = BlockId(f.blocks.len() as u32);

        // One header phi per argument register, carrying that register.
        let phi_of: Vec<(ValueId, ValueId)> = (0..arity)
            .map(|j| {
                let pv = param_of[&(j as u32)];
                let nv = f.new_value(f.value_ty(pv), j as u32);
                (pv, nv)
            })
            .collect();

        // Redirect the parameters *before* reading the call arguments back, so
        // a site that passes a parameter through unchanged feeds the phi with
        // the current iteration's value rather than the entry value.
        for &(pv, nv) in &phi_of {
            stats.replaced += replace_all_uses(f, pv, nv);
        }

        let phis: Vec<Phi> = phi_of
            .iter()
            .enumerate()
            .map(|(j, &(pv, nv))| {
                let mut incoming = vec![(BlockId(0), pv)];
                for s in &sites {
                    incoming.push((s.block, call_args(f, s)[j]));
                }
                Phi { dst: nv, incoming }
            })
            .collect();

        f.blocks.push(Block {
            phis,
            instrs: vec![],
            term: Terminator::Jump { target: first },
            handler: None,
        });
        f.blocks[0].term = Terminator::Jump { target: header };
        // The header now stands between the entry and the first lowered block.
        for phi in f.blocks[first.idx()].phis.iter_mut() {
            for (p, _) in phi.incoming.iter_mut() {
                if *p == BlockId(0) {
                    *p = header;
                }
            }
        }

        for s in &sites {
            let blk = &mut f.blocks[s.block.idx()];
            stats.eliminated += blk.instrs.len() - s.call;
            blk.instrs.truncate(s.call);
            blk.term = Terminator::Jump { target: header };
            stats.tail_calls += 1;
        }
        compact_values(f)?;
        Ok(stats)
    }
}

/// The argument list of a site's call.
fn call_args<'a>(f: &'a Function, s: &Site) -> &'a [ValueId] {
    match &f.blocks[s.block.idx()].instrs[s.call] {
        Instr::Call { args, .. } => args,
        _ => unreachable!("site indexes a Call"),
    }
}

/// Blocks whose `Ret` is fed, through copies only, by a call to `self_findex`
/// placed so that nothing else runs between the call and the return.
fn find_sites(f: &Function, self_findex: usize) -> Vec<Site> {
    let mut out = Vec::new();
    for (b, blk) in f.blocks.iter().enumerate() {
        let Terminator::Ret { value } = blk.term else {
            continue;
        };
        // A call covered by a handler is not in tail position: the handler
        // must stay live while the callee runs.
        if blk.handler.is_some() {
            continue;
        }
        // Where each value defined in this block comes from.
        let mut at: HashMap<ValueId, usize> = HashMap::new();
        for (k, ins) in blk.instrs.iter().enumerate() {
            if let Some(d) = ins.dst() {
                at.insert(d, k);
            }
        }
        // Walk the copy chain back to its origin.
        let mut cur = value;
        let call = loop {
            let Some(&k) = at.get(&cur) else { break None };
            match &blk.instrs[k] {
                Instr::Copy { src, .. } => cur = *src,
                Instr::Call { fun, .. } if *fun == self_findex => break Some(k),
                _ => break None,
            }
        };
        let Some(call) = call else { continue };
        // Everything after the call is dropped by the rewrite, so it must be
        // nothing but copies.
        if blk.instrs[call + 1..]
            .iter()
            .any(|i| !matches!(i, Instr::Copy { .. }))
        {
            continue;
        }
        out.push(Site {
            block: BlockId(b as u32),
            call,
        });
    }
    out
}
