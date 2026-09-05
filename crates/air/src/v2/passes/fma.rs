//! The fused-multiply-add peephole.

use super::{compact_values, Pass, PassOptions, PassStats};
use crate::v2::ir::*;
use anyhow::Result;
use std::collections::HashMap;

/// Rewrites a float multiply feeding a single add or subtract into
/// [`Instr::Fma`], which rounds the product-plus-addend once instead of
/// twice.
///
/// Fusion is what the reference implementations of these programs do — a C
/// port of the HL source built with clang's default `-ffp-contract=on`, and
/// the hxcpp and hxjava backends, all agree on the fused results — so the
/// peephole is on by default and [`PassOptions::fma`] turns it off for a
/// strict per-operation-rounding pipeline.
///
/// # Rewrite rules
///
/// Let `m: p = a * b` be a float `Mul` whose result `p` has **exactly one
/// use** in the whole function, and let that use be the instruction `u` in
/// the **same block**:
///
/// 1. `u: d = p + y` becomes `d = fma(a, b, y)`; `m` is deleted.
/// 2. `u: d = x + p` becomes `d = fma(a, b, x)`; `m` is deleted. IEEE
///    addition is commutative, so the operand order is free.
/// 3. `u: d = p - y` becomes `n = -y` immediately before `u`, then
///    `d = fma(a, b, n)`; `m` is deleted. `x - y` is exactly `x + (-y)` and
///    negation is exact, so the only rounding difference is the intended one.
/// 4. `u: d = x - p` replaces `m` in place with `n = -a`, then `u` becomes
///    `d = fma(n, b, x)`. `x - a*b` is exactly `(-a)*b + x`: negating a
///    multiplicand is exact and addition is commutative. The alternative
///    `-fma(a, b, -x)` would need two negations, so this form is preferred.
///
/// # Refusals
///
/// The rewrite is refused — leaving `Mul` and `Add`/`Sub` untouched — when:
///
/// * the product has more than one use, or its use is a phi source, a
///   terminator operand, or in another block;
/// * the type is not one of the module's float types
///   ([`Function::float_types`]), which also makes the pass inert when
///   lowering had no module info;
/// * the register of `a` or `b` is written between `m` and `u`, since the
///   fused instruction reads them at `u`'s position;
/// * for the negating forms 3 and 4, the register of `a` or `b` is the
///   product's own register: those forms write the negation into the register
///   the deleted product freed, and would clobber an operand still needed.
///
/// The freed product register is exactly what the negation values reuse, so
/// no rewrite here creates or extends a live range, and the de-SSA register
/// assignment stays valid without any new registers.
pub struct FmaPeephole;

#[derive(Debug, Clone, Copy)]
enum Form {
    /// Delete the `Mul`; the user becomes `fma(a, b, other)`.
    Direct,
    /// Delete the `Mul`; negate the addend just before the user.
    NegAddend,
    /// Replace the `Mul` with a negation of its first operand.
    NegProduct,
}

#[derive(Debug, Clone, Copy)]
struct Plan {
    mul: usize,
    user: usize,
    form: Form,
    a: ValueId,
    b: ValueId,
    /// The addend for `Direct`, the subtrahend to negate for `NegAddend`, the
    /// minuend for `NegProduct`.
    c: ValueId,
    /// Negation destination for the two negating forms.
    neg: Option<ValueId>,
}

impl Pass for FmaPeephole {
    fn name(&self) -> &'static str {
        "fma"
    }

    fn run(&self, f: &mut Function, opts: &PassOptions) -> Result<PassStats> {
        let mut stats = PassStats::default();
        if !opts.fma || f.float_types.is_empty() {
            return Ok(stats);
        }
        let counts = f.use_counts();

        for b in 0..f.blocks.len() {
            let plans = plan_block(f, &counts, b);
            if plans.is_empty() {
                continue;
            }
            // Allocate the negation values, reusing the product's register.
            let mut plans: Vec<Plan> = plans;
            for p in plans.iter_mut() {
                if matches!(p.form, Form::NegAddend | Form::NegProduct) {
                    let prod = f.blocks[b].instrs[p.mul]
                        .dst()
                        .expect("planned Mul defines a value");
                    let ty = f.value_ty(prod);
                    let reg = f.value_reg(prod);
                    p.neg = Some(f.new_value(ty, reg));
                }
            }
            let by_mul: HashMap<usize, Plan> = plans.iter().map(|p| (p.mul, *p)).collect();
            let by_user: HashMap<usize, Plan> = plans.iter().map(|p| (p.user, *p)).collect();

            let old = std::mem::take(&mut f.blocks[b].instrs);
            let mut out: Vec<Instr> = Vec::with_capacity(old.len());
            for (k, ins) in old.into_iter().enumerate() {
                if let Some(p) = by_mul.get(&k) {
                    if let Form::NegProduct = p.form {
                        out.push(Instr::UnOp {
                            op: UnOp::Neg,
                            dst: p.neg.expect("negating form allocates a value"),
                            src: p.a,
                        });
                    }
                    continue;
                }
                if let Some(p) = by_user.get(&k) {
                    let dst = ins.dst().expect("planned user defines a value");
                    match p.form {
                        Form::Direct => out.push(Instr::Fma {
                            dst,
                            a: p.a,
                            b: p.b,
                            c: p.c,
                        }),
                        Form::NegAddend => {
                            let n = p.neg.expect("negating form allocates a value");
                            out.push(Instr::UnOp {
                                op: UnOp::Neg,
                                dst: n,
                                src: p.c,
                            });
                            out.push(Instr::Fma {
                                dst,
                                a: p.a,
                                b: p.b,
                                c: n,
                            });
                        }
                        Form::NegProduct => out.push(Instr::Fma {
                            dst,
                            a: p.neg.expect("negating form allocates a value"),
                            b: p.b,
                            c: p.c,
                        }),
                    }
                    stats.fused += 1;
                    continue;
                }
                out.push(ins);
            }
            f.blocks[b].instrs = out;
        }

        if stats.fused > 0 {
            compact_values(f)?;
        }
        Ok(stats)
    }
}

/// True when nothing between the two instruction indices writes `reg`.
fn reg_untouched_between(f: &Function, b: usize, from: usize, to: usize, reg: u32) -> bool {
    for ins in &f.blocks[b].instrs[from + 1..to] {
        if let Some(d) = ins.dst() {
            if f.value_reg(d) == reg {
                return false;
            }
        }
        match ins {
            Instr::CellSet { cell, .. } | Instr::CellIncr { cell } | Instr::CellDecr { cell }
                if f.cells[cell.idx()].reg == reg =>
            {
                return false;
            }
            Instr::Asm { reg: r, .. } if *r == reg => return false,
            _ => {}
        }
    }
    true
}

fn plan_block(f: &Function, counts: &[usize], b: usize) -> Vec<Plan> {
    let instrs = &f.blocks[b].instrs;
    // Single-use float products defined in this block.
    let mut product: HashMap<ValueId, (usize, ValueId, ValueId)> = HashMap::new();
    for (k, ins) in instrs.iter().enumerate() {
        if let Instr::BinOp {
            op: BinOp::Mul,
            dst,
            a,
            b: mb,
        } = ins
        {
            if counts[dst.idx()] == 1 && f.value_is_float(*dst) {
                product.insert(*dst, (k, *a, *mb));
            }
        }
    }
    if product.is_empty() {
        return vec![];
    }

    let mut plans: Vec<Plan> = Vec::new();
    let mut consumed: Vec<ValueId> = Vec::new();
    for (k, ins) in instrs.iter().enumerate() {
        let Instr::BinOp { op, dst, a, b: rb } = ins else {
            continue;
        };
        let (add, sub) = (*op == BinOp::Add, *op == BinOp::Sub);
        if !(add || sub) || !f.value_is_float(*dst) {
            continue;
        }
        // Left operand first, then right: `p + p` cannot occur (two uses).
        for (side_left, cand, other) in [(true, *a, *rb), (false, *rb, *a)] {
            let Some(&(mk, ma, mb)) = product.get(&cand) else {
                continue;
            };
            if consumed.contains(&cand) || mk >= k {
                continue;
            }
            let form = match (add, side_left) {
                (true, _) => Form::Direct,
                (false, true) => Form::NegAddend,
                (false, false) => Form::NegProduct,
            };
            let prod_reg = f.value_reg(cand);
            let (ra, rbb) = (f.value_reg(ma), f.value_reg(mb));
            if !reg_untouched_between(f, b, mk, k, ra) || !reg_untouched_between(f, b, mk, k, rbb) {
                continue;
            }
            if matches!(form, Form::NegAddend | Form::NegProduct)
                && (ra == prod_reg || rbb == prod_reg)
            {
                continue;
            }
            plans.push(Plan {
                mul: mk,
                user: k,
                form,
                a: ma,
                b: mb,
                c: other,
                neg: None,
            });
            consumed.push(cand);
            break;
        }
    }
    plans
}
