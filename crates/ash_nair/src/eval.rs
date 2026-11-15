//! A reference interpreter for the AIR that [`crate::lower`] produces.
//!
//! It runs integers, floats and bools, branches, phis, direct calls, and nothing else. Its job
//! is to say what a lowered function computes, so a test can compare that with what the Haxe
//! source means without going through an engine.

use std::collections::HashMap;

use air::v2::ir::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Val {
    I(i32),
    F(f64),
    B(bool),
    Void,
}

/// The functions that can be called, and the constant pools they index.
pub struct Program<'a> {
    pub functions: HashMap<usize, &'a Function>,
    pub ints: &'a [i32],
    pub floats: &'a [f64],
}

const STEP_LIMIT: usize = 50_000_000;

pub fn run(p: &Program, findex: usize, args: &[Val]) -> Result<Val, String> {
    let mut steps = 0;
    call(p, findex, args, &mut steps, 0)
}

fn call(
    p: &Program,
    findex: usize,
    args: &[Val],
    steps: &mut usize,
    depth: usize,
) -> Result<Val, String> {
    if depth > 2000 {
        return Err("the call stack is too deep".into());
    }
    let f = p
        .functions
        .get(&findex)
        .ok_or_else(|| format!("no function {findex}"))?;
    let mut vals: Vec<Option<Val>> = vec![None; f.values.len()];
    let get = |vals: &[Option<Val>], v: ValueId| -> Result<Val, String> {
        vals[v.idx()].ok_or_else(|| format!("v{} is read before it is defined", v.0))
    };
    let mut block = 0usize;
    let mut prev: Option<usize> = None;
    loop {
        let b = &f.blocks[block];
        // phis read the values of the edge taken, all before any is written
        let mut incoming = Vec::new();
        for phi in &b.phis {
            let from = prev.ok_or("a phi in the entry block")?;
            let (_, v) = phi
                .incoming
                .iter()
                .find(|(p, _)| p.idx() == from)
                .ok_or_else(|| format!("phi v{} has no value from b{from}", phi.dst.0))?;
            incoming.push((phi.dst, get(&vals, *v)?));
        }
        for (dst, v) in incoming {
            vals[dst.idx()] = Some(v);
        }
        for ins in &b.instrs {
            *steps += 1;
            if *steps > STEP_LIMIT {
                return Err("the step limit was reached".into());
            }
            match ins {
                Instr::Param { dst, reg } => {
                    vals[dst.idx()] = Some(*args.get(*reg as usize).ok_or("a missing argument")?)
                }
                Instr::Pos { .. } => {}
                Instr::Copy { dst, src } => vals[dst.idx()] = Some(get(&vals, *src)?),
                Instr::Int { dst, idx } => {
                    let v = f
                        .int_at(*idx, |i| p.ints.get(i).copied())
                        .ok_or("an int outside the pool")?;
                    vals[dst.idx()] = Some(Val::I(v));
                }
                Instr::Float { dst, idx } => {
                    vals[dst.idx()] = Some(Val::F(
                        *p.floats.get(*idx).ok_or("a float outside the pool")?,
                    ))
                }
                Instr::Bool { dst, value } => vals[dst.idx()] = Some(Val::B(*value)),
                Instr::Null { dst } => vals[dst.idx()] = Some(Val::Void),
                Instr::BinOp { op, dst, a, b } => {
                    vals[dst.idx()] = Some(binop(*op, get(&vals, *a)?, get(&vals, *b)?)?)
                }
                Instr::UnOp { op, dst, src } => {
                    let v = get(&vals, *src)?;
                    vals[dst.idx()] = Some(match (op, v) {
                        (UnOp::Neg, Val::I(x)) => Val::I(x.wrapping_neg()),
                        (UnOp::Neg, Val::F(x)) => Val::F(-x),
                        (UnOp::Not, Val::B(x)) => Val::B(!x),
                        (UnOp::Incr, Val::I(x)) => Val::I(x.wrapping_add(1)),
                        (UnOp::Decr, Val::I(x)) => Val::I(x.wrapping_sub(1)),
                        (op, v) => return Err(format!("{op:?} of {v:?}")),
                    });
                }
                Instr::Cast { kind, dst, src } => {
                    let v = get(&vals, *src)?;
                    vals[dst.idx()] = Some(match (kind, v) {
                        (CastKind::ToSFloat, Val::I(x)) => Val::F(f64::from(x)),
                        (CastKind::ToInt, Val::F(x)) => Val::I(x as i32),
                        (k, v) => return Err(format!("{k:?} of {v:?}")),
                    });
                }
                Instr::Call { dst, fun, args: a } => {
                    let argv = a
                        .iter()
                        .map(|v| get(&vals, *v))
                        .collect::<Result<Vec<_>, _>>()?;
                    vals[dst.idx()] = Some(call(p, *fun, &argv, steps, depth + 1)?);
                }
                other => return Err(format!("the interpreter has no {other:?}")),
            }
        }
        *steps += 1;
        match &b.term {
            Terminator::Ret { value } => return get(&vals, *value),
            Terminator::Jump { target } => {
                prev = Some(block);
                block = target.idx();
            }
            Terminator::CondJump {
                cond,
                a,
                b: bb,
                if_true,
                if_false,
            } => {
                let a = get(&vals, *a)?;
                let bv = match bb {
                    Some(b) => Some(get(&vals, *b)?),
                    None => None,
                };
                let taken = compare(*cond, a, bv)?;
                prev = Some(block);
                block = if taken { if_true.idx() } else { if_false.idx() };
            }
            other => return Err(format!("the interpreter has no {other:?}")),
        }
    }
}

fn binop(op: BinOp, a: Val, b: Val) -> Result<Val, String> {
    Ok(match (a, b) {
        (Val::I(x), Val::I(y)) => Val::I(match op {
            BinOp::Add => x.wrapping_add(y),
            BinOp::Sub => x.wrapping_sub(y),
            BinOp::Mul => x.wrapping_mul(y),
            BinOp::SDiv | BinOp::SMod if y == 0 => return Err("division by zero".into()),
            BinOp::SDiv => x.wrapping_div(y),
            BinOp::SMod => x.wrapping_rem(y),
            BinOp::UDiv | BinOp::UMod if y == 0 => return Err("division by zero".into()),
            BinOp::UDiv => ((x as u32) / (y as u32)) as i32,
            BinOp::UMod => ((x as u32) % (y as u32)) as i32,
            BinOp::Shl => x.wrapping_shl(y as u32 & 31),
            BinOp::SShr => x.wrapping_shr(y as u32 & 31),
            BinOp::UShr => ((x as u32).wrapping_shr(y as u32 & 31)) as i32,
            BinOp::And => x & y,
            BinOp::Or => x | y,
            BinOp::Xor => x ^ y,
        }),
        (Val::F(x), Val::F(y)) => Val::F(match op {
            BinOp::Add => x + y,
            BinOp::Sub => x - y,
            BinOp::Mul => x * y,
            BinOp::SDiv => x / y,
            BinOp::SMod => x % y,
            other => return Err(format!("{other:?} on floats")),
        }),
        (a, b) => return Err(format!("{op:?} of {a:?} and {b:?}")),
    })
}

fn compare(cond: CondKind, a: Val, b: Option<Val>) -> Result<bool, String> {
    use CondKind::*;
    Ok(match (cond, a, b) {
        (True, Val::B(x), None) => x,
        (False, Val::B(x), None) => !x,
        (Eq, x, Some(y)) => x == y,
        (NotEq, x, Some(y)) => x != y,
        (SLt, Val::I(x), Some(Val::I(y))) => x < y,
        (SGte, Val::I(x), Some(Val::I(y))) => x >= y,
        (SGt, Val::I(x), Some(Val::I(y))) => x > y,
        (SLte, Val::I(x), Some(Val::I(y))) => x <= y,
        (ULt, Val::I(x), Some(Val::I(y))) => (x as u32) < (y as u32),
        (UGte, Val::I(x), Some(Val::I(y))) => (x as u32) >= (y as u32),
        (SLt, Val::F(x), Some(Val::F(y))) => x < y,
        (SGte, Val::F(x), Some(Val::F(y))) => x >= y,
        (SGt, Val::F(x), Some(Val::F(y))) => x > y,
        (SLte, Val::F(x), Some(Val::F(y))) => x <= y,
        (c, a, b) => return Err(format!("{c:?} of {a:?} and {b:?}")),
    })
}
