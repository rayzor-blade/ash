//! A reference interpreter for the AIR that [`crate::lower`] produces.
//!
//! It runs integers, floats and bools, branches, phis, direct calls, objects with their
//! fields and method tables, and reads of byte buffers, and nothing else. Its job
//! is to say what a lowered function computes, so a test can compare that with what the Haxe
//! source means without going through an engine.

use std::collections::HashMap;

use air::v2::ir::*;

use crate::hl::{HlType, ObjIx, TypeTable};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Val {
    I(i32),
    F(f64),
    B(bool),
    /// An object, by its place in the heap.
    Obj(usize),
    /// A buffer of bytes, by its place in the heap.
    Mem(usize),
    /// What a void call returns, and null.
    Void,
}

/// The functions that can be called, and the constant pools they index.
pub struct Program<'a> {
    pub functions: HashMap<usize, &'a Function>,
    pub ints: &'a [i32],
    pub floats: &'a [f64],
    /// The fields a new object of each type starts with, by type index: see [`layouts`].
    pub layouts: HashMap<usize, Vec<Val>>,
    /// The function each virtual slot of each object type runs, by type index.
    pub vtables: HashMap<usize, Vec<u32>>,
    /// The type table's types, so an access can ask what it reads.
    pub types: Vec<HlType>,
}

impl<'a> Program<'a> {
    pub fn new(
        functions: HashMap<usize, &'a Function>,
        ints: &'a [i32],
        floats: &'a [f64],
        table: &TypeTable,
    ) -> Program<'a> {
        let vtables = table
            .types
            .iter()
            .enumerate()
            .filter_map(|(ix, t)| match t {
                HlType::Obj(o) => Some((ix, table.objs[*o].virtuals.clone())),
                _ => None,
            })
            .collect();
        Program {
            functions,
            ints,
            floats,
            layouts: layouts(table),
            vtables,
            types: table.types.clone(),
        }
    }
}

/// The fields a new object of each object type holds, the superclass's first, as the runtime
/// zeroes them.
pub fn layouts(table: &TypeTable) -> HashMap<usize, Vec<Val>> {
    fn fill(table: &TypeTable, o: ObjIx, out: &mut Vec<Val>) {
        let obj = &table.objs[o];
        if let Some(sup) = obj.super_
            && let HlType::Obj(s) | HlType::Struct(s) = table.types[sup]
        {
            fill(table, s, out);
        }
        for (_, ty) in &obj.fields {
            out.push(match table.types[*ty] {
                HlType::I32 => Val::I(0),
                HlType::F64 => Val::F(0.0),
                HlType::Bool => Val::B(false),
                _ => Val::Void,
            });
        }
    }
    let mut out = HashMap::new();
    for (ix, t) in table.types.iter().enumerate() {
        if let HlType::Obj(o) = t {
            let mut fields = Vec::new();
            fill(table, *o, &mut fields);
            out.insert(ix, fields);
        }
    }
    out
}

/// What has been made so far: objects, each with the type it was made as and its fields, and
/// byte buffers.
#[derive(Default)]
pub struct Heap {
    objects: Vec<(usize, Vec<Val>)>,
    memory: Vec<Vec<u8>>,
}

impl Heap {
    /// A buffer holding `bytes`.
    pub fn bytes(&mut self, bytes: Vec<u8>) -> Val {
        self.memory.push(bytes);
        Val::Mem(self.memory.len() - 1)
    }

    /// An object of type `ty` with the fields the runtime would zero, and `set` written over them.
    pub fn object(&mut self, p: &Program, ty: usize, set: &[(usize, Val)]) -> Result<Val, String> {
        let mut fields = p
            .layouts
            .get(&ty)
            .ok_or_else(|| format!("no layout for type {ty}"))?
            .clone();
        for (at, v) in set {
            *fields.get_mut(*at).ok_or("a field outside the object")? = *v;
        }
        self.objects.push((ty, fields));
        Ok(Val::Obj(self.objects.len() - 1))
    }
}

const STEP_LIMIT: usize = 50_000_000;

pub fn run(p: &Program, findex: usize, args: &[Val]) -> Result<Val, String> {
    run_in(p, findex, args, &mut Heap::default())
}

/// [`run`] over a heap the caller has put objects in, which the arguments may name.
pub fn run_in(p: &Program, findex: usize, args: &[Val], heap: &mut Heap) -> Result<Val, String> {
    let mut steps = 0;
    call(p, findex, args, &mut steps, heap, 0)
}

fn call(
    p: &Program,
    findex: usize,
    args: &[Val],
    steps: &mut usize,
    heap: &mut Heap,
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
                        (CastKind::UnsafeCast, v) => v,
                        (k, v) => return Err(format!("{k:?} of {v:?}")),
                    });
                }
                Instr::Call { dst, fun, args: a } => {
                    let argv = a
                        .iter()
                        .map(|v| get(&vals, *v))
                        .collect::<Result<Vec<_>, _>>()?;
                    vals[dst.idx()] = Some(call(p, *fun, &argv, steps, heap, depth + 1)?);
                }
                Instr::CallMethod {
                    dst,
                    field,
                    args: a,
                } => {
                    let argv = a
                        .iter()
                        .map(|v| get(&vals, *v))
                        .collect::<Result<Vec<_>, _>>()?;
                    let Some(Val::Obj(h)) = argv.first().copied() else {
                        return Err("a method call on null".into());
                    };
                    let ty = heap.objects[h].0;
                    let target = *p
                        .vtables
                        .get(&ty)
                        .and_then(|v| v.get(*field))
                        .ok_or_else(|| format!("type {ty} has no virtual slot {field}"))?;
                    vals[dst.idx()] =
                        Some(call(p, target as usize, &argv, steps, heap, depth + 1)?);
                }
                Instr::New { dst } => {
                    let ty = f.values[dst.idx()].ty.0 as usize;
                    let fields = p
                        .layouts
                        .get(&ty)
                        .ok_or_else(|| format!("no layout for type {ty}"))?
                        .clone();
                    heap.objects.push((ty, fields));
                    vals[dst.idx()] = Some(Val::Obj(heap.objects.len() - 1));
                }
                Instr::FieldGet {
                    dst, obj, field, ..
                } => {
                    let Val::Obj(h) = get(&vals, *obj)? else {
                        return Err("a field of null".into());
                    };
                    let v = *heap.objects[h]
                        .1
                        .get(*field)
                        .ok_or("a field outside the object")?;
                    vals[dst.idx()] = Some(v);
                }
                Instr::FieldSet {
                    obj, field, src, ..
                } => {
                    let v = get(&vals, *src)?;
                    let Val::Obj(h) = get(&vals, *obj)? else {
                        return Err("a field of null".into());
                    };
                    *heap.objects[h]
                        .1
                        .get_mut(*field)
                        .ok_or("a field outside the object")? = v;
                }
                Instr::NullCheck { value } => {
                    if get(&vals, *value)? == Val::Void {
                        return Err("a null reference".into());
                    }
                }
                Instr::MemGet {
                    kind: MemAccess::Mem,
                    dst,
                    base,
                    index,
                } => {
                    let (Val::Mem(m), Val::I(at)) = (get(&vals, *base)?, get(&vals, *index)?)
                    else {
                        return Err("a read of something that is not bytes".into());
                    };
                    let at = usize::try_from(at).map_err(|_| "a negative byte offset")?;
                    let bytes = &heap.memory[m];
                    let ty = f.values[dst.idx()].ty.0 as usize;
                    vals[dst.idx()] = Some(match p.types.get(ty) {
                        Some(HlType::I32) => Val::I(i32::from_le_bytes(
                            bytes
                                .get(at..at + 4)
                                .ok_or("a read past the end of the bytes")?
                                .try_into()
                                .unwrap(),
                        )),
                        Some(HlType::F64) => Val::F(f64::from_le_bytes(
                            bytes
                                .get(at..at + 8)
                                .ok_or("a read past the end of the bytes")?
                                .try_into()
                                .unwrap(),
                        )),
                        other => return Err(format!("a memory read of {other:?}")),
                    });
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
        (Null, x, None) => x == Val::Void,
        (NotNull, x, None) => x != Val::Void,
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
