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
    /// A native array of values, by its place in the heap.
    Arr(usize),
    /// A reference to a cell, by its place in the heap.
    Ref(usize),
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
    /// The string constants; see [`Program::with_strings`].
    pub strings: Vec<String>,
    /// The `String` objects the module's globals hold, by global and text; see
    /// [`Heap::init_constants`].
    pub constants: Vec<(usize, usize)>,
    /// The native functions the interpreter runs itself, by function index; see
    /// [`Program::with_natives`].
    pub natives: HashMap<usize, String>,
    /// The fields of each structure type, by structure.
    pub virts: Vec<Vec<(String, usize)>>,
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
            strings: Vec::new(),
            constants: Vec::new(),
            natives: HashMap::new(),
            virts: table.virts.clone(),
        }
    }

    /// The natives the functions call. Those the interpreter runs: `alloc_array` (a native array
    /// of nulls), `array_blit`, `alloc_bytes`, `bytes_blit`, and `itos` and `ftos`, which write a number as
    /// UTF-16 and give its length through a reference.
    pub fn with_natives(mut self, natives: &[crate::lower::Native]) -> Program<'a> {
        self.natives = natives
            .iter()
            .map(|n| (n.findex as usize, n.name.clone()))
            .collect();
        self
    }

    /// The globals that hold string constants, which [`Heap::init_constants`] fills.
    pub fn with_constants(mut self, constants: &[crate::lower::Constant]) -> Program<'a> {
        self.constants = constants
            .iter()
            .map(|c| (c.global as usize, c.text))
            .collect();
        self
    }

    /// The string constants the functions index.
    pub fn with_strings(mut self, strings: &[String]) -> Program<'a> {
        self.strings = strings.to_vec();
        self
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
    let zero = |ty: usize| match table.types[ty] {
        HlType::I32 => Val::I(0),
        HlType::F64 => Val::F(0.0),
        HlType::Bool => Val::B(false),
        _ => Val::Void,
    };
    for (ix, t) in table.types.iter().enumerate() {
        match t {
            HlType::Obj(o) => {
                let mut fields = Vec::new();
                fill(table, *o, &mut fields);
                out.insert(ix, fields);
            }
            // a structure holds its fields in the order of its sorted names
            HlType::Virtual(v) => {
                out.insert(ix, table.virts[*v].iter().map(|(_, ty)| zero(*ty)).collect());
            }
            _ => {}
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
    arrays: Vec<Vec<Val>>,
    /// The registers whose address was taken: one slot each, for as long as the heap lives.
    cells: Vec<Val>,
    /// The fields set by name on dynamic objects, by object and string index.
    dynamic: HashMap<(usize, usize), Val>,
    globals: Vec<Val>,
}

impl Heap {
    /// A buffer holding `bytes`.
    pub fn bytes(&mut self, bytes: Vec<u8>) -> Val {
        self.memory.push(bytes);
        Val::Mem(self.memory.len() - 1)
    }

    /// A native array holding `items`.
    pub fn array(&mut self, items: Vec<Val>) -> Val {
        self.arrays.push(items);
        Val::Arr(self.arrays.len() - 1)
    }

    /// The elements of a native array.
    pub fn array_items(&self, array: Val) -> Option<&[Val]> {
        let Val::Arr(a) = array else { return None };
        self.arrays.get(a).map(Vec::as_slice)
    }

    /// Builds the `String` object of each string constant and puts it in its global, as the
    /// module does when it starts. `ty` is the `String` class, with its `bytes` and `length` at
    /// the given field indexes.
    pub fn init_constants(
        &mut self,
        p: &Program,
        ty: usize,
        bytes: usize,
        length: usize,
    ) -> Result<(), String> {
        for (global, text) in &p.constants {
            let text = p.strings.get(*text).ok_or("a constant outside the string pool")?;
            let buf = self.bytes(
                text.encode_utf16()
                    .chain(std::iter::once(0))
                    .flat_map(u16::to_le_bytes)
                    .collect(),
            );
            let units = Val::I(text.encode_utf16().count() as i32);
            let obj = self.object(p, ty, &[(bytes, buf), (length, units)])?;
            if self.globals.len() <= *global {
                self.globals.resize(global + 1, Val::Void);
            }
            self.globals[*global] = obj;
        }
        Ok(())
    }

    /// Field `at` of an object.
    pub fn field(&self, obj: Val, at: usize) -> Option<Val> {
        let Val::Obj(h) = obj else { return None };
        self.objects.get(h)?.1.get(at).copied()
    }

    /// The bytes of a buffer.
    pub fn memory(&self, buffer: Val) -> Option<&[u8]> {
        let Val::Mem(m) = buffer else { return None };
        self.memory.get(m).map(Vec::as_slice)
    }

    /// The text of a `String` object whose bytes and length sit in fields `bytes` and `length`.
    pub fn read_string(&self, v: Val, bytes: usize, length: usize) -> Option<String> {
        let Val::Obj(h) = v else { return None };
        let fields = &self.objects.get(h)?.1;
        let (Val::Mem(m), Val::I(n)) = (*fields.get(bytes)?, *fields.get(length)?) else {
            return None;
        };
        let raw = self.memory.get(m)?.get(..usize::try_from(n).ok()? * 2)?;
        let units: Vec<u16> = raw
            .as_chunks::<2>()
            .0
            .iter()
            .map(|c| u16::from_le_bytes(*c))
            .collect();
        String::from_utf16(&units).ok()
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
    if let Some(name) = p.natives.get(&findex) {
        return match (name.as_str(), args) {
            ("alloc_array", [_, Val::I(n)]) => {
                let n = usize::try_from(*n).map_err(|_| "a negative array size")?;
                Ok(heap.array(vec![Val::Void; n]))
            }
            ("alloc_bytes", [Val::I(n)]) => {
                let n = usize::try_from(*n).map_err(|_| "a negative size")?;
                Ok(heap.bytes(vec![0; n]))
            }
            ("bytes_blit", [Val::Mem(dst), Val::I(dpos), Val::Mem(src), Val::I(spos), Val::I(n)]) => {
                let (dpos, spos, n) = (*dpos as usize, *spos as usize, *n as usize);
                let from = heap.memory[*src]
                    .get(spos..spos + n)
                    .ok_or("a blit from past the end of the bytes")?
                    .to_vec();
                heap.memory[*dst]
                    .get_mut(dpos..dpos + n)
                    .ok_or("a blit to past the end of the bytes")?
                    .copy_from_slice(&from);
                Ok(Val::Void)
            }
            ("array_blit", [Val::Arr(dst), Val::I(dpos), Val::Arr(src), Val::I(spos), Val::I(n)]) => {
                let (dpos, spos, n) = (*dpos as usize, *spos as usize, *n as usize);
                let from = heap.arrays[*src]
                    .get(spos..spos + n)
                    .ok_or("a blit from past the end of the array")?
                    .to_vec();
                heap.arrays[*dst]
                    .get_mut(dpos..dpos + n)
                    .ok_or("a blit to past the end of the array")?
                    .copy_from_slice(&from);
                Ok(Val::Void)
            }
            ("itos" | "ftos", [number, Val::Ref(at)]) => {
                let text = match number {
                    Val::I(x) => x.to_string(),
                    Val::F(x) if x.is_nan() => "NaN".to_owned(),
                    Val::F(x) if x.is_infinite() => {
                        if *x > 0.0 { "Infinity" } else { "-Infinity" }.to_owned()
                    }
                    Val::F(x) => x.to_string(),
                    other => return Err(format!("{name} of {other:?}")),
                };
                let units: Vec<u16> = text.encode_utf16().collect();
                heap.cells[*at] = Val::I(units.len() as i32);
                Ok(heap.bytes(
                    units
                        .iter()
                        .chain(std::iter::once(&0))
                        .flat_map(|u| u.to_le_bytes())
                        .collect(),
                ))
            }
            _ => Err(format!("the interpreter has no native {name}")),
        };
    }
    let f = p
        .functions
        .get(&findex)
        .ok_or_else(|| format!("no function {findex}"))?;
    let mut vals: Vec<Option<Val>> = vec![None; f.values.len()];
    // each cell's place in the heap, made when it is first used
    let mut cells: Vec<Option<usize>> = vec![None; f.cells.len()];
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
                Instr::SetGlobal { global, src } => {
                    let v = get(&vals, *src)?;
                    if heap.globals.len() <= *global {
                        heap.globals.resize(global + 1, Val::Void);
                    }
                    heap.globals[*global] = v;
                }
                Instr::GetGlobal { dst, global } => {
                    vals[dst.idx()] = Some(heap.globals.get(*global).copied().unwrap_or(Val::Void));
                }
                Instr::String { dst, idx } => {
                    let text = p.strings.get(*idx).ok_or("a string outside the pool")?;
                    // UTF-16, with the terminator the runtime keeps
                    let bytes = text
                        .encode_utf16()
                        .chain(std::iter::once(0))
                        .flat_map(u16::to_le_bytes)
                        .collect();
                    vals[dst.idx()] = Some(heap.bytes(bytes));
                }
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
                Instr::Cast {
                    kind: CastKind::ToVirtual,
                    dst,
                    src,
                } => {
                    // a structure of the fields it names, each found by name in the object
                    let ty = f.values[dst.idx()].ty.0 as usize;
                    let Some(HlType::Virtual(v)) = p.types.get(ty) else {
                        return Err("a conversion to something that is not a structure".into());
                    };
                    let Val::Obj(h) = get(&vals, *src)? else {
                        return Err("a conversion of something that is not an object".into());
                    };
                    let mut fields = p.layouts.get(&ty).cloned().unwrap_or_default();
                    for (at, (name, _)) in p.virts[*v].iter().enumerate() {
                        let found = p
                            .strings
                            .iter()
                            .position(|s| s == name)
                            .and_then(|s| heap.dynamic.get(&(h, s)));
                        if let Some(x) = found {
                            fields[at] = *x;
                        }
                    }
                    heap.objects.push((ty, fields));
                    vals[dst.idx()] = Some(Val::Obj(heap.objects.len() - 1));
                }
                Instr::Cast { kind, dst, src } => {
                    let v = get(&vals, *src)?;
                    vals[dst.idx()] = Some(match (kind, v) {
                        (CastKind::ToSFloat, Val::I(x)) => Val::F(f64::from(x)),
                        (CastKind::ToInt, Val::F(x)) => Val::I(x as i32),
                        (CastKind::UnsafeCast | CastKind::SafeCast | CastKind::ToDyn, v) => v,
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
                    // a dynamic object has no fields until some are set by name
                    let fields = if p.types.get(ty) == Some(&HlType::DynObj) {
                        Vec::new()
                    } else {
                        p.layouts
                            .get(&ty)
                            .ok_or_else(|| format!("no layout for type {ty}"))?
                            .clone()
                    };
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
                    kind: kind @ (MemAccess::I8 | MemAccess::I16),
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
                    let v = if *kind == MemAccess::I8 {
                        i32::from(*bytes.get(at).ok_or("a read past the end of the bytes")?)
                    } else {
                        let pair = bytes
                            .get(at..at + 2)
                            .ok_or("a read past the end of the bytes")?;
                        i32::from(u16::from_le_bytes([pair[0], pair[1]]))
                    };
                    vals[dst.idx()] = Some(Val::I(v));
                }
                Instr::MemSet {
                    kind: kind @ (MemAccess::I8 | MemAccess::I16),
                    base,
                    index,
                    src,
                } => {
                    let (Val::Mem(m), Val::I(at), Val::I(x)) =
                        (get(&vals, *base)?, get(&vals, *index)?, get(&vals, *src)?)
                    else {
                        return Err("a write to something that is not bytes".into());
                    };
                    let at = usize::try_from(at).map_err(|_| "a negative byte offset")?;
                    let width = if *kind == MemAccess::I8 { 1 } else { 2 };
                    heap.memory[m]
                        .get_mut(at..at + width)
                        .ok_or("a write past the end of the bytes")?
                        .copy_from_slice(&x.to_le_bytes()[..width]);
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
                Instr::MemSet {
                    kind: MemAccess::Mem,
                    base,
                    index,
                    src,
                } => {
                    let (Val::Mem(m), Val::I(at)) = (get(&vals, *base)?, get(&vals, *index)?)
                    else {
                        return Err("a write to something that is not bytes".into());
                    };
                    let at = usize::try_from(at).map_err(|_| "a negative byte offset")?;
                    let value = match get(&vals, *src)? {
                        Val::I(x) => x.to_le_bytes().to_vec(),
                        Val::F(x) => x.to_le_bytes().to_vec(),
                        other => return Err(format!("a memory write of {other:?}")),
                    };
                    heap.memory[m]
                        .get_mut(at..at + value.len())
                        .ok_or("a write past the end of the bytes")?
                        .copy_from_slice(&value);
                }
                Instr::MemGet {
                    kind: MemAccess::Array,
                    dst,
                    base,
                    index,
                } => {
                    let (Val::Arr(a), Val::I(at)) = (get(&vals, *base)?, get(&vals, *index)?)
                    else {
                        return Err("a read of something that is not an array".into());
                    };
                    vals[dst.idx()] = Some(
                        *usize::try_from(at)
                            .ok()
                            .and_then(|at| heap.arrays[a].get(at))
                            .ok_or("an array read out of range")?,
                    );
                }
                Instr::MemSet {
                    kind: MemAccess::Array,
                    base,
                    index,
                    src,
                } => {
                    let (Val::Arr(a), Val::I(at)) = (get(&vals, *base)?, get(&vals, *index)?)
                    else {
                        return Err("a write to something that is not an array".into());
                    };
                    let v = get(&vals, *src)?;
                    *usize::try_from(at)
                        .ok()
                        .and_then(|at| heap.arrays[a].get_mut(at))
                        .ok_or("an array write out of range")? = v;
                }
                Instr::ArraySize { dst, array } => {
                    let Val::Arr(a) = get(&vals, *array)? else {
                        return Err("the size of something that is not an array".into());
                    };
                    vals[dst.idx()] = Some(Val::I(heap.arrays[a].len() as i32));
                }
                Instr::TypeConst { dst, ty } => vals[dst.idx()] = Some(Val::I(ty.0 as i32)),
                Instr::DynSet { obj, field, src } => {
                    let (Val::Obj(h), v) = (get(&vals, *obj)?, get(&vals, *src)?) else {
                        return Err("a field set on something that is not an object".into());
                    };
                    heap.dynamic.insert((h, *field), v);
                }
                Instr::DynGet { dst, obj, field } => {
                    let Val::Obj(h) = get(&vals, *obj)? else {
                        return Err("a field read on something that is not an object".into());
                    };
                    vals[dst.idx()] = Some(heap.dynamic.get(&(h, *field)).copied().unwrap_or(Val::Void));
                }
                Instr::CellRef { dst, cell } => {
                    let at = *cells[cell.idx()].get_or_insert_with(|| {
                        heap.cells.push(Val::Void);
                        heap.cells.len() - 1
                    });
                    vals[dst.idx()] = Some(Val::Ref(at));
                }
                Instr::CellSet { cell, src } => {
                    let v = get(&vals, *src)?;
                    let at = *cells[cell.idx()].get_or_insert_with(|| {
                        heap.cells.push(Val::Void);
                        heap.cells.len() - 1
                    });
                    heap.cells[at] = v;
                }
                Instr::CellGet { dst, cell } => {
                    let at = cells[cell.idx()].ok_or("a cell is read before it is written")?;
                    vals[dst.idx()] = Some(heap.cells[at]);
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
            Terminator::Throw { .. } => return Err("an exception was thrown".into()),
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
