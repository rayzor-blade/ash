//! Direct emission of WebAssembly from optimized AIR, without LLVM.
//!
//! The crate depends only on `air`, so it builds for wasm and can run inside a
//! browser. What a program is made of (types, constants) is asked of a
//! [`Program`]; what comes out is a module whose functions are named
//! `f<findex>`.
//!
//! Control flow is structured from the dominator tree (Ramsey, "Beyond
//! Relooper"): a `loop` per loop header, a `block` per merge node, an `if` per
//! conditional branch. Phi copies happen on the edge that carries them.
//!
//! This covers the numeric subset: functions whose values are booleans, 32- and
//! 64-bit integers and floats, and whose instructions are arithmetic,
//! comparisons, casts between those, direct calls and branches. A function
//! outside it is reported with the reason, and so is any function that calls
//! one. Objects, strings, exceptions and the runtime are not handled yet.
//!
//! A fused multiply-add has no wasm instruction, so the module imports it:
//! `ash.fma_f64` and `ash.fma_f32`, each `(a, b, c) -> a * b + c` rounded once.
//! The host supplies them, as the runtime does for LLVM-built modules.
//!
//! The module also imports `ash.poll`, called when the word at address 0 of
//! its memory is nonzero. [`Options::polls`] makes every loop header test that
//! word; [`Options::shadow`] keeps the shadow call stack stack traces read
//! (`(file << 32) | line` stored into a frame per function).
#![forbid(unsafe_code)]

use air::v2::CfgInfo;
use air::v2::ir::{
    BinOp, CastKind, CondKind, Function, Instr, Terminator, TypeRef, UnOp, ValueId,
};
use std::collections::HashMap;
use wasm_encoder as we;
use we::{BlockType, Instruction as I, ValType};

/// The kinds of value the emitter can hold. `Other` is anything it cannot.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ty {
    Void,
    Bool,
    I32,
    I64,
    F32,
    F64,
    Other,
}

impl Ty {
    fn wasm(self) -> Option<ValType> {
        match self {
            Ty::Bool | Ty::I32 => Some(ValType::I32),
            Ty::I64 => Some(ValType::I64),
            Ty::F32 => Some(ValType::F32),
            Ty::F64 => Some(ValType::F64),
            Ty::Void | Ty::Other => None,
        }
    }

    fn is_int(self) -> bool {
        matches!(self, Ty::Bool | Ty::I32 | Ty::I64)
    }

    fn is_float(self) -> bool {
        matches!(self, Ty::F32 | Ty::F64)
    }
}

/// What the emitter needs to know about the program a function belongs to.
pub trait Program {
    fn ty(&self, t: TypeRef) -> Ty;
    /// An entry of the integer constant pool.
    fn int(&self, idx: usize) -> Option<i32>;
    /// An entry of the float constant pool.
    fn float(&self, idx: usize) -> Option<f64>;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Sig {
    pub params: Vec<Ty>,
    pub ret: Ty,
}

/// Why a function cannot be emitted.
#[derive(Clone, Debug)]
pub struct Unsupported(pub String);

type Res<T> = Result<T, Unsupported>;

fn unsupported<T>(why: impl Into<String>) -> Res<T> {
    Err(Unsupported(why.into()))
}

/// Imports every module declares, ahead of its functions: `ash.fma_f64`,
/// `ash.fma_f32` and `ash.poll`.
const IMPORTS: u32 = 3;
const FMA64: u32 = 0;
const FMA32: u32 = 1;
const POLL: u32 = 2;
/// The module's own helpers, after the imports: open a shadow frame, close it,
/// and test the poll word.
const SHADOW_PUSH: u32 = 3;
const SHADOW_POP: u32 = 4;
const POLL_CHECK: u32 = 5;
/// Where the functions of the program start.
const FIRST: u32 = IMPORTS + 3;
/// Memory layout: the poll word, a scratch frame for pushes past capacity, then
/// the frames.
const SCRATCH_FRAME: i32 = 64;
const FRAMES: i32 = 128;
const FRAME_CAP: i32 = 1 << 15;
const FRAME_BYTES: i32 = 16;
const MEMORY_PAGES: u64 = 9;

/// What the emitted code carries besides the program's own work.
#[derive(Clone, Copy, Debug, Default)]
pub struct Options {
    /// Test the poll word at the head of every loop.
    pub polls: bool,
    /// Open a shadow frame per function and store source positions into it.
    pub shadow: bool,
}

pub struct Input<'a> {
    pub findex: usize,
    pub sig: Sig,
    pub body: &'a Function,
}

pub struct Emitted {
    pub bytes: Vec<u8>,
    /// The findex of each function in the module, in order.
    pub functions: Vec<usize>,
    pub skipped: Vec<(usize, String)>,
}

/// Emit every input that can be emitted, and report the rest.
pub fn emit_module<P: Program>(program: &P, inputs: &[Input]) -> Emitted {
    emit_module_with(program, inputs, Options::default())
}

/// [`emit_module`] with polls and shadow frames as asked.
pub fn emit_module_with<P: Program>(program: &P, inputs: &[Input], opts: Options) -> Emitted {
    // First pass: every input is assumed callable, to learn which fail on
    // their own and whom they call.
    let provisional: HashMap<usize, Callee> = inputs
        .iter()
        .enumerate()
        .map(|(i, inp)| {
            (
                inp.findex,
                Callee {
                    index: FIRST + i as u32,
                    sig: inp.sig.clone(),
                },
            )
        })
        .collect();
    let mut failure: Vec<Option<String>> = vec![None; inputs.len()];
    let mut callers: HashMap<usize, Vec<usize>> = HashMap::new();
    for (i, inp) in inputs.iter().enumerate() {
        match emit_function(program, inp, &provisional, opts) {
            Ok((_, called)) => {
                for callee in called {
                    callers.entry(callee).or_default().push(i);
                }
            }
            Err(e) => failure[i] = Some(e.0),
        }
    }
    // A function that calls one that failed fails with it.
    let position: HashMap<usize, usize> = inputs
        .iter()
        .enumerate()
        .map(|(i, inp)| (inp.findex, i))
        .collect();
    let mut work: Vec<usize> = (0..inputs.len()).filter(|i| failure[*i].is_some()).collect();
    while let Some(bad) = work.pop() {
        let findex = inputs[bad].findex;
        for &caller in callers.get(&findex).into_iter().flatten() {
            if failure[caller].is_none() {
                failure[caller] = Some(format!("calls f{findex}, which is not emitted"));
                work.push(caller);
            }
        }
    }
    let _ = position;

    // Second pass: the live functions, with their final indices.
    let live: Vec<usize> = (0..inputs.len())
        .filter(|i| failure[*i].is_none())
        .collect();
    let finals: HashMap<usize, Callee> = live
        .iter()
        .enumerate()
        .map(|(n, &i)| {
            (
                inputs[i].findex,
                Callee {
                    index: FIRST + n as u32,
                    sig: inputs[i].sig.clone(),
                },
            )
        })
        .collect();

    let mut types = TypeTable::default();
    let fma64 = types.get(vec![ValType::F64; 3], vec![ValType::F64]);
    let fma32 = types.get(vec![ValType::F32; 3], vec![ValType::F32]);
    let void = types.get(vec![], vec![]);
    let push = types.get(vec![ValType::I32], vec![ValType::I32]);
    let mut funcs = we::FunctionSection::new();
    let mut code = we::CodeSection::new();
    prelude(&mut funcs, &mut code, push, void);
    let mut exports = we::ExportSection::new();
    let mut functions = Vec::new();
    let mut skipped: Vec<(usize, String)> = Vec::new();
    for (n, &i) in live.iter().enumerate() {
        let inp = &inputs[i];
        match emit_function(program, inp, &finals, opts) {
            Ok((body, _)) => {
                let params: Vec<ValType> = inp.sig.params.iter().filter_map(|t| t.wasm()).collect();
                let results: Vec<ValType> = inp.sig.ret.wasm().into_iter().collect();
                funcs.function(types.get(params, results));
                code.function(&body);
                exports.export(
                    &format!("f{}", inp.findex),
                    we::ExportKind::Func,
                    FIRST + n as u32,
                );
                functions.push(inp.findex);
            }
            Err(e) => skipped.push((inp.findex, e.0)),
        }
    }
    for (i, why) in failure.into_iter().enumerate() {
        if let Some(why) = why {
            skipped.push((inputs[i].findex, why));
        }
    }

    let mut module = we::Module::new();
    let mut type_section = we::TypeSection::new();
    for (params, results) in &types.list {
        type_section
            .ty()
            .function(params.iter().copied(), results.iter().copied());
    }
    module.section(&type_section);
    let mut imports = we::ImportSection::new();
    imports.import("ash", "fma_f64", we::EntityType::Function(fma64));
    imports.import("ash", "fma_f32", we::EntityType::Function(fma32));
    imports.import("ash", "poll", we::EntityType::Function(void));
    module.section(&imports);
    module.section(&funcs);
    let mut memories = we::MemorySection::new();
    memories.memory(we::MemoryType {
        minimum: MEMORY_PAGES,
        maximum: None,
        memory64: false,
        shared: false,
        page_size_log2: None,
    });
    module.section(&memories);
    let mut globals = we::GlobalSection::new();
    globals.global(
        we::GlobalType {
            val_type: ValType::I32,
            mutable: true,
            shared: false,
        },
        &we::ConstExpr::i32_const(0),
    );
    module.section(&globals);
    exports.export("memory", we::ExportKind::Memory, 0);
    module.section(&exports);
    module.section(&code);
    Emitted {
        bytes: module.finish(),
        functions,
        skipped,
    }
}

/// The module's own helpers: `shadow_push`, `shadow_pop` and `poll_check`.
fn prelude(funcs: &mut we::FunctionSection, code: &mut we::CodeSection, push: u32, void: u32) {
    // shadow_push(findex) -> slot: frame `depth` lives at FRAMES + 16 * depth,
    // or in the scratch frame past capacity; the depth counts either way so
    // the pops balance. The position word sits 8 bytes into the frame.
    funcs.function(push);
    let mut f = we::Function::new([(2, ValType::I32)]);
    f.instruction(&I::GlobalGet(0));
    f.instruction(&I::LocalSet(1));
    f.instruction(&I::GlobalGet(0));
    f.instruction(&I::I32Const(1));
    f.instruction(&I::I32Add);
    f.instruction(&I::GlobalSet(0));
    f.instruction(&I::LocalGet(1));
    f.instruction(&I::I32Const(FRAME_CAP));
    f.instruction(&I::I32LtU);
    f.instruction(&I::If(BlockType::Result(ValType::I32)));
    f.instruction(&I::LocalGet(1));
    f.instruction(&I::I32Const(FRAME_BYTES));
    f.instruction(&I::I32Mul);
    f.instruction(&I::I32Const(FRAMES));
    f.instruction(&I::I32Add);
    f.instruction(&I::Else);
    f.instruction(&I::I32Const(SCRATCH_FRAME));
    f.instruction(&I::End);
    f.instruction(&I::LocalTee(2));
    f.instruction(&I::LocalGet(0));
    f.instruction(&I::I32Store(we::MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    f.instruction(&I::LocalGet(2));
    f.instruction(&I::I32Const(8));
    f.instruction(&I::I32Add);
    f.instruction(&I::End);
    code.function(&f);

    // shadow_pop()
    funcs.function(void);
    let mut f = we::Function::new([]);
    f.instruction(&I::GlobalGet(0));
    f.instruction(&I::I32Const(1));
    f.instruction(&I::I32Sub);
    f.instruction(&I::GlobalSet(0));
    f.instruction(&I::End);
    code.function(&f);

    // poll_check(): the host asks for a safepoint by writing the word at 0.
    funcs.function(void);
    let mut f = we::Function::new([]);
    f.instruction(&I::I32Const(0));
    f.instruction(&I::I32Load(we::MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    f.instruction(&I::If(BlockType::Empty));
    f.instruction(&I::Call(POLL));
    f.instruction(&I::End);
    f.instruction(&I::End);
    code.function(&f);
}

#[derive(Default)]
struct TypeTable {
    index: HashMap<(Vec<ValType>, Vec<ValType>), u32>,
    list: Vec<(Vec<ValType>, Vec<ValType>)>,
}

impl TypeTable {
    fn get(&mut self, params: Vec<ValType>, results: Vec<ValType>) -> u32 {
        let key = (params, results);
        if let Some(&t) = self.index.get(&key) {
            return t;
        }
        let t = self.list.len() as u32;
        self.list.push(key.clone());
        self.index.insert(key, t);
        t
    }
}

struct Callee {
    index: u32,
    sig: Sig,
}

#[derive(Clone, Copy, PartialEq)]
enum Frame {
    Loop(usize),
    Block(usize),
    If,
}

struct Fx<'a, P: Program> {
    program: &'a P,
    f: &'a Function,
    sig: &'a Sig,
    callees: &'a HashMap<usize, Callee>,
    opts: Options,
    frame_local: u32,
    called: Vec<usize>,
    out: we::Function,
    ctx: Vec<Frame>,
    nparams: u32,
    tys: Vec<Ty>,
    rpo_num: Vec<usize>,
    loop_header: Vec<bool>,
    merge: Vec<bool>,
    kids: Vec<Vec<usize>>,
}

fn emit_function<P: Program>(
    program: &P,
    inp: &Input,
    callees: &HashMap<usize, Callee>,
    opts: Options,
) -> Res<(we::Function, Vec<usize>)> {
    let f = inp.body;
    if inp.sig.params.iter().any(|t| t.wasm().is_none())
        || !(inp.sig.ret == Ty::Void || inp.sig.ret.wasm().is_some())
    {
        return unsupported("signature has a type outside the numeric subset");
    }
    let tys: Vec<Ty> = f.values.iter().map(|v| program.ty(v.ty)).collect();
    if let Some(t) = tys.iter().find(|t| matches!(t, Ty::Other)) {
        return unsupported(format!("a value of type {t:?}"));
    }
    let cfg = CfgInfo::build(f);
    let n = f.blocks.len();
    let rpo = cfg.rpo();
    let mut rpo_num = vec![usize::MAX; n];
    for (i, b) in rpo.iter().enumerate() {
        rpo_num[b.idx()] = i;
    }
    let mut fwd_in = vec![0usize; n];
    let mut forced = vec![false; n];
    let mut loop_header = vec![false; n];
    for b in &rpo {
        let from = b.idx();
        let blk = &f.blocks[from];
        if blk.handler.is_some() {
            return unsupported("a trap region");
        }
        let mut seen: Vec<usize> = Vec::new();
        for s in blk.term.successors() {
            let to = s.idx();
            if seen.contains(&to) {
                continue;
            }
            seen.push(to);
            if rpo_num[to] <= rpo_num[from] {
                loop_header[to] = true;
            } else {
                fwd_in[to] += 1;
            }
        }
        if let Terminator::Switch {
            targets, default, ..
        } = &blk.term
        {
            for t in targets.iter().chain(std::iter::once(default)) {
                forced[t.idx()] = true;
            }
        }
    }
    let mut merge = vec![false; n];
    for b in &rpo {
        let i = b.idx();
        merge[i] = i != rpo[0].idx() && (fwd_in[i] >= 2 || forced[i]);
    }
    let mut kids: Vec<Vec<usize>> = vec![Vec::new(); n];
    for b in &rpo {
        let i = b.idx();
        for &c in &cfg.dom.dom_children[i] {
            if c != i && merge[c] {
                kids[i].push(c);
            }
        }
        kids[i].sort_by_key(|k| rpo_num[*k]);
    }
    let mut locals: Vec<(u32, ValType)> = tys
        .iter()
        .map(|t| (1, t.wasm().unwrap_or(ValType::I32)))
        .collect();
    let frame_local = inp.sig.params.len() as u32 + locals.len() as u32;
    locals.push((1, ValType::I32));
    let mut fx = Fx {
        program,
        f,
        sig: &inp.sig,
        callees,
        opts,
        frame_local,
        called: Vec::new(),
        out: we::Function::new(locals),
        ctx: Vec::new(),
        nparams: inp.sig.params.len() as u32,
        tys,
        rpo_num,
        loop_header,
        merge,
        kids,
    };
    if opts.shadow {
        let findex = inp.findex as i32;
        fx.emit(&I::I32Const(findex));
        fx.emit(&I::Call(SHADOW_PUSH));
        let frame = fx.frame_local;
        fx.emit(&I::LocalSet(frame));
    }
    if !rpo.is_empty() {
        fx.do_tree(rpo[0].idx())?;
    }
    // Control never falls off the end; this satisfies the function's result.
    fx.emit(&I::Unreachable);
    fx.emit(&I::End);
    Ok((fx.out, fx.called))
}

impl<P: Program> Fx<'_, P> {
    fn emit(&mut self, i: &I) {
        self.out.instruction(i);
    }

    fn get(&mut self, v: ValueId) {
        self.emit(&I::LocalGet(self.nparams + v.0));
    }

    fn set(&mut self, v: ValueId) {
        self.emit(&I::LocalSet(self.nparams + v.0));
    }

    fn ty(&self, v: ValueId) -> Ty {
        self.tys[v.0 as usize]
    }

    fn label(&self, want: Frame) -> u32 {
        self.ctx
            .iter()
            .rev()
            .position(|f| *f == want)
            .expect("a branch target has no enclosing label") as u32
    }

    fn do_tree(&mut self, x: usize) -> Res<()> {
        let ys = self.kids[x].clone();
        if self.loop_header[x] {
            self.emit(&I::Loop(BlockType::Empty));
            self.ctx.push(Frame::Loop(x));
            if self.opts.polls {
                self.emit(&I::Call(POLL_CHECK));
            }
            self.node_within(x, &ys)?;
            self.ctx.pop();
            self.emit(&I::End);
        } else {
            self.node_within(x, &ys)?;
        }
        Ok(())
    }

    fn node_within(&mut self, x: usize, ys: &[usize]) -> Res<()> {
        match ys.split_last() {
            None => {
                self.code(x)?;
                self.terminator(x)
            }
            Some((&y, rest)) => {
                self.emit(&I::Block(BlockType::Empty));
                self.ctx.push(Frame::Block(y));
                self.node_within(x, rest)?;
                self.ctx.pop();
                self.emit(&I::End);
                self.do_tree(y)
            }
        }
    }

    /// The pairs `(phi, incoming)` that the edge `from -> to` copies.
    fn phi_pairs(&self, from: usize, to: usize) -> Vec<(ValueId, ValueId)> {
        self.f.blocks[to]
            .phis
            .iter()
            .filter_map(|p| {
                p.incoming
                    .iter()
                    .find(|(b, _)| b.idx() == from)
                    .map(|(_, v)| (p.dst, *v))
            })
            .collect()
    }

    /// A parallel copy: every source is read before any destination is written.
    fn phi_copies(&mut self, from: usize, to: usize) -> Res<()> {
        let pairs = self.phi_pairs(from, to);
        for (dst, src) in &pairs {
            if self.ty(*dst) != self.ty(*src) {
                return unsupported("a phi that joins values of different types");
            }
            self.get(*src);
        }
        for (dst, _) in pairs.iter().rev() {
            self.set(*dst);
        }
        Ok(())
    }

    fn is_back_edge(&self, from: usize, to: usize) -> bool {
        self.rpo_num[to] <= self.rpo_num[from]
    }

    fn branch(&mut self, from: usize, to: usize) -> Res<()> {
        self.phi_copies(from, to)?;
        if self.is_back_edge(from, to) {
            let l = self.label(Frame::Loop(to));
            self.emit(&I::Br(l));
        } else if self.merge[to] {
            let l = self.label(Frame::Block(to));
            self.emit(&I::Br(l));
        } else {
            self.do_tree(to)?;
        }
        Ok(())
    }

    fn zero(&mut self, t: Ty) {
        match t.wasm() {
            Some(ValType::I64) => self.emit(&I::I64Const(0)),
            Some(ValType::F32) => self.emit(&I::F32Const(0.0f32.into())),
            Some(ValType::F64) => self.emit(&I::F64Const(0.0f64.into())),
            _ => self.emit(&I::I32Const(0)),
        }
    }

    fn int_const(&self, idx: usize) -> Res<i32> {
        let base = self.f.int_pool_base;
        if idx >= base {
            return self
                .f
                .pending_ints
                .get(idx - base)
                .copied()
                .ok_or_else(|| Unsupported(format!("integer constant {idx} is not in the pool")));
        }
        self.program
            .int(idx)
            .ok_or_else(|| Unsupported(format!("integer constant {idx} is not in the pool")))
    }

    fn code(&mut self, x: usize) -> Res<()> {
        let f = self.f;
        let instrs = &f.blocks[x].instrs;
        for (i, ins) in instrs.iter().enumerate() {
            // Of consecutive positions only the last can be read back.
            if let Instr::Pos { .. } = ins
                && matches!(instrs.get(i + 1), Some(Instr::Pos { .. }))
            {
                continue;
            }
            self.instr(ins)?;
        }
        Ok(())
    }

    fn instr(&mut self, ins: &Instr) -> Res<()> {
        match ins {
            Instr::Pos { file, line, .. } => {
                if self.opts.shadow {
                    let frame = self.frame_local;
                    self.emit(&I::LocalGet(frame));
                    self.emit(&I::I64Const(((u64::from(*file) << 32) | u64::from(*line)) as i64));
                    self.emit(&I::I64Store(we::MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                }
            }
            Instr::Param { dst, reg } => {
                // Registers past the arguments are locals that start zeroed.
                if *reg < self.nparams {
                    if self.sig.params[*reg as usize] != self.ty(*dst) {
                        return unsupported("a parameter whose register type differs");
                    }
                    self.emit(&I::LocalGet(*reg));
                } else {
                    let t = self.ty(*dst);
                    self.zero(t);
                }
                self.set(*dst);
            }
            Instr::Copy { dst, src } => {
                if self.ty(*dst) != self.ty(*src) {
                    return unsupported("a copy between different types");
                }
                self.get(*src);
                self.set(*dst);
            }
            Instr::Int { dst, idx } => {
                let v = self.int_const(*idx)?;
                match self.ty(*dst) {
                    Ty::I32 | Ty::Bool => self.emit(&I::I32Const(v)),
                    Ty::I64 => self.emit(&I::I64Const(i64::from(v))),
                    t => return unsupported(format!("an integer constant into {t:?}")),
                }
                self.set(*dst);
            }
            Instr::Float { dst, idx } => {
                let v = self
                    .program
                    .float(*idx)
                    .ok_or_else(|| Unsupported(format!("float constant {idx} is not in the pool")))?;
                match self.ty(*dst) {
                    Ty::F64 => self.emit(&I::F64Const(v.into())),
                    Ty::F32 => self.emit(&I::F32Const((v as f32).into())),
                    t => return unsupported(format!("a float constant into {t:?}")),
                }
                self.set(*dst);
            }
            Instr::Bool { dst, value } => {
                if self.ty(*dst) != Ty::Bool {
                    return unsupported("a boolean constant into a non-boolean");
                }
                self.emit(&I::I32Const(i32::from(*value)));
                self.set(*dst);
            }
            Instr::BinOp { op, dst, a, b } => self.bin_op(*op, *dst, *a, *b)?,
            Instr::UnOp { op, dst, src } => self.un_op(*op, *dst, *src)?,
            Instr::Cast { kind, dst, src } => self.cast(*kind, *dst, *src)?,
            Instr::Call { dst, fun, args } => self.call(*dst, *fun, args)?,
            Instr::Fma { dst, a, b, c } => {
                let t = self.ty(*dst);
                if !t.is_float() || self.ty(*a) != t || self.ty(*b) != t || self.ty(*c) != t {
                    return unsupported("a fused multiply-add on mismatched types");
                }
                self.get(*a);
                self.get(*b);
                self.get(*c);
                self.emit(&I::Call(if t == Ty::F64 { FMA64 } else { FMA32 }));
                self.set(*dst);
            }
            other => {
                let name: String = format!("{other:?}")
                    .chars()
                    .take_while(|c| c.is_ascii_alphanumeric())
                    .collect();
                return unsupported(format!("the {name} instruction"));
            }
        }
        Ok(())
    }

    fn bin_op(&mut self, op: BinOp, dst: ValueId, a: ValueId, b: ValueId) -> Res<()> {
        let (ta, tb, td) = (self.ty(a), self.ty(b), self.ty(dst));
        let shift = matches!(op, BinOp::Shl | BinOp::SShr | BinOp::UShr);
        if ta.is_int() && td == ta && (tb == ta || (shift && ta == Ty::I64 && tb == Ty::I32)) {
            let wide = ta == Ty::I64;
            // The reference wraps INT_MIN / -1; wasm traps on it.
            if matches!(op, BinOp::SDiv) {
                self.get(b);
                if wide {
                    self.emit(&I::I64Const(-1));
                    self.emit(&I::I64Eq);
                    self.emit(&I::If(BlockType::Result(ValType::I64)));
                    self.emit(&I::I64Const(0));
                    self.get(a);
                    self.emit(&I::I64Sub);
                    self.emit(&I::Else);
                    self.get(a);
                    self.get(b);
                    self.emit(&I::I64DivS);
                } else {
                    self.emit(&I::I32Const(-1));
                    self.emit(&I::I32Eq);
                    self.emit(&I::If(BlockType::Result(ValType::I32)));
                    self.emit(&I::I32Const(0));
                    self.get(a);
                    self.emit(&I::I32Sub);
                    self.emit(&I::Else);
                    self.get(a);
                    self.get(b);
                    self.emit(&I::I32DivS);
                }
                self.emit(&I::End);
                self.set(dst);
                return Ok(());
            }
            self.get(a);
            self.get(b);
            if wide && tb == Ty::I32 {
                self.emit(&I::I64ExtendI32U);
            }
            let ins = if wide {
                match op {
                    BinOp::Add => I::I64Add,
                    BinOp::Sub => I::I64Sub,
                    BinOp::Mul => I::I64Mul,
                    BinOp::UDiv => I::I64DivU,
                    BinOp::SMod => I::I64RemS,
                    BinOp::UMod => I::I64RemU,
                    BinOp::Shl => I::I64Shl,
                    BinOp::SShr => I::I64ShrS,
                    BinOp::UShr => I::I64ShrU,
                    BinOp::And => I::I64And,
                    BinOp::Or => I::I64Or,
                    BinOp::Xor => I::I64Xor,
                    BinOp::SDiv => unreachable!("handled above"),
                }
            } else {
                match op {
                    BinOp::Add => I::I32Add,
                    BinOp::Sub => I::I32Sub,
                    BinOp::Mul => I::I32Mul,
                    BinOp::UDiv => I::I32DivU,
                    BinOp::SMod => I::I32RemS,
                    BinOp::UMod => I::I32RemU,
                    BinOp::Shl => I::I32Shl,
                    BinOp::SShr => I::I32ShrS,
                    BinOp::UShr => I::I32ShrU,
                    BinOp::And => I::I32And,
                    BinOp::Or => I::I32Or,
                    BinOp::Xor => I::I32Xor,
                    BinOp::SDiv => unreachable!("handled above"),
                }
            };
            self.emit(&ins);
            self.set(dst);
            return Ok(());
        }
        if ta.is_float() && ta == tb && ta == td {
            let f64_ = ta == Ty::F64;
            let ins = match (op, f64_) {
                (BinOp::Add, true) => I::F64Add,
                (BinOp::Sub, true) => I::F64Sub,
                (BinOp::Mul, true) => I::F64Mul,
                (BinOp::SDiv, true) => I::F64Div,
                (BinOp::Add, false) => I::F32Add,
                (BinOp::Sub, false) => I::F32Sub,
                (BinOp::Mul, false) => I::F32Mul,
                (BinOp::SDiv, false) => I::F32Div,
                _ => return unsupported(format!("{op:?} on floats")),
            };
            self.get(a);
            self.get(b);
            self.emit(&ins);
            self.set(dst);
            return Ok(());
        }
        unsupported(format!("{op:?} on {ta:?} and {tb:?}"))
    }

    fn un_op(&mut self, op: UnOp, dst: ValueId, src: ValueId) -> Res<()> {
        let (ts, td) = (self.ty(src), self.ty(dst));
        if ts != td {
            return unsupported("a unary operation that changes type");
        }
        match (op, ts) {
            (UnOp::Neg, Ty::I32) => {
                self.emit(&I::I32Const(0));
                self.get(src);
                self.emit(&I::I32Sub);
            }
            (UnOp::Neg, Ty::I64) => {
                self.emit(&I::I64Const(0));
                self.get(src);
                self.emit(&I::I64Sub);
            }
            (UnOp::Neg, Ty::F64) => {
                self.get(src);
                self.emit(&I::F64Neg);
            }
            (UnOp::Neg, Ty::F32) => {
                self.get(src);
                self.emit(&I::F32Neg);
            }
            (UnOp::Not, Ty::Bool) => {
                self.get(src);
                self.emit(&I::I32Eqz);
            }
            (UnOp::Not, Ty::I32) => {
                self.get(src);
                self.emit(&I::I32Const(-1));
                self.emit(&I::I32Xor);
            }
            (UnOp::Not, Ty::I64) => {
                self.get(src);
                self.emit(&I::I64Const(-1));
                self.emit(&I::I64Xor);
            }
            (UnOp::Incr | UnOp::Decr, Ty::I32) => {
                self.get(src);
                self.emit(&I::I32Const(1));
                self.emit(&if matches!(op, UnOp::Incr) {
                    I::I32Add
                } else {
                    I::I32Sub
                });
            }
            (UnOp::Incr | UnOp::Decr, Ty::I64) => {
                self.get(src);
                self.emit(&I::I64Const(1));
                self.emit(&if matches!(op, UnOp::Incr) {
                    I::I64Add
                } else {
                    I::I64Sub
                });
            }
            (op, t) => return unsupported(format!("{op:?} on {t:?}")),
        }
        self.set(dst);
        Ok(())
    }

    fn cast(&mut self, kind: CastKind, dst: ValueId, src: ValueId) -> Res<()> {
        let (ts, td) = (self.ty(src), self.ty(dst));
        match kind {
            CastKind::ToSFloat | CastKind::ToUFloat => {
                let signed = matches!(kind, CastKind::ToSFloat);
                self.get(src);
                match ts {
                    Ty::I32 | Ty::Bool => self.emit(&if signed {
                        I::F64ConvertI32S
                    } else {
                        I::F64ConvertI32U
                    }),
                    Ty::I64 => self.emit(&if signed {
                        I::F64ConvertI64S
                    } else {
                        I::F64ConvertI64U
                    }),
                    Ty::F32 => self.emit(&I::F64PromoteF32),
                    Ty::F64 => {}
                    t => return unsupported(format!("{kind:?} from {t:?}")),
                }
                match td {
                    Ty::F64 => {}
                    Ty::F32 => self.emit(&I::F32DemoteF64),
                    t => return unsupported(format!("{kind:?} into {t:?}")),
                }
            }
            CastKind::ToInt => {
                self.get(src);
                match (ts, td) {
                    // A float saturates at the destination's range.
                    (Ty::F64, Ty::I32) => self.emit(&I::I32TruncSatF64S),
                    (Ty::F64, Ty::I64) => self.emit(&I::I64TruncSatF64S),
                    (Ty::F32, Ty::I32) => self.emit(&I::I32TruncSatF32S),
                    (Ty::F32, Ty::I64) => self.emit(&I::I64TruncSatF32S),
                    (Ty::I32 | Ty::Bool, Ty::I64) => self.emit(&I::I64ExtendI32S),
                    (Ty::I64, Ty::I32) => self.emit(&I::I32WrapI64),
                    (Ty::I32 | Ty::Bool, Ty::I32) | (Ty::I64, Ty::I64) => {}
                    (a, b) => return unsupported(format!("ToInt from {a:?} into {b:?}")),
                }
            }
            other => return unsupported(format!("the {other:?} cast")),
        }
        self.set(dst);
        Ok(())
    }

    fn call(&mut self, dst: ValueId, fun: usize, args: &[ValueId]) -> Res<()> {
        let Some(callee) = self.callees.get(&fun) else {
            return unsupported(format!("a call to f{fun}, outside the module"));
        };
        if callee.sig.params.len() != args.len() {
            return unsupported(format!("a call to f{fun} with the wrong argument count"));
        }
        for (a, want) in args.iter().zip(&callee.sig.params) {
            if self.ty(*a) != *want {
                return unsupported(format!("a call to f{fun} with a mismatched argument"));
            }
        }
        let index = callee.index;
        let ret = callee.sig.ret;
        if ret != Ty::Void && self.ty(dst) != ret {
            return unsupported(format!("a call to f{fun} whose result type differs"));
        }
        for a in args {
            self.get(*a);
        }
        self.emit(&I::Call(index));
        self.called.push(fun);
        if ret != Ty::Void {
            self.set(dst);
        }
        Ok(())
    }

    fn condition(&mut self, c: CondKind, a: ValueId, b: Option<ValueId>) -> Res<()> {
        let t = self.ty(a);
        self.get(a);
        match b {
            Some(b) => {
                if self.ty(b) != t {
                    return unsupported("a comparison of different types");
                }
                self.get(b);
                let (op, negate) = compare(t, c)?;
                self.emit(&op);
                if negate {
                    self.emit(&I::I32Eqz);
                }
            }
            None => match (t, c) {
                (Ty::I32 | Ty::Bool, CondKind::True | CondKind::NotNull) => {}
                (Ty::I32 | Ty::Bool, CondKind::False | CondKind::Null) => self.emit(&I::I32Eqz),
                (Ty::I64, CondKind::True | CondKind::NotNull) => {
                    self.emit(&I::I64Eqz);
                    self.emit(&I::I32Eqz);
                }
                (Ty::I64, CondKind::False | CondKind::Null) => self.emit(&I::I64Eqz),
                (t, c) => return unsupported(format!("the condition {c:?} on {t:?}")),
            },
        }
        Ok(())
    }

    fn terminator(&mut self, x: usize) -> Res<()> {
        let f = self.f;
        match &f.blocks[x].term {
            Terminator::Ret { value } => {
                if self.opts.shadow {
                    self.emit(&I::Call(SHADOW_POP));
                }
                if self.sig.ret != Ty::Void {
                    if self.ty(*value) != self.sig.ret {
                        return unsupported("a return whose type differs from the signature");
                    }
                    self.get(*value);
                }
                self.emit(&I::Return);
            }
            Terminator::Jump { target } => self.branch(x, target.idx())?,
            Terminator::CondJump {
                cond,
                a,
                b,
                if_true,
                if_false,
            } => {
                self.condition(*cond, *a, *b)?;
                self.emit(&I::If(BlockType::Empty));
                self.ctx.push(Frame::If);
                self.branch(x, if_true.idx())?;
                self.emit(&I::Else);
                self.branch(x, if_false.idx())?;
                self.ctx.pop();
                self.emit(&I::End);
            }
            Terminator::Switch {
                value,
                targets,
                default,
            } => self.switch(x, *value, targets, *default)?,
            Terminator::Throw { .. } | Terminator::Rethrow { .. } | Terminator::Trap { .. } => {
                return unsupported("an exception");
            }
        }
        Ok(())
    }

    fn switch(
        &mut self,
        x: usize,
        value: ValueId,
        targets: &[air::v2::ir::BlockId],
        default: air::v2::ir::BlockId,
    ) -> Res<()> {
        if self.ty(value) != Ty::I32 {
            return unsupported("a switch on a non-i32");
        }
        let carries_phis = targets
            .iter()
            .chain(std::iter::once(&default))
            .any(|t| !self.phi_pairs(x, t.idx()).is_empty());
        if !carries_phis {
            let label = |fx: &Self, to: usize| {
                if fx.is_back_edge(x, to) {
                    fx.label(Frame::Loop(to))
                } else {
                    fx.label(Frame::Block(to))
                }
            };
            let labels: Vec<u32> = targets.iter().map(|t| label(self, t.idx())).collect();
            let d = label(self, default.idx());
            self.get(value);
            self.emit(&I::BrTable(labels.into(), d));
            return Ok(());
        }
        // A phi-carrying edge needs its copies, which a br_table cannot run:
        // test the cases one by one instead.
        for (i, t) in targets.iter().enumerate() {
            self.get(value);
            self.emit(&I::I32Const(i as i32));
            self.emit(&I::I32Eq);
            self.emit(&I::If(BlockType::Empty));
            self.ctx.push(Frame::If);
            self.branch(x, t.idx())?;
            self.ctx.pop();
            self.emit(&I::End);
        }
        self.branch(x, default.idx())
    }
}

/// The wasm comparison for `c` on `t`, and whether its result is inverted.
fn compare(t: Ty, c: CondKind) -> Res<(I<'static>, bool)> {
    use CondKind as C;
    let r = match t {
        Ty::I32 | Ty::Bool => match c {
            C::Eq => (I::I32Eq, false),
            C::NotEq => (I::I32Ne, false),
            C::SLt => (I::I32LtS, false),
            C::SGte => (I::I32GeS, false),
            C::SGt => (I::I32GtS, false),
            C::SLte => (I::I32LeS, false),
            C::ULt => (I::I32LtU, false),
            C::UGte => (I::I32GeU, false),
            C::NotLt => (I::I32LtS, true),
            C::NotGte => (I::I32GeS, true),
            _ => return unsupported(format!("the condition {c:?} with two operands")),
        },
        Ty::I64 => match c {
            C::Eq => (I::I64Eq, false),
            C::NotEq => (I::I64Ne, false),
            C::SLt => (I::I64LtS, false),
            C::SGte => (I::I64GeS, false),
            C::SGt => (I::I64GtS, false),
            C::SLte => (I::I64LeS, false),
            C::ULt => (I::I64LtU, false),
            C::UGte => (I::I64GeU, false),
            C::NotLt => (I::I64LtS, true),
            C::NotGte => (I::I64GeS, true),
            _ => return unsupported(format!("the condition {c:?} with two operands")),
        },
        Ty::F64 => match c {
            C::Eq => (I::F64Eq, false),
            C::NotEq => (I::F64Ne, false),
            C::SLt | C::ULt => (I::F64Lt, false),
            C::SGte | C::UGte => (I::F64Ge, false),
            C::SGt => (I::F64Gt, false),
            C::SLte => (I::F64Le, false),
            C::NotLt => (I::F64Lt, true),
            C::NotGte => (I::F64Ge, true),
            _ => return unsupported(format!("the condition {c:?} with two operands")),
        },
        Ty::F32 => match c {
            C::Eq => (I::F32Eq, false),
            C::NotEq => (I::F32Ne, false),
            C::SLt | C::ULt => (I::F32Lt, false),
            C::SGte | C::UGte => (I::F32Ge, false),
            C::SGt => (I::F32Gt, false),
            C::SLte => (I::F32Le, false),
            C::NotLt => (I::F32Lt, true),
            C::NotGte => (I::F32Ge, true),
            _ => return unsupported(format!("the condition {c:?} with two operands")),
        },
        Ty::Void | Ty::Other => return unsupported("a comparison of non-numbers"),
    };
    Ok(r)
}
