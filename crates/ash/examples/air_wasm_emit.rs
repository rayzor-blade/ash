//! Emit-only prototype of a direct AIR-to-wasm backend: how long it takes, and
//! whether the control flow it produces validates.
//!
//! Usage: air_wasm_emit <file.hl> [--out module.wasm]
//!
//! Every function's optimized AIR becomes a wasm function body. Control flow is
//! structured from the dominator tree ("Beyond Relooper"): `loop` for a loop
//! header, `block` for a merge node, `if` for a conditional branch. Cheap
//! instructions are real wasm instructions; the rest are calls to typed helper
//! imports, one per (kind, operand types, result type). Exceptions, switch
//! edges that carry phis and the runtime semantics of those helpers are NOT
//! modelled, so the module is well-formed but does not run.
use air::v2::ir::{CondKind, Function, Instr, Terminator};
use air::v2::CfgInfo;
use ash_core::hl_bindings as hl;
use std::collections::HashMap;
use std::time::Instant;
use wasm_encoder as we;
use we::{BlockType, Instruction as I, ValType};

#[derive(Clone, Copy, PartialEq)]
enum Frame {
    Loop(usize),
    Block(usize),
    If,
}

#[derive(Default)]
struct Registry {
    types: HashMap<(Vec<ValType>, Vec<ValType>), u32>,
    type_list: Vec<(Vec<ValType>, Vec<ValType>)>,
    helpers: HashMap<(String, Vec<ValType>, Option<ValType>), u32>,
    helper_list: Vec<u32>, // type index per import
}

impl Registry {
    fn ty(&mut self, params: Vec<ValType>, results: Vec<ValType>) -> u32 {
        let key = (params, results);
        if let Some(&t) = self.types.get(&key) {
            return t;
        }
        let t = self.type_list.len() as u32;
        self.type_list.push(key.clone());
        self.types.insert(key, t);
        t
    }
    fn helper(&mut self, kind: &str, params: Vec<ValType>, result: Option<ValType>) -> u32 {
        let key = (kind.to_string(), params.clone(), result);
        if let Some(&h) = self.helpers.get(&key) {
            return h;
        }
        let ty = self.ty(params, result.into_iter().collect());
        let h = self.helper_list.len() as u32;
        self.helper_list.push(ty);
        self.helpers.insert(key, h);
        h
    }
}

struct Fx<'a> {
    f: &'a Function,
    reg: &'a mut Registry,
    out: we::Function,
    ctx: Vec<Frame>,
    nparams: u32,
    ret: Option<ValType>,
    rpo_num: Vec<usize>,
    loop_header: Vec<bool>,
    merge: Vec<bool>,
    kids: Vec<Vec<usize>>,
    value_ty: Vec<ValType>,
}

fn valtype(bc: &ash_core::bytecode::DecodedBytecode, ty: usize) -> Option<ValType> {
    let k = bc.types[ty].kind;
    if k == hl::hl_type_kind_HVOID {
        None
    } else if k == hl::hl_type_kind_HF64 {
        Some(ValType::F64)
    } else if k == hl::hl_type_kind_HF32 {
        Some(ValType::F32)
    } else if k == hl::hl_type_kind_HI64 {
        Some(ValType::I64)
    } else {
        Some(ValType::I32)
    }
}

impl Fx<'_> {
    fn emit(&mut self, i: &I) {
        self.out.instruction(i);
    }
    fn get(&mut self, v: air::v2::ir::ValueId) {
        self.emit(&I::LocalGet(self.nparams + v.0));
    }
    fn set(&mut self, v: air::v2::ir::ValueId) {
        self.emit(&I::LocalSet(self.nparams + v.0));
    }
    fn vt(&self, v: air::v2::ir::ValueId) -> ValType {
        self.value_ty[v.0 as usize]
    }

    fn label(&self, want: Frame) -> u32 {
        self.ctx
            .iter()
            .rev()
            .position(|f| *f == want)
            .expect("branch target has no enclosing label") as u32
    }

    fn do_tree(&mut self, x: usize) {
        let ys = self.kids[x].clone();
        if self.loop_header[x] {
            self.emit(&I::Loop(BlockType::Empty));
            self.ctx.push(Frame::Loop(x));
            self.node_within(x, &ys);
            self.ctx.pop();
            self.emit(&I::End);
        } else {
            self.node_within(x, &ys);
        }
    }

    fn node_within(&mut self, x: usize, ys: &[usize]) {
        match ys.split_last() {
            None => {
                self.code(x);
                self.terminator(x);
            }
            Some((&y, rest)) => {
                self.emit(&I::Block(BlockType::Empty));
                self.ctx.push(Frame::Block(y));
                self.node_within(x, rest);
                self.ctx.pop();
                self.emit(&I::End);
                self.do_tree(y);
            }
        }
    }

    /// Copy the values `to`'s phis take along the edge from `from`.
    fn phi_copies(&mut self, from: usize, to: usize) {
        let blk = &self.f.blocks[to];
        let pairs: Vec<_> = blk
            .phis
            .iter()
            .filter_map(|p| {
                p.incoming
                    .iter()
                    .find(|(b, _)| b.idx() == from)
                    .map(|(_, v)| (p.dst, *v))
            })
            .collect();
        for (_, src) in &pairs {
            self.get(*src);
        }
        for (dst, _) in pairs.iter().rev() {
            self.set(*dst);
        }
    }

    fn branch(&mut self, from: usize, to: usize) {
        self.phi_copies(from, to);
        if self.rpo_num[to] <= self.rpo_num[from] {
            let l = self.label(Frame::Loop(to));
            self.emit(&I::Br(l));
        } else if self.merge[to] {
            let l = self.label(Frame::Block(to));
            self.emit(&I::Br(l));
        } else {
            self.do_tree(to);
        }
    }

    fn helper_call(&mut self, kind: &str, uses: &[air::v2::ir::ValueId], dst: Option<air::v2::ir::ValueId>) {
        let params: Vec<ValType> = uses.iter().map(|v| self.vt(*v)).collect();
        let result = dst.map(|d| self.vt(d));
        let h = self.reg.helper(kind, params, result);
        for v in uses {
            self.get(*v);
        }
        // Imports are numbered first, so a helper's function index is its own.
        self.emit(&I::Call(h));
        if let Some(d) = dst {
            self.set(d);
        }
    }

    fn zero(&mut self, t: ValType) {
        match t {
            ValType::I64 => self.emit(&I::I64Const(0)),
            ValType::F32 => self.emit(&I::F32Const(0.0f32.into())),
            ValType::F64 => self.emit(&I::F64Const(0.0f64.into())),
            _ => self.emit(&I::I32Const(0)),
        }
    }

    fn code(&mut self, x: usize) {
        let blk = &self.f.blocks[x];
        for ins in &blk.instrs {
            let kind: String = format!("{ins:?}")
                .chars()
                .take_while(|c| c.is_ascii_alphanumeric())
                .collect();
            let uses = ins.uses();
            let dst = ins.dst();
            match ins {
                Instr::Pos { .. } => {}
                Instr::Param { dst, reg } => {
                    // Registers past the arguments are locals that start zeroed.
                    if *reg < self.nparams {
                        self.emit(&I::LocalGet(*reg));
                    } else {
                        let t = self.vt(*dst);
                        self.zero(t);
                    }
                    self.set(*dst);
                }
                Instr::Copy { dst, src } => {
                    self.get(*src);
                    self.set(*dst);
                }
                Instr::Int { dst, .. }
                | Instr::Float { dst, .. }
                | Instr::Bool { dst, .. }
                | Instr::Null { dst } => {
                    let t = self.vt(*dst);
                    self.zero(t);
                    self.set(*dst);
                }
                Instr::BinOp { dst, a, b, .. }
                    if self.vt(*a) == self.vt(*dst) && self.vt(*b) == self.vt(*dst) =>
                {
                    self.get(*a);
                    self.get(*b);
                    let op = match self.vt(*dst) {
                        ValType::I64 => I::I64Add,
                        ValType::F32 => I::F32Add,
                        ValType::F64 => I::F64Add,
                        _ => I::I32Add,
                    };
                    self.emit(&op);
                    self.set(*dst);
                }
                _ => self.helper_call(&kind, &uses, dst),
            }
        }
    }

    fn condition(&mut self, c: CondKind, a: air::v2::ir::ValueId, b: Option<air::v2::ir::ValueId>) {
        let t = self.vt(a);
        self.get(a);
        match b {
            Some(b) if self.vt(b) == t => {
                self.get(b);
                let (op, negate) = match (t, c) {
                    (ValType::I64, CondKind::Eq) => (I::I64Eq, false),
                    (ValType::I64, CondKind::NotEq) => (I::I64Ne, false),
                    (ValType::I64, CondKind::SLt) => (I::I64LtS, false),
                    (ValType::I64, CondKind::SGte) => (I::I64GeS, false),
                    (ValType::I64, CondKind::SGt) => (I::I64GtS, false),
                    (ValType::I64, CondKind::SLte) => (I::I64LeS, false),
                    (ValType::I64, CondKind::ULt) => (I::I64LtU, false),
                    (ValType::I64, CondKind::UGte) => (I::I64GeU, false),
                    (ValType::I64, CondKind::NotLt) => (I::I64LtS, true),
                    (ValType::I64, _) => (I::I64GeS, true),
                    (ValType::F32, CondKind::Eq) => (I::F32Eq, false),
                    (ValType::F32, CondKind::NotEq) => (I::F32Ne, false),
                    (ValType::F32, CondKind::SLt | CondKind::ULt) => (I::F32Lt, false),
                    (ValType::F32, CondKind::SGte | CondKind::UGte) => (I::F32Ge, false),
                    (ValType::F32, CondKind::SGt) => (I::F32Gt, false),
                    (ValType::F32, CondKind::SLte) => (I::F32Le, false),
                    (ValType::F32, CondKind::NotLt) => (I::F32Lt, true),
                    (ValType::F32, _) => (I::F32Ge, true),
                    (ValType::F64, CondKind::Eq) => (I::F64Eq, false),
                    (ValType::F64, CondKind::NotEq) => (I::F64Ne, false),
                    (ValType::F64, CondKind::SLt | CondKind::ULt) => (I::F64Lt, false),
                    (ValType::F64, CondKind::SGte | CondKind::UGte) => (I::F64Ge, false),
                    (ValType::F64, CondKind::SGt) => (I::F64Gt, false),
                    (ValType::F64, CondKind::SLte) => (I::F64Le, false),
                    (ValType::F64, CondKind::NotLt) => (I::F64Lt, true),
                    (ValType::F64, _) => (I::F64Ge, true),
                    (_, CondKind::Eq) => (I::I32Eq, false),
                    (_, CondKind::NotEq) => (I::I32Ne, false),
                    (_, CondKind::SLt) => (I::I32LtS, false),
                    (_, CondKind::SGte) => (I::I32GeS, false),
                    (_, CondKind::SGt) => (I::I32GtS, false),
                    (_, CondKind::SLte) => (I::I32LeS, false),
                    (_, CondKind::ULt) => (I::I32LtU, false),
                    (_, CondKind::UGte) => (I::I32GeU, false),
                    (_, CondKind::NotLt) => (I::I32LtS, true),
                    (_, _) => (I::I32GeS, true),
                };
                self.emit(&op);
                if negate {
                    self.emit(&I::I32Eqz);
                }
            }
            _ => {
                // Unary (or a mistyped pair): test against zero.
                match t {
                    ValType::I64 => {
                        self.emit(&I::I64Eqz);
                        if matches!(c, CondKind::True | CondKind::NotNull) {
                            self.emit(&I::I32Eqz);
                        }
                    }
                    ValType::F32 | ValType::F64 => {
                        self.emit(&I::Drop);
                        self.emit(&I::I32Const(1));
                    }
                    _ => {
                        if matches!(c, CondKind::False | CondKind::Null) {
                            self.emit(&I::I32Eqz);
                        }
                    }
                }
            }
        }
    }

    fn terminator(&mut self, x: usize) {
        match &self.f.blocks[x].term {
            Terminator::Ret { value } => {
                if self.ret.is_some() {
                    let v = *value;
                    self.get(v);
                }
                self.emit(&I::Return);
            }
            Terminator::Jump { target } => self.branch(x, target.idx()),
            Terminator::CondJump {
                cond,
                a,
                b,
                if_true,
                if_false,
            } => {
                let (cond, a, b, t, e) = (*cond, *a, *b, if_true.idx(), if_false.idx());
                self.condition(cond, a, b);
                self.emit(&I::If(BlockType::Empty));
                self.ctx.push(Frame::If);
                self.branch(x, t);
                self.emit(&I::Else);
                self.branch(x, e);
                self.ctx.pop();
                self.emit(&I::End);
            }
            Terminator::Switch {
                value,
                targets,
                default,
            } => {
                let v = *value;
                let default = default.idx();
                let labels: Vec<u32> = targets.iter().map(|t| self.target_label(x, t.idx())).collect();
                let d = self.target_label(x, default);
                self.get(v);
                self.emit(&I::BrTable(labels.into(), d));
            }
            Terminator::Throw { exc } | Terminator::Rethrow { exc } => {
                let exc = *exc;
                self.helper_call("Throw", &[exc], None);
                self.emit(&I::Unreachable);
            }
            Terminator::Trap { normal, .. } => {
                let n = normal.idx();
                self.helper_call("TrapEnter", &[], None);
                self.branch(x, n);
            }
        }
    }

    fn target_label(&self, from: usize, to: usize) -> u32 {
        if self.rpo_num[to] <= self.rpo_num[from] {
            self.label(Frame::Loop(to))
        } else {
            self.label(Frame::Block(to))
        }
    }
}

fn emit_function(
    bc: &ash_core::bytecode::DecodedBytecode,
    f: &Function,
    sig: &(Vec<ValType>, Option<ValType>),
    reg: &mut Registry,
) -> we::Function {
    let cfg = CfgInfo::build(f);
    let n = f.blocks.len();
    let rpo = cfg.rpo();
    let mut rpo_num = vec![usize::MAX; n];
    for (i, b) in rpo.iter().enumerate() {
        rpo_num[b.idx()] = i;
    }
    // Forward in-edges, counted over the terminators' own successors.
    let mut fwd_in = vec![0usize; n];
    let mut forced = vec![false; n];
    let mut loop_header = vec![false; n];
    for b in &rpo {
        let from = b.idx();
        let blk = &f.blocks[from];
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
        match &blk.term {
            Terminator::Switch { targets, default, .. } => {
                for t in targets.iter().chain(std::iter::once(default)) {
                    forced[t.idx()] = true;
                }
            }
            Terminator::Trap { handler, .. } => forced[handler.idx()] = true,
            _ => {}
        }
        if let Some(h) = blk.handler {
            forced[h.idx()] = true;
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
        for c in &cfg.dom.dom_children[i] {
            if *c != i && merge[*c] {
                kids[i].push(*c);
            }
        }
        kids[i].sort_by_key(|k| rpo_num[*k]);
    }
    let value_ty: Vec<ValType> = f
        .values
        .iter()
        .map(|v| valtype(bc, v.ty.0 as usize).unwrap_or(ValType::I32))
        .collect();
    let locals: Vec<(u32, ValType)> = value_ty.iter().map(|t| (1, *t)).collect();
    let mut fx = Fx {
        f,
        reg,
        out: we::Function::new(locals),
        ctx: Vec::new(),
        nparams: sig.0.len() as u32,
        ret: sig.1,
        rpo_num,
        loop_header,
        merge,
        kids,
        value_ty,
    };
    if !rpo.is_empty() {
        fx.do_tree(rpo[0].idx());
    }
    fx.emit(&I::Unreachable);
    fx.emit(&I::End);
    fx.out
}

fn main() -> anyhow::Result<()> {
    let mut args = std::env::args().skip(1);
    let path = args.next().expect("usage: air_wasm_emit <file.hl> [--out m.wasm]");
    let out_path = match (args.next().as_deref(), args.next()) {
        (Some("--out"), Some(p)) => Some(p),
        _ => None,
    };
    std::thread::Builder::new()
        .stack_size(512 << 20)
        .spawn(move || run(&path, out_path))?
        .join()
        .expect("emitter thread panicked")
}

fn run(path: &str, out_path: Option<String>) -> anyhow::Result<()> {
    ash_core::native_lib::init_std_library()?;
    let bc = ash_core::bytecode::BytecodeDecoder::decode(std::path::Path::new(path))?;
    let m = ash_core::air_pipeline::AshModule::new(&bc);

    // AIR for every function first, so the timing below is the emitter alone.
    let began = Instant::now();
    let mut bodies = Vec::new();
    for f in &bc.functions {
        if let Ok(o) = ash_core::air_pipeline::optimized(&m, f) {
            bodies.push((f, o));
        }
    }
    println!(
        "AIR for {} functions: {:.0} ms (cached lowering, not part of the emitter)",
        bodies.len(),
        began.elapsed().as_secs_f64() * 1e3
    );

    let mut reg = Registry::default();
    let mut local_types: Vec<u32> = Vec::new();
    let mut code = we::CodeSection::new();
    let began = Instant::now();
    for (f, o) in &bodies {
        let fun = bc.types[f.type_.0].fun.as_ref().expect("function type");
        let params: Vec<ValType> = fun
            .args
            .iter()
            .map(|a| valtype(&bc, a.0).unwrap_or(ValType::I32))
            .collect();
        let ret = valtype(&bc, fun.ret.0);
        let sig = (params.clone(), ret);
        let ty = reg.ty(params, ret.into_iter().collect());
        local_types.push(ty);
        let body = emit_function(&bc, &o.ir, &sig, &mut reg);
        code.function(&body);
    }
    let emit = began.elapsed();

    let began = Instant::now();
    let mut module = we::Module::new();
    let mut types = we::TypeSection::new();
    for (p, r) in &reg.type_list {
        types.ty().function(p.iter().copied(), r.iter().copied());
    }
    module.section(&types);
    let mut imports = we::ImportSection::new();
    for (i, t) in reg.helper_list.iter().enumerate() {
        imports.import("h", &format!("h{i}"), we::EntityType::Function(*t));
    }
    module.section(&imports);
    let mut funcs = we::FunctionSection::new();
    for t in &local_types {
        funcs.function(*t);
    }
    module.section(&funcs);
    module.section(&code);
    let bytes = module.finish();
    let assemble = began.elapsed();

    println!(
        "emit {:.0} ms for {} functions; assemble {:.0} ms; module {} bytes; {} helper imports; {} types",
        emit.as_secs_f64() * 1e3,
        bodies.len(),
        assemble.as_secs_f64() * 1e3,
        bytes.len(),
        reg.helper_list.len(),
        reg.type_list.len()
    );

    let began = Instant::now();
    // Function by function, so a failure names the function.
    let mut validator = wasmparser::Validator::new();
    let (mut index, mut failed) = (0usize, 0usize);
    let mut first: Vec<String> = Vec::new();
    for payload in wasmparser::Parser::new(0).parse_all(&bytes) {
        match payload? {
            wasmparser::Payload::CodeSectionEntry(body) => {
                let (f, _) = &bodies[index];
                let checked = validator
                    .code_section_entry(&body)
                    .and_then(|func| func.into_validator(Default::default()).validate(&body));
                if let Err(e) = checked {
                    failed += 1;
                    if first.len() < 12 {
                        first.push(format!("findex {} {}: {e}", f.findex, f.name()));
                    }
                }
                index += 1;
            }
            other => {
                if let Err(e) = validator.payload(&other) {
                    println!("VALIDATION FAILED before the code section: {e}");
                    break;
                }
            }
        }
    }
    println!(
        "validation: {} of {} function bodies failed ({:.0} ms)",
        failed,
        index,
        began.elapsed().as_secs_f64() * 1e3
    );
    for line in first {
        println!("  {line}");
    }
    if let Some(p) = out_path {
        std::fs::write(&p, &bytes)?;
        println!("wrote {p}");
    }
    Ok(())
}
