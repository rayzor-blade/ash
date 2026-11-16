//! Lowers a function body from the typed program to an AIR [`Function`].
//!
//! This covers what a numeric kernel is made of: integer, float and bool values, locals,
//! arithmetic and comparisons, `if` and loops, `break` and `continue`, `return`, and direct calls
//! of static functions; and objects of non-generic classes: `new`, instance variables, `this`,
//! calls of methods, and `null` with `==` and `!=` against it; `new Array<T>()`, and reads and
//! writes of array elements by index, and their `length`. Anything else is refused with a message
//! that names it, so a body that does not lower is never half lowered.
//!
//! Locals are put in SSA form as the body is walked (Braun et al., "Simple and Efficient
//! Construction of Static Single Assignment Form"): a block is sealed once all its predecessors
//! are known, a read in an unsealed block makes an incomplete phi, and a phi that turns out to
//! merge one value is removed. Control flow is built from structure, so there is no goto.

use std::collections::{HashMap, HashSet};

use air::v2::ir::*;
use ash_tast::Tast;
use ash_tast::format::*;

use crate::build::TypeBuilder;
use crate::hl::{HlType, TypeIx};

mod arrays;
mod init;
mod intrinsics;
mod methods;
mod nulls;
mod objects;
mod statics;
mod strings;

pub type Result<T> = std::result::Result<T, String>;

/// A lowered function and where it belongs in the module.
pub struct Lowered {
    pub name: String,
    pub findex: u32,
    /// The function's type.
    pub sig: TypeIx,
    pub function: Function,
    /// The class this function is the constructor of.
    pub ctor_of: Option<u32>,
}

/// A constant object the module holds in a global: a string literal.
#[derive(Clone, Copy, Debug)]
pub struct Constant {
    pub global: u32,
    /// The text, as an index into the module's string pool.
    pub text: usize,
}

/// A function the runtime provides, which the module calls.
#[derive(Clone, Debug)]
pub struct Native {
    pub lib: String,
    pub name: String,
    /// The function type.
    pub ty: TypeIx,
    pub findex: u32,
}

/// The module being built: its types, and the constant pools the instructions index.
pub struct Module<'a> {
    pub types: TypeBuilder<'a>,
    tast: Tast<'a>,
    pub natives: Vec<Native>,
    requested: HashSet<u32>,
    pending: Vec<u32>,
    /// The functions some lowered body calls.
    called: HashSet<u32>,
    /// The functions a call through a method table may run, by the receiver's type and the slot.
    pub table_calls: HashMap<(TypeRef, usize), Vec<usize>>,
    pub ints: Vec<i32>,
    int_at: HashMap<i32, usize>,
    pub floats: Vec<f64>,
    float_at: HashMap<u64, usize>,
    pub strings: Vec<String>,
    string_at: HashMap<String, usize>,
    /// The global that holds each string literal's `String` object, and the constants to put in
    /// them when the module starts.
    string_globals: HashMap<String, u32>,
    pub constants: Vec<Constant>,
}

impl<'a> Module<'a> {
    pub fn new(tast: Tast<'a>) -> Module<'a> {
        Module {
            types: TypeBuilder::new(tast),
            tast,
            natives: Vec::new(),
            requested: HashSet::new(),
            pending: Vec::new(),
            called: HashSet::new(),
            table_calls: HashMap::new(),
            ints: Vec::new(),
            int_at: HashMap::new(),
            floats: Vec::new(),
            float_at: HashMap::new(),
            strings: Vec::new(),
            string_at: HashMap::new(),
            string_globals: HashMap::new(),
            constants: Vec::new(),
        }
    }

    fn int_index(&mut self, v: i32) -> usize {
        if let Some(i) = self.int_at.get(&v) {
            return *i;
        }
        self.ints.push(v);
        self.int_at.insert(v, self.ints.len() - 1);
        self.ints.len() - 1
    }

    fn float_index(&mut self, v: f64) -> usize {
        if let Some(i) = self.float_at.get(&v.to_bits()) {
            return *i;
        }
        self.floats.push(v);
        self.float_at.insert(v.to_bits(), self.floats.len() - 1);
        self.floats.len() - 1
    }

    /// Asks for the function a field holds to be lowered, if it has not been asked for.
    fn string_index(&mut self, text: &str) -> usize {
        if let Some(i) = self.string_at.get(text) {
            return *i;
        }
        self.strings.push(text.to_owned());
        self.string_at.insert(text.to_owned(), self.strings.len() - 1);
        self.strings.len() - 1
    }

    /// The global that holds the `String` constant `text`, made on first use.
    fn string_global(&mut self, text: &str, ty: TypeIx) -> u32 {
        if let Some(g) = self.string_globals.get(text) {
            return *g;
        }
        let global = self.types.table.globals.len() as u32;
        self.types.table.globals.push((String::new(), ty));
        let text_ix = self.string_index(text);
        self.string_globals.insert(text.to_owned(), global);
        self.constants.push(Constant {
            global,
            text: text_ix,
        });
        global
    }

    fn request(&mut self, field: u32) {
        if self.requested.insert(field) {
            self.pending.push(field);
        }
    }

    fn add_native(&mut self, lib: String, name: String, ty: TypeIx, findex: u32) {
        if !self.natives.iter().any(|n| n.findex == findex) {
            self.natives.push(Native {
                lib,
                name,
                ty,
                findex,
            });
        }
    }

    /// Lowers `entry` and every function it calls that has a body, and notes the natives.
    pub fn lower_program(&mut self, entry: u32) -> Result<Vec<Lowered>> {
        self.lower_all(&[entry])
    }

    /// Lowers each of `entries` and the functions they call. Every function a body names must
    /// end up defined, by a body or a native; one that only a layout names (a method in an
    /// object's method table that no lowered call reaches) is left out.
    pub fn lower_all(&mut self, entries: &[u32]) -> Result<Vec<Lowered>> {
        for entry in entries {
            self.request(*entry);
        }
        let mut out = Vec::new();
        while let Some(f) = self.pending.pop() {
            out.push(self.lower_field(f)?);
        }
        let defined: HashSet<u32> = out
            .iter()
            .map(|l| l.findex)
            .chain(self.natives.iter().map(|n| n.findex))
            .collect();
        for findex in &self.called {
            if !defined.contains(findex) {
                let (class, name) = &self.types.fun_names[*findex as usize];
                return Err(format!("{class}.{name} is called but not lowered"));
            }
        }
        // The pool kept growing after the first function was done. An index at or past a
        // function's base means a pass minted it, so every base is the final length, or a
        // function that finished early would read its callees' constants as minted ones once
        // they are inlined into it.
        for l in &mut out {
            l.function.int_pool_base = self.ints.len();
        }
        Ok(out)
    }

    /// Lowers the function a field holds, naming it as calls to it do.
    pub fn lower_field(&mut self, field: u32) -> Result<Lowered> {
        let rec = self.tast.fields()[field as usize];
        let func = rec.func.get();
        if func == NONE {
            return Err("the field has no body".into());
        }
        let class = self.types.class_name(rec.owner.get()).to_owned();
        let name = self.types.field_name(field).to_owned();
        let findex = self.types.fun_id(&class, &name);
        let mut sig = self.types.to_type(rec.ty.get())?;
        let flags = rec.flags.get();
        // a method takes its object first, which its declared type does not say
        let this_ty = if flags & field_flag::METHOD != 0 && flags & field_flag::STATIC == 0 {
            let owner = rec.owner.get();
            let t = self.types.class_type(owner, &[], false, None)?;
            if let HlType::Fun(params, ret) = self.types.table.types[sig].clone() {
                let mut all = vec![t];
                all.extend(params);
                sig = self.types.table.add(HlType::Fun(all, ret));
            }
            Some((t, owner))
        } else {
            None
        };
        let function = FnLower::new(self, func, this_ty)?.run()?;
        let ctor_of = (name == "new" && flags & field_flag::STATIC == 0).then(|| rec.owner.get());
        Ok(Lowered {
            name: format!("{class}.{name}"),
            findex,
            sig,
            function,
            ctor_of,
        })
    }
}

/// A variable of the body: a local of the file, or a temporary the lowering made.
type Var = u32;
const TEMP: Var = 0x8000_0000;
/// `this`, which is a variable of a method like any local.
const THIS: Var = 0x4000_0000;

struct FnLower<'m, 'a> {
    m: &'m mut Module<'a>,
    tast: Tast<'a>,
    f: Function,
    /// The terminator of each block, until the block is finished.
    term: Vec<Option<Terminator>>,
    cur: Option<BlockId>,
    preds: Vec<Vec<BlockId>>,
    sealed: Vec<bool>,
    defs: Vec<HashMap<Var, ValueId>>,
    incomplete: Vec<Vec<(Var, ValueId)>>,
    phi_block: HashMap<ValueId, BlockId>,
    var_ty: HashMap<Var, TypeIx>,
    replace: HashMap<ValueId, ValueId>,
    /// `(where continue goes, where break goes)` for each open loop.
    loops: Vec<(BlockId, BlockId)>,
    temps: u32,
    ret_ty: TypeIx,
    func: u32,
    this_ty: Option<TypeIx>,
    /// The class whose method this is.
    this_class: Option<u32>,
}

impl<'m, 'a> FnLower<'m, 'a> {
    fn new(m: &'m mut Module<'a>, func: u32, this: Option<(TypeIx, u32)>) -> Result<Self> {
        let this_ty = this.map(|(t, _)| t);
        let this_class = this.map(|(_, c)| c);
        let tast = m.tast;
        let rec = tast.funcs()[func as usize];
        let ret_ty = m.types.to_type(rec.ret.get())?;
        let mut param_tys = Vec::new();
        param_tys.extend(this_ty);
        for p in tast.func_params(&rec) {
            let local = tast.locals()[p.local.get() as usize];
            param_tys.push(m.types.to_type(local.ty.get())?);
        }
        let f = Function::new(param_tys.iter().map(|t| TypeRef(*t as u32)).collect());
        let mut s = FnLower {
            m,
            tast,
            f,
            term: Vec::new(),
            cur: None,
            preds: Vec::new(),
            sealed: Vec::new(),
            defs: Vec::new(),
            incomplete: Vec::new(),
            phi_block: HashMap::new(),
            var_ty: HashMap::new(),
            replace: HashMap::new(),
            loops: Vec::new(),
            temps: 0,
            ret_ty,
            func,
            this_ty,
            this_class,
        };
        // block 0 holds the parameters and falls into the body
        let entry = s.new_block();
        s.seal(entry);
        s.cur = Some(entry);
        let offset = usize::from(this_ty.is_some());
        if let Some(t) = this_ty {
            let v = s.f.new_value(TypeRef(t as u32), 0);
            s.emit(Instr::Param { dst: v, reg: 0 });
            s.var_ty.insert(THIS, t);
            s.write_var(THIS, entry, v);
        }
        for (i, p) in tast.func_params(&rec).iter().enumerate() {
            let at = i + offset;
            let ty = param_tys[at];
            let v = s.f.new_value(TypeRef(ty as u32), at as u32);
            s.emit(Instr::Param {
                dst: v,
                reg: at as u32,
            });
            let local = p.local.get();
            s.var_ty.insert(local, ty);
            s.write_var(local, entry, v);
        }
        Ok(s)
    }

    /// A function that takes nothing and has no body in the file, which the lowering builds from
    /// pieces: see [`Module::lower_init`].
    fn bare(m: &'m mut Module<'a>) -> Self {
        let tast = m.tast;
        let ret_ty = m.types.table.add(HlType::Void);
        let mut s = FnLower {
            m,
            tast,
            f: Function::new(Vec::new()),
            term: Vec::new(),
            cur: None,
            preds: Vec::new(),
            sealed: Vec::new(),
            defs: Vec::new(),
            incomplete: Vec::new(),
            phi_block: HashMap::new(),
            var_ty: HashMap::new(),
            replace: HashMap::new(),
            loops: Vec::new(),
            temps: 0,
            ret_ty,
            func: NONE,
            this_ty: None,
            this_class: None,
        };
        let entry = s.new_block();
        s.seal(entry);
        s.cur = Some(entry);
        s
    }

    fn run(mut self) -> Result<Function> {
        let rec = self.tast.funcs()[self.func as usize];
        let body = rec.root.get();
        if body == NONE {
            return Err("the function has no body".into());
        }
        let first = self.new_block();
        self.jump(first);
        self.seal(first);
        self.cur = Some(first);
        let v = self.expr(body)?;
        if self.cur.is_some() {
            // falling off the end returns the default of the return type
            let ret = self.ret_ty;
            let value = match v {
                Some(v) if !self.is_void(ret) => self.coerce(v, ret)?,
                _ => self.default_value(ret)?,
            };
            self.terminate(Terminator::Ret { value });
        }
        self.finish()
    }

    // ---- blocks and values

    fn new_block(&mut self) -> BlockId {
        let id = BlockId(self.f.blocks.len() as u32);
        self.f.blocks.push(Block {
            phis: Vec::new(),
            instrs: Vec::new(),
            term: Terminator::Ret { value: ValueId(0) },
            handler: None,
        });
        self.term.push(None);
        self.preds.push(Vec::new());
        self.sealed.push(false);
        self.defs.push(HashMap::new());
        self.incomplete.push(Vec::new());
        id
    }

    fn emit(&mut self, i: Instr) {
        let b = self.cur.expect("emitting into a finished block");
        self.f.blocks[b.idx()].instrs.push(i);
    }

    fn value(&mut self, ty: TypeIx) -> ValueId {
        let t = TypeRef(ty as u32);
        let reg = self.f.new_reg(t);
        self.f.new_value(t, reg)
    }

    fn val_ty(&self, v: ValueId) -> TypeIx {
        self.f.value_ty(v).0 as usize
    }

    fn ty_of(&mut self, t: HlType) -> TypeIx {
        self.m.types.table.add(t)
    }

    fn is_void(&self, t: TypeIx) -> bool {
        self.m.types.table.types[t] == HlType::Void
    }

    fn terminate(&mut self, t: Terminator) {
        let b = self.cur.take().expect("terminating a finished block");
        let mut seen = HashSet::new();
        for s in t.successors() {
            if seen.insert(s) {
                self.preds[s.idx()].push(b);
            }
        }
        self.term[b.idx()] = Some(t);
    }

    fn jump(&mut self, target: BlockId) {
        if self.cur.is_some() {
            self.terminate(Terminator::Jump { target });
        }
    }

    fn seal(&mut self, b: BlockId) {
        let todo = std::mem::take(&mut self.incomplete[b.idx()]);
        self.sealed[b.idx()] = true;
        for (var, phi) in todo {
            self.add_phi_operands(var, phi, b);
        }
    }

    // ---- SSA construction

    fn write_var(&mut self, var: Var, b: BlockId, v: ValueId) {
        self.defs[b.idx()].insert(var, v);
    }

    fn resolve(&self, mut v: ValueId) -> ValueId {
        while let Some(r) = self.replace.get(&v) {
            v = *r;
        }
        v
    }

    fn read_var(&mut self, var: Var, b: BlockId) -> Result<ValueId> {
        if let Some(v) = self.defs[b.idx()].get(&var).copied() {
            return Ok(self.resolve(v));
        }
        let ty = *self
            .var_ty
            .get(&var)
            .ok_or_else(|| format!("a variable read before its declaration (local {var})"))?;
        let v = if !self.sealed[b.idx()] {
            let phi = self.new_phi(b, ty);
            self.incomplete[b.idx()].push((var, phi));
            phi
        } else if self.preds[b.idx()].len() == 1 {
            let p = self.preds[b.idx()][0];
            self.read_var(var, p)?
        } else if self.preds[b.idx()].is_empty() {
            return Err(format!("local {var} is read where nothing defines it"));
        } else {
            let phi = self.new_phi(b, ty);
            self.write_var(var, b, phi);
            self.add_phi_operands(var, phi, b)
        };
        self.write_var(var, b, v);
        Ok(v)
    }

    fn new_phi(&mut self, b: BlockId, ty: TypeIx) -> ValueId {
        let v = self.value(ty);
        self.f.blocks[b.idx()].phis.push(Phi {
            dst: v,
            incoming: Vec::new(),
        });
        self.phi_block.insert(v, b);
        v
    }

    fn add_phi_operands(&mut self, var: Var, phi: ValueId, b: BlockId) -> ValueId {
        let preds = self.preds[b.idx()].clone();
        for p in preds {
            let v = self.read_var(var, p).unwrap_or(phi);
            if let Some(slot) = self.f.blocks[b.idx()]
                .phis
                .iter_mut()
                .find(|x| x.dst == phi)
            {
                slot.incoming.push((p, v));
            }
        }
        self.remove_trivial_phi(phi)
    }

    /// A phi whose operands are all one value (or itself) is that value.
    fn remove_trivial_phi(&mut self, phi: ValueId) -> ValueId {
        let Some(b) = self.phi_block.get(&phi).copied() else {
            return phi;
        };
        let Some(entry) = self.f.blocks[b.idx()].phis.iter().find(|x| x.dst == phi) else {
            return self.resolve(phi);
        };
        let mut same: Option<ValueId> = None;
        for (_, op) in entry.incoming.clone() {
            let op = self.resolve(op);
            if Some(op) == same || op == phi {
                continue;
            }
            if same.is_some() {
                return phi;
            }
            same = Some(op);
        }
        let Some(same) = same else { return phi };
        self.replace.insert(phi, same);
        self.f.blocks[b.idx()].phis.retain(|x| x.dst != phi);
        // phis that used it may have become trivial
        let users: Vec<ValueId> = self
            .f
            .blocks
            .iter()
            .flat_map(|blk| blk.phis.iter())
            .filter(|p| p.incoming.iter().any(|(_, v)| self.resolve(*v) == same))
            .map(|p| p.dst)
            .collect();
        for u in users {
            if u != phi {
                self.remove_trivial_phi(u);
            }
        }
        self.resolve(same)
    }

    fn temp(&mut self, ty: TypeIx) -> Var {
        self.temps += 1;
        let var = TEMP | self.temps;
        self.var_ty.insert(var, ty);
        var
    }

    // ---- constants and conversions

    fn int_const(&mut self, v: i32) -> ValueId {
        let ty = self.ty_of(HlType::I32);
        let idx = self.m.int_index(v);
        let dst = self.value(ty);
        self.emit(Instr::Int { dst, idx });
        dst
    }

    fn float_const(&mut self, v: f64) -> ValueId {
        let ty = self.ty_of(HlType::F64);
        let idx = self.m.float_index(v);
        let dst = self.value(ty);
        self.emit(Instr::Float { dst, idx });
        dst
    }

    fn bool_const(&mut self, value: bool) -> ValueId {
        let ty = self.ty_of(HlType::Bool);
        let dst = self.value(ty);
        self.emit(Instr::Bool { dst, value });
        dst
    }

    fn void_value(&mut self) -> ValueId {
        let ty = self.ty_of(HlType::Void);
        let dst = self.value(ty);
        self.emit(Instr::Null { dst });
        dst
    }

    /// What a variable holds before anything is assigned to it.
    fn default_value(&mut self, ty: TypeIx) -> Result<ValueId> {
        match self.m.types.table.types[ty].clone() {
            HlType::Void => Ok(self.void_value()),
            HlType::I32 => Ok(self.int_const(0)),
            HlType::F64 => Ok(self.float_const(0.0)),
            HlType::Bool => Ok(self.bool_const(false)),
            _ if self.m.types.table.is_nullable(ty) => {
                let dst = self.value(ty);
                self.emit(Instr::Null { dst });
                Ok(dst)
            }
            other => Err(format!("no default value for {other:?} yet")),
        }
    }

    fn coerce(&mut self, v: ValueId, to: TypeIx) -> Result<ValueId> {
        let from = self.val_ty(v);
        if from == to {
            return Ok(v);
        }
        if self.m.types.is_subclass_type(from, to) {
            let dst = self.value(to);
            self.emit(Instr::Cast {
                kind: CastKind::UnsafeCast,
                dst,
                src: v,
            });
            return Ok(dst);
        }
        let (a, b) = (
            self.m.types.table.types[from].clone(),
            self.m.types.table.types[to].clone(),
        );
        // an object is its own Dynamic: the runtime boxes values, never objects
        if matches!(b, HlType::Dyn)
            && matches!(a, HlType::Obj(_) | HlType::Struct(_) | HlType::Virtual(_) | HlType::Array)
        {
            let dst = self.value(to);
            self.emit(Instr::Cast {
                kind: CastKind::UnsafeCast,
                dst,
                src: v,
            });
            return Ok(dst);
        }
        let kind = match (a, b) {
            (HlType::I32, HlType::F64) => CastKind::ToSFloat,
            (HlType::F64, HlType::I32) => CastKind::ToInt,
            (HlType::I32 | HlType::F64 | HlType::Bool, HlType::Dyn) => CastKind::ToDyn,
            (a, b) => return Err(format!("no conversion from {a:?} to {b:?} yet")),
        };
        let dst = self.value(to);
        self.emit(Instr::Cast { kind, dst, src: v });
        Ok(dst)
    }

    fn node_ty(&mut self, n: &NodeRec) -> Result<TypeIx> {
        if n.ty.get() == NONE {
            return Err("a node without a type".into());
        }
        self.m.types.to_type(n.ty.get())
    }

    // ---- expressions

    fn kid(&self, n: &NodeRec, i: usize) -> u32 {
        self.tast.kid_ids(n).start + i as u32
    }

    fn kids(&self, n: &NodeRec) -> Vec<u32> {
        self.tast.kid_ids(n).collect()
    }

    /// Lowers a node. The value is `None` for a statement, and when control cannot reach it.
    fn expr(&mut self, id: u32) -> Result<Option<ValueId>> {
        if self.cur.is_none() {
            return Ok(None);
        }
        let n = *self.tast.node(id);
        match n.kind {
            node_kind::CONST if n.flags == const_kind::THIS => {
                if self.this_ty.is_none() {
                    return Err("`this` outside a method".into());
                }
                let b = self.cur.unwrap();
                self.read_var(THIS, b).map(Some)
            }
            node_kind::CONST => self.constant(&n).map(Some),
            node_kind::FIELD => self.field_get(&n),
            node_kind::ARRAY => self.array_get(&n),
            node_kind::ARRAY_DECL => self.array_literal(&n),
            node_kind::NEW => self.new_object(&n),
            node_kind::LOCAL => {
                let local = n.a.get();
                let b = self.cur.unwrap();
                self.ensure_var(local)?;
                self.read_var(local, b).map(Some)
            }
            node_kind::PARENTHESIS | node_kind::META => self.expr(self.kid(&n, 0)),
            node_kind::BLOCK => {
                let mut last = None;
                for k in self.kids(&n) {
                    last = self.expr(k)?;
                }
                Ok(last)
            }
            node_kind::VAR => self.var_decl(&n).map(|_| None),
            node_kind::BINOP => self.binop(id, &n),
            node_kind::UNOP => self.unop(&n),
            node_kind::IF => self.if_expr(&n),
            node_kind::WHILE => self.while_loop(&n).map(|_| None),
            node_kind::RETURN => {
                let value = if n.flags & node_flag::HAS_VALUE != 0 {
                    match self.expr(self.kid(&n, 0))? {
                        Some(v) => {
                            let ret = self.ret_ty;
                            self.coerce(v, ret)?
                        }
                        None => return Ok(None),
                    }
                } else {
                    self.void_value()
                };
                self.terminate(Terminator::Ret { value });
                Ok(None)
            }
            node_kind::BREAK => {
                let (_, target) = *self.loops.last().ok_or("break outside a loop")?;
                self.jump(target);
                Ok(None)
            }
            node_kind::CONTINUE => {
                let (target, _) = *self.loops.last().ok_or("continue outside a loop")?;
                self.jump(target);
                Ok(None)
            }
            node_kind::CALL => self.call(&n),
            node_kind::THROW => self.throw(&n),
            node_kind::CAST if n.flags & node_flag::HAS_TYPE == 0 => {
                let v = self.expr(self.kid(&n, 0))?;
                let to = self.node_ty(&n)?;
                match v {
                    Some(v) => self.coerce(v, to).map(Some),
                    None => Ok(None),
                }
            }
            other => Err(format!(
                "node kind {} is not lowered yet",
                NODE_KINDS
                    .iter()
                    .find(|e| e.1 == other)
                    .map_or("?", |e| e.0)
            )),
        }
    }

    /// A local used before the walk has seen it declared is a parameter or a declaration.
    fn ensure_var(&mut self, local: Var) -> Result<()> {
        if self.var_ty.contains_key(&local) {
            return Ok(());
        }
        let ty = self.tast.locals()[local as usize].ty.get();
        let ty = self.m.types.to_type(ty)?;
        self.var_ty.insert(local, ty);
        Ok(())
    }

    fn constant(&mut self, n: &NodeRec) -> Result<ValueId> {
        match n.flags {
            const_kind::INT => Ok(self.int_const(n.a.get() as i32)),
            const_kind::FLOAT => {
                let text = self.tast.string(n.a.get());
                let v: f64 = text
                    .parse()
                    .map_err(|_| format!("the float constant {text:?}"))?;
                Ok(self.float_const(v))
            }
            const_kind::BOOL => Ok(self.bool_const(n.a.get() != 0)),
            const_kind::NULL => self.null_value(n),
            const_kind::STRING => self.string_constant(n),
            other => Err(format!("constant kind {other} is not lowered yet")),
        }
    }

    fn var_decl(&mut self, n: &NodeRec) -> Result<()> {
        let local = n.a.get();
        self.ensure_var(local)?;
        let ty = self.var_ty[&local];
        let v = if n.flags & node_flag::HAS_INIT != 0 {
            match self.expr(self.kid(n, 0))? {
                Some(v) => self.coerce(v, ty)?,
                None => return Ok(()),
            }
        } else {
            self.default_value(ty)?
        };
        let b = self.cur.ok_or("a declaration in unreachable code")?;
        self.write_var(local, b, v);
        Ok(())
    }

    fn call(&mut self, n: &NodeRec) -> Result<Option<ValueId>> {
        let ids = self.kids(n);
        let callee = *self.tast.node(ids[0]);
        if callee.kind == node_kind::CONST && callee.flags == const_kind::SUPER {
            return self.super_constructor(&ids[1..]);
        }
        if callee.kind == node_kind::FIELD && callee.a.get() == field_access::INSTANCE {
            return self.method_call(&ids, &callee);
        }
        if callee.kind == node_kind::IDENT {
            let name = self.tast.string(callee.a.get());
            if name.starts_with('$') {
                return self.intrinsic(name, &ids[1..], n.ty.get());
            }
        }
        if callee.kind != node_kind::FIELD || callee.a.get() != field_access::STATIC {
            let kind = NODE_KINDS
                .iter()
                .find(|e| e.1 == callee.kind)
                .map_or("?", |e| e.0);
            let how = if callee.kind == node_kind::FIELD {
                FIELD_ACCESSES
                    .iter()
                    .find(|e| e.1 == callee.a.get())
                    .map_or("?", |e| e.0)
            } else if callee.kind == node_kind::IDENT {
                self.tast.string(callee.a.get())
            } else {
                ""
            };
            return Err(format!("a call of a {kind} {how} is not lowered yet"));
        }
        let field = callee.b.get();
        if field == NONE {
            return Err("a call of a field the file does not name".into());
        }
        let rec = self.tast.fields()[field as usize];
        let class = self.m.types.class_name(rec.owner.get()).to_owned();
        let name = self.m.types.field_name(field).to_owned();
        let fun = self.m.types.fun_id(&class, &name) as usize;
        self.m.called.insert(fun as u32);
        let sig = self.m.types.to_type(rec.ty.get())?;
        if let Some((lib, symbol)) = self.m.types.native_of(field) {
            if let HlType::Fun(args, ret) = self.m.types.table.types[sig].clone() {
                let args = args.iter().map(|a| TypeRef(*a as u32)).collect();
                let import = air::v2::module::NativeImport::new(
                    fun,
                    lib.clone(),
                    symbol.clone(),
                    args,
                    TypeRef(ret as u32),
                );
                let _ = self.f.natives.declare(import);
            }
            self.m.add_native(lib, symbol, sig, fun as u32);
        } else if rec.func.get() != NONE {
            self.m.request(field);
        } else {
            return Err(format!("{class}.{name} has no body to lower"));
        }
        let HlType::Fun(params, ret) = self.m.types.table.types[sig].clone() else {
            return Err(format!("{class}.{name} is not a function"));
        };
        if params.len() != ids.len() - 1 {
            return Err(format!(
                "{class}.{name}: {} arguments for {} parameters",
                ids.len() - 1,
                params.len()
            ));
        }
        let mut args = Vec::new();
        for (k, p) in ids[1..].iter().zip(&params) {
            let Some(v) = self.expr(*k)? else {
                return Ok(None);
            };
            args.push(self.coerce(v, *p)?);
        }
        let dst = self.value(ret);
        self.emit(Instr::Call { dst, fun, args });
        Ok((!self.is_void(ret)).then_some(dst))
    }

    // ---- operators

    fn binop(&mut self, id: u32, n: &NodeRec) -> Result<Option<ValueId>> {
        let op = BINOPS[n.a.get() as usize];
        let ids = self.kids(n);
        match op {
            "OpAssign" => self.assign(ids[0], ids[1], None),
            "OpAssignOp" => {
                let inner = BINOPS[n.b.get() as usize];
                self.assign(ids[0], ids[1], Some(inner))
            }
            "OpEq" | "OpNotEq" | "OpGt" | "OpGte" | "OpLt" | "OpLte" | "OpBoolAnd" | "OpBoolOr" => {
                self.bool_value(id)
            }
            _ => {
                let ty = self.node_ty(n)?;
                let (Some(a), Some(b)) = (self.expr(ids[0])?, self.expr(ids[1])?) else {
                    return Ok(None);
                };
                self.arith(op, a, b, ty).map(Some)
            }
        }
    }

    fn arith(&mut self, op: &str, a: ValueId, b: ValueId, ty: TypeIx) -> Result<ValueId> {
        if op == "OpAdd" && self.is_string(ty) {
            return self.concat(a, b);
        }
        let (a, b) = (self.coerce(a, ty)?, self.coerce(b, ty)?);
        let kind = match op {
            "OpAdd" => BinOp::Add,
            "OpSub" => BinOp::Sub,
            "OpMult" => BinOp::Mul,
            "OpDiv" => BinOp::SDiv,
            "OpMod" => BinOp::SMod,
            "OpShl" => BinOp::Shl,
            "OpShr" => BinOp::SShr,
            "OpUShr" => BinOp::UShr,
            "OpAnd" => BinOp::And,
            "OpOr" => BinOp::Or,
            "OpXor" => BinOp::Xor,
            other => return Err(format!("operator {other} is not lowered yet")),
        };
        let numeric = matches!(self.m.types.table.types[ty], HlType::I32 | HlType::F64);
        if !numeric {
            return Err(format!(
                "arithmetic on {:?} is not lowered yet",
                self.m.types.table.types[ty]
            ));
        }
        let dst = self.value(ty);
        self.emit(Instr::BinOp {
            op: kind,
            dst,
            a,
            b,
        });
        Ok(dst)
    }

    fn assign(&mut self, target: u32, value: u32, op: Option<&str>) -> Result<Option<ValueId>> {
        let t = *self.tast.node(target);
        if t.kind == node_kind::FIELD {
            return self.assign_field(&t, value, op);
        }
        if t.kind == node_kind::ARRAY {
            return self.assign_element(&t, value, op);
        }
        if t.kind != node_kind::LOCAL {
            return Err(
                "only assignment to a local, an instance variable or an array element is lowered yet"
                    .into(),
            );
        }
        let local = t.a.get();
        self.ensure_var(local)?;
        let ty = self.var_ty[&local];
        let Some(rhs) = self.expr(value)? else {
            return Ok(None);
        };
        let b = self.cur.ok_or("an assignment in unreachable code")?;
        let new = match op {
            None => self.coerce(rhs, ty)?,
            Some(op) => {
                let old = self.read_var(local, b)?;
                self.arith(op, old, rhs, ty)?
            }
        };
        self.write_var(local, b, new);
        Ok(Some(new))
    }

    fn unop(&mut self, n: &NodeRec) -> Result<Option<ValueId>> {
        let op = UNOPS[n.a.get() as usize];
        let operand = self.kid(n, 0);
        match op {
            "OpNot" => {
                let Some(v) = self.expr(operand)? else {
                    return Ok(None);
                };
                let ty = self.val_ty(v);
                let dst = self.value(ty);
                self.emit(Instr::UnOp {
                    op: UnOp::Not,
                    dst,
                    src: v,
                });
                Ok(Some(dst))
            }
            "OpNeg" => {
                let Some(v) = self.expr(operand)? else {
                    return Ok(None);
                };
                let ty = self.val_ty(v);
                let dst = self.value(ty);
                self.emit(Instr::UnOp {
                    op: UnOp::Neg,
                    dst,
                    src: v,
                });
                Ok(Some(dst))
            }
            "OpNegBits" => {
                let Some(v) = self.expr(operand)? else {
                    return Ok(None);
                };
                let ones = self.int_const(-1);
                self.arith("OpXor", v, ones, self.val_ty(v)).map(Some)
            }
            "OpIncrement" | "OpDecrement" => {
                let t = *self.tast.node(operand);
                let postfix = n.flags & node_flag::POSTFIX != 0;
                if t.kind == node_kind::FIELD {
                    return self.step_field(&t, op == "OpIncrement", postfix);
                }
                if t.kind != node_kind::LOCAL {
                    return Err("++ and -- on anything but a local or a field are not lowered yet".into());
                }
                let local = t.a.get();
                self.ensure_var(local)?;
                let b = self.cur.ok_or("an increment in unreachable code")?;
                let old = self.read_var(local, b)?;
                let new = self.stepped(old, op == "OpIncrement")?;
                self.write_var(local, b, new);
                Ok(Some(if postfix { old } else { new }))
            }
            other => Err(format!("operator {other} is not lowered yet")),
        }
    }

    /// `old` plus one, or minus one.
    fn stepped(&mut self, old: ValueId, up: bool) -> Result<ValueId> {
        let ty = self.val_ty(old);
        if self.m.types.table.types[ty] == HlType::I32 {
            let dst = self.value(ty);
            self.emit(Instr::UnOp {
                op: if up { UnOp::Incr } else { UnOp::Decr },
                dst,
                src: old,
            });
            return Ok(dst);
        }
        let one = self.float_const(1.0);
        self.arith(if up { "OpAdd" } else { "OpSub" }, old, one, ty)
    }

    /// A comparison or `&&`/`||` used as a value: branch, then merge a constant.
    fn bool_value(&mut self, id: u32) -> Result<Option<ValueId>> {
        let bool_ty = self.ty_of(HlType::Bool);
        let tmp = self.temp(bool_ty);
        let (yes, no, join) = (self.new_block(), self.new_block(), self.new_block());
        self.cond(id, yes, no)?;
        self.seal(yes);
        self.seal(no);
        self.cur = Some(yes);
        let t = self.bool_const(true);
        self.write_var(tmp, yes, t);
        self.jump(join);
        self.cur = Some(no);
        let f = self.bool_const(false);
        self.write_var(tmp, no, f);
        self.jump(join);
        self.seal(join);
        self.cur = Some(join);
        self.read_var(tmp, join).map(Some)
    }

    /// Branches to `yes` or `no` on the truth of a condition.
    fn cond(&mut self, id: u32, yes: BlockId, no: BlockId) -> Result<()> {
        if self.cur.is_none() {
            return Ok(());
        }
        let n = *self.tast.node(id);
        match n.kind {
            node_kind::PARENTHESIS => return self.cond(self.kid(&n, 0), yes, no),
            node_kind::UNOP if UNOPS[n.a.get() as usize] == "OpNot" => {
                return self.cond(self.kid(&n, 0), no, yes);
            }
            node_kind::CONST if n.flags == const_kind::BOOL => {
                self.jump(if n.a.get() != 0 { yes } else { no });
                return Ok(());
            }
            node_kind::BINOP => {
                let op = BINOPS[n.a.get() as usize];
                let ids = self.kids(&n);
                if matches!(op, "OpEq" | "OpNotEq")
                    && let Some(operand) = self.null_operand(&n)
                {
                    return self.null_test(operand, op == "OpEq", yes, no);
                }
                match op {
                    "OpBoolAnd" | "OpBoolOr" => {
                        let mid = self.new_block();
                        if op == "OpBoolAnd" {
                            self.cond(ids[0], mid, no)?;
                        } else {
                            self.cond(ids[0], yes, mid)?;
                        }
                        self.seal(mid);
                        self.cur = Some(mid);
                        return self.cond(ids[1], yes, no);
                    }
                    "OpEq" | "OpNotEq" | "OpGt" | "OpGte" | "OpLt" | "OpLte" => {
                        let (Some(a), Some(b)) = (self.expr(ids[0])?, self.expr(ids[1])?) else {
                            return Ok(());
                        };
                        let (a, b) = self.common(a, b)?;
                        // genhl compares unsigned when both sides are `UInt` or integer constants;
                        // the unsigned conditions are `<` and `>=`, so the others swap operands
                        let unsigned = self.is_unsigned(ids[0]) && self.is_unsigned(ids[1]);
                        let (cond, a, b) = match (op, unsigned) {
                            ("OpEq", _) => (CondKind::Eq, a, b),
                            ("OpNotEq", _) => (CondKind::NotEq, a, b),
                            ("OpGt", false) => (CondKind::SGt, a, b),
                            ("OpGte", false) => (CondKind::SGte, a, b),
                            ("OpLt", false) => (CondKind::SLt, a, b),
                            (_, false) => (CondKind::SLte, a, b),
                            ("OpGt", true) => (CondKind::ULt, b, a),
                            ("OpGte", true) => (CondKind::UGte, a, b),
                            ("OpLt", true) => (CondKind::ULt, a, b),
                            (_, true) => (CondKind::UGte, b, a),
                        };
                        self.terminate(Terminator::CondJump {
                            cond,
                            a,
                            b: Some(b),
                            if_true: yes,
                            if_false: no,
                        });
                        return Ok(());
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        let Some(v) = self.expr(id)? else {
            return Ok(());
        };
        self.terminate(Terminator::CondJump {
            cond: CondKind::True,
            a: v,
            b: None,
            if_true: yes,
            if_false: no,
        });
        Ok(())
    }

    /// Both operands at one type: a float if either is.
    fn common(&mut self, a: ValueId, b: ValueId) -> Result<(ValueId, ValueId)> {
        let (ta, tb) = (self.val_ty(a), self.val_ty(b));
        if ta == tb {
            return Ok((a, b));
        }
        let float = self.ty_of(HlType::F64);
        Ok((self.coerce(a, float)?, self.coerce(b, float)?))
    }

    // ---- control flow

    fn if_expr(&mut self, n: &NodeRec) -> Result<Option<ValueId>> {
        let ids = self.kids(n);
        let has_else = n.flags & node_flag::HAS_ELSE != 0;
        let ty = self.node_ty(n)?;
        let value_wanted = has_else && !self.is_void(ty);
        let tmp = value_wanted.then(|| self.temp(ty));
        let (then_b, else_b, join) = (self.new_block(), self.new_block(), self.new_block());
        self.cond(ids[0], then_b, if has_else { else_b } else { join })?;
        self.seal(then_b);
        if has_else {
            self.seal(else_b);
        }
        for (block, arm) in [(then_b, Some(ids[1])), (else_b, has_else.then(|| ids[2]))] {
            let Some(arm) = arm else { continue };
            if self.preds[block.idx()].is_empty() {
                continue;
            }
            self.cur = Some(block);
            let v = self.expr(arm)?;
            if let (Some(tmp), Some(v), Some(cur)) = (tmp, v, self.cur) {
                let v = self.coerce(v, ty)?;
                self.write_var(tmp, cur, v);
            }
            self.jump(join);
        }
        self.seal(join);
        self.cur = (!self.preds[join.idx()].is_empty()).then_some(join);
        match (tmp, self.cur) {
            (Some(tmp), Some(join)) => self.read_var(tmp, join).map(Some),
            _ => Ok(None),
        }
    }

    fn while_loop(&mut self, n: &NodeRec) -> Result<()> {
        let ids = self.kids(n);
        let do_while = n.flags & node_flag::DO_WHILE != 0;
        let (header, body, exit) = (self.new_block(), self.new_block(), self.new_block());
        // a do-while runs the body first; `continue` goes to the test either way
        let test = if do_while { self.new_block() } else { header };
        self.jump(if do_while { body } else { header });
        if do_while {
            // the body is the loop's header: its back edge comes from the test
            self.loops.push((test, exit));
            self.cur = Some(body);
            self.expr(ids[1])?;
            self.jump(test);
            self.seal(test);
            self.cur = (!self.preds[test.idx()].is_empty()).then_some(test);
            self.cond(ids[0], body, exit)?;
            self.seal(body);
        } else {
            self.cur = Some(header);
            self.loops.push((header, exit));
            self.cond(ids[0], body, exit)?;
            self.seal(body);
            self.cur = Some(body);
            self.expr(ids[1])?;
            self.jump(header);
            self.seal(header);
        }
        self.loops.pop();
        self.seal(exit);
        self.cur = (!self.preds[exit.idx()].is_empty()).then_some(exit);
        Ok(())
    }

    // ---- the finished function

    /// Values made for phis that were then removed have no definition; the table keeps only what
    /// the code defines, renumbered.
    fn drop_undefined_values(&mut self) {
        let n = self.f.values.len();
        let mut defined = vec![false; n];
        for blk in &self.f.blocks {
            for phi in &blk.phis {
                defined[phi.dst.idx()] = true;
            }
            for ins in &blk.instrs {
                if let Some(d) = ins.dst() {
                    defined[d.idx()] = true;
                }
            }
        }
        let mut map = vec![ValueId(u32::MAX); n];
        let mut values = Vec::new();
        for (i, d) in defined.iter().enumerate() {
            if *d {
                map[i] = ValueId(values.len() as u32);
                values.push(self.f.values[i].clone());
            }
        }
        for blk in &mut self.f.blocks {
            for phi in &mut blk.phis {
                phi.dst = map[phi.dst.idx()];
                for (_, v) in &mut phi.incoming {
                    *v = map[v.idx()];
                }
            }
            for ins in &mut blk.instrs {
                ins.map_uses(&mut |v| map[v.idx()]);
                ins.map_dst(&mut |v| map[v.idx()]);
            }
            blk.term.map_uses(&mut |v| map[v.idx()]);
        }
        self.f.values = values;
    }

    fn finish(mut self) -> Result<Function> {
        let n = self.f.blocks.len();
        // blocks nothing reaches are dropped
        let mut reach = vec![false; n];
        let mut stack = vec![0usize];
        while let Some(b) = stack.pop() {
            if std::mem::replace(&mut reach[b], true) {
                continue;
            }
            match &self.term[b] {
                Some(t) => stack.extend(t.successors().iter().map(|s| s.idx())),
                None => return Err(format!("block b{b} was never finished")),
            }
        }
        let mut renumber = vec![u32::MAX; n];
        let mut next = 0u32;
        for b in 0..n {
            if reach[b] {
                renumber[b] = next;
                next += 1;
            }
        }
        let replace = std::mem::take(&mut self.replace);
        let resolve = |mut v: ValueId| {
            while let Some(r) = replace.get(&v) {
                v = *r;
            }
            v
        };
        let old = std::mem::take(&mut self.f.blocks);
        let mut blocks = Vec::new();
        for (i, mut blk) in old.into_iter().enumerate() {
            if !reach[i] {
                continue;
            }
            let mut term = self.term[i].take().unwrap();
            term.map_targets(&mut |t| BlockId(renumber[t.idx()]));
            term.map_uses(&mut |v| resolve(v));
            for phi in &mut blk.phis {
                phi.incoming.retain(|(p, _)| reach[p.idx()]);
                for (p, v) in &mut phi.incoming {
                    *p = BlockId(renumber[p.idx()]);
                    *v = resolve(*v);
                }
            }
            for ins in &mut blk.instrs {
                ins.map_uses(&mut |v| resolve(v));
            }
            blk.term = term;
            blocks.push(blk);
        }
        self.f.blocks = blocks;
        self.drop_undefined_values();
        self.f.int_pool_base = self.m.ints.len();
        let mut floats: Vec<TypeRef> = Vec::new();
        for t in [HlType::F32, HlType::F64] {
            if let Some(ix) = self.m.types.table.lookup(&t) {
                floats.push(TypeRef(ix as u32));
            }
        }
        floats.sort_by_key(|t| t.0);
        self.f.float_types = floats;
        Ok(self.f)
    }
}
