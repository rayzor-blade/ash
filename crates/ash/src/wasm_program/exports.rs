//! The C functions a host calls program members by; see `host_export`. Each
//! takes one 64-bit word per argument, casts them into the member's
//! parameters, runs the member, and casts the result back to a word. A member
//! that throws does not unwind into the caller: the exception goes to the
//! host's `raise` and the function answers `unit`.
use anyhow::{Result, anyhow, bail};
use ash_wasm_emit::object::{Linkage, Sym};
use ash_wasm_emit::wasm_encoder::{BlockType, Instruction as I, ValType};

use super::casts::{Class, Scratch, word_from, word_to};
use super::{F, I32, Image, mem};
use crate::bytecode::DecodedBytecode;
use crate::hl_bindings as hl;
use crate::host_export::ResolvedExport;

const I64: ValType = ValType::I64;

/// What stands between a host's call and the member.
#[derive(Clone, Copy)]
pub(super) enum Guard {
    /// Nothing: the member and its casts cannot throw.
    None,
    /// A trap, so a throw reaches `raise`.
    Trap,
    /// A test of the receiver: only a null one can make the member throw.
    NullReceiver,
}

/// Choose each export's guard from what its member can raise.
pub(super) fn guard_exports(bc: &DecodedBytecode, exports: Vec<ResolvedExport>) -> Vec<(ResolvedExport, Guard)> {
    let module = crate::air_pipeline::AshModule::new(bc);
    exports
        .into_iter()
        .map(|e| {
            let guard = if !e.export.casts_cannot_throw() {
                Guard::Trap
            } else if module.frameless(e.findex) {
                Guard::None
            } else if e.fast_findex.is_some_and(|fast| module.frameless(fast)) {
                Guard::NullReceiver
            } else {
                Guard::Trap
            };
            (e, guard)
        })
        .collect()
}

impl Image<'_> {
    /// Define every export's function. Runs after the bodies are emitted:
    /// each calls the function its export resolved to.
    pub(super) fn export_wrappers(&mut self) -> Result<()> {
        let exports = std::mem::take(&mut self.exports);
        for (e, guard) in &exports {
            let symbol = e.export.symbol.as_str();
            if self.obj.lookup(symbol).is_some() {
                bail!("export `{symbol}`: the symbol is already defined");
            }
            match guard {
                Guard::None => {
                    self.export_body(e, e.findex, symbol, Linkage::Default, None)?;
                }
                Guard::Trap => {
                    let body = self.export_body(e, e.findex, &format!("ash_export_body_{symbol}"), Linkage::Local, None)?;
                    self.export_trapped(e, body, symbol, Linkage::Default);
                }
                // A null receiver takes the trapped path, where the member's
                // own null check raises; any other runs the body bare.
                Guard::NullReceiver => {
                    let body = self.export_body(e, e.findex, &format!("ash_export_body_{symbol}"), Linkage::Local, None)?;
                    let trapped = self.export_trapped(e, body, &format!("ash_export_trapped_{symbol}"), Linkage::Local);
                    let fast = e
                        .fast_findex
                        .ok_or_else(|| anyhow!("export `{symbol}`: the checked export has no fast stub"))?;
                    self.export_body(e, fast, symbol, Linkage::Default, Some(trapped))?;
                }
            }
        }
        self.exports = exports;
        Ok(())
    }

    /// Words in, word out: the casts around one call of the member as the
    /// function `name`. With `null_exit`, a null receiver is answered by
    /// calling it with the same words instead.
    fn export_body(
        &mut self,
        e: &ResolvedExport,
        findex: usize,
        name: &str,
        linkage: Linkage,
        null_exit: Option<Sym>,
    ) -> Result<Sym> {
        let x = &e.export;
        let n = e.params.len() as u32;
        let target = self
            .obj
            .lookup(&format!("Fun_{findex}"))
            .ok_or_else(|| anyhow!("export `{}`: the member's function f{findex} was not emitted", x.symbol))?;
        let kinds: Vec<hl::hl_type_kind> = e.params.iter().map(|&t| self.bc.types[t].kind).collect();
        let ret_kind = self.bc.types[e.ret].kind;

        let sym = self.obj.declare_function(name, vec![I64; n as usize], vec![I64], linkage);
        // The converted arguments follow the words, then the scratch locals.
        let mut locals: Vec<(u32, ValType)> = kinds.iter().map(|&k| (1, Class::of_kind(k).wasm())).collect();
        locals.extend(Scratch::locals(I32));
        let mut f = F::new(&locals);
        let scratch = Scratch::after(2 * n);

        for i in 0..e.params.len() {
            f.get(i as u32);
            let to = Class::of_kind(kinds[i]);
            match &x.arg_casts[i] {
                Some(cast) => self.host_cast(&mut f, Some(cast), Class::Long, to, e.params[i], &scratch, &x.symbol)?,
                None => word_to(&mut f, to),
            }
            f.set(n + i as u32);
        }
        if let Some(exit) = null_exit {
            if kinds.first().map(|&k| Class::of_kind(k)) != Some(Class::Pointer) {
                bail!("export `{}`: the receiver is not an object", x.symbol);
            }
            f.get(n).i(I::I32Eqz).i(I::If(BlockType::Empty));
            for i in 0..n {
                f.get(i);
            }
            f.call(exit).i(I::Return).i(I::End);
        }
        for i in 0..n {
            f.get(n + i);
        }
        f.call(target);
        let returns_value = ret_kind != hl::hl_type_kind_HVOID;
        if !returns_value {
            f.i(I::I64Const(x.unit as i64));
        } else {
            match &x.ret_cast {
                Some(cast) => self.host_cast(
                    &mut f,
                    Some(cast),
                    Class::of_kind(ret_kind),
                    Class::Long,
                    e.ret,
                    &scratch,
                    &x.symbol,
                )?,
                None => word_from(&mut f, Class::of_kind(ret_kind), ret_kind),
            }
        }
        self.obj.set_body(sym, f.finish());
        Ok(sym)
    }

    /// `name`, the body inside a trap, so a throw reaches `raise` rather than
    /// the caller. The trap is an exception handler (see `wasm_traps`), as
    /// the program's own entry point's is.
    fn export_trapped(&mut self, e: &ResolvedExport, body: Sym, name: &str, linkage: Linkage) -> Sym {
        let x = &e.export;
        let n = e.params.len() as u32;
        let setup = self.imp("hlp_setup_trap_jit", &[], &[I32]);
        let remove = self.imp("hlp_remove_trap_jit", &[], &[]);
        let get_exc = self.imp("hlp_get_exc_value", &[], &[I32]);
        let clear = self.imp("hlp_clear_exc_value", &[], &[]);
        let jump = self.imp("__wasm_longjmp", &[I32, I32], &[]);
        let raise = self.imp(&x.raise, &[I32], &[]);

        let sym = self.obj.declare_function(name, vec![I64; n as usize], vec![I64], linkage);
        // The stack pointer on entry, the trap's buffer, what was thrown, the
        // buffer it was copied from, and the exception.
        let (sp, buf, thrown, from, exc) = (n, n + 1, n + 2, n + 3, n + 4);
        let mut f = F::new(&[(5, I32)]);
        f.body.stack_pointer_get();
        f.set(sp);
        f.call(setup).set(buf);
        f.get(buf).get(buf).i(I::I32Store(mem(0, 2)));
        f.get(buf).c(-1).i(I::I32Store(mem(4, 2)));
        f.i(I::Block(BlockType::Result(I32)));
        f.body.try_long_jump(0);
        for i in 0..n {
            f.get(i);
        }
        f.call(body).call(remove).i(I::Return);
        f.i(I::End);
        f.i(I::Unreachable);
        f.i(I::End);
        f.set(thrown);
        f.get(sp);
        f.body.stack_pointer_set();
        f.get(thrown).i(I::I32Load(mem(0, 2))).set(from);
        f.get(from).i(I::I32Load(mem(0, 2))).get(buf).i(I::I32Eq);
        f.i(I::If(BlockType::Empty));
        f.call(get_exc).set(exc);
        f.call(clear);
        f.get(exc).call(raise);
        f.i(I::I64Const(x.unit as i64)).i(I::Return);
        f.i(I::Else);
        // Not ours: on to the trap outside.
        f.get(from).get(thrown).i(I::I32Load(mem(4, 2))).call(jump);
        f.i(I::Unreachable);
        f.i(I::End);
        f.i(I::Unreachable);
        self.obj.set_body(sym, f.finish());
        sym
    }
}
