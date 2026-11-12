//! Function values a host supplies as words of its own; see `HostClosure`.
//! The cast `ash:closure` makes a word a closure of the program's function
//! type; calling it runs an adapter of that type, which hands the host's
//! callee the word and the arguments as words.
use anyhow::{Result, anyhow, bail};
use ash_wasm_emit::object::{Linkage, Sym};
use ash_wasm_emit::wasm_encoder::{Instruction as I, ValType};

use super::casts::{Class, Scratch, word_from, word_to};
use super::{F, I32, Image};
use crate::hl_bindings as hl;

const I64: ValType = ValType::I64;

impl Image<'_> {
    /// The word on the stack as a closure of the program's function type
    /// `ty`: bound to what the type's `hold` makes of the word, calling the
    /// type's adapter. Allocated as the full type, so a dynamic call works as
    /// it does on any bound closure.
    pub(super) fn host_closure_value(
        &mut self,
        f: &mut F,
        from: Class,
        ty: usize,
        scratch: &Scratch,
        name: &str,
    ) -> Result<()> {
        let index = self
            .closures
            .iter()
            .position(|c| c.matches(self.bc, ty))
            .ok_or_else(|| anyhow!("{name}: ash:closure has no HostClosure for the program's type {ty}"))?;
        match from {
            Class::Long => {}
            Class::Bool | Class::Int => {
                f.i(I::I64ExtendI32U);
            }
            other => bail!("{name}: ash:closure takes a word, not {other:?}"),
        }
        let (hold, full) = {
            let c = &self.closures[index];
            (c.closure.hold.clone(), c.full)
        };
        let adapter = self.host_closure_adapter(index)?;
        let hold = self.imp(&hold, &[I64], &[I32]);
        let alloc = self.imp("hlp_alloc_closure_ptr", &[I32, I32, I32], &[I32]);
        f.call(hold).set(scratch.target);
        f.addr(self.types[full], 0);
        f.body.function_pointer(adapter);
        f.get(scratch.target).call(alloc);
        Ok(())
    }

    /// The function every closure of host closure type `index` calls: the
    /// signature compiled code calls a bound closure of that type with, the
    /// bound value first.
    fn host_closure_adapter(&mut self, index: usize) -> Result<Sym> {
        let name = format!("ash_host_closure_{index}");
        if let Some(existing) = self.obj.lookup(&name) {
            return Ok(existing);
        }
        let c = self.closures[index].closure.clone();
        let fun_ty = self.closures[index].fun;
        let fun = self.bc.types[fun_ty]
            .fun
            .clone()
            .ok_or_else(|| anyhow!("host closure type {fun_ty} is not a function"))?;
        let n = fun.args.len() as u32;
        let kinds: Vec<hl::hl_type_kind> = fun.args.iter().map(|a| self.bc.types[a.0].kind).collect();
        let ret_kind = self.bc.types[fun.ret.0].kind;

        let mut params = vec![I32];
        params.extend(kinds.iter().map(|&k| Class::of_kind(k).wasm()));
        let results: Vec<ValType> = if ret_kind == hl::hl_type_kind_HVOID {
            Vec::new()
        } else {
            vec![Class::of_kind(ret_kind).wasm()]
        };
        let sym = self
            .obj
            .declare_function(&name, params, results, Linkage::Local);
        let mut f = F::new(&Scratch::locals(I32));
        let scratch = Scratch::after(1 + n);

        // The host's callee word, then each argument as a word.
        let held = self.imp(&c.held, &[I32], &[I64]);
        f.get(0).call(held);
        for (i, &kind) in kinds.iter().enumerate() {
            f.get(1 + i as u32);
            let class = Class::of_kind(kind);
            match &c.arg_casts[i] {
                Some(cast) => {
                    self.host_cast(&mut f, Some(cast), class, Class::Long, fun.args[i].0, &scratch, &name)?;
                }
                None => word_from(&mut f, class, kind),
            }
        }
        let callee = self.imp(&c.callee, &vec![I64; 1 + n as usize], &[I64]);
        f.call(callee);
        if let Some(after) = &c.after {
            self.after_call(&mut f, after, c.after_flag.as_deref());
        }
        if ret_kind == hl::hl_type_kind_HVOID {
            f.i(I::Drop);
        } else {
            let class = Class::of_kind(ret_kind);
            match &c.ret_cast {
                Some(cast) => {
                    self.host_cast(&mut f, Some(cast), Class::Long, class, fun.ret.0, &scratch, &name)?;
                }
                None => word_to(&mut f, class),
            }
        }
        self.obj.set_body(sym, f.finish());
        Ok(sym)
    }
}
