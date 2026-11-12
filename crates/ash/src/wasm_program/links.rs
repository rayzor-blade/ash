//! The natives a host links ahead of time, as functions of the object: each
//! takes the program's types, casts to what the host's symbol takes, calls it
//! (or the symbol found in a side module at start-up), and casts the result
//! back. The LLVM path does the same in `generate_native_caller_linked`.
use anyhow::{Result, bail};
use ash_wasm_emit::object::{Linkage, Sym};
use ash_wasm_emit::wasm_encoder::{BlockType, Instruction as I, ValType};

use super::casts::{Class, Scratch};
use super::{F, I32, Image, mem};
use crate::native_lib::HostLink;

/// A native the host links, and the function that stands for it.
pub(super) struct LinkNative {
    pub findex: usize,
    pub sym: Sym,
    pub link: HostLink,
    /// The types of the arguments and of the result, by type index.
    pub args: Vec<usize>,
    pub ret: usize,
    /// The wasm types the program passes and receives.
    pub params: Vec<ValType>,
    pub results: Vec<ValType>,
}

impl Image<'_> {
    /// Define every linked native's function. Returns the (library, symbol)
    /// pairs whose callee is in a side module, which start-up looks up.
    pub(super) fn link_thunks(&mut self) -> Result<Vec<(String, String)>> {
        let links = std::mem::take(&mut self.links);
        let mut slots: Vec<(String, String)> = Vec::new();
        for l in &links {
            if let Some(lib) = &l.link.library {
                let key = (lib.clone(), l.link.symbol.clone());
                if !slots.contains(&key) {
                    slots.push(key);
                }
            }
            self.link_thunk(l)?;
        }
        self.links = links;
        Ok(slots)
    }

    fn link_thunk(&mut self, l: &LinkNative) -> Result<()> {
        let link = &l.link;
        let name = link.symbol.as_str();
        // An initialiser's first argument is the program's object, which the
        // callee never sees.
        let skip = usize::from(link.init.is_some());
        if l.params.len() != link.params.len() + skip || link.arg_casts.len() != link.params.len() {
            bail!(
                "{name}: the program declares {} arguments, the link {}",
                l.params.len(),
                link.params.len()
            );
        }
        if link.after_flag.is_some() && link.library.is_some() {
            bail!(
                "{name}: an after_flag cannot name data in the side module `{}`",
                link.library.as_deref().unwrap_or_default()
            );
        }
        let callee_params: Vec<ValType> = link.params.iter().map(|&w| Class::of_word(w).wasm()).collect();
        let callee_results: Vec<ValType> = link.ret.iter().map(|&w| Class::of_word(w).wasm()).collect();

        let n = l.params.len() as u32;
        let scratch = Scratch::after(n);
        let saved_type = callee_results.first().copied().unwrap_or(I32);
        let mut f = F::new(&Scratch::locals(saved_type));

        // A callee in a side module is found by its symbol at start-up, into
        // a slot; unresolved, it raises as an HDLL primitive does. Any other
        // is linked by name.
        let direct = match &link.library {
            None => Some(self.imp(name, &callee_params, &callee_results)),
            Some(lib) => {
                let slot_name = format!("ash_link_{lib}_{name}");
                let slot = match self.obj.lookup(&slot_name) {
                    Some(s) => s,
                    None => self.obj.declare_data(&slot_name, 4, 2, Linkage::Local),
                };
                let lib_s = self.cstr(lib);
                let sym_s = self.cstr(name);
                let missing = self.imp("hlp_aot_native_missing", &[I32, I32], &[]);
                f.addr(slot, 0).i(I::I32Load(mem(0, 2))).set(scratch.target);
                f.get(scratch.target).i(I::I32Eqz).i(I::If(BlockType::Empty));
                f.addr(lib_s, 0).addr(sym_s, 0).call(missing);
                f.i(I::Unreachable).i(I::End);
                None
            }
        };

        for j in 0..link.params.len() {
            let i = j + skip;
            f.get(i as u32);
            let from = Class::of_kind(self.bc.types[l.args[i]].kind);
            self.host_cast(
                &mut f,
                link.arg_casts[j].as_deref(),
                from,
                Class::of_word(link.params[j]),
                l.args[i],
                &scratch,
                name,
            )?;
        }
        match direct {
            Some(callee) => {
                f.call(callee);
            }
            None => {
                f.get(scratch.target);
                f.body.call_indirect(callee_params, callee_results);
            }
        }
        if link.ret.is_some() {
            f.set(scratch.saved);
        }

        if let Some(after) = &link.after {
            self.after_call(&mut f, after, link.after_flag.as_deref());
        }

        // An initialiser binds the program's object to what the callee made.
        if let Some(init) = &link.init {
            if link.ret.is_none() {
                bail!("{name}: an initialiser's callee returns nothing");
            }
            let init_fn = self.imp(init, &[l.params[0], saved_type, I32], &[]);
            f.get(0).get(scratch.saved).addr(self.types[l.args[0]], 0).call(init_fn);
            self.obj.set_body(l.sym, f.finish());
            return Ok(());
        }
        match (link.ret, l.results.first()) {
            (Some(word), Some(_)) => {
                f.get(scratch.saved);
                self.host_cast(
                    &mut f,
                    link.ret_cast.as_deref(),
                    Class::of_word(word),
                    Class::of_kind(self.bc.types[l.ret].kind),
                    l.ret,
                    &scratch,
                    name,
                )?;
            }
            // A result the program declares none for is dropped.
            (_, None) => {}
            (None, Some(_)) => {
                bail!("{name}: the callee returns nothing, the program declares a result");
            }
        }
        self.obj.set_body(l.sym, f.finish());
        Ok(())
    }
}
