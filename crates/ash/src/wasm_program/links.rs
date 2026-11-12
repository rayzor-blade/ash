//! The natives a host links ahead of time, as functions of the object: each
//! takes the program's types, casts to what the host's symbol takes, calls it
//! (or the symbol found in a side module at start-up), and casts the result
//! back. The LLVM path does the same in `generate_native_caller_linked`.
use anyhow::{Result, anyhow, bail};
use ash_wasm_emit::object::{Linkage, Sym};
use ash_wasm_emit::wasm_encoder::{BlockType, Instruction as I, ValType};

use super::{F, I32, Image, mem};
use crate::hl_bindings as hl;
use crate::native_lib::{HostLink, Word};

/// The tag bits of a boxed value in a number word, and the one NaN that
/// stands for every NaN.
const BOXED: i64 = 0x7ffc_0000_0000_0000;
const CANONICAL_NAN: i64 = 0x7ff8_0000_0000_0000;

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

/// What a value is to a cast, with the program's types and the host's words
/// reduced to one set.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Class {
    Bool,
    Int,
    Long,
    Float,
    Double,
    Pointer,
}

impl Class {
    fn of_kind(kind: hl::hl_type_kind) -> Class {
        match kind {
            k if k == hl::hl_type_kind_HBOOL => Class::Bool,
            k if k == hl::hl_type_kind_HUI8
                || k == hl::hl_type_kind_HUI16
                || k == hl::hl_type_kind_HI32 =>
            {
                Class::Int
            }
            k if k == hl::hl_type_kind_HI64 => Class::Long,
            k if k == hl::hl_type_kind_HF32 => Class::Float,
            k if k == hl::hl_type_kind_HF64 => Class::Double,
            _ => Class::Pointer,
        }
    }

    fn of_word(word: Word) -> Class {
        match word {
            Word::Bool => Class::Bool,
            Word::I32 => Class::Int,
            Word::I64 => Class::Long,
            Word::F32 => Class::Float,
            Word::F64 => Class::Double,
            Word::Ptr => Class::Pointer,
        }
    }

    fn wasm(self) -> ValType {
        match self {
            Class::Long => ValType::I64,
            Class::Float => ValType::F32,
            Class::Double => ValType::F64,
            _ => I32,
        }
    }
}

/// Where the thunk's scratch locals sit, after its parameters.
struct Scratch {
    /// The callee found in a slot.
    target: u32,
    /// A number word being unboxed.
    word: u32,
    /// A number being boxed.
    num: u32,
    /// The callee's result, kept across `after`.
    saved: u32,
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
        let scratch = Scratch {
            target: n,
            word: n + 1,
            num: n + 2,
            saved: n + 3,
        };
        let saved_type = callee_results.first().copied().unwrap_or(I32);
        let mut f = F::new(&[(1, I32), (1, ValType::I64), (1, ValType::F64), (1, saved_type)]);

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
            let after_fn = self.imp(after, &[], &[]);
            match &link.after_flag {
                None => {
                    f.call(after_fn);
                }
                Some(flag) => {
                    let flag = self.obj.import_data(flag);
                    f.addr(flag, 0).i(I::I32Load(mem(0, 2))).i(I::If(BlockType::Empty));
                    f.call(after_fn).i(I::End);
                }
            }
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

    /// Convert the value on the stack from `from` to `to`: by the host's cast
    /// if it named one, otherwise by widening or narrowing an integer.
    #[allow(clippy::too_many_arguments)]
    fn host_cast(
        &mut self,
        f: &mut F,
        cast: Option<&str>,
        from: Class,
        to: Class,
        ty: usize,
        scratch: &Scratch,
        name: &str,
    ) -> Result<()> {
        match cast {
            Some("ash:unbox_f64") => {
                match from {
                    Class::Long => {}
                    Class::Bool | Class::Int => {
                        f.i(I::I64ExtendI32U);
                    }
                    other => bail!("ash:unbox_f64 takes a word, not {other:?}"),
                }
                f.set(scratch.word);
                // A boxed value is not a number: NaN.
                f.get(scratch.word).i(I::F64ReinterpretI64);
                f.i(I::F64Const(f64::NAN.into()));
                f.get(scratch.word).i(I::I64Const(BOXED)).i(I::I64And);
                f.i(I::I64Const(BOXED)).i(I::I64Ne);
                f.i(I::Select);
                match to {
                    Class::Double => {}
                    Class::Float => {
                        f.i(I::F32DemoteF64);
                    }
                    other => bail!("ash:unbox_f64 answers a float, not {other:?}"),
                }
            }
            Some("ash:box_f64") => {
                match from {
                    Class::Double => {}
                    Class::Float => {
                        f.i(I::F64PromoteF32);
                    }
                    other => bail!("ash:box_f64 takes a float, not {other:?}"),
                }
                if to != Class::Long {
                    bail!("ash:box_f64 answers a word, not {to:?}");
                }
                f.set(scratch.num);
                // Every NaN is the one canonical NaN, so none reads as boxed.
                f.i(I::I64Const(CANONICAL_NAN));
                f.get(scratch.num).i(I::I64ReinterpretF64);
                f.get(scratch.num).get(scratch.num).i(I::F64Ne);
                f.i(I::Select);
            }
            Some("ash:closure") => bail!("{name}: the cast ash:closure needs the host's closure types"),
            Some(other) if other.starts_with("ash:") => bail!("no built-in cast `{other}`"),
            Some(cast) => {
                let function = self.imp(cast, &[from.wasm(), I32], &[to.wasm()]);
                f.addr(self.types[ty], 0).call(function);
            }
            None => self.widen_or_narrow(f, from, to, name)?,
        }
        Ok(())
    }

    /// An integer as another integer type: sign-extended, zero-extended from a
    /// bool, truncated, or tested against zero for a bool. Nothing else
    /// converts without a cast.
    fn widen_or_narrow(&mut self, f: &mut F, from: Class, to: Class, name: &str) -> Result<()> {
        use Class::{Bool, Int, Long};
        match (from, to) {
            (a, b) if a == b => {}
            (Bool, Int) => {}
            (Int, Bool) => {
                f.i(I::I32Eqz).i(I::I32Eqz);
            }
            (Long, Bool) => {
                f.i(I::I64Eqz).i(I::I32Eqz);
            }
            (Bool, Long) => {
                f.i(I::I64ExtendI32U);
            }
            (Int, Long) => {
                f.i(I::I64ExtendI32S);
            }
            (Long, Int) => {
                f.i(I::I32WrapI64);
            }
            (from, to) => {
                return Err(anyhow!("{name}: a {from:?} cannot become a {to:?} without a cast"));
            }
        }
        Ok(())
    }
}
