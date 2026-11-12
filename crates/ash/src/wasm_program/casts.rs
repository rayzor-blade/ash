//! The conversions at the boundary with a host: between the program's values
//! and the words or machine types the host's functions take. A link, an
//! export and a host closure all make them. The LLVM path's counterparts are
//! `emit_host_cast`, `emit_host_after`, `word_to` and `word_from`.
use anyhow::{Result, anyhow, bail};
use ash_wasm_emit::wasm_encoder::{BlockType, Instruction as I, ValType};

use super::{F, I32, Image, mem};
use crate::hl_bindings as hl;
use crate::native_lib::Word;

/// The tag bits of a boxed value in a number word, and the one NaN that
/// stands for every NaN.
const BOXED: i64 = 0x7ffc_0000_0000_0000;
const CANONICAL_NAN: i64 = 0x7ff8_0000_0000_0000;

/// What a value is to a cast, with the program's types and the host's words
/// reduced to one set.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(super) enum Class {
    Bool,
    Int,
    Long,
    Float,
    Double,
    Pointer,
}

impl Class {
    pub(super) fn of_kind(kind: hl::hl_type_kind) -> Class {
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

    pub(super) fn of_word(word: Word) -> Class {
        match word {
            Word::Bool => Class::Bool,
            Word::I32 => Class::Int,
            Word::I64 => Class::Long,
            Word::F32 => Class::Float,
            Word::F64 => Class::Double,
            Word::Ptr => Class::Pointer,
        }
    }

    pub(super) fn wasm(self) -> ValType {
        match self {
            Class::Long => ValType::I64,
            Class::Float => ValType::F32,
            Class::Double => ValType::F64,
            _ => I32,
        }
    }
}

/// Where a body's scratch locals sit, after its parameters.
pub(super) struct Scratch {
    /// A callee found in a slot, or a closure's bound value.
    pub target: u32,
    /// A number word being unboxed.
    pub word: u32,
    /// A number being boxed.
    pub num: u32,
    /// A result kept across a call.
    pub saved: u32,
}

impl Scratch {
    /// The scratch locals of a body with `params` parameters and the locals
    /// [`Scratch::locals`] declares, which come first after them.
    pub(super) fn after(params: u32) -> Scratch {
        Scratch {
            target: params,
            word: params + 1,
            num: params + 2,
            saved: params + 3,
        }
    }

    /// The locals to declare for it; the last holds a result of `saved` type.
    pub(super) fn locals(saved: ValType) -> [(u32, ValType); 4] {
        [(1, I32), (1, ValType::I64), (1, ValType::F64), (1, saved)]
    }
}

impl Image<'_> {
    /// Convert the value on the stack from `from` to `to`: by the host's cast
    /// if it named one, otherwise by widening or narrowing an integer.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn host_cast(
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
                    other => bail!("{name}: ash:unbox_f64 takes a word, not {other:?}"),
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
                    other => bail!("{name}: ash:unbox_f64 answers a float, not {other:?}"),
                }
            }
            Some("ash:box_f64") => {
                match from {
                    Class::Double => {}
                    Class::Float => {
                        f.i(I::F64PromoteF32);
                    }
                    other => bail!("{name}: ash:box_f64 takes a float, not {other:?}"),
                }
                if to != Class::Long {
                    bail!("{name}: ash:box_f64 answers a word, not {to:?}");
                }
                f.set(scratch.num);
                // Every NaN is the one canonical NaN, so none reads as boxed.
                f.i(I::I64Const(CANONICAL_NAN));
                f.get(scratch.num).i(I::I64ReinterpretF64);
                f.get(scratch.num).get(scratch.num).i(I::F64Ne);
                f.i(I::Select);
            }
            Some("ash:closure") => self.host_closure_value(f, from, ty, scratch, name)?,
            Some(other) if other.starts_with("ash:") => bail!("no built-in cast `{other}`"),
            Some(cast) => {
                let function = self.imp(cast, &[from.wasm(), I32], &[to.wasm()]);
                f.addr(self.types[ty], 0).call(function);
            }
            None => widen_or_narrow(f, from, to, name)?,
        }
        Ok(())
    }

    /// Call the host's `after`: always, or with `flag`, only when that 32-bit
    /// data word is not zero, so a call that left nothing pending costs a
    /// load and a test.
    pub(super) fn after_call(&mut self, f: &mut F, after: &str, flag: Option<&str>) {
        let after_fn = self.imp(after, &[], &[]);
        match flag {
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
}

/// An integer as another integer type: sign-extended, zero-extended from a
/// bool, truncated, or tested against zero for a bool. Nothing else converts
/// without a cast.
fn widen_or_narrow(f: &mut F, from: Class, to: Class, name: &str) -> Result<()> {
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

/// A host's word on the stack as a value of class `to`: an integer narrowed,
/// a bool tested, a float by its bits, a pointer by its address.
pub(super) fn word_to(f: &mut F, to: Class) {
    match to {
        Class::Bool => {
            f.i(I::I64Eqz).i(I::I32Eqz);
        }
        Class::Int | Class::Pointer => {
            f.i(I::I32WrapI64);
        }
        Class::Long => {}
        Class::Float => {
            f.i(I::I32WrapI64).i(I::F32ReinterpretI32);
        }
        Class::Double => {
            f.i(I::F64ReinterpretI64);
        }
    }
}

/// A value of class `from` and type kind `kind` on the stack as a host's
/// word: a signed integer sign-extended, any other zero-extended, a float by
/// its bits, a pointer by its address.
pub(super) fn word_from(f: &mut F, from: Class, kind: hl::hl_type_kind) {
    match from {
        Class::Long => {}
        Class::Int if kind == hl::hl_type_kind_HI32 => {
            f.i(I::I64ExtendI32S);
        }
        Class::Int | Class::Bool | Class::Pointer => {
            f.i(I::I64ExtendI32U);
        }
        Class::Float => {
            f.i(I::I32ReinterpretF32).i(I::I64ExtendI32U);
        }
        Class::Double => {
            f.i(I::I64ReinterpretF64);
        }
    }
}
