//! The bridge from a decoded program to `ash_wasm_emit`.
//!
//! The emitter knows AIR and a handful of questions about the program it
//! belongs to; this answers them from the bytecode. A function is handed to it
//! under the symbol the LLVM module gave that function, since callers on
//! either side of the link name it by that.
use crate::bytecode::DecodedBytecode;
use crate::hl_bindings as hl;
use ash_wasm_emit::{Input, ObjectEmitted, Options, Program, Sig, Ty};
use std::collections::HashMap;

pub struct BytecodeProgram<'a> {
    pub bc: &'a DecodedBytecode,
    /// Each bytecode function's symbol, as the module that lowers the rest of
    /// the program names it.
    pub symbols: &'a HashMap<usize, String>,
}

impl BytecodeProgram<'_> {
    fn ty_of(&self, index: usize) -> Ty {
        let k = self.bc.types[index].kind;
        if k == hl::hl_type_kind_HVOID {
            Ty::Void
        } else if k == hl::hl_type_kind_HBOOL {
            Ty::Bool
        } else if k == hl::hl_type_kind_HI32 {
            Ty::I32
        } else if k == hl::hl_type_kind_HI64 {
            Ty::I64
        } else if k == hl::hl_type_kind_HF32 {
            Ty::F32
        } else if k == hl::hl_type_kind_HF64 {
            Ty::F64
        } else {
            Ty::Other
        }
    }

    fn sig_of(&self, fun: &crate::types::HLTypeFun) -> Sig {
        Sig {
            params: fun.args.iter().map(|a| self.ty_of(a.0)).collect(),
            ret: self.ty_of(fun.ret.0),
        }
    }
}

impl Program for BytecodeProgram<'_> {
    fn ty(&self, t: air::v2::ir::TypeRef) -> Ty {
        self.ty_of(t.0 as usize)
    }

    fn int(&self, idx: usize) -> Option<i32> {
        self.bc.ints.get(idx).copied()
    }

    fn float(&self, idx: usize) -> Option<f64> {
        self.bc.floats.get(idx).copied()
    }

    fn symbol(&self, findex: usize) -> String {
        self.symbols
            .get(&findex)
            .cloned()
            .unwrap_or_else(|| format!("Fun_{findex}"))
    }

    fn signature(&self, findex: usize) -> Option<Sig> {
        // A native is called through a lowering of its own, not by symbol.
        let f = self
            .bc
            .functions
            .iter()
            .find(|f| f.findex as usize == findex)?;
        let fun = self.bc.types[f.type_.0].fun.as_ref()?;
        Some(self.sig_of(fun))
    }
}

/// Emit every function of `bc` that the direct backend can, as one relocatable
/// object.
pub fn emit_object(
    bc: &DecodedBytecode,
    symbols: &HashMap<usize, String>,
    opts: Options,
) -> ObjectEmitted {
    let program = BytecodeProgram { bc, symbols };
    let module = crate::air_pipeline::AshModule::new(bc);
    let mut optimized = Vec::new();
    for f in &bc.functions {
        let Ok(o) = crate::air_pipeline::optimized(&module, f) else {
            continue;
        };
        let Some(sig) = program.signature(f.findex as usize) else {
            continue;
        };
        optimized.push((f.findex as usize, sig, o));
    }
    let inputs: Vec<Input> = optimized
        .iter()
        .map(|(findex, sig, o)| Input {
            findex: *findex,
            sig: sig.clone(),
            body: &o.ir,
        })
        .collect();
    ash_wasm_emit::emit_object(&program, &inputs, opts)
}
