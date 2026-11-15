//! What the AIR passes ask a module: natives, constants, types, callee bodies.
//!
//! The passes live in the `air` crate and take any [`ModuleInfo`], so the lowered functions are
//! optimized with them directly: the inliner copies the lowered callees (`CalleeBody::Air`), and
//! no bytecode is involved.

use std::collections::HashMap;

use air::v2::facts::{self, CallSummary};
use air::v2::ir::{Function, Instr, TypeRef};
use air::v2::module::{CalleeBody, ModuleInfo, NativeImport};
use air::v2::passes::{OptLevel, PassManager, PassOptions, PassReport};

use crate::hl::HlType;
use crate::lower::{Lowered, Module};

pub struct NairModule {
    ints: Vec<i32>,
    types: Vec<HlType>,
    natives: HashMap<usize, NativeImport>,
    callees: HashMap<usize, Function>,
    summaries: HashMap<usize, CallSummary>,
}

impl NairModule {
    /// A snapshot of the module as lowered: the callees are the unoptimized bodies.
    pub fn new(module: &Module, lowered: &[Lowered]) -> NairModule {
        let types = module.types.table.types.clone();
        let mut natives = HashMap::new();
        for n in &module.natives {
            if let HlType::Fun(args, ret) = &types[n.ty] {
                let args = args.iter().map(|a| TypeRef(*a as u32)).collect();
                natives.insert(
                    n.findex as usize,
                    NativeImport::new(
                        n.findex as usize,
                        n.lib.clone(),
                        n.name.clone(),
                        args,
                        TypeRef(*ret as u32),
                    ),
                );
            }
        }
        let mut callees: HashMap<usize, Function> = lowered
            .iter()
            .map(|l| {
                (
                    l.findex as usize,
                    l.function.clone().with_findex(l.findex as usize),
                )
            })
            .collect();
        let summaries = facts::summarize(callees.iter().map(|(i, f)| (*i, f)));
        for f in callees.values_mut() {
            facts::attach(f, &summaries);
        }
        NairModule {
            ints: module.ints.clone(),
            types,
            natives,
            callees,
            summaries,
        }
    }

    /// What a call to the function may write, from the bodies lowered so far.
    pub fn summary(&self, findex: usize) -> Option<&CallSummary> {
        self.summaries.get(&findex)
    }

    fn ty(&self, t: TypeRef) -> Option<&HlType> {
        self.types.get(t.0 as usize)
    }
}

impl ModuleInfo for NairModule {
    fn native(&self, findex: usize) -> Option<NativeImport> {
        self.natives.get(&findex).cloned()
    }

    fn int_value(&self, idx: usize) -> Option<i32> {
        self.ints.get(idx).copied()
    }

    fn int_pool_len(&self) -> usize {
        self.ints.len()
    }

    fn is_float(&self, ty: TypeRef) -> bool {
        matches!(self.ty(ty), Some(HlType::F32 | HlType::F64))
    }

    fn value_is_i32(&self, ty: TypeRef) -> bool {
        matches!(self.ty(ty), Some(HlType::I32))
    }

    fn value_is_small_uint(&self, ty: TypeRef) -> bool {
        matches!(self.ty(ty), Some(HlType::UI8 | HlType::UI16))
    }

    fn type_size(&self, ty: TypeRef) -> Option<u32> {
        Some(match self.ty(ty)? {
            HlType::UI8 | HlType::Bool => 1,
            HlType::UI16 => 2,
            HlType::I32 | HlType::F32 => 4,
            HlType::I64 | HlType::F64 => 8,
            _ => return None,
        })
    }

    fn callee(&self, findex: usize) -> Option<CalleeBody> {
        self.callees
            .get(&findex)
            .map(|f| CalleeBody::Air(Box::new(f.clone())))
    }

    fn callee_ops(&self, findex: usize) -> Option<usize> {
        self.callees.get(&findex).map(instruction_count)
    }
}

/// Instructions in `f`, terminators included.
pub fn instruction_count(f: &Function) -> usize {
    f.blocks.iter().map(|b| b.instrs.len() + 1).sum()
}

/// Runs the AIR passes over `f` at `level`, with the module's call summaries attached to it.
///
/// `fma` forms fused multiply-adds, which change the rounding of float arithmetic on purpose;
/// turn it off for results that must match unfused arithmetic bit for bit.
pub fn optimize(
    module: &NairModule,
    f: &mut Function,
    level: OptLevel,
    fma: bool,
) -> Result<PassReport, String> {
    facts::attach(f, &module.summaries);
    let opts = PassOptions {
        fma,
        verify_each: true,
        ..PassOptions::default()
    };
    PassManager::with_module(level, module)
        .with_options(opts)
        .run(f)
        .map_err(|e| format!("{e:#}"))
}

/// The calls and loads a function makes, to see what the passes changed.
pub fn describe_mix(f: &Function) -> String {
    let (mut calls, mut other) = (0, 0);
    for b in &f.blocks {
        for i in &b.instrs {
            match i {
                Instr::Call { .. } => calls += 1,
                _ => other += 1,
            }
        }
    }
    format!(
        "{} blocks, {} calls, {} other instructions",
        f.blocks.len(),
        calls,
        other
    )
}
