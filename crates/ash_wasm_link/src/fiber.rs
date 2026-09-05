//! The fiber transform, built one checkable piece at a time.
//!
//! The design is `docs/wasm-fibers.md`. This module holds the rewrite; what
//! is here so far is its foundation, which is the part with no precedent we
//! can copy.
//!
//! # Emptying the operand stack across a call
//!
//! To unwind out of a call and later rewind back into it, every value that is
//! live at that call has to be saved somewhere addressable. Locals are;
//! the operand stack is not -- a suspended frame cannot enumerate it, and a
//! resumed one cannot rebuild it.
//!
//! Binaryen solves this with a separate `Flatten` pass that rewrites the whole
//! function into a form where nothing is ever on the stack across a call.
//! That pass aborts on `try_table`, which every module ash links contains, so
//! it is not available to us (`docs/wasm-fibers.md` §11 has the stack trace).
//!
//! [`empty_stack_at_calls`] does the same job locally and without a tree. At
//! each call site the operand stack is popped into locals and pushed straight
//! back: the values are unchanged and in the same order, and each one now also
//! sits in a local that an unwind can spill and a rewind can restore. The
//! validator running alongside the decoder is what makes this possible, since
//! it is what knows the type of each operand.
//!
//! Two things bound it, both of which the transform reports rather than
//! assumes:
//!
//! - A call whose stack holds nothing but its own arguments needs no locals at
//!   all: the arguments are consumed by the call, so there is nothing to
//!   preserve. This is the overwhelmingly common case in ash's output, so the
//!   transform is close to free.
//! - A value pushed *before* the current control frame began belongs to an
//!   enclosing frame and cannot be popped: wasm validation forbids a frame
//!   from touching operands below its base. Flattening those is exactly the
//!   global restructure this module exists to avoid, so a function containing
//!   one is refused rather than half-instrumented.

use std::collections::BTreeMap;

use anyhow::{anyhow, bail, Result};
use wasm_encoder::{Instruction, ValType};
use wasmparser::Operator;

use crate::cursor::{rewrite_module, Cursor};

/// What emptying the stack cost, over a whole module.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Spills {
    /// Call sites seen, direct and indirect.
    pub calls: usize,
    /// Call sites that needed no locals because the stack held only the
    /// call's own arguments.
    pub already_empty: usize,
    /// Values pushed through a local at a call site, summed over the module.
    /// This is the cost: two instructions each.
    pub moved: usize,
    /// Of those, the ones that actually needed preserving -- the values that
    /// were on the stack under the call's own arguments.
    pub live: usize,
    /// Locals added, summed over every function.
    pub locals: usize,
    /// Functions left alone because something in them cannot be instrumented:
    /// a value belonging to an enclosing frame live across a call, or a tail
    /// call, whose frame is gone before the callee returns.
    pub refused: usize,
}

/// Rewrite `bytes` so that at every call site in `instrument`, every live
/// operand also lives in a local.
///
/// `instrument` decides which functions are touched; a fiber transform passes
/// the suspend set from [`crate::suspend`]. Functions outside it are copied
/// through unchanged, as is every section but the code section.
///
/// The result is behaviour-preserving on its own: this is the preparation an
/// unwind needs, not the unwind.
pub fn empty_stack_at_calls(
    bytes: &[u8],
    instrument: &dyn Fn(u32) -> bool,
) -> Result<(Vec<u8>, Spills)> {
    // Which functions cannot be instrumented is settled before anything is
    // emitted. Discovering it mid-body would leave a function half done --
    // valid, and wrong in exactly the silent way this crate is written
    // against -- so the walk runs twice and the first result is thrown away.
    let mut refused: std::collections::BTreeSet<u32> = std::collections::BTreeSet::new();
    rewrite_module(bytes, |index, c, op| {
        if instrument(index) && !refused.contains(&index) && !can_instrument(c, op)? {
            refused.insert(index);
        }
        c.emit(op)
    })?;

    let mut spills = Spills {
        refused: refused.len(),
        ..Spills::default()
    };
    // Scratch locals, reused across the call sites of one function: a value
    // is only in one at a time, so `n` slots of a type serve every site that
    // needs at most `n` of it.
    let mut pool: BTreeMap<ValType, Vec<u32>> = BTreeMap::new();
    let mut current = u32::MAX;

    let out = rewrite_module(bytes, |index, c, op| {
        if index != current {
            current = index;
            pool.clear();
        }
        if !instrument(index) || refused.contains(&index) {
            return c.emit(op);
        }
        let arity = match op {
            Operator::Call { .. } | Operator::CallIndirect { .. } => call_arity(c, op)?,
            _ => return c.emit(op),
        };
        spills.calls += 1;

        // Everything the current frame owns, minus the arguments the call is
        // about to consume. The first pass guaranteed the frame's base is 0,
        // so the frame owns the whole stack.
        let owned = c.stack_height();
        if owned.saturating_sub(arity) == 0 {
            spills.already_empty += 1;
            return c.emit(op);
        }

        // Pop the whole stack, top first, then push it back. The arguments
        // have to go through locals too -- they are above the values we are
        // after, and wasm has no way to reach past them.
        let total = owned as usize;
        let mut used: BTreeMap<ValType, usize> = BTreeMap::new();
        let mut slots = Vec::with_capacity(total);
        for depth in 0..total {
            let ty = c
                .operand(depth)
                .ok_or_else(|| anyhow!("operand {depth} is past the bottom of the stack"))?
                .ok_or_else(|| {
                    anyhow!("operand {depth} has no type, so this code is unreachable")
                })?;
            let ty = encode_val_type(ty)?;
            let nth = used.entry(ty).or_default();
            let at = *nth;
            *nth += 1;
            let have = pool.entry(ty).or_default();
            while have.len() <= at {
                have.push(c.reserve_local(ty));
                spills.locals += 1;
            }
            slots.push(have[at]);
        }
        for &l in &slots {
            c.emit_new(&Instruction::LocalSet(l));
        }
        for &l in slots.iter().rev() {
            c.emit_new(&Instruction::LocalGet(l));
        }
        spills.moved += total;
        spills.live += (owned - arity) as usize;
        c.emit(op)
    })?;
    Ok((out, spills))
}

/// Whether this operator is one the transform can handle in this function.
///
/// Two shapes it cannot. A value that was on the stack before the current
/// control frame began belongs to an enclosing frame, and wasm forbids a
/// frame from popping below its own base -- reaching those is the global
/// restructure this module exists to avoid. And a tail call's frame is gone
/// before the callee returns, so there is no frame to unwind out of or rewind
/// back into; the suspend analysis records the edge, the rewrite declines it.
fn can_instrument(c: &Cursor, op: &Operator<'_>) -> Result<bool> {
    match op {
        Operator::ReturnCall { .. } | Operator::ReturnCallIndirect { .. } => Ok(false),
        Operator::Call { .. } | Operator::CallIndirect { .. } => {
            // A non-zero base is exactly the bad case: those values are live
            // across the call and this frame may not pop them.
            let base = c
                .frame(0)
                .ok_or_else(|| anyhow!("no control frame at a call"))?
                .height;
            Ok(base == 0)
        }
        _ => Ok(true),
    }
}

/// How many values the call about to be emitted will consume.
fn call_arity(c: &Cursor, op: &Operator<'_>) -> Result<u32> {
    use wasmparser::WasmModuleResources as _;
    let r = c.resources();
    let type_index = match op {
        Operator::Call { function_index } => r
            .type_index_of_function(*function_index)
            .ok_or_else(|| anyhow!("callee {function_index} has no type"))?,
        // An indirect call also pops the table index, which is not an
        // argument but is on the stack and so has to be counted.
        Operator::CallIndirect { type_index, .. } => *type_index,
        other => bail!("{other:?} is not a call"),
    };
    let sub = r
        .sub_type_at(type_index)
        .ok_or_else(|| anyhow!("type {type_index} is not in the module"))?;
    let params = match &sub.composite_type.inner {
        wasmparser::CompositeInnerType::Func(f) => f.params().len() as u32,
        other => bail!("a call names a non-function type {other:?}"),
    };
    Ok(match op {
        Operator::CallIndirect { .. } => params + 1,
        _ => params,
    })
}

fn encode_val_type(ty: wasmparser::ValType) -> Result<ValType> {
    use wasm_encoder::reencode::Reencode as _;
    wasm_encoder::reencode::RoundtripReencoder
        .val_type(ty)
        .map_err(|e| anyhow!("re-encoding an operand type: {e}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn validate(bytes: &[u8]) -> Result<()> {
        wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
            .validate_all(bytes)
            .map(|_| ())
            .map_err(|e| anyhow!("{e}"))
    }

    /// `$g` leaves a value on the stack, calls `$f`, then adds the two. The
    /// value under the call is what has to survive.
    fn module() -> Vec<u8> {
        let mut types = wasm_encoder::TypeSection::new();
        types.ty().function([ValType::I32], [ValType::I32]);
        let mut funcs = wasm_encoder::FunctionSection::new();
        funcs.function(0);
        funcs.function(0);
        let mut exports = wasm_encoder::ExportSection::new();
        exports.export("g", wasm_encoder::ExportKind::Func, 1);

        let mut f = wasm_encoder::Function::new([]);
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::End);

        let mut g = wasm_encoder::Function::new([]);
        g.instruction(&Instruction::LocalGet(0)); // lives across the call
        g.instruction(&Instruction::LocalGet(0));
        g.instruction(&Instruction::Call(0));
        g.instruction(&Instruction::I32Add);
        g.instruction(&Instruction::End);

        let mut code = wasm_encoder::CodeSection::new();
        code.function(&f);
        code.function(&g);
        let mut m = wasm_encoder::Module::new();
        m.section(&types);
        m.section(&funcs);
        m.section(&exports);
        m.section(&code);
        m.finish()
    }

    #[test]
    fn a_value_under_a_call_is_put_in_a_local() {
        let (out, spills) = empty_stack_at_calls(&module(), &|_| true).expect("transform");
        validate(&out).expect("the transformed module must validate");
        assert_eq!(spills.calls, 1);
        assert_eq!(spills.already_empty, 0);
        // Both the live value and the call's own argument go through a local;
        // one of the two actually needed preserving.
        assert_eq!(spills.moved, 2);
        assert_eq!(spills.live, 1);
        // Both are i32, so two slots of one type.
        assert_eq!(spills.locals, 2);
        assert_eq!(spills.refused, 0);
    }

    #[test]
    fn a_call_with_nothing_under_it_costs_nothing() {
        // `$f` has no calls; instrument only it, so the one call site in the
        // module is skipped and nothing at all is added.
        let input = module();
        let (out, spills) = empty_stack_at_calls(&input, &|i| i == 0).expect("transform");
        validate(&out).expect("must validate");
        assert_eq!(spills.calls, 0);
        assert_eq!(spills.locals, 0);
        // Only the immediate widths differ, so the module is no bigger.
        assert!(out.len() <= input.len(), "{} vs {}", out.len(), input.len());
    }

    #[test]
    fn a_tail_call_makes_a_function_refuse() {
        let mut types = wasm_encoder::TypeSection::new();
        types.ty().function([ValType::I32], [ValType::I32]);
        let mut funcs = wasm_encoder::FunctionSection::new();
        funcs.function(0);
        funcs.function(0);
        let mut f = wasm_encoder::Function::new([]);
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::End);
        let mut g = wasm_encoder::Function::new([]);
        g.instruction(&Instruction::LocalGet(0));
        g.instruction(&Instruction::ReturnCall(0));
        g.instruction(&Instruction::End);
        let mut code = wasm_encoder::CodeSection::new();
        code.function(&f);
        code.function(&g);
        let mut m = wasm_encoder::Module::new();
        m.section(&types);
        m.section(&funcs);
        m.section(&code);
        let input = m.finish();

        let (out, spills) = empty_stack_at_calls(&input, &|_| true).expect("transform");
        validate(&out).expect("must validate");
        assert_eq!(
            spills.refused, 1,
            "the tail-calling function must be left alone"
        );
        assert_eq!(spills.locals, 0);
    }

    #[test]
    fn a_value_from_an_enclosing_frame_makes_a_function_refuse() {
        // The `i32.const 5` is pushed before the block, so inside the block it
        // is below the frame's base and cannot be popped there.
        let mut types = wasm_encoder::TypeSection::new();
        types.ty().function([], []);
        types.ty().function([ValType::I32, ValType::I32], []);
        let mut funcs = wasm_encoder::FunctionSection::new();
        funcs.function(0);
        funcs.function(1);
        let mut f = wasm_encoder::Function::new([]);
        f.instruction(&Instruction::End);
        let mut g = wasm_encoder::Function::new([]);
        g.instruction(&Instruction::I32Const(5));
        g.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
        g.instruction(&Instruction::Call(0));
        g.instruction(&Instruction::End);
        g.instruction(&Instruction::I32Const(6));
        g.instruction(&Instruction::Call(1));
        g.instruction(&Instruction::End);
        let mut code = wasm_encoder::CodeSection::new();
        code.function(&f);
        code.function(&g);
        let mut m = wasm_encoder::Module::new();
        m.section(&types);
        m.section(&funcs);
        m.section(&code);
        let input = m.finish();

        let (out, spills) = empty_stack_at_calls(&input, &|_| true).expect("transform");
        validate(&out).expect("must validate");
        assert_eq!(
            spills.refused, 1,
            "the function with a stacked value must be left alone"
        );
        assert_eq!(spills.locals, 0);
    }
}
