//! Decoding a function body, and putting it back.
//!
//! Everything this linker does today it does without reading a single
//! instruction: a body is a byte range, a relocation is a fixed-width write at
//! a known offset inside it, and `emit` copies the range out untouched. That
//! is deliberate and it is why the crate is small. But a fiber transform has
//! to instrument bodies -- see `docs/wasm-fibers.md` -- and instrumenting
//! means decoding, changing, and re-encoding, which is a capability the crate
//! does not have.
//!
//! This module is that capability and nothing more. [`reencode`] takes a body
//! and returns a body that means exactly the same thing. It is deliberately
//! the identity: an identity round-trip is the one version of this code whose
//! correctness can be checked by comparison rather than by reasoning, so it
//! ships and is tested first, and the transform is built on a decoder already
//! known to survive every shape ash emits -- including the `try_table` and
//! `exnref` that ash's setjmp lowering produces and that Binaryen's own
//! `Flatten` pass aborts on.
//!
//! # Where this may run
//!
//! After `apply_relocations`, never before. Relocations name absolute byte
//! offsets into the code payload and are written as fixed-width slots; insert
//! or remove a byte ahead of one and the write lands across an opcode
//! boundary in a module that may still validate. By the time every relocation
//! has been spent, nothing reads an offset again, and a body may change size
//! freely.
//!
//! # A body is its contents
//!
//! `Object::code_bodies` holds ranges of body *contents* -- the locals
//! declarations and the instructions -- without the leading size prefix,
//! because that is what `wasmparser`'s `FunctionBody::range` reports and what
//! `wasm_encoder`'s `CodeSection::raw` expects (it writes the prefix itself).
//! [`reencode`] takes and returns the same shape, so a caller can swap one for
//! the other without touching anything around it.

use anyhow::{anyhow, Result};
use wasm_encoder::reencode::{Reencode, RoundtripReencoder};

/// Re-encode one function body, changing nothing.
///
/// Takes and returns body *contents*: locals declarations followed by
/// instructions, with no size prefix.
///
/// The result is not required to be byte-identical to the input and generally
/// is not: ash's own emitter pads every relocated immediate to five bytes so a
/// patch can be written without moving anything, and re-encoding writes each
/// one at its natural width. That is a saving rather than a discrepancy -- it
/// is the same instruction stream -- but it does mean "the bytes match" is the
/// wrong test for this function. The right one is that the operators match,
/// which is what the tests check.
pub fn reencode(contents: &[u8]) -> Result<Vec<u8>> {
    let body = wasmparser::FunctionBody::new(wasmparser::BinaryReader::new_features(
        contents,
        0,
        wasmparser::WasmFeatures::all(),
    ));

    let mut locals = Vec::new();
    for pair in body
        .get_locals_reader()
        .map_err(|e| anyhow!("reading locals: {e}"))?
    {
        let (count, ty) = pair.map_err(|e| anyhow!("reading a local declaration: {e}"))?;
        locals.push((
            count,
            RoundtripReencoder
                .val_type(ty)
                .map_err(|e| anyhow!("re-encoding a local type: {e}"))?,
        ));
    }

    let mut out = wasm_encoder::Function::new(locals);
    let mut reader = body
        .get_operators_reader()
        .map_err(|e| anyhow!("reading operators: {e}"))?;
    while !reader.eof() {
        let op = reader
            .read()
            .map_err(|e| anyhow!("decoding an operator: {e}"))?;
        out.instruction(
            &RoundtripReencoder
                .instruction(op)
                .map_err(|e| anyhow!("re-encoding an operator: {e}"))?,
        );
    }
    // `into_raw_body` gives contents without the size prefix, matching what we
    // were handed and what `CodeSection::raw` wants back.
    Ok(out.into_raw_body())
}

/// The operators of a body, as text, for comparing two encodings of what
/// should be the same function.
///
/// Bytes are the wrong comparison (see [`reencode`]) and a decoded operator
/// does not implement `PartialEq`, so the check that means something is that
/// the two streams print the same. This is a test helper living beside the
/// code it tests rather than in the tests, because both integration tests and
/// any future transform want it.
pub fn operator_trace(contents: &[u8]) -> Result<Vec<String>> {
    let body = wasmparser::FunctionBody::new(wasmparser::BinaryReader::new_features(
        contents,
        0,
        wasmparser::WasmFeatures::all(),
    ));
    let mut reader = body
        .get_operators_reader()
        .map_err(|e| anyhow!("reading operators: {e}"))?;
    let mut ops = Vec::new();
    while !reader.eof() {
        let op = reader
            .read()
            .map_err(|e| anyhow!("decoding an operator: {e}"))?;
        ops.push(format!("{op:?}"));
    }
    Ok(ops)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A body with locals, a block, a branch and a call: enough shape that a
    /// decoder that dropped or reordered anything would show it.
    fn sample() -> Vec<u8> {
        use wasm_encoder::{BlockType, Instruction, ValType};
        let mut f = wasm_encoder::Function::new([(2, ValType::I32), (1, ValType::F64)]);
        f.instruction(&Instruction::Block(BlockType::Empty));
        f.instruction(&Instruction::I32Const(7));
        f.instruction(&Instruction::LocalSet(0));
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::I32Eqz);
        f.instruction(&Instruction::BrIf(0));
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::F64Const(1.5f64.into()));
        f.instruction(&Instruction::LocalSet(2));
        f.instruction(&Instruction::End);
        f.into_raw_body()
    }

    #[test]
    fn a_body_survives_the_round_trip() {
        let original = sample();
        let again = reencode(&original).expect("re-encode");
        assert_eq!(
            operator_trace(&original).expect("trace original"),
            operator_trace(&again).expect("trace re-encoded"),
            "the operator stream changed across a round-trip"
        );
    }

    #[test]
    fn locals_survive_the_round_trip() {
        let again = reencode(&sample()).expect("re-encode");
        let body = wasmparser::FunctionBody::new(wasmparser::BinaryReader::new_features(
            &again,
            0,
            wasmparser::WasmFeatures::all(),
        ));
        let locals: Vec<_> = body
            .get_locals_reader()
            .expect("locals")
            .into_iter()
            .map(|p| p.expect("a local"))
            .collect();
        assert_eq!(locals.len(), 2, "local groups: {locals:?}");
        assert_eq!(locals[0].0, 2, "two i32 locals");
        assert_eq!(locals[1].0, 1, "one f64 local");
    }

    /// The round-trip must be stable: re-encoding an already re-encoded body
    /// changes nothing. If it does not hold, the encoder and decoder disagree
    /// about some immediate's width and the difference would accumulate.
    #[test]
    fn the_round_trip_reaches_a_fixed_point() {
        let once = reencode(&sample()).expect("first");
        let twice = reencode(&once).expect("second");
        assert_eq!(once, twice, "re-encoding is not idempotent");
    }
}
