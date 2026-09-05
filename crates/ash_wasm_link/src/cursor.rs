//! Rewriting a function body, with the validator as a co-pilot.
//!
//! [`crate::body`] proved the decode/re-encode round-trip. This is the part
//! that changes something, and it exists because two of the three ways a
//! wasm rewrite goes wrong are silent.
//!
//! # Why the validator runs alongside
//!
//! A fiber transform has to know, at each call site, what is on the operand
//! stack, because anything there is live across a suspend and has to be
//! spilled. Binaryen's answer is the `Flatten` pass, which rewrites the
//! function into a form where nothing is ever on the stack across a call --
//! and which aborts on `try_table`, so it is not available to us (see
//! `docs/wasm-fibers.md` §3). `wasmparser`'s [`wasmparser::FuncValidator`]
//! already computes the typed operand stack as a side effect of validating,
//! on the flat operator stream, with no tree and no special case for EH. Run
//! it in lockstep and the information Flatten exists to manufacture is simply
//! available.
//!
//! The validator sees the *original* body, always, whatever the rewrite
//! emits. It is an oracle about the input, not a check on the output; the
//! output is validated separately, as a module.
//!
//! # Why wrapping a region is an operation and not something you do by hand
//!
//! Branch immediates are relative. Wrap a region in a new block and every
//! branch inside it that targets something *outside* it moves by one, while
//! every branch that stays inside does not. Get that wrong and the module
//! validates, runs, and jumps to the wrong place -- the same failure class as
//! a wrong relocation, which is the one dangerous property this crate is
//! written around. So [`Cursor::open_block`] and [`Cursor::close_block`] are
//! the supported way to do it, and [`Cursor::emit`] renumbers every branch it
//! passes through.
//!
//! Operators that carry a label and are not handled here are refused rather
//! than copied, but only once a frame has actually been inserted: an identity
//! rewrite stays able to process any module, and a rewrite that would need a
//! renumbering it cannot do fails loudly.

use anyhow::{anyhow, bail, Result};
use wasm_encoder::reencode::{Reencode, RoundtripReencoder};
use wasm_encoder::Instruction;
use wasmparser::{FuncValidator, Operator, ValidatorResources};

/// One function body being rewritten.
///
/// Held by the closure [`rewrite_module`] calls. State queries answer for the
/// point *before* the operator just handed over, which is where a transform
/// needs to know what is on the stack.
pub struct Cursor {
    validator: FuncValidator<ValidatorResources>,
    /// Instructions are encoded as they are emitted, but a rewrite does not
    /// know how many locals it needs until it has seen the whole body, and
    /// `wasm_encoder::Function` takes its locals up front. So this one is
    /// built with none, and the real declarations are prepended at the end.
    out: wasm_encoder::Function,
    /// Length of the empty locals declaration `out` starts with, so the
    /// instruction bytes can be separated from it without assuming its width.
    prefix: usize,
    /// Local declarations for the emitted body: the original groups, then one
    /// entry per reserved local.
    locals: Vec<(u32, wasm_encoder::ValType)>,
    /// Next local index to hand out, which is one past every original local.
    next_local: u32,
    /// Original control depths at which this rewrite has an extra frame open.
    /// An entry `d` means the inserted frame encloses every original frame
    /// from index `d` on, so a branch that targets an original frame below
    /// `d` has to cross it.
    inserted: Vec<u32>,
}

impl Cursor {
    /// How many frames of the *original* body are open, counting the implicit
    /// function frame. One at the start of a body.
    pub fn depth(&self) -> u32 {
        self.validator.control_stack_height()
    }

    /// How many values are on the operand stack, across the whole function.
    pub fn stack_height(&self) -> u32 {
        self.validator.operand_stack_height()
    }

    /// The type of the operand `depth` from the top, `0` being the top.
    ///
    /// `None` when `depth` is past the bottom of the stack; `Some(None)` for
    /// a value whose type is unknown, which happens only after unreachable
    /// code has poisoned the frame.
    pub fn operand(&self, depth: usize) -> Option<Option<wasmparser::ValType>> {
        self.validator.get_operand_type(depth)
    }

    /// The enclosing control frame `depth` levels out, `0` being innermost.
    pub fn frame(&self, depth: usize) -> Option<&wasmparser::Frame> {
        self.validator.get_control_frame(depth)
    }

    /// Add a local to the emitted body and return its index.
    ///
    /// Original local indices are untouched: reserved locals are appended
    /// after every declaration the body already had, so an operator copied
    /// from the input still names what it named.
    pub fn reserve_local(&mut self, ty: wasm_encoder::ValType) -> u32 {
        let index = self.next_local;
        self.next_local += 1;
        self.locals.push((1, ty));
        index
    }

    /// The module the function lives in, for looking up a callee's type.
    pub fn resources(&self) -> &ValidatorResources {
        self.validator.resources()
    }

    /// The function's own index, imports included.
    pub fn function_index(&self) -> u32 {
        self.validator.index()
    }

    /// The function's result types.
    ///
    /// Needed to wrap a body in a block: the wrapper has to produce what the
    /// function produces, or a branch that used to return cannot target it.
    pub fn results(&self) -> Result<Vec<wasmparser::ValType>> {
        Ok(self.signature()?.1)
    }

    /// The function's parameter and result types.
    pub fn signature(&self) -> Result<(Vec<wasmparser::ValType>, Vec<wasmparser::ValType>)> {
        use wasmparser::WasmModuleResources as _;
        let r = self.validator.resources();
        let index = self.validator.index();
        let ti = r
            .type_index_of_function(index)
            .ok_or_else(|| anyhow!("function {index} has no type"))?;
        let sub = r
            .sub_type_at(ti)
            .ok_or_else(|| anyhow!("type {ti} is not in the module"))?;
        match &sub.composite_type.inner {
            wasmparser::CompositeInnerType::Func(f) => {
                Ok((f.params().to_vec(), f.results().to_vec()))
            }
            other => bail!("function {index} has a non-function type {other:?}"),
        }
    }

    /// The type of local `index`, parameters included.
    pub fn local(&self, index: u32) -> Option<wasmparser::ValType> {
        self.validator.get_local_type(index)
    }

    /// Emit an operator from the original body, renumbering any branch label
    /// it carries so it still means what it meant.
    pub fn emit(&mut self, op: &Operator<'_>) -> Result<()> {
        if self.inserted.is_empty() {
            return self.reencode(op);
        }
        match op {
            Operator::Br { relative_depth } => {
                let l = self.remap(*relative_depth)?;
                self.out.instruction(&Instruction::Br(l));
            }
            Operator::BrIf { relative_depth } => {
                let l = self.remap(*relative_depth)?;
                self.out.instruction(&Instruction::BrIf(l));
            }
            Operator::BrTable { targets } => {
                let mut ls = Vec::with_capacity(targets.len() as usize);
                for t in targets.targets() {
                    ls.push(self.remap(t.map_err(|e| anyhow!("a br_table target: {e}"))?)?);
                }
                let default = self.remap(targets.default())?;
                self.out
                    .instruction(&Instruction::BrTable(ls.into(), default));
            }
            Operator::TryTable { try_table } => {
                // A catch label is resolved before the try_table's own frame
                // is pushed, so it is relative to the same depth as the
                // instruction itself -- the same arithmetic as a `br` here.
                let mut catches = Vec::with_capacity(try_table.catches.len());
                for c in &try_table.catches {
                    catches.push(match *c {
                        wasmparser::Catch::One { tag, label } => wasm_encoder::Catch::One {
                            tag,
                            label: self.remap(label)?,
                        },
                        wasmparser::Catch::OneRef { tag, label } => wasm_encoder::Catch::OneRef {
                            tag,
                            label: self.remap(label)?,
                        },
                        wasmparser::Catch::All { label } => wasm_encoder::Catch::All {
                            label: self.remap(label)?,
                        },
                        wasmparser::Catch::AllRef { label } => wasm_encoder::Catch::AllRef {
                            label: self.remap(label)?,
                        },
                    });
                }
                let ty = RoundtripReencoder
                    .block_type(try_table.ty)
                    .map_err(|e| anyhow!("re-encoding a try_table type: {e}"))?;
                self.out
                    .instruction(&Instruction::TryTable(ty, catches.into()));
            }
            // Label-carrying operators this module does not renumber. None is
            // emitted by ash's toolchain -- the first two are the legacy
            // exception proposal, the rest are the GC proposal -- and copying
            // one unchanged past an inserted frame is precisely the silent
            // wrong-jump this type exists to prevent.
            Operator::Rethrow { .. }
            | Operator::Delegate { .. }
            | Operator::BrOnNull { .. }
            | Operator::BrOnNonNull { .. }
            | Operator::BrOnCast { .. }
            | Operator::BrOnCastFail { .. } => {
                bail!("{op:?} carries a branch label this rewrite cannot renumber")
            }
            _ => self.reencode(op)?,
        }
        Ok(())
    }

    /// Emit an instruction of the rewrite's own. Not renumbered: a label here
    /// is written in the emitted numbering, which is the only numbering the
    /// caller can know for an instruction that was not in the input.
    pub fn emit_new(&mut self, instruction: &Instruction) {
        self.out.instruction(instruction);
    }

    /// Open a `block` that was not in the original body. Branches emitted
    /// from here until the matching [`Cursor::close_block`] are renumbered to
    /// cross it.
    pub fn open_block(&mut self, ty: wasm_encoder::BlockType) {
        self.out.instruction(&Instruction::Block(ty));
        self.inserted.push(self.depth());
    }

    /// Close the innermost inserted block.
    ///
    /// Refuses if the original body has since entered or left a frame, which
    /// would mean the inserted block does not nest with the original control
    /// structure and the emitted body could not validate.
    pub fn close_block(&mut self) -> Result<()> {
        let at = self
            .inserted
            .pop()
            .ok_or_else(|| anyhow!("close_block with no inserted block open"))?;
        if at != self.depth() {
            bail!(
                "an inserted block opened at original depth {at} is being closed at depth {}, \
                 so it does not nest with the original control structure",
                self.depth()
            );
        }
        self.out.instruction(&Instruction::End);
        Ok(())
    }

    /// How many inserted frames are open.
    pub fn inserted_frames(&self) -> usize {
        self.inserted.len()
    }

    fn reencode(&mut self, op: &Operator<'_>) -> Result<()> {
        let i = RoundtripReencoder
            .instruction(op.clone())
            .map_err(|e| anyhow!("re-encoding {op:?}: {e}"))?;
        self.out.instruction(&i);
        Ok(())
    }

    /// A branch label in the original numbering, in the emitted one.
    fn remap(&self, label: u32) -> Result<u32> {
        let cur = self.depth();
        // `br N` targets the frame `N` levels out from the innermost, which is
        // original frame index `cur - 1 - N`.
        let target = cur
            .checked_sub(1)
            .and_then(|c| c.checked_sub(label))
            .ok_or_else(|| anyhow!("branch label {label} escapes the function at depth {cur}"))?;
        Ok(label + self.inserted.iter().filter(|&&at| at > target).count() as u32)
    }
}

/// Rewrite every function body of a module.
///
/// `f` is called once per operator of every body, in order, with the function
/// index and a cursor whose state describes the point just before that
/// operator. A rewrite that wants the body unchanged calls
/// [`Cursor::emit`] and nothing else; anything it does not emit is dropped.
///
/// Every section other than the code section is copied byte for byte, so a
/// module whose bodies are only re-emitted differs from its input exactly
/// where `wasm_encoder` and ash choose different immediate widths.
///
/// The module is validated as it is read: a body is only handed to `f` after
/// the module context it needs has itself validated.
pub fn rewrite_module<F>(bytes: &[u8], mut f: F) -> Result<Vec<u8>>
where
    F: FnMut(u32, &mut Cursor, &Operator<'_>) -> Result<()>,
{
    /// A section of the output, in the order it appeared in the input.
    enum Piece {
        Raw(u8, std::ops::Range<usize>),
        Code,
    }

    let mut validator = wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all());
    let mut pieces: Vec<Piece> = Vec::new();
    let mut code = wasm_encoder::CodeSection::new();
    let mut allocs = wasmparser::FuncValidatorAllocations::default();

    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        let payload = payload.map_err(|e| anyhow!("parsing the module: {e}"))?;

        match &payload {
            wasmparser::Payload::CodeSectionStart { .. } => pieces.push(Piece::Code),
            wasmparser::Payload::CodeSectionEntry(_) => {}
            other => {
                if let Some((id, range)) = other.as_section() {
                    pieces.push(Piece::Raw(id, range.start as usize..range.end as usize));
                }
            }
        }

        let valid = validator
            .payload(&payload)
            .map_err(|e| anyhow!("validating the input module: {e}"))?;
        if let wasmparser::ValidPayload::Func(to_validate, body) = valid {
            // The validator knows the function index, imports included, so
            // nothing here has to re-derive it from the section order.
            let validator = to_validate.into_validator(allocs);
            let index = validator.index();
            let (raw, used) = rewrite_body(body, validator, |c, o| f(index, c, o))?;
            allocs = used;
            code.raw(&raw);
        }
    }

    let mut out = wasm_encoder::Module::new();
    for piece in &pieces {
        match piece {
            Piece::Raw(id, range) => {
                out.section(&wasm_encoder::RawSection {
                    id: *id,
                    data: &bytes[range.clone()],
                });
            }
            Piece::Code => {
                out.section(&code);
            }
        }
    }
    Ok(out.finish())
}

/// Drive one body through `f`, returning its new contents and the validator
/// allocations so the next body can reuse them.
fn rewrite_body<F>(
    body: wasmparser::FunctionBody<'_>,
    mut validator: FuncValidator<ValidatorResources>,
    mut f: F,
) -> Result<(Vec<u8>, wasmparser::FuncValidatorAllocations)>
where
    F: FnMut(&mut Cursor, &Operator<'_>) -> Result<()>,
{
    let mut locals = Vec::new();
    let mut reader = body
        .get_locals_reader()
        .map_err(|e| anyhow!("reading locals: {e}"))?;
    let offset = reader.original_position();
    for _ in 0..reader.get_count() {
        let (count, ty) = reader
            .read()
            .map_err(|e| anyhow!("reading a local declaration: {e}"))?;
        validator
            .define_locals(offset, count, ty)
            .map_err(|e| anyhow!("validating a local declaration: {e}"))?;
        locals.push((
            count,
            RoundtripReencoder
                .val_type(ty)
                .map_err(|e| anyhow!("re-encoding a local type: {e}"))?,
        ));
    }

    let params = {
        use wasmparser::WasmModuleResources as _;
        let r = validator.resources();
        let index = validator.index();
        let ti = r
            .type_index_of_function(index)
            .ok_or_else(|| anyhow!("function {index} has no type"))?;
        let sub = r
            .sub_type_at(ti)
            .ok_or_else(|| anyhow!("type {ti} is not in the module"))?;
        match &sub.composite_type.inner {
            wasmparser::CompositeInnerType::Func(f) => f.params().len() as u32,
            other => bail!("function {index} has a non-function type {other:?}"),
        }
    };
    let declared: u32 = locals.iter().map(|(n, _)| *n).sum();
    let out = wasm_encoder::Function::new([]);
    let prefix = out.byte_len();
    let mut cursor = Cursor {
        validator,
        out,
        prefix,
        locals,
        // Parameters occupy the low indices, then the declared locals; a
        // reserved local starts after both.
        next_local: params + declared,
        inserted: Vec::new(),
    };
    let mut ops = body
        .get_operators_reader()
        .map_err(|e| anyhow!("reading operators: {e}"))?;
    while !ops.eof() {
        let (op, offset) = ops
            .read_with_offset()
            .map_err(|e| anyhow!("decoding an operator: {e}"))?;
        // `f` sees the state before the operator; the validator advances past
        // it afterwards, so the two never disagree about where they are.
        f(&mut cursor, &op)?;
        cursor
            .validator
            .op(offset, &op)
            .map_err(|e| anyhow!("validating {op:?}: {e}"))?;
    }
    // The body's last `End` pops the function frame. Anything else means the
    // operator stream ended inside a block, which `wasmparser` would already
    // have rejected -- checked anyway, because everything downstream assumes
    // the frame arithmetic in `remap` was fed a complete body.
    if cursor.depth() != 0 {
        bail!(
            "a body ended with {} control frame(s) still open",
            cursor.depth()
        );
    }
    if !cursor.inserted.is_empty() {
        bail!(
            "{} inserted block(s) left open at the end of a body",
            cursor.inserted.len()
        );
    }
    let Cursor {
        out,
        prefix,
        locals,
        validator,
        ..
    } = cursor;
    // Split the instruction bytes off the empty locals declaration and put
    // them behind the real one.
    let encoded = out.into_raw_body();
    let mut body = wasm_encoder::Function::new(locals);
    body.raw(encoded[prefix..].iter().copied());
    Ok((body.into_raw_body(), validator.into_allocations()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_encoder::{BlockType, ValType};

    /// A module with one function whose body branches both ways: `br_if 0`
    /// stays inside a block, `br 1` leaves the function. Wrapping the body
    /// has to move the second and leave the first alone, and nothing but a
    /// disassembly will say whether it did.
    fn module() -> Vec<u8> {
        let mut types = wasm_encoder::TypeSection::new();
        types.ty().function([ValType::I32], [ValType::I32]);
        let mut funcs = wasm_encoder::FunctionSection::new();
        funcs.function(0);
        let mut exports = wasm_encoder::ExportSection::new();
        exports.export("f", wasm_encoder::ExportKind::Func, 0);

        let mut f = wasm_encoder::Function::new([]);
        f.instruction(&Instruction::Block(BlockType::Empty));
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::I32Eqz);
        f.instruction(&Instruction::BrIf(0)); // the block: must not move
        f.instruction(&Instruction::I32Const(7));
        f.instruction(&Instruction::Br(1)); // the function: must move
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::End);
        let mut code = wasm_encoder::CodeSection::new();
        code.function(&f);

        let mut m = wasm_encoder::Module::new();
        m.section(&types);
        m.section(&funcs);
        m.section(&exports);
        m.section(&code);
        m.finish()
    }

    fn ops(bytes: &[u8]) -> Vec<String> {
        let mut all = Vec::new();
        for payload in wasmparser::Parser::new(0).parse_all(bytes) {
            if let wasmparser::Payload::CodeSectionEntry(body) = payload.expect("parse") {
                let r = body.range();
                all.extend(
                    crate::body::operator_trace(&bytes[r.start as usize..r.end as usize])
                        .expect("trace"),
                );
            }
        }
        all
    }

    fn validate(bytes: &[u8]) -> Result<()> {
        wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
            .validate_all(bytes)
            .map(|_| ())
            .map_err(|e| anyhow!("{e}"))
    }

    #[test]
    fn an_identity_rewrite_changes_nothing() {
        let input = module();
        let out = rewrite_module(&input, |_, c, op| c.emit(op)).expect("rewrite");
        validate(&out).expect("the identity rewrite must validate");
        assert_eq!(ops(&input), ops(&out), "the operator stream changed");
    }

    #[test]
    fn wrapping_the_body_moves_only_the_branches_that_leave_it() {
        let input = module();
        let out = rewrite_module(&input, |_, c, op| {
            // The wrapper opens before the first operator and closes just
            // before the body's final `End`, which is the one that takes the
            // original depth from 1 to 0.
            if c.depth() == 1 && c.inserted_frames() == 0 {
                let results = c.results().expect("results");
                assert_eq!(results.len(), 1);
                c.open_block(BlockType::Result(ValType::I32));
            }
            if matches!(op, Operator::End) && c.depth() == 1 {
                c.close_block()?;
            }
            c.emit(op)
        })
        .expect("rewrite");
        validate(&out).expect("the wrapped body must validate");

        let after = ops(&out);
        assert_eq!(
            after.iter().filter(|o| o.starts_with("BrIf")).count(),
            1,
            "{after:?}"
        );
        assert!(
            after
                .iter()
                .any(|o| o.contains("BrIf { relative_depth: 0 }")),
            "the branch that stays inside must keep its label: {after:?}"
        );
        assert!(
            after.iter().any(|o| o.contains("Br { relative_depth: 2 }")),
            "the branch that leaves the body must cross the wrapper: {after:?}"
        );
        assert_eq!(
            after.iter().filter(|o| o.starts_with("Block")).count(),
            2,
            "one original block and one inserted: {after:?}"
        );
    }

    #[test]
    fn a_br_table_has_every_target_renumbered() {
        // `br_table 0 1` inside one block: the first target stays, the second
        // leaves the function and so must cross an inserted wrapper.
        let mut types = wasm_encoder::TypeSection::new();
        types.ty().function([ValType::I32], []);
        let mut funcs = wasm_encoder::FunctionSection::new();
        funcs.function(0);
        let mut f = wasm_encoder::Function::new([]);
        f.instruction(&Instruction::Block(BlockType::Empty));
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::BrTable(vec![0u32].into(), 1));
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::End);
        let mut code = wasm_encoder::CodeSection::new();
        code.function(&f);
        let mut m = wasm_encoder::Module::new();
        m.section(&types);
        m.section(&funcs);
        m.section(&code);
        let input = m.finish();

        let out = rewrite_module(&input, |_, c, op| {
            if c.depth() == 1 && c.inserted_frames() == 0 {
                c.open_block(BlockType::Empty);
            }
            if matches!(op, Operator::End) && c.depth() == 1 {
                c.close_block()?;
            }
            c.emit(op)
        })
        .expect("rewrite");
        validate(&out).expect("must validate");
        let after = ops(&out);
        let table = after
            .iter()
            .find(|o| o.starts_with("BrTable"))
            .unwrap_or_else(|| panic!("no br_table in {after:?}"));
        assert!(
            table.contains("default: 2"),
            "the default leaves the function and must cross the wrapper: {table}"
        );
    }

    #[test]
    fn a_block_closed_at_the_wrong_depth_is_refused() {
        let input = module();
        let err = rewrite_module(&input, |_, c, op| {
            if c.depth() == 1 && c.inserted_frames() == 0 {
                c.open_block(BlockType::Empty);
            }
            // Closing at depth 2 -- inside the original block -- would emit a
            // body whose frames interleave rather than nest.
            if c.depth() == 2 && c.inserted_frames() == 1 {
                c.close_block()?;
            }
            c.emit(op)
        })
        .expect_err("a mis-nested close must be refused");
        assert!(
            err.to_string().contains("does not nest"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn a_reserved_local_is_usable_and_leaves_the_original_indices_alone() {
        let input = module();
        // Copy the parameter into a fresh local and read it back out, so the
        // function still returns what it returned only if both the original
        // index 0 and the reserved index mean what they should.
        let mut done = false;
        let out = rewrite_module(&input, |_, c, op| {
            if !done {
                done = true;
                let l = c.reserve_local(ValType::I32);
                assert_eq!(l, 1, "one parameter, no declared locals");
                c.emit_new(&Instruction::LocalGet(0));
                c.emit_new(&Instruction::LocalSet(l));
                c.emit_new(&Instruction::LocalGet(l));
                c.emit_new(&Instruction::LocalSet(0));
            }
            c.emit(op)
        })
        .expect("rewrite");
        validate(&out).expect("a body with a reserved local must validate");
        let after = ops(&out);
        assert_eq!(
            after.iter().filter(|o| o.starts_with("LocalSet")).count(),
            2,
            "{after:?}"
        );
    }

    #[test]
    fn a_block_left_open_is_refused() {
        let input = module();
        let err = rewrite_module(&input, |_, c, op| {
            if c.depth() == 1 && c.inserted_frames() == 0 {
                c.open_block(BlockType::Empty);
            }
            c.emit(op)
        })
        .expect_err("an unclosed inserted block must be refused");
        assert!(err.to_string().contains("left open"), "unexpected: {err}");
    }
}
