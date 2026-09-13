//! Source positions of a serialized body, read off its [`Instr::Pos`]
//! markers.
//!
//! The serializer emits nothing for a marker, so the opcodes carry no
//! positions of their own; this maps each emitted pc back to the marker in
//! force where its instruction came from, together with the inline site the
//! instruction sits in. A consumer that walks the serialized opcodes -- the
//! interpreter naming a frame -- gets the same answer the compiled tiers
//! record for the same code.

use super::analysis::CfgInfo;
use super::ir::{Function, InlineSite, Instr};
use super::serialize::Serialized;

/// The position an emitted opcode came from.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PcPosition {
    /// Debug-file index, or -1 when no marker reached the instruction.
    pub file: i32,
    pub line: i32,
    /// The inline site the instruction was copied in through, an index into
    /// [`Function::inline_sites`]; `None` for the function's own code.
    pub site: Option<u32>,
}

impl PcPosition {
    pub const NONE: PcPosition = PcPosition {
        file: -1,
        line: 0,
        site: None,
    };
}

/// One position per serialized opcode of `ser`, from the markers of `f`.
///
/// A block that opens without a marker -- one a pass minted -- inherits the
/// position its first already-visited predecessor ended on, which is the
/// position the code it was split off from had.
pub fn positions_by_pc(f: &Function, ser: &Serialized) -> Vec<PcPosition> {
    if !has_markers(f) {
        return vec![PcPosition::NONE; ser.ops.len()];
    }
    positions_by_pc_with(f, ser, &CfgInfo::build(f))
}

/// Whether `f` carries any marker at all: a body lowered without positions
/// has nothing to read, and no table to build.
pub fn has_markers(f: &Function) -> bool {
    f.blocks
        .iter()
        .any(|b| b.instrs.iter().any(|i| matches!(i, Instr::Pos { .. })))
}

/// [`positions_by_pc`] with a CFG the caller already built.
pub fn positions_by_pc_with(f: &Function, ser: &Serialized, cfg: &CfgInfo) -> Vec<PcPosition> {
    let mut out = vec![PcPosition::NONE; ser.ops.len()];
    if f.blocks.is_empty() || !has_markers(f) {
        return out;
    }
    let mut exit_pos: Vec<Option<PcPosition>> = vec![None; f.blocks.len()];
    let mut set = |pc: usize, pos: PcPosition| {
        if let Some(slot) = out.get_mut(pc) {
            *slot = pos;
        }
    };
    for b in cfg.rpo() {
        let blk = &f.blocks[b.idx()];
        let mut cur = cfg
            .preds
            .get(b.idx())
            .into_iter()
            .flatten()
            .find_map(|p| exit_pos[p.idx()])
            .unwrap_or(PcPosition::NONE);
        for (k, ins) in blk.instrs.iter().enumerate() {
            if let Instr::Pos { file, line, site } = ins {
                cur = PcPosition {
                    file: i32::try_from(*file).unwrap_or(-1),
                    line: i32::try_from(*line).unwrap_or(0),
                    site: *site,
                };
                continue;
            }
            if let Some(pc) = ser.instr_pcs.get(b.idx()).and_then(|pcs| pcs.get(k)) {
                set(*pc, cur);
            }
        }
        if let Some(pc) = ser.term_pcs.get(b.idx()) {
            set(*pc, cur);
        }
        exit_pos[b.idx()] = Some(cur);
    }
    out
}

/// The frames a position stands for, innermost first: `(findex, file,
/// line)` for the position itself in the function it was written in, then
/// the call that inlined it into each enclosing function. `root` is the
/// function the body belongs to. A position outside inlined code is one
/// frame.
pub fn frames_of(pos: PcPosition, sites: &[InlineSite], root: u32) -> Vec<(u32, i32, i32)> {
    let mut frames = Vec::new();
    for_each_frame(pos, sites, root, |findex, file, line| {
        frames.push((findex, file, line))
    });
    frames
}

/// [`frames_of`] without the vector: `visit` sees each frame innermost
/// first. A trace is captured on every throw, so the common one-frame case
/// allocates nothing.
pub fn for_each_frame(
    pos: PcPosition,
    sites: &[InlineSite],
    root: u32,
    mut visit: impl FnMut(u32, i32, i32),
) {
    let callee_of = |s: Option<u32>| match s {
        Some(i) => sites
            .get(i as usize)
            .map(|st| st.callee)
            .filter(|c| *c != u32::MAX)
            .unwrap_or(root),
        None => root,
    };
    visit(callee_of(pos.site), pos.file, pos.line);
    let mut cur = pos.site;
    while let Some(i) = cur {
        let Some(st) = sites.get(i as usize) else {
            break;
        };
        visit(
            callee_of(st.parent),
            i32::try_from(st.file).unwrap_or(-1),
            i32::try_from(st.line).unwrap_or(0),
        );
        cur = st.parent;
    }
}
