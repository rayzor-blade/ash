//! The one map of compiled code both tiers write and everything else reads.
//!
//! Who owns a machine address is a question the crash handler, the sampling
//! profiler, the stub bridge and OSR bookkeeping all ask, and until this
//! existed each tier answered it by registering what it felt like -- an entry
//! point here, an OSR entry there -- into the profiler's list, which stored
//! points rather than ranges. A shared-module LLVM promotion emits thousands
//! of bodies of which only one was registered, and a crash in any other was
//! reported as the nearest registered function below it: a 12-opcode
//! accessor was blamed for a pc 43,320 bytes past its entry (2026-09-02).
//!
//! Rules: a tier registers EVERY body it emits, at emission, under the
//! body's own findex; it passes the size when it knows it (Cranelift does,
//! and a shared LLVM batch can derive one body's size from the next); and a
//! lookup answers by containment when a size is known, otherwise by the
//! nearest start bounded by the next one. `ASH_JIT_MAP=1` dumps it at exit.

use std::sync::{Mutex, OnceLock};

pub use crate::profile::Tier;

/// What a registered range is.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CodeKind {
    /// A function's normal entry point, installed for callers.
    Entry,
    /// A body a tier emitted alongside something else -- a callee copied into
    /// a promotion module, or one of a shared batch. Callable at this address
    /// by anything holding it, but not what `functions_ptrs` names.
    Body,
    /// An OSR entry: enters mid-function from a transfer buffer.
    OsrEntry,
}

#[derive(Clone, Copy, Debug)]
pub struct CodeRange {
    pub start: usize,
    /// 0 when the tier could not say.
    pub size: usize,
    pub findex: u32,
    pub tier: Tier,
    pub kind: CodeKind,
    /// Source runs over this body, sorted by offset. Empty unless the tier
    /// recorded a map, which it does only when positions were asked for.
    pub positions: &'static [SourceRun],
}

/// One run of this body's code that came from a single source position.
///
/// Offsets are from the body's start, matching what [`Hit`] reports.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SourceRun {
    pub start: u32,
    pub end: u32,
    /// A position packed by [`pack_position`].
    pub packed: u32,
}

/// Pack `(file, line)` into the 32 bits a Cranelift `SourceLoc` carries.
///
/// Cranelift does not interpret those bits, but it reserves the all-ones
/// pattern for "no location", so a packed position must never be that. Twelve
/// bits of debug-file index and twenty of line covers every module ash emits;
/// outside that range this records nothing, because a wrong line is worse
/// than the function's own.
pub fn pack_position(file: u32, line: u32) -> Option<u32> {
    // `file + 1`, so 0 stays available as "no position".
    if file >= 0xFFE || line >= (1 << 20) {
        return None;
    }
    Some(((file + 1) << 20) | line)
}

/// The inverse. None for 0, which is what an unlabelled run holds.
pub fn unpack_position(packed: u32) -> Option<(u32, u32)> {
    let file = packed >> 20;
    Some((file.checked_sub(1)?, packed & 0xF_FFFF))
}

/// A lookup result: the range and how far into it the address is.
#[derive(Clone, Copy, Debug)]
pub struct Hit {
    pub range: CodeRange,
    pub offset: usize,
}

fn map() -> &'static Mutex<Vec<CodeRange>> {
    static M: OnceLock<Mutex<Vec<CodeRange>>> = OnceLock::new();
    M.get_or_init(|| Mutex::new(Vec::new()))
}

/// Record a range. Idempotent for an identical start.
pub fn register(findex: u32, tier: Tier, kind: CodeKind, start: usize, size: usize) {
    if start == 0 {
        return;
    }
    let mut m = map().lock().unwrap();
    let at = m.partition_point(|r| r.start < start);
    if let Some(r) = m.get_mut(at) {
        if r.start == start {
            // Same code seen again -- keep the better-informed record. A body
            // later installed as the function's entry IS the entry.
            if r.size == 0 {
                r.size = size;
            }
            if kind == CodeKind::Entry {
                r.kind = CodeKind::Entry;
            }
            return;
        }
    }
    m.insert(
        at,
        CodeRange {
            start,
            size,
            findex,
            tier,
            kind,
            positions: &[],
        },
    );
}

/// Which range contains `pc`, with the offset into it.
///
/// Containment when the size is known. Otherwise the nearest start at or
/// below `pc`, bounded by the next registered start -- and by `MAX_SLACK`
/// when there is none, because an unbounded nearest-below would claim every
/// address above the last function for it.
pub fn lookup(pc: usize) -> Option<Hit> {
    const MAX_SLACK: usize = 256 << 10;
    let m = map().try_lock().ok()?;
    let at = m.partition_point(|r| r.start <= pc);
    let r = *m.get(at.checked_sub(1)?)?;
    let offset = pc - r.start;
    let bound = if r.size > 0 {
        r.size
    } else {
        m[at..]
            .iter()
            .find(|n| n.start > r.start)
            .map(|n| n.start - r.start)
            .unwrap_or(MAX_SLACK)
            .min(MAX_SLACK)
    };
    (offset < bound).then_some(Hit { range: r, offset })
}

/// Attach a source map to the body registered at `start`.
///
/// Separate from `register` because only a tier that was asked for positions
/// produces one, and because the map is known a moment later -- the code has
/// to be emitted before its offsets exist.
///
/// Leaked: a reader may be walking a stack while another thread compiles, and
/// the map has to outlive every reader that has seen the range.
pub fn set_positions(start: usize, runs: Vec<SourceRun>) {
    if start == 0 || runs.is_empty() {
        return;
    }
    let Ok(mut m) = map().lock() else { return };
    let at = m.partition_point(|r| r.start < start);
    if let Some(r) = m.get_mut(at) {
        if r.start == start && r.positions.is_empty() {
            r.positions = Box::leak(runs.into_boxed_slice());
        }
    }
}

/// The `(file, line)` recorded for `pc`, when its tier recorded a map.
///
/// `pc` is taken as a RETURN address, which is what a stack walk yields: the
/// address itself belongs to whatever follows the call, so the byte before it
/// is what names the call. A frame with no run covering it gets nothing, and
/// the caller falls back to the function's own entry position.
pub fn position_of(pc: usize) -> Option<(u32, u32)> {
    // Blocking, unlike `lookup`. That one answers the crash handler, which
    // cannot afford to wait on a lock a faulting thread may hold; this one
    // runs while an ordinary trace is being built, and a `try_lock` that lost
    // to a compile on another thread would drop the line and report the
    // function's entry instead -- intermittently, and only under load.
    let hit = {
        let m = map().lock().ok()?;
        let at = m.partition_point(|r| r.start <= pc);
        let r = *m.get(at.checked_sub(1)?)?;
        let offset = pc.checked_sub(r.start)?;
        if r.size > 0 && offset >= r.size {
            return None;
        }
        Hit { range: r, offset }
    };
    let offset = u32::try_from(hit.offset).ok()?.saturating_sub(1);
    let runs = hit.range.positions;
    let at = runs
        .partition_point(|run| run.start <= offset)
        .checked_sub(1)?;
    let run = runs[at];
    if offset >= run.end {
        return None;
    }
    unpack_position(run.packed)
}

/// The findex whose code starts exactly at `addr`.
pub fn at_start(addr: usize) -> Option<u32> {
    if addr == 0 {
        return None;
    }
    let m = map().lock().ok()?;
    let at = m.partition_point(|r| r.start < addr);
    m.get(at).filter(|r| r.start == addr).map(|r| r.findex)
}

/// Every range, in address order.
pub fn snapshot() -> Vec<CodeRange> {
    map().lock().map(|m| m.clone()).unwrap_or_default()
}

/// Whether `ASH_JIT_MAP` asked for the map at exit.
pub fn dump_wanted() -> bool {
    std::env::var("ASH_JIT_MAP").is_ok_and(|v| v != "0" && !v.is_empty())
}

/// The map as text, one range per line, for the exit dump and for tests.
pub fn dump(name_of: impl Fn(u32) -> Option<String>) -> Vec<String> {
    let m = snapshot();
    let mut out = vec![format!("[jit-map] {} ranges", m.len())];
    for r in &m {
        out.push(format!(
            "  {:#014x} {:>7} {:<9} {:<8} findex={:<6} {}",
            r.start,
            if r.size > 0 {
                r.size.to_string()
            } else {
                "?".into()
            },
            match r.tier {
                Tier::Cranelift => "cranelift",
                Tier::Llvm => "llvm",
            },
            match r.kind {
                CodeKind::Entry => "entry",
                CodeKind::Body => "body",
                CodeKind::OsrEntry => "osr",
            },
            r.findex,
            name_of(r.findex).unwrap_or_default()
        ));
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    // The map is process-wide and tests run concurrently, so each test keeps
    // to its own address neighbourhood.
    const A: usize = 0x7f00_0000_0000;
    const B: usize = 0x7f10_0000_0000;
    const C: usize = 0x7f20_0000_0000;

    #[test]
    fn a_sized_range_answers_by_containment_and_nothing_past_it() {
        register(1, Tier::Llvm, CodeKind::Body, A, 0x100);
        register(2, Tier::Llvm, CodeKind::Body, A + 0x1000, 0x100);
        let hit = lookup(A + 0x40).expect("inside the first");
        assert_eq!((hit.range.findex, hit.offset), (1, 0x40));
        // Past the first body's size and before the second: nobody's.
        assert!(lookup(A + 0x200).is_none());
        let hit = lookup(A + 0x1000).expect("start of the second");
        assert_eq!(hit.range.findex, 2);
    }

    #[test]
    fn a_packed_position_round_trips_and_never_collides_with_cranelift() {
        for (file, line) in [(0, 0), (0, 1), (1, 0), (7, 42), (0xFFD, 0xFFFFE)] {
            let packed = pack_position(file, line).expect("in range");
            // Zero is "no position" and all-ones is Cranelift's own default,
            // so a real position must be neither.
            assert_ne!(packed, 0, "{file}:{line} packed to the empty marker");
            assert_ne!(
                packed,
                u32::MAX,
                "{file}:{line} packed to SourceLoc's default"
            );
            assert_eq!(unpack_position(packed), Some((file, line)));
        }
        // Out of range records nothing rather than a wrong line.
        assert_eq!(pack_position(0xFFE, 1), None);
        assert_eq!(pack_position(1, 1 << 20), None);
        assert_eq!(unpack_position(0), None);
    }

    #[test]
    fn a_position_answers_for_the_run_that_covers_the_call() {
        const D: usize = 0x7f30_0000_0000;
        register(21, Tier::Cranelift, CodeKind::Entry, D, 0x100);
        set_positions(
            D,
            vec![
                SourceRun {
                    start: 0,
                    end: 0x20,
                    packed: pack_position(3, 10).unwrap(),
                },
                SourceRun {
                    start: 0x20,
                    end: 0x40,
                    packed: pack_position(3, 11).unwrap(),
                },
                // A gap at 0x40..0x60 that no run covers.
                SourceRun {
                    start: 0x60,
                    end: 0x80,
                    packed: pack_position(4, 99).unwrap(),
                },
            ],
        );
        // A return address is taken as belonging to the run BEFORE it, so an
        // address one past a run's end resolves to that run and not the next.
        assert_eq!(position_of(D + 0x20), Some((3, 10)));
        assert_eq!(position_of(D + 0x21), Some((3, 11)));
        assert_eq!(position_of(D + 0x40), Some((3, 11)));
        // Inside the gap: nothing, so the caller falls back to the entry.
        assert_eq!(position_of(D + 0x55), None);
        assert_eq!(position_of(D + 0x70), Some((4, 99)));
        // A body with no map at all answers nothing.
        register(22, Tier::Cranelift, CodeKind::Entry, D + 0x1000, 0x100);
        assert_eq!(position_of(D + 0x1040), None);
    }

    #[test]
    fn an_unsized_range_is_bounded_by_the_next_start() {
        register(11, Tier::Cranelift, CodeKind::Entry, B, 0);
        register(12, Tier::Cranelift, CodeKind::Entry, B + 0x800, 0);
        assert_eq!(lookup(B + 0x7ff).unwrap().range.findex, 11);
        assert_eq!(lookup(B + 0x800).unwrap().range.findex, 12);
        // The last one is bounded by the slack, not unbounded.
        assert!(lookup(B + 0x800 + (256 << 10)).is_none());
    }

    #[test]
    fn a_body_later_installed_as_the_entry_becomes_the_entry() {
        register(21, Tier::Llvm, CodeKind::Body, C, 0x40);
        register(21, Tier::Llvm, CodeKind::Entry, C, 0);
        let r = lookup(C).unwrap().range;
        assert_eq!((r.kind, r.size), (CodeKind::Entry, 0x40));
        assert_eq!(at_start(C), Some(21));
        assert_eq!(at_start(C + 1), None);
    }
}
