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
            if r.size > 0 { r.size.to_string() } else { "?".into() },
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
