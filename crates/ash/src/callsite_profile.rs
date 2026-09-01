//! What each indirect call site actually called, observed by the interpreter.
//!
//! The JVM beats ash on exactly two call shapes — closure_call by 2.11x and
//! method_call by 1.59x — and the measured cost is not the dispatch loads
//! (hoisting them bought +0.1%) but the indirect call itself: a monomorphic
//! site there gets an inline cache and the callee inlined, so the loop body
//! carries NO call. The interpreter runs every hot site thousands of times
//! before its caller promotes, which is a profile nobody has to pay extra
//! for; this module is where it accumulates, and the LLVM tier's lowering is
//! where it turns into a guarded direct call the inliner can take.
//!
//! Keys are `(caller findex, opcode index)` into the serialized AIR V2 body
//! executed by the flat interpreter. Backends that still have that index use
//! the exact observation. Direct AIR V2 backends do not flatten their SSA CFG,
//! so they may use a caller-wide observation only when every recorded site in
//! that caller agrees on one monomorphic shape.
//!
//! A site is either monomorphic (one target ever observed) or poisoned. The
//! record is advisory: the emitted guard re-checks the target at runtime and
//! falls into the ordinary indirect path on a miss, so a stale or wrong entry
//! costs a compare, never correctness.

use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};

#[derive(Clone, Copy, PartialEq, Eq)]
enum Obs {
    /// One shape ever seen. For closures: the callee findex and whether the
    /// closure carried a bound value. For methods: the receiver's runtime
    /// `hl_type*` (stable for the process — the converter leaks them) and
    /// the findex its vtable slot resolved to.
    Mono(u64, u32),
    Poly,
}

fn key(caller: u32, pc: u32) -> u64 {
    ((caller as u64) << 32) | pc as u64
}

fn closure_sites() -> &'static Mutex<HashMap<u64, Obs>> {
    static M: OnceLock<Mutex<HashMap<u64, Obs>>> = OnceLock::new();
    M.get_or_init(|| Mutex::new(HashMap::new()))
}

fn method_sites() -> &'static Mutex<HashMap<u64, Obs>> {
    static M: OnceLock<Mutex<HashMap<u64, Obs>>> = OnceLock::new();
    M.get_or_init(|| Mutex::new(HashMap::new()))
}

fn record(map: &Mutex<HashMap<u64, Obs>>, caller: u32, pc: u32, a: u64, b: u32) {
    let mut m = map.lock().expect("callsite profile mutex poisoned");
    let e = m.entry(key(caller, pc)).or_insert(Obs::Mono(a, b));
    if *e != Obs::Mono(a, b) {
        *e = Obs::Poly;
    }
}

fn lookup(map: &Mutex<HashMap<u64, Obs>>, caller: u32, pc: u32) -> Option<(u64, u32)> {
    match map
        .lock()
        .expect("callsite profile mutex poisoned")
        .get(&key(caller, pc))
    {
        Some(Obs::Mono(a, b)) => Some((*a, *b)),
        _ => None,
    }
}

/// Return a caller-wide observation only when every recorded site in that
/// caller has the same monomorphic shape.
///
/// AIR V2 owns its SSA CFG and does not retain the flat serializer's opcode
/// index. A backend consuming AIR V2 therefore cannot use that index as a
/// stable site identity. The conservative caller-wide fallback recovers the
/// common one-dispatch-site case without guessing: disagreement or a single
/// polymorphic site disables the optimization. The generated fast arm still
/// carries its ordinary runtime guard.
fn lookup_uniform_caller(map: &Mutex<HashMap<u64, Obs>>, caller: u32) -> Option<(u64, u32)> {
    let m = map.lock().expect("callsite profile mutex poisoned");
    let mut uniform = None;
    for (&site, &observation) in m.iter() {
        if (site >> 32) as u32 != caller {
            continue;
        }
        let Obs::Mono(a, b) = observation else {
            return None;
        };
        match uniform {
            None => uniform = Some((a, b)),
            Some(previous) if previous == (a, b) => {}
            Some(_) => return None,
        }
    }
    uniform
}

/// The interpreter saw `caller`'s CallClosure at `pc` invoke bytecode
/// function `target`, with (`has_value`) or without a bound value.
pub fn record_closure(caller: u32, pc: u32, target: u32, has_value: bool) {
    record(closure_sites(), caller, pc, target as u64, has_value as u32);
}

/// The single (target findex, has_value) this closure site has ever called,
/// or `None` when the site is unseen or polymorphic.
pub fn closure_target(caller: u32, pc: u32) -> Option<(u32, bool)> {
    lookup(closure_sites(), caller, pc).map(|(t, hv)| (t as u32, hv != 0))
}

/// The uniform closure target observed across all sites in `caller`.
pub fn uniform_closure_target(caller: u32) -> Option<(u32, bool)> {
    lookup_uniform_caller(closure_sites(), caller).map(|(t, hv)| (t as u32, hv != 0))
}

/// The interpreter saw `caller`'s CallMethod/CallThis at `pc` dispatch on a
/// receiver whose runtime type header is `type_ptr`, resolving to `target`.
pub fn record_method(caller: u32, pc: u32, type_ptr: u64, target: u32) {
    record(method_sites(), caller, pc, type_ptr, target);
}

/// The single (receiver `hl_type*`, target findex) this method site has ever
/// dispatched on, or `None` when the site is unseen or polymorphic.
pub fn method_receiver(caller: u32, pc: u32) -> Option<(u64, u32)> {
    lookup(method_sites(), caller, pc)
}

/// The uniform method receiver and target observed across all sites in
/// `caller`.
pub fn uniform_method_receiver(caller: u32) -> Option<(u64, u32)> {
    lookup_uniform_caller(method_sites(), caller)
}

/// The same observation, in a form that survives leaving the process.
///
/// Two things have to change for an observation to outlive its process.
///
/// The ANCHOR. In-process, a method site is keyed on the receiver's runtime
/// `hl_type*`, which is right for the JIT -- it compiles inside the process
/// that watched the dispatch -- and meaningless anywhere else. The AOT guard
/// compares the vtable SLOT it already loaded against a function it can name,
/// so no runtime address is needed.
///
/// The IDENTITY. A findex is a POSITION, and positions move: adding a class
/// nobody calls shifted this benchmark's caller from 26 to 28 and silently
/// cost the whole optimisation. So a written profile names functions by
/// `HLFunction::compute_hash` -- a hash of the signature, register types and
/// opcode stream, with debug info deliberately excluded so relocating a
/// function or renumbering around it changes nothing. Two identical functions
/// collide, which costs nothing: the emitted guard re-checks the target at run
/// time, so a collision can only fail to fire.
///
/// Sites are keyed by CALLER, not by call site. The exact key is an opcode
/// index into the flat interpreter's AIR V2 body and the LLVM backend does not
/// flatten its CFG, so the exact form never matched anything; the caller-wide
/// observation is what actually fires.
fn aot_method_targets() -> &'static Mutex<HashMap<String, String>> {
    static M: OnceLock<Mutex<HashMap<String, String>>> = OnceLock::new();
    M.get_or_init(|| Mutex::new(HashMap::new()))
}

static PROFILE_HITS: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

/// Every caller whose recorded method sites all agree on one target, as
/// `(caller findex, target findex)`. Callers that disagree are omitted --
/// there is nothing to say about them, and saying it would emit a guard that
/// always misses.
pub fn monomorphic_callers() -> Vec<(u32, u32)> {
    let m = method_sites().lock().expect("callsite profile mutex poisoned");
    // Polymorphic sites are IGNORED rather than poisoning their caller.
    //
    // A caller whose eleven monomorphic sites agree and whose twelfth is
    // polymorphic still has a target worth guessing at those eleven. Refusing
    // the whole caller cost deltablue every one of its twelve guards for the
    // sake of one site. The guard re-reads the real target at run time, so a
    // site this guess does not fit falls into the ordinary indirect path and
    // pays one compare -- the same price a cache miss costs anywhere.
    let mut per_caller: HashMap<u32, Option<u32>> = HashMap::new();
    for (&k, obs) in m.iter() {
        let Obs::Mono(_, target) = obs else { continue };
        per_caller
            .entry((k >> 32) as u32)
            .and_modify(|e| {
                if *e != Some(*target) {
                    *e = None; // its monomorphic sites disagree: nothing to say
                }
            })
            .or_insert(Some(*target));
    }
    let mut out: Vec<(u32, u32)> = per_caller
        .into_iter()
        .filter_map(|(c, t)| t.map(|t| (c, t)))
        .collect();
    out.sort_unstable();
    out
}

/// Render a profile, naming functions by the stable name `to_name` supplies.
/// A function with no such name -- a closure, say -- is skipped rather than
/// written by index, which would look durable and not be.
pub fn render_profile(to_name: impl Fn(u32) -> Option<String>) -> String {
    let mut lines: Vec<String> = monomorphic_callers()
        .into_iter()
        .filter_map(|(caller, target)| Some((to_name(caller)?, to_name(target)?)))
        .map(|(c, t)| format!("m {c} {t}"))
        .collect();
    lines.sort(); // a profile that reorders is a profile that diffs
    let mut s = String::from("# ash callsite profile v3 (caller target, by name)\n");
    s.push_str(&lines.join("\n"));
    s.push('\n');
    s
}

/// Read what [`render_profile`] wrote. Unparseable lines are skipped: the
/// record is advisory, and a guard that never fires beats refusing to compile.
pub fn load_profile(text: &str) -> usize {
    let mut m = aot_method_targets()
        .lock()
        .expect("callsite profile mutex poisoned");
    let mut n = 0;
    for line in text.lines() {
        let mut it = line.split_whitespace();
        if it.next() != Some("m") {
            continue;
        }
        let (Some(c), Some(t)) = (it.next(), it.next()) else {
            continue;
        };
        m.insert(c.to_string(), t.to_string());
        n += 1;
    }
    n
}

/// The target a loaded profile associates with this caller.
pub fn aot_target_for(caller: &str) -> Option<String> {
    let hit = aot_method_targets()
        .lock()
        .expect("callsite profile mutex poisoned")
        .get(caller)
        .cloned();
    if hit.is_some() {
        PROFILE_HITS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    }
    hit
}

/// How many loaded entries were asked for and found. Zero against a non-empty
/// profile means every entry is stale -- the case that used to be silent.
pub fn aot_profile_hits() -> usize {
    PROFILE_HITS.load(std::sync::atomic::Ordering::Relaxed)
}

pub fn aot_profile_size() -> usize {
    aot_method_targets()
        .lock()
        .expect("callsite profile mutex poisoned")
        .len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn caller_fallback_requires_one_uniform_monomorphic_shape() {
        let map = Mutex::new(HashMap::from([
            (key(41, 3), Obs::Mono(7, 9)),
            (key(41, 12), Obs::Mono(7, 9)),
            (key(42, 1), Obs::Mono(8, 10)),
        ]));
        assert_eq!(lookup_uniform_caller(&map, 41), Some((7, 9)));

        map.lock()
            .expect("test profile mutex poisoned")
            .insert(key(41, 20), Obs::Mono(7, 11));
        assert_eq!(lookup_uniform_caller(&map, 41), None);

        let polymorphic = Mutex::new(HashMap::from([(key(43, 0), Obs::Poly)]));
        assert_eq!(lookup_uniform_caller(&polymorphic, 43), None);
        assert_eq!(lookup_uniform_caller(&polymorphic, 44), None);
    }
}
