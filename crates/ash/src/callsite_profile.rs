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
