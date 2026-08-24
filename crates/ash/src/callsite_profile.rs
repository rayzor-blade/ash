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
//! Keys are `(caller findex, opcode index)` into the AIR-optimized ops array
//! — the interpreter executes that same array (`air::Cache::body`) and the
//! promote path lowers it from the same shared cache, so the indices agree
//! by construction.
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
