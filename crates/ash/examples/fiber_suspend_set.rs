//! How much of a program would a link-time fiber transform have to instrument?
//!
//! The wasm-level answer is discouraging and is recorded in
//! `docs/wasm-fibers.md`: an indirect call there can reach any address-taken
//! function of a matching wasm type, and a Haxe program has thousands of
//! functions sharing a few dozen signatures, so the suspend set closes over
//! 86% of the module whatever the seeds are.
//!
//! This asks the same question one level up, where ash still knows what wasm
//! erases. A virtual call names a vtable SLOT, so it can only reach the
//! protos in that slot; a closure call can only reach a function some
//! `*Closure` opcode actually turned into a closure. Neither fact survives
//! into the wasm type.
//!
//! Usage: fiber_suspend_set <file.hl>
//!
//! It prints the closure under three edge policies -- direct calls only, the
//! HL-level over-approximation, and "every indirect call reaches anything",
//! which is what the wasm-level analysis is forced into. The middle number is
//! the one that decides whether the transform is affordable.

use std::collections::{HashMap, HashSet};

use ash_core::bytecode::BytecodeDecoder;
use ash_core::opcodes::Opcode;

/// Natives a fiber can be suspended inside. These are the operations that
/// park a Haxe thread; everything else reaches them through the call graph.
const BLOCKING: &[&str] = &[
    "deque_pop",
    "lock_wait",
    "condition_wait",
    "condition_timed_wait",
    "mutex_acquire",
    "thread_sleep",
    "sys_sleep",
];

fn main() -> anyhow::Result<()> {
    let path = std::env::args()
        .nth(1)
        .expect("usage: fiber_suspend_set <file.hl>");
    // The decoder hashes field names through the runtime.
    ash_core::native_lib::init_std_library()?;
    let bc = BytecodeDecoder::decode(std::path::Path::new(&path))?;

    // findex -> index into `functions`, so an edge can name a callee.
    let mut by_findex: HashMap<i32, usize> = HashMap::new();
    for (i, f) in bc.functions.iter().enumerate() {
        by_findex.insert(f.findex, i);
    }

    // Every proto in the program grouped by its vtable slot. A CallMethod or
    // CallThis names a slot, so this set is everything it can reach -- the
    // whole point of asking at this level rather than at the wasm one.
    let mut by_slot: HashMap<i32, HashSet<i32>> = HashMap::new();
    let mut protos = 0usize;
    for ty in &bc.types {
        let Some(obj) = &ty.obj else { continue };
        for p in &obj.proto {
            protos += 1;
            by_slot.entry(p.pindex).or_default().insert(p.findex);
        }
    }

    // Functions some opcode actually turns into a closure. A CallClosure can
    // reach these and nothing else. Bindings count: a `static dynamic`
    // function is installed into a field by the runtime.
    let mut closure_targets: HashSet<i32> = HashSet::new();
    for ty in &bc.types {
        let Some(obj) = &ty.obj else { continue };
        for pair in obj.bindings.chunks(2) {
            if let [_field, findex] = pair {
                closure_targets.insert(*findex);
            }
        }
    }
    let mut direct: Vec<HashSet<i32>> = vec![HashSet::new(); bc.functions.len()];
    let mut virtual_slots: Vec<HashSet<i32>> = vec![HashSet::new(); bc.functions.len()];
    let mut calls_closure = vec![false; bc.functions.len()];
    for (i, f) in bc.functions.iter().enumerate() {
        for op in &f.ops {
            match op {
                Opcode::Call0 { fun, .. }
                | Opcode::Call1 { fun, .. }
                | Opcode::Call2 { fun, .. }
                | Opcode::Call3 { fun, .. }
                | Opcode::Call4 { fun, .. }
                | Opcode::CallN { fun, .. } => {
                    direct[i].insert(fun.0 as i32);
                }
                Opcode::CallMethod { field, .. } | Opcode::CallThis { field, .. } => {
                    virtual_slots[i].insert(field.0 as i32);
                }
                Opcode::CallClosure { .. } => calls_closure[i] = true,
                Opcode::StaticClosure { fun, .. } => {
                    closure_targets.insert(fun.0 as i32);
                }
                Opcode::InstanceClosure { fun, .. } => {
                    closure_targets.insert(fun.0 as i32);
                }
                Opcode::VirtualClosure { field, .. } => {
                    if let Some(s) = by_slot.get(&(field.0 as i32)) {
                        closure_targets.extend(s.iter().copied());
                    }
                }
                _ => {}
            }
        }
    }

    // Seeds: anything that calls a blocking native directly.
    let blocking_findex: HashSet<i32> = bc
        .natives
        .iter()
        .filter(|n| BLOCKING.iter().any(|b| n.name.contains(b)))
        .map(|n| n.findex)
        .collect();
    let seeds: HashSet<usize> = (0..bc.functions.len())
        .filter(|&i| direct[i].iter().any(|c| blocking_findex.contains(c)))
        .collect();

    let total = bc.functions.len();
    println!("{path}");
    println!(
        "  functions {total}, protos {protos}, vtable slots {}, closure targets {}",
        by_slot.len(),
        closure_targets.len()
    );
    println!("  blocking natives {}, seed functions {}", blocking_findex.len(), seeds.len());

    // Slot fan-out is the number the whole idea rests on: a virtual call
    // reaches this many functions, where the wasm analysis reaches every
    // address-taken function sharing a signature.
    let mut fan: Vec<usize> = by_slot.values().map(|s| s.len()).collect();
    fan.sort_unstable();
    if !fan.is_empty() {
        let sum: usize = fan.iter().sum();
        println!(
            "  vtable slot fan-out: median {}, mean {:.1}, p95 {}, max {}",
            fan[fan.len() / 2],
            sum as f64 / fan.len() as f64,
            fan[fan.len() * 95 / 100],
            fan[fan.len() - 1]
        );
    }

    for (label, hl_edges, anything) in [
        ("direct calls only", false, false),
        ("HL-level (slots + closure set)", true, false),
        ("every indirect reaches anything", true, true),
    ] {
        let mut set = seeds.clone();
        loop {
            let suspending: HashSet<i32> = set.iter().map(|&i| bc.functions[i].findex).collect();
            // Which vtable slots hold a suspending implementation, and can a
            // closure call reach one?
            let hot_slots: HashSet<i32> = by_slot
                .iter()
                .filter(|(_, fs)| fs.iter().any(|f| suspending.contains(f)))
                .map(|(s, _)| *s)
                .collect();
            let closure_hot = closure_targets.iter().any(|f| suspending.contains(f));
            let mut grew = false;
            for i in 0..total {
                if set.contains(&i) {
                    continue;
                }
                let by_direct = direct[i].iter().any(|c| suspending.contains(c));
                let by_virtual = hl_edges && !virtual_slots[i].is_disjoint(&hot_slots);
                let by_closure = calls_closure[i] && (anything || (hl_edges && closure_hot));
                let by_any = anything && (!virtual_slots[i].is_empty() || calls_closure[i]);
                if by_direct || by_virtual || by_closure || by_any {
                    set.insert(i);
                    grew = true;
                }
            }
            if !grew {
                break;
            }
        }
        println!(
            "  {label:34} {:5} / {total}  ({:5.1}%)",
            set.len(),
            100.0 * set.len() as f64 / total as f64
        );
    }
    Ok(())
}
