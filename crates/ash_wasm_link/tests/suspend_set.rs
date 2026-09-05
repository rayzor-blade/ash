//! How much of a real module a fiber transform would have to instrument.
//!
//! `docs/wasm-fibers.md` records this number, and it decides whether the
//! transform is worth building: every instrumented function pays code size and
//! a state check on every call. The figure there was first obtained with a
//! throwaway script, which is not a thing anyone can re-run after the emitter
//! changes. This is the same measurement against the crate's own decoder, so
//! the claim stays checkable.
//!
//! Point it at a linked module:
//!
//! ```text
//! ASH_LINK_TEST_MODULE=/path/to/prog.wasm cargo test -p ash_wasm_link
//! ```
//!
//! Without the variable it says what it wanted and passes, like the other
//! tests here.

use std::collections::BTreeSet;
use std::path::PathBuf;

use ash_wasm_link::suspend::{program_from_module, Policy, Program};

/// Imports a fiber can be suspended inside.
///
/// `ash_host_fiber_yield` is the suspend point proper -- the host call a
/// parked fiber goes out through. The rest are the wasi calls that block a
/// real thread, and so are the places a fiber scheduler would have to take
/// over: they are measured separately because whether ash routes them through
/// a yield is a runtime decision, and the difference between the two seed sets
/// is exactly what that decision costs.
const YIELD: &[&str] = &["ash_host_fiber_yield"];
const BLOCKING: &[&str] = &[
    "poll_oneoff",
    "sched_yield",
    "sock_accept",
    "sock_recv",
    "sock_send",
    "ash_host_socket_accept",
    "ash_host_socket_connect",
    "ash_host_socket_recv",
    "ash_host_socket_send",
    "ash_host_socket_poll",
];

/// Import function indices whose name matches one of `names`.
///
/// Imported functions take the low indices in order, so this walks the import
/// section counting only the function imports, the same way
/// `program_from_module` assigns them.
fn imports_named(bytes: &[u8], names: &[&str]) -> BTreeSet<u32> {
    let mut found = BTreeSet::new();
    let mut next = 0u32;
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        let wasmparser::Payload::ImportSection(r) = payload.expect("parsing") else {
            continue;
        };
        for group in r {
            for import in group.expect("imports") {
                let (_, import) = import.expect("an import");
                if matches!(import.ty, wasmparser::TypeRef::Func(_)) {
                    if names.contains(&import.name) {
                        found.insert(next);
                    }
                    next += 1;
                }
            }
        }
    }
    found
}

fn report(p: &Program, total: usize, label: &str, seeds: &BTreeSet<u32>) -> [usize; 3] {
    let mut out = [0usize; 3];
    for (i, policy) in [Policy::DirectOnly, Policy::TypedTable, Policy::AnyIndirect]
        .into_iter()
        .enumerate()
    {
        // The seeds are imports and have no bodies, so they are not themselves
        // instrumented; only defined functions count against the total.
        let n = p
            .suspend_closure(seeds, policy)
            .iter()
            .filter(|f| p.edges.contains_key(f))
            .count();
        out[i] = n;
        eprintln!(
            "  {label:22} {:<12} {n:6} / {total}  ({:5.1}%)",
            format!("{policy:?}"),
            100.0 * n as f64 / total as f64
        );
    }
    out
}

#[test]
fn the_suspend_set_over_a_real_module() {
    let Some(path) = std::env::var("ASH_LINK_TEST_MODULE")
        .ok()
        .map(PathBuf::from)
    else {
        eprintln!("set ASH_LINK_TEST_MODULE to a linked .wasm to measure a real suspend set");
        return;
    };
    let bytes = std::fs::read(&path).unwrap_or_else(|e| panic!("reading {}: {e}", path.display()));
    let p = program_from_module(&bytes).expect("reading the module");
    let total = p.edges.len();
    assert!(total > 0, "the module has no defined functions");

    let indirect_callers = p.edges.values().filter(|e| !e.indirect.is_empty()).count();
    let types_called: BTreeSet<u32> = p
        .edges
        .values()
        .flat_map(|e| e.indirect.iter().copied())
        .collect();
    eprintln!("{}", path.display());
    eprintln!(
        "  {total} defined functions, {} address-taken, {} calling through the table, \
         {} distinct types called indirectly",
        p.address_taken.len(),
        indirect_callers,
        types_called.len()
    );

    let yield_seeds = imports_named(&bytes, YIELD);
    assert!(
        !yield_seeds.is_empty(),
        "the module does not import ash_host_fiber_yield, so there is no suspend point to \
         close over -- point this at a module built with the fiber host imports"
    );
    let mut all = yield_seeds.clone();
    all.extend(imports_named(&bytes, BLOCKING));

    let a = report(&p, total, "yield only", &yield_seeds);
    let b = report(&p, total, "yield + blocking", &all);

    for (label, r) in [("yield only", a), ("yield + blocking", b)] {
        assert!(
            r[0] <= r[1] && r[1] <= r[2],
            "{label}: the policies are not ordered: direct {}, typed {}, any {}",
            r[0],
            r[1],
            r[2]
        );
    }
    // Not an assertion on the fraction: that is a property of the program, and
    // pinning it would turn every emitter change into a test failure. What is
    // asserted is that the measurement ran on something with indirect calls at
    // all, since a module without them would make the numbers meaningless.
    assert!(
        indirect_callers > 0,
        "no function calls through the table, so this module cannot answer the question"
    );
}
