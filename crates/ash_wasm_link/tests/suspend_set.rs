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

/// Function names from the name section, for the indices that have one.
///
/// Only used to write the set out for comparison against another tool's, so a
/// module without a name section simply yields nothing.
fn names(bytes: &[u8]) -> std::collections::BTreeMap<u32, String> {
    let mut out = std::collections::BTreeMap::new();
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        let wasmparser::Payload::CustomSection(c) = payload.expect("parsing") else {
            continue;
        };
        if c.name() != "name" {
            continue;
        }
        let reader = wasmparser::NameSectionReader::new(wasmparser::BinaryReader::new_features(
            c.data(),
            c.data_offset(),
            wasmparser::WasmFeatures::all(),
        ));
        for sub in reader {
            if let Ok(wasmparser::Name::Function(map)) = sub {
                for entry in map {
                    let entry = entry.expect("a name entry");
                    out.insert(entry.index, entry.name.to_string());
                }
            }
        }
    }
    out
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

    let yield_seeds = ash_wasm_link::fiber::imports_named(&bytes, YIELD).expect("reading imports");
    assert!(
        !yield_seeds.is_empty(),
        "the module does not import ash_host_fiber_yield, so there is no suspend point to \
         close over -- point this at a module built with the fiber host imports"
    );
    let mut all = yield_seeds.clone();
    all.extend(ash_wasm_link::fiber::imports_named(&bytes, BLOCKING).expect("reading imports"));

    // Written out so the set can be diffed against another instrumenter's.
    // `docs/wasm-fibers.md` §6 makes ours being a subset of Binaryen's the
    // exit condition for this step, and a count alone cannot show that.
    if let Ok(path) = std::env::var("ASH_LINK_TEST_NAMES") {
        let names = names(&bytes);
        let mut lines: Vec<String> = p
            .suspend_closure(&yield_seeds, Policy::TypedTable)
            .iter()
            .filter(|f| p.edges.contains_key(f))
            .map(|f| {
                names
                    .get(f)
                    .cloned()
                    .unwrap_or_else(|| format!("func[{f}]"))
            })
            .collect();
        lines.sort();
        std::fs::write(&path, lines.join("\n") + "\n").expect("writing the name list");
        eprintln!("  wrote {} names to {path}", lines.len());
    }

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
