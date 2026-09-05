//! Every body of a real module, decoded and re-encoded, must still say the
//! same thing.
//!
//! The unit tests in `body.rs` use a body this crate wrote, which proves the
//! two halves agree with each other and nothing about what ash emits. This
//! runs the round-trip over a module ash actually produced, which is where the
//! shapes live that a decoder gets wrong: the `try_table` and `exnref` of the
//! setjmp lowering, multi-value blocks, saturating conversions, and every
//! immediate ash pads to five bytes so a relocation can be written into it.
//!
//! Point it at a linked module:
//!
//! ```text
//! ASH_LINK_TEST_MODULE=/path/to/prog.wasm cargo test -p ash_wasm_link
//! ```
//!
//! Without the variable it reports what it would have needed and passes,
//! following the convention of the other tests here: a machine with no
//! artefacts should not fail the suite.

use std::path::PathBuf;

fn module() -> Option<Vec<u8>> {
    let path = std::env::var("ASH_LINK_TEST_MODULE")
        .ok()
        .map(PathBuf::from)?;
    Some(std::fs::read(&path).unwrap_or_else(|e| panic!("reading {}: {e}", path.display())))
}

#[test]
fn every_body_survives_a_round_trip() {
    let Some(bytes) = module() else {
        eprintln!("set ASH_LINK_TEST_MODULE to a linked .wasm to exercise real bodies");
        return;
    };

    let mut bodies = 0usize;
    let mut with_eh = 0usize;
    let mut operators = 0usize;
    let mut failures: Vec<String> = Vec::new();
    // ash pads every relocated immediate to five bytes so a patch can be
    // written without moving anything. Re-encoding writes each at its natural
    // width, so the round-trip is also a measurement of what that padding
    // costs -- reported rather than asserted, since it is a property of the
    // input module and not of this code.
    let (mut before_bytes, mut after_bytes) = (0usize, 0usize);

    for payload in wasmparser::Parser::new(0).parse_all(&bytes) {
        let payload = payload.expect("parsing the module");
        let wasmparser::Payload::CodeSectionEntry(body) = payload else {
            continue;
        };
        let range = body.range();
        let contents = &bytes[range.start as usize..range.end as usize];
        bodies += 1;

        let before = match ash_wasm_link::body::operator_trace(contents) {
            Ok(ops) => ops,
            Err(e) => {
                failures.push(format!("body {bodies}: decoding the original failed: {e}"));
                continue;
            }
        };
        operators += before.len();
        // The shapes Binaryen's Flatten aborts on. Counted rather than
        // assumed: if a change to the emitter ever stops producing them, this
        // test quietly stops covering the case it exists for.
        if before
            .iter()
            .any(|op| op.starts_with("TryTable") || op.contains("ThrowRef"))
        {
            with_eh += 1;
        }

        match ash_wasm_link::body::reencode(contents) {
            Ok(again) => {
                before_bytes += contents.len();
                after_bytes += again.len();
                match ash_wasm_link::body::operator_trace(&again) {
                    Ok(after) if after == before => {}
                    Ok(after) => {
                        let at = before
                            .iter()
                            .zip(&after)
                            .position(|(a, b)| a != b)
                            .unwrap_or(before.len().min(after.len()));
                        failures.push(format!(
                        "body {bodies}: operator {at} became {:?}, was {:?} ({} operators before, {} after)",
                        after.get(at),
                        before.get(at),
                        before.len(),
                        after.len()
                    ));
                    }
                    Err(e) => failures.push(format!(
                        "body {bodies}: re-encoded body will not decode: {e}"
                    )),
                }
            }
            Err(e) => failures.push(format!("body {bodies}: re-encoding failed: {e}")),
        }
    }

    assert!(bodies > 0, "the module has no code section");
    eprintln!(
        "round-tripped {bodies} bodies, {operators} operators, {with_eh} carrying try_table/exnref"
    );
    eprintln!(
        "body bytes {before_bytes} -> {after_bytes} ({:+.1}%, the relocation padding recovered)",
        100.0 * (after_bytes as f64 - before_bytes as f64) / before_bytes as f64
    );
    assert!(
        with_eh > 0,
        "no body carried try_table or exnref, so this run did not cover the shapes \
         Binaryen cannot handle -- point the test at a module built from a program \
         that uses exceptions"
    );
    assert!(
        failures.is_empty(),
        "{} of {bodies} bodies did not survive:\n  {}",
        failures.len(),
        failures
            .iter()
            .take(10)
            .cloned()
            .collect::<Vec<_>>()
            .join("\n  ")
    );
}
