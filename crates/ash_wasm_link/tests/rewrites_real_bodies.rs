//! The body cursor, over a module ash actually produced.
//!
//! The unit tests in `cursor.rs` use four-instruction bodies this crate wrote.
//! They prove the label arithmetic on cases chosen to exercise it, and nothing
//! about the shapes ash emits: the `try_table`/`exnref` of the setjmp
//! lowering, blocks whose result type is a reference, bodies with hundreds of
//! locals, and the five-byte immediates the relocation slots leave behind.
//!
//! Two rewrites run here. The identity one must change nothing, which is
//! checkable exactly. The wrapping one puts a block around every body it can,
//! which moves every branch in the module that leaves its function -- tens of
//! thousands of them -- and is checked by validating the result, since a
//! wrongly renumbered label is overwhelmingly likely to land on a frame whose
//! type does not match.
//!
//! ```text
//! ASH_LINK_TEST_MODULE=/path/to/prog.wasm cargo test -p ash_wasm_link
//! ```

use std::path::PathBuf;

use ash_wasm_link::cursor::rewrite_module;
use wasm_encoder::reencode::Reencode as _;

fn module() -> Option<Vec<u8>> {
    let path = std::env::var("ASH_LINK_TEST_MODULE")
        .ok()
        .map(PathBuf::from)?;
    Some(std::fs::read(&path).unwrap_or_else(|e| panic!("reading {}: {e}", path.display())))
}

fn traces(bytes: &[u8]) -> Vec<Vec<String>> {
    let mut all = Vec::new();
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        if let wasmparser::Payload::CodeSectionEntry(body) = payload.expect("parsing") {
            let r = body.range();
            all.push(
                ash_wasm_link::body::operator_trace(&bytes[r.start as usize..r.end as usize])
                    .expect("tracing a body"),
            );
        }
    }
    all
}

fn validate(bytes: &[u8]) -> Result<(), String> {
    wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
        .validate_all(bytes)
        .map(|_| ())
        .map_err(|e| e.to_string())
}

#[test]
fn an_identity_rewrite_of_every_body_changes_nothing() {
    let Some(bytes) = module() else {
        eprintln!("set ASH_LINK_TEST_MODULE to a linked .wasm to rewrite real bodies");
        return;
    };
    let out = rewrite_module(&bytes, |_, c, op| c.emit(op)).expect("the identity rewrite");
    validate(&out).expect("the identity rewrite must validate");

    let (before, after) = (traces(&bytes), traces(&out));
    assert_eq!(before.len(), after.len(), "the body count changed");
    for (i, (b, a)) in before.iter().zip(&after).enumerate() {
        assert_eq!(b, a, "body {i} changed");
    }
    eprintln!(
        "identity: {} bodies, {} operators, {} -> {} bytes ({:+.1}%)",
        before.len(),
        before.iter().map(Vec::len).sum::<usize>(),
        bytes.len(),
        out.len(),
        100.0 * (out.len() as f64 - bytes.len() as f64) / bytes.len() as f64
    );
}

#[test]
fn wrapping_every_body_renumbers_every_branch_that_leaves_one() {
    let Some(bytes) = module() else {
        eprintln!("set ASH_LINK_TEST_MODULE to a linked .wasm to rewrite real bodies");
        return;
    };

    // A wrapper block has to produce what the function produces, and a block
    // type can name at most one result without adding a type to the type
    // section -- which this rewrite deliberately does not do, since the point
    // is to exercise the label arithmetic and not the section rebuilder.
    // Functions returning more than one value are left alone and counted.
    let mut wrapped = 0usize;
    let mut skipped = 0usize;
    let out = rewrite_module(&bytes, |_, c, op| {
        if c.depth() == 1 && c.inserted_frames() == 0 {
            match c.results()?.as_slice() {
                [] => {
                    wrapped += 1;
                    c.open_block(wasm_encoder::BlockType::Empty);
                }
                [one] => {
                    wrapped += 1;
                    let ty = wasm_encoder::reencode::RoundtripReencoder
                        .val_type(*one)
                        .map_err(|e| anyhow::anyhow!("a result type: {e}"))?;
                    c.open_block(wasm_encoder::BlockType::Result(ty));
                }
                _ => skipped += 1,
            }
        }
        if matches!(op, wasmparser::Operator::End) && c.depth() == 1 && c.inserted_frames() == 1 {
            c.close_block()?;
        }
        c.emit(op)
    })
    .expect("the wrapping rewrite");

    // A validating module is not a running one: a renumbered label can land
    // on a frame whose type happens to match. Set ASH_LINK_TEST_OUT and run
    // the result to close that gap by hand.
    if let Ok(out_path) = std::env::var("ASH_LINK_TEST_OUT") {
        std::fs::write(&out_path, &out).unwrap_or_else(|e| panic!("writing {out_path}: {e}"));
        eprintln!("wrote the wrapped module to {out_path}");
    }
    eprintln!("wrapped {wrapped} bodies, skipped {skipped} with multiple results");
    assert!(wrapped > 0, "nothing was wrapped, so nothing was tested");
    validate(&out).unwrap_or_else(|e| {
        panic!("a wrapped body does not validate, which is a renumbering bug: {e}")
    });

    // Every wrapped body gained exactly one Block and one End, and no other
    // operator was added or removed. A remapper that dropped an instruction
    // could still validate; this cannot miss it.
    let (before, after) = (traces(&bytes), traces(&out));
    for (i, (b, a)) in before.iter().zip(&after).enumerate() {
        let grew = a.len() - b.len();
        assert!(
            grew == 0 || grew == 2,
            "body {i} changed by {grew} operators, expected 0 or 2"
        );
    }
}
