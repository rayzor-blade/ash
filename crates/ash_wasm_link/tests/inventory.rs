//! What the two objects actually require, printed rather than assumed.
use std::path::PathBuf;

fn object(name: &str) -> Option<Vec<u8>> {
    let p = PathBuf::from(std::env::var("ASH_LINK_TEST_OBJECTS").ok()?).join(name);
    std::fs::read(p).ok()
}

#[test]
fn inventory() {
    use ash_wasm_link::object::ImportKind;
    for name in ["program.o", "runtime.o"] {
        let Some(bytes) = object(name) else { continue };
        let obj = ash_wasm_link::read(name, &bytes).expect("read");
        eprintln!("\n== {name}");
        eprintln!("  defined globals: {}", obj.globals.len());
        eprintln!(
            "  defined tables: {}  memories: {}  tags: {}",
            obj.tables.len(),
            obj.memories.len(),
            obj.tags.len()
        );
        eprintln!("  exports in section: {}", obj.exports.len());
        let mut by_kind = std::collections::BTreeMap::<&str, Vec<String>>::new();
        for imp in &obj.imports {
            let k = match imp.kind {
                ImportKind::Function { .. } => "func",
                ImportKind::Table(_) => "table",
                ImportKind::Memory(_) => "memory",
                ImportKind::Global { .. } => "global",
                ImportKind::Tag { .. } => "tag",
            };
            by_kind
                .entry(k)
                .or_default()
                .push(format!("{}.{}", imp.module, imp.name));
        }
        for (k, v) in &by_kind {
            if *k == "func" {
                eprintln!(
                    "  import {k}: {} (first 6: {:?})",
                    v.len(),
                    &v[..v.len().min(6)]
                );
            } else {
                eprintln!("  import {k}: {v:?}");
            }
        }
        let exported: Vec<&str> = obj
            .symbols
            .iter()
            .filter(|s| s.is_exported() && !s.is_undefined())
            .map(|s| s.name.as_str())
            .collect();
        eprintln!(
            "  symbols flagged exported: {} (first 6: {:?})",
            exported.len(),
            &exported[..exported.len().min(6)]
        );
        eprintln!("  segment_info entries: {}", obj.segment_info.len());
        if let Some(first) = obj.segment_info.first() {
            eprintln!("  first segment: {first:?}");
        }
        eprintln!(
            "  init funcs: {:?}",
            &obj.init_funcs[..obj.init_funcs.len().min(5)]
        );
    }
}
