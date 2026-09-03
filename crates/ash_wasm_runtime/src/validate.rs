//! Read a wasm module and say whether it is one, and what it still needs.
//!
//! Two questions, asked at different times. During the port the useful one is
//! "what does this module import that nothing can supply", because that list
//! is the remaining work. Once the list is empty the useful one is "is this
//! still a valid module that needs only WASI and the fiber import", which is
//! a gate a CI job can fail on.
//!
//! Both are answered here rather than by an external tool, so the answer
//! travels with the compiler and a build machine needs nothing installed.

use wasmparser::{Parser, Payload, TableType, Validator};

/// What a host can be expected to provide.
///
/// Everything else in an import list is something the runtime has not
/// supplied yet, or something a sandbox cannot do at all.
fn is_host_supplied(module: &str, name: &str) -> bool {
    module.starts_with("wasi_")
        // Everything ash asks a host for beyond WASI shares this prefix, and
        // each one is optional: a host that does not implement it says so
        // through the call's own failure, not by refusing to instantiate.
        || (module == "env" && name.starts_with("ash_host_"))
        // The linker's own placeholders, present in a relocatable object.
        || module == "GOT.mem"
        || module == "GOT.func"
        || (module == "env" && (name == "__linear_memory" || name == "__indirect_function_table"))
}

/// One import, as reported.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Import {
    pub module: String,
    pub name: String,
    /// Whether a host could satisfy it.
    pub host_supplied: bool,
}

/// What the module is and what it asks for.
#[derive(Debug, Default)]
pub struct Report {
    /// `None` if the module validates; the validator's complaint otherwise.
    pub invalid: Option<String>,
    /// True for a relocatable object rather than a linked module.
    pub relocatable: bool,
    pub imports: Vec<Import>,
    pub exports: Vec<String>,
    pub functions: usize,
    pub tables: Vec<TableSummary>,
    pub call_indirect_sites: usize,
    pub data_segments: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TableSummary {
    pub element_type: String,
    pub initial: u64,
    pub maximum: Option<u64>,
}

impl From<TableType> for TableSummary {
    fn from(t: TableType) -> Self {
        Self {
            element_type: format!("{:?}", t.element_type),
            initial: t.initial,
            maximum: t.maximum,
        }
    }
}

impl Report {
    /// The imports nothing can satisfy. Empty is the goal.
    pub fn unsatisfied(&self) -> Vec<&Import> {
        self.imports.iter().filter(|i| !i.host_supplied).collect()
    }

    /// Whether this module could be run by a host today.
    pub fn runnable(&self) -> bool {
        self.invalid.is_none() && !self.relocatable && self.unsatisfied().is_empty()
    }
}

/// Inspect `bytes`.
///
/// Parsing continues even when validation fails, because a module that does
/// not validate still has an import list, and that list is usually what
/// explains why.
pub fn inspect(bytes: &[u8]) -> Report {
    let mut report = Report {
        invalid: Validator::new()
            .validate_all(bytes)
            .err()
            .map(|e| e.to_string()),
        ..Default::default()
    };

    for payload in Parser::new(0).parse_all(bytes) {
        let Ok(payload) = payload else { break };
        match payload {
            Payload::ImportSection(reader) => {
                // `into_imports` flattens the compact encodings, where one
                // record carries a module name and many items.
                for import in reader.into_imports().flatten() {
                    report.imports.push(Import {
                        module: import.module.to_string(),
                        name: import.name.to_string(),
                        host_supplied: is_host_supplied(import.module, import.name),
                    });
                }
            }
            Payload::ExportSection(reader) => {
                for export in reader.into_iter().flatten() {
                    report.exports.push(export.name.to_string());
                }
            }
            Payload::FunctionSection(reader) => report.functions += reader.count() as usize,
            Payload::TableSection(reader) => {
                for table in reader.into_iter().flatten() {
                    report.tables.push(table.ty.into());
                }
            }
            Payload::DataSection(reader) => report.data_segments += reader.count() as usize,
            Payload::CodeSectionEntry(body) => {
                if let Ok(reader) = body.get_operators_reader() {
                    for op in reader.into_iter().flatten() {
                        if matches!(op, wasmparser::Operator::CallIndirect { .. }) {
                            report.call_indirect_sites += 1;
                        }
                    }
                }
            }
            // A `linking` section is what makes an object relocatable: it is
            // input to a linker, not something an engine can instantiate.
            Payload::CustomSection(section) if section.name() == "linking" => {
                report.relocatable = true;
            }
            _ => {}
        }
    }
    report
}
