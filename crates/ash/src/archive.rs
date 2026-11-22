//! The symbols a static library defines, read from its archive directly.
//!
//! Apple's `nm` cannot read the bitcode rustc leaves in a library's members,
//! so the members' object files are parsed here instead.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use anyhow::{Result, anyhow, bail};

/// A symbol a library defines: its name, and whether it is hidden.
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Defined {
    pub(crate) name: String,
    pub(crate) hidden: bool,
}

/// The global symbols the archive's objects define. A member that is not an
/// object file, such as raw bitcode, has none to give; an archive with no
/// readable member is an error.
pub(crate) fn defined_in(archive: &Path) -> Result<Vec<Defined>> {
    use object::{Object, ObjectSymbol, SymbolScope};

    let data =
        std::fs::read(archive).map_err(|e| anyhow!("could not read {}: {e}", archive.display()))?;
    let members = object::read::archive::ArchiveFile::parse(&*data)
        .map_err(|e| anyhow!("{} is not an archive: {e}", archive.display()))?;
    let mut defined = Vec::new();
    let mut readable = 0;
    for member in members.members() {
        let Ok(member) = member else { continue };
        let Ok(bytes) = member.data(&*data) else {
            continue;
        };
        let Ok(file) = object::File::parse(bytes) else {
            continue;
        };
        readable += 1;
        for symbol in file.symbols() {
            if symbol.is_undefined() || !symbol.is_global() {
                continue;
            }
            let Ok(name) = symbol.name() else { continue };
            defined.push(Defined {
                name: name.to_string(),
                hidden: symbol.scope() == SymbolScope::Linkage,
            });
        }
    }
    if readable == 0 {
        bail!(
            "{} has no object file whose symbols can be read",
            archive.display()
        );
    }
    Ok(defined)
}

/// The names the `archives` define, without a platform's leading underscore.
/// A file that is not a readable archive defines none.
pub fn defined_symbols(archives: &[PathBuf]) -> HashSet<String> {
    archives
        .iter()
        .filter_map(|archive| defined_in(archive).ok())
        .flatten()
        .map(|symbol| {
            symbol
                .name
                .strip_prefix('_')
                .unwrap_or(&symbol.name)
                .to_string()
        })
        .collect()
}

/// The function names the wasm objects and archives define, for a primitive the program can
/// call by its symbol. A file that is not readable defines none; the link says so when it comes
/// to it.
#[cfg(feature = "llvm")]
pub fn defined_wasm_functions(paths: &[PathBuf]) -> HashSet<String> {
    let mut names = HashSet::new();
    for path in paths {
        let Ok(bytes) = std::fs::read(path) else {
            continue;
        };
        let name = path
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_default();
        if let Ok(defined) = ash_wasm_link::defined_functions(&name, &bytes) {
            names.extend(defined);
        }
    }
    names
}

/// Without the `llvm` feature there is no wasm build, so no library defines anything for one.
#[cfg(not(feature = "llvm"))]
pub fn defined_wasm_functions(_paths: &[PathBuf]) -> HashSet<String> {
    HashSet::new()
}

/// Whether a program's native is one the linked libraries define as `hlp_<name>`, so the program
/// calls it by that symbol instead of asking a library the host opens.
pub fn defines_native(defined: Option<&HashSet<String>>, name: &str) -> bool {
    defined.is_some_and(|d| d.contains(&format!("hlp_{}", name.trim_start_matches("hlp_"))))
}

/// Which of a program's HDLL libraries the linked archives stand in for.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct LibraryCoverage {
    /// Libraries with at least one primitive an archive defines.
    pub linked: Vec<String>,
    /// Libraries no archive touches, which still have to be loaded.
    pub loaded: Vec<String>,
}

impl LibraryCoverage {
    /// Whether the program can take every library from the archives. A
    /// primitive its own archive lacks stays unresolved and raises when it is
    /// reached, as it does on a phone.
    pub fn is_complete(&self) -> bool {
        !self.linked.is_empty() && self.loaded.is_empty()
    }
}

/// Classify the `(library, primitive)` pairs of a program's natives, leaving
/// out the runtime's own libraries.
pub fn library_coverage<'a>(
    natives: impl IntoIterator<Item = (&'a str, &'a str)>,
    defined: &HashSet<String>,
) -> LibraryCoverage {
    let mut libraries: Vec<(&str, bool)> = Vec::new();
    for (lib, name) in natives {
        if crate::native_lib::is_runtime_lib(lib) {
            continue;
        }
        let linked = defines_native(Some(defined), name);
        match libraries.iter_mut().find(|(known, _)| *known == lib) {
            Some(entry) => entry.1 |= linked,
            None => libraries.push((lib, linked)),
        }
    }
    let mut coverage = LibraryCoverage::default();
    for (lib, linked) in libraries {
        let list = if linked {
            &mut coverage.linked
        } else {
            &mut coverage.loaded
        };
        list.push(lib.trim_start_matches('?').to_string());
    }
    coverage
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_library_with_one_primitive_in_an_archive_is_linked() {
        let defined: HashSet<String> = ["hlp_toy_add".to_string()].into();
        let natives = [("toy", "toy_add"), ("toy", "toy_absent"), ("std", "hlp_x")];
        let coverage = library_coverage(natives, &defined);
        assert_eq!(coverage.linked, ["toy"]);
        assert!(coverage.is_complete());
    }

    #[test]
    fn a_library_no_archive_touches_still_loads() {
        let defined: HashSet<String> = ["hlp_toy_add".to_string()].into();
        let natives = [("toy", "toy_add"), ("fmt", "png_decode")];
        let coverage = library_coverage(natives, &defined);
        assert_eq!(coverage.linked, ["toy"]);
        assert_eq!(coverage.loaded, ["fmt"]);
        assert!(!coverage.is_complete());
    }

    #[test]
    fn no_archives_and_no_libraries_are_not_a_static_program() {
        let none = HashSet::new();
        assert!(!library_coverage([("toy", "toy_add")], &none).is_complete());
        assert!(!library_coverage([("std", "hlp_x")], &none).is_complete());
    }

    #[test]
    fn a_native_is_defined_by_the_hlp_symbol_of_its_name() {
        let defined: HashSet<String> = ["hlp_greeter_new".to_string()].into();
        assert!(defines_native(Some(&defined), "greeter_new"));
        // A name written with its own prefix names the same symbol.
        assert!(defines_native(Some(&defined), "hlp_greeter_new"));
        assert!(!defines_native(Some(&defined), "greeter_greet"));
        assert!(!defines_native(None, "greeter_new"));
    }
}
