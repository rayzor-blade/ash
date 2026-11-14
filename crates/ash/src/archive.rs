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
