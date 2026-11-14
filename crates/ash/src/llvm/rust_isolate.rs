//! Rust static libraries in one phone program.
//!
//! A static library built by rustc contains its own copy of the standard
//! library. Two of them in one link define the same unmangled symbols
//! (`rust_eh_personality`) and, built by the same compiler, the same mangled
//! ones too. Apple's linker has no option to accept a duplicate definition, so
//! each such library is merged into one object in which every symbol outside
//! its interface is local, and that object joins the link in its place.
//!
//! The symbols are read from the archive directly, because Apple's `nm`
//! cannot read the bitcode rustc leaves in a library's members. The merge uses
//! the compiler driver, and on Android the NDK's `llvm-objcopy` finishes it.

use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{Result, anyhow, bail};

use crate::archive::defined_in;

/// How to run the platform's tools.
pub(crate) struct Tools<'a> {
    /// The compiler driver and the arguments that select the target.
    pub program: &'a str,
    pub args: &'a [String],
    pub os: &'a str,
}

/// The link inputs once Rust libraries are isolated.
pub(crate) struct Prepared {
    pub objects: Vec<PathBuf>,
    pub runtime: PathBuf,
    _scratch: Option<tempfile::TempDir>,
}

/// Whether `symbol` belongs to a Rust library's internals: mangled names,
/// the unwinding personality and the allocator shims. `prefix` is the
/// platform's extra leading underscore (`_` on Mach-O, none on ELF).
pub(crate) fn is_rust_internal(symbol: &str, prefix: &str) -> bool {
    let Some(name) = symbol.strip_prefix(prefix) else {
        return false;
    };
    name == "rust_eh_personality"
        || name.starts_with("__rust")
        || name.starts_with("__rdl")
        || name.starts_with("__rg")
        // v0 mangling: `_R` and an uppercase letter.
        || name
            .strip_prefix("_R")
            .is_some_and(|rest| rest.starts_with(|c: char| c.is_ascii_uppercase()))
        // Legacy mangling ends in a 16-digit hash: `_ZN...17h<hash>E`.
        || (name.starts_with("_ZN") && has_legacy_hash(name))
}

fn has_legacy_hash(name: &str) -> bool {
    let Some(body) = name.strip_suffix('E') else {
        return false;
    };
    let Some(hash) = body.len().checked_sub(16).map(|at| &body[at..]) else {
        return false;
    };
    body[..body.len() - 16].ends_with("17h") && hash.bytes().all(|b| b.is_ascii_hexdigit())
}

/// An NDK tool, found beside the compiler driver.
fn ndk_tool(tools: &Tools<'_>, name: &str) -> Result<PathBuf> {
    let bin = Path::new(tools.program)
        .parent()
        .ok_or_else(|| anyhow!("no directory beside {}", tools.program))?;
    let tool = bin.join(name);
    if tool.is_file() {
        Ok(tool)
    } else {
        bail!(
            "{name} is not beside {}, which isolating a Rust library needs",
            tools.program
        )
    }
}

fn prefix(tools: &Tools<'_>) -> &'static str {
    if tools.os == "ios" { "_" } else { "" }
}

/// Merge `archive` into the object `out`, keeping only its interface global.
/// `None` when the archive is not a Rust library, which needs no isolation.
fn isolate(archive: &Path, out: &Path, tools: &Tools<'_>) -> Result<Option<()>> {
    let defined = defined_in(archive)?;
    let prefix = prefix(tools);
    if !defined.iter().any(|d| is_rust_internal(&d.name, prefix)) {
        return Ok(None);
    }
    let mut keep: Vec<&str> = defined
        .iter()
        .filter(|d| !d.hidden && !is_rust_internal(&d.name, prefix))
        .map(|d| d.name.as_str())
        .collect();
    keep.sort_unstable();
    keep.dedup();
    if keep.is_empty() {
        bail!("{} exports nothing but Rust internals", archive.display());
    }
    let list = out.with_extension("keep");
    std::fs::write(&list, keep.join("\n") + "\n")?;

    let mut merge = Command::new(tools.program);
    merge.args(tools.args).arg("-nostdlib");
    if tools.os == "ios" {
        // Mach-O's linker makes every global outside the list local in a
        // relocatable output.
        merge
            .arg("-Wl,-r")
            .arg("-Wl,-all_load")
            .arg(format!("-Wl,-exported_symbols_list,{}", list.display()))
            .arg(archive);
    } else {
        // The driver's own `-r`: the NDK driver otherwise adds `-pie`, which
        // lld refuses with a relocatable output.
        merge
            .arg("-r")
            .arg("-Wl,--whole-archive")
            .arg(archive)
            .arg("-Wl,--no-whole-archive");
    }
    merge.arg("-o").arg(out);
    run(&mut merge, "merging a Rust library")?;
    if tools.os != "ios" {
        let mut objcopy = Command::new(ndk_tool(tools, "llvm-objcopy")?);
        objcopy
            .arg(format!("--keep-global-symbols={}", list.display()))
            .arg(out);
        run(&mut objcopy, "isolating a Rust library")?;
    }
    Ok(Some(()))
}

fn run(command: &mut Command, what: &str) -> Result<()> {
    let output = command
        .output()
        .map_err(|e| anyhow!("could not run {what}: {e}"))?;
    if output.status.success() {
        return Ok(());
    }
    bail!(
        "{what} failed:\n{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

/// Isolate each Rust static library in `objects` and, when one was, the
/// runtime too. Nothing changes for a program with no such library.
pub(crate) fn prepare(
    objects: &[PathBuf],
    runtime: &Path,
    tools: &Tools<'_>,
) -> Result<Prepared> {
    let is_archive = |p: &Path| p.extension().is_some_and(|e| e == "a");
    if !objects.iter().any(|o| is_archive(o)) {
        return Ok(Prepared {
            objects: objects.to_vec(),
            runtime: runtime.to_path_buf(),
            _scratch: None,
        });
    }
    let scratch = tempfile::tempdir()?;
    let mut prepared = Vec::with_capacity(objects.len());
    let mut any = false;
    for (index, object) in objects.iter().enumerate() {
        if is_archive(object) {
            let out = scratch.path().join(format!("isolated{index}.o"));
            if isolate(object, &out, tools)?.is_some() {
                prepared.push(out);
                any = true;
                continue;
            }
        }
        prepared.push(object.clone());
    }
    let mut runtime_out = runtime.to_path_buf();
    if any && is_archive(runtime) {
        let out = scratch.path().join("isolated_runtime.o");
        if isolate(runtime, &out, tools)?.is_some() {
            runtime_out = out;
        }
    }
    Ok(Prepared {
        objects: prepared,
        runtime: runtime_out,
        _scratch: Some(scratch),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rust_internals_are_named_by_mangling_and_shim_prefixes() {
        assert!(is_rust_internal("_rust_eh_personality", "_"));
        assert!(is_rust_internal("rust_eh_personality", ""));
        assert!(is_rust_internal("__RNvNtCs4O6wlheEp7z_3std9panicking11EMPTY_PANIC", "_"));
        assert!(is_rust_internal("___rust_alloc", "_"));
        assert!(is_rust_internal(
            "_ZN3std9panicking11EMPTY_PANIC17h0123456789abcdefE",
            ""
        ));
        // An interface symbol and a C++ name.
        assert!(!is_rust_internal("_hlp_toy_add", "_"));
        assert!(!is_rust_internal("_ZN4blinc5layoutEv", ""));
        assert!(!is_rust_internal("xwindow_main", ""));
    }
}
