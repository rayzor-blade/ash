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
//! the compiler driver, and for ELF targets an `objcopy` finishes it.
//!
//! On Mach-O the unwinding personality is the one internal the libraries share:
//! compact unwind has room for only a few personalities in an image, so each
//! object offers its copy as a weak private external and the link keeps one.

use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{Result, anyhow, bail};
use object::macho::{LC_SYMTAB, MH_MAGIC_64, N_EXT, N_PEXT, N_SECT, N_STAB, N_TYPE, N_WEAK_DEF};
use object::read::elf::{FileHeader, SectionHeader, Sym};
use object::{Endianness, SymbolIndex, elf};

use crate::archive::defined_in;

/// The unwinding personality a Rust library defines, by its Mach-O name.
const PERSONALITY: &str = "_rust_eh_personality";

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

/// The tool that rewrites an ELF object's symbol table: the one beside the
/// compiler driver, as in the NDK, else one on the search path.
fn objcopy_tool(tools: &Tools<'_>) -> Result<PathBuf> {
    let beside = Path::new(tools.program)
        .parent()
        .map(|bin| bin.join("llvm-objcopy"))
        .filter(|tool| tool.is_file());
    let on_path = || {
        let path = std::env::var_os("PATH")?;
        ["llvm-objcopy", "objcopy"].into_iter().find_map(|name| {
            std::env::split_paths(&path)
                .map(|dir| dir.join(name))
                .find(|tool| tool.is_file())
        })
    };
    beside.or_else(on_path).ok_or_else(|| {
        anyhow!(
            "no llvm-objcopy or objcopy beside {} or on the search path, which isolating a Rust library needs",
            tools.program
        )
    })
}

/// Whether the target's objects are Mach-O: an iOS app or a macOS program.
fn mach_o(tools: &Tools<'_>) -> bool {
    tools.os == "ios" || tools.os == "macos"
}

fn prefix(tools: &Tools<'_>) -> &'static str {
    if mach_o(tools) { "_" } else { "" }
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
    let shares_personality = mach_o(tools) && defined.iter().any(|d| d.name == PERSONALITY);
    if shares_personality {
        keep.push(PERSONALITY);
    }
    let list = out.with_extension("keep");
    std::fs::write(&list, keep.join("\n") + "\n")?;

    let mut merge = Command::new(tools.program);
    merge.args(tools.args).arg("-nostdlib");
    if mach_o(tools) {
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
    if !mach_o(tools) {
        let mut objcopy = Command::new(objcopy_tool(tools)?);
        objcopy
            .arg(format!("--keep-global-symbols={}", list.display()))
            .arg(out);
        run(&mut objcopy, "isolating a Rust library")?;
        rewrite(out, archive, keep_local_groups)?;
    }
    if shares_personality {
        rewrite(out, archive, share_personality)?;
    }
    Ok(Some(()))
}

/// Apply `patch` to the object merged from `archive`.
fn rewrite(out: &Path, archive: &Path, patch: fn(&mut [u8]) -> Result<()>) -> Result<()> {
    let mut object =
        std::fs::read(out).map_err(|e| anyhow!("could not read {}: {e}", out.display()))?;
    patch(&mut object).map_err(|e| anyhow!("isolating {}: {e}", archive.display()))?;
    std::fs::write(out, object).map_err(|e| anyhow!("could not write {}: {e}", out.display()))
}

/// Stop a linker treating the groups with a local signature as COMDAT. Such a
/// group is this library's own copy, such as the one that names its unwinding
/// personality. A linker that still matches it by name keeps one library's and
/// discards the others', which their unwind tables refer to. `llvm-objcopy`
/// clears the flag when it makes the signature local; GNU `objcopy` does not.
fn keep_local_groups(object: &mut [u8]) -> Result<()> {
    let flags = match object.get(4) {
        Some(&elf::ELFCLASS32) => local_group_flags::<elf::FileHeader32<Endianness>>(object)?,
        Some(&elf::ELFCLASS64) => local_group_flags::<elf::FileHeader64<Endianness>>(object)?,
        _ => bail!("the merged object is not an ELF object"),
    };
    for at in flags {
        // `GRP_COMDAT` is the one flag a group's first word carries.
        object
            .get_mut(at..at + 4)
            .ok_or_else(|| anyhow!("the merged object is truncated"))?
            .fill(0);
    }
    Ok(())
}

/// Where the flag word of each section group with a local signature starts.
fn local_group_flags<Elf: FileHeader<Endian = Endianness>>(object: &[u8]) -> Result<Vec<usize>> {
    let unreadable = |e: object::read::Error| anyhow!("reading the merged object: {e}");
    let header = Elf::parse(object).map_err(unreadable)?;
    let endian = header.endian().map_err(unreadable)?;
    let sections = header.sections(endian, object).map_err(unreadable)?;
    let symbols = sections
        .symbols(endian, object, elf::SHT_SYMTAB)
        .map_err(unreadable)?;
    let mut flags = Vec::new();
    for section in sections.iter() {
        if section.sh_type(endian) != elf::SHT_GROUP {
            continue;
        }
        let signature = SymbolIndex(section.sh_info(endian) as usize);
        if symbols.symbol(signature).map_err(unreadable)?.st_bind() == elf::STB_LOCAL {
            let at: u64 = section.sh_offset(endian).into();
            flags.push(usize::try_from(at)?);
        }
    }
    Ok(flags)
}

/// Mark the exported personality of a 64-bit Mach-O object as a weak private
/// external definition, which a link merges with the other copies and does not
/// export from the image.
fn share_personality(object: &mut [u8]) -> Result<()> {
    let truncated = || anyhow!("the merged object is truncated");
    let word = |object: &[u8], at: usize| -> Result<u32> {
        let bytes = object.get(at..at + 4).ok_or_else(truncated)?;
        Ok(u32::from_le_bytes(bytes.try_into()?))
    };
    if word(object, 0)? != MH_MAGIC_64 {
        bail!("the merged object is not a 64-bit Mach-O object");
    }
    // The load commands follow the 32-byte header, each led by its kind and size.
    let mut command = 32;
    let mut symbols = None;
    for _ in 0..word(object, 16)? {
        if word(object, command)? == LC_SYMTAB {
            symbols = Some([
                word(object, command + 8)? as usize,
                word(object, command + 12)? as usize,
                word(object, command + 16)? as usize,
            ]);
        }
        command += word(object, command + 4)? as usize;
    }
    let [table, count, strings] =
        symbols.ok_or_else(|| anyhow!("the merged object has no symbol table"))?;
    let mut marked = false;
    // Each entry: name offset (4), type (1), section (1), description (2), value (8).
    for entry in (0..count).map(|i| table + i * 16) {
        let name = strings + word(object, entry)? as usize;
        let kind = *object.get(entry + 4).ok_or_else(truncated)?;
        let exported = kind & N_STAB == 0 && kind & N_TYPE == N_SECT && kind & N_EXT != 0;
        let named = object
            .get(name..=name + PERSONALITY.len())
            .is_some_and(|n| n.strip_suffix(&[0]) == Some(PERSONALITY.as_bytes()));
        if exported && named {
            let desc = object.get_mut(entry + 6..entry + 8).ok_or_else(truncated)?;
            let weak = u16::from_le_bytes([desc[0], desc[1]]) | N_WEAK_DEF;
            desc.copy_from_slice(&weak.to_le_bytes());
            object[entry + 4] |= N_PEXT;
            marked = true;
        }
    }
    if !marked {
        bail!("the merged object does not export {PERSONALITY}");
    }
    Ok(())
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

    /// A 64-bit Mach-O object that is only a symbol table: each of `symbols`
    /// as a name and a type byte, defined in section 1.
    fn object(symbols: &[(&str, u8)]) -> Vec<u8> {
        let table = 32 + 24;
        let strings = table + symbols.len() * 16;
        let mut names = vec![0u8];
        let mut entries = Vec::new();
        for (name, kind) in symbols {
            entries.extend((names.len() as u32).to_le_bytes());
            entries.extend([*kind, 1, 0, 0]);
            entries.extend(0u64.to_le_bytes());
            names.extend(name.as_bytes());
            names.push(0);
        }
        let mut out = Vec::new();
        for word in [MH_MAGIC_64, 0, 0, 1, 1, 24, 0, 0] {
            out.extend(word.to_le_bytes());
        }
        for word in [
            LC_SYMTAB,
            24,
            table as u32,
            symbols.len() as u32,
            strings as u32,
            names.len() as u32,
        ] {
            out.extend(word.to_le_bytes());
        }
        out.extend(entries);
        out.extend(names);
        out
    }

    #[test]
    fn only_the_exported_personality_becomes_a_weak_private_external() {
        let exported = N_SECT | N_EXT;
        let before = object(&[
            ("_hlp_toy_add", exported),
            (PERSONALITY, N_SECT),
            (PERSONALITY, exported),
        ]);
        let mut after = before.clone();
        share_personality(&mut after).unwrap();
        let entry = |i: usize| 32 + 24 + i * 16;
        // The other symbol and the local copy are as they were.
        assert_eq!(after[entry(0)..entry(2)], before[entry(0)..entry(2)]);
        assert_eq!(after[entry(2) + 4], exported | N_PEXT);
        assert_eq!(after[entry(2) + 6..entry(2) + 8], N_WEAK_DEF.to_le_bytes());
    }

    /// A little-endian ELF64 object with two COMDAT groups, the first signed by
    /// a local symbol and the second by a global one. Returns it with where
    /// each group's flag word is.
    fn elf_with_two_groups() -> (Vec<u8>, [usize; 2]) {
        let (header, section) = (64usize, 64usize);
        // Two groups of two words, three symbols, and one empty string table.
        let groups = [header, header + 8];
        let symtab = header + 16;
        let strtab = symtab + 3 * 24;
        let table = strtab + 8;
        let mut out = vec![0u8; table + 5 * section];
        out[..7].copy_from_slice(&[0x7f, b'E', b'L', b'F', elf::ELFCLASS64, 1, 1]);
        out[16] = 1; // relocatable
        out[20] = 1; // version
        out[0x28..0x30].copy_from_slice(&(table as u64).to_le_bytes());
        for (at, value) in [(0x34, 64u16), (0x3a, 64), (0x3c, 5), (0x3e, 4)] {
            out[at..at + 2].copy_from_slice(&value.to_le_bytes());
        }
        for at in groups {
            out[at..at + 4].copy_from_slice(&elf::GRP_COMDAT.to_le_bytes());
        }
        // The second and third symbols: local, then global.
        out[symtab + 24 + 4] = elf::STB_LOCAL << 4;
        out[symtab + 48 + 4] = elf::STB_GLOBAL << 4;
        // Each section: type, offset, size, link, info.
        let sections = [
            (elf::SHT_GROUP, groups[0], 8, 3, 1),
            (elf::SHT_GROUP, groups[1], 8, 3, 2),
            (elf::SHT_SYMTAB, symtab, 72, 4, 2),
            (elf::SHT_STRTAB, strtab, 1, 0, 0),
        ];
        for (i, (kind, offset, size, link, info)) in sections.into_iter().enumerate() {
            let at = table + (i + 1) * section;
            out[at + 4..at + 8].copy_from_slice(&kind.to_le_bytes());
            out[at + 24..at + 32].copy_from_slice(&(offset as u64).to_le_bytes());
            out[at + 32..at + 40].copy_from_slice(&(size as u64).to_le_bytes());
            out[at + 40..at + 44].copy_from_slice(&(link as u32).to_le_bytes());
            out[at + 44..at + 48].copy_from_slice(&(info as u32).to_le_bytes());
            if kind == elf::SHT_SYMTAB {
                out[at + 56..at + 64].copy_from_slice(&24u64.to_le_bytes());
            }
        }
        (out, groups)
    }

    #[test]
    fn only_a_group_with_a_local_signature_stops_being_comdat() {
        let (before, [local, global]) = elf_with_two_groups();
        let mut after = before.clone();
        keep_local_groups(&mut after).unwrap();
        assert_eq!(after[local..local + 4], 0u32.to_le_bytes());
        assert_eq!(after[global..global + 4], elf::GRP_COMDAT.to_le_bytes());
        // Nothing but that word changed.
        assert_eq!(after[..local], before[..local]);
        assert_eq!(after[local + 4..], before[local + 4..]);
    }

    #[test]
    fn an_object_without_the_personality_is_refused() {
        let mut other = object(&[("_hlp_toy_add", N_SECT | N_EXT)]);
        let why = share_personality(&mut other).unwrap_err().to_string();
        assert!(why.contains("does not export"), "{why}");
        let why = share_personality(&mut [0u8; 40]).unwrap_err().to_string();
        assert!(why.contains("not a 64-bit Mach-O"), "{why}");
    }

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
