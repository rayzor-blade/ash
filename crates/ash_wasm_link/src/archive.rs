//! Static libraries: an `ar` archive of relocatable wasm objects, linked the way a native
//! linker links one.
//!
//! A member joins the link only when it defines a symbol something already in the link names and
//! nothing defines, and the members it brings are searched in turn for what they name. A library
//! therefore costs only the members the program reaches.
//!
//! A pulled member can also define a symbol the link already has: a Rust library carries its own
//! `std`, and so defines `rust_eh_personality` and the allocator entry points the runtime
//! defines. The earlier definition wins. The member's copy is turned into a reference, so the
//! linker's by-name resolution binds everything that named it, the member's own code included, to
//! the winner. Two plain objects defining one name remain an error; only a library's definitions
//! give way. Each one that did is reported, because binding code to a definition it was not
//! built with is a decision someone should be able to see.

use std::collections::{HashMap, HashSet, VecDeque};

use anyhow::{Context, Result, bail};
use object::read::archive::ArchiveFile;
use wasmparser::{RelocationType, SymbolFlags};

use crate::link::{Kind, host_can_supply, kind_of};
use crate::object::{ImportKind, Object, SymbolTarget, read};

/// Whether `bytes` begin like an `ar` archive.
pub fn is_archive(bytes: &[u8]) -> bool {
    bytes.starts_with(b"!<arch>\n") || bytes.starts_with(b"!<thin>\n")
}

/// The wasm objects of one archive, each handed out at most once.
#[derive(Clone)]
pub struct Archive {
    pub name: String,
    members: Vec<Option<Object>>,
}

impl std::fmt::Debug for Archive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Archive({}, {} members)", self.name, self.members.len())
    }
}

impl Archive {
    /// An archive of objects already read, for a caller that has them in hand.
    pub fn from_objects(name: &str, objects: Vec<Object>) -> Archive {
        Archive {
            name: name.to_string(),
            members: objects.into_iter().map(Some).collect(),
        }
    }

    /// How many members the archive holds.
    pub fn len(&self) -> usize {
        self.members.len()
    }

    pub fn is_empty(&self) -> bool {
        self.members.is_empty()
    }

    /// The members not yet pulled, by the name each is linked under.
    pub fn member_names(&self) -> Vec<String> {
        self.members
            .iter()
            .flatten()
            .map(|o| o.name.clone())
            .collect()
    }
}

/// Reads an archive. A member that is not a wasm object (a symbol table is not a member, but
/// metadata a toolchain adds can be) is skipped; a wasm member that cannot be read is an error.
pub fn read_archive(name: &str, bytes: &[u8]) -> Result<Archive> {
    if bytes.starts_with(b"!<thin>\n") {
        bail!("{name} is a thin archive, whose members live in other files; make a regular one");
    }
    let file =
        ArchiveFile::parse(bytes).map_err(|e| anyhow::anyhow!("{name} is not an archive: {e}"))?;
    let mut members = Vec::new();
    for member in file.members() {
        let member = member.map_err(|e| anyhow::anyhow!("{name}: reading a member: {e}"))?;
        let member_name = String::from_utf8_lossy(member.name()).into_owned();
        let data = member
            .data(bytes)
            .map_err(|e| anyhow::anyhow!("{name}({member_name}): {e}"))?;
        if !data.starts_with(b"\0asm") {
            continue;
        }
        let object = read(&format!("{name}({member_name})"), data)
            .with_context(|| format!("reading member {member_name} of {name}"))?;
        members.push(Some(object));
    }
    Ok(Archive {
        name: name.to_string(),
        members,
    })
}

/// The function names a file defines for other objects to call: an archive's members', or one
/// object's. A file that is neither is an error.
pub fn defined_functions(name: &str, bytes: &[u8]) -> Result<HashSet<String>> {
    let objects: Vec<Object> = if is_archive(bytes) {
        read_archive(name, bytes)?
            .members
            .into_iter()
            .flatten()
            .collect()
    } else {
        vec![read(name, bytes)?]
    };
    let mut names = HashSet::new();
    for obj in &objects {
        for (_, (kind, symbol), _) in definitions(obj) {
            if kind == Kind::Function {
                names.insert(symbol);
            }
        }
    }
    Ok(names)
}

/// A definition in a pulled member that gave way to one the link already had.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Shadowed {
    pub symbol: String,
    /// The member whose copy was dropped.
    pub member: String,
    /// The object whose definition every reference now binds to.
    pub kept: String,
}

/// What an archive search added.
#[derive(Default)]
pub struct Pulled {
    /// The members the link needs, in the order they were pulled.
    pub objects: Vec<Object>,
    /// How many members were pulled from each archive, in the order the archives were given.
    pub taken: Vec<usize>,
    pub shadowed: Vec<Shadowed>,
    /// Functions the pulled members name that nothing defines and no host supplies. Each is bound
    /// to the trap the linker writes for a weak reference, so reaching one stops there.
    pub stubbed: Vec<String>,
}

impl Pulled {
    /// The dropped definitions whose symbol is one of `names`: functions the program calls by
    /// name that a member defined and something already linked defined too. The program's call
    /// reaches the winner, which is a different function when the two are unrelated primitives.
    pub fn shadowed_named(&self, names: &[String]) -> Vec<&Shadowed> {
        self.shadowed
            .iter()
            .filter(|s| names.contains(&s.symbol))
            .collect()
    }
}

type Key = (Kind, String);

/// The global symbols an object defines.
fn definitions(obj: &Object) -> impl Iterator<Item = (usize, Key, bool)> + '_ {
    obj.symbols.iter().enumerate().filter_map(|(i, sym)| {
        let name = obj.symbol_name(sym);
        if !sym.defines() || sym.is_local() || name.is_empty() {
            return None;
        }
        Some((i, (kind_of(&sym.target)?, name.to_string()), sym.is_weak()))
    })
}

/// The symbols an object refers to and does not define, as a native linker counts them: only a
/// symbol a kept section relocates against, and not one that is weak, which may stay unresolved.
fn references(obj: &Object) -> Vec<Key> {
    let referenced: HashSet<u32> = obj
        .code_relocs
        .iter()
        .chain(obj.data_relocs.iter())
        .filter(|entry| entry.ty != RelocationType::TypeIndexLeb)
        .map(|entry| entry.index)
        .collect();
    obj.symbols
        .iter()
        .enumerate()
        .filter(|(i, sym)| {
            sym.is_undefined() && !sym.is_weak() && referenced.contains(&(*i as u32))
        })
        .filter_map(|(_, sym)| {
            let name = obj.symbol_name(sym);
            if name.is_empty() {
                return None;
            }
            Some((kind_of(&sym.target)?, name.to_string()))
        })
        .collect()
}

/// Turns a definition into a reference to the name it carried.
fn demote(obj: &mut Object, symbol: usize) {
    let flags = &mut obj.symbols[symbol].flags;
    *flags |= SymbolFlags::UNDEFINED;
    // A reference cannot be a root, and an exported name belongs to the definition that won.
    *flags &= !(SymbolFlags::NO_STRIP | SymbolFlags::EXPORTED);
}

/// The members of `archives` the link of `base` needs.
///
/// `base` is every object that is in the link whatever the archives hold, the runtime included,
/// so a definition any of them has is known before a member is looked at. The result goes into
/// the link beside `base`.
pub fn pull(base: &[Object], mut archives: Vec<Archive>) -> Result<Pulled> {
    let mut defined: HashMap<Key, (String, bool)> = HashMap::new();
    let mut wanted: VecDeque<Key> = VecDeque::new();
    for obj in base {
        for (_, key, weak) in definitions(obj) {
            let strong_over_weak = defined.get(&key).is_some_and(|(_, w)| *w) && !weak;
            if strong_over_weak || !defined.contains_key(&key) {
                defined.insert(key, (obj.name.clone(), weak));
            }
        }
        wanted.extend(references(obj));
    }

    // Which members define a name, in archive order then member order.
    let mut index: HashMap<Key, Vec<(usize, usize)>> = HashMap::new();
    for (a, archive) in archives.iter().enumerate() {
        for (m, member) in archive.members.iter().enumerate() {
            let Some(obj) = member else { continue };
            for (_, key, _) in definitions(obj) {
                index.entry(key).or_default().push((a, m));
            }
        }
    }

    let mut pulled = Pulled {
        taken: vec![0; archives.len()],
        ..Pulled::default()
    };
    while let Some(key) = wanted.pop_front() {
        if defined.contains_key(&key) {
            continue;
        }
        let Some(&(a, m)) = index
            .get(&key)
            .and_then(|at| at.iter().find(|(a, m)| archives[*a].members[*m].is_some()))
        else {
            continue;
        };
        let mut obj = archives[a].members[m].take().expect("checked just above");
        let found: Vec<(usize, Key, bool)> = definitions(&obj).collect();
        for (symbol, key, weak) in found {
            match defined.get(&key).cloned() {
                None => {
                    defined.insert(key, (obj.name.clone(), weak));
                }
                // A weak definition never collides with anything.
                Some(_) if weak => {}
                // A strong one displaces a weak one, as the linker's own rule has it.
                Some((_, true)) => {
                    defined.insert(key, (obj.name.clone(), false));
                }
                Some((kept, false)) => {
                    pulled.shadowed.push(Shadowed {
                        symbol: key.1.clone(),
                        member: obj.name.clone(),
                        kept,
                    });
                    demote(&mut obj, symbol);
                }
            }
        }
        wanted.extend(references(&obj));
        pulled.taken[a] += 1;
        pulled.objects.push(obj);
    }

    // A library carries what it was built with, `std` included, and `std` names libc functions
    // the runtime had no use for. Most of them are never called. Refusing the build over each
    // would make every library unlinkable, so a function that nothing defines and no host can
    // supply is bound to the trap a weak reference gets: a program that never reaches it is
    // unaffected, and one that does stops at a call that says what it was.
    for obj in &mut pulled.objects {
        let func_imports: Vec<(String, String)> = obj
            .imports
            .iter()
            .filter(|i| matches!(i.kind, ImportKind::Function { .. }))
            .map(|i| (i.module.clone(), i.name.clone()))
            .collect();
        for symbol in 0..obj.symbols.len() {
            let sym = &obj.symbols[symbol];
            let SymbolTarget::Function { index } = sym.target else {
                continue;
            };
            if !sym.is_undefined() || sym.is_weak() {
                continue;
            }
            let Some((module, import)) = func_imports.get(index as usize) else {
                continue;
            };
            let name = obj.symbol_name(sym).to_string();
            if name.is_empty()
                || host_can_supply(module, import)
                || defined.contains_key(&(Kind::Function, name.clone()))
            {
                continue;
            }
            obj.symbols[symbol].flags |= SymbolFlags::BINDING_WEAK;
            pulled.stubbed.push(name);
        }
    }
    pulled.stubbed.sort_unstable();
    pulled.stubbed.dedup();
    Ok(pulled)
}
