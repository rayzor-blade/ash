//! Joining relocatable objects into a module.
//!
//! The shape of the job, in order: decide what defines what, give every index
//! space its final numbering, place the data in linear memory, write the
//! resolved values into the patch sites, and emit.
//!
//! # What makes this small enough to trust
//!
//! Two inputs, not a thousand: the program and one prelinked runtime. No
//! archives, no lazy pull.
//!
//! Tree shaking is here, but it arrived second and deliberately so. A
//! `--no-gc-sections` link of exactly these two objects was measured to run
//! correctly first, which established that reachability is an optimisation
//! rather than a correctness requirement -- and that meant the linker could
//! be proven right before it was made small. It removes about a third of the
//! module.
//!
//! # The rule every patch follows
//!
//! A relocation entry names a symbol and an offset. The value written is
//! whatever that symbol resolved to in the output, and the width is fixed by
//! the relocation type, so nothing moves and no offset is ever recomputed.
//! The one exception is `TYPE_INDEX_LEB`, whose index is a type index rather
//! than a symbol index -- a distinction that is invisible at the call site
//! and would silently mis-type every `call_indirect` if missed.

use std::collections::HashMap;

use anyhow::{anyhow, bail, Context, Result};
use wasmparser::{RelocationEntry, RelocationType};

use crate::object::{ImportKind, ObjImport, Object, SymbolTarget};

/// How the output is laid out.
#[derive(Debug, Clone)]
pub struct LinkOptions {
    /// Bytes reserved for the shadow stack, which occupies the bottom of
    /// linear memory with data above it. `__stack_pointer` starts at the top
    /// of this region and grows down, so an overflow runs into address zero
    /// rather than into the program's own data.
    pub stack_size: u32,
    /// Export every defined function that is neither local nor hidden.
    ///
    /// Off by default, and that is a size decision rather than a taste one:
    /// an exported function is reachable by definition, so exporting
    /// everything pins every function in the module and leaves tree shaking
    /// nothing to remove.
    pub export_all_functions: bool,
    /// Drop functions nothing can reach.
    pub tree_shake: bool,
    /// Names to keep whatever else happens, because the host calls them by
    /// name and no relocation points at them.
    pub roots: Vec<String>,
}

impl Default for LinkOptions {
    fn default() -> Self {
        // One page, which is what LLD reserves, and what the module this
        // linker replaces was verified running with.
        Self {
            stack_size: 65536,
            export_all_functions: false,
            tree_shake: true,
            roots: ["main", "ash_module_init", "_start", "_initialize"]
                .iter()
                .map(|s| s.to_string())
                .collect(),
        }
    }
}

/// The first usable table slot. Zero is left empty so that calling a null
/// function pointer traps rather than calling whatever landed first, and
/// `__table_base` is this same number.
const TABLE_BASE: u32 = 1;

/// Where a symbol ended up in the output.
#[derive(Debug, Clone, Copy)]
enum Resolved {
    Function(u32),
    /// An address in linear memory.
    Data(u32),
    Global(u32),
    Table(u32),
    Tag(u32),
}

/// A function import that survived into the output, keyed by what it names.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ImportKey {
    module: String,
    name: String,
}

struct Layout {
    /// Output index of each object's locally defined functions, or `None`
    /// for one that tree shaking removed.
    func_out: Vec<Vec<Option<u32>>>,
    /// Output type index for each object's local type indices.
    type_map: Vec<Vec<u32>>,
    /// Address of each object's data segments.
    segment_addr: Vec<Vec<u32>>,
    /// Table slot for an output function index, for functions whose address
    /// is taken.
    table_slot: HashMap<u32, u32>,
    /// Imports, in output order.
    imports: Vec<(ImportKey, u32)>,
    /// Output index of an imported function, by what it names.
    import_index: HashMap<ImportKey, u32>,
    /// The output tag index, if any object defined one.
    tag_index: Option<u32>,
    stack_pointer_global: u32,
    memory_base_global: u32,
    table_base_global: u32,
    /// The global that holds a symbol's address, for a symbol accessed
    /// through the global offset table. Values are output global indices.
    got: HashMap<(Kind, String), u32>,
    /// What each GOT global is initialised to, in output-index order after
    /// the three the linker defines outright.
    got_init: Vec<i32>,
    heap_base: u32,
    memory_pages: u32,
    /// Where data starts, which is also the top of the shadow stack.
    data_base: u32,
    /// One past the last byte of placed data.
    data_end: u32,
}

/// Link `objects` into a module.
pub fn link(mut objects: Vec<Object>, opts: &LinkOptions) -> Result<Vec<u8>> {
    refuse_unsupported(&objects)?;

    let defs = resolve_definitions(&objects)?;
    let layout = plan(&objects, &defs, opts)?;
    report_unresolved(&objects, &defs, &layout)?;
    // Patching mutates each object's kept payloads in place.
    apply_relocations(&mut objects, &defs, &layout)?;
    emit(&objects, &defs, &layout, opts)
}

/// Say no to what has not been implemented, rather than producing a module
/// that is quietly missing something.
fn refuse_unsupported(objects: &[Object]) -> Result<()> {
    for obj in objects {
        for sym in &obj.symbols {
            if sym.is_tls() {
                bail!(
                    "{}: symbol {} is thread-local, which this linker does not lay out",
                    obj.name,
                    sym.name
                );
            }
        }
        for (i, seg) in obj.data_segments.iter().enumerate() {
            if seg.passive {
                bail!(
                    "{}: data segment {i} is passive. Passive segments are addressed by \
                     index by `memory.init`, and this linker merges every segment into \
                     one, which would renumber them",
                    obj.name
                );
            }
        }
        if !obj.globals.is_empty() {
            bail!(
                "{}: defines {} globals. Their initialisers can hold relocated values \
                 and no `reloc.GLOBAL` section exists to describe them, so they cannot \
                 be copied through safely",
                obj.name,
                obj.globals.len()
            );
        }
    }
    Ok(())
}

/// Name to the object and symbol that defines it.
fn resolve_definitions(objects: &[Object]) -> Result<HashMap<(Kind, String), (usize, usize)>> {
    let mut defs: HashMap<(Kind, String), (usize, usize)> = HashMap::new();
    for (oi, obj) in objects.iter().enumerate() {
        for (si, sym) in obj.symbols.iter().enumerate() {
            let name = obj.symbol_name(sym);
            if !sym.defines() || sym.is_local() || name.is_empty() {
                continue;
            }
            let Some(kind) = kind_of(&sym.target) else {
                continue;
            };
            let key = (kind, name.to_string());
            match defs.get(&key) {
                None => {
                    defs.insert(key, (oi, si));
                }
                Some(&(poi, psi)) => {
                    let previous = &objects[poi].symbols[psi];
                    // A strong definition displaces a weak one. Two strong
                    // definitions of the same name are a real conflict and
                    // the program cannot be linked as written.
                    if previous.is_weak() && !sym.is_weak() {
                        defs.insert(key, (oi, si));
                    } else if !previous.is_weak() && !sym.is_weak() {
                        bail!(
                            "{} is defined in both {} and {}",
                            name,
                            objects[poi].name,
                            obj.name
                        );
                    }
                }
            }
        }
    }
    Ok(defs)
}

/// The function imports of an object, in the order they occupy the bottom of
/// its function index space.
fn function_imports(obj: &Object) -> Vec<&ObjImport> {
    obj.imports
        .iter()
        .filter(|i| matches!(i.kind, ImportKind::Function { .. }))
        .collect()
}

fn plan(
    objects: &[Object],
    defs: &HashMap<(Kind, String), (usize, usize)>,
    opts: &LinkOptions,
) -> Result<Layout> {
    // --- imports that nothing defines ---
    let mut imports: Vec<(ImportKey, u32)> = Vec::new();
    let mut import_index: HashMap<ImportKey, u32> = HashMap::new();
    let mut type_map: Vec<Vec<u32>> = Vec::new();

    // Types are deduplicated across objects, because two objects that call
    // the same shape must agree on its index.
    let mut type_keys: HashMap<String, u32> = HashMap::new();
    let mut next_type: u32 = 0;
    for obj in objects {
        let mut map = Vec::with_capacity(obj.types.len());
        for ty in &obj.types {
            let key = type_key(ty);
            let index = *type_keys.entry(key).or_insert_with(|| {
                let i = next_type;
                next_type += 1;
                i
            });
            map.push(index);
        }
        type_map.push(map);
    }

    for (oi, obj) in objects.iter().enumerate() {
        let func_imports = function_imports(obj);
        for sym in &obj.symbols {
            if !sym.is_undefined() {
                continue;
            }
            if defs.contains_key(&(Kind::Function, obj.symbol_name(sym).to_string())) {
                continue;
            }
            // Undefined and undefinable: it has to come from the host. Only
            // a function can, and the object's own import entry says under
            // what name.
            let Some(local) = undefined_function_index(sym) else {
                continue;
            };
            let Some(import) = func_imports.get(local as usize) else {
                bail!(
                    "{}: undefined symbol {} has no import entry",
                    obj.name,
                    sym.name
                );
            };
            let key = ImportKey {
                module: import.module.clone(),
                name: import.name.clone(),
            };
            if import_index.contains_key(&key) {
                continue;
            }
            let ImportKind::Function { type_index } = import.kind else {
                continue;
            };
            let out_type = type_map[oi][type_index as usize];
            let index = imports.len() as u32;
            import_index.insert(key.clone(), index);
            imports.push((key, out_type));
        }
    }

    // --- function index space: imports, then whatever survives ---
    let kept = if opts.tree_shake {
        mark_reachable(objects, defs, opts)?
    } else {
        objects
            .iter()
            .map(|o| vec![true; o.functions.len()])
            .collect()
    };
    let mut func_out: Vec<Vec<Option<u32>>> = Vec::with_capacity(objects.len());
    let mut next = imports.len() as u32;
    for keep in &kept {
        let mut map = Vec::with_capacity(keep.len());
        for &k in keep {
            if k {
                map.push(Some(next));
                next += 1;
            } else {
                map.push(None);
            }
        }
        func_out.push(map);
    }

    // --- linker-defined globals ---
    let stack_pointer_global = 0;
    let memory_base_global = 1;

    // --- data layout ---
    // The stack is first, so a stack overflow walks into address zero rather
    // than into the program's own data.
    let mut address = opts.stack_size;
    let mut segment_addr = Vec::with_capacity(objects.len());
    for obj in objects {
        let mut addrs = Vec::with_capacity(obj.data_segments.len());
        for (i, seg) in obj.data_segments.iter().enumerate() {
            let align_log2 = obj
                .segment_info
                .get(i)
                .map(|s| s.align_log2)
                .unwrap_or(0)
                .min(16);
            let align = 1u32 << align_log2;
            address = address.next_multiple_of(align);
            addrs.push(address);
            address += (seg.range.end - seg.range.start) as u32;
        }
        segment_addr.push(addrs);
    }
    let heap_base = address.next_multiple_of(16);
    let memory_pages = heap_base.div_ceil(65536).max(1);

    // --- table slots for functions whose address is taken ---
    //
    // Driven by the relocations that need a slot, not by the objects' element
    // segments. An element segment records the addresses taken *within one
    // object*, so a program that takes the address of a runtime function has
    // a relocation for it and no element entry anywhere -- which reads as
    // "this function was given no table slot" at patch time. The relocations
    // are the complete list by construction: every one of them is a place
    // that will hold a slot number.
    //
    // Slot zero is left empty so that calling a null function pointer traps
    // rather than calling whatever landed first.
    let mut table_slot: HashMap<u32, u32> = HashMap::new();
    let mut next_slot: u32 = TABLE_BASE;
    let mut give_slot = |out: u32, table_slot: &mut HashMap<u32, u32>| {
        table_slot.entry(out).or_insert_with(|| {
            let s = next_slot;
            next_slot += 1;
            s
        });
    };
    for (oi, obj) in objects.iter().enumerate() {
        // Only the code that survived. A relocation inside a function that
        // tree shaking removed is a slot nobody will ever read, and asking
        // for it would resurrect the whole graph it points into.
        let live_code = obj
            .code_bodies
            .iter()
            .enumerate()
            .filter(|(i, _)| func_out[oi][*i].is_some())
            .flat_map(|(_, body)| relocations_in(&obj.code_relocs, body));
        for entry in live_code.chain(obj.data_relocs.iter()) {
            if !matches!(
                entry.ty,
                RelocationType::TableIndexSleb | RelocationType::TableIndexI32
            ) {
                continue;
            }
            let out = function_symbol(objects, defs, &func_out, &import_index, oi, entry.index)?;
            give_slot(out, &mut table_slot);
        }
        // Element entries too: a function listed there but never relocated is
        // reachable only through the table, and dropping it would leave a
        // hole where a call_indirect expects a body.
        for &local in &obj.elements {
            let Some(out) = local_function(objects, defs, &func_out, &import_index, oi, local)?
            else {
                // Removed by tree shaking. Nothing that survived references
                // its slot, or it would have been kept.
                continue;
            };
            give_slot(out, &mut table_slot);
        }
    }

    // --- the global offset table ---
    //
    // A `GLOBAL_INDEX` relocation is allowed to name a DATA symbol, and then
    // it does not mean "this global" -- it means "the global holding that
    // symbol's address". That is the GOT, and the object asks for it by
    // importing `GOT.mem.<name>`. ash emits a handful for large constants.
    //
    // Each one becomes an ordinary immutable global initialised to the
    // address the layout just assigned, so the code reading it needs no
    // relocation of its own.
    let mut got: HashMap<(Kind, String), u32> = HashMap::new();
    let mut got_init: Vec<i32> = Vec::new();
    let first_got = 3u32; // after __stack_pointer, __memory_base, __table_base
    for (oi, obj) in objects.iter().enumerate() {
        for entry in obj.code_relocs.iter().chain(obj.data_relocs.iter()) {
            if !matches!(
                entry.ty,
                RelocationType::GlobalIndexLeb | RelocationType::GlobalIndexI32
            ) {
                continue;
            }
            let Some(sym) = obj.symbols.get(entry.index as usize) else {
                continue;
            };
            let (doi, def) = definition_of(objects, defs, oi, sym);
            let name = objects[doi].symbol_name(def).to_string();
            let value = match def.target {
                SymbolTarget::Data {
                    segment, offset, ..
                } => {
                    let base = segment_addr
                        .get(doi)
                        .and_then(|s| s.get(segment as usize))
                        .ok_or_else(|| anyhow!("{name}: data segment {segment} out of range"))?;
                    (base + offset) as i32
                }
                // A global symbol resolves to the global itself, not a GOT
                // entry, and needs nothing here.
                SymbolTarget::Global { .. } => continue,
                SymbolTarget::Undefined => match linker_address_early(&name, opts, address) {
                    Some(v) => v as i32,
                    None => continue,
                },
                _ => continue,
            };
            let key = (Kind::Data, name);
            if got.contains_key(&key) {
                continue;
            }
            got.insert(key, first_got + got_init.len() as u32);
            got_init.push(value);
        }
    }

    let tag_index = objects.iter().any(|o| !o.tags.is_empty()).then_some(0);

    Ok(Layout {
        data_base: opts.stack_size,
        data_end: address,
        func_out,
        type_map,
        segment_addr,
        table_slot,
        imports,
        import_index,
        tag_index,
        stack_pointer_global,
        memory_base_global,
        table_base_global: 2,
        got,
        got_init,
        heap_base,
        memory_pages,
    })
}

/// A structural key for a function type, so two objects that describe the
/// same shape share one output type.
fn type_key(ty: &wasmparser::FuncType) -> String {
    let mut key = String::new();
    for p in ty.params() {
        key.push_str(&format!("{p:?},"));
    }
    key.push(';');
    for r in ty.results() {
        key.push_str(&format!("{r:?},"));
    }
    key
}

/// The function-space index an undefined function symbol occupies, which is
/// also its position among that object's function imports.
fn undefined_function_index(sym: &crate::object::Symbol) -> Option<u32> {
    match sym.target {
        SymbolTarget::Function { index } => Some(index),
        _ => None,
    }
}

/// Which defined functions anything can reach.
///
/// # What is a root, and why the export list matters
///
/// A function is kept if the host can name it, if a kept function calls it,
/// or if any data holds its address. The last of those is what keeps this
/// sound for a program like ash: its compiled code reaches most functions
/// through a table whose entries are written into *data* by relocations, not
/// through direct calls. Data is kept whole here, so every function whose
/// address is written anywhere is a root, and a function reached by pointer
/// arithmetic cannot be removed by accident.
///
/// The cost of that soundness is real -- it keeps everything ash's own type
/// tables point at -- and it is the right trade while the alternative is a
/// module that validates, runs, and calls into a hole.
fn mark_reachable(
    objects: &[Object],
    defs: &HashMap<(Kind, String), (usize, usize)>,
    opts: &LinkOptions,
) -> Result<Vec<Vec<bool>>> {
    let mut kept: Vec<Vec<bool>> = objects
        .iter()
        .map(|o| vec![false; o.functions.len()])
        .collect();
    let mut work: Vec<(usize, u32)> = Vec::new();

    let root =
        |oi: usize, defined: u32, kept: &mut Vec<Vec<bool>>, work: &mut Vec<(usize, u32)>| {
            if let Some(slot) = kept[oi].get_mut(defined as usize) {
                if !*slot {
                    *slot = true;
                    work.push((oi, defined));
                }
            }
        };

    // Named by the host, asked to be exported, or marked no-strip.
    for (oi, obj) in objects.iter().enumerate() {
        for sym in &obj.symbols {
            let SymbolTarget::Function { index } = sym.target else {
                continue;
            };
            if !sym.defines() {
                continue;
            }
            let name = obj.symbol_name(sym);
            let wanted = sym.is_exported()
                || sym.is_no_strip()
                || opts.roots.iter().any(|r| r == name)
                || (opts.export_all_functions && !sym.is_local() && !sym.is_hidden());
            if !wanted {
                continue;
            }
            let imported = obj.imported_functions();
            if index >= imported {
                root(oi, index - imported, &mut kept, &mut work);
            }
        }
        // Constructors run before anything else, so they are reachable by
        // definition even though nothing calls them.
        for init in &obj.init_funcs {
            if let Some(sym) = obj.symbols.get(init.symbol as usize) {
                let (doi, def) = definition_of(objects, defs, oi, sym);
                if let SymbolTarget::Function { index } = def.target {
                    let imported = objects[doi].imported_functions();
                    if index >= imported {
                        root(doi, index - imported, &mut kept, &mut work);
                    }
                }
            }
        }
        // Every address written into data, because all data is kept.
        for entry in &obj.data_relocs {
            if !matches!(
                entry.ty,
                RelocationType::TableIndexSleb
                    | RelocationType::TableIndexI32
                    | RelocationType::FunctionIndexLeb
            ) {
                continue;
            }
            let Some(sym) = obj.symbols.get(entry.index as usize) else {
                continue;
            };
            let (doi, def) = definition_of(objects, defs, oi, sym);
            if let SymbolTarget::Function { index } = def.target {
                let imported = objects[doi].imported_functions();
                if index >= imported {
                    root(doi, index - imported, &mut kept, &mut work);
                }
            }
        }
    }

    // Then everything those reach, transitively.
    while let Some((oi, defined)) = work.pop() {
        let obj = &objects[oi];
        let Some(body) = obj.code_bodies.get(defined as usize) else {
            continue;
        };
        for entry in relocations_in(&obj.code_relocs, body) {
            if !matches!(
                entry.ty,
                RelocationType::FunctionIndexLeb
                    | RelocationType::TableIndexSleb
                    | RelocationType::TableIndexI32
            ) {
                continue;
            }
            let Some(sym) = obj.symbols.get(entry.index as usize) else {
                continue;
            };
            let (doi, def) = definition_of(objects, defs, oi, sym);
            if let SymbolTarget::Function { index } = def.target {
                let imported = objects[doi].imported_functions();
                if index >= imported {
                    root(doi, index - imported, &mut kept, &mut work);
                }
            }
        }
    }

    Ok(kept)
}

/// The relocations that fall inside one function body.
///
/// `reloc.*` entries are emitted in ascending offset order, so the ones
/// belonging to a body are a contiguous run and can be found by bisection
/// rather than by scanning sixty thousand entries per function.
fn relocations_in<'a>(
    relocs: &'a [RelocationEntry],
    body: &std::ops::Range<usize>,
) -> &'a [RelocationEntry] {
    let start = relocs.partition_point(|r| (r.offset as usize) < body.start);
    let end = relocs.partition_point(|r| (r.offset as usize) < body.end);
    &relocs[start..end]
}

/// What index space a symbol lives in.
///
/// Part of a symbol's identity, not a detail of it. The linking format lets a
/// data symbol and a global symbol share a name -- ash emits exactly that for
/// a constant and the global that addresses it -- so a table keyed by name
/// alone answers a global relocation with a data symbol. It surfaced as
/// "Bytes_0 resolved to Data rather than a global" on a large program; had
/// the two kinds been compatible instead of obviously wrong, it would have
/// resolved to the wrong thing quietly.
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
enum Kind {
    Function,
    Data,
    Global,
    Table,
    Tag,
    Section,
}

fn kind_of(target: &SymbolTarget) -> Option<Kind> {
    Some(match target {
        SymbolTarget::Function { .. } => Kind::Function,
        SymbolTarget::Data { .. } => Kind::Data,
        SymbolTarget::Global { .. } => Kind::Global,
        SymbolTarget::Table { .. } => Kind::Table,
        SymbolTarget::Tag { .. } => Kind::Tag,
        SymbolTarget::Section { .. } => Kind::Section,
        SymbolTarget::Undefined => return None,
    })
}

/// The kind an undefined symbol is looking for, which its own target still
/// records even though it defines nothing.
fn wanted_kind(sym: &crate::object::Symbol) -> Option<Kind> {
    kind_of(&sym.target)
}

/// Which symbol actually defines the one being referenced.
///
/// The locality check is the whole point. A local symbol is private to its
/// object, so two objects may each have a different `foo` and both be right;
/// looking `foo` up in the global table would bind one object's private
/// function to the other's public one. Nothing about that fails: the call
/// site has the right shape, the module validates, and the program calls the
/// wrong function -- which is how it first showed up here, as JSON encoding
/// returning an empty string while everything around it was correct.
fn definition_of<'a>(
    objects: &'a [Object],
    defs: &HashMap<(Kind, String), (usize, usize)>,
    oi: usize,
    sym: &'a crate::object::Symbol,
) -> (usize, &'a crate::object::Symbol) {
    if sym.defines() && sym.is_local() {
        return (oi, sym);
    }
    let Some(kind) = wanted_kind(sym) else {
        return (oi, sym);
    };
    match defs.get(&(kind, objects[oi].symbol_name(sym).to_string())) {
        Some(&(doi, dsi)) => (doi, &objects[doi].symbols[dsi]),
        None => (oi, sym),
    }
}

/// The output function index a symbol names, using only the parts of the
/// layout that exist before table slots are assigned.
fn function_symbol(
    objects: &[Object],
    defs: &HashMap<(Kind, String), (usize, usize)>,
    func_out: &[Vec<Option<u32>>],
    import_index: &HashMap<ImportKey, u32>,
    oi: usize,
    sym_index: u32,
) -> Result<u32> {
    let obj = &objects[oi];
    let sym = obj
        .symbols
        .get(sym_index as usize)
        .ok_or_else(|| anyhow!("{}: symbol {sym_index} is out of range", obj.name))?;
    let (doi, def) = definition_of(objects, defs, oi, sym);
    let SymbolTarget::Function { index } = def.target else {
        bail!(
            "{} is not a function, but its address is taken",
            objects[doi].symbol_name(def)
        );
    };
    let imported = objects[doi].imported_functions();
    if index >= imported {
        return func_out[doi][(index - imported) as usize].ok_or_else(|| {
            anyhow!(
                "{} was removed by tree shaking but its address is taken",
                objects[doi].symbol_name(def)
            )
        });
    }
    let func_imports = function_imports(&objects[doi]);
    let import = func_imports
        .get(index as usize)
        .ok_or_else(|| anyhow!("{}: function {index} is not an import", objects[doi].name))?;
    let key = ImportKey {
        module: import.module.clone(),
        name: import.name.clone(),
    };
    import_index
        .get(&key)
        .copied()
        .ok_or_else(|| anyhow!("no import assigned for {}.{}", key.module, key.name))
}

/// Map one object's local function index to the output.
fn local_function(
    objects: &[Object],
    defs: &HashMap<(Kind, String), (usize, usize)>,
    func_out: &[Vec<Option<u32>>],
    import_index: &HashMap<ImportKey, u32>,
    oi: usize,
    local: u32,
) -> Result<Option<u32>> {
    let obj = &objects[oi];
    let imported = obj.imported_functions();
    if local >= imported {
        return Ok(func_out[oi][(local - imported) as usize]);
    }
    // An imported slot: whatever the corresponding symbol resolves to.
    let func_imports = function_imports(obj);
    let import = func_imports
        .get(local as usize)
        .ok_or_else(|| anyhow!("{}: function {local} is not an import", obj.name))?;
    if let Some(&(doi, dsi)) = defs.get(&(Kind::Function, import.name.clone())) {
        let def = &objects[doi].symbols[dsi];
        if let SymbolTarget::Function { index } = def.target {
            let d_imported = objects[doi].imported_functions();
            if index >= d_imported {
                return Ok(func_out[doi][(index - d_imported) as usize]);
            }
        }
    }
    let key = ImportKey {
        module: import.module.clone(),
        name: import.name.clone(),
    };
    import_index.get(&key).copied().map(Some).ok_or_else(|| {
        anyhow!(
            "{}: no import assigned for {}.{}",
            obj.name,
            key.module,
            key.name
        )
    })
}

/// Name every symbol nothing can define, in one go.
///
/// Patching stops at the first one, which turns "this link is missing five
/// things" into five separate runs. Since the answer is usually a set -- the
/// addresses a linker is expected to define, for instance -- it is worth
/// saying so all at once.
fn report_unresolved(
    objects: &[Object],
    defs: &HashMap<(Kind, String), (usize, usize)>,
    layout: &Layout,
) -> Result<()> {
    let mut missing: Vec<String> = Vec::new();
    for obj in objects {
        for sym in &obj.symbols {
            if !sym.is_undefined() {
                continue;
            }
            let name = obj.symbol_name(sym);
            let known = wanted_kind(sym).is_some_and(|k| defs.contains_key(&(k, name.to_string())));
            if name.is_empty() || known {
                continue;
            }
            // A function can be imported from the host; an address cannot.
            if matches!(sym.target, SymbolTarget::Function { .. }) {
                continue;
            }
            if linker_address(name, layout).is_some() {
                continue;
            }
            if matches!(sym.target, SymbolTarget::Global { .. })
                && matches!(name, "__stack_pointer" | "__memory_base" | "__table_base")
            {
                continue;
            }
            if matches!(
                sym.target,
                SymbolTarget::Table { .. } | SymbolTarget::Tag { .. }
            ) {
                continue;
            }
            if !missing.contains(&name.to_string()) {
                missing.push(name.to_string());
            }
        }
    }
    if !missing.is_empty() {
        bail!(
            "{} symbol(s) nothing defines and nothing can import: {}",
            missing.len(),
            missing.join(", ")
        );
    }
    Ok(())
}

/// The linker-defined addresses, for the one caller that needs them while the
/// layout is still being built.
fn linker_address_early(name: &str, opts: &LinkOptions, data_end: u32) -> Option<u32> {
    Some(match name {
        "__heap_base" => data_end.next_multiple_of(16),
        "__data_end" => data_end,
        "__global_base" | "__stack_high" => opts.stack_size,
        "__wasm_first_page_end" => 65536,
        "__stack_low" => 0,
        _ => return None,
    })
}

/// The addresses only the linker can know, because only it placed the data.
///
/// These are the names LLD defines, with the same meanings, because the
/// runtime being linked was compiled against LLD's contract: `ash_std`'s
/// allocator asks where the heap starts and how far memory goes, and answers
/// that disagree with the data layout produce a heap that overlaps the
/// program's own constants.
fn linker_address(name: &str, layout: &Layout) -> Option<u32> {
    Some(match name {
        // Where the data ends and the allocator may begin.
        "__heap_base" => layout.heap_base,
        // The end of the memory the module starts with.
        "__heap_end" => layout.memory_pages * 65536,
        "__data_end" => layout.data_end,
        // The bottom of the data region, above the shadow stack.
        "__global_base" => layout.data_base,
        "__wasm_first_page_end" => 65536,
        // The shadow stack occupies the bottom of memory and grows down, so
        // "low" is zero and "high" is where `__stack_pointer` starts.
        "__stack_low" => 0,
        "__stack_high" => layout.data_base,
        _ => return None,
    })
}

/// What a symbol referenced by a relocation resolves to in the output.
fn resolve_symbol(
    objects: &[Object],
    defs: &HashMap<(Kind, String), (usize, usize)>,
    layout: &Layout,
    oi: usize,
    sym_index: u32,
) -> Result<Resolved> {
    let obj = &objects[oi];
    let sym = obj
        .symbols
        .get(sym_index as usize)
        .ok_or_else(|| anyhow!("{}: symbol {sym_index} is out of range", obj.name))?;

    let (doi, def) = definition_of(objects, defs, oi, sym);

    match def.target {
        SymbolTarget::Function { index } => {
            let imported = objects[doi].imported_functions();
            if index >= imported {
                layout.func_out[doi][(index - imported) as usize]
                    .map(Resolved::Function)
                    .ok_or_else(|| {
                        anyhow!(
                            "{} was removed by tree shaking but something calls it",
                            objects[doi].symbol_name(def)
                        )
                    })
            } else {
                let func_imports = function_imports(&objects[doi]);
                let import = func_imports.get(index as usize).ok_or_else(|| {
                    anyhow!("{}: function {index} is not an import", objects[doi].name)
                })?;
                let key = ImportKey {
                    module: import.module.clone(),
                    name: import.name.clone(),
                };
                layout
                    .import_index
                    .get(&key)
                    .copied()
                    .map(Resolved::Function)
                    .ok_or_else(|| {
                        anyhow!("no import assigned for {}", objects[doi].symbol_name(def))
                    })
            }
        }
        SymbolTarget::Data {
            segment, offset, ..
        } => {
            let base = layout
                .segment_addr
                .get(doi)
                .and_then(|s| s.get(segment as usize))
                .ok_or_else(|| {
                    anyhow!(
                        "{}: data segment {segment} is out of range",
                        objects[doi].name
                    )
                })?;
            Ok(Resolved::Data(base + offset))
        }
        SymbolTarget::Global { .. } => {
            // Every global in this link is one the linker defines; objects
            // define none. Which one is decided by name, because that is the
            // only thing an import carries.
            let index = match objects[doi].symbol_name(def) {
                "__stack_pointer" => layout.stack_pointer_global,
                "__memory_base" => layout.memory_base_global,
                "__table_base" => layout.table_base_global,
                other => bail!("no definition for global {other:?}"),
            };
            Ok(Resolved::Global(index))
        }
        SymbolTarget::Table { .. } => Ok(Resolved::Table(0)),
        SymbolTarget::Tag { .. } => {
            Ok(Resolved::Tag(layout.tag_index.ok_or_else(|| {
                anyhow!("a tag is referenced but none is defined")
            })?))
        }
        SymbolTarget::Section { .. } => {
            bail!("a section symbol is referenced outside the debug sections")
        }
        SymbolTarget::Undefined => {
            // Some addresses are the linker's to know, not any object's: an
            // object references `__heap_base` and leaves it undefined because
            // only the thing that placed the data can say where the data
            // ended. Anything else undefined is a real missing definition --
            // a wasm module cannot import an address, so there is nowhere
            // else for it to come from.
            let name = objects[doi].symbol_name(def);
            linker_address(name, layout)
                .map(Resolved::Data)
                .ok_or_else(|| anyhow!("undefined symbol: {name}"))
        }
    }
}

fn apply_relocations(
    objects: &mut [Object],
    defs: &HashMap<(Kind, String), (usize, usize)>,
    layout: &Layout,
) -> Result<()> {
    for oi in 0..objects.len() {
        // The payloads are taken out so the resolver can borrow the objects.
        // Only the bodies that survived. A relocation inside a removed
        // function points into the graph that removal was meant to drop, and
        // resolving it would report the removal as an error.
        let code_relocs = std::mem::take(&mut objects[oi].code_relocs);
        let live: Vec<RelocationEntry> = objects[oi]
            .code_bodies
            .iter()
            .enumerate()
            .filter(|(i, _)| layout.func_out[oi][*i].is_some())
            .flat_map(|(_, body)| relocations_in(&code_relocs, body).iter().copied())
            .collect();
        let mut code = std::mem::take(&mut objects[oi].code_payload);
        patch(objects, defs, layout, oi, &live, &mut code)
            .with_context(|| format!("{}: patching code", objects[oi].name))?;
        objects[oi].code_payload = code;
        objects[oi].code_relocs = code_relocs;

        let data_relocs = std::mem::take(&mut objects[oi].data_relocs);
        let mut data = std::mem::take(&mut objects[oi].data_payload);
        patch(objects, defs, layout, oi, &data_relocs, &mut data)
            .with_context(|| format!("{}: patching data", objects[oi].name))?;
        objects[oi].data_payload = data;
        objects[oi].data_relocs = data_relocs;
    }
    Ok(())
}

fn patch(
    objects: &[Object],
    defs: &HashMap<(Kind, String), (usize, usize)>,
    layout: &Layout,
    oi: usize,
    relocs: &[RelocationEntry],
    buf: &mut [u8],
) -> Result<()> {
    for entry in relocs {
        let offset = entry.offset as usize;
        let addend = entry.addend as i32;
        match entry.ty {
            // The one whose index is not a symbol index. Getting this wrong
            // types every `call_indirect` against the wrong signature.
            RelocationType::TypeIndexLeb => {
                let out = *layout.type_map[oi]
                    .get(entry.index as usize)
                    .ok_or_else(|| anyhow!("type {} is out of range", entry.index))?;
                write_u32_leb5(buf, offset, out)?;
            }
            RelocationType::FunctionIndexLeb => {
                let Resolved::Function(f) = resolve_symbol(objects, defs, layout, oi, entry.index)?
                else {
                    bail!("a function relocation names something that is not a function");
                };
                write_u32_leb5(buf, offset, f)?;
            }
            // The same slot, counted from `__table_base` rather than from
            // zero. The base is one here -- slot zero is left empty so a null
            // function pointer traps -- so this is the slot less one, and
            // writing the absolute value instead would call the neighbour.
            RelocationType::TableIndexRelSleb => {
                let Resolved::Function(f) = resolve_symbol(objects, defs, layout, oi, entry.index)?
                else {
                    bail!("a table relocation names something that is not a function");
                };
                let slot = *layout.table_slot.get(&f).ok_or_else(|| {
                    anyhow!("function {f} has its address taken but was given no table slot")
                })?;
                write_i32_leb5(buf, offset, slot as i32 - TABLE_BASE as i32)?;
            }
            RelocationType::TableIndexSleb | RelocationType::TableIndexI32 => {
                let Resolved::Function(f) = resolve_symbol(objects, defs, layout, oi, entry.index)?
                else {
                    bail!("a table relocation names something that is not a function");
                };
                let slot = *layout.table_slot.get(&f).ok_or_else(|| {
                    anyhow!("function {f} has its address taken but was given no table slot")
                })?;
                if entry.ty == RelocationType::TableIndexI32 {
                    write_u32(buf, offset, slot)?;
                } else {
                    write_i32_leb5(buf, offset, slot as i32)?;
                }
            }
            RelocationType::MemoryAddrLeb
            | RelocationType::MemoryAddrSleb
            | RelocationType::MemoryAddrI32
            | RelocationType::MemoryAddrRelSleb => {
                let Resolved::Data(base) = resolve_symbol(objects, defs, layout, oi, entry.index)?
                else {
                    bail!("a memory relocation names something that is not data");
                };
                // `__memory_base` is zero in a non-relocatable output, so the
                // relative form is the same arithmetic as the absolute one.
                let value = (base as i64 + addend as i64) as i32;
                match entry.ty {
                    RelocationType::MemoryAddrI32 => write_u32(buf, offset, value as u32)?,
                    RelocationType::MemoryAddrLeb => write_u32_leb5(buf, offset, value as u32)?,
                    _ => write_i32_leb5(buf, offset, value)?,
                }
            }
            RelocationType::GlobalIndexLeb | RelocationType::GlobalIndexI32 => {
                let resolved = resolve_symbol(objects, defs, layout, oi, entry.index)?;
                let g = match resolved {
                    Resolved::Global(g) => g,
                    // Naming data means the GOT entry that holds its address.
                    _ => {
                        let sym = objects[oi].symbols.get(entry.index as usize);
                        let name = sym
                            .map(|s| {
                                let (doi, def) = definition_of(objects, defs, oi, s);
                                objects[doi].symbol_name(def).to_string()
                            })
                            .unwrap_or_default();
                        *layout.got.get(&(Kind::Data, name.clone())).ok_or_else(|| {
                            anyhow!(
                                "a global relocation names {name:?}, which resolved to \
                                 {resolved:?} and has no global offset table entry"
                            )
                        })?
                    }
                };
                if entry.ty == RelocationType::GlobalIndexI32 {
                    write_u32(buf, offset, g)?;
                } else {
                    write_u32_leb5(buf, offset, g)?;
                }
            }
            RelocationType::TableNumberLeb => {
                let Resolved::Table(t) = resolve_symbol(objects, defs, layout, oi, entry.index)?
                else {
                    bail!("a table-number relocation names something that is not a table");
                };
                write_u32_leb5(buf, offset, t)?;
            }
            // Spelled `EventIndexLeb` here, `R_WASM_TAG_INDEX_LEB` in the
            // spec: the exceptions proposal renamed events to tags and the
            // relocation kept its number. There are four of these in a link
            // of sixty thousand, and they are the ones that decide whether a
            // `catch` matches.
            RelocationType::EventIndexLeb => {
                let Resolved::Tag(t) = resolve_symbol(objects, defs, layout, oi, entry.index)?
                else {
                    bail!("a tag relocation names something that is not a tag");
                };
                write_u32_leb5(buf, offset, t)?;
            }
            other => bail!("relocation {other:?} is not implemented"),
        }
    }
    Ok(())
}

/// Write a value into the five-byte slot an object reserved for it.
///
/// The width is fixed by the relocation type, not by the value: a five-byte
/// encoding of a small number is padded with continuation bits rather than
/// shortened, because shortening it would move every byte after it.
fn write_u32_leb5(buf: &mut [u8], offset: usize, value: u32) -> Result<()> {
    let slot = buf
        .get_mut(offset..offset + 5)
        .ok_or_else(|| anyhow!("relocation at {offset} is past the end of its section"))?;
    for (i, byte) in slot.iter_mut().enumerate() {
        let bits = ((value >> (7 * i)) & 0x7f) as u8;
        *byte = if i < 4 { bits | 0x80 } else { bits };
    }
    Ok(())
}

/// The signed form. The last byte carries the sign, so this uses an
/// arithmetic shift; a logical one would encode a negative value as a large
/// positive one, which is in range, validates, and is wrong.
fn write_i32_leb5(buf: &mut [u8], offset: usize, value: i32) -> Result<()> {
    let slot = buf
        .get_mut(offset..offset + 5)
        .ok_or_else(|| anyhow!("relocation at {offset} is past the end of its section"))?;
    for (i, byte) in slot.iter_mut().enumerate() {
        let bits = ((value >> (7 * i)) & 0x7f) as u8;
        *byte = if i < 4 { bits | 0x80 } else { bits };
    }
    Ok(())
}

fn write_u32(buf: &mut [u8], offset: usize, value: u32) -> Result<()> {
    let slot = buf
        .get_mut(offset..offset + 4)
        .ok_or_else(|| anyhow!("relocation at {offset} is past the end of its section"))?;
    slot.copy_from_slice(&value.to_le_bytes());
    Ok(())
}

fn emit(
    objects: &[Object],
    defs: &HashMap<(Kind, String), (usize, usize)>,
    layout: &Layout,
    opts: &LinkOptions,
) -> Result<Vec<u8>> {
    use wasm_encoder::{
        CodeSection, ConstExpr, DataSection, ElementSection, Elements, EntityType, ExportKind,
        ExportSection, FunctionSection, GlobalSection, GlobalType, ImportSection, MemorySection,
        MemoryType, Module, RefType, StartSection, TableSection, TableType, TagKind, TagSection,
        TagType, TypeSection, ValType,
    };

    let mut module = Module::new();

    // --- types ---
    let mut types = TypeSection::new();
    let mut seen: HashMap<u32, ()> = HashMap::new();
    let mut ordered: Vec<(u32, &wasmparser::FuncType)> = Vec::new();
    for (oi, obj) in objects.iter().enumerate() {
        for (li, ty) in obj.types.iter().enumerate() {
            let out = layout.type_map[oi][li];
            if seen.insert(out, ()).is_none() {
                ordered.push((out, ty));
            }
        }
    }
    ordered.sort_by_key(|(i, _)| *i);
    for (_, ty) in &ordered {
        types.ty().function(
            ty.params().iter().map(val_type).collect::<Vec<_>>(),
            ty.results().iter().map(val_type).collect::<Vec<_>>(),
        );
    }
    module.section(&types);

    // --- imports ---
    let mut imports = ImportSection::new();
    for (key, type_index) in &layout.imports {
        imports.import(&key.module, &key.name, EntityType::Function(*type_index));
    }
    module.section(&imports);

    // --- functions ---
    let mut functions = FunctionSection::new();
    let mut kept_functions: u32 = 0;
    for (oi, obj) in objects.iter().enumerate() {
        for (i, &local_type) in obj.functions.iter().enumerate() {
            if layout.func_out[oi][i].is_none() {
                continue;
            }
            functions.function(layout.type_map[oi][local_type as usize]);
            kept_functions += 1;
        }
    }
    // One more: the constructor runner this linker synthesises.
    let ctor_type = ordered
        .iter()
        .find(|(_, t)| t.params().is_empty() && t.results().is_empty())
        .map(|(i, _)| *i)
        .ok_or_else(|| anyhow!("no () -> () type to give __wasm_call_ctors"))?;
    functions.function(ctor_type);
    let ctors_index = layout.imports.len() as u32 + kept_functions;
    module.section(&functions);

    // --- table, memory, tag ---
    let slots = layout.table_slot.len() as u64 + 1;
    let mut tables = TableSection::new();
    tables.table(TableType {
        element_type: RefType::FUNCREF,
        minimum: slots,
        maximum: Some(slots),
        table64: false,
        shared: false,
    });
    module.section(&tables);

    let mut memories = MemorySection::new();
    memories.memory(MemoryType {
        minimum: layout.memory_pages as u64,
        maximum: None,
        memory64: false,
        shared: false,
        page_size_log2: None,
    });
    module.section(&memories);

    if layout.tag_index.is_some() {
        let mut tags = TagSection::new();
        for (oi, obj) in objects.iter().enumerate() {
            for &local_type in &obj.tags {
                tags.tag(TagType {
                    kind: TagKind::Exception,
                    func_type_idx: layout.type_map[oi][local_type as usize],
                });
            }
        }
        module.section(&tags);
    }

    // --- globals ---
    let mut globals = GlobalSection::new();
    globals.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: true,
            shared: false,
        },
        &ConstExpr::i32_const(opts.stack_size as i32),
    );
    globals.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: false,
            shared: false,
        },
        &ConstExpr::i32_const(0),
    );
    // `__table_base`: where this module's function pointers start in the
    // table, which is slot one because slot zero is left empty so a null
    // function pointer traps.
    globals.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: false,
            shared: false,
        },
        &ConstExpr::i32_const(TABLE_BASE as i32),
    );
    // Then the global offset table, in the order `plan` numbered it.
    for value in &layout.got_init {
        globals.global(
            GlobalType {
                val_type: ValType::I32,
                mutable: false,
                shared: false,
            },
            &ConstExpr::i32_const(*value),
        );
    }
    module.section(&globals);

    // --- exports ---
    let mut exports = ExportSection::new();
    exports.export("memory", ExportKind::Memory, 0);
    let mut exported_names: HashMap<&str, ()> = HashMap::new();
    {
        for (oi, obj) in objects.iter().enumerate() {
            for sym in &obj.symbols {
                let name = obj.symbol_name(sym);
                if !sym.defines() || name.is_empty() {
                    continue;
                }
                let SymbolTarget::Function { index } = sym.target else {
                    continue;
                };
                // The same test `mark_reachable` uses, so everything the host
                // is told about is something tree shaking was told to keep.
                let wanted = sym.is_exported()
                    || sym.is_no_strip()
                    || opts.roots.iter().any(|r| r == name)
                    || (opts.export_all_functions && !sym.is_local() && !sym.is_hidden());
                if !wanted {
                    continue;
                }
                // Only the object that owns the definition exports it.
                if defs
                    .get(&(Kind::Function, name.to_string()))
                    .map(|&(d, _)| d)
                    != Some(oi)
                {
                    continue;
                }
                if exported_names.insert(name, ()).is_some() {
                    continue;
                }
                let imported = obj.imported_functions();
                if index < imported {
                    continue;
                }
                let Some(out) = layout.func_out[oi][(index - imported) as usize] else {
                    continue;
                };
                exports.export(name, ExportKind::Func, out);
            }
        }
    }
    module.section(&exports);

    // --- start: run the constructors ---
    //
    // The engine calls this at instantiation, before any export can be
    // reached. LLD instead leaves `__wasm_call_ctors` to be called by the
    // entry point it generates, which a module linked with `--no-entry` does
    // not have -- and a constructor that never runs leaves whatever it was
    // going to initialise holding zeroes, which is not a crash but a wrong
    // answer somewhere later.
    module.section(&StartSection {
        function_index: ctors_index,
    });

    // --- element segment: the functions whose address is taken ---
    let mut slots_by_index: Vec<(u32, u32)> =
        layout.table_slot.iter().map(|(f, s)| (*s, *f)).collect();
    slots_by_index.sort_unstable();
    let functions_in_table: Vec<u32> = slots_by_index.iter().map(|(_, f)| *f).collect();
    let mut elements = ElementSection::new();
    if !functions_in_table.is_empty() {
        elements.active(
            Some(0),
            &ConstExpr::i32_const(TABLE_BASE as i32),
            Elements::Functions(functions_in_table.as_slice().into()),
        );
    }
    module.section(&elements);

    // --- code ---
    let mut code = CodeSection::new();
    for (oi, obj) in objects.iter().enumerate() {
        for (i, body) in obj.code_bodies.iter().enumerate() {
            if layout.func_out[oi][i].is_none() {
                continue;
            }
            code.raw(&obj.code_payload[body.clone()]);
        }
    }
    code.raw(&constructor_body(objects, defs, layout)?);
    module.section(&code);

    // --- data: every segment, merged, at the addresses just assigned ---
    let mut merged: Vec<u8> = Vec::new();
    let mut base: Option<u32> = None;
    for (oi, obj) in objects.iter().enumerate() {
        for (si, seg) in obj.data_segments.iter().enumerate() {
            let addr = layout.segment_addr[oi][si];
            let start = *base.get_or_insert(addr);
            let want = (addr - start) as usize;
            // The gap is the alignment padding the layout already accounted
            // for, so it is filled rather than skipped.
            if merged.len() < want {
                merged.resize(want, 0);
            }
            merged.extend_from_slice(&obj.data_payload[seg.range.clone()]);
        }
    }
    let mut data = DataSection::new();
    if let Some(start) = base {
        data.active(0, &ConstExpr::i32_const(start as i32), merged);
    }
    module.section(&data);

    // A `name` section, so a trap names the function it happened in.
    //
    // Without it every wasm backtrace is `<wasm function 7685>`: no
    // symbolication, and none of the recovery a native build gets from a
    // signal handler. The linker is the only thing that knows both the symbol
    // names and the indices they ended up at, so it is the only thing that can
    // write this.
    //
    // Last, and a custom section, so an engine that does not want it can skip
    // it and nothing before it moves.
    let mut names = wasm_encoder::NameMap::new();
    let mut named: Vec<(u32, &str)> = Vec::new();
    for (oi, obj) in objects.iter().enumerate() {
        let imported = obj.imported_functions();
        let mut by_fspace: HashMap<u32, &str> = HashMap::new();
        for sym in &obj.symbols {
            if let SymbolTarget::Function { index } = sym.target {
                if !sym.is_undefined() {
                    by_fspace.entry(index).or_insert(sym.name.as_str());
                }
            }
        }
        for li in 0..obj.functions.len() {
            let Some(out) = layout.func_out[oi][li] else {
                continue;
            };
            if let Some(name) = by_fspace.get(&(imported + li as u32)) {
                named.push((out, name));
            }
        }
    }
    named.sort_by_key(|(idx, _)| *idx);
    named.dedup_by_key(|(idx, _)| *idx);
    for (idx, name) in &named {
        names.append(*idx, name);
    }
    let mut name_section = wasm_encoder::NameSection::new();
    name_section.functions(&names);
    module.section(&name_section);

    Ok(module.finish())
}

/// The body of `__wasm_call_ctors`: every constructor, in priority order.
fn constructor_body(
    objects: &[Object],
    defs: &HashMap<(Kind, String), (usize, usize)>,
    layout: &Layout,
) -> Result<Vec<u8>> {
    use wasm_encoder::{Encode, Function, Instruction};

    let mut calls: Vec<(u32, u32)> = Vec::new();
    for (oi, obj) in objects.iter().enumerate() {
        for init in &obj.init_funcs {
            let Resolved::Function(f) = resolve_symbol(objects, defs, layout, oi, init.symbol)?
            else {
                bail!("a constructor names something that is not a function");
            };
            calls.push((init.priority, f));
        }
    }
    calls.sort_unstable();

    let mut function = Function::new([]);
    for (_, f) in calls {
        function.instruction(&Instruction::Call(f));
    }
    function.instruction(&Instruction::End);
    // `CodeSection::raw` adds the size prefix, so hand it the body alone.
    let mut bytes = Vec::new();
    function.encode(&mut bytes);
    // `Function::encode` writes a length-prefixed body; strip that prefix so
    // the section encoder can write its own.
    let mut reader = &bytes[..];
    let mut len: u32 = 0;
    let mut shift = 0;
    loop {
        let byte = reader[0];
        reader = &reader[1..];
        len |= ((byte & 0x7f) as u32) << shift;
        if byte & 0x80 == 0 {
            break;
        }
        shift += 7;
    }
    Ok(reader[..len as usize].to_vec())
}

fn val_type(v: &wasmparser::ValType) -> wasm_encoder::ValType {
    use wasm_encoder::ValType as E;
    match v {
        wasmparser::ValType::I32 => E::I32,
        wasmparser::ValType::I64 => E::I64,
        wasmparser::ValType::F32 => E::F32,
        wasmparser::ValType::F64 => E::F64,
        wasmparser::ValType::V128 => E::V128,
        wasmparser::ValType::Ref(_) => E::FUNCREF,
    }
}
