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

use std::collections::{HashMap, HashSet};

use anyhow::{anyhow, bail, Context, Result};
use wasmparser::{RelocationEntry, RelocationType};

use crate::object::{ImportKind, ObjImport, Object, SegmentInfo, SymbolTarget};

/// Bytes reserved for the shadow stack unless a caller says otherwise.
///
/// wasm-ld's default is 64 KB, which is fine for a program whose stack usage
/// is its own. It is not fine here: every module carries the ash runtime,
/// and that runtime recurses -- the regex compiler over a pattern's syntax
/// tree, XML parsing, the exception machinery. 64 KB overflowed on the Haxe
/// conformance suite, and because the stack sits at the bottom of memory and
/// grows down, the symptom was a fault just below zero (`0xffffff88`, -120)
/// inside whatever happened to be allocating -- which reads as a heap bug and
/// is not one.
///
/// 8 MB matches what a native thread gets by default. It costs address space
/// and nothing else: the region is untouched until the stack reaches it, and
/// a 32-bit address space has room to spare beside a heap measured in
/// hundreds of megabytes.
pub const DEFAULT_STACK_SIZE: u32 = 8 * 1024 * 1024;

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
    /// Names a separately-loaded native library will import from this module,
    /// which is what makes this module a host for one.
    ///
    /// A wasm HDLL is a `dylink.0` side module: it imports this module's
    /// memory and function table, so it works on the same heap the collector
    /// scans and a function pointer it returns is an index this module can
    /// call -- which is what lets `DEFINE_PRIM` signatures keep passing
    /// `vbyte*`, `varray*` and `vclosure*` across the boundary. It then
    /// imports each runtime function it calls by name, the way an HDLL links
    /// against libhl.
    ///
    /// The names are listed rather than assumed because exporting a function
    /// pins it: tree shaking cannot remove what the host is told about. A
    /// build knows which libraries it ships, so it can read their imports and
    /// export exactly those. Empty means this module hosts nothing and its
    /// ABI stays as narrow as it was.
    pub hdll_imports: Vec<String>,
    /// Data symbols a separately-loaded native library needs the address of.
    ///
    /// A position-independent library reaches a symbol it does not define
    /// through the global offset table, importing `GOT.mem.<name>` -- a
    /// global holding that symbol's address. `errno` is the one every C
    /// library wants. Each name here becomes an exported global holding where
    /// the linker put it.
    pub hdll_data: Vec<String>,
    /// Emit a module several threads can instantiate against one memory.
    ///
    /// A thread on wasm is another instance of the same module, and what
    /// makes them one program rather than two is that they share the memory.
    /// So the memory stops being the module's own and becomes `env.memory`,
    /// imported, shared, and with the maximum a shared memory is required to
    /// declare.
    ///
    /// The data then cannot be an active segment. An active segment is
    /// written at instantiation, and the second instance would write the
    /// program's initial data over whatever the first one had got to. So the
    /// image becomes one passive segment and a start function copies it in
    /// exactly once, behind a compare-and-swap on a flag word: the instance
    /// that wins initialises and then wakes the others, and the others wait
    /// on that word rather than racing it.
    ///
    /// Off by default, and the gate is that the emitted module is otherwise
    /// byte-identical: nothing here runs for a build that did not ask.
    pub shared_memory: bool,
    /// Instrument the module so a fiber can suspend inside it and be resumed.
    ///
    /// Off by default, and the gate is not that the code path is skipped but
    /// that the emitted module is byte-identical: with this false, `link`
    /// returns exactly what `emit` produced and nothing in
    /// [`crate::fiber`] runs. See `docs/wasm/fibers.md`.
    pub fibers: bool,
}

impl Default for LinkOptions {
    fn default() -> Self {
        // One page, which is what LLD reserves, and what the module this
        // linker replaces was verified running with.
        Self {
            stack_size: DEFAULT_STACK_SIZE,
            export_all_functions: false,
            tree_shake: true,
            roots: ["main", "ash_module_init", "_start", "_initialize"]
                .iter()
                .map(|s| s.to_string())
                .collect(),
            fibers: false,
            shared_memory: false,
            hdll_imports: Vec::new(),
            hdll_data: Vec::new(),
        }
    }
}

/// The first usable table slot. Zero is left empty so that calling a null
/// function pointer traps rather than calling whatever landed first, and
/// `__table_base` is this same number.
const TABLE_BASE: u32 = 1;

/// Does this segment hold thread-local data?
///
/// Two things say so and only one of them is dependable. The `linking`
/// section has a flag for it, and `lld -r` does not carry that flag through
/// its own output -- every segment in the prelinked runtime object comes back
/// with flags of zero, `.tdata` included. The name survives, and LLVM fixes
/// it: `.tdata` and `.tbss`, with a `.<symbol>` suffix when each datum gets
/// its own section. So either is enough, and `refuse_unsupported` checks the
/// answer against the symbols that point into the segment.
fn segment_is_tls(info: Option<&SegmentInfo>) -> bool {
    /// `WASM_SEG_FLAG_TLS`.
    const TLS: u32 = 0x2;
    let Some(info) = info else {
        return false;
    };
    if info.flags & TLS != 0 {
        return true;
    }
    [".tdata", ".tbss"]
        .iter()
        .any(|stem| info.name == *stem || info.name.starts_with(&format!("{stem}.")))
}

/// Where thread-local data went.
///
/// A thread-local is not at an address. It is at an offset from
/// `__tls_base`, a mutable global holding the base of the block belonging to
/// whichever thread is running -- so the same code reads a different variable
/// depending on who runs it, and a `MEMORY_ADDR_TLS_SLEB` relocation holds
/// that offset rather than an address.
///
/// Two copies of the data are placed. The template is the image a thread's
/// block starts as and nothing ever writes it; the main thread gets its own
/// block, so that a thread starting later still copies the initial values
/// rather than whatever main has since stored. LLD does the same with a
/// passive segment and `memory.init`; a second placed copy needs no passive
/// segment and no `data.drop` bookkeeping, and costs the size of the block.
#[derive(Debug, Clone)]
struct TlsLayout {
    /// Offset within the block of each object's TLS segments, and `None` for
    /// a segment that is not thread-local.
    offset: Vec<Vec<Option<u32>>>,
    /// `__tls_size`: how much one thread's block is.
    size: u32,
    /// `__tls_align`: what a block must be aligned to.
    align: u32,
    /// Address of the template every thread's block is copied from.
    template: u32,
    /// Address of the main thread's own block, which `__tls_base` starts at.
    main: u32,
    base_global: u32,
    size_global: u32,
    align_global: u32,
}

/// Does this link have thread-local storage in it at all?
///
/// A `.tdata` segment is the obvious sign, and the globals are the other one:
/// an object built for a threads target imports `__tls_base` and calls
/// `__wasm_init_tls` whether or not it ended up with any thread-locals of its
/// own, and something has to define those.
fn needs_tls(objects: &[Object]) -> bool {
    objects.iter().any(|obj| {
        obj.symbols.iter().any(|s| s.is_tls())
            || obj.imports.iter().any(|i| {
                matches!(
                    i.name.as_str(),
                    "__tls_base" | "__tls_size" | "__tls_align" | "__wasm_init_tls"
                )
            })
    })
}

/// The functions the linker writes rather than any object supplying them.
///
/// An object built for a threads target imports `__wasm_init_tls` because
/// only the linker knows where the thread-local template ended up. Nothing
/// else is in this position, so the list is one name long.
fn is_linker_function(name: &str) -> bool {
    name == "__wasm_init_tls"
}

/// A weak reference to a function nothing in this link defines.
///
/// Not an error, and not an import either. C writes `if (fn) fn(...)` against
/// a symbol that may or may not have been linked in: the address has to be
/// zero for that test to answer no, and the call the test guards still has to
/// be a call to something. wasi-libc's `__wasilibc_futex_wait_maybe_busy` is
/// the one this link meets. LLD emits a body that traps and resolves the
/// address to zero, and so does this -- a program that reaches the trap took
/// a branch it had just tested its way out of.
fn weak_undefined_function<'a>(
    defs: &HashMap<(Kind, String), (usize, usize)>,
    obj: &'a Object,
    sym: &'a crate::object::Symbol,
) -> Option<&'a str> {
    if !matches!(sym.target, SymbolTarget::Function { .. }) || !sym.is_undefined() || !sym.is_weak()
    {
        return None;
    }
    let name = obj.symbol_name(sym);
    if defs.contains_key(&(Kind::Function, name.to_string())) {
        return None;
    }
    Some(name)
}

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

impl Layout {
    /// Output index of `__wasm_call_ctors`, which is always the first
    /// function the linker writes itself.
    fn ctors(&self) -> u32 {
        self.synthetic_base
    }

    /// Output index of `__wasm_init_tls`, when there is anything to
    /// initialise.
    fn init_tls(&self) -> Option<u32> {
        self.tls.as_ref().map(|_| self.synthetic_base + 1)
    }

    /// Output index of `__wasm_init_memory`, when the memory is shared.
    fn init_memory(&self) -> Option<u32> {
        self.init_flag
            .map(|_| self.synthetic_base + 1 + u32::from(self.tls.is_some()))
    }

    /// Output index of the first trapping stub, which follows all of those.
    fn first_stub(&self) -> u32 {
        self.synthetic_base
            + 1
            + u32::from(self.tls.is_some())
            + u32::from(self.init_flag.is_some())
    }

    /// Output index of the trapping stub standing in for a weak reference.
    fn weak_stub(&self, name: &str) -> Option<u32> {
        let first = self.first_stub();
        self.weak_stubs
            .iter()
            .position(|(n, _)| n == name)
            .map(|i| first + i as u32)
    }
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
    /// Data symbols a native library asks for, and the global holding each
    /// one's address, to be exported under the symbol's own name.
    hdll_data_globals: Vec<(String, u32)>,
    heap_base: u32,
    memory_pages: u32,
    /// Where data starts, which is also the top of the shadow stack.
    data_base: u32,
    /// One past the last byte of placed data.
    data_end: u32,
    /// Thread-local storage, when the objects have any.
    tls: Option<TlsLayout>,
    /// A trapping body for each weak reference nothing defines, by name, with
    /// the output type it has to have.
    weak_stubs: Vec<(String, u32)>,
    /// Output index of the first function the linker writes itself.
    /// `__wasm_call_ctors` is there, then `__wasm_init_tls` if there is any
    /// thread-local storage, then `__wasm_init_memory` if the memory is
    /// shared, then the weak stubs in order.
    synthetic_base: u32,
    /// Address of the word `__wasm_init_memory` races on, when the memory is
    /// shared. Placed above the data image, because the initialisation it
    /// guards would otherwise write over the flag that says it happened.
    init_flag: Option<u32>,
}

/// Link `objects` into a module.
pub fn link(mut objects: Vec<Object>, opts: &LinkOptions) -> Result<Vec<u8>> {
    refuse_unsupported(&objects)?;

    let defs = resolve_definitions(&objects)?;
    check_hdll_imports(&defs, opts)?;
    let layout = plan(&objects, &defs, opts)?;
    report_unresolved(&objects, &defs, &layout)?;
    // Patching mutates each object's kept payloads in place.
    apply_relocations(&mut objects, &defs, &layout)?;
    let module = emit(&objects, &defs, &layout, opts)?;
    if !opts.fibers {
        return Ok(module);
    }
    // Only here, and never earlier. A relocation names an absolute byte
    // offset into a body and is written into a fixed-width slot; re-encoding
    // a body writes every immediate at its natural width, so a rewrite ahead
    // of `apply_relocations` would leave every later patch landing across an
    // opcode boundary in a module that still validates. By this point every
    // relocation has been spent and nothing reads an offset again.
    Ok(crate::fiber::instrument(&module)?.0)
}

/// Say no to what has not been implemented, rather than producing a module
/// that is quietly missing something.
fn refuse_unsupported(objects: &[Object]) -> Result<()> {
    for obj in objects {
        // Thread-local data is laid out, and the two things that say a
        // segment holds it have to agree. A symbol flagged TLS whose segment
        // is not, or a symbol in a TLS segment that is not flagged, means one
        // of the two is being read wrongly -- and the result would be an
        // address measured from the wrong base, which is a running program
        // reading someone else's variable rather than a crash.
        for sym in &obj.symbols {
            let SymbolTarget::Data { segment, .. } = sym.target else {
                continue;
            };
            let tls_segment = segment_is_tls(obj.segment_info.get(segment as usize));
            if sym.is_tls() != tls_segment {
                bail!(
                    "{}: symbol {} is {} but segment {segment} ({}) is {}",
                    obj.name,
                    obj.symbol_name(sym),
                    if sym.is_tls() { "thread-local" } else { "not thread-local" },
                    obj.segment_info
                        .get(segment as usize)
                        .map(|s| s.name.as_str())
                        .unwrap_or("unnamed"),
                    if tls_segment { "thread-local" } else { "not" },
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

/// Refuse a link whose libraries ask for something the runtime does not have.
///
/// A side module gets its libc, and everything else it did not bring, from the
/// program that hosts it -- that is the model, and it is why the program
/// exports what the libraries import. So a name the runtime never defined is
/// not a warning: the library would fail to instantiate at run time, with the
/// evidence a build away from where it could be acted on. Saying it here
/// costs nothing and points at the right thing.
fn check_hdll_imports(
    defs: &HashMap<(Kind, String), (usize, usize)>,
    opts: &LinkOptions,
) -> Result<()> {
    let missing: Vec<&String> = opts
        .hdll_imports
        .iter()
        .filter(|name| !defs.contains_key(&(Kind::Function, (*name).clone())))
        .collect();
    if missing.is_empty() {
        return Ok(());
    }
    let shown: Vec<&str> = missing.iter().take(8).map(|n| n.as_str()).collect();
    bail!(
        "a native library beside the output imports {} function(s) this runtime does not \
         define, the first few being {}. A wasm library takes libc and the runtime from the \
         program that hosts it, so it can only use what ash_std itself pulled in.",
        missing.len(),
        shown.join(", ")
    );
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
            let name = obj.symbol_name(sym);
            // Two undefined things this linker answers itself rather than
            // asking a host for: the function only it can write, and a weak
            // reference nothing defines. Importing either would put a name in
            // the module's ABI that no host has any business supplying.
            if is_linker_function(name) || weak_undefined_function(defs, obj, sym).is_some() {
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
    // Where the functions the linker writes itself begin: after every
    // function any object supplied.
    let synthetic_base = next;

    // --- linker-defined globals ---
    let stack_pointer_global = 0;
    let memory_base_global = 1;
    let table_base_global = 2;
    let tls_globals = 3;

    // --- data layout ---
    // The stack is first, so a stack overflow walks into address zero rather
    // than into the program's own data.
    let mut address = opts.stack_size;
    let alignment = |obj: &Object, i: usize| {
        1u32 << obj
            .segment_info
            .get(i)
            .map(|s| s.align_log2)
            .unwrap_or(0)
            .min(16)
    };
    let mut segment_addr: Vec<Vec<u32>> = objects
        .iter()
        .map(|o| vec![0; o.data_segments.len()])
        .collect();
    let mut tls_offset: Vec<Vec<Option<u32>>> = objects
        .iter()
        .map(|o| vec![None; o.data_segments.len()])
        .collect();
    for (oi, obj) in objects.iter().enumerate() {
        for (i, seg) in obj.data_segments.iter().enumerate() {
            if segment_is_tls(obj.segment_info.get(i)) {
                continue;
            }
            address = address.next_multiple_of(alignment(obj, i));
            segment_addr[oi][i] = address;
            address += (seg.range.end - seg.range.start) as u32;
        }
    }

    // Then the thread-local data, last so that its two copies are one
    // contiguous run and `__tls_size` is the distance between them.
    //
    // `.tdata` and `.tbss` are placed in whatever order the objects hold
    // them, because both are placed as bytes: a wasm object carries its
    // zero-initialised data as zeroes rather than as a size, so there is no
    // fill to keep at the end.
    let tls = needs_tls(objects).then(|| {
        let mut align = 1u32;
        for obj in objects {
            for i in 0..obj.data_segments.len() {
                if segment_is_tls(obj.segment_info.get(i)) {
                    align = align.max(alignment(obj, i));
                }
            }
        }
        address = address.next_multiple_of(align);
        let template = address;
        for (oi, obj) in objects.iter().enumerate() {
            for (i, seg) in obj.data_segments.iter().enumerate() {
                if !segment_is_tls(obj.segment_info.get(i)) {
                    continue;
                }
                address = address.next_multiple_of(alignment(obj, i));
                segment_addr[oi][i] = address;
                tls_offset[oi][i] = Some(address - template);
                address += (seg.range.end - seg.range.start) as u32;
            }
        }
        let size = address - template;
        address = address.next_multiple_of(align);
        let main = address;
        address += size;
        TlsLayout {
            offset: tls_offset,
            size,
            align,
            template,
            main,
            base_global: tls_globals,
            size_global: tls_globals + 1,
            align_global: tls_globals + 2,
        }
    });

    // The word `__wasm_init_memory` races on. Above the image and not in it,
    // because `memory.init` would otherwise write the flag back to zero after
    // the instance that won had set it, and a second instance would then
    // initialise the memory a second time.
    let init_flag = opts.shared_memory.then(|| {
        address = address.next_multiple_of(4);
        let at = address;
        address += 4;
        at
    });

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
            // The address of a weak reference nothing defines is zero, which
            // is what the `if (fn)` around the call tests for. A slot would
            // be an address, and an address tests as present.
            if weak_undefined_at(objects, defs, oi, entry.index).is_some() {
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
    // After __stack_pointer, __memory_base and __table_base, and after the
    // three thread-local globals when there are any.
    let first_got = if tls.is_some() { tls_globals + 3 } else { 3 };
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
            if def.is_tls() {
                bail!(
                    "{name} is thread-local and is reached through the global offset \
                     table, which holds one address per symbol and a thread-local has \
                     one per thread"
                );
            }
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
                SymbolTarget::Undefined | SymbolTarget::UndefinedData => {
                    match linker_address_early(&name, opts, address) {
                        Some(v) => v as i32,
                        None => continue,
                    }
                }
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

    // The same globals, for the addresses a native library asks the program
    // for. A library reaches them exactly as this module reaches its own --
    // through the global offset table -- so they are the same kind of entry,
    // and the only difference is that these are exported under the symbol's
    // own name for the loader to find.
    let mut hdll_data_globals: Vec<(String, u32)> = Vec::new();
    for name in &opts.hdll_data {
        let key = (Kind::Data, name.clone());
        if let Some(&existing) = got.get(&key) {
            hdll_data_globals.push((name.clone(), existing));
            continue;
        }
        let Some(&(doi, si)) = defs.get(&key) else {
            continue;
        };
        let SymbolTarget::Data {
            segment, offset, ..
        } = objects[doi].symbols[si].target
        else {
            continue;
        };
        let Some(base) = segment_addr
            .get(doi)
            .and_then(|s| s.get(segment as usize))
        else {
            continue;
        };
        let index = first_got + got_init.len() as u32;
        got.insert(key, index);
        got_init.push((base + offset) as i32);
        hdll_data_globals.push((name.clone(), index));
    }

    let tag_index = objects.iter().any(|o| !o.tags.is_empty()).then_some(0);

    // --- a trapping body for each weak reference nothing defines ---
    //
    // The type comes from the import entry the object left behind, because a
    // stub has to have the shape the call site already encoded.
    let mut weak_stubs: Vec<(String, u32)> = Vec::new();
    let mut stub_named: HashSet<String> = HashSet::new();
    for (oi, obj) in objects.iter().enumerate() {
        let func_imports = function_imports(obj);
        for sym in &obj.symbols {
            let Some(name) = weak_undefined_function(defs, obj, sym) else {
                continue;
            };
            if !stub_named.insert(name.to_string()) {
                continue;
            }
            let Some(local) = undefined_function_index(sym) else {
                continue;
            };
            let import = func_imports.get(local as usize).ok_or_else(|| {
                anyhow!("{}: weak symbol {name} has no import entry", obj.name)
            })?;
            let ImportKind::Function { type_index } = import.kind else {
                bail!("{}: weak symbol {name} does not name a function", obj.name);
            };
            weak_stubs.push((name.to_string(), type_map[oi][type_index as usize]));
        }
    }
    Ok(Layout {
        data_base: opts.stack_size,
        data_end: address,
        tls,
        weak_stubs,
        synthetic_base,
        init_flag,
        func_out,
        type_map,
        segment_addr,
        table_slot,
        imports,
        import_index,
        tag_index,
        stack_pointer_global,
        memory_base_global,
        table_base_global,
        got,
        got_init,
        hdll_data_globals,
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
                || opts.hdll_imports.iter().any(|r| r == name)
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
        SymbolTarget::UndefinedData => Kind::Data,
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
    // Two of them are not imports: a function the linker writes itself, and a
    // weak reference nothing defines. Neither belongs in the table -- the
    // element segment lists what had its address taken, and both of these
    // have their addresses answered elsewhere.
    if is_linker_function(&import.name)
        || obj
            .symbols
            .iter()
            .any(|s| weak_undefined_function(defs, obj, s) == Some(import.name.as_str()))
    {
        return Ok(None);
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
        // Only symbols a KEPT section refers to. Debug sections are dropped,
        // and their relocations with them, but their symbols stay in the
        // table: wasi-sdk's libc carries DWARF whose location expressions
        // name `__tls_base`, a global nothing else in the object touches, and
        // a link against it refused over a symbol that would never have been
        // patched.
        let referenced: HashSet<u32> = obj
            .code_relocs
            .iter()
            .chain(obj.data_relocs.iter())
            .filter(|entry| entry.ty != RelocationType::TypeIndexLeb)
            .map(|entry| entry.index)
            .collect();
        for (si, sym) in obj.symbols.iter().enumerate() {
            if !sym.is_undefined() || !referenced.contains(&(si as u32)) {
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
                && linker_global(name, layout).is_some()
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

/// The globals the linker defines, because no object may define one.
///
/// The three thread-local ones exist only when this link laid out
/// thread-local storage; an object referencing them without it has been built
/// for a threads target and linked against a runtime that was not.
fn linker_global(name: &str, layout: &Layout) -> Option<u32> {
    let tls = layout.tls.as_ref();
    Some(match name {
        "__stack_pointer" => layout.stack_pointer_global,
        "__memory_base" => layout.memory_base_global,
        "__table_base" => layout.table_base_global,
        "__tls_base" => tls?.base_global,
        "__tls_size" => tls?.size_global,
        "__tls_align" => tls?.align_global,
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
            let name = objects[doi].symbol_name(def);
            // The two kinds of undefined function this linker answers itself,
            // before the import table is consulted: the one only it can write,
            // and a weak reference nothing defines.
            if is_linker_function(name) {
                return layout.init_tls().map(Resolved::Function).ok_or_else(|| {
                    anyhow!("{name} is called but this link has no thread-local storage")
                });
            }
            if weak_undefined_function(defs, &objects[doi], def).is_some() {
                return layout
                    .weak_stub(name)
                    .map(Resolved::Function)
                    .ok_or_else(|| anyhow!("no stub was planned for weak symbol {name}"));
            }
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
            let name = objects[doi].symbol_name(def);
            let index = linker_global(name, layout)
                .ok_or_else(|| anyhow!("no definition for global {name:?}"))?;
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
        SymbolTarget::Undefined | SymbolTarget::UndefinedData => {
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

/// The offset within a thread's block of the symbol a TLS relocation names.
fn tls_offset_of(
    objects: &[Object],
    defs: &HashMap<(Kind, String), (usize, usize)>,
    layout: &Layout,
    oi: usize,
    sym_index: u32,
) -> Result<u32> {
    let sym = objects[oi]
        .symbols
        .get(sym_index as usize)
        .ok_or_else(|| anyhow!("{}: symbol {sym_index} is out of range", objects[oi].name))?;
    let (doi, def) = definition_of(objects, defs, oi, sym);
    let name = objects[doi].symbol_name(def);
    let tls = layout
        .tls
        .as_ref()
        .ok_or_else(|| anyhow!("{name} is thread-local but this link laid out no TLS"))?;
    let SymbolTarget::Data {
        segment, offset, ..
    } = def.target
    else {
        bail!("a thread-local relocation names {name}, which is not data");
    };
    tls.offset
        .get(doi)
        .and_then(|s| s.get(segment as usize))
        .copied()
        .flatten()
        .map(|base| base + offset)
        .ok_or_else(|| anyhow!("{name} is in segment {segment}, which is not thread-local"))
}

/// The name of the weak reference a relocation names, if nothing defines it.
fn weak_undefined_at<'a>(
    objects: &'a [Object],
    defs: &HashMap<(Kind, String), (usize, usize)>,
    oi: usize,
    sym_index: u32,
) -> Option<&'a str> {
    let sym = objects[oi].symbols.get(sym_index as usize)?;
    let (doi, def) = definition_of(objects, defs, oi, sym);
    weak_undefined_function(defs, &objects[doi], def)
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
                // The address of a weak reference nothing defines is zero, so
                // that the `if (fn)` guarding the call answers no. Asked for
                // before the symbol is resolved, because resolving it gives
                // the trapping stub, and a stub in the table is an address
                // that tests as present.
                if weak_undefined_at(objects, defs, oi, entry.index).is_some() {
                    if entry.ty == RelocationType::TableIndexI32 {
                        write_u32(buf, offset, 0)?;
                    } else {
                        write_i32_leb5(buf, offset, 0)?;
                    }
                    continue;
                }
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
            // A thread-local is not at an address: it is at an offset from
            // `__tls_base`, and the code that reads it adds the two. So this
            // writes the offset within the block, and the same symbol read
            // from two threads reaches two different variables.
            RelocationType::MemoryAddrTlsSleb => {
                let offset_in_block = tls_offset_of(objects, defs, layout, oi, entry.index)?;
                write_i32_leb5(buf, offset, (offset_in_block as i64 + addend as i64) as i32)?;
            }
            RelocationType::MemoryAddrLeb
            | RelocationType::MemoryAddrSleb
            | RelocationType::MemoryAddrI32
            | RelocationType::MemoryAddrRelSleb => {
                // An absolute address for a thread-local would be one
                // thread's copy, silently shared by every thread that ran the
                // code. There are none in what this linker links, and one
                // arriving is a compiler emitting a form this does not
                // implement rather than something to guess at.
                if let Some(sym) = objects[oi].symbols.get(entry.index as usize) {
                    let (doi, def) = definition_of(objects, defs, oi, sym);
                    if def.is_tls() {
                        bail!(
                            "{:?} wants the address of {}, which is thread-local",
                            entry.ty,
                            objects[doi].symbol_name(def)
                        );
                    }
                }
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
    // A shared memory is imported rather than defined, because a module that
    // defines one gets a fresh one per instantiation -- and every thread is
    // another instantiation of this same module. The host makes one and hands
    // it to all of them.
    if opts.shared_memory {
        imports.import("env", "memory", shared_memory_type(layout));
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
    // Then the functions this linker writes itself, in the order `plan`
    // numbered them: the constructor runner, `__wasm_init_tls` when there is
    // thread-local storage, and a stub for each weak reference nothing
    // defines.
    let ctor_type = ordered
        .iter()
        .find(|(_, t)| t.params().is_empty() && t.results().is_empty())
        .map(|(i, _)| *i)
        .ok_or_else(|| anyhow!("no () -> () type to give __wasm_call_ctors"))?;
    functions.function(ctor_type);
    let ctors_index = layout.imports.len() as u32 + kept_functions;
    if ctors_index != layout.ctors() {
        bail!(
            "the layout put the linker's own functions at {} and the emitter at \
             {ctors_index}",
            layout.synthetic_base
        );
    }
    if layout.tls.is_some() {
        let ty = ordered
            .iter()
            .find(|(_, t)| t.params() == [wasmparser::ValType::I32] && t.results().is_empty())
            .map(|(i, _)| *i)
            .ok_or_else(|| anyhow!("no (i32) -> () type to give __wasm_init_tls"))?;
        functions.function(ty);
    }
    if layout.init_flag.is_some() {
        functions.function(ctor_type);
    }
    for (at, (name, ty)) in (layout.first_stub()..).zip(&layout.weak_stubs) {
        functions.function(*ty);
        if layout.weak_stub(name) != Some(at) {
            bail!(
                "the layout put the stub for {name} at {:?} and the emitter at {at}",
                layout.weak_stub(name)
            );
        }
    }
    module.section(&functions);

    // --- table, memory, tag ---
    let slots = layout.table_slot.len() as u64 + 1;
    let mut tables = TableSection::new();
    tables.table(TableType {
        element_type: RefType::FUNCREF,
        minimum: slots,
        // A module hosting native libraries cannot fix its table: a side
        // module's functions are appended to it at load, so the loader has to
        // be able to grow it.
        maximum: if opts.hdll_imports.is_empty() {
            Some(slots)
        } else {
            None
        },
        table64: false,
        shared: false,
    });
    module.section(&tables);

    if !opts.shared_memory {
        let mut memories = MemorySection::new();
        memories.memory(MemoryType {
            minimum: layout.memory_pages as u64,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        module.section(&memories);
    }

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
    // The three a threads build needs. `__tls_base` starts at the main
    // thread's own block, which is placed data like anything else, so the
    // main thread has its thread-locals before a line runs and without a
    // start function to run them. It is mutable because `__wasm_init_tls`
    // moves it as each further thread arrives.
    if let Some(tls) = &layout.tls {
        // Zero when every thread instantiates this module for itself:
        // `__wasm_init_memory` gives the first instance the main block, and
        // `wasi_thread_start` gives each later one its own before it touches
        // a thread-local. Starting them all at the main block instead would
        // have a thread that touched one too early read main's copy and get
        // an answer, rather than trap.
        let start_at = if opts.shared_memory { 0 } else { tls.main as i32 };
        globals.global(
            GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            &ConstExpr::i32_const(start_at),
        );
        for value in [tls.size, tls.align] {
            globals.global(
                GlobalType {
                    val_type: ValType::I32,
                    mutable: false,
                    shared: false,
                },
                &ConstExpr::i32_const(value as i32),
            );
        }
    }
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
    if !opts.hdll_imports.is_empty() {
        // What a `dylink.0` side module imports before anything of its own:
        // the table its functions are appended to, and the two globals saying
        // where the loader placed its data and its table entries. The stack
        // pointer goes with them because a side module's own frames use it.
        exports.export("__indirect_function_table", ExportKind::Table, 0);
        exports.export("__memory_base", ExportKind::Global, layout.memory_base_global);
        exports.export("__table_base", ExportKind::Global, layout.table_base_global);
        for (name, index) in &layout.hdll_data_globals {
            exports.export(name, ExportKind::Global, *index);
        }
    }
    if opts.fibers || !opts.hdll_imports.is_empty() {
        // Two fibers cannot share one shadow stack. The transform leaves a
        // suspended frame's shadow allocation in place, which is right for
        // one coroutine; with a second, the frames that were between the
        // suspend and the scheduler return, restore this pointer above the
        // suspended frames, and the next allocation writes over them. So a
        // fiber runs on a region of its own and the runtime swaps this on the
        // way in and out -- which it can only do if it can see it.
        //
        // Gated, because exporting it hands every host arbitrary access to
        // the guest's stack pointer, and a build without fibers should not
        // widen its ABI to pay for a feature it did not ask for.
        exports.export(
            "__stack_pointer",
            ExportKind::Global,
            layout.stack_pointer_global,
        );
    }
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
                    || opts.hdll_imports.iter().any(|r| r == name)
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
        function_index: layout.init_memory().unwrap_or(ctors_index),
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

    // --- how many data segments the code will name ---
    //
    // `memory.init` and `data.drop` name a segment by index, and the module
    // has to say how many there are before the code that names them, so that
    // a validator can check the index without having read the data section
    // yet. Only a shared-memory build has either instruction.
    if opts.shared_memory {
        module.section(&wasm_encoder::DataCountSection { count: 1 });
    }

    // --- the data image ---
    //
    // Built before the code section rather than beside the data section,
    // because `__wasm_init_memory` copies this image and has to be told
    // where it goes and how much of it there is.
    //
    // Placed by address rather than in the order the objects hold them,
    // because the thread-local segments were moved to the end of memory and
    // appear in the middle of an object's list. The gaps are the alignment
    // padding the layout already accounted for.
    let mut placed: Vec<(u32, &[u8])> = Vec::new();
    for (oi, obj) in objects.iter().enumerate() {
        for (si, seg) in obj.data_segments.iter().enumerate() {
            let bytes = &obj.data_payload[seg.range.clone()];
            placed.push((layout.segment_addr[oi][si], bytes));
            // A thread-local segment is placed twice: once as the template a
            // new thread copies from, and once as the main thread's own copy,
            // which it is then free to write.
            if let Some((tls, offset)) = layout
                .tls
                .as_ref()
                .and_then(|t| t.offset[oi][si].map(|o| (t, o)))
            {
                placed.push((tls.main + offset, bytes));
            }
        }
    }
    let image = placed.iter().map(|(a, _)| *a).min().map(|start| {
        let end = placed
            .iter()
            .map(|(a, b)| *a + b.len() as u32)
            .max()
            .unwrap_or(start);
        let mut merged = vec![0u8; (end - start) as usize];
        for (addr, bytes) in &placed {
            let at = (*addr - start) as usize;
            merged[at..at + bytes.len()].copy_from_slice(bytes);
        }
        (start, merged)
    });

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
    if let Some(tls) = &layout.tls {
        code.raw(&init_tls_body(tls));
    }
    if layout.init_flag.is_some() {
        let sizes = image.as_ref().map(|(start, bytes)| (*start, bytes.len()));
        code.raw(&init_memory_body(layout, sizes));
    }
    for _ in &layout.weak_stubs {
        code.raw(&trap_body());
    }
    module.section(&code);

    // --- data ---
    //
    // Active for a memory this module owns, because then instantiation is the
    // one time it happens. Passive for a shared one, where it happens once
    // across every instance and `__wasm_init_memory` decides which.
    let mut data = DataSection::new();
    if let Some((start, merged)) = image {
        if opts.shared_memory {
            data.passive(merged);
        } else {
            data.active(0, &ConstExpr::i32_const(start as i32), merged);
        }
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
        for (li, out) in layout.func_out[oi].iter().enumerate() {
            let Some(out) = *out else {
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

/// How much a shared memory may grow to.
///
/// A shared memory must declare a maximum -- an engine cannot move it when it
/// grows, because other threads hold the old base, so it reserves the whole
/// range up front. One gigabyte is what the Rust `wasm32-wasip1-threads`
/// target passes to its own linker, so a module built by this one is asking
/// for the same thing as a module built beside it.
const SHARED_MEMORY_MAX_PAGES: u64 = 16384;

fn shared_memory_type(layout: &Layout) -> wasm_encoder::EntityType {
    wasm_encoder::EntityType::Memory(wasm_encoder::MemoryType {
        minimum: layout.memory_pages as u64,
        maximum: Some(SHARED_MEMORY_MAX_PAGES),
        memory64: false,
        shared: true,
        page_size_log2: None,
    })
}

/// The body of `__wasm_init_memory`: put the data in memory exactly once.
///
/// Every thread instantiates this module, so every thread runs this. Only one
/// of them may write the data image: the others are already running on it,
/// and copying it again would put the program's initial values back over
/// whatever they had reached. Nor may the others simply skip it, because the
/// first one has not necessarily finished -- a thread that ran ahead would
/// read half-initialised memory.
///
/// So the instances race on one word, and the three outcomes of a
/// compare-and-swap decide it: zero means this instance won and initialises,
/// one means another is doing it and this one waits, two means it is done.
/// The winner stores two and wakes everyone waiting. This is the shape LLD
/// emits, and the flag lives above the image so that the copy does not undo
/// the claim.
///
/// The constructors are called on the winning path only, for the same reason:
/// a constructor runs once per program, not once per thread.
fn init_memory_body(layout: &Layout, image: Option<(u32, usize)>) -> Vec<u8> {
    use wasm_encoder::{BlockType, Function, Instruction, MemArg};

    let flag = layout.init_flag.unwrap_or(0);
    let word = MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };
    let mut f = Function::new([]);
    // block $done { block $wait { block $init { ... br_table } ... } ... }
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::I32Const(flag as i32));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32AtomicRmwCmpxchg(word));
    f.instruction(&Instruction::BrTable(vec![0, 1].into(), 2));
    f.instruction(&Instruction::End);

    // Won: copy the image in, hand the main thread its thread-locals, run the
    // constructors, then say so and wake the others.
    if let Some((start, len)) = image {
        f.instruction(&Instruction::I32Const(start as i32));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(len as i32));
        f.instruction(&Instruction::MemoryInit {
            mem: 0,
            data_index: 0,
        });
    }
    if let Some(tls) = &layout.tls {
        f.instruction(&Instruction::I32Const(tls.main as i32));
        f.instruction(&Instruction::GlobalSet(tls.base_global));
    }
    f.instruction(&Instruction::Call(layout.ctors()));
    f.instruction(&Instruction::I32Const(flag as i32));
    f.instruction(&Instruction::I32Const(2));
    f.instruction(&Instruction::I32AtomicStore(word));
    f.instruction(&Instruction::I32Const(flag as i32));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::MemoryAtomicNotify(word));
    f.instruction(&Instruction::Drop);
    f.instruction(&Instruction::Br(1));
    f.instruction(&Instruction::End);

    // Lost: wait until the winner stores two. The wait returns immediately if
    // the word is no longer one, which is the case where it finished between
    // the swap and here.
    f.instruction(&Instruction::I32Const(flag as i32));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I64Const(-1));
    f.instruction(&Instruction::MemoryAtomicWait32(word));
    f.instruction(&Instruction::Drop);
    f.instruction(&Instruction::End);

    // Either way this instance is done with the segment, and dropping it
    // lets the engine release its copy.
    if image.is_some() {
        f.instruction(&Instruction::DataDrop(0));
    }
    f.instruction(&Instruction::End);
    strip_size_prefix(f)
}

/// The body of `__wasm_init_tls`: give this thread the block it was handed.
///
/// A thread arrives with a block of its own and no thread-locals in it. This
/// points `__tls_base` at the block and copies the template over it, so the
/// thread starts from the initial values rather than from whatever the thread
/// that allocated the block left there. Every read of a thread-local
/// afterwards is `__tls_base` plus the offset a `MEMORY_ADDR_TLS_SLEB`
/// relocation wrote, so this one store is what makes the same code reach a
/// different variable per thread.
fn init_tls_body(tls: &TlsLayout) -> Vec<u8> {
    use wasm_encoder::{Function, Instruction};

    let mut function = Function::new([]);
    function.instruction(&Instruction::LocalGet(0));
    function.instruction(&Instruction::GlobalSet(tls.base_global));
    if tls.size > 0 {
        function.instruction(&Instruction::LocalGet(0));
        function.instruction(&Instruction::I32Const(tls.template as i32));
        function.instruction(&Instruction::I32Const(tls.size as i32));
        function.instruction(&Instruction::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
    }
    function.instruction(&Instruction::End);
    strip_size_prefix(function)
}

/// The body of a stub standing in for a weak reference nothing defines: a
/// trap. `unreachable` satisfies any result type, so one body serves every
/// shape.
fn trap_body() -> Vec<u8> {
    use wasm_encoder::{Function, Instruction};

    let mut function = Function::new([]);
    function.instruction(&Instruction::Unreachable);
    function.instruction(&Instruction::End);
    strip_size_prefix(function)
}

/// `Function::encode` writes a length-prefixed body and `CodeSection::raw`
/// adds a length of its own, so the prefix comes back off.
fn strip_size_prefix(function: wasm_encoder::Function) -> Vec<u8> {
    use wasm_encoder::Encode;

    let mut bytes = Vec::new();
    function.encode(&mut bytes);
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
    reader[..len as usize].to_vec()
}

/// The body of `__wasm_call_ctors`: every constructor, in priority order.
fn constructor_body(
    objects: &[Object],
    defs: &HashMap<(Kind, String), (usize, usize)>,
    layout: &Layout,
) -> Result<Vec<u8>> {
    use wasm_encoder::{Function, Instruction};

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
    Ok(strip_size_prefix(function))
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
