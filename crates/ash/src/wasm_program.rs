//! A whole program as one wasm object, with no LLVM in it: the function
//! bodies `ash_wasm_emit` writes, and beside them everything the program
//! needs to start and to be reflected on -- the type descriptors, the constant
//! pool, the tables the runtime reads, the start-up routine, `main` and the
//! call trampolines.
//!
//! The layouts here are the ones the runtime reads on wasm32, which
//! `JITModule::check_hl_layouts` checks against the runtime's own at build
//! time for the LLVM path: a pointer and an `int` are four bytes, and every
//! structure is laid out in declaration order with natural alignment.
use std::collections::{BTreeMap, BTreeSet, HashMap};

use anyhow::{Result, anyhow};
use ash_wasm_emit::object::{Body, DataRef, Linkage, ObjectBuilder, Sym};
use ash_wasm_emit::wasm_encoder::{BlockType, Instruction as I, MemArg, ValType};

use crate::bytecode::{DecodedBytecode, enum_construct_layout};
use crate::hl_bindings as hl;
use crate::wasm_direct::{ProgramData, emit_into};

mod links;
use links::LinkNative;

fn mem(offset: u64, align: u32) -> MemArg {
    MemArg {
        offset,
        align,
        memory_index: 0,
    }
}

/// `hl_runtime_obj`: where the field count and the table of field offsets sit.
const RT_NFIELDS: u64 = 4;
const RT_FIELD_INDEXES: u64 = 32;
/// Where a `vdynamic` keeps its value.
const VDYNAMIC_VALUE: u64 = 8;

/// The bytes of one data symbol, and the references among them.
#[derive(Default)]
struct Blob {
    bytes: Vec<u8>,
    refs: Vec<(u32, DataRef)>,
}

impl Blob {
    fn u8(&mut self, v: u8) {
        self.bytes.push(v);
    }
    fn u32(&mut self, v: u32) {
        self.bytes.extend(v.to_le_bytes());
    }
    fn i32(&mut self, v: i32) {
        self.bytes.extend(v.to_le_bytes());
    }
    fn u64(&mut self, v: u64) {
        self.bytes.extend(v.to_le_bytes());
    }
    fn pad(&mut self, align: usize) {
        while self.bytes.len() % align != 0 {
            self.bytes.push(0);
        }
    }
    /// A pointer to `to` plus `add`, or null.
    fn ptr(&mut self, to: Option<Sym>, add: i32) {
        if let Some(to) = to {
            self.refs.push((self.bytes.len() as u32, DataRef::Address(to, add)));
        }
        self.u32(0);
    }
    fn function(&mut self, to: Sym) {
        self.refs.push((self.bytes.len() as u32, DataRef::Function(to)));
        self.u32(0);
    }
}

/// A function body being written.
struct F {
    body: Body,
}

impl F {
    fn new(locals: &[(u32, ValType)]) -> F {
        F {
            body: Body::new(locals.to_vec()),
        }
    }
    fn i(&mut self, i: I) -> &mut F {
        self.body.instruction(&i);
        self
    }
    fn c(&mut self, v: i32) -> &mut F {
        self.i(I::I32Const(v))
    }
    fn get(&mut self, local: u32) -> &mut F {
        self.i(I::LocalGet(local))
    }
    fn set(&mut self, local: u32) -> &mut F {
        self.i(I::LocalSet(local))
    }
    fn call(&mut self, sym: Sym) -> &mut F {
        self.body.call(sym);
        self
    }
    fn addr(&mut self, sym: Sym, add: i32) -> &mut F {
        self.body.address_of(sym, add);
        self
    }
    fn finish(mut self) -> Body {
        self.body.instruction(&I::End);
        self.body
    }
}

const I32: ValType = ValType::I32;

/// The wasm type a kind of value has, if it has one.
fn wasm_of(t: ash_wasm_emit::Ty) -> Option<ValType> {
    use ash_wasm_emit::Ty;
    match t {
        Ty::Bool | Ty::I32 | Ty::U8 | Ty::U16 | Ty::Ptr => Some(ValType::I32),
        Ty::I64 => Some(ValType::I64),
        Ty::F32 => Some(ValType::F32),
        Ty::F64 => Some(ValType::F64),
        Ty::V128 => Some(ValType::V128),
        Ty::Void | Ty::Other => None,
    }
}

struct Image<'a> {
    obj: &'a mut ObjectBuilder,
    bc: &'a DecodedBytecode,
    /// Type descriptors, by type index.
    types: Vec<Sym>,
    /// Text in UTF-16, by content.
    names: HashMap<String, Sym>,
    /// C strings, by content.
    cstrs: HashMap<String, Sym>,
    strings: Vec<Sym>,
    nfun: usize,
    nglobals: usize,
    globals: Sym,
    functions: Sym,
    function_types: Sym,
    module_ctx: Sym,
    /// Natives the program takes as values -- binds to a method, closes over --
    /// and so needs an address for in the function table.
    value_natives: Vec<ValueNative>,
    /// Natives the host links, each defined as a function of the object.
    links: Vec<LinkNative>,
}

/// A native with an entry in the function table.
struct ValueNative {
    findex: usize,
    params: Vec<ValType>,
    results: Vec<ValType>,
    how: NativeKind,
}

enum NativeKind {
    /// Exported by the runtime under this name.
    Runtime(String),
    /// In a library the host opens, reached through this slot.
    Slot { slot: String, lib: String, prim: String },
}

impl<'a> Image<'a> {
    fn data(&mut self, name: &str, blob: Blob, align_log2: u32, linkage: Linkage) -> Sym {
        // A symbol cannot be empty, and a table with no entries is still named.
        let mut blob = blob;
        if blob.bytes.is_empty() {
            blob.bytes.extend([0u8; 4]);
        }
        let sym = self
            .obj
            .declare_data(name, blob.bytes.len() as u32, align_log2, linkage);
        self.obj.set_data(sym, &blob.bytes, &blob.refs);
        sym
    }

    /// NUL-terminated UTF-16, the form every name in a type is in.
    fn utf16(&mut self, text: &str) -> Sym {
        if let Some(&s) = self.names.get(text) {
            return s;
        }
        let mut blob = Blob::default();
        for unit in text.encode_utf16().chain(std::iter::once(0)) {
            blob.bytes.extend(unit.to_le_bytes());
        }
        let name = format!("ash_name.{}", self.names.len());
        let sym = self.data(&name, blob, 1, Linkage::Local);
        self.names.insert(text.to_string(), sym);
        sym
    }

    /// NUL-terminated bytes.
    fn cstr(&mut self, text: &str) -> Sym {
        if let Some(&s) = self.cstrs.get(text) {
            return s;
        }
        let mut blob = Blob::default();
        blob.bytes.extend(text.as_bytes());
        blob.bytes.push(0);
        let name = format!("ash_cstr.{}", self.cstrs.len());
        let sym = self.data(&name, blob, 0, Linkage::Local);
        self.cstrs.insert(text.to_string(), sym);
        sym
    }

    fn type_ptr(&self, t: Option<usize>) -> Option<Sym> {
        t.and_then(|t| self.types.get(t).copied())
    }

    fn global_slot(&self, global_value: u32) -> (Option<Sym>, i32) {
        // 1-based; zero means the type has no global.
        match (global_value as usize).checked_sub(1) {
            Some(i) if i < self.nglobals => (Some(self.globals), 4 * i as i32),
            _ => (None, 0),
        }
    }

    fn fields(&mut self, name: &str, fields: &[crate::types::HLObjField]) -> Option<Sym> {
        if fields.is_empty() {
            return None;
        }
        let mut blob = Blob::default();
        for f in fields {
            let n = self.utf16(&f.name);
            blob.ptr(Some(n), 0);
            blob.ptr(self.type_ptr(Some(f.type_.0)), 0);
            blob.i32(f.hashed_name);
        }
        Some(self.data(name, blob, 2, Linkage::Local))
    }

    fn i32_array(&mut self, name: &str, values: &[i32]) -> Option<Sym> {
        if values.is_empty() {
            return None;
        }
        let mut blob = Blob::default();
        for &v in values {
            blob.i32(v);
        }
        Some(self.data(name, blob, 2, Linkage::Local))
    }

    fn type_array(&mut self, name: &str, types: &[crate::types::TypeRef]) -> Option<Sym> {
        if types.is_empty() {
            return None;
        }
        let mut blob = Blob::default();
        for t in types {
            blob.ptr(self.type_ptr(Some(t.0)), 0);
        }
        Some(self.data(name, blob, 2, Linkage::Local))
    }

    /// Every type descriptor, with what hangs off it.
    fn type_table(&mut self) {
        let bc = self.bc;
        for i in 0..bc.types.len() {
            let t = &bc.types[i];
            let mut blob = Blob::default();
            blob.u32(t.kind as u32);
            let arm: Option<Sym> = match t.kind {
                k if (k == hl::hl_type_kind_HOBJ || k == hl::hl_type_kind_HSTRUCT) && t.obj.is_some() => {
                    let o = t.obj.as_ref().expect("checked");
                    let name = self.utf16(&o.name);
                    let fields = self.fields(&format!("ash_fields.{i}"), &o.fields);
                    let proto = if o.proto.is_empty() {
                        None
                    } else {
                        let mut p = Blob::default();
                        for pr in &o.proto {
                            let n = self.utf16(&pr.name);
                            p.ptr(Some(n), 0);
                            p.i32(pr.findex);
                            p.i32(pr.pindex);
                            p.i32(pr.hashed_name);
                        }
                        Some(self.data(&format!("ash_protos.{i}"), p, 2, Linkage::Local))
                    };
                    let bindings = self.i32_array(&format!("ash_bindings.{i}"), &o.bindings);
                    let (gv, gv_add) = self.global_slot(o.global_value);
                    let mut b = Blob::default();
                    b.i32(o.fields.len() as i32);
                    b.i32(o.proto.len() as i32);
                    b.i32((o.bindings.len() / 2) as i32);
                    b.ptr(Some(name), 0);
                    b.ptr(self.type_ptr(o.super_.as_ref().map(|s| s.0)), 0);
                    b.ptr(fields, 0);
                    b.ptr(proto, 0);
                    b.ptr(bindings, 0);
                    b.ptr(gv, gv_add);
                    b.ptr(Some(self.module_ctx), 0);
                    b.ptr(None, 0); // rt, filled by the runtime
                    Some(self.data(&format!("ash_obj.{i}"), b, 2, Linkage::Local))
                }
                k if (k == hl::hl_type_kind_HFUN || k == hl::hl_type_kind_HMETHOD) && t.fun.is_some() => {
                    let f = t.fun.as_ref().expect("checked");
                    let args = self.type_array(&format!("ash_fun_args.{i}"), &f.args);
                    let mut b = Blob::default();
                    b.ptr(args, 0);
                    b.ptr(self.type_ptr(Some(f.ret.0)), 0);
                    b.i32(f.args.len() as i32);
                    b.ptr(self.type_ptr(f.parent.as_ref().map(|p| p.0)), 0);
                    // closure_type { kind, p } and closure { args, ret, nargs,
                    // parent }: scratch the runtime fills.
                    for _ in 0..6 {
                        b.u32(0);
                    }
                    Some(self.data(&format!("ash_fun.{i}"), b, 2, Linkage::Local))
                }
                k if k == hl::hl_type_kind_HENUM && t.tenum.is_some() => {
                    let e = t.tenum.as_ref().expect("checked");
                    let name = self.utf16(&e.name);
                    let (gv, gv_add) = self.global_slot(e.global_value);
                    let constructs = if e.constructs.is_empty() {
                        None
                    } else {
                        let mut cs = Blob::default();
                        for (ci, c) in e.constructs.iter().enumerate() {
                            let cname = self.utf16(&c.name);
                            let params = self.type_array(&format!("ash_enum_params.{i}.{ci}"), &c.params);
                            let kinds = c.params.iter().map(|p| bc.types[p.0].kind);
                            let (offsets, size) = enum_construct_layout(kinds, 4);
                            let offs = self.i32_array(&format!("ash_enum_offsets.{i}.{ci}"), &offsets);
                            cs.ptr(Some(cname), 0);
                            cs.i32(c.params.len() as i32);
                            cs.ptr(params, 0);
                            cs.i32(size);
                            cs.u8(u8::from(c.hasptr));
                            cs.pad(4);
                            cs.ptr(offs, 0);
                        }
                        Some(self.data(&format!("ash_enum_constructs.{i}"), cs, 2, Linkage::Local))
                    };
                    let mut b = Blob::default();
                    b.ptr(Some(name), 0);
                    b.i32(e.constructs.len() as i32);
                    b.ptr(constructs, 0);
                    b.ptr(gv, gv_add);
                    Some(self.data(&format!("ash_enum.{i}"), b, 2, Linkage::Local))
                }
                k if k == hl::hl_type_kind_HVIRTUAL && t.virt.is_some() => {
                    let v = t.virt.as_ref().expect("checked");
                    let fields = self.fields(&format!("ash_vfields.{i}"), &v.fields);
                    let indexes = self.i32_array(&format!("ash_virt_idx.{i}"), &v.indexes[..v.fields.len().min(v.indexes.len())]);
                    let mut b = Blob::default();
                    b.ptr(fields, 0);
                    b.i32(v.fields.len() as i32);
                    b.i32(v.data_size as i32);
                    b.ptr(indexes, 0);
                    b.ptr(None, 0); // lookup, built by the first access
                    Some(self.data(&format!("ash_virt.{i}"), b, 2, Linkage::Local))
                }
                k if k == hl::hl_type_kind_HABSTRACT => t.abs_name.as_deref().map(|n| self.utf16(n)),
                k if k == hl::hl_type_kind_HNULL
                    || k == hl::hl_type_kind_HREF
                    || k == hl::hl_type_kind_HPACKED =>
                {
                    self.type_ptr(t.tparam.as_ref().map(|p| p.0))
                }
                _ => None,
            };
            blob.ptr(arm, 0);
            blob.u32(0); // vobj_proto
            blob.u32(0); // mark_bits
            let sym = self.types[i];
            self.obj.set_data(sym, &blob.bytes, &blob.refs);
        }
    }
}

/// What `build` found it could not do, which sends the caller to the LLVM
/// path.
pub enum Outcome {
    Built(Vec<u8>),
    Declined(String),
}

/// Build `path` for wasm32 as one object, if nothing the program asks for is
/// outside this path. A function that cannot be emitted fails the build, or,
/// with `allow_refused`, becomes a body that raises when it is called.
pub fn build(
    path: &std::path::Path,
    triple: &str,
    polls: bool,
    shadow: bool,
    allow_refused: bool,
    quiet: bool,
) -> Result<Outcome> {
    crate::native_lib::choose_std_linkage(path);
    crate::native_lib::init_std_library()?;
    let abi = crate::target_abi::TargetAbi::for_triple(triple)?;
    let bc = crate::bytecode::BytecodeDecoder::decode_for_abi(path, &abi)?;

    // A cast to a host closure needs the host's closure types, which the
    // request carries and this path does not read yet.
    let host_closure_cast = bc.natives.iter().any(|n| {
        crate::native_lib::host_link(&n.lib, &n.name).is_some_and(|l| {
            l.arg_casts
                .iter()
                .chain(std::iter::once(&l.ret_cast))
                .any(|c| c.as_deref() == Some("ash:closure"))
        })
    });
    if host_closure_cast {
        return Ok(Outcome::Declined("a link casts to a host closure".into()));
    }

    let nfun = bc
        .functions
        .iter()
        .map(|f| f.findex as usize)
        .chain(bc.natives.iter().map(|n| n.findex as usize))
        .max()
        .map_or(1, |m| m + 1);
    let symbols: HashMap<usize, String> = bc
        .functions
        .iter()
        .map(|f| (f.findex as usize, format!("Fun_{}", f.findex)))
        .collect();
    let barriers = {
        let cards = std::env::var("ASH_GC_CARDS");
        !cards.as_deref().is_ok_and(|v| v == "0")
            && abi.generational_gc()
            && (cards.as_deref().is_ok_and(|v| v == "force")
                || bc.natives.iter().all(|n| crate::native_lib::is_runtime_lib(&n.lib)))
    };
    let data = ProgramData {
        globals: Some("ash_globals".to_string()),
        functions: Some("ash_functions".to_string()),
        types: (0..bc.types.len()).map(|i| (i, format!("ash_type_{i}"))).collect(),
        sized_alloc: crate::llvm::function::sized_alloc_enabled(),
        object_tails: HashMap::new(),
        object_drops: std::collections::HashSet::new(),
        barriers,
        link_thunks: true,
    };

    let mut obj = ObjectBuilder::new();
    // The functions that stand for the host's linked natives, declared before
    // the bodies that call them are emitted.
    let mut links: Vec<LinkNative> = Vec::new();
    {
        use ash_wasm_emit::Program;
        let program = crate::wasm_direct::BytecodeProgram {
            bc: &bc,
            symbols: &symbols,
            data: &data,
        };
        for n in &bc.natives {
            let findex = n.findex as usize;
            let Some(link) = crate::native_lib::host_link(&n.lib, &n.name) else { continue };
            let Some(sig) = program.signature(findex) else { continue };
            let Some(fun) = bc.types[n.type_.0].fun.as_ref() else { continue };
            let params: Vec<ValType> = sig.params.iter().filter_map(|t| wasm_of(*t)).collect();
            let results: Vec<ValType> = wasm_of(sig.ret).into_iter().collect();
            let sym = obj.declare_function(&program.symbol(findex), params.clone(), results.clone(), Linkage::Hidden);
            links.push(LinkNative {
                findex,
                sym,
                link,
                args: fun.args.iter().map(|a| a.0).collect(),
                ret: fun.ret.0,
                params,
                results,
            });
        }
    }
    let emitted = emit_into(
        &mut obj,
        &bc,
        &symbols,
        &data,
        ash_wasm_emit::Options {
            polls,
            shadow,
            stub_skipped: true,
        },
    );
    if !emitted.skipped.is_empty() && !allow_refused {
        let mut msg = format!(
            "{} function(s) could not be lowered; refusing to write a binary \
             that would abort when one is reached:",
            emitted.skipped.len()
        );
        for (fx, why) in emitted.skipped.iter().take(10) {
            msg.push_str(&format!("\n    findex={fx}: {}", why.lines().next().unwrap_or("")));
        }
        if emitted.skipped.len() > 10 {
            msg.push_str(&format!("\n    ... and {} more", emitted.skipped.len() - 10));
        }
        msg.push_str("\n  --allow-refused emits anyway, if those paths are never taken.");
        anyhow::bail!(msg);
    }
    if !quiet {
        eprintln!(
            "[aot] direct wasm backend: {} of {} functions, no LLVM",
            emitted.functions.len(),
            bc.functions.len()
        );
    }

    // The natives of libraries the host opens, that the code reached.
    let mut slots: BTreeMap<String, (String, String)> = BTreeMap::new();
    for n in &bc.natives {
        let lib = n.lib.strip_prefix('?').unwrap_or(&n.lib).to_string();
        let slot = format!("ash_native_{lib}_{}", n.name);
        if emitted.data.iter().any(|d| *d == slot) {
            slots.insert(slot, (lib, n.name.clone()));
        }
    }

    // The natives the program takes as values: methods bound to them, and
    // functions closed over, in code and in constants.
    let mut as_values: BTreeSet<usize> = BTreeSet::new();
    for t in &bc.types {
        if let Some(o) = &t.obj {
            as_values.extend(o.proto.iter().map(|p| p.findex as usize));
        }
    }
    for func in &bc.functions {
        for op in func.ops() {
            match op {
                crate::opcodes::Opcode::StaticClosure { fun, .. }
                | crate::opcodes::Opcode::InstanceClosure { fun, .. } => {
                    as_values.insert(fun.0);
                }
                _ => {}
            }
        }
    }
    for c in &bc.constants {
        let Some(global_type) = bc.globals.get(c.global as usize) else { continue };
        let Some(o) = bc.types[global_type.0].obj.as_ref() else { continue };
        for (j, &value) in c.fields.iter().enumerate() {
            let Some(field) = o.fields.get(j) else { break };
            let kind = bc.types[field.type_.0].kind;
            if kind == hl::hl_type_kind_HFUN || kind == hl::hl_type_kind_HMETHOD {
                as_values.insert(value as usize);
            }
        }
    }
    let mut value_natives: Vec<ValueNative> = Vec::new();
    {
        use ash_wasm_emit::Program;
        let program = crate::wasm_direct::BytecodeProgram {
            bc: &bc,
            symbols: &symbols,
            data: &data,
        };
        for &findex in &as_values {
            let Some(n) = bc.natives.iter().find(|n| n.findex as usize == findex) else { continue };
            if links.iter().any(|l| l.findex == findex) {
                continue;
            }
            let Some(sig) = program.signature(findex) else { continue };
            let params: Vec<ValType> = sig.params.iter().filter_map(|t| wasm_of(*t)).collect();
            let results: Vec<ValType> = wasm_of(sig.ret).into_iter().collect();
            let how = match program.native_slot(findex) {
                Some(s) => {
                    slots.insert(s.slot.clone(), (s.lib.clone(), s.prim.clone()));
                    NativeKind::Slot {
                        slot: s.slot,
                        lib: s.lib,
                        prim: s.prim,
                    }
                }
                None => NativeKind::Runtime(format!("hlp_{}", n.name)),
            };
            value_natives.push(ValueNative {
                findex,
                params,
                results,
                how,
            });
        }
    }

    let mut image = {
        let globals = obj.declare_data("ash_globals", 4 * bc.globals.len().max(1) as u32, 2, Linkage::Hidden);
        let functions = obj.declare_data("ash_functions", 4 * nfun as u32, 2, Linkage::Hidden);
        let function_types = obj.declare_data("ash_function_types", 4 * nfun as u32, 2, Linkage::Hidden);
        let module_ctx = obj.declare_data("ash_module_ctx", 12, 2, Linkage::Local);
        let types = (0..bc.types.len())
            .map(|i| obj.declare_data(&format!("ash_type_{i}"), 16, 2, Linkage::Hidden))
            .collect();
        Image {
            obj: &mut obj,
            bc: &bc,
            types,
            names: HashMap::new(),
            cstrs: HashMap::new(),
            strings: Vec::new(),
            nfun,
            nglobals: bc.globals.len(),
            globals,
            functions,
            function_types,
            module_ctx,
            value_natives,
            links,
        }
    };
    image.finish(&slots, barriers)?;
    Ok(Outcome::Built(obj.finish()))
}

impl Image<'_> {
    fn finish(&mut self, slots: &BTreeMap<String, (String, String)>, barriers: bool) -> Result<()> {
        let bc = self.bc;
        self.type_table();

        // The constant pool: strings as UTF-16, byte blobs as they are.
        for (i, s) in bc.strings.iter().enumerate() {
            let mut blob = Blob::default();
            for unit in s.encode_utf16().chain(std::iter::once(0)) {
                blob.bytes.extend(unit.to_le_bytes());
            }
            let sym = self.data(&format!("String_{i}"), blob, 1, Linkage::Hidden);
            self.strings.push(sym);
        }
        for (i, &pos) in bc.bytes_pos.iter().enumerate() {
            let end = bc.bytes_pos.get(i + 1).copied().unwrap_or(bc.bytes_data.len());
            let blob = Blob {
                bytes: bc.bytes_data[pos..end].to_vec(),
                refs: Vec::new(),
            };
            self.data(&format!("Bytes_{i}"), blob, 0, Linkage::Hidden);
        }

        // The slots of natives in host libraries.
        for slot in slots.keys() {
            self.obj.declare_data(slot, 4, 2, Linkage::Local);
        }

        // Function addresses and types.
        let mut fns = Blob::default();
        let mut fts = Blob::default();
        let mut type_of: Vec<Option<usize>> = vec![None; self.nfun];
        for f in &bc.functions {
            type_of[f.findex as usize] = Some(f.type_.0);
        }
        for n in &bc.natives {
            type_of[n.findex as usize] = Some(n.type_.0);
        }
        let link_slots = self.link_thunks()?;
        let is_function: std::collections::HashSet<usize> = bc
            .functions
            .iter()
            .map(|f| f.findex as usize)
            .chain(self.links.iter().map(|l| l.findex))
            .collect();
        let mut native_entry: HashMap<usize, Sym> = HashMap::new();
        for i in 0..self.value_natives.len() {
            let (findex, params, results) = {
                let n = &self.value_natives[i];
                (n.findex, n.params.clone(), n.results.clone())
            };
            let sym = match &self.value_natives[i].how {
                NativeKind::Runtime(name) => self.obj.import_function(name, params, results),
                NativeKind::Slot { slot, lib, prim } => {
                    let (slot, lib, prim) = (slot.clone(), lib.clone(), prim.clone());
                    self.slot_caller(&slot, &lib, &prim, params, results)?
                }
            };
            native_entry.insert(findex, sym);
        }
        for findex in 0..self.nfun {
            if is_function.contains(&findex) {
                match self.obj.lookup(&format!("Fun_{findex}")) {
                    Some(sym) => fns.function(sym),
                    None => fns.u32(0),
                }
            } else if let Some(&sym) = native_entry.get(&findex) {
                fns.function(sym);
            } else {
                fns.u32(0);
            }
            fts.ptr(self.type_ptr(type_of[findex]), 0);
        }
        self.obj.set_data(self.functions, &fns.bytes, &fns.refs);
        self.obj.set_data(self.function_types, &fts.bytes, &fts.refs);
        let mut ctx = Blob::default();
        ctx.u32(0);
        ctx.ptr(Some(self.functions), 0);
        ctx.ptr(Some(self.function_types), 0);
        self.obj.set_data(self.module_ctx, &ctx.bytes, &ctx.refs);

        let placed = self.static_constants();
        let trampolines = self.trampolines()?;
        let late = self.late_init(slots, &link_slots, &trampolines)?;
        let init = self.module_init(barriers, &placed)?;
        self.entry(init, late)?;
        Ok(())
    }

    /// The constant objects whose fields are all known without running
    /// anything, laid out as data: a class header, strings and type
    /// descriptors by address, and integers. The global slots point at them
    /// from the start. What cannot be placed (a field holding a closure or
    /// another constant) is built by the start-up routine.
    fn static_constants(&mut self) -> (std::collections::HashSet<usize>, Option<(Sym, u32)>) {
        let bc = self.bc;
        let mut handled = std::collections::HashSet::new();
        let mut blob = Blob::default();
        let mut members: Vec<(usize, u32)> = Vec::new();
        for c in &bc.constants {
            let g = c.global as usize;
            if g >= bc.globals.len() {
                continue;
            }
            let type_index = bc.globals[g].0;
            let t = &bc.types[type_index];
            if t.kind != hl::hl_type_kind_HOBJ && t.kind != hl::hl_type_kind_HSTRUCT {
                continue;
            }
            let Some(o) = t.obj.as_ref() else { continue };
            let Some(layout) = crate::layout::object_layout_for(&bc.types, type_index, 4) else {
                continue;
            };
            if c.fields.len() > o.fields.len() || layout.field_offsets.len() < o.fields.len() {
                continue;
            }
            let placeable = c.fields.iter().enumerate().all(|(j, _)| {
                let k = bc.types[o.fields[j].type_.0].kind;
                !(k == hl::hl_type_kind_HFUN
                    || k == hl::hl_type_kind_HMETHOD
                    || k == hl::hl_type_kind_HOBJ
                    || k == hl::hl_type_kind_HSTRUCT)
            });
            if !placeable {
                continue;
            }
            // Each member starts where a boxed value may: eight-aligned.
            blob.pad(8);
            let base = blob.bytes.len();
            let start = layout.field_offsets.len() - o.fields.len();
            let mut cursor: i32 = 0;
            if t.kind == hl::hl_type_kind_HOBJ {
                blob.ptr(Some(self.types[type_index]), 0);
                cursor = 4;
            }
            let mut fits = true;
            for (j, &value) in c.fields.iter().enumerate() {
                let off = layout.field_offsets[start + j];
                if off < cursor {
                    fits = false;
                    break;
                }
                while (blob.bytes.len() - base) < off as usize {
                    blob.u8(0);
                }
                let at = value as usize;
                let k = bc.types[o.fields[j].type_.0].kind;
                if k == hl::hl_type_kind_HBYTES {
                    blob.ptr(self.strings.get(at).copied(), 0);
                } else if k == hl::hl_type_kind_HTYPE {
                    blob.ptr(self.types.get(at).copied(), 0);
                } else {
                    // The field value is an index into the integer pool.
                    blob.i32(bc.ints.get(at).copied().unwrap_or(value));
                }
                cursor = off + 4;
            }
            if !fits {
                blob.bytes.truncate(base);
                blob.refs.retain(|(o, _)| (*o as usize) < base);
                continue;
            }
            while (blob.bytes.len() - base) < layout.size.max(cursor) as usize {
                blob.u8(0);
            }
            members.push((g, base as u32));
            handled.insert(g);
        }
        blob.pad(8);
        if members.is_empty() {
            return (handled, None);
        }
        let size = blob.bytes.len() as u32;
        let sym = self.data("ash_constants", blob, 3, Linkage::Hidden);
        // The global slots hold their addresses already.
        let mut slots = Blob::default();
        for g in 0..self.nglobals.max(1) {
            match members.iter().find(|(m, _)| *m == g) {
                Some((_, base)) => slots.ptr(Some(sym), *base as i32),
                None => slots.u32(0),
            }
        }
        self.obj.set_data(self.globals, &slots.bytes, &slots.refs);
        (handled, Some((sym, size)))
    }

    /// One function per distinct signature the program calls through a
    /// pointer, taking `(fun, args, out)`, so that the runtime can call a
    /// function whose shape it learns at run time.
    fn trampolines(&mut self) -> Result<Option<(Sym, Sym, usize)>> {
        use crate::llvm::aot_trampoline::signature_key;
        let bc = self.bc;
        let mut wanted: BTreeMap<u64, (u32, Vec<u32>)> = BTreeMap::new();
        let kind_of = |t: &crate::types::TypeRef| bc.types[t.0].kind as u32;
        for ty in &bc.types {
            let Some(fun) = ty.fun.as_ref() else { continue };
            let ret = kind_of(&fun.ret);
            let args: Vec<u32> = fun.args.iter().map(kind_of).collect();
            wanted.insert(signature_key(ret, &args), (ret, args));
            if let Some(closure) = fun.closure.as_ref() {
                let ret = kind_of(&closure.ret);
                let args: Vec<u32> = closure.args.iter().map(kind_of).collect();
                wanted.insert(signature_key(ret, &args), (ret, args));
            }
            if !fun.args.is_empty() {
                let ret = kind_of(&fun.ret);
                let args: Vec<u32> = fun.args[1..].iter().map(kind_of).collect();
                wanted.insert(signature_key(ret, &args), (ret, args));
            }
        }
        if wanted.is_empty() {
            return Ok(None);
        }
        let mut keys = Blob::default();
        let mut fns = Blob::default();
        for (key, (ret, args)) in &wanted {
            let sym = self.trampoline(*key, *ret, args);
            keys.u64(*key);
            fns.function(sym);
        }
        let keys = self.data("ash_tramp_keys", keys, 3, Linkage::Local);
        let fns = self.data("ash_tramp_fns", fns, 2, Linkage::Local);
        Ok(Some((keys, fns, wanted.len())))
    }

    /// A function that calls a native of a host library through its slot, or
    /// reports that it is missing, which is what a table entry for it is.
    fn slot_caller(
        &mut self,
        slot: &str,
        lib: &str,
        prim: &str,
        params: Vec<ValType>,
        results: Vec<ValType>,
    ) -> Result<Sym> {
        let name = format!("{slot}_caller");
        if let Some(s) = self.obj.lookup(&name) {
            return Ok(s);
        }
        let slot_sym = self
            .obj
            .lookup(slot)
            .ok_or_else(|| anyhow!("no slot {slot}"))?;
        let lib_s = self.cstr(lib);
        let prim_s = self.cstr(prim);
        let missing = self.imp("hlp_aot_native_missing", &[I32, I32], &[]);
        let n = params.len() as u32;
        let sym = self
            .obj
            .declare_function(&name, params.clone(), results.clone(), Linkage::Local);
        let mut f = F::new(&[(1, I32)]);
        for i in 0..n {
            f.get(i);
        }
        f.addr(slot_sym, 0).i(I::I32Load(mem(0, 2))).set(n);
        f.get(n).i(I::I32Eqz).i(I::If(BlockType::Empty));
        f.addr(lib_s, 0).addr(prim_s, 0).call(missing);
        f.i(I::Unreachable).i(I::End);
        f.get(n);
        f.body.call_indirect(params, results);
        self.obj.set_body(sym, f.finish());
        Ok(sym)
    }

    fn imp(&mut self, name: &str, p: &[ValType], r: &[ValType]) -> Sym {
        self.obj.import_function(name, p.to_vec(), r.to_vec())
    }

    fn trampoline(&mut self, key: u64, ret: u32, args: &[u32]) -> Sym {
        use hl::*;
        let ret_kind = ret as hl_type_kind;
        let wasm_of = |k: hl_type_kind| match k {
            k if k == hl_type_kind_HF32 => ValType::F32,
            k if k == hl_type_kind_HF64 => ValType::F64,
            k if k == hl_type_kind_HI64 => ValType::I64,
            _ => ValType::I32,
        };
        let params: Vec<ValType> = args.iter().map(|&k| wasm_of(k as hl_type_kind)).collect();
        let void = ret_kind == hl_type_kind_HVOID;
        let scalar = ret_kind == hl_type_kind_HF32
            || ret_kind == hl_type_kind_HF64
            || ret_kind == hl_type_kind_HI64
            || ret_kind == hl_type_kind_HBOOL
            || ret_kind == hl_type_kind_HUI8
            || ret_kind == hl_type_kind_HUI16
            || ret_kind == hl_type_kind_HI32;
        let results: Vec<ValType> = if void { vec![] } else { vec![wasm_of(ret_kind)] };
        let sym = self.obj.declare_function(
            &format!("ash_tramp_{key:016x}"),
            vec![I32, I32, I32],
            vec![I32],
            Linkage::Local,
        );
        // Parameters: 0 the function, 1 the argument slots, 2 the box the
        // result goes into. Local 3 holds a scalar result.
        let mut f = F::new(&[(1, wasm_of(ret_kind))]);
        for (i, &kind) in args.iter().enumerate() {
            let kind = kind as hl_type_kind;
            f.get(1).i(I::I32Load(mem(4 * i as u64, 2)));
            // A pointer travels as itself; everything else is read from where
            // the slot points, an integer as a double.
            if kind == hl_type_kind_HF32 {
                f.i(I::F32Load(mem(0, 2)));
            } else if kind == hl_type_kind_HF64 {
                f.i(I::F64Load(mem(0, 3)));
            } else if kind == hl_type_kind_HI64 {
                f.i(I::I64Load(mem(0, 3)));
            } else if kind == hl_type_kind_HBOOL
                || kind == hl_type_kind_HUI8
                || kind == hl_type_kind_HUI16
                || kind == hl_type_kind_HI32
            {
                f.i(I::F64Load(mem(0, 3))).i(I::I32TruncSatF64S);
            }
        }
        f.get(0);
        f.body.call_indirect(params, results);
        if void {
            f.c(0);
        } else if scalar {
            // The result goes into the value word of the box the caller made.
            f.set(3).get(2).get(3);
            f.i(if ret_kind == hl_type_kind_HF32 {
                I::F32Store(mem(VDYNAMIC_VALUE, 2))
            } else if ret_kind == hl_type_kind_HF64 {
                I::F64Store(mem(VDYNAMIC_VALUE, 3))
            } else if ret_kind == hl_type_kind_HI64 {
                I::I64Store(mem(VDYNAMIC_VALUE, 3))
            } else {
                I::I32Store(mem(VDYNAMIC_VALUE, 2))
            });
            f.c(0);
        }
        // A pointer result is on the stack already, and is the answer.
        self.obj.set_body(sym, f.finish());
        sym
    }

    /// `ash_module_init`: tell the runtime what the program is, then build the
    /// constant objects the bytecode expects to exist.
    fn module_init(
        &mut self,
        barriers: bool,
        placed: &(std::collections::HashSet<usize>, Option<(Sym, u32)>),
    ) -> Result<Sym> {
        let bc = self.bc;
        let init = self
            .obj
            .declare_function("ash_module_init", vec![], vec![], Linkage::Default);
        // 0 the object, 1 its runtime layout, 2 where its own fields start,
        // 3 the table of field offsets.
        let mut f = F::new(&[(4, I32)]);

        let mut libs: Vec<String> = bc
            .natives
            .iter()
            .map(|n| n.lib.strip_prefix('?').unwrap_or(&n.lib).to_string())
            .filter(|lib| !crate::native_lib::is_runtime_lib(lib))
            .collect();
        libs.sort_unstable();
        libs.dedup();
        if !barriers && libs.is_empty() {
            libs.push("this program, built without the write barrier".to_string());
        }
        if !libs.is_empty() {
            let loaded = self.imp("hlp_gc_native_library_loaded", &[I32, I32, I32], &[]);
            for lib in &libs {
                let s = self.cstr(lib);
                f.addr(s, 0).c(lib.len() as i32).c(0).call(loaded);
            }
        }

        let set_globals = self.imp("hlp_gc_set_globals", &[I32, I32], &[]);
        f.addr(self.globals, 0).c(self.nglobals as i32).call(set_globals);
        // The constants laid out as data are immortal, but a field of one can
        // be assigned a heap object later, so the collector scans the range.
        if let Some((blob, size)) = placed.1 {
            let add = self.imp("hlp_gc_add_static_range", &[I32, I32], &[]);
            f.addr(blob, 0).c(size as i32).call(add);
        }

        // Names of fields, methods and dynamic fields, so a hash can be turned
        // back into one.
        let mut names: BTreeSet<String> = BTreeSet::new();
        for t in &bc.types {
            if let Some(o) = &t.obj {
                names.extend(o.fields.iter().map(|x| x.name.clone()));
                names.extend(o.proto.iter().map(|x| x.name.clone()));
            }
            if let Some(v) = &t.virt {
                names.extend(v.fields.iter().map(|x| x.name.clone()));
            }
        }
        for func in &bc.functions {
            for op in func.ops() {
                match op {
                    crate::opcodes::Opcode::DynGet { field, .. }
                    | crate::opcodes::Opcode::DynSet { field, .. } => {
                        if let Some(name) = bc.strings.get(field.0) {
                            names.insert(name.clone());
                        }
                    }
                    _ => {}
                }
            }
        }
        let hash_gen = self.imp("hlp_hash_gen", &[I32, I32], &[I32]);
        for name in names.iter().filter(|n| !n.is_empty()) {
            let s = self.utf16(name);
            f.addr(s, 0).c(1).call(hash_gen).i(I::Drop);
        }

        // Names of functions, for a trace; each body's entry position; and the
        // table of source files.
        let keys = crate::types::function_keys(&bc.types, &bc.functions);
        let mut name_table = Blob::default();
        let mut positions = Blob::default();
        let mut entry_pos: Vec<u64> = vec![0; self.nfun];
        for func in &bc.functions {
            let at = func.findex as usize;
            if let Some(found) = func.debug().get(..2).filter(|e| e[0] >= 0 && e[1] > 0) {
                // `file + 1`, so that a packed position is never 0 for a body
                // that has one.
                entry_pos[at] = ((found[0] as u64 + 1) << 32) | (found[1] as u64);
            }
        }
        for findex in 0..self.nfun {
            let name = keys
                .get(&(findex as u32))
                .cloned()
                .unwrap_or_else(|| format!("Fun_{findex}"));
            let s = self.cstr(&name);
            name_table.ptr(Some(s), 0);
            positions.u64(entry_pos[findex]);
        }
        let names_sym = self.data("ash_function_names", name_table, 2, Linkage::Local);
        let positions_sym = self.data("ash_function_positions", positions, 3, Linkage::Local);
        let register = self.imp("hlp_register_aot_symbols", &[I32, I32, I32], &[]);
        f.addr(self.functions, 0).addr(names_sym, 0).c(self.nfun as i32).call(register);
        let register_pos = self.imp("hlp_register_aot_positions", &[I32, I32], &[]);
        f.addr(positions_sym, 0).c(self.nfun as i32).call(register_pos);

        let mut files = Blob::default();
        for file in &bc.debug_files {
            let s = self.cstr(file);
            files.ptr(Some(s), 0);
        }
        let files_sym = self.data("ash_debug_files", files, 2, Linkage::Local);
        let register_files = self.imp("hlp_register_aot_debug_files", &[I32, I32], &[]);
        f.addr(files_sym, 0).c(bc.debug_files.len() as i32).call(register_files);

        let policy = self.imp("hlp_set_null_write_raises", &[I32], &[]);
        f.c(i32::from(bc.has_debug)).call(policy);
        let install = self.imp("hlp_install_static_call", &[], &[]);
        f.call(install);
        let runner = self.imp("hlp_install_closure_runner", &[], &[]);
        f.call(runner);
        let worker = self.imp("hlp_set_compiled_worker_mode", &[I32], &[]);
        f.c(1).call(worker);

        // The constant objects, allocated and filled the way the JIT does
        // when it starts.
        let alloc_obj = self.imp("hlp_alloc_obj", &[I32], &[I32]);
        let root = self.imp("hlp_gc_register_root", &[I32], &[]);
        let get_rt = self.imp("hlp_get_obj_rt", &[I32], &[I32]);
        let closure = self.imp("hlp_alloc_closure_void", &[I32, I32], &[I32]);
        for constant in &bc.constants {
            let g = constant.global as usize;
            if g >= bc.globals.len() || placed.0.contains(&g) {
                continue;
            }
            let type_index = bc.globals[g].0;
            let t = &bc.types[type_index];
            if t.kind != hl::hl_type_kind_HOBJ && t.kind != hl::hl_type_kind_HSTRUCT {
                continue;
            }
            let Some(o) = t.obj.as_ref() else { continue };
            f.addr(self.types[type_index], 0).call(alloc_obj).set(0);
            f.addr(self.globals, 4 * g as i32).get(0).i(I::I32Store(mem(0, 2)));
            f.get(0).call(root);
            if constant.fields.is_empty() {
                continue;
            }
            f.addr(self.types[type_index], 0).call(get_rt).set(1);
            // The parent's fields come first; a constant carries its own.
            f.get(1).i(I::I32Load(mem(RT_NFIELDS, 2))).c(o.fields.len() as i32).i(I::I32Sub).set(2);
            f.get(1).i(I::I32Load(mem(RT_FIELD_INDEXES, 2))).set(3);
            for (j, &value) in constant.fields.iter().enumerate() {
                if j >= o.fields.len() {
                    break;
                }
                let field_type = o.fields[j].type_.0;
                let kind = bc.types[field_type].kind;
                // obj + indexes[start + j]
                f.get(0).get(3).get(2).c(j as i32).i(I::I32Add).c(4).i(I::I32Mul).i(I::I32Add);
                f.i(I::I32Load(mem(0, 2))).i(I::I32Add);
                let at = value as usize;
                if kind == hl::hl_type_kind_HFUN || kind == hl::hl_type_kind_HMETHOD {
                    if at >= self.nfun {
                        f.i(I::Drop);
                        continue;
                    }
                    f.addr(self.types[field_type], 0)
                        .addr(self.functions, 4 * at as i32)
                        .i(I::I32Load(mem(0, 2)))
                        .call(closure)
                        .i(I::I32Store(mem(0, 2)));
                } else if kind == hl::hl_type_kind_HTYPE {
                    match self.types.get(at).copied() {
                        Some(sym) => {
                            f.addr(sym, 0).i(I::I32Store(mem(0, 2)));
                        }
                        None => {
                            f.i(I::Drop);
                        }
                    }
                } else if kind == hl::hl_type_kind_HOBJ || kind == hl::hl_type_kind_HSTRUCT {
                    if at < self.nglobals {
                        f.addr(self.globals, 4 * at as i32)
                            .i(I::I32Load(mem(0, 2)))
                            .i(I::I32Store(mem(0, 2)));
                    } else {
                        f.i(I::Drop);
                    }
                } else if kind == hl::hl_type_kind_HBYTES {
                    match self.strings.get(at).copied() {
                        Some(sym) => {
                            f.addr(sym, 0).i(I::I32Store(mem(0, 2)));
                        }
                        None => {
                            f.i(I::Drop);
                        }
                    }
                } else {
                    // The field value is an index into the integer pool.
                    let v = bc.ints.get(at).copied().unwrap_or(value);
                    f.c(v).i(I::I32Store(mem(0, 2)));
                }
            }
        }
        self.obj.set_body(init, f.finish());
        Ok(init)
    }

    /// `ash_late_init`: what needs the bodies to exist -- the call
    /// trampolines, and the slots of natives in host libraries.
    fn late_init(
        &mut self,
        slots: &BTreeMap<String, (String, String)>,
        link_slots: &[(String, String)],
        trampolines: &Option<(Sym, Sym, usize)>,
    ) -> Result<Sym> {
        let late = self
            .obj
            .declare_function("ash_late_init", vec![], vec![], Linkage::Local);
        let mut f = F::new(&[]);
        if let Some((keys, fns, count)) = trampolines {
            let register = self.imp("hlp_register_call_trampolines", &[I32, I32, I32], &[]);
            f.addr(*keys, 0).addr(*fns, 0).c(*count as i32).call(register);
        }
        if !slots.is_empty() {
            let resolve = self.imp("hlp_aot_native", &[I32, I32], &[I32]);
            for (slot, (lib, prim)) in slots {
                let slot_sym = self
                    .obj
                    .lookup(slot)
                    .ok_or_else(|| anyhow!("no slot {slot}"))?;
                let lib_s = self.cstr(lib);
                let prim_s = self.cstr(prim);
                f.addr(slot_sym, 0)
                    .addr(lib_s, 0)
                    .addr(prim_s, 0)
                    .call(resolve)
                    .i(I::I32Store(mem(0, 2)));
            }
        }
        // The callees of linked natives that are in side modules, found by
        // their own names.
        if !link_slots.is_empty() {
            let find = self.imp("hlp_aot_symbol", &[I32, I32], &[I32]);
            for (lib, symbol) in link_slots {
                let slot = self
                    .obj
                    .lookup(&format!("ash_link_{lib}_{symbol}"))
                    .ok_or_else(|| anyhow!("no link slot for {symbol} in {lib}"))?;
                let lib_s = self.cstr(lib);
                let sym_s = self.cstr(symbol);
                f.addr(slot, 0)
                    .addr(lib_s, 0)
                    .addr(sym_s, 0)
                    .call(find)
                    .i(I::I32Store(mem(0, 2)));
            }
        }
        self.obj.set_body(late, f.finish());
        Ok(late)
    }

    /// `main`, the outer exception boundary around the entrypoint, and the
    /// boundary a thread body runs under.
    fn entry(&mut self, init: Sym, late: Sym) -> Result<()> {
        let bc = self.bc;
        let entrypoint = self
            .obj
            .lookup(&format!("Fun_{}", bc.entrypoint))
            .ok_or_else(|| anyhow!("the entrypoint f{} has no body", bc.entrypoint))?;
        let setup = self.imp("hlp_setup_trap_jit", &[], &[I32]);
        let remove = self.imp("hlp_remove_trap_jit", &[], &[]);
        let get_exc = self.imp("hlp_get_exc_value", &[], &[I32]);
        let print = self.imp("hlp_print_uncaught_exception", &[I32], &[]);
        let clear = self.imp("hlp_clear_exc_value", &[], &[]);
        let jump = self.imp("__wasm_longjmp", &[I32, I32], &[]);

        // __ash_safe_entrypoint() -> i64: 0 after a normal return, 1 after
        // printing an exception nothing caught. Locals: 0 the stack pointer on
        // entry, 1 the trap's buffer, 2 what was thrown, 3 the buffer it was
        // copied from.
        let safe = self
            .obj
            .declare_function("__ash_safe_entrypoint", vec![], vec![ValType::I64], Linkage::Local);
        let mut f = F::new(&[(4, I32)]);
        f.body.stack_pointer_get();
        f.set(0);
        f.call(setup).set(1);
        f.get(1).get(1).i(I::I32Store(mem(0, 2)));
        f.get(1).c(-1).i(I::I32Store(mem(4, 2)));
        f.i(I::Block(BlockType::Result(ValType::I32)));
        f.body.try_long_jump(0);
        f.call(entrypoint).call(remove);
        f.i(I::I64Const(0)).i(I::Return);
        f.i(I::End);
        f.i(I::Unreachable);
        f.i(I::End);
        f.set(2);
        f.get(0);
        f.body.stack_pointer_set();
        f.get(2).i(I::I32Load(mem(0, 2))).set(3);
        f.get(3).i(I::I32Load(mem(0, 2))).get(1).i(I::I32Eq);
        f.i(I::If(BlockType::Empty));
        f.call(get_exc).call(print).call(clear);
        f.i(I::I64Const(1)).i(I::Return);
        f.i(I::Else);
        f.get(3).get(2).i(I::I32Load(mem(4, 2))).call(jump);
        f.i(I::Unreachable);
        f.i(I::End);
        f.i(I::Unreachable);
        self.obj.set_body(safe, f.finish());

        // __ash_trap_boundary(setup, remove, callback, context) -> i32: the
        // frame a thread body runs under, 1 when an exception landed.
        let boundary = self.obj.declare_function(
            "__ash_trap_boundary",
            vec![I32, I32, I32, I32],
            vec![I32],
            Linkage::Local,
        );
        let mut f = F::new(&[(4, I32)]);
        f.body.stack_pointer_get();
        f.set(4);
        f.get(0);
        f.body.call_indirect(vec![], vec![I32]);
        f.set(5);
        f.get(5).get(5).i(I::I32Store(mem(0, 2)));
        f.get(5).c(-1).i(I::I32Store(mem(4, 2)));
        f.i(I::Block(BlockType::Result(ValType::I32)));
        f.body.try_long_jump(0);
        f.get(3).get(2);
        f.body.call_indirect(vec![I32], vec![]);
        f.get(1);
        f.body.call_indirect(vec![], vec![]);
        f.c(0).i(I::Return);
        f.i(I::End);
        f.i(I::Unreachable);
        f.i(I::End);
        f.set(6);
        f.get(4);
        f.body.stack_pointer_set();
        f.get(6).i(I::I32Load(mem(0, 2))).set(7);
        f.get(7).i(I::I32Load(mem(0, 2))).get(5).i(I::I32Eq);
        f.i(I::If(BlockType::Empty));
        f.c(1).i(I::Return);
        f.i(I::Else);
        f.get(7).get(6).i(I::I32Load(mem(4, 2))).call(jump);
        f.i(I::Unreachable);
        f.i(I::End);
        f.i(I::Unreachable);
        self.obj.set_body(boundary, f.finish());

        // main(argc, argv)
        let main = self
            .obj
            .declare_function("main", vec![I32, I32], vec![I32], Linkage::Default);
        let gc_init = self.imp("hlp_gc_init", &[], &[]);
        let stack_top = self.imp("hlp_gc_set_stack_top", &[I32], &[]);
        let start = self.imp("hlp_program_start", &[], &[I32]);
        let set_boundary = self.imp("hlp_set_trap_boundary", &[I32], &[]);
        let mut f = F::new(&[(1, I32)]);
        f.call(gc_init);
        f.body.stack_pointer_get();
        f.call(stack_top);
        f.call(init).call(late);
        f.call(start).set(2);
        f.get(2);
        f.i(I::If(BlockType::Empty));
        f.get(2).i(I::Return);
        f.i(I::End);
        f.body.function_pointer(boundary);
        f.call(set_boundary);
        f.call(safe).i(I::I32WrapI64);
        self.obj.set_body(main, f.finish());
        Ok(())
    }
}
