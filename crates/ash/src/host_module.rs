//! Classes a host exposes to a program before it runs.
//!
//! A host (an embedder such as caribou) describes classes with HashLink-typed
//! fields and methods, each method backed by a C entry point. Registering the
//! description into a `DecodedBytecode` appends types, natives and globals so
//! that the interpreter built afterwards, and every tier above it, sees them
//! as decoded: a call into a host method is a typed native call in ash's own
//! tables, resolved from the registration rather than from a library on disk.

use crate::bytecode::{field_hash, DecodedBytecode};
use crate::hl;
use crate::native_lib::HostNative;
use crate::types::{HLNative, HLObjField, HLObjProto, HLType, HLTypeFun, HLTypeObj, TypeRef};
use anyhow::{anyhow, bail, Result};
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::ffi::{c_char, c_void, CString};

#[derive(Debug, Clone, Deserialize)]
pub struct HostModule {
    /// The library tag the program's `@:hlNative("<lib>", ...)` names.
    pub lib: String,
    pub classes: Vec<HostClass>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct HostClass {
    /// Fully qualified, as the program spells it: `wren.hud.Hud`.
    pub name: String,
    /// A host or program class, by name.
    #[serde(default)]
    pub superclass: Option<String>,
    #[serde(default)]
    pub fields: Vec<HostField>,
    #[serde(default)]
    pub methods: Vec<HostMethod>,
    #[serde(default)]
    pub statics: Vec<HostMethod>,
    #[serde(default)]
    pub ctor: Option<HostMethod>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct HostField {
    pub name: String,
    pub ty: HostType,
}

#[derive(Debug, Clone, Deserialize)]
pub struct HostMethod {
    pub name: String,
    /// The native's name: `@:hlNative(lib, symbol)` on the program side.
    pub symbol: String,
    #[serde(default)]
    pub params: Vec<HostType>,
    #[serde(default = "HostType::void")]
    pub ret: HostType,
    /// The C entry: a typed function taking the HL arguments directly, not a
    /// DEFINE_PRIM resolver. Not part of the JSON form; `resolve` fills it.
    #[serde(skip, default = "std::ptr::null")]
    pub func: *const c_void,
    /// Passed to `func` before the HL arguments when non-null, so one entry
    /// can serve many methods (`native_lib::HostNative`). Not in the JSON
    /// form.
    #[serde(skip, default = "std::ptr::null")]
    pub context: *const c_void,
    /// `func` takes its arguments as a record of words
    /// (`native_lib::HostNative::record`). Not in the JSON form.
    #[serde(skip)]
    pub record: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub enum HostType {
    Void,
    Bool,
    I32,
    I64,
    F32,
    F64,
    Bytes,
    Dyn,
    /// A class, host or program, by name.
    Obj(String),
    /// An `hl.Abstract<"name">`, created on demand.
    Abstract(String),
    Array,
    /// A closure, opaque to the host.
    Fun,
}

impl HostType {
    fn void() -> Self {
        HostType::Void
    }
}

/// Where a registered class landed in the program's tables.
#[derive(Debug, Clone)]
pub struct HostClassEntry {
    pub name: String,
    pub type_index: usize,
    pub companion_index: usize,
    /// The global slot holding the class object (an instance of the
    /// companion), which is what `hlp_type_get_global` answers with.
    pub global_index: usize,
}

/// `pkg.Name` -> `pkg.$Name`, the companion type Haxe emits for statics.
fn companion_name(name: &str) -> String {
    match name.rfind('.') {
        Some(dot) => format!("{}.${}", &name[..dot], &name[dot + 1..]),
        None => format!("${name}"),
    }
}

fn kind_name(kind: hl::hl_type_kind) -> &'static str {
    match kind {
        hl::hl_type_kind_HVOID => "void",
        hl::hl_type_kind_HUI8 => "ui8",
        hl::hl_type_kind_HUI16 => "ui16",
        hl::hl_type_kind_HI32 => "i32",
        hl::hl_type_kind_HI64 => "i64",
        hl::hl_type_kind_HF32 => "f32",
        hl::hl_type_kind_HF64 => "f64",
        hl::hl_type_kind_HBOOL => "bool",
        hl::hl_type_kind_HBYTES => "bytes",
        hl::hl_type_kind_HDYN => "dynamic",
        hl::hl_type_kind_HFUN => "fun",
        hl::hl_type_kind_HOBJ => "obj",
        hl::hl_type_kind_HARRAY => "array",
        hl::hl_type_kind_HTYPE => "type",
        hl::hl_type_kind_HREF => "ref",
        hl::hl_type_kind_HVIRTUAL => "virtual",
        hl::hl_type_kind_HDYNOBJ => "dynobj",
        hl::hl_type_kind_HABSTRACT => "abstract",
        hl::hl_type_kind_HENUM => "enum",
        hl::hl_type_kind_HNULL => "null",
        hl::hl_type_kind_HMETHOD => "method",
        hl::hl_type_kind_HSTRUCT => "struct",
        hl::hl_type_kind_HPACKED => "packed",
        _ => "?",
    }
}

impl DecodedBytecode {
    /// Index of the object type named `name`, program or host.
    pub fn type_index_of(&self, name: &str) -> Option<usize> {
        self.types
            .iter()
            .position(|t| t.obj.as_ref().is_some_and(|o| o.name == name))
    }

    /// Render a function type as `(a, b) -> r` by kinds, for diagnostics.
    fn signature_text(&self, type_index: usize) -> String {
        let Some(fun) = self.types.get(type_index).and_then(|t| t.fun.as_ref()) else {
            return "<not a function type>".into();
        };
        let kind = |r: &TypeRef| {
            self.types
                .get(r.0)
                .map(|t| kind_name(t.kind))
                .unwrap_or("?")
        };
        let args: Vec<&str> = fun.args.iter().map(kind).collect();
        format!("({}) -> {}", args.join(", "), kind(&fun.ret))
    }

    /// Append `m`'s classes and natives to the program.
    ///
    /// Per class: an HOBJ type, its `$Class` companion (super `hl.Class`,
    /// carrying the `__constructor__` binding when there is a ctor), one
    /// global for the class object, and one HFUN type plus one `HLNative` per
    /// method, static and ctor, numbered after the last findex the program
    /// uses. Field and proto hashes come from the same function the decoder
    /// uses, so the compiled Haxe side computes the same values.
    ///
    /// Errors: a class the program already declares, a native registered
    /// twice, a program-declared native whose signature disagrees with the
    /// registration, or a type name nothing declares.
    pub fn register_host_module(&mut self, m: &HostModule) -> Result<()> {
        let hl_class = self.type_index_of("hl.Class").ok_or_else(|| {
            anyhow!("cannot register host module `{}`: the program has no hl.Class type, which every class companion extends", m.lib)
        })?;

        // Validate before touching anything, and undo on a later failure, so
        // a refused registration leaves the program as it was.
        let mut seen_classes: HashSet<&str> = HashSet::new();
        let mut seen_natives: HashSet<&str> = HashSet::new();
        for c in &m.classes {
            if self.type_index_of(&c.name).is_some() {
                bail!("host class `{}` is already declared by the program", c.name);
            }
            if !seen_classes.insert(&c.name) {
                bail!(
                    "host class `{}` is declared twice in module `{}`",
                    c.name,
                    m.lib
                );
            }
            for method in c.methods.iter().chain(&c.statics).chain(&c.ctor) {
                let key = (m.lib.clone(), method.symbol.clone());
                if self.host_natives.contains_key(&key) || !seen_natives.insert(&method.symbol) {
                    bail!(
                        "host native `{}@{}` is registered twice",
                        m.lib,
                        method.symbol
                    );
                }
                if method.func.is_null() {
                    bail!(
                        "host native `{}@{}` has no entry point",
                        m.lib,
                        method.symbol
                    );
                }
            }
        }

        let mark = (
            self.types.len(),
            self.natives.len(),
            self.globals.len(),
            self.host_classes.len(),
        );
        let result = self.append_host_module(m, hl_class);
        if result.is_err() {
            self.types.truncate(mark.0);
            self.natives.truncate(mark.1);
            self.globals.truncate(mark.2);
            self.host_classes.truncate(mark.3);
            for c in &m.classes {
                for method in c.methods.iter().chain(&c.statics).chain(&c.ctor) {
                    self.host_natives
                        .remove(&(m.lib.clone(), method.symbol.clone()));
                }
            }
        }
        result
    }

    fn append_host_module(&mut self, m: &HostModule, hl_class: usize) -> Result<()> {
        // Class types first, as empty shells, so a field, parameter or
        // superclass can name any class of the module regardless of order.
        let mut class_indices = Vec::with_capacity(m.classes.len());
        for c in &m.classes {
            let index = self.push_type(HLType {
                kind: hl::hl_type_kind_HOBJ,
                obj: Some(HLTypeObj {
                    name: c.name.clone(),
                    ..Default::default()
                }),
                ..Default::default()
            });
            class_indices.push(index);
        }

        let mut next_findex = self
            .functions
            .iter()
            .map(|f| f.findex)
            .chain(self.natives.iter().map(|n| n.findex))
            .max()
            .unwrap_or(-1)
            + 1;
        let void = self.type_of_kind(hl::hl_type_kind_HVOID);
        let ctor_field = self.constructor_field_index(hl_class)?;

        let mut new_natives: Vec<(String, usize)> = Vec::new();
        for (c, &class_index) in m.classes.iter().zip(&class_indices) {
            let super_ = match &c.superclass {
                Some(name) => Some(TypeRef(self.type_index_of(name).ok_or_else(|| {
                    anyhow!(
                        "host class `{}` extends `{}`, which neither the program nor the module declares",
                        c.name,
                        name
                    )
                })?)),
                None => None,
            };
            let mut fields = Vec::with_capacity(c.fields.len());
            for f in &c.fields {
                let type_ = TypeRef(self.type_index_for(&f.ty)?);
                fields.push(HLObjField {
                    name: f.name.clone(),
                    type_,
                    hashed_name: field_hash(&f.name),
                });
            }

            // Each entry is a native in the shared findex numbering. A
            // host-supplied AIR body would go in here instead: an HLFunction
            // at the same findex whose lowering is the host's
            // air::v2::Function, installed into the SSA cache.
            let mut add_native = |this: &mut Self,
                                  method: &HostMethod,
                                  receiver: Option<usize>,
                                  ret: Option<usize>|
             -> Result<i32> {
                let mut args = Vec::with_capacity(method.params.len() + 1);
                if let Some(r) = receiver {
                    args.push(TypeRef(r));
                }
                for p in &method.params {
                    args.push(TypeRef(this.type_index_for(p)?));
                }
                let ret = match ret {
                    Some(r) => r,
                    None => this.type_index_for(&method.ret)?,
                };
                let fun_type = this.push_type(HLType {
                    kind: hl::hl_type_kind_HFUN,
                    fun: Some(HLTypeFun {
                        args,
                        ret: TypeRef(ret),
                        ..Default::default()
                    }),
                    ..Default::default()
                });
                let findex = next_findex;
                next_findex += 1;
                this.natives.push(HLNative {
                    lib: m.lib.clone(),
                    name: method.symbol.clone(),
                    type_: TypeRef(fun_type),
                    findex,
                });
                this.host_natives.insert(
                    (m.lib.clone(), method.symbol.clone()),
                    HostNative {
                        addr: method.func as usize,
                        context: method.context as usize,
                        record: method.record,
                    },
                );
                new_natives.push((method.symbol.clone(), fun_type));
                Ok(findex)
            };

            let mut proto = Vec::with_capacity(c.methods.len());
            for method in &c.methods {
                let findex = add_native(self, method, Some(class_index), None)?;
                proto.push(HLObjProto {
                    name: method.name.clone(),
                    findex,
                    // No override can exist, so no vtable slot: what Haxe
                    // emits for a method nothing overrides.
                    pindex: -1,
                    hashed_name: field_hash(&method.name),
                });
            }
            for method in &c.statics {
                add_native(self, method, None, None)?;
            }
            let mut bindings = Vec::new();
            if let Some(ctor) = &c.ctor {
                let findex = add_native(self, ctor, Some(class_index), Some(void))?;
                bindings.push(ctor_field);
                bindings.push(findex);
            }

            let companion_index = self.push_type(HLType {
                kind: hl::hl_type_kind_HOBJ,
                obj: Some(HLTypeObj {
                    name: companion_name(&c.name),
                    super_: Some(TypeRef(hl_class)),
                    fields: Vec::new(),
                    proto: Vec::new(),
                    bindings,
                    global_value: 0,
                }),
                ..Default::default()
            });
            self.globals.push(TypeRef(companion_index));
            let global_index = self.globals.len() - 1;

            let obj = self.types[class_index]
                .obj
                .as_mut()
                .expect("host class shell is an object type");
            obj.super_ = super_;
            obj.fields = fields;
            obj.proto = proto;
            // 1-based, 0 meaning none, as the bytecode spells it.
            obj.global_value = global_index as u32 + 1;

            self.host_classes.push(HostClassEntry {
                name: c.name.clone(),
                type_index: class_index,
                companion_index,
                global_index,
            });
        }

        self.check_declared_natives(&m.lib, &new_natives)
    }

    /// A program-declared native that a host also registered must agree
    /// with it in argument kinds and return kind: that is where the compiled
    /// `@:hlNative` meets the registration.
    fn check_declared_natives(&self, lib: &str, registered: &[(String, usize)]) -> Result<()> {
        let by_symbol: HashMap<&str, usize> =
            registered.iter().map(|(s, t)| (s.as_str(), *t)).collect();
        for native in &self.natives {
            let clean = native.lib.strip_prefix('?').unwrap_or(&native.lib);
            if clean != lib {
                continue;
            }
            let Some(&host_type) = by_symbol.get(native.name.as_str()) else {
                continue;
            };
            if native.type_.0 == host_type {
                continue; // the registration's own entry
            }
            let declared = self.types[native.type_.0].fun.as_ref();
            let promised = self.types[host_type].fun.as_ref();
            let agree = match (declared, promised) {
                (Some(d), Some(p)) => {
                    d.args.len() == p.args.len()
                        && d.args
                            .iter()
                            .zip(&p.args)
                            .all(|(a, b)| self.types[a.0].kind == self.types[b.0].kind)
                        && self.types[d.ret.0].kind == self.types[p.ret.0].kind
                }
                _ => false,
            };
            if !agree {
                bail!(
                    "native `{}@{}`: the program declares it as {} but the host registers it as {}",
                    lib,
                    native.name,
                    self.signature_text(native.type_.0),
                    self.signature_text(host_type)
                );
            }
        }
        Ok(())
    }

    fn push_type(&mut self, t: HLType) -> usize {
        self.types.push(t);
        self.types.len() - 1
    }

    /// The program's type of a primitive kind, created if it has none.
    fn type_of_kind(&mut self, kind: hl::hl_type_kind) -> usize {
        match self.types.iter().position(|t| t.kind == kind) {
            Some(i) => i,
            None => self.push_type(HLType {
                kind,
                ..Default::default()
            }),
        }
    }

    fn type_index_for(&mut self, ty: &HostType) -> Result<usize> {
        Ok(match ty {
            HostType::Void => self.type_of_kind(hl::hl_type_kind_HVOID),
            HostType::Bool => self.type_of_kind(hl::hl_type_kind_HBOOL),
            HostType::I32 => self.type_of_kind(hl::hl_type_kind_HI32),
            HostType::I64 => self.type_of_kind(hl::hl_type_kind_HI64),
            HostType::F32 => self.type_of_kind(hl::hl_type_kind_HF32),
            HostType::F64 => self.type_of_kind(hl::hl_type_kind_HF64),
            HostType::Bytes => self.type_of_kind(hl::hl_type_kind_HBYTES),
            HostType::Dyn => self.type_of_kind(hl::hl_type_kind_HDYN),
            HostType::Array => self.type_of_kind(hl::hl_type_kind_HARRAY),
            HostType::Obj(name) => self.type_index_of(name).ok_or_else(|| {
                anyhow!("host type `{name}` is not a class the program or the module declares")
            })?,
            HostType::Abstract(name) => {
                let found = self.types.iter().position(|t| {
                    t.kind == hl::hl_type_kind_HABSTRACT && t.abs_name.as_deref() == Some(name)
                });
                match found {
                    Some(i) => i,
                    None => self.push_type(HLType {
                        kind: hl::hl_type_kind_HABSTRACT,
                        abs_name: Some(name.clone()),
                        ..Default::default()
                    }),
                }
            }
            // Opaque: only the kind is promised, so any closure type will do
            // for marshaling and for the declared-vs-registered check.
            HostType::Fun => {
                let void = self.type_of_kind(hl::hl_type_kind_HVOID);
                let found = self.types.iter().position(|t| {
                    t.kind == hl::hl_type_kind_HFUN
                        && t.fun
                            .as_ref()
                            .is_some_and(|f| f.args.is_empty() && f.ret.0 == void)
                });
                match found {
                    Some(i) => i,
                    None => self.push_type(HLType {
                        kind: hl::hl_type_kind_HFUN,
                        fun: Some(HLTypeFun {
                            args: Vec::new(),
                            ret: TypeRef(void),
                            ..Default::default()
                        }),
                        ..Default::default()
                    }),
                }
            }
        })
    }

    /// Flat field index of `hl.Class.__constructor__`, counted the way a
    /// binding names it: every ancestor's fields first.
    fn constructor_field_index(&self, hl_class: usize) -> Result<i32> {
        let class = self.types[hl_class]
            .obj
            .as_ref()
            .ok_or_else(|| anyhow!("hl.Class is not an object type"))?;
        let mut inherited = 0;
        let mut parent = class.super_.as_ref();
        while let Some(sup) = parent {
            let sup = self.types[sup.0]
                .obj
                .as_ref()
                .ok_or_else(|| anyhow!("hl.Class has a non-object ancestor"))?;
            inherited += sup.fields.len();
            parent = sup.super_.as_ref();
        }
        let own = class
            .fields
            .iter()
            .position(|f| f.name == "__constructor__")
            .ok_or_else(|| anyhow!("hl.Class has no __constructor__ field"))?;
        Ok((inherited + own) as i32)
    }
}

/// Register a host module described as JSON into the `DecodedBytecode` at
/// `bc`, for a host that reaches ash through its C exports rather than as a
/// Rust crate; a Rust host calls `register_host_module` and
/// `NativeFunctionResolver::with_host_natives` directly. Exported from
/// ash_core, not ash_std: the decode lives here and ash_std cannot see it.
///
/// `json` is a `HostModule` without `func` fields; `resolve` maps each
/// method's `symbol` to its C entry, and a null answer is an error. The
/// natives are also published to the process-wide host registry the native
/// resolver consults, since such a host holds no resolver.
///
/// Returns false, with the reason on stderr, on any error.
///
/// # Safety
/// `bc` must point to a live `DecodedBytecode` that nothing else is using,
/// and `json` must be readable for `len` bytes.
#[no_mangle]
pub unsafe extern "C" fn hlp_host_register_module(
    bc: *mut c_void,
    json: *const u8,
    len: usize,
    resolve: unsafe extern "C" fn(*const c_char) -> *const c_void,
) -> bool {
    let result = (|| -> Result<()> {
        if bc.is_null() || json.is_null() {
            bail!("null program or description");
        }
        let text = std::slice::from_raw_parts(json, len);
        let mut module: HostModule = serde_json::from_slice(text)?;
        for class in &mut module.classes {
            for method in class
                .methods
                .iter_mut()
                .chain(class.statics.iter_mut())
                .chain(class.ctor.iter_mut())
            {
                let symbol = CString::new(method.symbol.as_str())?;
                method.func = resolve(symbol.as_ptr());
                if method.func.is_null() {
                    bail!(
                        "symbol `{}` of host class `{}` resolved to null",
                        method.symbol,
                        class.name
                    );
                }
            }
        }
        let bc = &mut *(bc as *mut DecodedBytecode);
        bc.register_host_module(&module)?;
        crate::native_lib::publish_host_natives(&bc.host_natives);
        Ok(())
    })();
    match result {
        Ok(()) => true,
        Err(e) => {
            eprintln!("[ash] host module registration failed: {e:#}");
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode::BytecodeDecoder;
    use crate::native_lib::init_std_library;
    use std::path::PathBuf;

    fn fixture() -> DecodedBytecode {
        init_std_library().expect("std library");
        let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        path.push("test/test.hl");
        BytecodeDecoder::decode(&path).expect("decode fixture")
    }

    extern "C" fn stub_make() -> *mut c_void {
        std::ptr::null_mut()
    }
    extern "C" fn stub_bump(_this: *mut c_void) -> i32 {
        0
    }
    extern "C" fn stub_ctor(_this: *mut c_void) {}

    fn greeter_module() -> HostModule {
        HostModule {
            lib: "host".into(),
            classes: vec![HostClass {
                name: "test.Greeter".into(),
                superclass: None,
                fields: vec![HostField {
                    name: "count".into(),
                    ty: HostType::I32,
                }],
                methods: vec![HostMethod {
                    name: "bump".into(),
                    symbol: "greeter_bump".into(),
                    params: vec![],
                    ret: HostType::I32,
                    func: stub_bump as *const c_void,
                    context: std::ptr::null(),
                    record: false,
                }],
                statics: vec![HostMethod {
                    name: "make".into(),
                    symbol: "greeter_make".into(),
                    params: vec![],
                    ret: HostType::Obj("test.Greeter".into()),
                    func: stub_make as *const c_void,
                    context: std::ptr::null(),
                    record: false,
                }],
                ctor: Some(HostMethod {
                    name: "new".into(),
                    symbol: "greeter_new".into(),
                    params: vec![],
                    ret: HostType::Void,
                    func: stub_ctor as *const c_void,
                    context: std::ptr::null(),
                    record: false,
                }),
            }],
        }
    }

    #[test]
    fn registration_extends_every_table() {
        let mut bc = fixture();
        let ntypes = bc.types.len();
        let nglobals = bc.globals.len();
        let nnatives = bc.natives.len();
        let last_findex = bc
            .functions
            .iter()
            .map(|f| f.findex)
            .chain(bc.natives.iter().map(|n| n.findex))
            .max()
            .unwrap();

        bc.register_host_module(&greeter_module())
            .expect("register");

        let class = bc.type_index_of("test.Greeter").expect("class type");
        let companion = bc.type_index_of("test.$Greeter").expect("companion type");
        assert!(class >= ntypes && companion >= ntypes, "both appended");

        // The class: one field, one proto per instance method, hashes from
        // the decoder's function, the global pointing at the companion.
        let obj = bc.types[class].obj.as_ref().unwrap();
        assert_eq!(bc.types[class].kind, hl::hl_type_kind_HOBJ);
        assert!(obj.super_.is_none());
        assert_eq!(obj.fields.len(), 1);
        assert_eq!(obj.fields[0].name, "count");
        assert_eq!(bc.types[obj.fields[0].type_.0].kind, hl::hl_type_kind_HI32);
        assert_eq!(obj.fields[0].hashed_name, field_hash("count"));
        assert_eq!(obj.proto.len(), 1);
        assert_eq!(obj.proto[0].name, "bump");
        assert_eq!(obj.proto[0].pindex, -1);
        assert_eq!(obj.proto[0].hashed_name, field_hash("bump"));
        assert!(obj.bindings.is_empty());
        assert_eq!(bc.globals.len(), nglobals + 1);
        assert_eq!(obj.global_value as usize, nglobals + 1);
        assert_eq!(bc.globals[nglobals].0, companion);

        // The companion: extends hl.Class, no fields of its own, and binds
        // __constructor__ (flat field 4: three hl.BaseType fields, then
        // __name__) to the ctor's findex.
        let comp = bc.types[companion].obj.as_ref().unwrap();
        let hl_class = bc.type_index_of("hl.Class").unwrap();
        assert_eq!(comp.super_.as_ref().unwrap().0, hl_class);
        assert!(comp.fields.is_empty() && comp.proto.is_empty());
        assert_eq!(comp.global_value, 0);
        assert_eq!(comp.bindings.len(), 2);
        assert_eq!(comp.bindings[0], 4);

        // Natives: three, sequential after the program's last findex, typed
        // with the receiver first for the method and the ctor.
        assert_eq!(bc.natives.len(), nnatives + 3);
        let added = &bc.natives[nnatives..];
        let findexes: Vec<i32> = added.iter().map(|n| n.findex).collect();
        assert_eq!(
            findexes,
            vec![last_findex + 1, last_findex + 2, last_findex + 3]
        );
        assert!(added.iter().all(|n| n.lib == "host"));
        let by_name = |s: &str| added.iter().find(|n| n.name == s).unwrap();
        let bump = bc.types[by_name("greeter_bump").type_.0]
            .fun
            .as_ref()
            .unwrap();
        assert_eq!(bump.args.len(), 1);
        assert_eq!(bump.args[0].0, class);
        assert_eq!(bc.types[bump.ret.0].kind, hl::hl_type_kind_HI32);
        let make = bc.types[by_name("greeter_make").type_.0]
            .fun
            .as_ref()
            .unwrap();
        assert!(make.args.is_empty());
        assert_eq!(make.ret.0, class);
        let ctor = bc.types[by_name("greeter_new").type_.0]
            .fun
            .as_ref()
            .unwrap();
        assert_eq!(ctor.args[0].0, class);
        assert_eq!(bc.types[ctor.ret.0].kind, hl::hl_type_kind_HVOID);
        assert_eq!(obj.proto[0].findex, by_name("greeter_bump").findex);
        assert_eq!(comp.bindings[1], by_name("greeter_new").findex);

        // The host's entries.
        assert_eq!(
            bc.host_natives[&("host".to_string(), "greeter_bump".to_string())].addr,
            stub_bump as usize
        );
        assert_eq!(bc.host_classes.len(), 1);
        assert_eq!(bc.host_classes[0].type_index, class);
        assert_eq!(bc.host_classes[0].companion_index, companion);
        assert_eq!(bc.host_classes[0].global_index, nglobals);
    }

    #[test]
    fn a_class_the_program_declares_is_rejected() {
        let mut bc = fixture();
        let mut m = greeter_module();
        m.classes[0].name = "String".into();
        let err = bc.register_host_module(&m).unwrap_err().to_string();
        assert!(err.contains("`String`"), "{err}");
        assert!(err.contains("already declared"), "{err}");
    }

    #[test]
    fn registering_twice_is_rejected_and_leaves_the_first_intact() {
        let mut bc = fixture();
        bc.register_host_module(&greeter_module()).expect("first");
        let ntypes = bc.types.len();
        let err = bc
            .register_host_module(&greeter_module())
            .unwrap_err()
            .to_string();
        assert!(err.contains("test.Greeter"), "{err}");
        assert_eq!(
            bc.types.len(),
            ntypes,
            "a refused registration appends nothing"
        );

        // The same lib+symbol under a fresh class name is the native clash.
        let mut m = greeter_module();
        m.classes[0].name = "test.Other".into();
        let err = bc.register_host_module(&m).unwrap_err().to_string();
        assert!(
            err.contains("host@greeter_bump") || err.contains("host@greeter_make"),
            "{err}"
        );
        assert!(err.contains("twice"), "{err}");
    }

    #[test]
    fn a_declared_native_that_disagrees_fails_the_load() {
        let mut bc = fixture();
        // The program side: `@:hlNative("host", "greeter_bump")` declared as
        // (i32) -> i32, where the host promises (test.Greeter) -> i32.
        let i32_t = bc.type_of_kind(hl::hl_type_kind_HI32);
        let declared_type = bc.push_type(HLType {
            kind: hl::hl_type_kind_HFUN,
            fun: Some(HLTypeFun {
                args: vec![TypeRef(i32_t)],
                ret: TypeRef(i32_t),
                ..Default::default()
            }),
            ..Default::default()
        });
        let findex = bc.natives.iter().map(|n| n.findex).max().unwrap() + 1;
        bc.natives.push(HLNative {
            lib: "host".into(),
            name: "greeter_bump".into(),
            type_: TypeRef(declared_type),
            findex,
        });
        let err = bc
            .register_host_module(&greeter_module())
            .unwrap_err()
            .to_string();
        assert!(err.contains("host@greeter_bump"), "{err}");
        assert!(err.contains("(i32) -> i32"), "{err}");
        assert!(err.contains("(obj) -> i32"), "{err}");
    }

    #[test]
    fn a_declared_native_that_agrees_loads() {
        let mut bc = fixture();
        let class_placeholder = bc.type_index_of("String").unwrap();
        let i32_t = bc.type_of_kind(hl::hl_type_kind_HI32);
        let declared_type = bc.push_type(HLType {
            kind: hl::hl_type_kind_HFUN,
            fun: Some(HLTypeFun {
                args: vec![TypeRef(class_placeholder)],
                ret: TypeRef(i32_t),
                ..Default::default()
            }),
            ..Default::default()
        });
        let findex = bc.natives.iter().map(|n| n.findex).max().unwrap() + 1;
        bc.natives.push(HLNative {
            lib: "?host".into(),
            name: "greeter_bump".into(),
            type_: TypeRef(declared_type),
            findex,
        });
        bc.register_host_module(&greeter_module())
            .expect("kinds agree: (obj) -> i32 on both sides");
    }

    #[test]
    fn the_json_form_round_trips_through_the_export() {
        unsafe extern "C" fn resolve(sym: *const c_char) -> *const c_void {
            let name = std::ffi::CStr::from_ptr(sym).to_string_lossy();
            match name.as_ref() {
                "greeter_bump" => stub_bump as *const c_void,
                "greeter_make" => stub_make as *const c_void,
                _ => std::ptr::null(),
            }
        }
        let json = r#"{
            "lib": "host",
            "classes": [{
                "name": "test.Greeter",
                "fields": [{"name": "count", "ty": "I32"}],
                "methods": [{"name": "bump", "symbol": "greeter_bump", "ret": "I32"}],
                "statics": [{"name": "make", "symbol": "greeter_make",
                             "ret": {"Obj": "test.Greeter"}}]
            }]
        }"#;
        let mut bc = fixture();
        let ok = unsafe {
            hlp_host_register_module(
                &mut bc as *mut DecodedBytecode as *mut c_void,
                json.as_ptr(),
                json.len(),
                resolve,
            )
        };
        assert!(ok);
        assert!(bc.type_index_of("test.$Greeter").is_some());
        assert_eq!(
            bc.host_natives[&("host".to_string(), "greeter_make".to_string())].addr,
            stub_make as usize
        );

        // A symbol the host cannot resolve refuses the whole module.
        let mut fresh = fixture();
        let bad = json.replace("greeter_make", "greeter_missing");
        let ok = unsafe {
            hlp_host_register_module(
                &mut fresh as *mut DecodedBytecode as *mut c_void,
                bad.as_ptr(),
                bad.len(),
                resolve,
            )
        };
        assert!(!ok);
        assert!(fresh.type_index_of("test.Greeter").is_none());
    }
}
