//! Builds HashLink's type table from a typed program: the port of `to_type`, `class_type`,
//! `enum_type` and the override analysis of `genhl.ml`.
//!
//! The runtime reads the layouts this produces, so they match the HashLink generator's
//! exactly: a subclass's fields start where its superclass's end, a method gets a virtual slot
//! only when some class in the program overrides it, an interface is a structure of its
//! methods, and an implemented interface occupies a field of its own.

use std::collections::{HashMap, HashSet};

use ash_tast::Tast;
use ash_tast::format::*;

use crate::hl::*;

pub type Result<T> = std::result::Result<T, String>;

/// What a class declares, gathered from the file once.
struct ClassInfo {
    name: String,
    flags: u32,
    /// The superclass as a type id.
    super_ty: Option<u32>,
    /// The implemented interfaces as type ids.
    ifaces: Vec<u32>,
    /// The instance fields, constructor excluded, in declaration order.
    instance: Vec<u32>,
    statics: Vec<u32>,
    ctor: Option<u32>,
}

/// Work that waits for the outermost class to finish, so a field whose type reaches the
/// class again finds it.
enum Delayed {
    Field {
        obj: ObjIx,
        slot: usize,
        name: String,
        fid: i32,
        ty: u32,
        packed: bool,
    },
    Interface {
        obj: ObjIx,
        slot: usize,
        fid: i32,
        ty: u32,
    },
}

pub struct TypeBuilder<'a> {
    tast: Tast<'a>,
    classes: Vec<ClassInfo>,
    by_name: HashMap<String, u32>,
    /// `(method name, class name)`: some class below this one overrides the method.
    overrides: HashSet<(String, String)>,
    pub table: TypeTable,
    cached: HashMap<String, TypeIx>,
    anons: HashMap<u32, TypeIx>,
    /// Types being resolved, with the definition once it exists: how a recursive abstract
    /// finds the type it is part of.
    rec: Vec<(u32, Option<TypeIx>)>,
    depth: u32,
    delayed: Vec<Delayed>,
    fun_ids: HashMap<(String, String), u32>,
    /// `(class name, function name)` by function index.
    pub fun_names: Vec<(String, String)>,
    global_ids: HashMap<String, u32>,
}

const ARRAY_IMPLS: [&str; 6] = [
    "hl.types.ArrayDyn",
    "hl.types.ArrayBytes_Int",
    "hl.types.ArrayBytes_Float",
    "hl.types.ArrayObj",
    "hl.types.ArrayBytes_hl_F32",
    "hl.types.ArrayBytes_hl_UI16",
];

fn statics_name(name: &str) -> String {
    match name.rsplit_once('.') {
        Some((pack, last)) => format!("{pack}.${last}"),
        None => format!("${name}"),
    }
}

impl<'a> TypeBuilder<'a> {
    pub fn new(tast: Tast<'a>) -> TypeBuilder<'a> {
        let mut classes = Vec::with_capacity(tast.classes().len());
        let mut by_name = HashMap::new();
        for (id, c) in tast.classes().iter().enumerate() {
            let name = tast.opt_string(c.name).unwrap_or("").to_owned();
            let flags = c.flags.get();
            let mut info = ClassInfo {
                name: name.clone(),
                flags,
                super_ty: (c.super_type.get() != NONE).then(|| c.super_type.get()),
                ifaces: tast
                    .type_args(c.interfaces_first, c.interfaces_count)
                    .iter()
                    .map(|t| t.get())
                    .collect(),
                instance: Vec::new(),
                statics: Vec::new(),
                ctor: None,
            };
            let first = c.fields_first.get();
            for f in first..first + c.fields_count.get() {
                let rec = &tast.fields()[f as usize];
                let fname = tast.opt_string(rec.name).unwrap_or("");
                let fl = rec.flags.get();
                if fl & field_flag::STATIC != 0 {
                    info.statics.push(f);
                } else if f == first && fname == "new" && fl & field_flag::METHOD != 0 {
                    info.ctor = Some(f);
                } else {
                    info.instance.push(f);
                }
            }
            if flags & (class_flag::TYPE_PARAM | class_flag::CONST_ARG) == 0 {
                by_name.entry(name).or_insert(id as u32);
            }
            classes.push(info);
        }
        let mut b = TypeBuilder {
            tast,
            classes,
            by_name,
            overrides: HashSet::new(),
            table: TypeTable::default(),
            cached: HashMap::new(),
            anons: HashMap::new(),
            rec: Vec::new(),
            depth: 0,
            delayed: Vec::new(),
            fun_ids: HashMap::new(),
            fun_names: Vec::new(),
            global_ids: HashMap::new(),
        };
        b.find_overrides();
        b
    }

    // ---- the file

    fn type_rec(&self, id: u32) -> TypeRec {
        self.tast.types()[id as usize]
    }

    fn field_flags(&self, f: u32) -> u32 {
        self.tast.fields()[f as usize].flags.get()
    }

    pub fn field_name(&self, f: u32) -> &'a str {
        self.tast
            .opt_string(self.tast.fields()[f as usize].name)
            .unwrap_or("")
    }

    fn has_meta(&self, owner_kind: u32, owner: u32, name: &str) -> bool {
        self.tast
            .metas_of(owner_kind, owner)
            .any(|m| self.tast.string(m.name.get()) == name)
    }

    fn class_named(&self, name: &str) -> Result<u32> {
        self.by_name
            .get(name)
            .copied()
            .ok_or_else(|| format!("the program has no class {name}"))
    }

    fn is_extern(&self, c: u32) -> bool {
        self.classes[c as usize].flags & class_flag::EXTERN != 0
    }

    fn is_interface(&self, c: u32) -> bool {
        self.classes[c as usize].flags & class_flag::INTERFACE != 0
    }

    /// `genhl.ml`'s `is_extern_field`: no storage, a native function, or declared `extern`.
    fn is_extern_field(&self, f: u32) -> bool {
        let fl = self.field_flags(f);
        let normal_method =
            fl & field_flag::METHOD != 0 && fl & (field_flag::INLINE | field_flag::DYNAMIC) == 0;
        fl & field_flag::NOT_PHYSICAL != 0
            || (normal_method && self.is_native(f))
            || fl & field_flag::EXTERN != 0
    }

    /// A function the runtime provides: marked `@:hlNative`, or an empty static method of a class
    /// marked `@:hlNative("library", "prefix")`, which the generator marks itself.
    fn is_native(&self, f: u32) -> bool {
        if self.has_meta(meta_owner::FIELD, f, ":hlNative") {
            return true;
        }
        let rec = &self.tast.fields()[f as usize];
        let fl = rec.flags.get();
        if fl & field_flag::STATIC == 0
            || fl & (field_flag::INLINE | field_flag::DYNAMIC) != 0
            || fl & field_flag::METHOD == 0
        {
            return false;
        }
        let owner = rec.owner.get();
        self.has_meta(meta_owner::CLASS, owner, ":hlNative") && self.has_empty_body(rec.func.get())
    }

    /// No body, `{}`, or `{ return <constant>; }`: what a native binding is declared with.
    fn has_empty_body(&self, func: u32) -> bool {
        if func == NONE {
            return true;
        }
        let root = self.tast.funcs()[func as usize].root.get();
        if root == NONE {
            return true;
        }
        let body = self.tast.node(root);
        if body.kind != node_kind::BLOCK {
            return false;
        }
        match self.tast.kids(body) {
            [] => true,
            [one] if one.kind == node_kind::RETURN && one.flags & node_flag::HAS_VALUE != 0 => self
                .tast
                .kids(one)
                .first()
                .is_some_and(|v| v.kind == node_kind::CONST),
            _ => false,
        }
    }

    // ---- overrides

    /// `add_types`: every class from the one a method overrides up to the topmost that declares
    /// it is marked, because a call through any of them needs a virtual slot.
    fn find_overrides(&mut self) {
        let program: Vec<u32> = self
            .tast
            .order()
            .iter()
            .filter(|o| o.kind.get() == u32::from(module_kind::CLASS))
            .map(|o| o.id.get())
            .collect();
        for c in program {
            let sup = self.super_class(c);
            let overriding: Vec<u32> = self.classes[c as usize]
                .instance
                .iter()
                .copied()
                .filter(|f| self.field_flags(*f) & field_flag::OVERRIDE != 0)
                .collect();
            for f in overriding {
                let name = self.field_name(f).to_owned();
                self.mark_override(sup, &name);
            }
        }
    }

    fn mark_override(&mut self, class: Option<u32>, name: &str) -> bool {
        let Some(p) = class else { return false };
        let declares = self.classes[p as usize]
            .instance
            .iter()
            .any(|f| self.field_name(*f) == name);
        if declares || self.mark_override(self.super_class(p), name) {
            let key = (name.to_owned(), self.classes[p as usize].name.clone());
            self.overrides.insert(key);
            true
        } else {
            false
        }
    }

    fn is_overridden(&self, c: u32, name: &str) -> bool {
        self.overrides
            .contains(&(name.to_owned(), self.classes[c as usize].name.clone()))
    }

    fn super_class(&self, c: u32) -> Option<u32> {
        self.classes[c as usize]
            .super_ty
            .map(|t| self.type_rec(t).def.get())
    }

    // ---- ids

    fn alloc_fun_path(&mut self, class: &str, name: &str) -> u32 {
        let key = (name.to_owned(), class.to_owned());
        if let Some(id) = self.fun_ids.get(&key) {
            return *id;
        }
        let id = self.fun_ids.len() as u32;
        self.fun_ids.insert(key, id);
        self.fun_names.push((class.to_owned(), name.to_owned()));
        id
    }

    fn alloc_global(&mut self, name: &str, t: TypeIx) -> u32 {
        if let Some(g) = self.global_ids.get(name) {
            return *g;
        }
        let g = self.table.globals.len() as u32;
        self.table.globals.push((name.to_owned(), t));
        self.global_ids.insert(name.to_owned(), g);
        g
    }

    // ---- types

    pub fn to_type(&mut self, id: u32) -> Result<TypeIx> {
        self.ty(id, None)
    }

    /// Resolves `id` the way a recursive abstract needs: `tref`, when given, names the entry of
    /// `rec` that the definition is stored in as soon as it exists.
    fn rec_cache(
        &mut self,
        id: u32,
        f: impl FnOnce(&mut Self, usize) -> Result<TypeIx>,
    ) -> Result<TypeIx> {
        if let Some((_, found)) = self.rec.iter().rev().find(|(k, _)| *k == id) {
            return match found {
                Some(t) => Ok(*t),
                None => Ok(self.table.add(HlType::Dyn)),
            };
        }
        self.rec.push((id, None));
        let at = self.rec.len() - 1;
        let out = f(self, at);
        self.rec.pop();
        out
    }

    fn ty(&mut self, id: u32, tref: Option<usize>) -> Result<TypeIx> {
        let t = self.type_rec(id);
        match t.kind {
            type_kind::MONO | type_kind::DYNAMIC => Ok(self.table.add(HlType::Dyn)),
            type_kind::FUN => {
                let args: Vec<u32> = self
                    .tast
                    .type_args(t.args_first, t.args_count)
                    .iter()
                    .map(|a| a.get())
                    .collect();
                let mask = t.opt_mask.get();
                let mut out = Vec::with_capacity(args.len());
                for (i, a) in args.iter().enumerate() {
                    let pt = self.ty(*a, None)?;
                    let optional = mask >> (i.min(31) as u32) & 1 != 0;
                    out.push(if optional && !self.table.is_nullable(pt) {
                        self.table.add(HlType::Ref(pt))
                    } else {
                        pt
                    });
                }
                let ret = self.ty(t.def.get(), None)?;
                Ok(self.table.add(HlType::Fun(out, ret)))
            }
            type_kind::ANON => match t.flags {
                anon_status::STATICS => self.class_type(t.args_first.get(), &[], true, None),
                anon_status::ENUM_STATICS => self.enum_class(t.args_first.get()),
                _ => self.anon_type(id, tref),
            },
            type_kind::ENUM => self.enum_type(t.def.get(), tref),
            type_kind::CLASS => self.class_instance_type(id, tref),
            type_kind::ABSTRACT => self.abstract_type(id, tref),
            other => Err(format!("type kind {other}")),
        }
    }

    fn anon_type(&mut self, id: u32, tref: Option<usize>) -> Result<TypeIx> {
        let t = self.type_rec(id);
        if t.args_count.get() == 0 {
            return Ok(self.table.add(HlType::Dyn));
        }
        if let Some(found) = self.anons.get(&id) {
            return Ok(*found);
        }
        self.table.virts.push(Vec::new());
        let v = self.table.virts.len() - 1;
        let ix = self.table.push_raw(HlType::Virtual(v));
        if let Some(r) = tref {
            self.rec[r].1 = Some(ix);
        }
        self.anons.insert(id, ix);
        let first = t.def.get() as usize;
        let mut fields = Vec::new();
        for f in &self.tast.anon_fields()[first..first + t.args_count.get() as usize] {
            let name = self.tast.string(f.name.get()).to_owned();
            let ft = self.ty(f.ty.get(), None)?;
            let ft = self.as_method(f.flags.get(), ft);
            fields.push((name, ft));
        }
        fields.sort_by(|a, b| a.0.cmp(&b.0));
        self.table.virts[v] = fields;
        Ok(ix)
    }

    /// A method a structure or interface declares is a method pointer, not a function value.
    fn as_method(&mut self, flags: u32, t: TypeIx) -> TypeIx {
        let normal = flags & field_flag::METHOD != 0 && flags & field_flag::DYNAMIC == 0;
        match (&self.table.types[t], normal) {
            (HlType::Fun(args, ret), true) => {
                let (args, ret) = (args.clone(), *ret);
                self.table.add(HlType::Method(args, ret))
            }
            _ => t,
        }
    }

    fn class_instance_type(&mut self, id: u32, tref: Option<usize>) -> Result<TypeIx> {
        let t = self.type_rec(id);
        let c = t.def.get();
        let args: Vec<u32> = self
            .tast
            .type_args(t.args_first, t.args_count)
            .iter()
            .map(|a| a.get())
            .collect();
        let info = &self.classes[c as usize];
        // `hl.Abstract<"name">`
        if info.name == "hl.Abstract" && args.len() == 1 {
            let arg = self.class_of_type(args[0]);
            if let Some(arg) = arg {
                let arg = &self.classes[arg as usize];
                if arg.flags & class_flag::CONST_ARG != 0 && arg.name.starts_with('S') {
                    let name = arg.name[1..].to_owned();
                    return Ok(self.table.add(HlType::Abstract(name)));
                }
            }
        }
        // A type parameter is its first constraint that is a class, else Dynamic.
        if info.flags & class_flag::TYPE_PARAM != 0 {
            let constraints = info.ifaces.clone();
            for k in constraints {
                if let Some(cc) = self.class_of_type(k)
                    && !self.is_interface(cc)
                {
                    return self.ty(k, tref);
                }
            }
            return Ok(self.table.add(HlType::Dyn));
        }
        self.class_type(c, &args, false, tref)
    }

    fn class_of_type(&self, ty: u32) -> Option<u32> {
        let t = self.type_rec(ty);
        (t.kind == type_kind::CLASS).then(|| t.def.get())
    }

    fn abstract_type(&mut self, id: u32, tref: Option<usize>) -> Result<TypeIx> {
        let t = self.type_rec(id);
        let a = t.def.get();
        let name = self
            .tast
            .opt_string(self.tast.abstracts()[a as usize].name)
            .unwrap_or("")
            .to_owned();
        let args: Vec<u32> = self
            .tast
            .type_args(t.args_first, t.args_count)
            .iter()
            .map(|a| a.get())
            .collect();
        if name == "Null" && args.len() == 1 {
            let inner = self.ty(args[0], tref)?;
            return Ok(
                if !self.table.is_nullable(inner) && self.table.types[inner] != HlType::Void {
                    self.table.add(HlType::Null(inner))
                } else {
                    inner
                },
            );
        }
        if self.has_meta(meta_owner::ABSTRACT, a, ":coreType") {
            return match name.as_str() {
                "Void" => Ok(self.table.add(HlType::Void)),
                "Int" | "UInt" => Ok(self.table.add(HlType::I32)),
                "Float" => Ok(self.table.add(HlType::F64)),
                "Single" => Ok(self.table.add(HlType::F32)),
                "Bool" => Ok(self.table.add(HlType::Bool)),
                "Dynamic" | "EnumValue" => Ok(self.table.add(HlType::Dyn)),
                "Class" => {
                    let c = self.class_named("hl.Class")?;
                    self.class_type(c, &[], false, None)
                }
                "Enum" => {
                    let c = self.class_named("hl.BaseType")?;
                    self.class_type(c, &[], false, None)
                }
                "hl.Ref" => {
                    let inner = self.ty(args[0], None)?;
                    Ok(self.table.add(HlType::Ref(inner)))
                }
                "hl.Bytes" | "hl.BytesAccess" => Ok(self.table.add(HlType::Bytes)),
                "hl.Type" => Ok(self.table.add(HlType::Type)),
                "hl.UI16" => Ok(self.table.add(HlType::UI16)),
                "hl.UI8" => Ok(self.table.add(HlType::UI8)),
                "hl.I64" => Ok(self.table.add(HlType::I64)),
                "hl.NativeArray" => Ok(self.table.add(HlType::Array)),
                "haxe.macro.Position" => Ok(self.table.add(HlType::Abstract("macro_pos".into()))),
                other => Err(format!("Unknown core type {other}")),
            };
        }
        let via = t.via.get();
        self.rec_cache(id, |me, at| {
            if via == NONE {
                Ok(me.table.add(HlType::Dyn))
            } else {
                me.ty(via, Some(at))
            }
        })
    }

    // ---- classes

    /// `Array<T>` is the class for T's representation; any other extern class is not supported.
    fn resolve_class(&mut self, c: u32, pl: &[u32], statics: bool) -> Result<u32> {
        let name = self.classes[c as usize].name.clone();
        if name == "Array" {
            if statics {
                return self.class_named("hl.types.ArrayBase");
            }
            let elem = match pl.first() {
                Some(t) => self.ty(*t, None)?,
                None => self.table.add(HlType::Dyn),
            };
            let impl_name = match self.table.types[elem] {
                HlType::I32 => "hl.types.ArrayBytes_Int",
                HlType::UI16 => "hl.types.ArrayBytes_hl_UI16",
                HlType::F32 => "hl.types.ArrayBytes_hl_F32",
                HlType::F64 => "hl.types.ArrayBytes_Float",
                HlType::Dyn => "hl.types.ArrayDyn",
                _ => "hl.types.ArrayObj",
            };
            return self.class_named(impl_name);
        }
        if self.is_extern(c) {
            return Err(format!("Extern type not supported : {name}"));
        }
        Ok(c)
    }

    fn get_index(&self, obj: ObjIx, name: &str) -> Option<(i32, TypeIx)> {
        let mut at = Some(obj);
        while let Some(o) = at {
            if let Some(found) = self.table.objs[o].index.get(name) {
                return Some(*found);
            }
            at = self.table.objs[o]
                .super_
                .and_then(|s| match self.table.types[s] {
                    HlType::Obj(p) | HlType::Struct(p) => Some(p),
                    _ => None,
                });
        }
        None
    }

    fn cfield_type(&mut self, f: u32) -> Result<(String, TypeIx)> {
        let ty = self.tast.fields()[f as usize].ty.get();
        let t = self.ty(ty, None)?;
        let fl = self.field_flags(f);
        let inline_or_normal = fl & field_flag::METHOD != 0 && fl & field_flag::DYNAMIC == 0;
        let t = if inline_or_normal {
            self.as_method(fl, t)
        } else {
            t
        };
        Ok((self.field_name(f).to_owned(), t))
    }

    /// An interface is a structure of its methods: its own fields by descending name, then the
    /// interfaces it extends, the last one first.
    fn interface_fields(&mut self, c: u32) -> Result<Vec<(String, TypeIx)>> {
        let mut acc: Vec<(String, TypeIx)> = Vec::new();
        for iface in self.classes[c as usize].ifaces.clone() {
            let Some(ic) = self.class_of_type(iface) else {
                continue;
            };
            let mut l = self.interface_fields(ic)?;
            l.extend(acc);
            acc = l;
        }
        let mut own = self.classes[c as usize].instance.clone();
        own.sort_by(|a, b| self.field_name(*a).cmp(self.field_name(*b)));
        let mut out = Vec::new();
        for f in own.iter().rev() {
            out.push(self.cfield_type(*f)?);
        }
        out.extend(acc);
        Ok(out)
    }

    pub fn class_type(
        &mut self,
        c: u32,
        pl: &[u32],
        statics: bool,
        tref: Option<usize>,
    ) -> Result<TypeIx> {
        let c = if self.is_extern(c) {
            self.resolve_class(c, pl, statics)?
        } else {
            c
        };
        let name = self.classes[c as usize].name.clone();
        let key = if statics {
            statics_name(&name)
        } else {
            name.clone()
        };
        if let Some(t) = self.cached.get(&key) {
            return Ok(*t);
        }
        if self.is_interface(c) && !statics {
            self.table.virts.push(Vec::new());
            let v = self.table.virts.len() - 1;
            let ix = self.table.push_raw(HlType::Virtual(v));
            self.cached.insert(key, ix);
            self.table.virts[v] = self.interface_fields(c)?;
            return Ok(ix);
        }

        let is_struct = self.has_meta(meta_owner::CLASS, c, ":struct") && !statics;
        let obj = self.table.objs.len();
        self.table.objs.push(ObjType {
            name: key.clone(),
            is_struct,
            nfields: -1,
            ..ObjType::default()
        });
        let t = self.table.push_raw(if is_struct {
            HlType::Struct(obj)
        } else {
            HlType::Obj(obj)
        });
        if let Some(r) = tref {
            self.rec[r].1 = Some(t);
        }
        self.depth += 1;
        self.cached.insert(key.clone(), t);
        if name == "Array" {
            return Err("Array has no class of its own".into());
        }
        let base_class = self.class_named("hl.Class")?;
        if c == base_class {
            if statics {
                return Err("the Class object has no statics".into());
            }
            self.table.objs[obj].nfields = 1;
        }
        let tsup = match (self.super_class(c), statics) {
            (Some(sup), false) => Some(self.class_type(sup, &[], false, None)?),
            _ if statics => Some(self.class_type(base_class, &[], false, None)?),
            _ => None,
        };
        let (start_field, mut virtuals) = match tsup {
            None => (0, Vec::new()),
            Some(pt) => match self.table.types[pt].clone() {
                HlType::Obj(po) | HlType::Struct(po) => {
                    let is_super_struct = matches!(self.table.types[pt], HlType::Struct(_));
                    if is_struct != is_super_struct {
                        return Err(if is_struct {
                            format!("{name}: a struct cannot extend a class")
                        } else {
                            format!("{name}: a class cannot extend a struct")
                        });
                    }
                    if self.table.objs[po].nfields < 0 {
                        return Err(format!("{name}: the superclass is not laid out yet"));
                    }
                    self.table.objs[obj].super_ = Some(pt);
                    (
                        self.table.objs[po].nfields,
                        self.table.objs[po].virtuals.clone(),
                    )
                }
                other => return Err(format!("{name}: the superclass is {other:?}")),
            },
        };

        let void = self.table.add(HlType::Void);
        let mut fa: Vec<(String, TypeIx)> = Vec::new();
        let mut pa: Vec<Proto> = Vec::new();
        let mut bindings: Vec<(u32, u32)> = Vec::new();
        let list = if statics {
            self.classes[c as usize].statics.clone()
        } else {
            self.classes[c as usize].instance.clone()
        };
        for f in list {
            let fname = self.field_name(f).to_owned();
            if self.is_extern_field(f) || (statics && fname == "__meta__") {
                continue;
            }
            let fl = self.field_flags(f);
            let is_method = fl & field_flag::METHOD != 0;
            let dynamic = fl & field_flag::DYNAMIC != 0;
            let overriding = fl & field_flag::OVERRIDE != 0;
            let fid: Option<i32> = if is_method && !dynamic && !statics {
                let g = self.alloc_fun_path(&name, &fname);
                let virt = if overriding {
                    let (idx, _) = self
                        .get_index(obj, &fname)
                        .ok_or_else(|| format!("{name}.{fname} overrides nothing"))?;
                    let vid = (-idx - 1) as usize;
                    virtuals[vid] = g;
                    Some(vid as u32)
                } else if self.is_overridden(c, &fname) {
                    let vid = virtuals.len();
                    virtuals.push(g);
                    self.table.objs[obj]
                        .index
                        .insert(fname.clone(), (-(vid as i32) - 1, void));
                    Some(vid as u32)
                } else {
                    None
                };
                pa.push(Proto {
                    name: fname.clone(),
                    findex: g,
                    vslot: virt,
                });
                None
            } else if is_method && dynamic && overriding {
                Some(
                    self.get_index(obj, &fname)
                        .ok_or_else(|| format!("{name}.{fname} overrides nothing"))?
                        .0,
                )
            } else {
                let fid = fa.len() as i32 + start_field;
                self.table.objs[obj]
                    .index
                    .insert(fname.clone(), (fid, void));
                fa.push((fname.clone(), void));
                let ty = self.tast.fields()[f as usize].ty.get();
                let packed = self.has_meta(meta_owner::FIELD, f, ":packed");
                self.delayed.push(Delayed::Field {
                    obj,
                    slot: fa.len() - 1,
                    name: fname.clone(),
                    fid,
                    ty,
                    packed,
                });
                Some(fid)
            };
            let has_body = self.tast.fields()[f as usize].func.get() != NONE;
            if let (true, true, Some(fid)) = (is_method, has_body, fid) {
                let g = self.alloc_fun_path(&name, &fname);
                bindings.push((fid as u32, g));
            }
        }

        if !statics {
            for iface in self.classes[c as usize].ifaces.clone() {
                let fid = fa.len() as i32 + start_field;
                self.table.objs[obj]
                    .index
                    .insert(String::new(), (fid, void));
                fa.push((String::new(), void));
                self.delayed.push(Delayed::Interface {
                    obj,
                    slot: fa.len() - 1,
                    fid,
                    ty: iface,
                });
            }
            // a class that has a `toString` and no `__string` gets the one the runtime calls
            let to_string = self.classes[c as usize]
                .instance
                .iter()
                .copied()
                .find(|f| self.field_name(*f) == "toString");
            if let Some(cf) = to_string {
                let has_string = self.classes[c as usize]
                    .instance
                    .iter()
                    .any(|f| self.field_name(*f) == "__string");
                let ty = self.tast.fields()[cf as usize].ty.get();
                if self.field_flags(cf) & field_flag::OVERRIDE == 0
                    && !has_string
                    && self.is_to_string(ty)
                {
                    let g = self.alloc_fun_path(&name, "__string");
                    pa.push(Proto {
                        name: "__string".into(),
                        findex: g,
                        vslot: None,
                    });
                }
            }
        } else if let Some(ctor) = self.classes[c as usize].ctor
            && !self.is_extern_field(ctor)
        {
            let (slot, _) = self
                .get_index(obj, "__constructor__")
                .ok_or("the Class object has no __constructor__")?;
            let g = self.alloc_fun_path(&name, "new");
            bindings.push((slot as u32, g));
        }

        let o = &mut self.table.objs[obj];
        o.nfields = fa.len() as i32 + start_field;
        o.fields = fa;
        o.proto = pa;
        // the generator conses each binding on, so the last one found comes first
        bindings.reverse();
        o.bindings = bindings;
        o.virtuals = virtuals;
        self.depth -= 1;
        if self.depth == 0 {
            self.run_delayed()?;
        }
        let core_type = self.by_name.get("hl.CoreType").copied();
        let core_enum = self.by_name.get("hl.CoreEnum").copied();
        if !statics && Some(c) != core_type && Some(c) != core_enum {
            let (g, _) = self.class_global(c, true)?;
            self.table.objs[obj].global = Some(g);
        }
        Ok(t)
    }

    fn run_delayed(&mut self) -> Result<()> {
        while !self.delayed.is_empty() {
            let todo = std::mem::take(&mut self.delayed);
            for d in todo {
                match d {
                    Delayed::Field {
                        obj,
                        slot,
                        name,
                        fid,
                        ty,
                        packed,
                    } => {
                        let mut t = self.ty(ty, None)?;
                        if packed {
                            if !matches!(self.table.types[t], HlType::Struct(_)) {
                                return Err(format!("{name}: a packed field must be a struct"));
                            }
                            t = self.table.add(HlType::Packed(t));
                        }
                        self.table.objs[obj].fields[slot] = (name.clone(), t);
                        self.table.objs[obj].index.insert(name, (fid, t));
                    }
                    Delayed::Interface { obj, slot, fid, ty } => {
                        let t = self.ty(ty, None)?;
                        self.table.objs[obj].interfaces.push((t, fid as u32));
                        self.table.objs[obj].fields[slot] = (String::new(), t);
                        self.table.objs[obj].index.insert(String::new(), (fid, t));
                    }
                }
            }
        }
        Ok(())
    }

    fn is_to_string(&self, ty: u32) -> bool {
        let t = self.type_rec(ty);
        if t.kind != type_kind::FUN || t.args_count.get() != 0 {
            return false;
        }
        let ret = self.type_rec(t.def.get());
        ret.kind == type_kind::CLASS
            && ret.args_count.get() == 0
            && self.classes[ret.def.get() as usize].name == "String"
    }

    /// The global that holds a class object, and the object's type.
    pub fn class_global(&mut self, c: u32, resolve: bool) -> Result<(u32, TypeIx)> {
        let base_class = self.class_named("hl.Class")?;
        let statics = c != base_class;
        let mut c = c;
        if resolve && ARRAY_IMPLS.contains(&self.classes[c as usize].name.as_str()) {
            c = self.class_named("hl.types.ArrayBase")?;
        }
        let c = self.resolve_class(c, &[], statics)?;
        let t = self.class_type(c, &[], statics, None)?;
        let name = format!("${}", self.classes[c as usize].name);
        Ok((self.alloc_global(&name, t), t))
    }

    // ---- enums

    pub fn enum_type(&mut self, e: u32, tref: Option<usize>) -> Result<TypeIx> {
        let name = self
            .tast
            .opt_string(self.tast.enums()[e as usize].name)
            .unwrap_or("")
            .to_owned();
        if let Some(t) = self.cached.get(&name) {
            return Ok(*t);
        }
        let ix = self.table.enums.len();
        self.table.enums.push(EnumType {
            name: name.clone(),
            ..EnumType::default()
        });
        let t = self.table.push_raw(HlType::Enum(ix));
        if let Some(r) = tref {
            self.rec[r].1 = Some(t);
        }
        self.cached.insert(name, t);
        let rec = self.tast.enums()[e as usize];
        let first = rec.ctors_first.get() as usize;
        let mut fields = Vec::new();
        for ctor in &self.tast.enum_ctors()[first..first + rec.ctors_count.get() as usize] {
            let cname = self.tast.string(ctor.name.get()).to_owned();
            let args: Vec<u32> = self
                .tast
                .type_args(ctor.args_first, ctor.args_count)
                .iter()
                .map(|a| a.get())
                .collect();
            let mut tys = Vec::with_capacity(args.len());
            for a in args {
                tys.push(self.ty(a, None)?);
            }
            fields.push((cname, tys));
        }
        self.table.enums[ix].fields = fields;
        let class = self.enum_class(e)?;
        let class_name = match self.table.types[class] {
            HlType::Obj(o) => self.table.objs[o].name.clone(),
            _ => return Err("an enum's class is not an object".into()),
        };
        let g = self.alloc_global(&class_name, class);
        self.table.enums[ix].global = Some(g);
        Ok(t)
    }

    /// The object that holds an enum's static data.
    pub fn enum_class(&mut self, e: u32) -> Result<TypeIx> {
        let name = self
            .tast
            .opt_string(self.tast.enums()[e as usize].name)
            .unwrap_or("")
            .to_owned();
        let key = statics_name(&name);
        if let Some(t) = self.cached.get(&key) {
            return Ok(*t);
        }
        let obj = self.table.objs.len();
        self.table.objs.push(ObjType {
            name: key.clone(),
            nfields: -1,
            ..ObjType::default()
        });
        let t = self.table.push_raw(HlType::Obj(obj));
        self.cached.insert(key, t);
        let base_enum = self.class_named("hl.Enum")?;
        let sup = self.class_type(base_enum, &[], false, None)?;
        self.table.objs[obj].super_ = Some(sup);
        Ok(t)
    }

    // ---- the program

    /// Lays out every class and enum the program defines, the way the start-up function
    /// registers them, so each has its object, its statics and its global.
    pub fn build_program(&mut self) -> Result<()> {
        let order: Vec<(u32, u32)> = self
            .tast
            .order()
            .iter()
            .map(|o| (o.kind.get(), o.id.get()))
            .collect();
        for (kind, id) in order {
            if kind == u32::from(module_kind::CLASS) {
                if self.is_extern(id) {
                    continue;
                }
                let name = self.classes[id as usize].name.clone();
                if name == "Array" {
                    continue;
                }
                self.class_global(id, false)?;
                self.class_type(id, &[], false, None)?;
            } else if kind == u32::from(module_kind::ENUM) {
                self.enum_type(id, None)?;
            }
        }
        Ok(())
    }

    /// The class named `name`, if the program has one.
    pub fn class_id(&self, name: &str) -> Option<u32> {
        self.by_name.get(name).copied()
    }

    pub fn tast(&self) -> Tast<'a> {
        self.tast
    }

    pub fn class_name(&self, c: u32) -> &str {
        &self.classes[c as usize].name
    }

    /// The function index a call to `class.name` names, allocated on first use. The definition
    /// of the function and every call to it ask the same way, so they agree.
    pub fn fun_id(&mut self, class: &str, name: &str) -> u32 {
        self.alloc_fun_path(class, name)
    }
}
