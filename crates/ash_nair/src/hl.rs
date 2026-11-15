//! HashLink's type table: the types a program uses and the layouts of its objects.
//!
//! Types refer to each other by index, so a cycle (a class with a field of its own type) needs
//! no special case. An object, enum or structure type is a definition in its own arena and a
//! `TypeIx` names it.

use std::collections::HashMap;

pub type TypeIx = usize;
pub type ObjIx = usize;
pub type EnumIx = usize;
pub type VirtIx = usize;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum HlType {
    Void,
    UI8,
    UI16,
    I32,
    I64,
    F32,
    F64,
    Bool,
    Bytes,
    Dyn,
    Array,
    Type,
    DynObj,
    Fun(Vec<TypeIx>, TypeIx),
    /// A method pointer: a function whose first argument is its object.
    Method(Vec<TypeIx>, TypeIx),
    Obj(ObjIx),
    Struct(ObjIx),
    Enum(EnumIx),
    Virtual(VirtIx),
    Ref(TypeIx),
    Null(TypeIx),
    Packed(TypeIx),
    Abstract(String),
}

/// One entry of an object's method table.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Proto {
    pub name: String,
    pub findex: u32,
    /// The method's virtual slot, when a subclass may override it.
    pub vslot: Option<u32>,
}

/// A class's runtime layout. `fields` and `proto` are the class's own; a subclass's field
/// indexes start after `nfields` of its superclass.
#[derive(Clone, Debug, Default)]
pub struct ObjType {
    pub name: String,
    pub is_struct: bool,
    pub super_: Option<TypeIx>,
    /// The global that holds the class object, when it has one.
    pub global: Option<u32>,
    pub fields: Vec<(String, TypeIx)>,
    pub proto: Vec<Proto>,
    /// `(field index, function index)`: a function stored into a field of the class object.
    pub bindings: Vec<(u32, u32)>,
    /// Fields including the superclass's. Not part of the file; the runtime recomputes it.
    pub nfields: i32,
    /// Function index per virtual slot, including the superclass's. Not part of the file.
    pub virtuals: Vec<u32>,
    /// Field or method name to `(index, type)`; a virtual method is `-slot - 1`. Not part of the file.
    pub index: HashMap<String, (i32, TypeIx)>,
    /// The field that holds each implemented interface, by interface type. Not part of the file.
    pub interfaces: Vec<(TypeIx, u32)>,
}

#[derive(Clone, Debug, Default)]
pub struct EnumType {
    pub name: String,
    pub global: Option<u32>,
    pub fields: Vec<(String, Vec<TypeIx>)>,
}

#[derive(Default)]
pub struct TypeTable {
    pub types: Vec<HlType>,
    pub objs: Vec<ObjType>,
    pub enums: Vec<EnumType>,
    pub virts: Vec<Vec<(String, TypeIx)>>,
    pub globals: Vec<(String, TypeIx)>,
    intern: HashMap<HlType, TypeIx>,
}

impl TypeTable {
    /// The index of `t`, adding it on first use. Objects, enums and structures are added once
    /// by whoever defines them, so two of them never share an index by accident.
    pub fn add(&mut self, t: HlType) -> TypeIx {
        if let Some(ix) = self.intern.get(&t) {
            return *ix;
        }
        let ix = self.types.len();
        self.types.push(t.clone());
        self.intern.insert(t, ix);
        ix
    }

    /// The index `t` was added under, if it was.
    pub fn lookup(&self, t: &HlType) -> Option<TypeIx> {
        self.intern.get(t).copied()
    }

    /// Adds `t` as its own entry, as a file lists its types: no sharing.
    pub fn push_raw(&mut self, t: HlType) -> TypeIx {
        self.types.push(t);
        self.types.len() - 1
    }

    pub fn is_nullable(&self, ix: TypeIx) -> bool {
        use HlType::*;
        match &self.types[ix] {
            Bytes | Dyn | Fun(..) | Obj(_) | Array | Virtual(_) | DynObj | Abstract(_)
            | Enum(_) | Null(_) | Ref(_) | Type | Method(..) | Struct(_) => true,
            UI8 | UI16 | I32 | I64 | F32 | F64 | Bool | Void | Packed(_) => false,
        }
    }

    /// The type as text, with objects and enums by name. Two tables that mean the same type
    /// print it the same way, whatever their indexes are.
    pub fn describe(&self, ix: TypeIx) -> String {
        let mut stack = Vec::new();
        self.describe_in(ix, &mut stack)
    }

    fn describe_in(&self, ix: TypeIx, stack: &mut Vec<VirtIx>) -> String {
        use HlType::*;
        let list = |tys: &[TypeIx], stack: &mut Vec<VirtIx>| {
            tys.iter()
                .map(|t| self.describe_in(*t, stack))
                .collect::<Vec<_>>()
                .join(",")
        };
        match &self.types[ix] {
            Void => "void".into(),
            UI8 => "ui8".into(),
            UI16 => "ui16".into(),
            I32 => "i32".into(),
            I64 => "i64".into(),
            F32 => "f32".into(),
            F64 => "f64".into(),
            Bool => "bool".into(),
            Bytes => "bytes".into(),
            Dyn => "dyn".into(),
            Array => "array".into(),
            Type => "type".into(),
            DynObj => "dynobj".into(),
            Fun(args, ret) => format!(
                "fun({})->{}",
                list(args, stack),
                self.describe_in(*ret, stack)
            ),
            Method(args, ret) => {
                format!(
                    "method({})->{}",
                    list(args, stack),
                    self.describe_in(*ret, stack)
                )
            }
            Obj(o) => format!("obj:{}", self.objs[*o].name),
            Struct(o) => format!("struct:{}", self.objs[*o].name),
            Enum(e) => format!("enum:{}", self.enums[*e].name),
            Virtual(v) => {
                if stack.contains(v) {
                    return "{..}".into();
                }
                stack.push(*v);
                let fields = self.virts[*v]
                    .iter()
                    .map(|(n, t)| format!("{n}:{}", self.describe_in(*t, stack)))
                    .collect::<Vec<_>>()
                    .join(",");
                stack.pop();
                format!("{{{fields}}}")
            }
            Ref(t) => format!("ref<{}>", self.describe_in(*t, stack)),
            Null(t) => format!("null<{}>", self.describe_in(*t, stack)),
            Packed(t) => format!("packed<{}>", self.describe_in(*t, stack)),
            Abstract(n) => format!("abstract:{n}"),
        }
    }

    /// The object named `name`, if the table has one.
    pub fn obj_named(&self, name: &str) -> Option<ObjIx> {
        self.objs.iter().position(|o| o.name == name)
    }
}
