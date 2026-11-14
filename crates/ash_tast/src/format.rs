//! The TAST wire format.
//!
//! A file is a [`Header`], a table of [`SectionRec`]s, and the sections. Every
//! section is an array of fixed-size records (the string blob is bytes), and
//! every cross-reference is an index into another section. [`NONE`] means "no
//! reference". All words are little-endian `u32` unless a field says
//! otherwise, and no record needs alignment, so a mapped file is read in place.
//!
//! # Layers
//!
//! A file can be a layer of a stack: the standard library, then a library, then
//! the program. A layer's ids are plain indexes into its own tables. A reference
//! to something in a lower layer is a tagged id (see [`tag`]), so a layer is
//! written without knowing what is below it beyond a key index, and a lower layer
//! is built once and read in place by every program above it. A layer lists the
//! layers below it in `Parents`, bottom first, by identity. `Keys` is the index a
//! layer above uses to find what is in this one by name, and `Kept` lists what the
//! compiler's dead-code removal kept from the layers below.
//!
//! # Node trees
//!
//! A function body is a tree of [`NodeRec`]s in the `Nodes` section. Nodes are
//! written breadth-first: the children of a node are consecutive, and
//! `first_kid..first_kid + kid_count` indexes `Nodes`. A node's children come
//! after it (`first_kid > its own index`), which the reader enforces, so no
//! walk can loop. `kind` is the constructor index of Haxe's `TypedExprDef`
//! for `0..=28`, and a pseudo-kind from 32 up for the parts of a node that
//! `TypedExprDef` keeps in anonymous structures.
//!
//! What each kind puts in `flags`, `a`, `b` and its children:
//!
//! | kind | flags | a | b | children |
//! |---|---|---|---|---|
//! | `CONST` | [`const_kind`] | `Int`: the value; `Float`, `String`: a string id; `Bool`: 0 or 1 | | |
//! | `LOCAL` | | local id | | |
//! | `ARRAY` | | | | array, index |
//! | `BINOP` | | `Binop` constructor index | for `OpAssignOp`, the inner operator's index | left, right |
//! | `FIELD` | | [`field_access`] | instance, static, closure: field id; anon, dynamic: string id; enum: enum constructor id | the object |
//! | `TYPE_EXPR` | [`module_kind`] | index into the table `flags` names | | |
//! | `PARENTHESIS` | | | | the expression |
//! | `OBJECT_DECL` | | | | `OBJECT_FIELD`s |
//! | `ARRAY_DECL` | | | | the elements |
//! | `CALL` | | | | callee, arguments |
//! | `NEW` | | class id | | arguments |
//! | `UNOP` | `POSTFIX` | `Unop` constructor index | | the operand |
//! | `FUNCTION` | | function id | | |
//! | `VAR` | `HAS_INIT` | local id | | the initializer, when `HAS_INIT` |
//! | `BLOCK` | | | | the statements |
//! | `FOR` | | local id | | iterable, body |
//! | `IF` | `HAS_ELSE` | | | condition, then, else when `HAS_ELSE` |
//! | `WHILE` | `DO_WHILE` | | | condition, body |
//! | `SWITCH` | `HAS_DEFAULT` | | | subject, `CASE`s, then the `DEFAULT` when `HAS_DEFAULT` |
//! | `TRY` | | | | body, `CATCH`es |
//! | `RETURN` | `HAS_VALUE` | | | the value, when `HAS_VALUE` |
//! | `BREAK`, `CONTINUE` | | | | |
//! | `THROW` | | | | the expression |
//! | `CAST` | `HAS_TYPE`, and [`module_kind`] shifted left by 1 | index into the table the kind names | | the expression |
//! | `META` | | string id of the name | | the expression, then the metadata arguments |
//! | `ENUM_PARAMETER` | | enum constructor id | the parameter index | the expression |
//! | `ENUM_INDEX` | | | | the expression |
//! | `IDENT` | | string id | | |
//! | `CASE` | | number of values | | the values, then the body |
//! | `CATCH` | | local id | | the body |
//! | `OBJECT_FIELD` | | string id of the name | | the value |
//! | `DEFAULT` | | | | the body |
//!
//! Every node also carries `ty` (a type id, [`NONE`] when unknown) and `pos`
//! (a position id, [`NONE`] when unknown).

use zerocopy::little_endian::{U16, U32};
use zerocopy::{FromBytes, Immutable, IntoBytes, KnownLayout, Unaligned};

/// The first word of every file: the bytes `TAST`.
pub const MAGIC: u32 = u32::from_le_bytes(*b"TAST");
/// The layout version. Any change to a record or a section bumps it.
pub const VERSION: u32 = 1;
/// A reference to nothing.
pub const NONE: u32 = u32::MAX;

/// The wire name of a field type, for the generators.
pub trait Wire {
    const NAME: &'static str;
}

impl Wire for u8 {
    const NAME: &'static str = "u8";
}
impl Wire for U16 {
    const NAME: &'static str = "u16";
}
impl Wire for U32 {
    const NAME: &'static str = "u32";
}

/// One field of a record: its name, wire type and byte offset.
#[derive(Debug, Clone, Copy)]
pub struct FieldDesc {
    pub name: &'static str,
    pub ty: &'static str,
    pub offset: usize,
}

/// A record's name, size and fields, read from the struct that defines it.
#[derive(Debug, Clone, Copy)]
pub struct RecordDesc {
    pub name: &'static str,
    pub size: usize,
    pub fields: &'static [FieldDesc],
}

/// Defines a `#[repr(C)]` record and its [`RecordDesc`] together, so the
/// struct and what the generators read cannot differ.
macro_rules! record {
    ($(#[$meta:meta])* $name:ident { $($(#[$fmeta:meta])* $field:ident : $ty:ty),+ $(,)? }) => {
        $(#[$meta])*
        #[repr(C)]
        #[derive(Debug, Clone, Copy, FromBytes, IntoBytes, KnownLayout, Immutable, Unaligned)]
        pub struct $name {
            $($(#[$fmeta])* pub $field: $ty),+
        }

        impl $name {
            pub const SIZE: usize = core::mem::size_of::<$name>();
            pub const DESC: RecordDesc = RecordDesc {
                name: stringify!($name),
                size: core::mem::size_of::<$name>(),
                fields: &[$(FieldDesc {
                    name: stringify!($field),
                    ty: <$ty as Wire>::NAME,
                    offset: core::mem::offset_of!($name, $field),
                }),+],
            };
        }
    };
}

record! {
    /// The start of the file.
    Header {
        /// [`MAGIC`].
        magic: U32,
        /// [`VERSION`].
        version: U32,
        /// Low word of [`schema_hash`](crate::format::schema_hash).
        schema_hash_lo: U32,
        /// High word of the schema hash.
        schema_hash_hi: U32,
        /// The Haxe compiler version, `major * 10000 + minor * 100 + patch`.
        haxe_version: U32,
        /// Low word of the layer's identity: what a layer above it names in `Parents`.
        layer_id_lo: U32,
        /// High word of the layer's identity.
        layer_id_hi: U32,
        /// Reserved; zero.
        flags: U32,
        /// The program's entry function id, or [`NONE`].
        entry: U32,
        /// How many [`SectionRec`]s follow the header.
        section_count: U32,
    }
}

record! {
    /// Where a section is: its byte offset in the file and its record count.
    SectionRec {
        offset: U32,
        count: U32,
    }
}

record! {
    /// A string: a byte range of the `Strings` blob, always valid UTF-8.
    StrRec {
        offset: U32,
        len: U32,
    }
}

record! {
    /// A source position: a file name and a byte range in it.
    PosRec {
        file: U32,
        start: U32,
        end: U32,
    }
}

record! {
    /// A type, as Haxe's `Type` after typedefs are followed.
    ///
    /// | kind | `def` | `args_first..+args_count` |
    /// |---|---|---|
    /// | `ENUM` | enum id | type arguments |
    /// | `CLASS` | class id | type arguments |
    /// | `ABSTRACT` | abstract id | type arguments |
    /// | `FUN` | return type id | argument types |
    /// | `ANON` | first anonymous field id | `args_count` is the field count |
    /// | `MONO`, `DYNAMIC` | [`NONE`] | none |
    TypeRec {
        kind: u8,
        flags: u8,
        _pad: U16,
        /// A string id, or [`NONE`].
        name: U32,
        def: U32,
        args_first: U32,
        args_count: U32,
    }
}

record! {
    /// A class or interface.
    ClassRec {
        name: U32,
        /// The superclass as a type id (with its arguments), or [`NONE`].
        super_type: U32,
        /// [`class_flag`] bits.
        flags: U32,
        /// The class's fields, instance then static, are `fields_first..+fields_count` in `Fields`.
        fields_first: U32,
        fields_count: U32,
        /// Implemented interfaces as type ids, in `TypeArgs`.
        interfaces_first: U32,
        interfaces_count: U32,
    }
}

record! {
    /// A field of a class.
    FieldRec {
        name: U32,
        ty: U32,
        /// The owning class id.
        owner: U32,
        /// [`field_flag`] bits.
        flags: U32,
        /// The body as a function id, or [`NONE`].
        func: U32,
        /// A position id, or [`NONE`].
        pos: U32,
    }
}

record! {
    /// An abstract type.
    AbstractRec {
        name: U32,
        /// The underlying type id, or [`NONE`].
        underlying: U32,
        /// The implementation class id, or [`NONE`].
        impl_class: U32,
        flags: U32,
    }
}

record! {
    /// An enum.
    EnumRec {
        name: U32,
        /// Constructors are `ctors_first..+ctors_count` in `EnumCtors`.
        ctors_first: U32,
        ctors_count: U32,
        flags: U32,
    }
}

record! {
    /// An enum constructor.
    EnumCtorRec {
        name: U32,
        enum_id: U32,
        /// The constructor's index in its enum.
        index: U32,
        /// The constructor's type id.
        ty: U32,
        /// The parameter types, in `TypeArgs`.
        args_first: U32,
        args_count: U32,
    }
}

record! {
    /// A field of an anonymous structure type.
    AnonFieldRec {
        name: U32,
        ty: U32,
        flags: U32,
    }
}

record! {
    /// A local variable or function parameter.
    LocalRec {
        name: U32,
        ty: U32,
        /// [`local_flag`] bits.
        flags: U32,
    }
}

record! {
    /// A function parameter and its default value.
    ParamRec {
        local: U32,
        /// The default's root node id, or [`NONE`].
        default_value: U32,
    }
}

record! {
    /// A function: a method body or a closure.
    FuncRec {
        ret: U32,
        /// Parameters are `params_first..+params_count` in `Params`.
        params_first: U32,
        params_count: U32,
        /// The body's root node id, or [`NONE`] for an extern.
        root: U32,
        flags: U32,
        /// A position id, or [`NONE`].
        pos: U32,
    }
}

record! {
    /// One node of an expression tree. See the module documentation.
    NodeRec {
        kind: u8,
        flags: u8,
        _pad: U16,
        ty: U32,
        pos: U32,
        a: U32,
        b: U32,
        first_kid: U32,
        kid_count: U32,
    }
}

record! {
    /// One entry of a layer's key index: how the exporter found a type, class, field
    /// or enum by name, so a program built on this layer finds it again.
    KeyRec {
        /// A [`key_kind`].
        kind: U32,
        /// A string id: the key's text.
        key: U32,
        /// The entry's id in this layer, or 0 for a kind with no id.
        value: U32,
    }
}

record! {
    /// An entry the compiler's dead-code removal kept in a lower layer.
    KeptRec {
        /// A [`kept_kind`].
        kind: U32,
        /// A tagged id of the kept class, field, enum or abstract.
        id: U32,
    }
}

record! {
    /// A lower layer this one was built on: its identity, in stacking order.
    ParentRec {
        id_lo: U32,
        id_hi: U32,
    }
}

/// A section's name and element size.
#[derive(Debug, Clone, Copy)]
pub struct SectionDesc {
    pub name: &'static str,
    pub elem: usize,
}

/// Section indexes, in file order. A file's section table is indexed by these.
pub mod section {
    pub const STRINGS: usize = 0;
    pub const STRING_INDEX: usize = 1;
    pub const POSITIONS: usize = 2;
    pub const TYPES: usize = 3;
    pub const TYPE_ARGS: usize = 4;
    pub const CLASSES: usize = 5;
    pub const FIELDS: usize = 6;
    pub const ABSTRACTS: usize = 7;
    pub const ENUMS: usize = 8;
    pub const ENUM_CTORS: usize = 9;
    pub const ANON_FIELDS: usize = 10;
    pub const LOCALS: usize = 11;
    pub const PARAMS: usize = 12;
    pub const FUNCS: usize = 13;
    pub const NODES: usize = 14;
    pub const KEYS: usize = 15;
    pub const KEPT: usize = 16;
    pub const PARENTS: usize = 17;
}

/// Every section, in [`section`] order, with its element size.
pub const SECTIONS: &[SectionDesc] = &[
    SectionDesc {
        name: "Strings",
        elem: 1,
    },
    SectionDesc {
        name: "StringIndex",
        elem: StrRec::SIZE,
    },
    SectionDesc {
        name: "Positions",
        elem: PosRec::SIZE,
    },
    SectionDesc {
        name: "Types",
        elem: TypeRec::SIZE,
    },
    SectionDesc {
        name: "TypeArgs",
        elem: 4,
    },
    SectionDesc {
        name: "Classes",
        elem: ClassRec::SIZE,
    },
    SectionDesc {
        name: "Fields",
        elem: FieldRec::SIZE,
    },
    SectionDesc {
        name: "Abstracts",
        elem: AbstractRec::SIZE,
    },
    SectionDesc {
        name: "Enums",
        elem: EnumRec::SIZE,
    },
    SectionDesc {
        name: "EnumCtors",
        elem: EnumCtorRec::SIZE,
    },
    SectionDesc {
        name: "AnonFields",
        elem: AnonFieldRec::SIZE,
    },
    SectionDesc {
        name: "Locals",
        elem: LocalRec::SIZE,
    },
    SectionDesc {
        name: "Params",
        elem: ParamRec::SIZE,
    },
    SectionDesc {
        name: "Funcs",
        elem: FuncRec::SIZE,
    },
    SectionDesc {
        name: "Nodes",
        elem: NodeRec::SIZE,
    },
    SectionDesc {
        name: "Keys",
        elem: KeyRec::SIZE,
    },
    SectionDesc {
        name: "Kept",
        elem: KeptRec::SIZE,
    },
    SectionDesc {
        name: "Parents",
        elem: ParentRec::SIZE,
    },
];

/// Every record, for the generators and the schema hash.
pub const RECORDS: &[&RecordDesc] = &[
    &Header::DESC,
    &SectionRec::DESC,
    &StrRec::DESC,
    &PosRec::DESC,
    &TypeRec::DESC,
    &ClassRec::DESC,
    &FieldRec::DESC,
    &AbstractRec::DESC,
    &EnumRec::DESC,
    &EnumCtorRec::DESC,
    &AnonFieldRec::DESC,
    &LocalRec::DESC,
    &ParamRec::DESC,
    &FuncRec::DESC,
    &NodeRec::DESC,
    &KeyRec::DESC,
    &KeptRec::DESC,
    &ParentRec::DESC,
];

/// A group of named byte constants, as an enum for the generators.
macro_rules! constants {
    ($(#[$meta:meta])* $group:ident, $table:ident, $ty:ty { $($(#[$cmeta:meta])* $name:ident = $value:expr),+ $(,)? }) => {
        $(#[$meta])*
        pub mod $group {
            $($(#[$cmeta])* pub const $name: $ty = $value;)+
        }
        /// The group's names and values, in order.
        pub const $table: &[(&str, $ty)] = &[$((stringify!($name), $value)),+];
    };
}

constants! {
    /// `NodeRec::kind`. `0..=28` are the constructors of Haxe's `TypedExprDef`
    /// in declaration order; the rest are parts it keeps inside other nodes.
    node_kind, NODE_KINDS, u8 {
        CONST = 0,
        LOCAL = 1,
        ARRAY = 2,
        BINOP = 3,
        FIELD = 4,
        TYPE_EXPR = 5,
        PARENTHESIS = 6,
        OBJECT_DECL = 7,
        ARRAY_DECL = 8,
        CALL = 9,
        NEW = 10,
        UNOP = 11,
        FUNCTION = 12,
        VAR = 13,
        BLOCK = 14,
        FOR = 15,
        IF = 16,
        WHILE = 17,
        SWITCH = 18,
        TRY = 19,
        RETURN = 20,
        BREAK = 21,
        CONTINUE = 22,
        THROW = 23,
        CAST = 24,
        META = 25,
        ENUM_PARAMETER = 26,
        ENUM_INDEX = 27,
        IDENT = 28,
        CASE = 32,
        CATCH = 33,
        OBJECT_FIELD = 34,
        DEFAULT = 35,
    }
}

/// The `TypedExprDef` constructors, by node kind `0..=28`. The Haxe side checks
/// these against the compiler it runs in.
pub const TYPED_EXPR_DEF: [&str; 29] = [
    "TConst",
    "TLocal",
    "TArray",
    "TBinop",
    "TField",
    "TTypeExpr",
    "TParenthesis",
    "TObjectDecl",
    "TArrayDecl",
    "TCall",
    "TNew",
    "TUnop",
    "TFunction",
    "TVar",
    "TBlock",
    "TFor",
    "TIf",
    "TWhile",
    "TSwitch",
    "TTry",
    "TReturn",
    "TBreak",
    "TContinue",
    "TThrow",
    "TCast",
    "TMeta",
    "TEnumParameter",
    "TEnumIndex",
    "TIdent",
];

constants! {
    /// `NodeRec::flags`, per kind.
    node_flag, NODE_FLAGS, u8 {
        /// `IF`: there is an else branch.
        HAS_ELSE = 1,
        /// `RETURN`: there is a value.
        HAS_VALUE = 1,
        /// `VAR`: there is an initializer.
        HAS_INIT = 1,
        /// `UNOP`: the operator follows its operand.
        POSTFIX = 1,
        /// `WHILE`: the body runs before the first test.
        DO_WHILE = 1,
        /// `SWITCH`: the last child is the `DEFAULT`.
        HAS_DEFAULT = 1,
        /// `CAST`: a module type is named.
        HAS_TYPE = 1,
    }
}

constants! {
    /// `CONST` nodes' `flags`.
    const_kind, CONST_KINDS, u8 {
        INT = 0,
        FLOAT = 1,
        STRING = 2,
        BOOL = 3,
        NULL = 4,
        THIS = 5,
        SUPER = 6,
    }
}

constants! {
    /// `FIELD` nodes' `a`.
    field_access, FIELD_ACCESSES, u32 {
        INSTANCE = 0,
        STATIC = 1,
        CLOSURE = 2,
        ANON = 3,
        DYNAMIC = 4,
        ENUM = 5,
    }
}

constants! {
    /// Which table a module-type index names.
    module_kind, MODULE_KINDS, u8 {
        CLASS = 0,
        ENUM = 1,
        ABSTRACT = 2,
    }
}

constants! {
    /// `TypeRec::kind`, the constructors of Haxe's `Type` that survive typing.
    type_kind, TYPE_KINDS, u8 {
        MONO = 0,
        ENUM = 1,
        CLASS = 2,
        FUN = 3,
        ANON = 4,
        DYNAMIC = 5,
        ABSTRACT = 6,
    }
}

constants! {
    /// `ClassRec::flags`.
    class_flag, CLASS_FLAGS, u32 {
        INTERFACE = 1,
        FINAL = 2,
        EXTERN = 4,
        ABSTRACT = 8,
        TYPE_PARAM = 16,
    }
}

constants! {
    /// `FieldRec::flags`.
    field_flag, FIELD_FLAGS, u32 {
        STATIC = 1,
        METHOD = 2,
        FINAL = 4,
        INLINE = 8,
        PUBLIC = 16,
        OVERRIDE = 32,
    }
}

constants! {
    /// `LocalRec::flags`.
    local_flag, LOCAL_FLAGS, u32 {
        /// A closure captures it.
        CAPTURED = 1,
        /// It is a function parameter.
        ARGUMENT = 2,
    }
}

constants! {
    /// `KeyRec::kind`: which of the exporter's maps an entry belongs to.
    key_kind, KEY_KINDS, u32 {
        /// A class type with no arguments, by the class path, to a type id.
        INST_TYPE = 0,
        /// An abstract type with no arguments, by the abstract path, to a type id.
        ABSTRACT_TYPE = 1,
        /// An enum type with no arguments, by the enum path, to a type id.
        ENUM_TYPE = 2,
        /// A typedef with no arguments, by the typedef path, to a type id.
        TYPEDEF_TYPE = 3,
        /// Any other type, by the compiler's text for it, to a type id.
        TEXT_TYPE = 4,
        /// A type by its structure, to a type id.
        STRUCT_TYPE = 5,
        /// A class path to a class id.
        CLASS = 6,
        /// An enum path to an enum id.
        ENUM = 7,
        /// An abstract path to an abstract id.
        ABSTRACT = 8,
        /// A class path, `#i:` or `#s:`, and a field name, to a field id.
        FIELD = 9,
        /// An enum path, a dot and a constructor name, to an enum constructor id.
        ENUM_CTOR = 10,
        /// A source root the layer was built from; `value` is unused.
        ROOT = 11,
        /// What the layer was built from: its defines and its source files.
        DIGEST = 12,
    }
}

constants! {
    /// `KeptRec::kind`.
    kept_kind, KEPT_KINDS, u32 {
        CLASS = 0,
        FIELD = 1,
        ENUM = 2,
        ABSTRACT = 3,
    }
}

/// The ids that name something in a lower layer. A layer's own ids are plain
/// indexes into its own tables. A reference into a lower layer sets the top bit
/// and carries the layer's position among the parents and the index in its table:
/// `0x8000_0000 | layer << 24 | index`. Only ids of types, classes, fields,
/// abstracts, enums and enum constructors can be tagged.
pub mod tag {
    use super::NONE;

    pub const BIT: u32 = 0x8000_0000;
    /// The most layers a stack can have below the top one.
    pub const MAX_LAYERS: u32 = 126;
    /// The most entries a table of a layer can have and still be named from above.
    pub const MAX_INDEX: u32 = (1 << 24) - 1;

    /// The id of entry `index` of the table of the parent at position `layer`.
    pub fn tagged(layer: u32, index: u32) -> u32 {
        debug_assert!(layer < MAX_LAYERS && index <= MAX_INDEX);
        BIT | (layer << 24) | index
    }

    /// `(layer, index)` when `id` names an entry of a parent.
    pub fn split(id: u32) -> Option<(u32, u32)> {
        (id != NONE && id & BIT != 0).then_some(((id >> 24) & 0x7f, id & MAX_INDEX))
    }
}

/// `Binop` constructors of `haxe.macro.Expr`, by the index `BINOP` nodes carry.
pub const BINOPS: [&str; 25] = [
    "OpAdd",
    "OpMult",
    "OpDiv",
    "OpSub",
    "OpAssign",
    "OpEq",
    "OpNotEq",
    "OpGt",
    "OpGte",
    "OpLt",
    "OpLte",
    "OpAnd",
    "OpOr",
    "OpXor",
    "OpBoolAnd",
    "OpBoolOr",
    "OpShl",
    "OpShr",
    "OpUShr",
    "OpMod",
    "OpAssignOp",
    "OpInterval",
    "OpArrow",
    "OpIn",
    "OpNullCoal",
];

/// The `BINOP` index of `OpAssignOp`, whose inner operator `b` names.
pub const BINOP_ASSIGN_OP: u32 = 20;

/// `Unop` constructors of `haxe.macro.Expr`, by the index `UNOP` nodes carry.
pub const UNOPS: [&str; 6] = [
    "OpIncrement",
    "OpDecrement",
    "OpNot",
    "OpNeg",
    "OpNegBits",
    "OpSpread",
];

/// A hash of everything a reader and a writer must agree on: the records, the
/// sections and every constant table. The header carries it, and a reader
/// refuses a file whose hash differs. FNV-1a, so it is the same everywhere.
pub fn schema_hash() -> u64 {
    let mut text = String::new();
    for record in RECORDS {
        text.push_str(&format!("{}:{};", record.name, record.size));
        for field in record.fields {
            text.push_str(&format!("{}:{}@{},", field.name, field.ty, field.offset));
        }
    }
    for section in SECTIONS {
        text.push_str(&format!("section {}:{};", section.name, section.elem));
    }
    let mut group = |name: &str, entries: Vec<(&str, u64)>| {
        text.push_str(name);
        for (entry, value) in entries {
            text.push_str(&format!(" {entry}={value}"));
        }
        text.push(';');
    };
    group(
        "node_kind",
        NODE_KINDS.iter().map(|(n, v)| (*n, *v as u64)).collect(),
    );
    group(
        "node_flag",
        NODE_FLAGS.iter().map(|(n, v)| (*n, *v as u64)).collect(),
    );
    group(
        "const_kind",
        CONST_KINDS.iter().map(|(n, v)| (*n, *v as u64)).collect(),
    );
    group(
        "field_access",
        FIELD_ACCESSES
            .iter()
            .map(|(n, v)| (*n, *v as u64))
            .collect(),
    );
    group(
        "module_kind",
        MODULE_KINDS.iter().map(|(n, v)| (*n, *v as u64)).collect(),
    );
    group(
        "type_kind",
        TYPE_KINDS.iter().map(|(n, v)| (*n, *v as u64)).collect(),
    );
    group(
        "class_flag",
        CLASS_FLAGS.iter().map(|(n, v)| (*n, *v as u64)).collect(),
    );
    group(
        "field_flag",
        FIELD_FLAGS.iter().map(|(n, v)| (*n, *v as u64)).collect(),
    );
    group(
        "local_flag",
        LOCAL_FLAGS.iter().map(|(n, v)| (*n, *v as u64)).collect(),
    );
    group(
        "key_kind",
        KEY_KINDS.iter().map(|(n, v)| (*n, *v as u64)).collect(),
    );
    group(
        "kept_kind",
        KEPT_KINDS.iter().map(|(n, v)| (*n, *v as u64)).collect(),
    );
    group(
        "tag",
        vec![
            ("bit", tag::BIT as u64),
            ("max_layers", tag::MAX_LAYERS as u64),
        ],
    );
    group(
        "binops",
        BINOPS
            .iter()
            .enumerate()
            .map(|(i, n)| (*n, i as u64))
            .collect(),
    );
    group(
        "unops",
        UNOPS
            .iter()
            .enumerate()
            .map(|(i, n)| (*n, i as u64))
            .collect(),
    );
    group(
        "typed_expr_def",
        TYPED_EXPR_DEF
            .iter()
            .enumerate()
            .map(|(i, n)| (*n, i as u64))
            .collect(),
    );

    let mut hash = 0xcbf2_9ce4_8422_2325u64;
    for byte in text.bytes() {
        hash ^= byte as u64;
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    hash
}
