//! Reads a TAST file in place.
//!
//! [`Tast::parse`] borrows the bytes, casts each section to a slice of records
//! without copying, and checks every cross-reference once. After it returns,
//! every id in the file is in range, every node's children lie after it, and
//! every node has the number of children its kind needs, so the accessors do
//! not re-check and a walk cannot loop.

use std::fmt;

use zerocopy::little_endian::U32;
use zerocopy::{FromBytes, Immutable, KnownLayout};

use crate::format::*;

/// Why a file was refused.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TastError {
    /// The file ends before its header or section table does.
    TooShort,
    /// The first word is not [`MAGIC`].
    BadMagic(u32),
    /// The file's layout version is not [`VERSION`].
    BadVersion(u32),
    /// The writer and this reader disagree on the layout. Regenerate the
    /// Haxe writer or use the matching ash.
    SchemaMismatch { file: u64, reader: u64 },
    /// The section table has fewer sections than the format has.
    MissingSections { found: u32, need: usize },
    /// A section's bytes are outside the file or not a whole number of records.
    SectionOutOfBounds { section: &'static str },
    /// The string blob is not UTF-8, or a string is not on character boundaries.
    BadString { index: usize },
    /// A field holds an id or range outside the table it names.
    BadReference {
        table: &'static str,
        index: usize,
        field: &'static str,
        value: u32,
        limit: usize,
    },
    /// A node's children do not lie after it.
    KidsNotAfterParent { node: usize },
    /// A node has a kind the format does not define.
    UnknownNodeKind { node: usize, kind: u8 },
    /// A node has the wrong number of children for its kind.
    BadArity {
        node: usize,
        kind: &'static str,
        kids: u32,
    },
}

impl fmt::Display for TastError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TastError::TooShort => write!(f, "the file is too short to be a TAST file"),
            TastError::BadMagic(m) => write!(f, "not a TAST file (magic {m:#010x})"),
            TastError::BadVersion(v) => {
                write!(f, "TAST layout version {v}, this reader reads {VERSION}")
            }
            TastError::SchemaMismatch { file, reader } => write!(
                f,
                "the file's schema hash {file:#018x} differs from the reader's {reader:#018x}; \
                 update the Haxe exporter or ash so they match"
            ),
            TastError::MissingSections { found, need } => {
                write!(
                    f,
                    "the section table has {found} sections, the format needs {need}"
                )
            }
            TastError::SectionOutOfBounds { section } => {
                write!(f, "section {section} lies outside the file")
            }
            TastError::BadString { index } => write!(f, "string {index} is not valid UTF-8"),
            TastError::BadReference {
                table,
                index,
                field,
                value,
                limit,
            } => write!(
                f,
                "{table}[{index}].{field} is {value}, outside its table of {limit}"
            ),
            TastError::KidsNotAfterParent { node } => {
                write!(f, "node {node} has children that do not come after it")
            }
            TastError::UnknownNodeKind { node, kind } => {
                write!(f, "node {node} has unknown kind {kind}")
            }
            TastError::BadArity { node, kind, kids } => {
                write!(
                    f,
                    "node {node} ({kind}) has {kids} children, which its kind does not allow"
                )
            }
        }
    }
}

impl std::error::Error for TastError {}

/// A validated TAST file, borrowed from its bytes.
#[derive(Debug, Clone, Copy)]
pub struct Tast<'a> {
    header: &'a Header,
    blob: &'a str,
    str_index: &'a [StrRec],
    positions: &'a [PosRec],
    types: &'a [TypeRec],
    type_args: &'a [U32],
    classes: &'a [ClassRec],
    fields: &'a [FieldRec],
    abstracts: &'a [AbstractRec],
    enums: &'a [EnumRec],
    enum_ctors: &'a [EnumCtorRec],
    anon_fields: &'a [AnonFieldRec],
    locals: &'a [LocalRec],
    params: &'a [ParamRec],
    funcs: &'a [FuncRec],
    nodes: &'a [NodeRec],
}

fn slice<'a, T: FromBytes + KnownLayout + Immutable>(
    bytes: &'a [u8],
    table: &[SectionRec],
    which: usize,
) -> Result<&'a [T], TastError> {
    let name = SECTIONS[which].name;
    let out = TastError::SectionOutOfBounds { section: name };
    let rec = &table[which];
    let (offset, count) = (rec.offset.get() as usize, rec.count.get() as usize);
    let end = count
        .checked_mul(SECTIONS[which].elem)
        .and_then(|n| offset.checked_add(n))
        .filter(|end| *end <= bytes.len())
        .ok_or_else(|| out.clone())?;
    <[T]>::ref_from_bytes(&bytes[offset..end]).map_err(|_| out)
}

/// `value` names an entry of a table of `limit` records. `optional` allows [`NONE`].
fn reference(
    table: &'static str,
    index: usize,
    field: &'static str,
    value: U32,
    limit: usize,
    optional: bool,
) -> Result<(), TastError> {
    let value = value.get();
    if (optional && value == NONE) || (value != NONE && (value as usize) < limit) {
        Ok(())
    } else {
        Err(TastError::BadReference {
            table,
            index,
            field,
            value,
            limit,
        })
    }
}

/// `first..first + count` lies inside a table of `limit` records.
fn range(
    table: &'static str,
    index: usize,
    field: &'static str,
    first: U32,
    count: U32,
    limit: usize,
) -> Result<(), TastError> {
    let (first, count) = (first.get(), count.get());
    if u64::from(first) + u64::from(count) <= limit as u64 {
        Ok(())
    } else {
        Err(TastError::BadReference {
            table,
            index,
            field,
            value: first,
            limit,
        })
    }
}

impl<'a> Tast<'a> {
    /// Check `bytes` and borrow them as a TAST file.
    pub fn parse(bytes: &'a [u8]) -> Result<Tast<'a>, TastError> {
        let (header, rest) = Header::ref_from_prefix(bytes).map_err(|_| TastError::TooShort)?;
        if header.magic.get() != MAGIC {
            return Err(TastError::BadMagic(header.magic.get()));
        }
        if header.version.get() != VERSION {
            return Err(TastError::BadVersion(header.version.get()));
        }
        let file =
            u64::from(header.schema_hash_hi.get()) << 32 | u64::from(header.schema_hash_lo.get());
        let reader = schema_hash();
        if file != reader {
            return Err(TastError::SchemaMismatch { file, reader });
        }
        let found = header.section_count.get();
        if (found as usize) < SECTIONS.len() {
            return Err(TastError::MissingSections {
                found,
                need: SECTIONS.len(),
            });
        }
        let table_bytes = (found as usize)
            .checked_mul(SectionRec::SIZE)
            .filter(|n| *n <= rest.len())
            .ok_or(TastError::TooShort)?;
        let table = <[SectionRec]>::ref_from_bytes(&rest[..table_bytes])
            .map_err(|_| TastError::TooShort)?;

        let blob_bytes: &[u8] = {
            let rec = &table[section::STRINGS];
            let (offset, count) = (rec.offset.get() as usize, rec.count.get() as usize);
            offset
                .checked_add(count)
                .filter(|end| *end <= bytes.len())
                .map(|end| &bytes[offset..end])
                .ok_or(TastError::SectionOutOfBounds { section: "Strings" })?
        };
        let blob =
            std::str::from_utf8(blob_bytes).map_err(|_| TastError::BadString { index: 0 })?;

        let tast = Tast {
            header,
            blob,
            str_index: slice(bytes, table, section::STRING_INDEX)?,
            positions: slice(bytes, table, section::POSITIONS)?,
            types: slice(bytes, table, section::TYPES)?,
            type_args: slice(bytes, table, section::TYPE_ARGS)?,
            classes: slice(bytes, table, section::CLASSES)?,
            fields: slice(bytes, table, section::FIELDS)?,
            abstracts: slice(bytes, table, section::ABSTRACTS)?,
            enums: slice(bytes, table, section::ENUMS)?,
            enum_ctors: slice(bytes, table, section::ENUM_CTORS)?,
            anon_fields: slice(bytes, table, section::ANON_FIELDS)?,
            locals: slice(bytes, table, section::LOCALS)?,
            params: slice(bytes, table, section::PARAMS)?,
            funcs: slice(bytes, table, section::FUNCS)?,
            nodes: slice(bytes, table, section::NODES)?,
        };
        tast.validate()?;
        Ok(tast)
    }

    fn validate(&self) -> Result<(), TastError> {
        let (strs, types, classes, fields) = (
            self.str_index.len(),
            self.types.len(),
            self.classes.len(),
            self.fields.len(),
        );
        let (funcs, locals, nodes) = (self.funcs.len(), self.locals.len(), self.nodes.len());
        let (positions, type_args) = (self.positions.len(), self.type_args.len());

        reference("Header", 0, "entry", self.header.entry, funcs, true)?;

        for (i, s) in self.str_index.iter().enumerate() {
            let (offset, len) = (s.offset.get() as usize, s.len.get() as usize);
            let ok = offset.checked_add(len).is_some_and(|end| {
                end <= self.blob.len()
                    && self.blob.is_char_boundary(offset)
                    && self.blob.is_char_boundary(end)
            });
            if !ok {
                return Err(TastError::BadString { index: i });
            }
        }
        for (i, p) in self.positions.iter().enumerate() {
            reference("Positions", i, "file", p.file, strs, true)?;
        }
        for (i, id) in self.type_args.iter().enumerate() {
            reference("TypeArgs", i, "type", *id, types, false)?;
        }
        for (i, t) in self.types.iter().enumerate() {
            reference("Types", i, "name", t.name, strs, true)?;
            match t.kind {
                type_kind::ENUM => reference("Types", i, "def", t.def, self.enums.len(), false)?,
                type_kind::CLASS => reference("Types", i, "def", t.def, classes, false)?,
                type_kind::ABSTRACT => {
                    reference("Types", i, "def", t.def, self.abstracts.len(), false)?
                }
                type_kind::FUN => reference("Types", i, "def", t.def, types, false)?,
                type_kind::ANON => range(
                    "Types",
                    i,
                    "def",
                    t.def,
                    t.args_count,
                    self.anon_fields.len(),
                )?,
                type_kind::MONO | type_kind::DYNAMIC => {}
                _ => {
                    return Err(TastError::BadReference {
                        table: "Types",
                        index: i,
                        field: "kind",
                        value: u32::from(t.kind),
                        limit: TYPE_KINDS.len(),
                    });
                }
            }
            if t.kind != type_kind::ANON {
                range(
                    "Types",
                    i,
                    "args_first",
                    t.args_first,
                    t.args_count,
                    type_args,
                )?;
            }
        }
        for (i, c) in self.classes.iter().enumerate() {
            reference("Classes", i, "name", c.name, strs, true)?;
            reference("Classes", i, "super_type", c.super_type, types, true)?;
            range(
                "Classes",
                i,
                "fields_first",
                c.fields_first,
                c.fields_count,
                fields,
            )?;
            range(
                "Classes",
                i,
                "interfaces_first",
                c.interfaces_first,
                c.interfaces_count,
                type_args,
            )?;
        }
        for (i, f) in self.fields.iter().enumerate() {
            reference("Fields", i, "name", f.name, strs, false)?;
            reference("Fields", i, "ty", f.ty, types, false)?;
            reference("Fields", i, "owner", f.owner, classes, false)?;
            reference("Fields", i, "func", f.func, funcs, true)?;
            reference("Fields", i, "pos", f.pos, positions, true)?;
        }
        for (i, a) in self.abstracts.iter().enumerate() {
            reference("Abstracts", i, "name", a.name, strs, true)?;
            reference("Abstracts", i, "underlying", a.underlying, types, true)?;
            reference("Abstracts", i, "impl_class", a.impl_class, classes, true)?;
        }
        for (i, e) in self.enums.iter().enumerate() {
            reference("Enums", i, "name", e.name, strs, true)?;
            range(
                "Enums",
                i,
                "ctors_first",
                e.ctors_first,
                e.ctors_count,
                self.enum_ctors.len(),
            )?;
        }
        for (i, c) in self.enum_ctors.iter().enumerate() {
            reference("EnumCtors", i, "name", c.name, strs, false)?;
            reference(
                "EnumCtors",
                i,
                "enum_id",
                c.enum_id,
                self.enums.len(),
                false,
            )?;
            reference("EnumCtors", i, "ty", c.ty, types, false)?;
            range(
                "EnumCtors",
                i,
                "args_first",
                c.args_first,
                c.args_count,
                type_args,
            )?;
        }
        for (i, f) in self.anon_fields.iter().enumerate() {
            reference("AnonFields", i, "name", f.name, strs, false)?;
            reference("AnonFields", i, "ty", f.ty, types, false)?;
        }
        for (i, l) in self.locals.iter().enumerate() {
            reference("Locals", i, "name", l.name, strs, true)?;
            reference("Locals", i, "ty", l.ty, types, false)?;
        }
        for (i, p) in self.params.iter().enumerate() {
            reference("Params", i, "local", p.local, locals, false)?;
            reference("Params", i, "default_value", p.default_value, nodes, true)?;
        }
        for (i, f) in self.funcs.iter().enumerate() {
            reference("Funcs", i, "ret", f.ret, types, true)?;
            range(
                "Funcs",
                i,
                "params_first",
                f.params_first,
                f.params_count,
                self.params.len(),
            )?;
            reference("Funcs", i, "root", f.root, nodes, true)?;
            reference("Funcs", i, "pos", f.pos, positions, true)?;
        }
        for i in 0..nodes {
            self.validate_node(i)?;
        }
        Ok(())
    }

    fn validate_node(&self, i: usize) -> Result<(), TastError> {
        use node_kind as k;
        let n = &self.nodes[i];
        reference("Nodes", i, "ty", n.ty, self.types.len(), true)?;
        reference("Nodes", i, "pos", n.pos, self.positions.len(), true)?;
        let kids = n.kid_count.get();
        if kids > 0 && (n.first_kid.get() as usize) <= i {
            return Err(TastError::KidsNotAfterParent { node: i });
        }
        range(
            "Nodes",
            i,
            "first_kid",
            n.first_kid,
            n.kid_count,
            self.nodes.len(),
        )?;

        let strs = self.str_index.len();
        let (flags, a, b) = (n.flags, n.a, n.b);
        let arity =
            |name: &'static str, exact: Option<u32>, at_least: u32| -> Result<(), TastError> {
                let ok = match exact {
                    Some(want) => kids == want,
                    None => kids >= at_least,
                };
                if ok {
                    Ok(())
                } else {
                    Err(TastError::BadArity {
                        node: i,
                        kind: name,
                        kids,
                    })
                }
            };
        let module = |kind: u8, index: U32| -> Result<(), TastError> {
            let limit = match kind {
                module_kind::CLASS => self.classes.len(),
                module_kind::ENUM => self.enums.len(),
                module_kind::ABSTRACT => self.abstracts.len(),
                _ => {
                    return Err(TastError::BadReference {
                        table: "Nodes",
                        index: i,
                        field: "module kind",
                        value: u32::from(kind),
                        limit: MODULE_KINDS.len(),
                    });
                }
            };
            reference("Nodes", i, "a", index, limit, false)
        };

        match n.kind {
            k::CONST => {
                arity("CONST", Some(0), 0)?;
                match flags {
                    const_kind::FLOAT | const_kind::STRING => {
                        reference("Nodes", i, "a", a, strs, false)?
                    }
                    const_kind::BOOL if a.get() > 1 => {
                        return Err(TastError::BadReference {
                            table: "Nodes",
                            index: i,
                            field: "a",
                            value: a.get(),
                            limit: 2,
                        });
                    }
                    const_kind::INT
                    | const_kind::BOOL
                    | const_kind::NULL
                    | const_kind::THIS
                    | const_kind::SUPER => {}
                    _ => {
                        return Err(TastError::BadReference {
                            table: "Nodes",
                            index: i,
                            field: "flags",
                            value: u32::from(flags),
                            limit: CONST_KINDS.len(),
                        });
                    }
                }
            }
            k::LOCAL => {
                arity("LOCAL", Some(0), 0)?;
                reference("Nodes", i, "a", a, self.locals.len(), false)?;
            }
            k::ARRAY => arity("ARRAY", Some(2), 0)?,
            k::BINOP => {
                arity("BINOP", Some(2), 0)?;
                reference("Nodes", i, "a", a, BINOPS.len(), false)?;
                if a.get() == BINOP_ASSIGN_OP {
                    reference("Nodes", i, "b", b, BINOPS.len(), false)?;
                }
            }
            k::FIELD => {
                arity("FIELD", Some(1), 0)?;
                let limit = match a.get() {
                    field_access::INSTANCE | field_access::STATIC | field_access::CLOSURE => {
                        self.fields.len()
                    }
                    field_access::ANON | field_access::DYNAMIC => strs,
                    field_access::ENUM => self.enum_ctors.len(),
                    other => {
                        return Err(TastError::BadReference {
                            table: "Nodes",
                            index: i,
                            field: "a",
                            value: other,
                            limit: FIELD_ACCESSES.len(),
                        });
                    }
                };
                reference("Nodes", i, "b", b, limit, false)?;
            }
            k::TYPE_EXPR => {
                arity("TYPE_EXPR", Some(0), 0)?;
                module(flags, a)?;
            }
            k::PARENTHESIS => arity("PARENTHESIS", Some(1), 0)?,
            k::OBJECT_DECL | k::ARRAY_DECL | k::BLOCK => {}
            k::CALL => arity("CALL", None, 1)?,
            k::NEW => reference("Nodes", i, "a", a, self.classes.len(), false)?,
            k::UNOP => {
                arity("UNOP", Some(1), 0)?;
                reference("Nodes", i, "a", a, UNOPS.len(), false)?;
            }
            k::FUNCTION => {
                arity("FUNCTION", Some(0), 0)?;
                reference("Nodes", i, "a", a, self.funcs.len(), false)?;
            }
            k::VAR => {
                arity("VAR", Some(u32::from(flags & node_flag::HAS_INIT)), 0)?;
                reference("Nodes", i, "a", a, self.locals.len(), false)?;
            }
            k::FOR => {
                arity("FOR", Some(2), 0)?;
                reference("Nodes", i, "a", a, self.locals.len(), false)?;
            }
            k::IF => arity("IF", Some(2 + u32::from(flags & node_flag::HAS_ELSE)), 0)?,
            k::WHILE => arity("WHILE", Some(2), 0)?,
            k::SWITCH => arity(
                "SWITCH",
                None,
                1 + u32::from(flags & node_flag::HAS_DEFAULT),
            )?,
            k::TRY => arity("TRY", None, 1)?,
            k::RETURN => arity("RETURN", Some(u32::from(flags & node_flag::HAS_VALUE)), 0)?,
            k::BREAK | k::CONTINUE => arity("BREAK/CONTINUE", Some(0), 0)?,
            k::THROW => arity("THROW", Some(1), 0)?,
            k::CAST => {
                arity("CAST", Some(1), 0)?;
                if flags & node_flag::HAS_TYPE != 0 {
                    module(flags >> 1, a)?;
                }
            }
            k::META => {
                arity("META", None, 1)?;
                reference("Nodes", i, "a", a, strs, false)?;
            }
            k::ENUM_PARAMETER => {
                arity("ENUM_PARAMETER", Some(1), 0)?;
                reference("Nodes", i, "a", a, self.enum_ctors.len(), false)?;
            }
            k::ENUM_INDEX => arity("ENUM_INDEX", Some(1), 0)?,
            k::IDENT => {
                arity("IDENT", Some(0), 0)?;
                reference("Nodes", i, "a", a, strs, false)?;
            }
            k::CASE => arity("CASE", None, a.get().saturating_add(1))?,
            k::CATCH => {
                arity("CATCH", Some(1), 0)?;
                reference("Nodes", i, "a", a, self.locals.len(), false)?;
            }
            k::OBJECT_FIELD => {
                arity("OBJECT_FIELD", Some(1), 0)?;
                reference("Nodes", i, "a", a, strs, false)?;
            }
            k::DEFAULT => arity("DEFAULT", Some(1), 0)?,
            kind => return Err(TastError::UnknownNodeKind { node: i, kind }),
        }
        Ok(())
    }

    /// The program's entry function id.
    pub fn entry(&self) -> Option<u32> {
        let entry = self.header.entry.get();
        (entry != NONE).then_some(entry)
    }

    /// The Haxe compiler version the file was written by, as `major * 10000 + minor * 100 + patch`.
    pub fn haxe_version(&self) -> u32 {
        self.header.haxe_version.get()
    }

    /// A string by id. Ids are validated, so this does not fail.
    pub fn string(&self, id: u32) -> &'a str {
        let s = &self.str_index[id as usize];
        let offset = s.offset.get() as usize;
        &self.blob[offset..offset + s.len.get() as usize]
    }

    /// A string by a possibly absent id.
    pub fn opt_string(&self, id: U32) -> Option<&'a str> {
        (id.get() != NONE).then(|| self.string(id.get()))
    }

    pub fn strings_len(&self) -> usize {
        self.str_index.len()
    }
    pub fn positions(&self) -> &'a [PosRec] {
        self.positions
    }
    pub fn types(&self) -> &'a [TypeRec] {
        self.types
    }
    pub fn classes(&self) -> &'a [ClassRec] {
        self.classes
    }
    pub fn fields(&self) -> &'a [FieldRec] {
        self.fields
    }
    pub fn abstracts(&self) -> &'a [AbstractRec] {
        self.abstracts
    }
    pub fn enums(&self) -> &'a [EnumRec] {
        self.enums
    }
    pub fn enum_ctors(&self) -> &'a [EnumCtorRec] {
        self.enum_ctors
    }
    pub fn anon_fields(&self) -> &'a [AnonFieldRec] {
        self.anon_fields
    }
    pub fn locals(&self) -> &'a [LocalRec] {
        self.locals
    }
    pub fn params(&self) -> &'a [ParamRec] {
        self.params
    }
    pub fn funcs(&self) -> &'a [FuncRec] {
        self.funcs
    }
    pub fn nodes(&self) -> &'a [NodeRec] {
        self.nodes
    }

    /// The type ids `first..first + count` of the `TypeArgs` section.
    pub fn type_args(&self, first: U32, count: U32) -> &'a [U32] {
        let first = first.get() as usize;
        &self.type_args[first..first + count.get() as usize]
    }

    /// A node by id.
    pub fn node(&self, id: u32) -> &'a NodeRec {
        &self.nodes[id as usize]
    }

    /// The children of `node`, which are consecutive in the node array.
    pub fn kids(&self, node: &NodeRec) -> &'a [NodeRec] {
        let first = node.first_kid.get() as usize;
        &self.nodes[first..first + node.kid_count.get() as usize]
    }

    /// The ids of the children of `node`.
    pub fn kid_ids(&self, node: &NodeRec) -> std::ops::Range<u32> {
        node.first_kid.get()..node.first_kid.get() + node.kid_count.get()
    }

    /// A function's body root, when it has one.
    pub fn body(&self, func: &FuncRec) -> Option<&'a NodeRec> {
        let root = func.root.get();
        (root != NONE).then(|| self.node(root))
    }

    /// The parameters of `func`.
    pub fn func_params(&self, func: &FuncRec) -> &'a [ParamRec] {
        let first = func.params_first.get() as usize;
        &self.params[first..first + func.params_count.get() as usize]
    }
}
