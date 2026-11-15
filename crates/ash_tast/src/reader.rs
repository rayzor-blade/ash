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
    /// The layers given do not stack: a layer's `Parents` is not the identities of
    /// the layers below it.
    ParentMismatch { layer: usize },
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
            TastError::ParentMismatch { layer } => {
                write!(
                    f,
                    "layer {layer} was built on different layers than the ones it is stacked on"
                )
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

/// The tables an id can name in a lower layer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Table {
    Types,
    Classes,
    Fields,
    Abstracts,
    Enums,
    EnumCtors,
}

/// How many entries each taggable table of a layer has.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Counts {
    pub types: usize,
    pub classes: usize,
    pub fields: usize,
    pub abstracts: usize,
    pub enums: usize,
    pub enum_ctors: usize,
}

impl Counts {
    fn of(&self, table: Table) -> usize {
        match table {
            Table::Types => self.types,
            Table::Classes => self.classes,
            Table::Fields => self.fields,
            Table::Abstracts => self.abstracts,
            Table::Enums => self.enums,
            Table::EnumCtors => self.enum_ctors,
        }
    }
}

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
    keys: &'a [KeyRec],
    kept: &'a [KeptRec],
    parents: &'a [ParentRec],
    order: &'a [OrderRec],
    metas: &'a [MetaRec],
    defines: &'a [DefineRec],
    resources: &'a [ResourceRec],
    resource_data: &'a [u8],
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

/// The header and the section table of `bytes`, after the checks that say the
/// file is a TAST file of this layout.
fn open(bytes: &[u8]) -> Result<(&Header, &[SectionRec]), TastError> {
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
    let table =
        <[SectionRec]>::ref_from_bytes(&rest[..table_bytes]).map_err(|_| TastError::TooShort)?;
    Ok((header, table))
}

/// A file's own identity and the identities of the layers it names below it, read
/// without checking the rest.
pub fn lineage(bytes: &[u8]) -> Result<(u64, Vec<u64>), TastError> {
    let (header, table) = open(bytes)?;
    let parents: &[ParentRec] = slice(bytes, table, section::PARENTS)?;
    let id = |lo: U32, hi: U32| u64::from(hi.get()) << 32 | u64::from(lo.get());
    Ok((
        id(header.layer_id_lo, header.layer_id_hi),
        parents.iter().map(|p| id(p.id_lo, p.id_hi)).collect(),
    ))
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
    /// Check `bytes` and borrow them as a TAST file with no layers below it.
    pub fn parse(bytes: &'a [u8]) -> Result<Tast<'a>, TastError> {
        Tast::parse_in(bytes, &[])
    }

    /// Check `bytes` and borrow them as a layer stacked on layers whose table sizes
    /// are `parents`, bottom first. A tagged id is valid when the parent it names
    /// has that entry.
    pub fn parse_in(bytes: &'a [u8], parents: &[Counts]) -> Result<Tast<'a>, TastError> {
        let (header, table) = open(bytes)?;

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
            keys: slice(bytes, table, section::KEYS)?,
            kept: slice(bytes, table, section::KEPT)?,
            parents: slice(bytes, table, section::PARENTS)?,
            order: slice(bytes, table, section::ORDER)?,
            metas: slice(bytes, table, section::METAS)?,
            defines: slice(bytes, table, section::DEFINES)?,
            resources: slice(bytes, table, section::RESOURCES)?,
            resource_data: slice(bytes, table, section::RESOURCE_DATA)?,
        };
        tast.validate(parents)?;
        Ok(tast)
    }

    /// The size of each taggable table of this layer.
    pub fn counts(&self) -> Counts {
        Counts {
            types: self.types.len(),
            classes: self.classes.len(),
            fields: self.fields.len(),
            abstracts: self.abstracts.len(),
            enums: self.enums.len(),
            enum_ctors: self.enum_ctors.len(),
        }
    }

    /// `value` names an entry of `table`: in this layer, or in the parent a tag names.
    fn id_ref(
        &self,
        parents: &[Counts],
        at: (&'static str, usize, &'static str),
        value: U32,
        table: Table,
        optional: bool,
    ) -> Result<(), TastError> {
        let (name, index, field) = at;
        let raw = value.get();
        if optional && raw == NONE {
            return Ok(());
        }
        let limit = match tag::split(raw) {
            Some((layer, entry)) => {
                let size = parents.get(layer as usize).map_or(0, |c| c.of(table));
                if (entry as usize) < size {
                    return Ok(());
                }
                size
            }
            None => {
                let size = self.counts().of(table);
                if raw != NONE && (raw as usize) < size {
                    return Ok(());
                }
                size
            }
        };
        Err(TastError::BadReference {
            table: name,
            index,
            field,
            value: raw,
            limit,
        })
    }

    fn validate(&self, parents: &[Counts]) -> Result<(), TastError> {
        let (strs, types, classes, fields) = (
            self.str_index.len(),
            self.types.len(),
            self.classes.len(),
            self.fields.len(),
        );
        let (funcs, locals, nodes) = (self.funcs.len(), self.locals.len(), self.nodes.len());
        let (positions, type_args) = (self.positions.len(), self.type_args.len());
        let id = |at, value, table, optional| self.id_ref(parents, at, value, table, optional);

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
        for (i, t) in self.type_args.iter().enumerate() {
            id(("TypeArgs", i, "type"), *t, Table::Types, false)?;
        }
        for (i, t) in self.types.iter().enumerate() {
            reference("Types", i, "name", t.name, strs, true)?;
            match t.kind {
                type_kind::ENUM => id(("Types", i, "def"), t.def, Table::Enums, false)?,
                type_kind::CLASS => id(("Types", i, "def"), t.def, Table::Classes, false)?,
                type_kind::ABSTRACT => id(("Types", i, "def"), t.def, Table::Abstracts, false)?,
                type_kind::FUN => id(("Types", i, "def"), t.def, Table::Types, false)?,
                type_kind::ANON => {
                    range(
                        "Types",
                        i,
                        "def",
                        t.def,
                        t.args_count,
                        self.anon_fields.len(),
                    )?;
                    match t.flags {
                        0 => {}
                        anon_status::STATICS => id(
                            ("Types", i, "args_first"),
                            t.args_first,
                            Table::Classes,
                            false,
                        )?,
                        anon_status::ENUM_STATICS => id(
                            ("Types", i, "args_first"),
                            t.args_first,
                            Table::Enums,
                            false,
                        )?,
                        anon_status::ABSTRACT_STATICS => id(
                            ("Types", i, "args_first"),
                            t.args_first,
                            Table::Abstracts,
                            false,
                        )?,
                        other => {
                            return Err(TastError::BadReference {
                                table: "Types",
                                index: i,
                                field: "flags",
                                value: u32::from(other),
                                limit: ANON_STATUSES.len() + 1,
                            });
                        }
                    }
                }
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
            id(("Types", i, "via"), t.via, Table::Types, true)?;
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
            id(
                ("Classes", i, "super_type"),
                c.super_type,
                Table::Types,
                true,
            )?;
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
            reference("Classes", i, "init_func", c.init_func, funcs, true)?;
            reference("Classes", i, "meta_root", c.meta_root, nodes, true)?;
        }
        for (i, f) in self.fields.iter().enumerate() {
            reference("Fields", i, "name", f.name, strs, false)?;
            id(("Fields", i, "ty"), f.ty, Table::Types, false)?;
            id(("Fields", i, "owner"), f.owner, Table::Classes, false)?;
            reference("Fields", i, "func", f.func, funcs, true)?;
            reference("Fields", i, "pos", f.pos, positions, true)?;
        }
        for (i, a) in self.abstracts.iter().enumerate() {
            reference("Abstracts", i, "name", a.name, strs, true)?;
            id(
                ("Abstracts", i, "underlying"),
                a.underlying,
                Table::Types,
                true,
            )?;
            id(
                ("Abstracts", i, "impl_class"),
                a.impl_class,
                Table::Classes,
                true,
            )?;
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
            reference("Enums", i, "meta_root", e.meta_root, nodes, true)?;
        }
        for (i, c) in self.enum_ctors.iter().enumerate() {
            reference("EnumCtors", i, "name", c.name, strs, false)?;
            id(("EnumCtors", i, "enum_id"), c.enum_id, Table::Enums, false)?;
            id(("EnumCtors", i, "ty"), c.ty, Table::Types, false)?;
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
            id(("AnonFields", i, "ty"), f.ty, Table::Types, false)?;
        }
        for (i, l) in self.locals.iter().enumerate() {
            reference("Locals", i, "name", l.name, strs, true)?;
            id(("Locals", i, "ty"), l.ty, Table::Types, false)?;
        }
        for (i, p) in self.params.iter().enumerate() {
            reference("Params", i, "local", p.local, locals, false)?;
            reference("Params", i, "default_value", p.default_value, nodes, true)?;
        }
        for (i, f) in self.funcs.iter().enumerate() {
            id(("Funcs", i, "ret"), f.ret, Table::Types, true)?;
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
        for (i, o) in self.order.iter().enumerate() {
            let limit = match o.kind.get() {
                k if k == u32::from(module_kind::CLASS) => classes,
                k if k == u32::from(module_kind::ENUM) => self.enums.len(),
                k if k == u32::from(module_kind::ABSTRACT) => self.abstracts.len(),
                other => {
                    return Err(TastError::BadReference {
                        table: "Order",
                        index: i,
                        field: "kind",
                        value: other,
                        limit: MODULE_KINDS.len(),
                    });
                }
            };
            reference("Order", i, "id", o.id, limit, false)?;
        }
        for (i, m) in self.metas.iter().enumerate() {
            let limit = match m.owner_kind.get() {
                meta_owner::CLASS => classes,
                meta_owner::FIELD => fields,
                meta_owner::ENUM => self.enums.len(),
                meta_owner::ENUM_CTOR => self.enum_ctors.len(),
                meta_owner::ABSTRACT => self.abstracts.len(),
                meta_owner::LOCAL => locals,
                other => {
                    return Err(TastError::BadReference {
                        table: "Metas",
                        index: i,
                        field: "owner_kind",
                        value: other,
                        limit: META_OWNERS.len(),
                    });
                }
            };
            reference("Metas", i, "owner", m.owner, limit, false)?;
            reference("Metas", i, "name", m.name, strs, false)?;
            range("Metas", i, "args_first", m.args_first, m.args_count, nodes)?;
        }
        for (i, d) in self.defines.iter().enumerate() {
            reference("Defines", i, "name", d.name, strs, false)?;
            reference("Defines", i, "value", d.value, strs, false)?;
        }
        for (i, r) in self.resources.iter().enumerate() {
            reference("Resources", i, "name", r.name, strs, false)?;
            range(
                "Resources",
                i,
                "offset",
                r.offset,
                r.len,
                self.resource_data.len(),
            )?;
        }
        for (i, k) in self.keys.iter().enumerate() {
            reference("Keys", i, "key", k.key, strs, false)?;
            match k.kind.get() {
                key_kind::INST_TYPE
                | key_kind::ABSTRACT_TYPE
                | key_kind::ENUM_TYPE
                | key_kind::TYPEDEF_TYPE
                | key_kind::TEXT_TYPE => reference("Keys", i, "value", k.value, types, false)?,
                key_kind::CLASS => reference("Keys", i, "value", k.value, classes, false)?,
                key_kind::ENUM => reference("Keys", i, "value", k.value, self.enums.len(), false)?,
                key_kind::ABSTRACT => {
                    reference("Keys", i, "value", k.value, self.abstracts.len(), false)?
                }
                key_kind::FIELD => reference("Keys", i, "value", k.value, fields, false)?,
                key_kind::ENUM_CTOR => {
                    reference("Keys", i, "value", k.value, self.enum_ctors.len(), false)?
                }
                key_kind::ROOT | key_kind::DIGEST | key_kind::DEFINE | key_kind::USED_DEFINE => {}
                other => {
                    return Err(TastError::BadReference {
                        table: "Keys",
                        index: i,
                        field: "kind",
                        value: other,
                        limit: KEY_KINDS.len(),
                    });
                }
            }
        }
        for (i, k) in self.kept.iter().enumerate() {
            let table = match k.kind.get() {
                kept_kind::CLASS => Table::Classes,
                kept_kind::FIELD => Table::Fields,
                kept_kind::ENUM => Table::Enums,
                kept_kind::ABSTRACT => Table::Abstracts,
                other => {
                    return Err(TastError::BadReference {
                        table: "Kept",
                        index: i,
                        field: "kind",
                        value: other,
                        limit: KEPT_KINDS.len(),
                    });
                }
            };
            id(("Kept", i, "id"), k.id, table, false)?;
        }
        for i in 0..nodes {
            self.validate_node(i, parents)?;
        }
        Ok(())
    }

    fn validate_node(&self, i: usize, parents: &[Counts]) -> Result<(), TastError> {
        use node_kind as k;
        let n = &self.nodes[i];
        let id = |field, value, table, optional| {
            self.id_ref(parents, ("Nodes", i, field), value, table, optional)
        };
        id("ty", n.ty, Table::Types, true)?;
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
            let table = match kind {
                module_kind::CLASS => Table::Classes,
                module_kind::ENUM => Table::Enums,
                module_kind::ABSTRACT => Table::Abstracts,
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
            id("a", index, table, false)
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
                match a.get() {
                    field_access::INSTANCE | field_access::STATIC | field_access::CLOSURE => {
                        id("b", b, Table::Fields, false)?
                    }
                    field_access::ANON | field_access::DYNAMIC => {
                        reference("Nodes", i, "b", b, strs, false)?
                    }
                    field_access::ENUM => id("b", b, Table::EnumCtors, false)?,
                    other => {
                        return Err(TastError::BadReference {
                            table: "Nodes",
                            index: i,
                            field: "a",
                            value: other,
                            limit: FIELD_ACCESSES.len(),
                        });
                    }
                }
            }
            k::TYPE_EXPR => {
                arity("TYPE_EXPR", Some(0), 0)?;
                module(flags, a)?;
            }
            k::PARENTHESIS => arity("PARENTHESIS", Some(1), 0)?,
            k::OBJECT_DECL | k::ARRAY_DECL | k::BLOCK => {}
            k::CALL => arity("CALL", None, 1)?,
            k::NEW => id("a", a, Table::Classes, false)?,
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
                id("a", a, Table::EnumCtors, false)?;
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

    /// This layer's identity, which layers above it name in `Parents`.
    pub fn layer_id(&self) -> u64 {
        u64::from(self.header.layer_id_hi.get()) << 32 | u64::from(self.header.layer_id_lo.get())
    }

    /// The identities of the layers this one was built on, bottom first.
    pub fn parent_ids(&self) -> impl Iterator<Item = u64> + 'a {
        self.parents
            .iter()
            .map(|p| u64::from(p.id_hi.get()) << 32 | u64::from(p.id_lo.get()))
    }

    /// The program's types in the order the compiler generates them (see [`OrderRec`]).
    pub fn order(&self) -> &'a [OrderRec] {
        self.order
    }

    /// Every metadata entry on a declaration.
    pub fn metas(&self) -> &'a [MetaRec] {
        self.metas
    }

    /// The metadata entries of one owner, in source order.
    pub fn metas_of(&self, owner_kind: u32, owner: u32) -> impl Iterator<Item = &'a MetaRec> + 'a {
        self.metas
            .iter()
            .filter(move |m| m.owner_kind.get() == owner_kind && m.owner.get() == owner)
    }

    /// The argument nodes of a metadata entry.
    pub fn meta_args(&self, meta: &MetaRec) -> &'a [NodeRec] {
        let first = meta.args_first.get() as usize;
        &self.nodes[first..first + meta.args_count.get() as usize]
    }

    /// The compiler defines the program was built with.
    pub fn defines(&self) -> &'a [DefineRec] {
        self.defines
    }

    /// The value of define `name`, if it is set.
    pub fn define(&self, name: &str) -> Option<&'a str> {
        self.defines
            .iter()
            .find(|d| self.string(d.name.get()) == name)
            .map(|d| self.string(d.value.get()))
    }

    /// The embedded resources.
    pub fn resources(&self) -> &'a [ResourceRec] {
        self.resources
    }

    /// The bytes of a resource.
    pub fn resource_data(&self, resource: &ResourceRec) -> &'a [u8] {
        let first = resource.offset.get() as usize;
        &self.resource_data[first..first + resource.len.get() as usize]
    }

    /// The key index layers above use to find what is in this one.
    pub fn keys(&self) -> &'a [KeyRec] {
        self.keys
    }

    /// What the compiler's dead-code removal kept from lower layers.
    pub fn kept(&self) -> &'a [KeptRec] {
        self.kept
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
