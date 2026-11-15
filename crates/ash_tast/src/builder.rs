//! Builds TAST files from Rust, for tests and for producers other than the
//! Haxe macro.
//!
//! Expression trees are given as [`Tree`] values and laid out breadth-first,
//! so a node's children are consecutive, the way the format requires and the
//! Haxe writer lays them out.

use std::collections::HashMap;

use zerocopy::IntoBytes;
use zerocopy::little_endian::{U16, U32};

use crate::format::*;

/// One expression node and its children, before layout.
#[derive(Debug, Clone)]
pub struct Tree {
    pub kind: u8,
    pub flags: u8,
    pub ty: u32,
    pub pos: u32,
    pub a: u32,
    pub b: u32,
    pub kids: Vec<Tree>,
}

impl Tree {
    pub fn new(kind: u8) -> Tree {
        Tree {
            kind,
            flags: 0,
            ty: NONE,
            pos: NONE,
            a: 0,
            b: 0,
            kids: Vec::new(),
        }
    }
    pub fn flags(mut self, flags: u8) -> Tree {
        self.flags = flags;
        self
    }
    pub fn ty(mut self, ty: u32) -> Tree {
        self.ty = ty;
        self
    }
    pub fn a(mut self, a: u32) -> Tree {
        self.a = a;
        self
    }
    pub fn b(mut self, b: u32) -> Tree {
        self.b = b;
        self
    }
    pub fn kids(mut self, kids: Vec<Tree>) -> Tree {
        self.kids = kids;
        self
    }
}

fn u32s(values: &[u32]) -> Vec<U32> {
    values.iter().map(|v| U32::new(*v)).collect()
}

/// Collects the tables of a TAST file and writes it.
#[derive(Default)]
pub struct Builder {
    blob: Vec<u8>,
    str_index: Vec<StrRec>,
    interned: HashMap<String, u32>,
    pub positions: Vec<PosRec>,
    pub types: Vec<TypeRec>,
    pub type_args: Vec<U32>,
    pub classes: Vec<ClassRec>,
    pub fields: Vec<FieldRec>,
    pub abstracts: Vec<AbstractRec>,
    pub enums: Vec<EnumRec>,
    pub enum_ctors: Vec<EnumCtorRec>,
    pub anon_fields: Vec<AnonFieldRec>,
    pub locals: Vec<LocalRec>,
    pub params: Vec<ParamRec>,
    pub funcs: Vec<FuncRec>,
    pub keys: Vec<KeyRec>,
    pub kept: Vec<KeptRec>,
    pub parents: Vec<u64>,
    pub order: Vec<OrderRec>,
    pub metas: Vec<MetaRec>,
    pub defines: Vec<DefineRec>,
    pub resources: Vec<ResourceRec>,
    pub generics: Vec<GenericRec>,
    resource_data: Vec<u8>,
    bodies: Vec<(usize, Tree)>,
    /// The argument trees of each entry of `metas`, by index.
    meta_args: Vec<(usize, Vec<Tree>)>,
    pub entry: Option<u32>,
    pub haxe_version: u32,
    /// This layer's identity, which layers above name in `Parents`.
    pub layer_id: u64,
}

impl Builder {
    pub fn new() -> Builder {
        Builder {
            haxe_version: 40306,
            ..Builder::default()
        }
    }

    /// The id of `text`, adding it on first use.
    pub fn string(&mut self, text: &str) -> u32 {
        if let Some(id) = self.interned.get(text) {
            return *id;
        }
        let id = self.str_index.len() as u32;
        self.str_index.push(StrRec {
            offset: U32::new(self.blob.len() as u32),
            len: U32::new(text.len() as u32),
        });
        self.blob.extend_from_slice(text.as_bytes());
        self.interned.insert(text.to_owned(), id);
        id
    }

    /// Append `ids` to `TypeArgs` and answer `(first, count)`.
    pub fn type_arg_list(&mut self, ids: &[u32]) -> (u32, u32) {
        let first = self.type_args.len() as u32;
        self.type_args.extend(u32s(ids));
        (first, ids.len() as u32)
    }

    /// Add a type of `kind`, naming `def` in the table the kind says, with `args`.
    pub fn add_type(&mut self, kind: u8, name: Option<&str>, def: u32, args: &[u32]) -> u32 {
        let name = name.map_or(NONE, |n| self.string(n));
        let (first, count) = self.type_arg_list(args);
        self.types.push(TypeRec {
            kind,
            flags: 0,
            _pad: U16::new(0),
            name: U32::new(name),
            def: U32::new(def),
            args_first: U32::new(first),
            args_count: U32::new(count),
            opt_mask: U32::new(0),
            via: U32::new(NONE),
        });
        (self.types.len() - 1) as u32
    }

    /// Add metadata `name` (with its leading colon) on `owner` of `owner_kind`, with `args`.
    pub fn add_meta(&mut self, owner_kind: u32, owner: u32, name: &str, args: Vec<Tree>) {
        let name = self.string(name);
        self.metas.push(MetaRec {
            owner_kind: U32::new(owner_kind),
            owner: U32::new(owner),
            name: U32::new(name),
            args_first: U32::new(0),
            args_count: U32::new(args.len() as u32),
        });
        self.meta_args.push((self.metas.len() - 1, args));
    }

    /// Add a define.
    pub fn add_define(&mut self, name: &str, value: &str) {
        let (name, value) = (self.string(name), self.string(value));
        self.defines.push(DefineRec {
            name: U32::new(name),
            value: U32::new(value),
        });
    }

    /// Add an embedded resource.
    pub fn add_resource(&mut self, name: &str, data: &[u8]) {
        let name = self.string(name);
        self.resources.push(ResourceRec {
            name: U32::new(name),
            offset: U32::new(self.resource_data.len() as u32),
            len: U32::new(data.len() as u32),
        });
        self.resource_data.extend_from_slice(data);
    }

    /// Index `text` as `kind`, answering to `value`, for layers above this one.
    pub fn add_key(&mut self, kind: u32, text: &str, value: u32) {
        let key = self.string(text);
        self.keys.push(KeyRec {
            kind: U32::new(kind),
            key: U32::new(key),
            value: U32::new(value),
        });
    }

    /// Record that the dead-code removal kept `id` (a tagged id) of a lower layer.
    pub fn add_kept(&mut self, kind: u32, id: u32) {
        self.kept.push(KeptRec {
            kind: U32::new(kind),
            id: U32::new(id),
        });
    }

    /// Add a local and answer its id.
    pub fn add_local(&mut self, name: &str, ty: u32, flags: u32) -> u32 {
        let name = self.string(name);
        self.locals.push(LocalRec {
            name: U32::new(name),
            ty: U32::new(ty),
            flags: U32::new(flags),
        });
        (self.locals.len() - 1) as u32
    }

    /// Add a function whose parameters are `params` (local ids) and whose body is `body`.
    pub fn add_func(&mut self, ret: u32, params: &[u32], body: Option<Tree>) -> u32 {
        let first = self.params.len() as u32;
        for local in params {
            self.params.push(ParamRec {
                local: U32::new(*local),
                default_value: U32::new(NONE),
            });
        }
        self.funcs.push(FuncRec {
            ret: U32::new(ret),
            params_first: U32::new(first),
            params_count: U32::new(params.len() as u32),
            root: U32::new(NONE),
            flags: U32::new(0),
            pos: U32::new(NONE),
        });
        let id = self.funcs.len() - 1;
        if let Some(body) = body {
            self.bodies.push((id, body));
        }
        id as u32
    }

    /// Lay out the trees and write the file.
    pub fn finish(mut self) -> Vec<u8> {
        let mut queue: Vec<&Tree> = self.bodies.iter().map(|(_, tree)| tree).collect();
        for (root, (func, _)) in self.bodies.iter().enumerate() {
            self.funcs[*func].root = U32::new(root as u32);
        }
        // A metadata entry's arguments are consecutive roots.
        for (meta, args) in &self.meta_args {
            self.metas[*meta].args_first = U32::new(queue.len() as u32);
            queue.extend(args.iter());
        }
        let mut nodes: Vec<NodeRec> = Vec::new();
        let mut next = queue.len() as u32;
        let mut at = 0;
        while at < queue.len() {
            let tree = queue[at];
            at += 1;
            nodes.push(NodeRec {
                kind: tree.kind,
                flags: tree.flags,
                _pad: U16::new(0),
                ty: U32::new(tree.ty),
                pos: U32::new(tree.pos),
                a: U32::new(tree.a),
                b: U32::new(tree.b),
                first_kid: U32::new(next),
                kid_count: U32::new(tree.kids.len() as u32),
            });
            next += tree.kids.len() as u32;
            queue.extend(tree.kids.iter());
        }

        let parents: Vec<ParentRec> = self
            .parents
            .iter()
            .map(|id| ParentRec {
                id_lo: U32::new(*id as u32),
                id_hi: U32::new((*id >> 32) as u32),
            })
            .collect();
        let sections: [&[u8]; 24] = [
            &self.blob,
            self.str_index.as_slice().as_bytes(),
            self.positions.as_slice().as_bytes(),
            self.types.as_slice().as_bytes(),
            self.type_args.as_slice().as_bytes(),
            self.classes.as_slice().as_bytes(),
            self.fields.as_slice().as_bytes(),
            self.abstracts.as_slice().as_bytes(),
            self.enums.as_slice().as_bytes(),
            self.enum_ctors.as_slice().as_bytes(),
            self.anon_fields.as_slice().as_bytes(),
            self.locals.as_slice().as_bytes(),
            self.params.as_slice().as_bytes(),
            self.funcs.as_slice().as_bytes(),
            nodes.as_slice().as_bytes(),
            self.keys.as_slice().as_bytes(),
            self.kept.as_slice().as_bytes(),
            parents.as_slice().as_bytes(),
            self.order.as_slice().as_bytes(),
            self.metas.as_slice().as_bytes(),
            self.defines.as_slice().as_bytes(),
            self.resources.as_slice().as_bytes(),
            &self.resource_data,
            self.generics.as_slice().as_bytes(),
        ];
        let counts: [usize; 24] = [
            self.blob.len(),
            self.str_index.len(),
            self.positions.len(),
            self.types.len(),
            self.type_args.len(),
            self.classes.len(),
            self.fields.len(),
            self.abstracts.len(),
            self.enums.len(),
            self.enum_ctors.len(),
            self.anon_fields.len(),
            self.locals.len(),
            self.params.len(),
            self.funcs.len(),
            nodes.len(),
            self.keys.len(),
            self.kept.len(),
            parents.len(),
            self.order.len(),
            self.metas.len(),
            self.defines.len(),
            self.resources.len(),
            self.resource_data.len(),
            self.generics.len(),
        ];
        debug_assert_eq!(sections.len(), SECTIONS.len());

        let hash = schema_hash();
        let header = Header {
            magic: U32::new(MAGIC),
            version: U32::new(VERSION),
            schema_hash_lo: U32::new(hash as u32),
            schema_hash_hi: U32::new((hash >> 32) as u32),
            haxe_version: U32::new(self.haxe_version),
            layer_id_lo: U32::new(self.layer_id as u32),
            layer_id_hi: U32::new((self.layer_id >> 32) as u32),
            flags: U32::new(0),
            entry: U32::new(self.entry.unwrap_or(NONE)),
            section_count: U32::new(SECTIONS.len() as u32),
        };
        let mut out = header.as_bytes().to_vec();
        let mut offset = Header::SIZE + SECTIONS.len() * SectionRec::SIZE;
        for (bytes, count) in sections.iter().zip(counts) {
            let rec = SectionRec {
                offset: U32::new(offset as u32),
                count: U32::new(count as u32),
            };
            out.extend_from_slice(rec.as_bytes());
            offset += bytes.len();
        }
        for bytes in sections {
            out.extend_from_slice(bytes);
        }
        out
    }
}
