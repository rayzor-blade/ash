//! Runs the AIR that `ash_nair` lowers on the existing engines, through the file they already read.
//!
//! The engines take a module of types, pools and functions whose bodies are HashLink opcodes.
//! This serializes each lowered AIR function to opcodes and assembles the module, then writes
//! it as a `.hl` file. It is a bridge: what the TAST knows beyond the bytecode does not survive
//! it, so what runs is the lowering's plain AIR and nothing of what the TAST adds. It exists to
//! run and time the lowering on the real tiers and to compare its output with genhl's.

use ash_core::bytecode::DecodedBytecode;
use ash_core::hl_bindings as hl;
use ash_core::types::{
    HLConstant, HLEnumConstruct, HLFunction, HLNative, HLObjField, HLObjProto, HLType, HLTypeEnum,
    HLTypeFun, HLTypeObj, HLTypeVirtual, TypeRef,
};
use ash_nair::hl::{HlType, ObjType};
use ash_nair::lower::{Lowered, Module};

pub type Result<T> = std::result::Result<T, String>;

/// The index of `text` in the string table, which the file spells every name through.
fn intern(strings: &mut Vec<String>, text: &str) -> u32 {
    match strings.iter().position(|s| s == text) {
        Some(i) => i as u32,
        None => {
            strings.push(text.to_owned());
            (strings.len() - 1) as u32
        }
    }
}

fn plain(kind: u32) -> HLType {
    HLType {
        kind,
        ..Default::default()
    }
}

/// An object's or structure's entry in the file: its name, layout and method table.
fn object(kind: u32, o: &ObjType) -> HLType {
    HLType {
        kind,
        obj: Some(HLTypeObj {
            name: o.name.clone(),
            super_: o.super_.map(TypeRef),
            fields: o
                .fields
                .iter()
                .map(|(name, ty)| HLObjField {
                    name: name.clone(),
                    type_: TypeRef(*ty),
                    hashed_name: 0,
                })
                .collect(),
            proto: o
                .proto
                .iter()
                .map(|p| HLObjProto {
                    name: p.name.clone(),
                    findex: p.findex as i32,
                    pindex: p.vslot.map_or(-1, |v| v as i32),
                    hashed_name: 0,
                })
                .collect(),
            bindings: o
                .bindings
                .iter()
                .flat_map(|(field, fun)| [*field as i32, *fun as i32])
                .collect(),
            // the file counts globals from one, and has zero for none
            global_value: o.global.map_or(0, |g| g + 1),
        }),
        ..Default::default()
    }
}

/// A type as the file writes it.
fn file_type(module: &Module, t: &HlType) -> HLType {
    let table = &module.types.table;
    let function = |kind: u32, args: &[usize], ret: usize| HLType {
        kind,
        fun: Some(HLTypeFun {
            args: args.iter().map(|a| TypeRef(*a)).collect(),
            ret: TypeRef(ret),
            ..Default::default()
        }),
        ..Default::default()
    };
    let wrapper = |kind: u32, inner: usize| HLType {
        kind,
        tparam: Some(TypeRef(inner)),
        ..Default::default()
    };
    match t {
        HlType::Void => plain(hl::hl_type_kind_HVOID),
        HlType::UI8 => plain(hl::hl_type_kind_HUI8),
        HlType::UI16 => plain(hl::hl_type_kind_HUI16),
        HlType::I32 => plain(hl::hl_type_kind_HI32),
        HlType::I64 => plain(hl::hl_type_kind_HI64),
        HlType::F32 => plain(hl::hl_type_kind_HF32),
        HlType::F64 => plain(hl::hl_type_kind_HF64),
        HlType::Bool => plain(hl::hl_type_kind_HBOOL),
        HlType::Bytes => plain(hl::hl_type_kind_HBYTES),
        HlType::Dyn => plain(hl::hl_type_kind_HDYN),
        HlType::Type => plain(hl::hl_type_kind_HTYPE),
        HlType::Array => plain(hl::hl_type_kind_HARRAY),
        HlType::DynObj => plain(hl::hl_type_kind_HDYNOBJ),
        HlType::Fun(args, ret) => function(hl::hl_type_kind_HFUN, args, *ret),
        HlType::Method(args, ret) => function(hl::hl_type_kind_HMETHOD, args, *ret),
        HlType::Obj(o) => object(hl::hl_type_kind_HOBJ, &table.objs[*o]),
        HlType::Struct(o) => object(hl::hl_type_kind_HSTRUCT, &table.objs[*o]),
        HlType::Ref(inner) => wrapper(hl::hl_type_kind_HREF, *inner),
        HlType::Null(inner) => wrapper(hl::hl_type_kind_HNULL, *inner),
        HlType::Packed(inner) => wrapper(hl::hl_type_kind_HPACKED, *inner),
        HlType::Abstract(name) => HLType {
            kind: hl::hl_type_kind_HABSTRACT,
            abs_name: Some(name.clone()),
            ..Default::default()
        },
        HlType::Virtual(v) => HLType {
            kind: hl::hl_type_kind_HVIRTUAL,
            virt: Some(HLTypeVirtual {
                fields: table.virts[*v]
                    .iter()
                    .map(|(name, ty)| HLObjField {
                        name: name.clone(),
                        type_: TypeRef(*ty),
                        hashed_name: 0,
                    })
                    .collect(),
                ..Default::default()
            }),
            ..Default::default()
        },
        HlType::Enum(e) => {
            let e = &table.enums[*e];
            HLType {
                kind: hl::hl_type_kind_HENUM,
                tenum: Some(HLTypeEnum {
                    name: e.name.clone(),
                    constructs: e
                        .fields
                        .iter()
                        .map(|(name, params)| HLEnumConstruct {
                            name: name.clone(),
                            params: params.iter().map(|p| TypeRef(*p)).collect(),
                            ..Default::default()
                        })
                        .collect(),
                    global_value: e.global.map_or(0, |g| g + 1),
                }),
                ..Default::default()
            }
        }
    }
}

/// The module as the runtime reads it, with `entry` as the function it starts in.
pub fn assemble(module: &Module, lowered: &[Lowered], entry: u32) -> Result<DecodedBytecode> {
    let mut bc = DecodedBytecode::default();
    for t in &module.types.table.types {
        bc.types.push(file_type(module, t));
    }
    bc.globals = module
        .types
        .table
        .globals
        .iter()
        .map(|(_, ty)| TypeRef(*ty))
        .collect();
    bc.ints = module.ints.clone();
    bc.floats = module.floats.clone();
    // the strings the functions index come first, so their indexes stand
    bc.strings = module.strings.clone();
    // every name the file spells goes through the string table
    for t in &bc.types.clone() {
        let mut names: Vec<&str> = Vec::new();
        if let Some(o) = &t.obj {
            names.push(&o.name);
            names.extend(o.fields.iter().map(|f| f.name.as_str()));
            names.extend(o.proto.iter().map(|p| p.name.as_str()));
        }
        if let Some(v) = &t.virt {
            names.extend(v.fields.iter().map(|f| f.name.as_str()));
        }
        if let Some(e) = &t.tenum {
            names.push(&e.name);
            names.extend(e.constructs.iter().map(|c| c.name.as_str()));
        }
        if let Some(a) = &t.abs_name {
            names.push(a);
        }
        for name in names {
            intern(&mut bc.strings, name);
        }
    }
    for n in &module.natives {
        for text in [&n.lib, &n.name] {
            intern(&mut bc.strings, text);
        }
    }
    for n in &module.natives {
        bc.natives.push(HLNative {
            lib: n.lib.clone(),
            name: n.name.clone(),
            type_: TypeRef(n.ty),
            findex: n.findex as i32,
        });
    }
    // a string constant is a `String` whose buffer is a string of the table and whose length, in
    // UTF-16 units, is an int of the pool
    for c in &module.constants {
        let text = &module.strings[c.text];
        let length = text.encode_utf16().count() as i32;
        bc.ints.push(length);
        bc.constants.push(HLConstant {
            global: c.global,
            fields: vec![c.text as i32, (bc.ints.len() - 1) as i32],
        });
    }
    for l in lowered {
        let s =
            air::v2::serialize::serialize(&l.function).map_err(|e| format!("{}: {e}", l.name))?;
        if !s.new_ints.is_empty() {
            return Err(format!(
                "{}: the serializer asked for constants the pool lacks",
                l.name
            ));
        }
        let mut f = HLFunction::with_body(s.ops, Vec::new());
        f.type_ = TypeRef(l.sig);
        f.findex = l.findex as i32;
        f.regs = s.reg_types.iter().map(|t| TypeRef(t.0 as usize)).collect();
        bc.functions.push(f);
    }
    bc.entrypoint = entry;
    Ok(bc)
}

/// The module encoded as a `.hl` file.
pub fn encode(bc: &DecodedBytecode) -> Result<Vec<u8>> {
    ash_core::bytecode_encode::encode(bc, 5).map_err(|e| e.to_string())
}
