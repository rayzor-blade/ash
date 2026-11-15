//! Runs nAIR's output on the existing engines, through the file they already read.
//!
//! The engines take a module of types, pools and functions whose bodies are HashLink opcodes.
//! This serializes each lowered AIR function to opcodes and assembles the module, then writes
//! it as a `.hl` file. It is a bridge: the facts nAIR knows beyond the bytecode do not survive
//! it, so what runs is nAIR's lowering and nothing yet of what the TAST adds. It exists to run
//! and time the lowering on the real tiers and to compare its output with genhl's.

use ash_core::bytecode::DecodedBytecode;
use ash_core::hl_bindings as hl;
use ash_core::types::{HLFunction, HLNative, HLType, HLTypeFun, TypeRef};
use ash_nair::hl::HlType;
use ash_nair::lower::{Lowered, Module};

pub type Result<T> = std::result::Result<T, String>;

fn kind_of(t: &HlType) -> Result<(u32, Option<HLTypeFun>)> {
    let plain = |k: u32| Ok((k, None));
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
        HlType::Fun(args, ret) => Ok((
            hl::hl_type_kind_HFUN,
            Some(HLTypeFun {
                args: args.iter().map(|a| TypeRef(*a)).collect(),
                ret: TypeRef(*ret),
                ..Default::default()
            }),
        )),
        other => Err(format!("the bridge does not write {other:?} yet")),
    }
}

/// The module as the runtime reads it, with `entry` as the function it starts in.
pub fn assemble(module: &Module, lowered: &[Lowered], entry: u32) -> Result<DecodedBytecode> {
    let mut bc = DecodedBytecode::default();
    for t in &module.types.table.types {
        let (kind, fun) = kind_of(t)?;
        bc.types.push(HLType {
            kind,
            fun,
            ..Default::default()
        });
    }
    bc.ints = module.ints.clone();
    bc.floats = module.floats.clone();
    // the encoder finds a native's library and symbol in the string table
    for n in &module.natives {
        for text in [&n.lib, &n.name] {
            if !bc.strings.contains(text) {
                bc.strings.push(text.clone());
            }
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
