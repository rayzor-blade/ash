//! The runtime's type kinds, as AIR names them.

use air::v2::TypeKind;

use crate::hl::*;

/// The AIR name of a runtime type kind. `None` for a number the runtime does not define.
pub fn type_kind(kind: hl_type_kind) -> Option<TypeKind> {
    Some(match kind {
        hl_type_kind_HVOID => TypeKind::Void,
        hl_type_kind_HUI8 => TypeKind::UI8,
        hl_type_kind_HUI16 => TypeKind::UI16,
        hl_type_kind_HI32 => TypeKind::I32,
        hl_type_kind_HI64 => TypeKind::I64,
        hl_type_kind_HF32 => TypeKind::F32,
        hl_type_kind_HF64 => TypeKind::F64,
        hl_type_kind_HBOOL => TypeKind::Bool,
        hl_type_kind_HBYTES => TypeKind::Bytes,
        hl_type_kind_HDYN => TypeKind::Dyn,
        hl_type_kind_HFUN => TypeKind::Fun,
        hl_type_kind_HOBJ => TypeKind::Obj,
        hl_type_kind_HARRAY => TypeKind::Array,
        hl_type_kind_HTYPE => TypeKind::Type,
        hl_type_kind_HREF => TypeKind::Ref,
        hl_type_kind_HVIRTUAL => TypeKind::Virtual,
        hl_type_kind_HDYNOBJ => TypeKind::DynObj,
        hl_type_kind_HABSTRACT => TypeKind::Abstract,
        hl_type_kind_HENUM => TypeKind::Enum,
        hl_type_kind_HNULL => TypeKind::Null,
        hl_type_kind_HMETHOD => TypeKind::Method,
        hl_type_kind_HSTRUCT => TypeKind::Struct,
        hl_type_kind_HPACKED => TypeKind::Packed,
        _ => return None,
    })
}

/// The runtime's number for an AIR type kind.
pub fn hl_kind(kind: TypeKind) -> hl_type_kind {
    match kind {
        TypeKind::Void => hl_type_kind_HVOID,
        TypeKind::UI8 => hl_type_kind_HUI8,
        TypeKind::UI16 => hl_type_kind_HUI16,
        TypeKind::I32 => hl_type_kind_HI32,
        TypeKind::I64 => hl_type_kind_HI64,
        TypeKind::F32 => hl_type_kind_HF32,
        TypeKind::F64 => hl_type_kind_HF64,
        TypeKind::Bool => hl_type_kind_HBOOL,
        TypeKind::Bytes => hl_type_kind_HBYTES,
        TypeKind::Dyn => hl_type_kind_HDYN,
        TypeKind::Fun => hl_type_kind_HFUN,
        TypeKind::Obj => hl_type_kind_HOBJ,
        TypeKind::Array => hl_type_kind_HARRAY,
        TypeKind::Type => hl_type_kind_HTYPE,
        TypeKind::Ref => hl_type_kind_HREF,
        TypeKind::Virtual => hl_type_kind_HVIRTUAL,
        TypeKind::DynObj => hl_type_kind_HDYNOBJ,
        TypeKind::Abstract => hl_type_kind_HABSTRACT,
        TypeKind::Enum => hl_type_kind_HENUM,
        TypeKind::Null => hl_type_kind_HNULL,
        TypeKind::Method => hl_type_kind_HMETHOD,
        TypeKind::Struct => hl_type_kind_HSTRUCT,
        TypeKind::Packed => hl_type_kind_HPACKED,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_kind_the_runtime_defines_has_a_name() {
        for k in 0..hl_type_kind_HLAST {
            let named = type_kind(k).expect("a name");
            assert_eq!(hl_kind(named), k, "kind {k} round-trips");
        }
        assert_eq!(type_kind(hl_type_kind_HLAST), None);
    }
}
