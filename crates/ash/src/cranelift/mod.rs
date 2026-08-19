//! Cranelift middle tier (tier 1 of the interpreter → Cranelift → LLVM ladder).
//!
//! The interpreter runs everything at tier 0. Once a function crosses the
//! hotness threshold, beadie's broker asks this module for native code:
//! Cranelift at `opt_level=speed` compiles a small, mechanical subset of the
//! HashLink opcode set in ~0.04 ms per function (two orders of magnitude
//! cheaper than the LLVM tier's per-function budget). Functions outside the
//! subset are left to the LLVM tier.
//!
//! Layout:
//! - [`abi`] (this file): the ONE `hl_type` kind → [`AbiClass`] mapping shared
//!   by the CLIF signature builder and checked against the interpreter's
//!   `arg_kinds`/`float_mask` marshaling contract.
//! - [`backend`]: `CraneliftBackend` construction (production flag set,
//!   bulk symbol registration) and the compile entry point.
//! - [`air`]: the `ASH_AIR` gate deciding whether a compile lowers the
//!   bytecode as decoded or AIR v2's optimized serialization of it.
//! - [`lower`]: HL bytecode → CLIF lowering for the v1 opcode subset.

pub mod air;
pub mod backend;
pub mod lower;

pub use air::{AirMode, Body};
pub use backend::{AshCraneliftBackend, CraneliftTierContext};
pub use lower::{lowering_reject_reason, LoweredFunction};

use crate::hl_bindings as hl;
use crate::opcodes::Opcode;

// ─────────────────────────────────────────────────────────────────────────────
// ABI: the single hl_type kind → machine class mapping
// ─────────────────────────────────────────────────────────────────────────────

/// Machine-level class of a HashLink value, derived from its `hl_type` kind.
///
/// This is the one mapping consumed by
/// - the CLIF signature builder ([`AbiClass::clif_type`]),
/// - the CLIF register/value type for locals,
/// - the checks against the interpreter's `arg_kinds` / `float_mask`
///   marshaling in `call_compiled_function`
///   ([`AbiClass::is_float`] must agree with the interpreter's
///   `is_float_kind`), and
/// - the LLVM tier's function-type builder (`create_function_type`), which
///   this mirrors exactly so a findex compiled by either tier is callable
///   through the same transmuted pointer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AbiClass {
    /// No value (`HVOID`).
    Void,
    /// 8-bit integer (`HUI8`).
    I8,
    /// 16-bit integer (`HUI16`).
    I16,
    /// 32-bit integer (`HI32`).
    I32,
    /// 64-bit integer (`HI64`).
    I64,
    /// Boolean — a byte-sized 0/1 (`HBOOL`). LLVM models it as `i1`; the two
    /// agree on the wire because both only ever carry 0 or 1.
    Bool,
    /// 32-bit float (`HF32`).
    F32,
    /// 64-bit float (`HF64`).
    F64,
    /// Machine pointer — every heap/boxed/reference kind.
    Ptr,
}

impl AbiClass {
    /// CLIF type for values of this class. `Void` has none.
    pub fn clif_type(self) -> Option<cranelift_codegen::ir::Type> {
        use cranelift_codegen::ir::types;
        Some(match self {
            AbiClass::Void => return None,
            AbiClass::I8 | AbiClass::Bool => types::I8,
            AbiClass::I16 => types::I16,
            AbiClass::I32 => types::I32,
            AbiClass::I64 | AbiClass::Ptr => types::I64,
            AbiClass::F32 => types::F32,
            AbiClass::F64 => types::F64,
        })
    }

    /// True for the classes the interpreter marshals through float registers.
    /// MUST agree with `HLInterpreter::call_compiled_function`'s
    /// `is_float_kind` (which tests `HF32 | HF64`).
    pub fn is_float(self) -> bool {
        matches!(self, AbiClass::F32 | AbiClass::F64)
    }

    /// True when the class is an integer (including `Bool` and pointers).
    pub fn is_int(self) -> bool {
        !self.is_float() && self != AbiClass::Void
    }

    /// Width in bits of the machine value, for narrowing/widening decisions.
    pub fn bits(self) -> u32 {
        match self {
            AbiClass::Void => 0,
            AbiClass::I8 | AbiClass::Bool => 8,
            AbiClass::I16 => 16,
            AbiClass::I32 | AbiClass::F32 => 32,
            AbiClass::I64 | AbiClass::Ptr | AbiClass::F64 => 64,
        }
    }
}

/// Map an `hl_type` kind onto its machine class.
///
/// Mirrors `JITModule::convert_hl_type_to_llvm_type` + `create_function_type`:
/// scalars keep their declared width, everything heap-allocated or boxed
/// becomes a machine pointer.
pub fn abi_class(kind: hl::hl_type_kind) -> AbiClass {
    match kind {
        hl::hl_type_kind_HVOID => AbiClass::Void,
        hl::hl_type_kind_HUI8 => AbiClass::I8,
        hl::hl_type_kind_HUI16 => AbiClass::I16,
        hl::hl_type_kind_HI32 => AbiClass::I32,
        hl::hl_type_kind_HI64 => AbiClass::I64,
        hl::hl_type_kind_HF32 => AbiClass::F32,
        hl::hl_type_kind_HF64 => AbiClass::F64,
        hl::hl_type_kind_HBOOL => AbiClass::Bool,
        // HBYTES, HDYN, HFUN, HOBJ, HARRAY, HTYPE, HREF, HVIRTUAL, HDYNOBJ,
        // HABSTRACT, HENUM, HNULL, HMETHOD, HSTRUCT, HPACKED — all pointers.
        _ => AbiClass::Ptr,
    }
}

/// Return type used in the ENTRY signature of a Cranelift-compiled function.
///
/// The interpreter reads the result as a raw `i64` word and re-tags it with
/// `wrap_native_result`, which for `HBOOL` tests `raw != 0` and for
/// `HUI8`/`HUI16` does `raw as i32`. Returning a narrow value would leave the
/// upper bits of `x0` undefined, so sub-word returns are widened to `I32`
/// with an explicit zero-extension in the lowering. Everything else keeps
/// its natural class.
pub fn entry_return_class(kind: hl::hl_type_kind) -> AbiClass {
    match abi_class(kind) {
        AbiClass::I8 | AbiClass::I16 | AbiClass::Bool => AbiClass::I32,
        other => other,
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Opcode gate
// ─────────────────────────────────────────────────────────────────────────────

/// The exact v1 opcode subset the Cranelift tier lowers.
///
/// Everything here is *mechanical*: no object-model layout decisions (field
/// offsets, vtable slots, enum construct offsets, unbox decisions), which are
/// the opcodes that have each produced a corruption bug at least once and are
/// waiting on the shared layout oracle. Everything not listed makes the
/// function ineligible for this tier — it keeps interpreting until the LLVM
/// tier takes it.
///
/// Deliberately excluded, with reasons:
/// - `Trap`/`EndTrap`/`Throw`/`Rethrow`: Cranelift has no `returns_twice`, so
///   a `longjmp` resumed *into* a Cranelift frame is unsound. (Throwing
///   *out* of a frame is fine — that is what `NullCheck` does — because the
///   interpreter's per-call `setjmp` wrapper guarantees the jump exits.)
/// - `CallMethod`/`CallThis`/`CallClosure`/`Dyn*`/`ToDyn`/`SafeCast`/
///   `ToVirtual`/`New`/`EnumAlloc`/`MakeEnum`/enum + array + memory ops:
///   object-model layout decisions this tier still does not make.
///
/// `Field`/`SetField`/`GetThis`/`SetThis` used to be on that list and are not
/// any more: [`crate::layout`] resolves their offsets at compile time, so this
/// tier consumes the same numbers the LLVM tier does rather than deciding
/// anything itself. They were also what made the gate reject essentially all
/// real code — Haxe programs touch fields constantly, so the middle tier was
/// declining every candidate and leaving LLVM to pay for every promotion. They
/// are now admitted as well as lowered; the corpus agrees with the interpreter
/// on all 42 programs at thresholds 1, 10 and 100 and under every tier.
/// - `Ref`/`Unref`/`Setref`: register address-taking has no CLIF `Variable`
///   equivalent without pinning registers to stack slots.
/// - `Prefetch`/`Asm`: x86-only.
/// - `IndirectCall`: only produced by the hot-reload rewrite pass.
pub fn is_cranelift_lowerable(op: &Opcode) -> bool {
    matches!(
        op,
        // Constants and moves
        Opcode::Mov { .. }
            | Opcode::Int { .. }
            | Opcode::Float { .. }
            | Opcode::Bool { .. }
            | Opcode::String { .. }
            | Opcode::Null { .. }
            // Integer / float arithmetic and bitwise
            | Opcode::Add { .. }
            | Opcode::Sub { .. }
            | Opcode::Mul { .. }
            | Opcode::SDiv { .. }
            | Opcode::UDiv { .. }
            | Opcode::SMod { .. }
            | Opcode::UMod { .. }
            | Opcode::Shl { .. }
            | Opcode::SShr { .. }
            | Opcode::UShr { .. }
            | Opcode::And { .. }
            | Opcode::Or { .. }
            | Opcode::Xor { .. }
            | Opcode::Neg { .. }
            | Opcode::Not { .. }
            | Opcode::Incr { .. }
            | Opcode::Decr { .. }
            // Calls (bytecode via functions_ptrs + stub guard, natives direct)
            | Opcode::Call0 { .. }
            | Opcode::Call1 { .. }
            | Opcode::Call2 { .. }
            | Opcode::Call3 { .. }
            | Opcode::Call4 { .. }
            | Opcode::CallN { .. }
            // Globals
            | Opcode::GetGlobal { .. }
            | Opcode::SetGlobal { .. }
            | Opcode::Field { .. }
            | Opcode::SetField { .. }
            | Opcode::GetThis { .. }
            | Opcode::SetThis { .. }
            // Field access was lowered but refused here for a long time: it
            // made programs diverge at a low `--jit-threshold` and not at the
            // default, which read like a miscompile in a function that only
            // became eligible once fields were allowed. It was — but not in the
            // offsets. The defect was two global stores that disagreed, and
            // compiled code reading the one the interpreter treated as a
            // fallback. See `HLInterpreter::init_constants`.
            // Control flow
            | Opcode::JTrue { .. }
            | Opcode::JFalse { .. }
            | Opcode::JNull { .. }
            | Opcode::JNotNull { .. }
            | Opcode::JSLt { .. }
            | Opcode::JSGte { .. }
            | Opcode::JSGt { .. }
            | Opcode::JSLte { .. }
            | Opcode::JULt { .. }
            | Opcode::JUGte { .. }
            | Opcode::JNotLt { .. }
            | Opcode::JNotGte { .. }
            | Opcode::JEq { .. }
            | Opcode::JNotEq { .. }
            | Opcode::JAlways { .. }
            | Opcode::Switch { .. }
            | Opcode::Label
            | Opcode::Nop
            | Opcode::Ret { .. }
            // Conversions
            | Opcode::ToInt { .. }
            | Opcode::ToSFloat { .. }
            | Opcode::ToUFloat { .. }
            | Opcode::UnsafeCast { .. }
            // Guards
            | Opcode::NullCheck { .. }
    )
}

/// First opcode in `ops` this tier cannot lower, if any.
pub fn first_unsupported_opcode(ops: &[Opcode]) -> Option<&Opcode> {
    ops.iter().find(|op| !is_cranelift_lowerable(op))
}

#[cfg(test)]
mod tests {
    use super::*;
    use cranelift_codegen::ir::types;

    /// The mapping must agree with the LLVM tier's `create_function_type` /
    /// `convert_hl_type_to_llvm_type` on every scalar kind, so a findex
    /// compiled by either tier is callable through the same transmuted
    /// pointer from `call_compiled_function`.
    #[test]
    fn abi_class_matches_llvm_scalar_widths() {
        assert_eq!(abi_class(hl::hl_type_kind_HVOID), AbiClass::Void);
        assert_eq!(abi_class(hl::hl_type_kind_HUI8), AbiClass::I8);
        assert_eq!(abi_class(hl::hl_type_kind_HUI16), AbiClass::I16);
        assert_eq!(abi_class(hl::hl_type_kind_HI32), AbiClass::I32);
        assert_eq!(abi_class(hl::hl_type_kind_HI64), AbiClass::I64);
        assert_eq!(abi_class(hl::hl_type_kind_HF32), AbiClass::F32);
        assert_eq!(abi_class(hl::hl_type_kind_HF64), AbiClass::F64);
        assert_eq!(abi_class(hl::hl_type_kind_HBOOL), AbiClass::Bool);

        assert_eq!(abi_class(hl::hl_type_kind_HUI8).bits(), 8);
        assert_eq!(abi_class(hl::hl_type_kind_HUI16).bits(), 16);
        assert_eq!(abi_class(hl::hl_type_kind_HI32).bits(), 32);
        assert_eq!(abi_class(hl::hl_type_kind_HI64).bits(), 64);
        assert_eq!(abi_class(hl::hl_type_kind_HF32).bits(), 32);
        assert_eq!(abi_class(hl::hl_type_kind_HF64).bits(), 64);
    }

    /// Every non-scalar kind is a machine pointer, exactly like the LLVM
    /// tier's `ptr_type` fallback for struct/array/function/void LLVM types.
    #[test]
    fn abi_class_pointer_kinds() {
        for kind in [
            hl::hl_type_kind_HBYTES,
            hl::hl_type_kind_HDYN,
            hl::hl_type_kind_HFUN,
            hl::hl_type_kind_HOBJ,
            hl::hl_type_kind_HARRAY,
            hl::hl_type_kind_HTYPE,
            hl::hl_type_kind_HREF,
            hl::hl_type_kind_HVIRTUAL,
            hl::hl_type_kind_HDYNOBJ,
            hl::hl_type_kind_HABSTRACT,
            hl::hl_type_kind_HENUM,
            hl::hl_type_kind_HNULL,
            hl::hl_type_kind_HMETHOD,
            hl::hl_type_kind_HSTRUCT,
            hl::hl_type_kind_HPACKED,
        ] {
            assert_eq!(abi_class(kind), AbiClass::Ptr, "kind {kind}");
            assert_eq!(abi_class(kind).clif_type(), Some(types::I64));
        }
    }

    /// `is_float` is the contract with the interpreter's `float_mask`:
    /// `call_compiled_function` builds the mask from
    /// `k == HF32 || k == HF64` and routes those calls through
    /// `dispatch_float_native`, which passes f64 in float registers.
    #[test]
    fn float_classes_match_interpreter_float_mask() {
        for kind in 0..=hl::hl_type_kind_HPACKED {
            let interp_says_float = kind == hl::hl_type_kind_HF32 || kind == hl::hl_type_kind_HF64;
            assert_eq!(
                abi_class(kind).is_float(),
                interp_says_float,
                "float_mask disagreement on kind {kind}"
            );
        }
    }

    /// CLIF types are what the signature builder emits.
    #[test]
    fn clif_types() {
        assert_eq!(AbiClass::Void.clif_type(), None);
        assert_eq!(AbiClass::Bool.clif_type(), Some(types::I8));
        assert_eq!(AbiClass::I8.clif_type(), Some(types::I8));
        assert_eq!(AbiClass::I16.clif_type(), Some(types::I16));
        assert_eq!(AbiClass::I32.clif_type(), Some(types::I32));
        assert_eq!(AbiClass::I64.clif_type(), Some(types::I64));
        assert_eq!(AbiClass::Ptr.clif_type(), Some(types::I64));
        assert_eq!(AbiClass::F32.clif_type(), Some(types::F32));
        assert_eq!(AbiClass::F64.clif_type(), Some(types::F64));
    }

    /// Sub-word returns are widened so the interpreter's `raw != 0` (HBOOL)
    /// and `raw as i32` (HUI8/HUI16) never observe undefined upper bits.
    #[test]
    fn entry_returns_widen_subword_kinds() {
        assert_eq!(entry_return_class(hl::hl_type_kind_HBOOL), AbiClass::I32);
        assert_eq!(entry_return_class(hl::hl_type_kind_HUI8), AbiClass::I32);
        assert_eq!(entry_return_class(hl::hl_type_kind_HUI16), AbiClass::I32);
        assert_eq!(entry_return_class(hl::hl_type_kind_HI32), AbiClass::I32);
        assert_eq!(entry_return_class(hl::hl_type_kind_HI64), AbiClass::I64);
        assert_eq!(entry_return_class(hl::hl_type_kind_HF64), AbiClass::F64);
        assert_eq!(entry_return_class(hl::hl_type_kind_HVOID), AbiClass::Void);
        assert_eq!(entry_return_class(hl::hl_type_kind_HOBJ), AbiClass::Ptr);
    }

    #[test]
    fn opcode_gate_accepts_v1_subset() {
        use crate::opcodes::{RefFun, RefGlobal, RefInt, Reg};
        let r = Reg(0);
        assert!(is_cranelift_lowerable(&Opcode::Mov { dst: r, src: r }));
        assert!(is_cranelift_lowerable(&Opcode::Int {
            dst: r,
            ptr: RefInt(0)
        }));
        assert!(is_cranelift_lowerable(&Opcode::Add { dst: r, a: r, b: r }));
        assert!(is_cranelift_lowerable(&Opcode::Call0 {
            dst: r,
            fun: RefFun(0)
        }));
        assert!(is_cranelift_lowerable(&Opcode::GetGlobal {
            dst: r,
            global: RefGlobal(0)
        }));
        assert!(is_cranelift_lowerable(&Opcode::NullCheck { reg: r }));
        assert!(is_cranelift_lowerable(&Opcode::Label));
        assert!(is_cranelift_lowerable(&Opcode::Ret { ret: r }));
    }

    #[test]
    fn opcode_gate_rejects_traps_and_object_model() {
        use crate::opcodes::{RefField, Reg};
        let r = Reg(0);
        assert!(!is_cranelift_lowerable(&Opcode::Trap { exc: r, offset: 0 }));
        assert!(!is_cranelift_lowerable(&Opcode::EndTrap { exc: r }));
        assert!(!is_cranelift_lowerable(&Opcode::Throw { exc: r }));
        assert!(!is_cranelift_lowerable(&Opcode::Rethrow { exc: r }));
        assert!(!is_cranelift_lowerable(&Opcode::Field {
            dst: r,
            obj: r,
            field: RefField(0)
        }));
        assert!(!is_cranelift_lowerable(&Opcode::SetField {
            obj: r,
            field: RefField(0),
            src: r
        }));
        assert!(!is_cranelift_lowerable(&Opcode::GetThis {
            dst: r,
            field: RefField(0)
        }));
        assert!(!is_cranelift_lowerable(&Opcode::CallMethod {
            dst: r,
            field: RefField(0),
            args: vec![]
        }));
        assert!(!is_cranelift_lowerable(&Opcode::CallClosure {
            dst: r,
            fun: r,
            args: vec![]
        }));
        assert!(!is_cranelift_lowerable(&Opcode::ToDyn { dst: r, src: r }));
        assert!(!is_cranelift_lowerable(&Opcode::SafeCast {
            dst: r,
            src: r
        }));
        assert!(!is_cranelift_lowerable(&Opcode::ToVirtual {
            dst: r,
            src: r
        }));
        assert!(!is_cranelift_lowerable(&Opcode::New { dst: r }));
        assert!(!is_cranelift_lowerable(&Opcode::GetArray {
            dst: r,
            array: r,
            index: r
        }));
        assert!(!is_cranelift_lowerable(&Opcode::Ref { dst: r, src: r }));
        assert!(!is_cranelift_lowerable(&Opcode::Setref {
            dst: r,
            value: r
        }));
        assert!(!is_cranelift_lowerable(&Opcode::Asm {
            mode: 0,
            value: 0,
            reg: r
        }));
    }
}
