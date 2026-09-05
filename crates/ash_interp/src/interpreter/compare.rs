//! Comparing and equality-testing values.
//!
//! HashLink comparison is not one operation but several that share a name:
//! two registers of the same primitive kind, a nullable against a bare value,
//! a UTF-16 string against another, and Dynamic against anything at all --
//! where the answer depends on what is actually boxed rather than on the
//! static type. Gathered here because the cases only make sense beside each
//! other: each one's job is to decide it does not apply and hand on.

use std::ffi::c_void;

use ash_core::bytecode::DecodedBytecode;
use ash_core::hl_bindings as hl;
use ash_core::hl_bindings::hl_type;
use ash_core::types::HLFunction;

use crate::tiering::env_flag;
use crate::values::NanBoxedValue;

use super::{CmpOp, DynamicScalar, HLInterpreter};

impl HLInterpreter {
    /// Helper: compare two register values.
    pub(super) fn compare_regs(
        &self,
        bytecode: &DecodedBytecode,
        func_idx: usize,
        a: u32,
        b: u32,
        op: CmpOp,
    ) -> bool {
        let func = self.air.body(bytecode, func_idx);
        self.compare_regs_in(bytecode, func, func_idx, a, b, op)
    }

    /// [`compare_regs`](Self::compare_regs) against an explicit function.
    ///
    /// The comparison is type-directed — HNULL unboxing, string-object and
    /// dynamic equality all depend on the operands' static kinds — so the SSA
    /// dispatcher in [`crate::ssa`] passes its value-type view here instead of
    /// having a second implementation of those rules.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn compare_regs_in(
        &self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        a: u32,
        b: u32,
        op: CmpOp,
    ) -> bool {
        let frame = self.stack.last().unwrap();
        let va = frame.registers.get(a);
        let vb = frame.registers.get(b);
        let ak = bytecode.types[func.regs[a as usize].0].kind;
        let bk = bytecode.types[func.regs[b as usize].0].kind;
        if let Some(result) = unsafe {
            self.try_compare_nullable_operands(
                bytecode, func, a as usize, va, ak, b as usize, vb, bk, op,
            )
        } {
            if env_flag!("ASH_TRACE_EQ") {
                eprintln!(
                    "[CMP-HNULL] f{} op={:?} ak={} bk={} va={:?} vb={:?} -> {}",
                    func_idx, op, ak, bk, va, vb, result
                );
            }
            return result;
        }
        // The generic NanBox comparison fast path only knows i32 and f64.
        // Type-directed numeric comparison is also required for HI64 and for
        // mixed-width operands (the checks inside Int64.parseString and
        // Int64.toInt are ordinary jump opcodes, not nullable comparisons).
        if let Some(result) = Self::compare_numeric_values(va, ak, vb, bk, op) {
            return result;
        }
        // Haxe lowers relational operators on `Dynamic` values to the same
        // jump opcodes as concrete values.  The registers still have HDYN
        // type, though, so the concrete numeric path above cannot interpret
        // their boxes.  HashLink compares the boxed payloads (and String
        // contents) rather than the box addresses.
        if matches!(
            op,
            CmpOp::SLt | CmpOp::SGt | CmpOp::SLte | CmpOp::SGte | CmpOp::ULt | CmpOp::UGte
        ) && (ak == hl::hl_type_kind_HDYN || bk == hl::hl_type_kind_HDYN)
        {
            if let Some(ord) = unsafe { self.dynamic_value_cmp(va, vb) } {
                return match op {
                    CmpOp::SLt | CmpOp::ULt => ord.is_lt(),
                    CmpOp::SGt => ord.is_gt(),
                    CmpOp::SLte => ord.is_le(),
                    CmpOp::SGte | CmpOp::UGte => ord.is_ge(),
                    _ => unreachable!(),
                };
            }
        }
        // Ordering between strings. Without this the operands fall through to
        // NanBoxedValue::compare, which has no ordering for pointers and
        // answers None -> false, so every `<` and `>` between strings was
        // false. That is not merely a wrong answer: haxe.ds.ArraySort — which
        // is what Array<String>.sort delegates to, there being no native
        // object sort — relies on a consistent comparator, and an always-false
        // one walks its merge off the end of the array and segfaults.
        if matches!(op, CmpOp::SLt | CmpOp::SGt | CmpOp::SLte | CmpOp::SGte) {
            let sa = unsafe { self.string_operand_utf16(va, ak) };
            let sb = unsafe { self.string_operand_utf16(vb, bk) };
            if let (Some((ap, al)), Some((bp, bl))) = (sa, sb) {
                let ord = unsafe { Self::utf16_cmp(ap, al, bp, bl) };
                let result = match op {
                    CmpOp::SLt => ord.is_lt(),
                    CmpOp::SGt => ord.is_gt(),
                    CmpOp::SLte => ord.is_le(),
                    _ => ord.is_ge(),
                };
                if env_flag!("ASH_TRACE_EQ") {
                    eprintln!(
                        "[CMP] f{} op={:?} ak={} bk={} (string-order) -> {}",
                        func_idx, op, ak, bk, result
                    );
                }
                return result;
            }
        }
        if op == CmpOp::Eq || op == CmpOp::NotEq {
            // Upstream `hl_dyn_compare` compares a virtual by its wrapped
            // value (TK2(HOBJ,HVIRTUAL) and friends): a view over an object
            // IS that object for equality. Unwrap before any pointer
            // identity below, or `interface_var == object` is always false.
            let unwrap_view = |v: NanBoxedValue, declared: hl::hl_type_kind| -> NanBoxedValue {
                // Only kinds whose register value starts with an hl_type
                // header may be probed — an HBYTES register holds raw UTF-16
                // data, and a Dynamic register can carry a pointer-shaped
                // immediate (hence the 0x10000 floor other probes here use).
                let headered = matches!(
                    declared,
                    hl::hl_type_kind_HVIRTUAL
                        | hl::hl_type_kind_HDYN
                        | hl::hl_type_kind_HOBJ
                        | hl::hl_type_kind_HDYNOBJ
                );
                if headered && v.is_ptr() && !v.is_null() && !v.is_void() && v.as_ptr() >= 0x10000 {
                    unsafe {
                        let hdr = *(v.as_ptr() as *const *mut hl_type);
                        if !hdr.is_null()
                            && (hdr as usize) >= 0x10000
                            && (*hdr).kind == hl::hl_type_kind_HVIRTUAL
                        {
                            let value = (*(v.as_ptr() as *const hl::vvirtual)).value;
                            if !value.is_null() {
                                return NanBoxedValue::from_ptr(value as usize);
                            }
                        }
                    }
                }
                v
            };
            let va = unwrap_view(va, ak);
            let vb = unwrap_view(vb, bk);
            // Identity after unwrapping settles it for every pointer kind:
            // a view and its object, or two views over one object, are equal.
            // Decided here because the declared-kind arms below want matching
            // kinds on both sides, which a view/object mix never has.
            if va.is_ptr()
                && vb.is_ptr()
                && !va.is_null()
                && !vb.is_null()
                && va.as_ptr() == vb.as_ptr()
            {
                return op == CmpOp::Eq;
            }
            if ak == hl::hl_type_kind_HBYTES && bk == hl::hl_type_kind_HBYTES {
                let pa = if va.is_null() || va.is_void() {
                    std::ptr::null()
                } else {
                    va.as_ptr() as *const u16
                };
                let pb = if vb.is_null() || vb.is_void() {
                    std::ptr::null()
                } else {
                    vb.as_ptr() as *const u16
                };
                let eq = unsafe { Self::utf16z_eq(pa, pb) };
                if env_flag!("ASH_TRACE_EQ") {
                    eprintln!(
                        "[CMP] f{} op={:?} ak={} bk={} (bytes) -> {}",
                        func_idx, op, ak, bk, eq
                    );
                }
                return if op == CmpOp::Eq { eq } else { !eq };
            }
            if ak == hl::hl_type_kind_HOBJ && bk == hl::hl_type_kind_HOBJ {
                let pa = if va.is_null() || va.is_void() {
                    std::ptr::null_mut()
                } else {
                    va.as_ptr() as *mut hl::vdynamic
                };
                let pb = if vb.is_null() || vb.is_void() {
                    std::ptr::null_mut()
                } else {
                    vb.as_ptr() as *mut hl::vdynamic
                };
                if !pa.is_null() && !pb.is_null() {
                    // Same object ⇒ equal, whatever the type — including
                    // String, where identity implies content equality. Also
                    // skips two name decodes on the hot object-compare path.
                    if pa == pb {
                        return op == CmpOp::Eq;
                    }
                    let ta_name = self.dynamic_type_name(pa);
                    let tb_name = self.dynamic_type_name(pb);
                    if env_flag!("ASH_TRACE_EQ") {
                        eprintln!(
                            "[CMP-OBJ] f{} op={:?} ta={:?} tb={:?} pa={:#x} pb={:#x}",
                            func_idx,
                            op,
                            ta_name,
                            tb_name,
                            va.as_ptr(),
                            vb.as_ptr()
                        );
                    }
                    if ta_name == tb_name && matches!(ta_name.as_deref(), Some("String")) {
                        let sa = unsafe {
                            self.try_extract_string_object_raw(va.as_ptr() as *mut c_void)
                        };
                        let sb = unsafe {
                            self.try_extract_string_object_raw(vb.as_ptr() as *mut c_void)
                        };
                        if env_flag!("ASH_TRACE_EQ") {
                            eprintln!(
                                "[CMP-OBJ] f{} string-extract sa={} sb={}",
                                func_idx,
                                sa.is_some(),
                                sb.is_some()
                            );
                        }
                        if let (Some((ab, al)), Some((bb, bl))) = (sa, sb) {
                            let eq = al == bl && unsafe { Self::utf16_len_eq(ab, bb, al as usize) };
                            if env_flag!("ASH_TRACE_EQ") {
                                eprintln!(
                                    "[CMP] f{} op={:?} ak={} bk={} (string-obj) -> {}",
                                    func_idx, op, ak, bk, eq
                                );
                            }
                            return if op == CmpOp::Eq { eq } else { !eq };
                        }
                    }
                }
            }
            // A Dynamic register holding an object carries the raw object
            // pointer, exactly like an HOBJ register.  Optimisation can keep
            // the other operand at its concrete HOBJ kind (`DynamicString ==
            // "literal"` is one such shape), so restricting content-aware
            // comparison to HDYN/HDYN made equal Strings compare unequal.
            // Both layouts have an hl_type header and are safe inputs to
            // dynamic_eq; primitive mixed-kind cases take other paths.
            if (ak == hl::hl_type_kind_HDYN && bk == hl::hl_type_kind_HDYN)
                || (ak == hl::hl_type_kind_HDYN && bk == hl::hl_type_kind_HOBJ)
                || (ak == hl::hl_type_kind_HOBJ && bk == hl::hl_type_kind_HDYN)
            {
                let pa = if !va.is_ptr() || va.is_null() || va.is_void() {
                    std::ptr::null_mut()
                } else {
                    va.as_ptr() as *mut hl::vdynamic
                };
                let pb = if !vb.is_ptr() || vb.is_null() || vb.is_void() {
                    std::ptr::null_mut()
                } else {
                    vb.as_ptr() as *mut hl::vdynamic
                };
                let eq = unsafe { self.dynamic_value_eq(va, vb) };
                if env_flag!("ASH_TRACE_EQ") {
                    eprintln!(
                        "[CMP] f{} op={:?} ak={} bk={} va={:?} vb={:?} (dyn) -> {}",
                        func_idx, op, ak, bk, va, vb, eq
                    );
                    if !eq {
                        let ka_dyn = if pa.is_null()
                            || !Self::is_derefable_dynamic(pa)
                            || unsafe { (*pa).t.is_null() }
                        {
                            0
                        } else {
                            unsafe { (*(*pa).t).kind }
                        };
                        let kb_dyn = if pb.is_null()
                            || !Self::is_derefable_dynamic(pb)
                            || unsafe { (*pb).t.is_null() }
                        {
                            0
                        } else {
                            unsafe { (*(*pb).t).kind }
                        };
                        eprintln!(
                            "[CMP_DYN] ka_dyn={} kb_dyn={} pa={:#x} pb={:#x}",
                            ka_dyn, kb_dyn, pa as usize, pb as usize
                        );
                    }
                }
                return if op == CmpOp::Eq { eq } else { !eq };
            }
        }
        let result = va.compare(vb, op).unwrap_or(false);
        if env_flag!("ASH_TRACE_EQ") && (op == CmpOp::Eq || op == CmpOp::NotEq) {
            eprintln!(
                "[CMP] f{} op={:?} ak={} bk={} va={:?} vb={:?} -> {}",
                func_idx, op, ak, bk, va, vb, result
            );
        }
        result
    }

    #[allow(clippy::too_many_arguments)]
    unsafe fn try_compare_nullable_operands(
        &self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        a_idx: usize,
        va: NanBoxedValue,
        ak: hl::hl_type_kind,
        b_idx: usize,
        vb: NanBoxedValue,
        bk: hl::hl_type_kind,
        op: CmpOp,
    ) -> Option<bool> {
        if ak != hl::hl_type_kind_HNULL && bk != hl::hl_type_kind_HNULL {
            return None;
        }

        let (av, ak_eff) =
            self.normalize_nullable_compare_operand(bytecode, func, a_idx, ak, va)?;
        let (bv, bk_eff) =
            self.normalize_nullable_compare_operand(bytecode, func, b_idx, bk, vb)?;

        if av.is_none() || bv.is_none() {
            let eq = av.is_none() && bv.is_none();
            return Some(match op {
                CmpOp::Eq => eq,
                CmpOp::NotEq => !eq,
                _ => false,
            });
        }

        let av = av.unwrap();
        let bv = bv.unwrap();
        if let Some(result) = Self::compare_numeric_values(av, ak_eff, bv, bk_eff, op) {
            return Some(result);
        }

        if op == CmpOp::Eq || op == CmpOp::NotEq {
            if let Some(result) = av.compare(bv, op) {
                return Some(result);
            }
        }

        None
    }

    unsafe fn normalize_nullable_compare_operand(
        &self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        reg_idx: usize,
        reg_kind: hl::hl_type_kind,
        val: NanBoxedValue,
    ) -> Option<(Option<NanBoxedValue>, hl::hl_type_kind)> {
        if reg_kind != hl::hl_type_kind_HNULL {
            return Some((Some(val), reg_kind));
        }
        if val.is_null() || val.is_void() || (val.is_ptr() && val.as_ptr() == 0) {
            return Some((None, reg_kind));
        }
        if !val.is_ptr() {
            return Some((Some(val), reg_kind));
        }

        let reg_type_idx = match func.regs.get(reg_idx) {
            Some(r) => r.0,
            None => return Some((Some(val), reg_kind)),
        };
        let reg_type = match bytecode.types.get(reg_type_idx) {
            Some(t) => t,
            None => return Some((Some(val), reg_kind)),
        };
        let tparam_idx = match reg_type.tparam.as_ref() {
            Some(tp) => tp.0,
            None => return Some((Some(val), reg_kind)),
        };
        let inner_kind = match bytecode.types.get(tparam_idx) {
            Some(t) => t.kind,
            None => return Some((Some(val), reg_kind)),
        };

        if !Self::is_primitive_or_bytes_kind(inner_kind) {
            return Some((Some(val), reg_kind));
        }

        let d = val.as_ptr() as *mut hl::vdynamic;
        if d.is_null() {
            return Some((None, inner_kind));
        }
        if let Some(unboxed) = Self::unbox_dynamic_to_kind(d, inner_kind) {
            if unboxed.is_null() || unboxed.is_void() {
                return Some((None, inner_kind));
            }
            return Some((Some(unboxed), inner_kind));
        }

        Some((Some(val), reg_kind))
    }

    fn compare_numeric_values(
        av: NanBoxedValue,
        ak: hl::hl_type_kind,
        bv: NanBoxedValue,
        bk: hl::hl_type_kind,
        op: CmpOp,
    ) -> Option<bool> {
        if !Self::is_numeric_or_bool_kind(ak) || !Self::is_numeric_or_bool_kind(bk) {
            return None;
        }

        let has_float = ak == hl::hl_type_kind_HF32
            || ak == hl::hl_type_kind_HF64
            || bk == hl::hl_type_kind_HF32
            || bk == hl::hl_type_kind_HF64;

        if has_float {
            let l = Self::numeric_as_f64(av, ak)?;
            let r = Self::numeric_as_f64(bv, bk)?;
            return Some(match op {
                CmpOp::SLt | CmpOp::ULt => l < r,
                CmpOp::SGte | CmpOp::UGte => l >= r,
                CmpOp::SGt => l > r,
                CmpOp::SLte => l <= r,
                CmpOp::Eq => l == r,
                CmpOp::NotEq => l != r,
            });
        }

        match op {
            CmpOp::ULt | CmpOp::UGte => {
                let l = Self::numeric_as_u64(av, ak)?;
                let r = Self::numeric_as_u64(bv, bk)?;
                Some(match op {
                    CmpOp::ULt => l < r,
                    CmpOp::UGte => l >= r,
                    _ => unreachable!(),
                })
            }
            CmpOp::SLt | CmpOp::SGte | CmpOp::SGt | CmpOp::SLte | CmpOp::Eq | CmpOp::NotEq => {
                let l = Self::numeric_as_i64(av, ak)?;
                let r = Self::numeric_as_i64(bv, bk)?;
                Some(match op {
                    CmpOp::SLt => l < r,
                    CmpOp::SGte => l >= r,
                    CmpOp::SGt => l > r,
                    CmpOp::SLte => l <= r,
                    CmpOp::Eq => l == r,
                    CmpOp::NotEq => l != r,
                    _ => unreachable!(),
                })
            }
        }
    }

    fn numeric_as_f64(v: NanBoxedValue, kind: hl::hl_type_kind) -> Option<f64> {
        match kind {
            k if k == hl::hl_type_kind_HI32 => Some(v.as_i32() as f64),
            k if k == hl::hl_type_kind_HUI8 => Some((v.as_i32() as u8) as f64),
            k if k == hl::hl_type_kind_HUI16 => Some((v.as_i32() as u16) as f64),
            k if k == hl::hl_type_kind_HI64 => Some(v.as_i64_lossy() as f64),
            k if k == hl::hl_type_kind_HF32 || k == hl::hl_type_kind_HF64 => Some(v.as_f64()),
            k if k == hl::hl_type_kind_HBOOL => Some(if v.as_bool() { 1.0 } else { 0.0 }),
            _ => None,
        }
    }

    fn numeric_as_i64(v: NanBoxedValue, kind: hl::hl_type_kind) -> Option<i64> {
        match kind {
            k if k == hl::hl_type_kind_HI32 => Some(v.as_i32() as i64),
            k if k == hl::hl_type_kind_HUI8 => Some((v.as_i32() as u8) as i64),
            k if k == hl::hl_type_kind_HUI16 => Some((v.as_i32() as u16) as i64),
            k if k == hl::hl_type_kind_HI64 => Some(v.as_i64_lossy()),
            k if k == hl::hl_type_kind_HBOOL => Some(if v.as_bool() { 1 } else { 0 }),
            _ => None,
        }
    }

    fn numeric_as_u64(v: NanBoxedValue, kind: hl::hl_type_kind) -> Option<u64> {
        match kind {
            k if k == hl::hl_type_kind_HI32 => Some((v.as_i32() as u32) as u64),
            k if k == hl::hl_type_kind_HUI8 => Some((v.as_i32() as u8) as u64),
            k if k == hl::hl_type_kind_HUI16 => Some((v.as_i32() as u16) as u64),
            k if k == hl::hl_type_kind_HI64 => Some(v.as_i64_lossy() as u64),
            k if k == hl::hl_type_kind_HBOOL => Some(if v.as_bool() { 1 } else { 0 }),
            _ => None,
        }
    }

    unsafe fn utf16z_eq(a: *const u16, b: *const u16) -> bool {
        if a == b {
            return true;
        }
        if a.is_null() || b.is_null() {
            return false;
        }
        let mut i = 0usize;
        loop {
            let ca = *a.add(i);
            let cb = *b.add(i);
            if ca != cb {
                return false;
            }
            if ca == 0 {
                return true;
            }
            i += 1;
        }
    }

    /// The UTF-16 buffer behind a comparison operand, for HBYTES (which is
    /// NUL-terminated) and for a String object (which carries an explicit
    /// length). Returns None for anything that is not a string.
    unsafe fn string_operand_utf16(
        &self,
        v: NanBoxedValue,
        kind: hl::hl_type_kind,
    ) -> Option<(*const u16, i32)> {
        if v.is_null() || v.is_void() {
            return None;
        }
        if kind == hl::hl_type_kind_HBYTES {
            let p = v.as_ptr() as *const u16;
            if p.is_null() {
                return None;
            }
            let mut n = 0i32;
            while *p.add(n as usize) != 0 {
                n += 1;
            }
            return Some((p, n));
        }
        if kind == hl::hl_type_kind_HOBJ {
            let name = self.dynamic_type_name(v.as_ptr() as *mut hl::vdynamic);
            if !matches!(name.as_deref(), Some("String")) {
                return None;
            }
            return self.try_extract_string_object_raw(v.as_ptr() as *mut c_void);
        }
        None
    }

    /// Lexicographic order over UTF-16 code units, shorter-is-less on a
    /// common prefix — the ordering `hl_dyn_compare` gives strings, and the
    /// one Haxe's `<` on String is defined to produce.
    unsafe fn utf16_cmp(a: *const u16, alen: i32, b: *const u16, blen: i32) -> std::cmp::Ordering {
        let n = alen.min(blen).max(0) as usize;
        for i in 0..n {
            let (x, y) = (*a.add(i), *b.add(i));
            if x != y {
                return x.cmp(&y);
            }
        }
        alen.cmp(&blen)
    }

    unsafe fn utf16_len_eq(a: *const u16, b: *const u16, len: usize) -> bool {
        if a.is_null() || b.is_null() {
            return false;
        }
        for i in 0..len {
            if *a.add(i) != *b.add(i) {
                return false;
            }
        }
        true
    }

    unsafe fn try_extract_string_object_raw(
        &self,
        obj_ptr: *mut c_void,
    ) -> Option<(*const u16, i32)> {
        if obj_ptr.is_null() || self.fn_get_obj_rt.is_null() {
            return None;
        }
        let type_ptr = *(obj_ptr as *const *mut hl::hl_type);
        if type_ptr.is_null() || (*type_ptr).kind != hl::hl_type_kind_HOBJ {
            return None;
        }
        let bytes_val = Self::read_obj_field(
            obj_ptr as *mut u8,
            0,
            hl::hl_type_kind_HBYTES,
            type_ptr as *mut c_void,
            hl::hl_type_kind_HOBJ,
            self.fn_get_obj_rt,
        );
        let len_val = Self::read_obj_field(
            obj_ptr as *mut u8,
            1,
            hl::hl_type_kind_HI32,
            type_ptr as *mut c_void,
            hl::hl_type_kind_HOBJ,
            self.fn_get_obj_rt,
        );
        if bytes_val.is_null() || len_val.is_null() || len_val.is_void() {
            return None;
        }
        let bytes = bytes_val.as_ptr() as *const u16;
        let len = len_val.as_i32();
        if bytes.is_null() || len < 0 {
            return None;
        }
        Some((bytes, len))
    }

    unsafe fn dynamic_eq(&self, a: *mut hl::vdynamic, b: *mut hl::vdynamic) -> bool {
        if a == b {
            return true;
        }
        if a.is_null() || b.is_null() {
            return false;
        }
        // Unboxed payloads in Dynamic slots are not boxes — see
        // is_derefable_dynamic. Distinct non-box words are simply unequal
        // (the identity case above already answered equal ones).
        if !Self::is_derefable_dynamic(a) || !Self::is_derefable_dynamic(b) {
            return false;
        }
        let ta = (*a).t;
        let tb = (*b).t;
        if ta.is_null() || tb.is_null() {
            return false;
        }
        let ka = (*ta).kind;
        let kb = (*tb).kind;
        match (ka, kb) {
            (ka, kb)
                if matches!(ka, hl::hl_type_kind_HOBJ | hl::hl_type_kind_HDYNOBJ)
                    && kb == hl::hl_type_kind_HVIRTUAL =>
            {
                let value = (*(b as *mut hl::vvirtual)).value;
                return !value.is_null() && self.dynamic_eq(a, value);
            }
            (ka, kb)
                if ka == hl::hl_type_kind_HVIRTUAL
                    && matches!(kb, hl::hl_type_kind_HOBJ | hl::hl_type_kind_HDYNOBJ) =>
            {
                let value = (*(a as *mut hl::vvirtual)).value;
                return !value.is_null() && self.dynamic_eq(value, b);
            }
            (ka, kb) if ka == hl::hl_type_kind_HVIRTUAL && kb == hl::hl_type_kind_HVIRTUAL => {
                let av = (*(a as *mut hl::vvirtual)).value;
                let bv = (*(b as *mut hl::vvirtual)).value;
                // HashLink reports an invalid comparison for two distinct
                // self-backed virtual records. For equality that means false,
                // not "compare their null value slots as equal".
                return !av.is_null() && !bv.is_null() && self.dynamic_eq(av, bv);
            }
            _ => {}
        }
        if ka == kb {
            return match ka {
                k if k == hl::hl_type_kind_HI32 => (*a).v.i == (*b).v.i,
                k if k == hl::hl_type_kind_HUI8 => (*a).v.ui8 == (*b).v.ui8,
                k if k == hl::hl_type_kind_HUI16 => (*a).v.ui16 == (*b).v.ui16,
                k if k == hl::hl_type_kind_HI64 => (*a).v.i64_ == (*b).v.i64_,
                k if k == hl::hl_type_kind_HF32 => (*a).v.f == (*b).v.f,
                k if k == hl::hl_type_kind_HF64 => (*a).v.d == (*b).v.d,
                k if k == hl::hl_type_kind_HBOOL => (*a).v.b == (*b).v.b,
                k if k == hl::hl_type_kind_HBYTES => {
                    Self::utf16z_eq((*a).v.bytes as *const u16, (*b).v.bytes as *const u16)
                }
                _ => {
                    if ka == hl::hl_type_kind_HOBJ {
                        let ta_name = self.dynamic_type_name(a);
                        let tb_name = self.dynamic_type_name(b);
                        if ta_name == tb_name && matches!(ta_name.as_deref(), Some("String")) {
                            if let (Some((ab, al)), Some((bb, bl))) = (
                                self.try_extract_string_object_raw(a.cast()),
                                self.try_extract_string_object_raw(b.cast()),
                            ) {
                                return al == bl && Self::utf16_len_eq(ab, bb, al as usize);
                            }
                        }

                        // `a` and `b` are the objects themselves, not boxes
                        // whose payload starts at `v.ptr`.  Reading that union
                        // member therefore reads offset 8 of the object -- its
                        // first field.  Distinct objects with the same first
                        // field consequently compared equal (two IntWrap(1)
                        // instances made Array.remove remove the wrong one).
                        // Strings are the content-equality exception handled
                        // above; every other object uses identity, matching
                        // hlp_dyn_compare's HOBJ fallback.
                        return false;
                    }
                    if ka == hl::hl_type_kind_HENUM {
                        // Enum values are heap objects whose first word is
                        // their hl_type*.  They are not vdynamic boxes, so
                        // reading `v.ptr` observes the constructor index at
                        // offset 8.  That made any two zero-argument enum
                        // values with the same constructor index compare
                        // equal, even when they belonged to different enum
                        // types.  HashLink's HENUM/HENUM comparison is pointer
                        // identity; the equal-pointer case was handled above.
                        return false;
                    }
                    (*a).v.ptr == (*b).v.ptr
                }
            };
        }
        // Cross-kind numeric equality (e.g. Int dynamic vs Float dynamic)
        let a_num = match ka {
            k if k == hl::hl_type_kind_HI32 => Some((*a).v.i as f64),
            k if k == hl::hl_type_kind_HUI8 => Some((*a).v.ui8 as f64),
            k if k == hl::hl_type_kind_HUI16 => Some((*a).v.ui16 as f64),
            k if k == hl::hl_type_kind_HI64 => Some((*a).v.i64_ as f64),
            k if k == hl::hl_type_kind_HF32 => Some((*a).v.f as f64),
            k if k == hl::hl_type_kind_HF64 => Some((*a).v.d),
            _ => None,
        };
        let b_num = match kb {
            k if k == hl::hl_type_kind_HI32 => Some((*b).v.i as f64),
            k if k == hl::hl_type_kind_HUI8 => Some((*b).v.ui8 as f64),
            k if k == hl::hl_type_kind_HUI16 => Some((*b).v.ui16 as f64),
            k if k == hl::hl_type_kind_HI64 => Some((*b).v.i64_ as f64),
            k if k == hl::hl_type_kind_HF32 => Some((*b).v.f as f64),
            k if k == hl::hl_type_kind_HF64 => Some((*b).v.d),
            _ => None,
        };
        match (a_num, b_num) {
            (Some(x), Some(y)) => x == y,
            _ => false,
        }
    }

    unsafe fn dynamic_value_eq(&self, a: NanBoxedValue, b: NanBoxedValue) -> bool {
        if a.is_null() || a.is_void() || b.is_null() || b.is_void() {
            return (a.is_null() || a.is_void()) && (b.is_null() || b.is_void());
        }
        if a.is_ptr() && b.is_ptr() {
            return self.dynamic_eq(
                a.as_ptr() as *mut hl::vdynamic,
                b.as_ptr() as *mut hl::vdynamic,
            );
        }
        if a.raw_bits() == b.raw_bits() {
            return true;
        }

        match (Self::dynamic_scalar(a), Self::dynamic_scalar(b)) {
            (Some(DynamicScalar::Int(x)), Some(DynamicScalar::Int(y))) => x == y,
            (Some(DynamicScalar::Float(x)), Some(DynamicScalar::Float(y))) => x == y,
            (Some(DynamicScalar::Int(x)), Some(DynamicScalar::Float(y))) => x as f64 == y,
            (Some(DynamicScalar::Float(x)), Some(DynamicScalar::Int(y))) => x == y as f64,
            (Some(DynamicScalar::Bool(x)), Some(DynamicScalar::Bool(y))) => x == y,
            _ => false,
        }
    }

    unsafe fn dynamic_value_cmp(
        &self,
        a: NanBoxedValue,
        b: NanBoxedValue,
    ) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering;

        let a_null = a.is_null() || a.is_void();
        let b_null = b.is_null() || b.is_void();
        if a_null || b_null {
            return Some(match (a_null, b_null) {
                (true, true) => Ordering::Equal,
                (true, false) => Ordering::Less,
                (false, true) => Ordering::Greater,
                _ => unreachable!(),
            });
        }
        if a.raw_bits() == b.raw_bits() {
            return Some(Ordering::Equal);
        }

        let scalar_number = |v| match v {
            DynamicScalar::Int(x) => x as f64,
            DynamicScalar::Float(x) => x,
            DynamicScalar::Bool(x) => {
                if x {
                    1.0
                } else {
                    0.0
                }
            }
        };
        if let (Some(x), Some(y)) = (Self::dynamic_scalar(a), Self::dynamic_scalar(b)) {
            // Match hl_dyn_compare: NaN is neither less nor greater, so it
            // compares equal for ordering purposes.
            let (x, y) = (scalar_number(x), scalar_number(y));
            return Some(if x < y {
                Ordering::Less
            } else if x > y {
                Ordering::Greater
            } else {
                Ordering::Equal
            });
        }

        if a.is_ptr() && b.is_ptr() {
            let (ap, bp) = (
                a.as_ptr() as *mut hl::vdynamic,
                b.as_ptr() as *mut hl::vdynamic,
            );
            if Self::is_derefable_dynamic(ap)
                && Self::is_derefable_dynamic(bp)
                && !(*ap).t.is_null()
                && !(*bp).t.is_null()
            {
                let (ak, bk) = ((*(*ap).t).kind, (*(*bp).t).kind);
                if ak == hl::hl_type_kind_HBYTES && bk == hl::hl_type_kind_HBYTES {
                    let (ab, bb) = ((*ap).v.bytes as *const u16, (*bp).v.bytes as *const u16);
                    let mut al = 0i32;
                    let mut bl = 0i32;
                    while !ab.is_null() && *ab.add(al as usize) != 0 {
                        al += 1;
                    }
                    while !bb.is_null() && *bb.add(bl as usize) != 0 {
                        bl += 1;
                    }
                    return Some(Self::utf16_cmp(ab, al, bb, bl));
                }
                if ak == hl::hl_type_kind_HOBJ && bk == hl::hl_type_kind_HOBJ {
                    if let (Some((ab, al)), Some((bb, bl))) = (
                        self.try_extract_string_object_raw(ap.cast()),
                        self.try_extract_string_object_raw(bp.cast()),
                    ) {
                        return Some(Self::utf16_cmp(ab, al, bb, bl));
                    }
                }
            }
            return Some(a.as_ptr().cmp(&b.as_ptr()));
        }

        None
    }

    unsafe fn dynamic_scalar(v: NanBoxedValue) -> Option<DynamicScalar> {
        if v.is_i32() {
            return Some(DynamicScalar::Int(v.as_i32() as i64));
        }
        if v.is_i64() {
            return Some(DynamicScalar::Int(v.as_i64_lossy()));
        }
        if v.is_f64() {
            return Some(DynamicScalar::Float(v.as_f64()));
        }
        if v.is_bool() {
            return Some(DynamicScalar::Bool(v.as_bool()));
        }
        if !v.is_ptr() {
            return None;
        }

        let d = v.as_ptr() as *mut hl::vdynamic;
        if !Self::is_derefable_dynamic(d) || (*d).t.is_null() {
            return None;
        }
        match (*(*d).t).kind {
            hl::hl_type_kind_HI32 => Some(DynamicScalar::Int((*d).v.i as i64)),
            hl::hl_type_kind_HUI8 => Some(DynamicScalar::Int((*d).v.ui8 as i64)),
            hl::hl_type_kind_HUI16 => Some(DynamicScalar::Int((*d).v.ui16 as i64)),
            hl::hl_type_kind_HI64 => Some(DynamicScalar::Int((*d).v.i64_)),
            hl::hl_type_kind_HF32 => Some(DynamicScalar::Float((*d).v.f as f64)),
            hl::hl_type_kind_HF64 => Some(DynamicScalar::Float((*d).v.d)),
            hl::hl_type_kind_HBOOL => Some(DynamicScalar::Bool((*d).v.b)),
            _ => None,
        }
    }
}
