//! # Writing HashLink bytecode back out
//!
//! The inverse of [`BytecodeDecoder`](crate::bytecode::BytecodeDecoder), so an
//! optimized module can leave ash as a `.hl` file that stock `hl`, HL/C or
//! hl2 will run. AIR already produces what this needs: `optimized()` hands
//! back a serialized form whose `ops` and `reg_types` ARE HL opcodes and
//! register types, because that is what the interpreter walks.
//!
//! ## Two deliberate losses
//!
//! **Debug info is not written.** The decoder discards the `assigns` table
//! (`// skip assigns (no need here)` in `read_functions`), so a debug-carrying
//! module cannot be reproduced faithfully. Rather than emit a half-truth, the
//! output sets `flags = 0`. That also happens to be the honest choice for an
//! *optimized* module: once the inliner has run, a pc->line mapping describes
//! a function the source no longer has.
//!
//! **String indices are rebuilt, not preserved.** `read_string` resolves a
//! table index to a `String` and drops the index, so an identical string that
//! appeared twice in the original table collapses to one index here. The
//! output is therefore semantically equal to the input, not byte-identical,
//! and the round-trip test compares decoded structures rather than bytes.

use std::collections::HashMap;
use std::io::{self, Write};

use byteorder::{LittleEndian, WriteBytesExt};

use crate::bytecode::DecodedBytecode;
use crate::hl::{self, *};
use crate::opcodes::Opcode;
use crate::types::{HLType, OP_NARGS};

/// Encode `bc` as HashLink bytecode of `version` (4 or 5).
pub fn encode(bc: &DecodedBytecode, version: usize) -> io::Result<Vec<u8>> {
    Encoder::new(bc, version).run()
}

struct Encoder<'b> {
    bc: &'b DecodedBytecode,
    version: usize,
    out: Vec<u8>,
    /// First index of each distinct string, for the name fields that are
    /// stored as table indices.
    string_index: HashMap<&'b str, usize>,
}

impl<'b> Encoder<'b> {
    fn new(bc: &'b DecodedBytecode, version: usize) -> Self {
        let mut string_index = HashMap::with_capacity(bc.strings.len());
        for (i, s) in bc.strings.iter().enumerate() {
            string_index.entry(s.as_str()).or_insert(i);
        }
        Encoder {
            bc,
            version,
            out: Vec::new(),
            string_index,
        }
    }

    // ── primitives ──────────────────────────────────────────────────────────

    /// The decoder's `read_var_int`, inverted: one byte for 0..=127, two for a
    /// signed 13-bit magnitude, four for a signed 29-bit one, with the sign in
    /// bit 0x20 and the continuation shape in bits 0x80/0x40.
    fn var_int(&mut self, v: i32) {
        let neg = v < 0;
        let m = v.unsigned_abs();
        if !neg && m < 0x80 {
            self.out.push(m as u8);
        } else if m < 0x2000 {
            let sign = if neg { 0x20 } else { 0 };
            self.out.push(0x80 | sign | ((m >> 8) as u8 & 0x1F));
            self.out.push((m & 0xFF) as u8);
        } else {
            let sign = if neg { 0x20 } else { 0 };
            self.out.push(0xC0 | sign | ((m >> 24) as u8 & 0x1F));
            self.out.push(((m >> 16) & 0xFF) as u8);
            self.out.push(((m >> 8) & 0xFF) as u8);
            self.out.push((m & 0xFF) as u8);
        }
    }

    fn var_u(&mut self, v: u32) {
        self.var_int(v as i32);
    }

    fn type_ref(&mut self, t: &crate::types::TypeRef) {
        self.var_int(t.0 as i32);
    }

    /// A name field: the decoder reads an index into the string table.
    fn string_ref(&mut self, s: &str) -> io::Result<()> {
        let idx = *self.string_index.get(s).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("string {s:?} is not in the module's string table"),
            )
        })?;
        self.var_int(idx as i32);
        Ok(())
    }

    // ── sections ────────────────────────────────────────────────────────────

    fn run(mut self) -> io::Result<Vec<u8>> {
        if self.version < 4 || self.version > 5 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("unsupported output version {}", self.version),
            ));
        }
        self.header();
        self.ints();
        self.floats();
        self.strings()?;
        if self.version >= 5 {
            self.bytes();
        }
        // No debug files: `flags` says the module carries no debug info.
        for i in 0..self.bc.types.len() {
            self.write_type(i)?;
        }
        for g in &self.bc.globals.clone() {
            self.type_ref(g);
        }
        self.natives()?;
        self.functions()?;
        self.constants();
        Ok(self.out)
    }

    fn header(&mut self) {
        self.out.extend_from_slice(b"HLB");
        self.out.push(self.version as u8);
        self.var_u(0); // flags: bit 0 is has_debug, and we write none
        self.var_u(self.bc.ints.len() as u32);
        self.var_u(self.bc.floats.len() as u32);
        self.var_u(self.bc.strings.len() as u32);
        if self.version >= 5 {
            self.var_u(self.bc.bytes_pos.len() as u32);
        }
        self.var_u(self.bc.types.len() as u32);
        self.var_u(self.bc.globals.len() as u32);
        self.var_u(self.bc.natives.len() as u32);
        self.var_u(self.bc.functions.len() as u32);
        if self.version >= 4 {
            self.var_u(self.bc.constants.len() as u32);
        }
        self.var_u(self.bc.entrypoint);
    }

    fn ints(&mut self) {
        for v in &self.bc.ints.clone() {
            self.out.write_i32::<LittleEndian>(*v).expect("vec write");
        }
    }

    fn floats(&mut self) {
        for v in &self.bc.floats.clone() {
            self.out.write_f64::<LittleEndian>(*v).expect("vec write");
        }
    }

    /// Size-prefixed blob of null-terminated UTF-8, then one `var_u` per
    /// string giving its length without the terminator.
    fn strings(&mut self) -> io::Result<()> {
        let mut blob = Vec::new();
        for s in &self.bc.strings {
            blob.extend_from_slice(s.as_bytes());
            blob.push(0);
        }
        self.out
            .write_i32::<LittleEndian>(blob.len() as i32)
            .expect("vec write");
        self.out.extend_from_slice(&blob);
        let lens: Vec<u32> = self.bc.strings.iter().map(|s| s.len() as u32).collect();
        for l in lens {
            self.var_u(l);
        }
        Ok(())
    }

    fn bytes(&mut self) {
        self.out
            .write_i32::<LittleEndian>(self.bc.bytes_data.len() as i32)
            .expect("vec write");
        self.out.extend_from_slice(&self.bc.bytes_data);
        for p in &self.bc.bytes_pos.clone() {
            self.var_u(*p as u32);
        }
    }

    fn natives(&mut self) -> io::Result<()> {
        for n in &self.bc.natives.clone() {
            self.string_ref(&n.lib)?;
            self.string_ref(&n.name)?;
            self.type_ref(&n.type_);
            self.var_u(n.findex as u32);
        }
        Ok(())
    }

    fn functions(&mut self) -> io::Result<()> {
        for f in &self.bc.functions.clone() {
            self.type_ref(&f.type_);
            self.var_u(f.findex as u32);
            self.var_u(f.regs.len() as u32);
            self.var_u(f.ops.len() as u32);
            for r in &f.regs {
                self.type_ref(r);
            }
            for op in &f.ops {
                self.opcode(op)?;
            }
            // No debug infos and no assigns: see the module comment.
        }
        Ok(())
    }

    fn constants(&mut self) {
        if self.version < 4 {
            return;
        }
        for c in &self.bc.constants.clone() {
            self.var_u(c.global);
            self.var_u(c.fields.len() as u32);
            for f in &c.fields {
                self.var_u(*f as u32);
            }
        }
    }

    fn write_type(&mut self, idx: usize) -> io::Result<()> {
        let t: HLType = self.bc.types[idx].clone();
        self.out.push(t.kind as u8);
        match t.kind {
            hl::hl_type_kind_HFUN | hl::hl_type_kind_HMETHOD => {
                let fun = t.fun.as_ref().ok_or_else(|| bad("HFUN without fun"))?;
                self.var_u(fun.args.len() as u32);
                for a in &fun.args {
                    self.type_ref(a);
                }
                self.type_ref(&fun.ret);
            }
            hl::hl_type_kind_HOBJ | hl::hl_type_kind_HSTRUCT => {
                let o = t
                    .obj
                    .as_ref()
                    .ok_or_else(|| bad("HOBJ without obj"))?
                    .clone();
                self.string_ref(&o.name)?;
                self.var_int(o.super_.map(|s| s.0 as i32).unwrap_or(-1));
                self.var_u(o.global_value);
                self.var_u(o.fields.len() as u32);
                self.var_u(o.proto.len() as u32);
                self.var_u((o.bindings.len() / 2) as u32);
                for f in &o.fields {
                    self.string_ref(&f.name)?;
                    self.type_ref(&f.type_);
                }
                for p in &o.proto {
                    self.string_ref(&p.name)?;
                    self.var_u(p.findex as u32);
                    self.var_int(p.pindex);
                }
                for b in &o.bindings {
                    self.var_u(*b as u32);
                }
            }
            hl::hl_type_kind_HREF | hl::hl_type_kind_HNULL | hl::hl_type_kind_HPACKED => {
                self.type_ref(
                    &t.tparam
                        .clone()
                        .ok_or_else(|| bad("wrapper without tparam"))?,
                );
            }
            hl::hl_type_kind_HVIRTUAL => {
                let v = t
                    .virt
                    .as_ref()
                    .ok_or_else(|| bad("HVIRTUAL without virt"))?
                    .clone();
                self.var_u(v.fields.len() as u32);
                for f in &v.fields {
                    self.string_ref(&f.name)?;
                    self.type_ref(&f.type_);
                }
            }
            hl::hl_type_kind_HENUM => {
                let e = t
                    .tenum
                    .as_ref()
                    .ok_or_else(|| bad("HENUM without tenum"))?
                    .clone();
                self.string_ref(&e.name)?;
                self.var_int(e.global_value as i32);
                self.var_u(e.constructs.len() as u32);
                for c in &e.constructs {
                    self.string_ref(&c.name)?;
                    self.var_u(c.params.len() as u32);
                    for p in &c.params {
                        self.type_ref(p);
                    }
                }
            }
            hl::hl_type_kind_HABSTRACT => {
                let n = t
                    .abs_name
                    .clone()
                    .ok_or_else(|| bad("HABSTRACT without name"))?;
                self.string_ref(&n)?;
            }
            _ => {}
        }
        Ok(())
    }

    /// Mirror of `read_opcode`: the operand count comes from `OP_NARGS`, and
    /// the variable-argument shapes write their count before their entries.
    fn opcode(&mut self, op: &Opcode) -> io::Result<()> {
        let (code, p1, p2, p3, extras) = Self::raw(op)?;
        self.out.push(code as u8);
        let nargs = OP_NARGS[code as usize];
        match nargs {
            0 => {}
            1 => self.var_int(p1),
            2 => {
                self.var_int(p1);
                self.var_int(p2);
            }
            3 => {
                self.var_int(p1);
                self.var_int(p2);
                self.var_int(p3);
            }
            4 => {
                self.var_int(p1);
                self.var_int(p2);
                self.var_int(p3);
                self.var_int(extras[0]);
            }
            -1 => match code {
                hl_op_OCallN | hl_op_OCallClosure | hl_op_OCallMethod | hl_op_OCallThis
                | hl_op_OMakeEnum => {
                    self.var_int(p1);
                    self.var_int(p2);
                    self.out.push(p3 as u8);
                    for e in &extras {
                        self.var_int(*e);
                    }
                }
                hl_op_OSwitch => {
                    // Every field here is read back with `read_var_u`, so a
                    // negative one would decode as an error rather than a
                    // wrapped value. Say which it was.
                    if p1 < 0 || p2 < 0 || p3 < 0 || extras.iter().any(|e| *e < 0) {
                        return Err(bad(&format!(
                            "Switch has a negative field: reg={p1} n={p2} end={p3} offsets={extras:?}"
                        )));
                    }
                    self.var_u(p1 as u32);
                    self.var_u(p2 as u32);
                    for e in &extras {
                        self.var_u(*e as u32);
                    }
                    self.var_u(p3 as u32);
                }
                _ => {
                    return Err(bad("variable-argument opcode has no writer"));
                }
            },
            _ => {
                self.var_int(p1);
                self.var_int(p2);
                self.var_int(p3);
                for e in &extras {
                    self.var_int(*e);
                }
            }
        }
        Ok(())
    }

    /// `Opcode` -> the `(op, p1, p2, p3, extras)` shape the file stores.
    /// Generated from the decoder's own match, so the two stay in step.
    fn raw(op: &Opcode) -> io::Result<(hl_op, i32, i32, i32, Vec<i32>)> {
        Ok(match op {
            Opcode::Mov { dst, src } => (hl_op_OMov, dst.0 as i32, src.0 as i32, 0, Vec::new()),
            Opcode::Int { dst, ptr } => (hl_op_OInt, dst.0 as i32, ptr.0 as i32, 0, Vec::new()),
            Opcode::Float { dst, ptr } => (hl_op_OFloat, dst.0 as i32, ptr.0 as i32, 0, Vec::new()),
            Opcode::Bool { dst, value } => (
                hl_op_OBool,
                dst.0 as i32,
                if *value { 1 } else { 0 },
                0,
                Vec::new(),
            ),
            Opcode::Bytes { dst, ptr } => (hl_op_OBytes, dst.0 as i32, ptr.0 as i32, 0, Vec::new()),
            Opcode::String { dst, ptr } => {
                (hl_op_OString, dst.0 as i32, ptr.0 as i32, 0, Vec::new())
            }
            Opcode::Null { dst } => (hl_op_ONull, dst.0 as i32, 0, 0, Vec::new()),
            Opcode::Add { dst, a, b } => {
                (hl_op_OAdd, dst.0 as i32, a.0 as i32, b.0 as i32, Vec::new())
            }
            Opcode::Sub { dst, a, b } => {
                (hl_op_OSub, dst.0 as i32, a.0 as i32, b.0 as i32, Vec::new())
            }
            Opcode::Mul { dst, a, b } => {
                (hl_op_OMul, dst.0 as i32, a.0 as i32, b.0 as i32, Vec::new())
            }
            Opcode::SDiv { dst, a, b } => (
                hl_op_OSDiv,
                dst.0 as i32,
                a.0 as i32,
                b.0 as i32,
                Vec::new(),
            ),
            Opcode::UDiv { dst, a, b } => (
                hl_op_OUDiv,
                dst.0 as i32,
                a.0 as i32,
                b.0 as i32,
                Vec::new(),
            ),
            Opcode::SMod { dst, a, b } => (
                hl_op_OSMod,
                dst.0 as i32,
                a.0 as i32,
                b.0 as i32,
                Vec::new(),
            ),
            Opcode::UMod { dst, a, b } => (
                hl_op_OUMod,
                dst.0 as i32,
                a.0 as i32,
                b.0 as i32,
                Vec::new(),
            ),
            Opcode::Shl { dst, a, b } => {
                (hl_op_OShl, dst.0 as i32, a.0 as i32, b.0 as i32, Vec::new())
            }
            Opcode::SShr { dst, a, b } => (
                hl_op_OSShr,
                dst.0 as i32,
                a.0 as i32,
                b.0 as i32,
                Vec::new(),
            ),
            Opcode::UShr { dst, a, b } => (
                hl_op_OUShr,
                dst.0 as i32,
                a.0 as i32,
                b.0 as i32,
                Vec::new(),
            ),
            Opcode::And { dst, a, b } => {
                (hl_op_OAnd, dst.0 as i32, a.0 as i32, b.0 as i32, Vec::new())
            }
            Opcode::Or { dst, a, b } => {
                (hl_op_OOr, dst.0 as i32, a.0 as i32, b.0 as i32, Vec::new())
            }
            Opcode::Xor { dst, a, b } => {
                (hl_op_OXor, dst.0 as i32, a.0 as i32, b.0 as i32, Vec::new())
            }
            Opcode::Neg { dst, src } => (hl_op_ONeg, dst.0 as i32, src.0 as i32, 0, Vec::new()),
            Opcode::Not { dst, src } => (hl_op_ONot, dst.0 as i32, src.0 as i32, 0, Vec::new()),
            Opcode::Incr { dst } => (hl_op_OIncr, dst.0 as i32, 0, 0, Vec::new()),
            Opcode::Decr { dst } => (hl_op_ODecr, dst.0 as i32, 0, 0, Vec::new()),
            Opcode::Call0 { dst, fun } => (hl_op_OCall0, dst.0 as i32, fun.0 as i32, 0, Vec::new()),
            Opcode::Call1 { dst, fun, arg0 } => (
                hl_op_OCall1,
                dst.0 as i32,
                fun.0 as i32,
                arg0.0 as i32,
                Vec::new(),
            ),
            Opcode::StaticClosure { dst, fun } => (
                hl_op_OStaticClosure,
                dst.0 as i32,
                fun.0 as i32,
                0,
                Vec::new(),
            ),
            Opcode::InstanceClosure { dst, fun, obj } => (
                hl_op_OInstanceClosure,
                dst.0 as i32,
                fun.0 as i32,
                obj.0 as i32,
                Vec::new(),
            ),
            Opcode::VirtualClosure { dst, obj, field } => (
                hl_op_OVirtualClosure,
                dst.0 as i32,
                obj.0 as i32,
                field.0 as i32,
                Vec::new(),
            ),
            Opcode::GetGlobal { dst, global } => (
                hl_op_OGetGlobal,
                dst.0 as i32,
                global.0 as i32,
                0,
                Vec::new(),
            ),
            Opcode::SetGlobal { global, src } => (
                hl_op_OSetGlobal,
                global.0 as i32,
                src.0 as i32,
                0,
                Vec::new(),
            ),
            Opcode::Field { dst, obj, field } => (
                hl_op_OField,
                dst.0 as i32,
                obj.0 as i32,
                field.0 as i32,
                Vec::new(),
            ),
            Opcode::SetField { obj, field, src } => (
                hl_op_OSetField,
                obj.0 as i32,
                field.0 as i32,
                src.0 as i32,
                Vec::new(),
            ),
            Opcode::GetThis { dst, field } => {
                (hl_op_OGetThis, dst.0 as i32, field.0 as i32, 0, Vec::new())
            }
            Opcode::SetThis { field, src } => {
                (hl_op_OSetThis, field.0 as i32, src.0 as i32, 0, Vec::new())
            }
            Opcode::DynGet { dst, obj, field } => (
                hl_op_ODynGet,
                dst.0 as i32,
                obj.0 as i32,
                field.0 as i32,
                Vec::new(),
            ),
            Opcode::DynSet { obj, field, src } => (
                hl_op_ODynSet,
                obj.0 as i32,
                field.0 as i32,
                src.0 as i32,
                Vec::new(),
            ),
            Opcode::JTrue { cond, offset } => {
                (hl_op_OJTrue, cond.0 as i32, *offset as i32, 0, Vec::new())
            }
            Opcode::JFalse { cond, offset } => {
                (hl_op_OJFalse, cond.0 as i32, *offset as i32, 0, Vec::new())
            }
            Opcode::JNull { reg, offset } => {
                (hl_op_OJNull, reg.0 as i32, *offset as i32, 0, Vec::new())
            }
            Opcode::JNotNull { reg, offset } => {
                (hl_op_OJNotNull, reg.0 as i32, *offset as i32, 0, Vec::new())
            }
            Opcode::JSLt { a, b, offset } => (
                hl_op_OJSLt,
                a.0 as i32,
                b.0 as i32,
                *offset as i32,
                Vec::new(),
            ),
            Opcode::JSGte { a, b, offset } => (
                hl_op_OJSGte,
                a.0 as i32,
                b.0 as i32,
                *offset as i32,
                Vec::new(),
            ),
            Opcode::JSGt { a, b, offset } => (
                hl_op_OJSGt,
                a.0 as i32,
                b.0 as i32,
                *offset as i32,
                Vec::new(),
            ),
            Opcode::JSLte { a, b, offset } => (
                hl_op_OJSLte,
                a.0 as i32,
                b.0 as i32,
                *offset as i32,
                Vec::new(),
            ),
            Opcode::JULt { a, b, offset } => (
                hl_op_OJULt,
                a.0 as i32,
                b.0 as i32,
                *offset as i32,
                Vec::new(),
            ),
            Opcode::JUGte { a, b, offset } => (
                hl_op_OJUGte,
                a.0 as i32,
                b.0 as i32,
                *offset as i32,
                Vec::new(),
            ),
            Opcode::JNotLt { a, b, offset } => (
                hl_op_OJNotLt,
                a.0 as i32,
                b.0 as i32,
                *offset as i32,
                Vec::new(),
            ),
            Opcode::JNotGte { a, b, offset } => (
                hl_op_OJNotGte,
                a.0 as i32,
                b.0 as i32,
                *offset as i32,
                Vec::new(),
            ),
            Opcode::JEq { a, b, offset } => (
                hl_op_OJEq,
                a.0 as i32,
                b.0 as i32,
                *offset as i32,
                Vec::new(),
            ),
            Opcode::JNotEq { a, b, offset } => (
                hl_op_OJNotEq,
                a.0 as i32,
                b.0 as i32,
                *offset as i32,
                Vec::new(),
            ),
            Opcode::JAlways { offset } => (hl_op_OJAlways, *offset as i32, 0, 0, Vec::new()),
            Opcode::ToDyn { dst, src } => (hl_op_OToDyn, dst.0 as i32, src.0 as i32, 0, Vec::new()),
            Opcode::ToSFloat { dst, src } => {
                (hl_op_OToSFloat, dst.0 as i32, src.0 as i32, 0, Vec::new())
            }
            Opcode::ToUFloat { dst, src } => {
                (hl_op_OToUFloat, dst.0 as i32, src.0 as i32, 0, Vec::new())
            }
            Opcode::ToInt { dst, src } => (hl_op_OToInt, dst.0 as i32, src.0 as i32, 0, Vec::new()),
            Opcode::SafeCast { dst, src } => {
                (hl_op_OSafeCast, dst.0 as i32, src.0 as i32, 0, Vec::new())
            }
            Opcode::UnsafeCast { dst, src } => {
                (hl_op_OUnsafeCast, dst.0 as i32, src.0 as i32, 0, Vec::new())
            }
            Opcode::ToVirtual { dst, src } => {
                (hl_op_OToVirtual, dst.0 as i32, src.0 as i32, 0, Vec::new())
            }
            Opcode::Label => (hl_op_OLabel, 0, 0, 0, Vec::new()),
            Opcode::Ret { ret } => (hl_op_ORet, ret.0 as i32, 0, 0, Vec::new()),
            Opcode::Throw { exc } => (hl_op_OThrow, exc.0 as i32, 0, 0, Vec::new()),
            Opcode::Rethrow { exc } => (hl_op_ORethrow, exc.0 as i32, 0, 0, Vec::new()),
            Opcode::NullCheck { reg } => (hl_op_ONullCheck, reg.0 as i32, 0, 0, Vec::new()),
            Opcode::Trap { exc, offset } => {
                (hl_op_OTrap, exc.0 as i32, *offset as i32, 0, Vec::new())
            }
            Opcode::EndTrap { exc } => (hl_op_OEndTrap, exc.0 as i32, 0, 0, Vec::new()),
            Opcode::GetI8 { dst, bytes, index } => (
                hl_op_OGetI8,
                dst.0 as i32,
                bytes.0 as i32,
                index.0 as i32,
                Vec::new(),
            ),
            Opcode::GetI16 { dst, bytes, index } => (
                hl_op_OGetI16,
                dst.0 as i32,
                bytes.0 as i32,
                index.0 as i32,
                Vec::new(),
            ),
            Opcode::GetMem { dst, bytes, index } => (
                hl_op_OGetMem,
                dst.0 as i32,
                bytes.0 as i32,
                index.0 as i32,
                Vec::new(),
            ),
            Opcode::GetArray { dst, array, index } => (
                hl_op_OGetArray,
                dst.0 as i32,
                array.0 as i32,
                index.0 as i32,
                Vec::new(),
            ),
            Opcode::SetI8 { bytes, index, src } => (
                hl_op_OSetI8,
                bytes.0 as i32,
                index.0 as i32,
                src.0 as i32,
                Vec::new(),
            ),
            Opcode::SetI16 { bytes, index, src } => (
                hl_op_OSetI16,
                bytes.0 as i32,
                index.0 as i32,
                src.0 as i32,
                Vec::new(),
            ),
            Opcode::SetMem { bytes, index, src } => (
                hl_op_OSetMem,
                bytes.0 as i32,
                index.0 as i32,
                src.0 as i32,
                Vec::new(),
            ),
            Opcode::SetArray { array, index, src } => (
                hl_op_OSetArray,
                array.0 as i32,
                index.0 as i32,
                src.0 as i32,
                Vec::new(),
            ),
            Opcode::New { dst } => (hl_op_ONew, dst.0 as i32, 0, 0, Vec::new()),
            Opcode::ArraySize { dst, array } => (
                hl_op_OArraySize,
                dst.0 as i32,
                array.0 as i32,
                0,
                Vec::new(),
            ),
            Opcode::Type { dst, ty } => (hl_op_OType, dst.0 as i32, ty.0 as i32, 0, Vec::new()),
            Opcode::GetType { dst, src } => {
                (hl_op_OGetType, dst.0 as i32, src.0 as i32, 0, Vec::new())
            }
            Opcode::GetTID { dst, src } => {
                (hl_op_OGetTID, dst.0 as i32, src.0 as i32, 0, Vec::new())
            }
            Opcode::Ref { dst, src } => (hl_op_ORef, dst.0 as i32, src.0 as i32, 0, Vec::new()),
            Opcode::Unref { dst, src } => (hl_op_OUnref, dst.0 as i32, src.0 as i32, 0, Vec::new()),
            Opcode::Setref { dst, value } => {
                (hl_op_OSetref, dst.0 as i32, value.0 as i32, 0, Vec::new())
            }
            Opcode::EnumAlloc { dst, construct } => (
                hl_op_OEnumAlloc,
                dst.0 as i32,
                construct.0 as i32,
                0,
                Vec::new(),
            ),
            Opcode::EnumIndex { dst, value } => (
                hl_op_OEnumIndex,
                dst.0 as i32,
                value.0 as i32,
                0,
                Vec::new(),
            ),
            Opcode::SetEnumField { value, field, src } => (
                hl_op_OSetEnumField,
                value.0 as i32,
                field.0 as i32,
                src.0 as i32,
                Vec::new(),
            ),
            Opcode::Assert => (hl_op_OAssert, 0, 0, 0, Vec::new()),
            Opcode::RefData { dst, src } => {
                (hl_op_ORefData, dst.0 as i32, src.0 as i32, 0, Vec::new())
            }
            Opcode::RefOffset { dst, reg, offset } => (
                hl_op_ORefOffset,
                dst.0 as i32,
                reg.0 as i32,
                offset.0 as i32,
                Vec::new(),
            ),
            Opcode::Nop => (hl_op_ONop, 0, 0, 0, Vec::new()),
            Opcode::Prefetch { value, field, mode } => (
                hl_op_OPrefetch,
                value.0 as i32,
                field.0 as i32,
                *mode,
                Vec::new(),
            ),

            // Variable-argument shapes. `read_opcode` reads these under
            // `nargs == -1`, with the count in `p3` (or `p2` for `Switch`)
            // followed by that many entries, so the inverse writes the count
            // and then the entries.
            Opcode::Call2 {
                dst,
                fun,
                arg0,
                arg1,
            } => (
                hl_op_OCall2,
                dst.0 as i32,
                fun.0 as i32,
                arg0.0 as i32,
                vec![arg1.0 as i32],
            ),
            Opcode::Call3 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
            } => (
                hl_op_OCall3,
                dst.0 as i32,
                fun.0 as i32,
                arg0.0 as i32,
                vec![arg1.0 as i32, arg2.0 as i32],
            ),
            Opcode::Call4 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
                arg3,
            } => (
                hl_op_OCall4,
                dst.0 as i32,
                fun.0 as i32,
                arg0.0 as i32,
                vec![arg1.0 as i32, arg2.0 as i32, arg3.0 as i32],
            ),
            Opcode::CallN { dst, fun, args } => (
                hl_op_OCallN,
                dst.0 as i32,
                fun.0 as i32,
                args.len() as i32,
                args.iter().map(|a| a.0 as i32).collect(),
            ),
            Opcode::CallMethod { dst, field, args } => (
                hl_op_OCallMethod,
                dst.0 as i32,
                field.0 as i32,
                args.len() as i32,
                args.iter().map(|a| a.0 as i32).collect(),
            ),
            Opcode::CallThis { dst, field, args } => (
                hl_op_OCallThis,
                dst.0 as i32,
                field.0 as i32,
                args.len() as i32,
                args.iter().map(|a| a.0 as i32).collect(),
            ),
            Opcode::CallClosure { dst, fun, args } => (
                hl_op_OCallClosure,
                dst.0 as i32,
                fun.0 as i32,
                args.len() as i32,
                args.iter().map(|a| a.0 as i32).collect(),
            ),
            Opcode::MakeEnum {
                dst,
                construct,
                args,
            } => (
                hl_op_OMakeEnum,
                dst.0 as i32,
                construct.0 as i32,
                args.len() as i32,
                args.iter().map(|a| a.0 as i32).collect(),
            ),
            Opcode::Switch { reg, offsets, end } => (
                hl_op_OSwitch,
                reg.0 as i32,
                offsets.len() as i32,
                *end,
                offsets.iter().map(|o| *o as i32).collect(),
            ),
            Opcode::EnumField {
                dst,
                value,
                construct,
                field,
            } => (
                hl_op_OEnumField,
                dst.0 as i32,
                value.0 as i32,
                construct.0 as i32,
                vec![field.0 as i32],
            ),
            Opcode::Asm { mode, value, reg } => {
                (hl_op_OAsm, *mode, *value, reg.0 as i32, Vec::new())
            }
            // Ash-internal: emitted only under hot reload, where a direct call
            // would bake in an address the next version will not live at. It
            // has no HashLink encoding, so a module carrying one cannot be
            // written out -- and one never should be, since `--hot-reload` and
            // emitting a file are different jobs.
            Opcode::IndirectCall { .. } => {
                return Err(bad("IndirectCall has no HashLink encoding"))
            }
        })
    }
}

fn bad(msg: &str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, msg.to_string())
}
