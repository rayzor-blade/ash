//! Reads the type table of a `.hl` file, the way `Genhl.write_code` writes it.
//!
//! It is the oracle for the layouts [`crate::build`] computes: the same program compiled by
//! the HashLink generator carries them, field for field. Only the types are read; the code is
//! skipped.

use crate::hl::{EnumType, HlType, ObjType, Proto, TypeTable};

struct Reader<'a> {
    bytes: &'a [u8],
    at: usize,
}

type Result<T> = std::result::Result<T, String>;

impl<'a> Reader<'a> {
    fn byte(&mut self) -> Result<u8> {
        let b = *self
            .bytes
            .get(self.at)
            .ok_or_else(|| format!("the file ends at {}", self.at))?;
        self.at += 1;
        Ok(b)
    }

    fn take(&mut self, n: usize) -> Result<&'a [u8]> {
        let end = self.at + n;
        let out = self
            .bytes
            .get(self.at..end)
            .ok_or_else(|| format!("the file ends at {}", self.at))?;
        self.at = end;
        Ok(out)
    }

    fn i32_le(&mut self) -> Result<i32> {
        Ok(i32::from_le_bytes(self.take(4)?.try_into().unwrap()))
    }

    /// `Genhl.write_index_gen`: one byte below 0x80, two below 0x2000, else four; a set bit
    /// 0x20 in the first byte of the longer forms makes the value negative.
    fn index(&mut self) -> Result<i32> {
        let b = i32::from(self.byte()?);
        if b & 0x80 == 0 {
            return Ok(b);
        }
        let negative = b & 0x20 != 0;
        let value = if b & 0x40 == 0 {
            ((b & 0x1F) << 8) | i32::from(self.byte()?)
        } else {
            let (b1, b2, b3) = (
                i32::from(self.byte()?),
                i32::from(self.byte()?),
                i32::from(self.byte()?),
            );
            ((b & 0x1F) << 24) | (b1 << 16) | (b2 << 8) | b3
        };
        Ok(if negative { -value } else { value })
    }

    fn uindex(&mut self) -> Result<usize> {
        let v = self.index()?;
        usize::try_from(v).map_err(|_| format!("a negative index {v} where a count belongs"))
    }

    /// A block of strings: the byte length, the bytes (each ends in a zero), then each length.
    fn strings(&mut self, count: usize) -> Result<Vec<String>> {
        let total = self.i32_le()? as usize;
        let data = self.take(total)?;
        let mut out = Vec::with_capacity(count);
        let mut offset = 0;
        for _ in 0..count {
            let len = self.uindex()?;
            let s = data
                .get(offset..offset + len)
                .ok_or("a string runs past its block")?;
            out.push(String::from_utf8_lossy(s).into_owned());
            offset += len + 1;
        }
        Ok(out)
    }
}

/// The type table of `bytes`.
pub fn read_types(bytes: &[u8]) -> Result<TypeTable> {
    let mut r = Reader { bytes, at: 0 };
    if r.take(3)? != b"HLB" {
        return Err("not a HashLink bytecode file".into());
    }
    let version = r.byte()?;
    let flags = r.byte()?;
    let nints = r.uindex()?;
    let nfloats = r.uindex()?;
    let nstrings = r.uindex()?;
    let nbytes = if version >= 5 { r.uindex()? } else { 0 };
    let ntypes = r.uindex()?;
    let _nglobals = r.uindex()?;
    let _nnatives = r.uindex()?;
    let _nfunctions = r.uindex()?;
    let _nconstants = r.uindex()?;
    let _entry = r.uindex()?;

    r.take(nints * 4)?;
    r.take(nfloats * 8)?;
    let strings = r.strings(nstrings)?;
    if version >= 5 {
        let total = r.i32_le()? as usize;
        r.take(total)?;
        for _ in 0..nbytes {
            r.index()?;
        }
    }
    if flags & 1 != 0 {
        let files = r.uindex()?;
        r.strings(files)?;
    }

    let string = |i: usize| -> Result<String> {
        strings
            .get(i)
            .cloned()
            .ok_or_else(|| format!("string {i} of {}", strings.len()))
    };
    let mut table = TypeTable::default();
    for _ in 0..ntypes {
        let tag = r.byte()?;
        let t = match tag {
            0 => HlType::Void,
            1 => HlType::UI8,
            2 => HlType::UI16,
            3 => HlType::I32,
            4 => HlType::I64,
            5 => HlType::F32,
            6 => HlType::F64,
            7 => HlType::Bool,
            8 => HlType::Bytes,
            9 => HlType::Dyn,
            10 | 20 => {
                let n = usize::from(r.byte()?);
                let mut args = Vec::with_capacity(n);
                for _ in 0..n {
                    args.push(r.uindex()?);
                }
                let ret = r.uindex()?;
                if tag == 10 {
                    HlType::Fun(args, ret)
                } else {
                    HlType::Method(args, ret)
                }
            }
            11 | 21 => {
                let name = string(r.uindex()?)?;
                let sup = r.index()?;
                let global = r.uindex()?;
                let (nfields, nproto, nbindings) = (r.uindex()?, r.uindex()?, r.uindex()?);
                let mut obj = ObjType {
                    name,
                    is_struct: tag == 21,
                    super_: usize::try_from(sup).ok(),
                    global: global.checked_sub(1).map(|g| g as u32),
                    ..ObjType::default()
                };
                for _ in 0..nfields {
                    let name = string(r.uindex()?)?;
                    obj.fields.push((name, r.uindex()?));
                }
                for _ in 0..nproto {
                    let name = string(r.uindex()?)?;
                    let findex = r.uindex()? as u32;
                    let vslot = r.index()?;
                    obj.proto.push(Proto {
                        name,
                        findex,
                        vslot: u32::try_from(vslot).ok(),
                    });
                }
                for _ in 0..nbindings {
                    let field = r.uindex()? as u32;
                    obj.bindings.push((field, r.uindex()? as u32));
                }
                table.objs.push(obj);
                let ix = table.objs.len() - 1;
                if tag == 21 {
                    HlType::Struct(ix)
                } else {
                    HlType::Obj(ix)
                }
            }
            12 => HlType::Array,
            13 => HlType::Type,
            14 => HlType::Ref(r.uindex()?),
            15 => {
                let n = r.uindex()?;
                let mut fields = Vec::with_capacity(n);
                for _ in 0..n {
                    let name = string(r.uindex()?)?;
                    fields.push((name, r.uindex()?));
                }
                table.virts.push(fields);
                HlType::Virtual(table.virts.len() - 1)
            }
            16 => HlType::DynObj,
            17 => HlType::Abstract(string(r.uindex()?)?),
            18 => {
                let name = string(r.uindex()?)?;
                let global = r.uindex()?;
                let n = r.uindex()?;
                let mut e = EnumType {
                    name,
                    global: global.checked_sub(1).map(|g| g as u32),
                    fields: Vec::with_capacity(n),
                };
                for _ in 0..n {
                    let cname = string(r.uindex()?)?;
                    let nargs = usize::from(r.byte()?);
                    let mut args = Vec::with_capacity(nargs);
                    for _ in 0..nargs {
                        args.push(r.uindex()?);
                    }
                    e.fields.push((cname, args));
                }
                table.enums.push(e);
                HlType::Enum(table.enums.len() - 1)
            }
            19 => HlType::Null(r.uindex()?),
            22 => HlType::Packed(r.uindex()?),
            other => return Err(format!("unknown type tag {other} at {}", r.at)),
        };
        table.push_raw(t);
    }
    Ok(table)
}
