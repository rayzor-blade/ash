use crate::hl::{self, *};
use crate::opcodes::{JumpOffset, Opcode, RefBytes, RefEnumConstruct, RefField, RefFloat, RefFun, RefGlobal, RefInt, RefString, RefType, Reg};
use crate::types::{
    HLConstant, HLEnumConstruct, HLFunction, HLNative, HLObjField, HLObjProto, HLType, HLTypeEnum,
    HLTypeFun, HLTypeObj, HLTypeVirtual, TypeRef, ValueTypeKind, OP_NARGS,
};
use ash_macro::load_symbol;
use byteorder::{LittleEndian, ReadBytesExt};
use num_enum::TryFromPrimitive;
use std::borrow::BorrowMut;
use std::ffi::c_void;
use std::sync::atomic::{AtomicPtr, Ordering};

use crate::native_lib::STD_LIBRARY;
use std::io::{self, BufRead, BufReader, Cursor, Read, Seek};
use std::mem;
use std::os::macos::raw;
use std::path::Path;
use std::rc::Rc;

#[load_symbol]
extern "C" {
    fn hlp_hash_gen(name: *const uchar, cache_name: bool) -> i32;
}

#[derive(Default)]
pub struct BytecodeDecoder {
    // cursor: Cursor<Vec<u8>>,
    version: usize,
    flags: u32,
    nints: u32,
    nfloats: u32,
    nstrings: u32,
    nbytes: u32,
    ntypes: u32,
    nglobals: u32,
    nnatives: u32,
    nfunctions: u32,
    nconstants: u32,
    entrypoint: u32,
    has_debug: bool,
    ndebug_files: usize,
    decoded: DecodedBytecode,
}

impl BytecodeDecoder {
    pub fn decode(path: &Path) -> Result<DecodedBytecode, io::Error> {
        let mut decoder = BytecodeDecoder::default();
        let file = std::fs::File::open(path)?;
        let mut reader = io::BufReader::with_capacity(512 * 1024, file);

        // Search for the magic header
        let finder = memchr::memmem::Finder::new("HLB");
        let mut header_found = false;

        loop {
            let buffer = match reader.fill_buf() {
                Ok(buf) => buf,
                Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
                Err(e) => return Err(e),
            };

            if buffer.is_empty() {
                if header_found {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::UnexpectedEof,
                        "Unexpected end of file after finding header",
                    ));
                } else {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        "Could not find HashLink bytecode magic header",
                    ));
                }
            }

            if let Some(index) = finder.find(buffer) {
                reader.consume(index);
                header_found = true;
                break;
            }

            let len = buffer.len();
            reader.consume(len.saturating_sub(2));
        }

        // Read the header
        match decoder.read_header(&mut reader) {
            Ok(_) => (),
            Err(e) => {
                return Err(io::Error::new(
                    e.kind(),
                    format!("Error reading header: {}", e),
                ))
            }
        }

        // Decode the rest of the file
        match decoder._decode(&mut reader) {
            Ok(decoded) => Ok(decoded),
            Err(e) => Err(std::io::Error::new(
                e.kind(),
                format!("Error decoding bytecode: {}", e),
            )),
        }
    }

    fn read_header(&mut self, r: &mut impl BufRead) -> Result<(), std::io::Error> {
        let mut magic = [0u8; 3];
        r.read_exact(&mut magic)?;
        if &magic != b"HLB" {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "Invalid magic header",
            ));
        }

        self.version = r.read_u8()? as usize;
        if self.version <= 1 || self.version > 5 {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "Unsupported version",
            ));
        }

        self.flags = self.read_var_u(r)?;
        self.nints = self.read_var_u(r)?;
        self.nfloats = self.read_var_u(r)?;
        self.nstrings = self.read_var_u(r)?;

        if self.version >= 5 {
            self.nbytes = self.read_var_u(r)?;
        }

        self.ntypes = self.read_var_u(r)?;
        self.nglobals = self.read_var_u(r)?;
        self.nnatives = self.read_var_u(r)?;
        self.nfunctions = self.read_var_u(r)?;

        if self.version >= 4 {
            self.nconstants = self.read_var_u(r)?;
        }

        self.entrypoint = self.read_var_u(r)?;
        self.has_debug = self.flags & 1 == 1;
        self.ndebug_files = 0;

        Ok(())
    }

    fn read_var_int(&mut self, reader: &mut impl BufRead) -> Result<i32, std::io::Error> {
        let mut read_byte = || -> Result<u8, std::io::Error> {
            let mut buffer = [0; 1];
            reader.read_exact(&mut buffer)?;
            Ok(buffer[0])
        };

        let b = read_byte()?;

        if (b & 0x80) == 0 {
            Ok((b & 0x7F) as i32)
        } else if (b & 0x40) == 0 {
            let v = (read_byte()? as u32) | (((b & 31) as u32) << 8);
            let v = v as i32;
            Ok(if (b & 0x20) == 0 { v } else { -v })
        } else {
            let c = read_byte()? as u32;
            let d = read_byte()? as u32;
            let e = read_byte()? as u32;
            let v = ((b & 31) as u32) << 24 | c << 16 | d << 8 | e;
            let v = v as i32;
            Ok(if (b & 0x20) == 0 { v } else { -v })
        }
    }
    fn read_var_u(&mut self, r: &mut impl BufRead) -> Result<u32, std::io::Error> {
        let i = self.read_var_int(r)?;
        if i < 0 {
            Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Got negative index '{i}' (expected >= 0)"),
            ))
        } else {
            Ok(i as u32)
        }
    }

    pub fn _decode(&mut self, r: &mut impl BufRead) -> Result<DecodedBytecode, std::io::Error> {
        self.decoded.ints = self.read_ints(r)?;
        // println!("INTS {:?}", self.decoded.ints.len());
        self.decoded.floats = self.read_floats(r)?;
        // println!("Floats {:?}", self.decoded.floats.len());
        self.decoded.strings = self.read_strings(r)?;
        // println!("Strings {:?}", self.decoded.strings.len());
        self.decoded.bytes_pos = self.read_bytes(r)?;
        // println!("Bytes {:?}", self.decoded.bytes_pos.len());
        // println!("Has debug {}", self.has_debug);
        if self.has_debug {
            self.decoded.debug_files = self.read_debug_files(r)?;
            // println!("Debug files {:?}", self.decoded.debug_files.len());
        }

        self.decoded.types = self.read_types(r)?;
        // println!("Types {:?}", self.decoded.types.len());
        self.decoded.globals = self.read_globals(r)?;
        // println!("Globals {:?}", self.decoded.globals.len());
        self.decoded.natives = self.read_natives(r)?;
        // println!("Natives {:?}", self.decoded.natives.len());
        self.decoded.functions = self.read_functions(r)?;
        // println!("Functions {:?}", self.decoded.functions.len());
        self.decoded.constants = self.read_constants(r)?;
        // println!("Constants {:?}", self.decoded.constants.len());

        self.decoded.entrypoint = self.entrypoint;
        self.decoded.has_debug = self.has_debug;

        Ok(self.decoded.clone())
    }

    fn read_ints(&mut self, r: &mut impl BufRead) -> Result<Vec<i32>, std::io::Error> {
        (0..self.nints)
            .map(|_| r.read_i32::<LittleEndian>())
            .collect()
    }

    fn read_floats(&mut self, r: &mut impl BufRead) -> Result<Vec<f64>, std::io::Error> {
        (0..self.nfloats)
            .map(|_| r.read_f64::<LittleEndian>())
            .collect()
    }

    fn read_strings(&mut self, r: &mut impl BufRead) -> Result<Vec<String>, std::io::Error> {
        self._read_strings(r, self.nstrings as usize)
    }

    fn _read_strings(
        &mut self,
        r: &mut impl BufRead,
        len: usize,
    ) -> Result<Vec<String>, std::io::Error> {
        let size = r.read_i32::<LittleEndian>()? as usize;
        let mut data = vec![0u8; size];
        r.read_exact(&mut data)?;

        let mut strings = Vec::with_capacity(len);
        let mut start = 0;
        for _ in 0..len {
            let sz = self.read_var_u(r)? as usize + 1;
            if let Ok(s) = String::from_utf8(data[start..(start + sz - 1)].to_vec()) {
                strings.push(s);
            } else {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "Invalid string",
                ));
            }
            start += sz;
        }
        Ok(strings)
    }

    fn read_string(&mut self, r: &mut impl BufRead) -> Result<Vec<u16>, std::io::Error> {
        let index = self.read_var_int(r)? as usize;
        if index < 0 || index >= self.nstrings.clone() as usize {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "Invalid string index",
            ));
        }

        let string = &self.decoded.strings[index as usize];
        Ok(str_to_uchar_ptr(string))
    }

    fn read_bytes(&mut self, r: &mut impl BufRead) -> Result<Vec<usize>, std::io::Error> {
        if self.version < 5 {
            return Ok(Vec::new());
        }

        let size = r.read_i32::<LittleEndian>()? as usize;

        let mut data = vec![0u8; size];
        r.read_exact(&mut data)?;

        let mut bytes_pos = Vec::new();
        for _ in 0..self.nbytes {
            let pos = self.read_var_u(r)? as usize;
            bytes_pos.push(pos);
        }
        Ok(bytes_pos)
    }

    fn read_types(&mut self, r: &mut impl BufRead) -> Result<Vec<HLType>, std::io::Error> {
        // println!("Reading types..");
        (0..self.ntypes).map(|_| self.read_type(r)).collect()
    }

    fn get_type(&mut self, r: &mut impl BufRead) -> Result<TypeRef, std::io::Error> {
        let pos = self.read_var_int(r)? as usize;
        if pos < 0 || pos >= self.ntypes as usize {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "Invalid type index",
            ));
        }

        Ok(TypeRef(pos))
    }

    fn read_type(&mut self, r: &mut impl BufRead) -> Result<HLType, std::io::Error> {
        let kind = r.read_u8()? as u32;

        let mut _type = HLType::default();

        _type.kind = kind;

        // Read type-specific data based on kind
        let type_data = match kind {
            hl::hl_type_kind_HFUN | hl::hl_type_kind_HMETHOD => {
                let fun = self.read_type_fun(r)?;
                _type.fun = Some(fun);
            }
            hl::hl_type_kind_HOBJ | hl::hl_type_kind_HSTRUCT => {
                let obj = self.read_type_obj(r)?;
                _type.obj = Some(obj);
            }
            hl::hl_type_kind_HREF | hl::hl_type_kind_HNULL | hl::hl_type_kind_HPACKED => {
                _type.tparam = Some(self.get_type(r)?);
            }
            hl::hl_type_kind_HVIRTUAL => {
                let nfields = self.read_var_u(r)?;
                let mut virt = HLTypeVirtual::default();

                unsafe {
                    let mut fields = vec![HLObjField::default(); nfields as usize];

                    for i in 0..nfields {
                        let name = self.read_string(r)?.into_boxed_slice();

                        let hashed_name = __hlp_hash_gen(name.as_ptr(), true);
                        let mut field = HLObjField {
                            name: String::from_utf16_lossy(name.as_ref()),
                            type_: self.get_type(r)?,
                            hashed_name,
                        };
                        fields[i as usize] = field;
                    }
                    virt.fields = fields;
                }

                _type.virt = Some(virt);
            }
            hl::hl_type_kind_HENUM => {
                let name = self.read_string(r)?;
                let global_value_index = self.read_var_int(r)?; //as *mut *mut c_void;
                let nconstructs = self.read_var_u(r)? as i32;
                let mut _constructs = vec![HLEnumConstruct::default(); nconstructs as usize];
                for i in 0..nconstructs as usize {
                    let construct = &mut _constructs[i];
                    construct.name =
                        String::from_utf16_lossy(self.read_string(r)?.into_boxed_slice().as_ref());
                    let nparams = self.read_var_u(r)? as i32;
                    let mut _params = Vec::with_capacity(nparams as usize);
                    let mut _offsets = vec![0; nparams as usize];
                    for j in 0..nparams as usize {
                        let param = self.get_type(r)?;
                        _params.insert(j, param);
                    }

                    construct.params = _params;
                    construct.offsets = _offsets;
                }
                let mut tenum = HLTypeEnum {
                    name: String::from_utf16_lossy(name.as_ref()),
                    constructs: _constructs,
                    global_value: global_value_index as u32,
                };

                _type.tenum = Some(tenum);
            }
            hl::hl_type_kind_HABSTRACT => {
                let abs_name = self.read_string(r)?;
                _type.abs_name = Some(String::from_utf16_lossy(&abs_name));
            }
            _ => {
                if kind >= hl::hl_type_kind_HLAST {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        "Invalid type",
                    ));
                }
            }
        };

        Ok(_type)
    }

    fn read_type_fun(&mut self, r: &mut impl BufRead) -> Result<HLTypeFun, std::io::Error> {
        let nargs = r.read_u8()? as i32;
        let mut args = Vec::with_capacity(nargs as usize);
        for _ in 0..nargs {
            args.push(self.get_type(r)?);
        }
        let ret = self.get_type(r)?;

        Ok(HLTypeFun {
            args,
            ret,
            parent: None,
            closure_type: None,
            closure: None,
        })
    }

    fn read_type_obj(&mut self, r: &mut impl BufRead) -> Result<HLTypeObj, std::io::Error> {
        let name = self.read_string(r)?.into_boxed_slice();
        let super_type_index = self.read_var_int(r)?;
        let super_ = if super_type_index < 0 {
            None
        } else {
            Some(TypeRef(super_type_index as usize))
        };
        let global_index = self.read_var_u(r)?;
        let nfields = self.read_var_u(r)? as i32;
        let nproto = self.read_var_u(r)? as i32;
        let nbindings = self.read_var_u(r)? as i32;

        let mut fields = Vec::with_capacity(nfields as usize);
        let mut proto = Vec::with_capacity(nproto as usize);
        let mut bindings = vec![0; (nbindings * 2) as usize];

        for _ in 0..nfields as usize {
            let mut fname = self.read_string(r)?;
            fname.push(0); // null terminator for hash computation
            let hash = unsafe { __hlp_hash_gen(fname.as_ptr(), true) };
            let name = String::from_utf16_lossy(&fname[..fname.len() - 1]);
            let t = self.get_type(r)?;
            fields.push(HLObjField {
                name,
                type_: t,
                hashed_name: hash,
            });
        }

        for _ in 0..nproto as usize {
            let mut pname = self.read_string(r)?;
            pname.push(0); // null terminator for hash computation
            let hash = unsafe { __hlp_hash_gen(pname.as_ptr(), true) };
            let name = String::from_utf16_lossy(&pname[..pname.len() - 1]);
            let findex = self.read_var_u(r)? as i32;
            let pindex = self.read_var_int(r)? as i32;
            proto.push(HLObjProto {
                name,
                hashed_name: hash,
                findex,
                pindex,
            });
        }

        for j in 0..nbindings as usize {
            bindings[(j << 1) as usize] = self.read_var_u(r)? as i32;
            bindings[((j << 1) | 1) as usize] = self.read_var_u(r)? as i32;
        }

        Ok(HLTypeObj {
            name: String::from_utf16_lossy(name.as_ref()),
            super_,
            fields,
            proto,
            bindings,
            global_value: global_index,
        })
    }

    fn read_globals(&mut self, r: &mut impl BufRead) -> Result<Vec<TypeRef>, std::io::Error> {
        (0..self.nglobals).map(|_| self.get_type(r)).collect()
    }

    fn read_natives(&mut self, r: &mut impl BufRead) -> Result<Vec<HLNative>, std::io::Error> {
        (0..self.nnatives)
            .map(|i| {
                let mut native = HLNative::default();
                native.lib =
                    String::from_utf16_lossy(self.read_string(r)?.into_boxed_slice().as_ref());
                native.name =
                    String::from_utf16_lossy(self.read_string(r)?.into_boxed_slice().as_ref());
                native.type_ = self.get_type(r)?;
                native.findex = self.read_var_u(r)? as i32;
                Ok(native)
            })
            .collect()
    }

    fn read_functions(&mut self, r: &mut impl BufRead) -> Result<Vec<HLFunction>, std::io::Error> {
        let mut funcs = vec![HLFunction::default(); self.nfunctions as usize];
        for i in 0..self.nfunctions {
            let mut fun = HLFunction::default();
            fun.type_ = self.get_type(r)?;
            fun.findex = self.read_var_u(r)? as i32;
            let nregs = self.read_var_u(r)? as i32;
            let nops = self.read_var_u(r)? as i32;
            let mut regs: Vec<TypeRef> = Vec::with_capacity(nregs as usize);
            let mut ops = Vec::with_capacity(nops as usize);
            for i in 0..nregs as usize {
                let reg_type = self.get_type(r)?;
                regs.insert(i, reg_type);
            }
            fun.regs = regs;

            for i in 0..nops as usize {
                let opcode = self.read_opcode(r)?;
                ops.insert(i, opcode);
            }
            fun.ops = ops;

            if (self.has_debug) {
                fun.debug = self.read_debug_infos(r, nops as usize)?;

                if self.version >= 3 {
                    // skip assigns (no need here)
                    let nassigns = self.read_var_u(r)? as usize;
                    for i in 0..nassigns {
                        self.read_var_u(r).expect("expected to read uint");
                        self.read_var_int(r).expect("expected to read int");
                    }
                }
            }
            funcs[i as usize] = fun;
        }

        Ok(funcs)
    }

    #[inline]
    fn read_opcode(&mut self, r: &mut impl BufRead) -> Result<Opcode, std::io::Error> {
        let op: hl_op = r.read_u8()?.into();
        let mut extras = Vec::new();
        let mut p1 = 0;
        let mut p2 = 0;
        let mut p3 = 0;

        if op >= hl_op_OLast {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "Invalid opcode",
            ));
        }

        let nargs = OP_NARGS[op as usize];

        match nargs {
            0 => {}
            1 => {
                p1 = self.read_var_int(r)? as i32;
            }
            2 => {
                p1 = self.read_var_int(r)? as i32;
                p2 = self.read_var_int(r)? as i32;
            }
            3 => {
                p1 = self.read_var_int(r)? as i32;
                p2 = self.read_var_int(r)? as i32;
                p3 = self.read_var_int(r)? as i32;
            }
            4 => {
                p1 = self.read_var_int(r)? as i32;
                p2 = self.read_var_int(r)? as i32;
                p3 = self.read_var_int(r)? as i32;
                extras.push(self.read_var_int(r)? as i32);
            }
            -1 => match op {
                hl_op_OCallN | hl_op_OCallClosure | hl_op_OCallMethod | hl_op_OCallThis
                | hl_op_OMakeEnum => {
                    p1 = self.read_var_int(r)? as i32;
                    p2 = self.read_var_int(r)? as i32;
                    p3 = r.read_u8()? as i32;

                    for i in 0..p3 as usize {
                        extras.push(self.read_var_int(r)? as i32);
                    }
                }
                hl_op_OSwitch => {
                    p1 = self.read_var_u(r)? as i32;
                    p2 = self.read_var_u(r)? as i32;

                    for i in 0..p2 as usize {
                        extras.push(self.read_var_u(r)? as i32);
                    }
                    p3 = self.read_var_u(r)? as i32;
                }
                _ => {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        "Don't know how to process opcode",
                    ));
                }
            },
            _ => {
                let size = OP_NARGS[op as usize] - 3;

                p1 = self.read_var_int(r)?;
                p2 = self.read_var_int(r)?;
                p3 = self.read_var_int(r)?;
                for i in 0..size as usize {
                    let arg = self.read_var_int(r)?;
                    extras.push(arg);
                }
            }
        }

        Ok(match op {
            hl_op_OMov => Opcode::Mov {
                dst: Reg(p1 as u32),
                src: Reg(p2 as u32),
            },
            hl_op_OInt => Opcode::Int {
                dst: Reg(p1 as u32),
                ptr: RefInt(p2 as usize),
            },
            hl_op_OFloat => Opcode::Float {
                dst: Reg(p1 as u32),
                ptr: RefFloat(p2 as usize),
            },
            hl_op_OBool => Opcode::Bool {
                dst: Reg(p1 as u32),
                value: p2 != 0,
            },
            hl_op_OBytes => Opcode::Bytes {
                dst: Reg(p1 as u32),
                ptr: RefBytes(p2 as usize),
            },
            hl_op_OString => Opcode::String {
                dst: Reg(p1 as u32),
                ptr: RefString(p2 as usize),
            },
            hl_op_ONull => Opcode::Null {
                dst: Reg(p1 as u32),
            },
            hl_op_OAdd => Opcode::Add {
                dst: Reg(p1 as u32),
                a: Reg(p2 as u32),
                b: Reg(p3 as u32),
            },
            hl_op_OSub => Opcode::Sub {
                dst: Reg(p1 as u32),
                a: Reg(p2 as u32),
                b: Reg(p3 as u32),
            },
            hl_op_OMul => Opcode::Mul {
                dst: Reg(p1 as u32),
                a: Reg(p2 as u32),
                b: Reg(p3 as u32),
            },
            hl_op_OSDiv => Opcode::SDiv {
                dst: Reg(p1 as u32),
                a: Reg(p2 as u32),
                b: Reg(p3 as u32),
            },
            hl_op_OUDiv => Opcode::UDiv {
                dst: Reg(p1 as u32),
                a: Reg(p2 as u32),
                b: Reg(p3 as u32),
            },
            hl_op_OSMod => Opcode::SMod {
                dst: Reg(p1 as u32),
                a: Reg(p2 as u32),
                b: Reg(p3 as u32),
            },
            hl_op_OUMod => Opcode::UMod {
                dst: Reg(p1 as u32),
                a: Reg(p2 as u32),
                b: Reg(p3 as u32),
            },
            hl_op_OShl => Opcode::Shl {
                dst: Reg(p1 as u32),
                a: Reg(p2 as u32),
                b: Reg(p3 as u32),
            },
            hl_op_OSShr => Opcode::SShr {
                dst: Reg(p1 as u32),
                a: Reg(p2 as u32),
                b: Reg(p3 as u32),
            },
            hl_op_OUShr => Opcode::UShr {
                dst: Reg(p1 as u32),
                a: Reg(p2 as u32),
                b: Reg(p3 as u32),
            },
            hl_op_OAnd => Opcode::And {
                dst: Reg(p1 as u32),
                a: Reg(p2 as u32),
                b: Reg(p3 as u32),
            },
            hl_op_OOr => Opcode::Or {
                dst: Reg(p1 as u32),
                a: Reg(p2 as u32),
                b: Reg(p3 as u32),
            },
            hl_op_OXor => Opcode::Xor {
                dst: Reg(p1 as u32),
                a: Reg(p2 as u32),
                b: Reg(p3 as u32),
            },
            hl_op_ONeg => Opcode::Neg {
                dst: Reg(p1 as u32),
                src: Reg(p2 as u32),
            },
            hl_op_ONot => Opcode::Not {
                dst: Reg(p1 as u32),
                src: Reg(p2 as u32),
            },
            hl_op_OIncr => Opcode::Incr {
                dst: Reg(p1 as u32),
            },
            hl_op_ODecr => Opcode::Decr {
                dst: Reg(p1 as u32),
            },
            hl_op_OCall0 => Opcode::Call0 {
                dst: Reg(p1 as u32),
                fun: RefFun(p2 as usize),
            },
            hl_op_OCall1 => Opcode::Call1 {
                dst: Reg(p1 as u32),
                fun: RefFun(p2 as usize),
                arg0: Reg(p3 as u32),
            },
            hl_op_OCall2 => {
                Opcode::Call2 {
                    dst: Reg(p1 as u32),
                    fun: RefFun(p2 as usize),
                    arg0: Reg(p3 as u32),
                    arg1: Reg(extras[0] as u32),
                }
            }
            hl_op_OCall3 => {
               let code = Opcode::Call3 {
                    dst: Reg(p1 as u32),
                    fun: RefFun(p2 as usize),
                    arg0: Reg(p3 as u32),
                    arg1: Reg(extras[0] as u32),
                    arg2: Reg(extras[1] as u32),
                };

                unsafe {
                    std::mem::forget(extras);
                }

                code
            }
            hl_op_OCall4 => {
                Opcode::Call4 {
                    dst: Reg(p1 as u32),
                    fun: RefFun(p2 as usize),
                    arg0: Reg(p3 as u32),
                    arg1: Reg(extras[0] as u32),
                    arg2: Reg(extras[1] as u32),
                    arg3: Reg(extras[2] as u32),
                }
            }
            hl_op_OCallN => {
                let mut args = Vec::new();
                for i in 0..p3 as usize {
                    args.push(Reg(extras[i] as u32));
                }
                Opcode::CallN {
                    dst: Reg(p1 as u32),
                    fun: RefFun(p2 as usize),
                    args,
                }
            }
            hl_op_OCallMethod => {
                let mut args = Vec::new();
                for i in 0..p3 as usize {
                    args.push(Reg(extras[i] as u32));
                }
                Opcode::CallMethod {
                    dst: Reg(p1 as u32),
                    field: RefField(p2 as usize),
                    args,
                }
            }
            hl_op_OCallThis => {
                let mut args = Vec::new();
                for i in 0..p3 as usize {
                    args.push(Reg(extras[i] as u32));
                }
                Opcode::CallThis {
                    dst: Reg(p1 as u32),
                    field: RefField(p2 as usize),
                    args,
                }
            }
            hl_op_OCallClosure => {
                let mut args = Vec::new();
                for i in 0..p3 as usize {
                    args.push(Reg(extras[i] as u32));
                }
                Opcode::CallClosure {
                    dst: Reg(p1 as u32),
                    fun: Reg(p2 as u32),
                    args,
                }
            }
            hl_op_OStaticClosure => Opcode::StaticClosure {
                dst: Reg(p1 as u32),
                fun: RefFun(p2 as usize),
            },
            hl_op_OInstanceClosure => Opcode::InstanceClosure {
                dst: Reg(p1 as u32),
                fun: RefFun(p2 as usize),
                obj: Reg(p3 as u32),
            },
            hl_op_OVirtualClosure => Opcode::VirtualClosure {
                dst: Reg(p1 as u32),
                obj: Reg(p2 as u32),
                field: Reg(p3 as u32),
            },
            hl_op_OGetGlobal => Opcode::GetGlobal {
                dst: Reg(p1 as u32),
                global: RefGlobal(p2 as usize),
            },
            hl_op_OSetGlobal => Opcode::SetGlobal {
                global: RefGlobal(p1 as usize),
                src: Reg(p2 as u32),
            },
            hl_op_OField => Opcode::Field {
                dst: Reg(p1 as u32),
                obj: Reg(p2 as u32),
                field: RefField(p3 as usize),
            },
            hl_op_OSetField => Opcode::SetField {
                obj: Reg(p1 as u32),
                field: RefField(p2 as usize),
                src: Reg(p3 as u32),
            },
            hl_op_OGetThis => Opcode::GetThis {
                dst: Reg(p1 as u32),
                field: RefField(p2 as usize),
            },
            hl_op_OSetThis => Opcode::SetThis {
                field: RefField(p1 as usize),
                src: Reg(p2 as u32),
            },
            hl_op_ODynGet => Opcode::DynGet {
                dst: Reg(p1 as u32),
                obj: Reg(p2 as u32),
                field: RefString(p3 as usize),
            },
            hl_op_ODynSet => Opcode::DynSet {
                obj: Reg(p1 as u32),
                field: RefString(p2 as usize),
                src: Reg(p3 as u32),
            },
            hl_op_OJTrue => Opcode::JTrue {
                cond: Reg(p1 as u32),
                offset: p2 as JumpOffset,
            },
            hl_op_OJFalse => Opcode::JFalse {
                cond: Reg(p1 as u32),
                offset: p2 as JumpOffset,
            },
            hl_op_OJNull => Opcode::JNull {
                reg: Reg(p1 as u32),
                offset: p2 as JumpOffset,
            },
            hl_op_OJNotNull => Opcode::JNotNull {
                reg: Reg(p1 as u32),
                offset: p2 as JumpOffset,
            },
            hl_op_OJSLt => Opcode::JSLt {
                a: Reg(p1 as u32),
                b: Reg(p2 as u32),
                offset: p3 as JumpOffset,
            },
            hl_op_OJSGte => Opcode::JSGte {
                a: Reg(p1 as u32),
                b: Reg(p2 as u32),
                offset: p3 as JumpOffset,
            },
            hl_op_OJSGt => Opcode::JSGt {
                a: Reg(p1 as u32),
                b: Reg(p2 as u32),
                offset: p3 as JumpOffset,
            },
            hl_op_OJSLte => Opcode::JSLte {
                a: Reg(p1 as u32),
                b: Reg(p2 as u32),
                offset: p3 as JumpOffset,
            },
            hl_op_OJULt => Opcode::JULt {
                a: Reg(p1 as u32),
                b: Reg(p2 as u32),
                offset: p3 as JumpOffset,
            },
            hl_op_OJUGte => Opcode::JUGte {
                a: Reg(p1 as u32),
                b: Reg(p2 as u32),
                offset: p3 as JumpOffset,
            },
            hl_op_OJNotLt => Opcode::JNotLt {
                a: Reg(p1 as u32),
                b: Reg(p2 as u32),
                offset: p3 as JumpOffset,
            },
            hl_op_OJNotGte => Opcode::JNotGte {
                a: Reg(p1 as u32),
                b: Reg(p2 as u32),
                offset: p3 as JumpOffset,
            },
            hl_op_OJEq => Opcode::JEq {
                a: Reg(p1 as u32),
                b: Reg(p2 as u32),
                offset: p3 as JumpOffset,
            },
            hl_op_OJNotEq => Opcode::JNotEq {
                a: Reg(p1 as u32),
                b: Reg(p2 as u32),
                offset: p3 as JumpOffset,
            },
            hl_op_OJAlways => Opcode::JAlways {
                offset: p1 as JumpOffset,
            },
            hl_op_OToDyn => Opcode::ToDyn {
                dst: Reg(p1 as u32),
                src: Reg(p2 as u32),
            },
            hl_op_OToSFloat => Opcode::ToSFloat {
                dst: Reg(p1 as u32),
                src: Reg(p2 as u32),
            },
            hl_op_OToUFloat => Opcode::ToUFloat {
                dst: Reg(p1 as u32),
                src: Reg(p2 as u32),
            },
            hl_op_OToInt => Opcode::ToInt {
                dst: Reg(p1 as u32),
                src: Reg(p2 as u32),
            },
            hl_op_OSafeCast => Opcode::SafeCast {
                dst: Reg(p1 as u32),
                src: Reg(p2 as u32),
            },
            hl_op_OUnsafeCast => Opcode::UnsafeCast {
                dst: Reg(p1 as u32),
                src: Reg(p2 as u32),
            },
            hl_op_OToVirtual => Opcode::ToVirtual {
                dst: Reg(p1 as u32),
                src: Reg(p2 as u32),
            },
            hl_op_OLabel => Opcode::Label,
            hl_op_ORet => Opcode::Ret {
                ret: Reg(p1 as u32),
            },
            hl_op_OThrow => Opcode::Throw {
                exc: Reg(p1 as u32),
            },
            hl_op_ORethrow => Opcode::Rethrow {
                exc: Reg(p1 as u32),
            },
            hl_op_OSwitch => {
                let mut offsets = Vec::new();
                for i in 0..p2 as usize {
                    offsets.push(extras[i] as JumpOffset);
                }
                Opcode::Switch {
                    reg: Reg(p1 as u32),
                    offsets,
                    end: p3 as JumpOffset,
                }
            }
            hl_op_ONullCheck => Opcode::NullCheck {
                reg: Reg(p1 as u32),
            },
            hl_op_OTrap => Opcode::Trap {
                exc: Reg(p1 as u32),
                offset: p2 as JumpOffset,
            },
            hl_op_OEndTrap => Opcode::EndTrap {
                exc: Reg(p1 as u32),
            },
            hl_op_OGetI8 => Opcode::GetI8 {
                dst: Reg(p1 as u32),
                bytes: Reg(p2 as u32),
                index: Reg(p3 as u32),
            },
            hl_op_OGetI16 => Opcode::GetI16 {
                dst: Reg(p1 as u32),
                bytes: Reg(p2 as u32),
                index: Reg(p3 as u32),
            },
            hl_op_OGetMem => Opcode::GetMem {
                dst: Reg(p1 as u32),
                bytes: Reg(p2 as u32),
                index: Reg(p3 as u32),
            },
            hl_op_OGetArray => Opcode::GetArray {
                dst: Reg(p1 as u32),
                array: Reg(p2 as u32),
                index: Reg(p3 as u32),
            },
            hl_op_OSetI8 => Opcode::SetI8 {
                bytes: Reg(p1 as u32),
                index: Reg(p2 as u32),
                src: Reg(p3 as u32),
            },
            hl_op_OSetI16 => Opcode::SetI16 {
                bytes: Reg(p1 as u32),
                index: Reg(p2 as u32),
                src: Reg(p3 as u32),
            },
            hl_op_OSetMem => Opcode::SetMem {
                bytes: Reg(p1 as u32),
                index: Reg(p2 as u32),
                src: Reg(p3 as u32),
            },
            hl_op_OSetArray => Opcode::SetArray {
                array: Reg(p1 as u32),
                index: Reg(p2 as u32),
                src: Reg(p3 as u32),
            },
            hl_op_ONew => Opcode::New {
                dst: Reg(p1 as u32),
            },
            hl_op_OArraySize => Opcode::ArraySize {
                dst: Reg(p1 as u32),
                array: Reg(p2 as u32),
            },
            hl_op_OType => Opcode::Type {
                dst: Reg(p1 as u32),
                ty: RefType(p2 as usize),
            },
            hl_op_OGetType => Opcode::GetType {
                dst: Reg(p1 as u32),
                src: Reg(p2 as u32),
            },
            hl_op_OGetTID => Opcode::GetTID {
                dst: Reg(p1 as u32),
                src: Reg(p2 as u32),
            },
            hl_op_ORef => Opcode::Ref {
                dst: Reg(p1 as u32),
                src: Reg(p2 as u32),
            },
            hl_op_OUnref => Opcode::Unref {
                dst: Reg(p1 as u32),
                src: Reg(p2 as u32),
            },
            hl_op_OSetref => Opcode::Setref {
                dst: Reg(p1 as u32),
                value: Reg(p2 as u32),
            },
            hl_op_OMakeEnum => {
                let mut args = Vec::new();
                for i in 0..p3 as usize {
                    args.push(Reg(extras[i] as u32));
                }
                Opcode::MakeEnum {
                    dst: Reg(p1 as u32),
                    construct: RefEnumConstruct(p2 as usize),
                    args,
                }
            }
            hl_op_OEnumAlloc => Opcode::EnumAlloc {
                dst: Reg(p1 as u32),
                construct: RefEnumConstruct(p2 as usize),
            },
            hl_op_OEnumIndex => Opcode::EnumIndex {
                dst: Reg(p1 as u32),
                value: Reg(p2 as u32),
            },
            hl_op_OEnumField => {
                Opcode::EnumField {
                    dst: Reg(p1 as u32),
                    value: Reg(p2 as u32),
                    construct: RefEnumConstruct(p3 as usize),
                    field: RefField(extras[0] as usize),
                }
            }
            hl_op_OSetEnumField => Opcode::SetEnumField {
                value: Reg(p1 as u32),
                field: RefField(p2 as usize),
                src: Reg(p3 as u32),
            },
            hl_op_OAssert => Opcode::Assert,
            hl_op_ORefData => Opcode::RefData {
                dst: Reg(p1 as u32),
                src: Reg(p2 as u32),
            },
            hl_op_ORefOffset => Opcode::RefOffset {
                dst: Reg(p1 as u32),
                reg: Reg(p2 as u32),
                offset: Reg(p3 as u32),
            },
            hl_op_ONop => Opcode::Nop,
            hl_op_OPrefetch => Opcode::Prefetch {
                value: Reg(p1 as u32),
                field: RefField(p2 as usize),
                mode: p3,
            },
            hl_op_OAsm => Opcode::Asm {
                mode: p1,
                value: p2,
                reg: Reg(p3 as u32),
            },
            _ => panic!("Unknown opcode: {:?}", op),
        })
    }

    fn read_constants(&mut self, r: &mut impl BufRead) -> Result<Vec<HLConstant>, std::io::Error> {
        if self.version < 4 {
            return Ok(Vec::new());
        }

        (0..self.nconstants)
            .map(|_| {
                let mut constant = HLConstant::default();
                constant.global = self.read_var_u(r)?;
                let nfields = self.read_var_u(r)? as i32;
                let mut fields = Vec::with_capacity(nfields as usize);
                // Read fields
                for i in 0..nfields {
                    fields.push(self.read_var_u(r)? as i32);
                }
                constant.fields = fields;
                Ok(constant)
            })
            .collect()
    }

    fn read_debug_files(&mut self, r: &mut impl BufRead) -> Result<Vec<String>, std::io::Error> {
        self.ndebug_files = self.read_var_u(r)? as usize;
        self._read_strings(r, self.ndebug_files as usize)
    }

    fn read_debug_infos(&self, r: &mut impl BufRead, nops: usize) -> io::Result<Vec<i32>> {
        let mut curfile = -1;
        let mut curline = 0;
        let mut debug = Vec::with_capacity(nops * 2);
        let mut debug_infos = Vec::with_capacity(nops);
        let mut i = 0;

        while i < nops {
            let mut c = r.read_u8()? as i32;

            if c & 1 != 0 {
                c >>= 1;
                curfile = (c << 8) | (r.read_u8()? as i32);
                if curfile >= self.ndebug_files as i32 {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "Invalid debug file",
                    ));
                }
            } else if c & 2 != 0 {
                let delta = c >> 6;
                let mut count = (c >> 2) & 15;
                if i + (count as usize) > nops {
                    return Err(io::Error::new(io::ErrorKind::InvalidData, "Outside range"));
                }
                while count > 0 {
                    debug_infos.push((curfile, curline));
                    i += 1;
                    count -= 1;
                }
                curline += delta;
            } else if c & 4 != 0 {
                curline += c >> 3;
                debug_infos.push((curfile, curline));
                i += 1;
            } else {
                let b2 = r.read_u8()? as i32;
                let b3 = r.read_u8()? as i32;
                curline = (c >> 3) | (b2 << 5) | (b3 << 13);
                debug_infos.push((curfile, curline));
                i += 1;
            }
        }

        for (file, pos) in debug_infos {
            debug.push(file);
            debug.push(pos);
        }

        Ok(debug)
    }
}

#[derive(Default, Clone)]
pub struct DecodedBytecode {
    pub ints: Vec<i32>,
    pub floats: Vec<f64>,
    pub strings: Vec<String>,
    pub bytes_pos: Vec<usize>,
    pub types: Vec<HLType>,
    pub globals: Vec<TypeRef>,
    pub natives: Vec<HLNative>,
    pub functions: Vec<HLFunction>,
    pub constants: Vec<HLConstant>,
    pub debug_files: Vec<String>,
    pub has_debug: bool,
    pub entrypoint: u32,
}

pub fn str_to_uchar_ptr(s: &str) -> Vec<u16> {
    // Convert the &str to a UTF-16 vector
    s.encode_utf16().collect()
}

pub(crate) const CRC_TABLE: [u32; 256] = [
    0x00000000, 0x04c11db7, 0x09823b6e, 0x0d4326d9, 0x130476dc, 0x17c56b6b, 0x1a864db2, 0x1e475005,
    0x2608edb8, 0x22c9f00f, 0x2f8ad6d6, 0x2b4bcb61, 0x350c9b64, 0x31cd86d3, 0x3c8ea00a, 0x384fbdbd,
    0x4c11db70, 0x48d0c6c7, 0x4593e01e, 0x4152fda9, 0x5f15adac, 0x5bd4b01b, 0x569796c2, 0x52568b75,
    0x6a1936c8, 0x6ed82b7f, 0x639b0da6, 0x675a1011, 0x791d4014, 0x7ddc5da3, 0x709f7b7a, 0x745e66cd,
    0x9823b6e0, 0x9ce2ab57, 0x91a18d8e, 0x95609039, 0x8b27c03c, 0x8fe6dd8b, 0x82a5fb52, 0x8664e6e5,
    0xbe2b5b58, 0xbaea46ef, 0xb7a96036, 0xb3687d81, 0xad2f2d84, 0xa9ee3033, 0xa4ad16ea, 0xa06c0b5d,
    0xd4326d90, 0xd0f37027, 0xddb056fe, 0xd9714b49, 0xc7361b4c, 0xc3f706fb, 0xceb42022, 0xca753d95,
    0xf23a8028, 0xf6fb9d9f, 0xfbb8bb46, 0xff79a6f1, 0xe13ef6f4, 0xe5ffeb43, 0xe8bccd9a, 0xec7dd02d,
    0x34867077, 0x30476dc0, 0x3d044b19, 0x39c556ae, 0x278206ab, 0x23431b1c, 0x2e003dc5, 0x2ac12072,
    0x128e9dcf, 0x164f8078, 0x1b0ca6a1, 0x1fcdbb16, 0x018aeb13, 0x054bf6a4, 0x0808d07d, 0x0cc9cdca,
    0x7897ab07, 0x7c56b6b0, 0x71159069, 0x75d48dde, 0x6b93dddb, 0x6f52c06c, 0x6211e6b5, 0x66d0fb02,
    0x5e9f46bf, 0x5a5e5b08, 0x571d7dd1, 0x53dc6066, 0x4d9b3063, 0x495a2dd4, 0x44190b0d, 0x40d816ba,
    0xaca5c697, 0xa864db20, 0xa527fdf9, 0xa1e6e04e, 0xbfa1b04b, 0xbb60adfc, 0xb6238b25, 0xb2e29692,
    0x8aad2b2f, 0x8e6c3698, 0x832f1041, 0x87ee0df6, 0x99a95df3, 0x9d684044, 0x902b669d, 0x94ea7b2a,
    0xe0b41de7, 0xe4750050, 0xe9362689, 0xedf73b3e, 0xf3b06b3b, 0xf771768c, 0xfa325055, 0xfef34de2,
    0xc6bcf05f, 0xc27dede8, 0xcf3ecb31, 0xcbffd686, 0xd5b88683, 0xd1799b34, 0xdc3abded, 0xd8fba05a,
    0x690ce0ee, 0x6dcdfd59, 0x608edb80, 0x644fc637, 0x7a089632, 0x7ec98b85, 0x738aad5c, 0x774bb0eb,
    0x4f040d56, 0x4bc510e1, 0x46863638, 0x42472b8f, 0x5c007b8a, 0x58c1663d, 0x558240e4, 0x51435d53,
    0x251d3b9e, 0x21dc2629, 0x2c9f00f0, 0x285e1d47, 0x36194d42, 0x32d850f5, 0x3f9b762c, 0x3b5a6b9b,
    0x0315d626, 0x07d4cb91, 0x0a97ed48, 0x0e56f0ff, 0x1011a0fa, 0x14d0bd4d, 0x19939b94, 0x1d528623,
    0xf12f560e, 0xf5ee4bb9, 0xf8ad6d60, 0xfc6c70d7, 0xe22b20d2, 0xe6ea3d65, 0xeba91bbc, 0xef68060b,
    0xd727bbb6, 0xd3e6a601, 0xdea580d8, 0xda649d6f, 0xc423cd6a, 0xc0e2d0dd, 0xcda1f604, 0xc960ebb3,
    0xbd3e8d7e, 0xb9ff90c9, 0xb4bcb610, 0xb07daba7, 0xae3afba2, 0xaafbe615, 0xa7b8c0cc, 0xa379dd7b,
    0x9b3660c6, 0x9ff77d71, 0x92b45ba8, 0x9675461f, 0x8832161a, 0x8cf30bad, 0x81b02d74, 0x857130c3,
    0x5d8a9099, 0x594b8d2e, 0x5408abf7, 0x50c9b640, 0x4e8ee645, 0x4a4ffbf2, 0x470cdd2b, 0x43cdc09c,
    0x7b827d21, 0x7f436096, 0x7200464f, 0x76c15bf8, 0x68860bfd, 0x6c47164a, 0x61043093, 0x65c52d24,
    0x119b4be9, 0x155a565e, 0x18197087, 0x1cd86d30, 0x029f3d35, 0x065e2082, 0x0b1d065b, 0x0fdc1bec,
    0x3793a651, 0x3352bbe6, 0x3e119d3f, 0x3ad08088, 0x2497d08d, 0x2056cd3a, 0x2d15ebe3, 0x29d4f654,
    0xc5a92679, 0xc1683bce, 0xcc2b1d17, 0xc8ea00a0, 0xd6ad50a5, 0xd26c4d12, 0xdf2f6bcb, 0xdbee767c,
    0xe3a1cbc1, 0xe760d676, 0xea23f0af, 0xeee2ed18, 0xf0a5bd1d, 0xf464a0aa, 0xf9278673, 0xfde69bc4,
    0x89b8fd09, 0x8d79e0be, 0x803ac667, 0x84fbdbd0, 0x9abc8bd5, 0x9e7d9662, 0x933eb0bb, 0x97ffad0c,
    0xafb010b1, 0xab710d06, 0xa6322bdf, 0xa2f33668, 0xbcb4666d, 0xb8757bda, 0xb5365d03, 0xb1f740b4,
];

pub fn H(hash: u32, b: u8) -> u32 {
    (hash >> 8) ^ CRC_TABLE[((hash ^ u32::from(b)) & 0xFF) as usize]
}

pub fn H32(hash: u32, i: u32) -> u32 {
    let mut result = hash;
    result = H(result, (i & 0xFF) as u8);
    result = H(result, ((i >> 8) & 0xFF) as u8);
    result = H(result, ((i >> 16) & 0xFF) as u8);
    result = H(result, ((i >> 24) & 0xFF) as u8);
    result
}

#[cfg(test)]
mod test {
    use super::BytecodeDecoder;
    use crate::native_lib::init_std_library;
    use std::fs::File;
    use std::io::{BufRead, BufReader, Read};
    use std::path::*;
    use std::str::FromStr;

    #[test]
    fn decode_binary() -> Result<(), std::io::Error> {
        init_std_library();

        let mut cwd =
            PathBuf::from_str(env!("CARGO_MANIFEST_DIR")).expect("Expected to get manifest path");
        cwd.push("test/test.hl");

        println!("{:?}", cwd.try_exists().err());

        let mut bytecode = BytecodeDecoder::decode(&cwd)?;

        // println!("Functions \n{:?}", bytecode.functions.len());

        // let mut reader = &mut BufReader::with_capacity(512 * 1024, File::open(cwd.as_path())?);
        // let finder = memchr::memmem::Finder::new("HLB");
        // loop {
        //     let buffer = reader.fill_buf()?;
        //     if let Some(index) = finder.find(buffer) {
        //         reader.consume(index);

        //         break;
        //     }
        //     let len = buffer.len();
        //     // Edge case is when this buffer ends with 'HL', we must not consume
        //     // the last 2 bytes, so they can be used for the next search.
        //     reader.consume(len - 2);
        // }

        Ok(())
    }
}
