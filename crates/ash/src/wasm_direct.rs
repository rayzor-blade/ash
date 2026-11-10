//! The bridge from a decoded program to `ash_wasm_emit`.
//!
//! The emitter knows AIR and a handful of questions about the program it
//! belongs to; this answers them from the bytecode. A function is handed to it
//! under the symbol the LLVM module gave that function, since callers on
//! either side of the link name it by that.
use crate::bytecode::DecodedBytecode;
use crate::hl_bindings as hl;
use ash_wasm_emit::{Alloc, Input, ObjectEmitted, Options, Program, Shape, Sig, Ty};
use std::collections::HashMap;

pub struct BytecodeProgram<'a> {
    pub bc: &'a DecodedBytecode,
    /// Each bytecode function's symbol, as the module that lowers the rest of
    /// the program names it.
    pub symbols: &'a HashMap<usize, String>,
    /// The data the module that lowers the rest of the program defines.
    pub data: &'a ProgramData,
}

/// Names of the data a program's other object defines, for an object that
/// takes their addresses.
#[derive(Default)]
pub struct ProgramData {
    pub globals: Option<String>,
    /// The table of function addresses, indexed by findex.
    pub functions: Option<String>,
    pub types: HashMap<usize, String>,
    /// Whether an ordinary class is allocated with its size known up front.
    pub sized_alloc: bool,
    /// Extra bytes a host module appends to objects of a type.
    pub object_tails: HashMap<usize, u64>,
    /// Types whose objects a host module must be told about when they die.
    pub object_drops: std::collections::HashSet<usize>,
    /// Whether reference stores mark cards for the generational collector.
    pub barriers: bool,
}

impl BytecodeProgram<'_> {
    fn ty_of(&self, index: usize) -> Ty {
        let k = self.bc.types[index].kind;
        if k == hl::hl_type_kind_HVOID {
            Ty::Void
        } else if k == hl::hl_type_kind_HBOOL {
            Ty::Bool
        } else if k == hl::hl_type_kind_HI32 {
            Ty::I32
        } else if k == hl::hl_type_kind_HI64 {
            Ty::I64
        } else if k == hl::hl_type_kind_HF32 {
            Ty::F32
        } else if k == hl::hl_type_kind_HF64 {
            Ty::F64
        } else if k == hl::hl_type_kind_HUI8 || k == hl::hl_type_kind_HUI16 {
            // Narrow unsigned integers wrap on every store; not handled yet.
            Ty::Other
        } else {
            Ty::Ptr
        }
    }

    /// A native the runtime itself exports as `hlp_<name>`, called with the
    /// signature the bytecode declares. A host's native, an HDLL primitive
    /// and a native that takes a context word are reached some other way.
    fn runtime_native(&self, findex: usize) -> Option<&crate::types::HLNative> {
        use crate::native_lib as lib;
        let n = self.bc.natives.iter().find(|n| n.findex as usize == findex)?;
        let plain = lib::is_runtime_lib(&n.lib)
            && lib::host_link(&n.lib, &n.name).is_none()
            && lib::host_native_context(&n.lib, &n.name) == 0
            && !lib::host_native_record(&n.lib, &n.name);
        plain.then_some(n)
    }

    fn sig_of(&self, fun: &crate::types::HLTypeFun) -> Sig {
        Sig {
            params: fun.args.iter().map(|a| self.ty_of(a.0)).collect(),
            ret: self.ty_of(fun.ret.0),
        }
    }
}

impl Program for BytecodeProgram<'_> {
    fn ty(&self, t: air::v2::ir::TypeRef) -> Ty {
        self.ty_of(t.0 as usize)
    }

    fn int(&self, idx: usize) -> Option<i32> {
        self.bc.ints.get(idx).copied()
    }

    fn float(&self, idx: usize) -> Option<f64> {
        self.bc.floats.get(idx).copied()
    }

    fn symbol(&self, findex: usize) -> String {
        if let Some(name) = self.symbols.get(&findex) {
            return name.clone();
        }
        match self.runtime_native(findex) {
            Some(n) => format!("hlp_{}", n.name),
            None => format!("Fun_{findex}"),
        }
    }

    fn array_elem_size(&self, t: air::v2::ir::TypeRef) -> Option<u32> {
        let kind = self.bc.types.get(t.0 as usize)?.kind;
        u32::try_from(crate::layout::array_elem_size_for(kind, 4)).ok()
    }

    fn shape(&self, t: air::v2::ir::TypeRef) -> Shape {
        let Some(info) = self.bc.types.get(t.0 as usize) else {
            return Shape::Other;
        };
        let k = info.kind;
        if k == hl::hl_type_kind_HDYN {
            Shape::Dyn
        } else if k == hl::hl_type_kind_HNULL {
            Shape::Null
        } else if k == hl::hl_type_kind_HVIRTUAL {
            Shape::Virtual
        } else if k == hl::hl_type_kind_HOBJ {
            Shape::Obj
        } else if k == hl::hl_type_kind_HSTRUCT {
            Shape::Struct
        } else if k == hl::hl_type_kind_HBYTES
            || k == hl::hl_type_kind_HTYPE
            || k == hl::hl_type_kind_HREF
            || k == hl::hl_type_kind_HABSTRACT
        {
            Shape::Boxed
        } else {
            Shape::Other
        }
    }

    fn null_param(&self, t: air::v2::ir::TypeRef) -> Option<air::v2::ir::TypeRef> {
        let p = self.bc.types.get(t.0 as usize)?.tparam.as_ref()?;
        Some(air::v2::ir::TypeRef(p.0 as u32))
    }

    fn functions_symbol(&self) -> Option<String> {
        self.data.functions.clone()
    }

    fn function_type(&self, findex: usize) -> Option<usize> {
        if let Some(f) = self.bc.functions.iter().find(|f| f.findex as usize == findex) {
            return Some(f.type_.0);
        }
        self.bc
            .natives
            .iter()
            .find(|n| n.findex as usize == findex)
            .map(|n| n.type_.0)
    }

    fn virtual_field(&self, obj_ty: air::v2::ir::TypeRef, field: usize) -> Option<i32> {
        let t = self.bc.types.get(obj_ty.0 as usize)?;
        if t.kind != hl::hl_type_kind_HVIRTUAL {
            return None;
        }
        Some(t.virt.as_ref()?.fields.get(field)?.hashed_name as i32)
    }

    fn field(&self, obj_ty: air::v2::ir::TypeRef, field: usize) -> Option<(u32, Ty)> {
        let index = obj_ty.0 as usize;
        let k = self.bc.types.get(index)?.kind;
        if k != hl::hl_type_kind_HOBJ && k != hl::hl_type_kind_HSTRUCT {
            return None;
        }
        // wasm32 words are four bytes.
        let (offset, declared) =
            crate::layout::field_offset_and_kind_for(&self.bc.types, index, field, 4)?;
        let ty = if declared == hl::hl_type_kind_HBOOL {
            Ty::Bool
        } else if declared == hl::hl_type_kind_HI32 {
            Ty::I32
        } else if declared == hl::hl_type_kind_HI64 {
            Ty::I64
        } else if declared == hl::hl_type_kind_HF32 {
            Ty::F32
        } else if declared == hl::hl_type_kind_HF64 {
            Ty::F64
        } else if declared == hl::hl_type_kind_HVOID
            || declared == hl::hl_type_kind_HUI8
            || declared == hl::hl_type_kind_HUI16
        {
            return None;
        } else {
            Ty::Ptr
        };
        u32::try_from(offset).ok().map(|o| (o, ty))
    }

    fn dynamic_compare(&self, t: air::v2::ir::TypeRef) -> bool {
        let Some(ty) = self.bc.types.get(t.0 as usize) else {
            return false;
        };
        let k = ty.kind;
        k == hl::hl_type_kind_HDYN
            || k == hl::hl_type_kind_HNULL
            || k == hl::hl_type_kind_HOBJ
            || k == hl::hl_type_kind_HVIRTUAL
    }

    fn invalid_comparison(&self) -> i32 {
        hl::hl_invalid_comparison as i32
    }

    fn alloc(&self, type_index: usize) -> Option<Alloc> {
        let ty = self.bc.types.get(type_index)?;
        let k = ty.kind;
        if k == hl::hl_type_kind_HDYNOBJ {
            return Some(Alloc::DynObj);
        }
        if k == hl::hl_type_kind_HVIRTUAL {
            return Some(Alloc::Virtual);
        }
        if k != hl::hl_type_kind_HOBJ && k != hl::hl_type_kind_HSTRUCT {
            return None;
        }
        // The same decision the LLVM lowering makes: a class with no closure
        // bindings and a known size is allocated sized; a struct keeps the
        // general path, having no type header to stamp.
        let sized = (k == hl::hl_type_kind_HOBJ && self.data.sized_alloc)
            .then(|| {
                let no_bindings = ty.obj.as_ref().is_some_and(|o| o.bindings.is_empty());
                let size = crate::layout::object_layout_for(&self.bc.types, type_index, 4)
                    .map(|l| l.size);
                let tail = self.data.object_tails.get(&type_index).copied().unwrap_or(0);
                match (no_bindings, size) {
                    (true, Some(size)) if size > 0 => {
                        let flags = if self.data.object_drops.contains(&type_index) {
                            ash_std::rt::RT_OBJECT_HOST_DROP
                        } else {
                            0
                        };
                        u32::try_from(size as u64 + tail).ok().map(|n| (n, flags))
                    }
                    _ => None,
                }
            })
            .flatten();
        Some(Alloc::Object { sized })
    }

    fn write_barriers(&self) -> bool {
        self.data.barriers
    }

    fn globals_symbol(&self) -> Option<String> {
        self.data.globals.clone()
    }

    fn type_symbol(&self, type_index: usize) -> Option<String> {
        self.data.types.get(&type_index).cloned()
    }

    fn signature(&self, findex: usize) -> Option<Sig> {
        let Some(f) = self.bc.functions.iter().find(|f| f.findex as usize == findex) else {
            let n = self.runtime_native(findex)?;
            let sig = self.sig_of(self.bc.types[n.type_.0].fun.as_ref()?);
            let narrow = |t: &Ty| *t == Ty::Other;
            return (!sig.params.iter().any(narrow) && !narrow(&sig.ret)).then_some(sig);
        };
        let fun = self.bc.types[f.type_.0].fun.as_ref()?;
        Some(self.sig_of(fun))
    }
}

/// Emit every function of `bc` that the direct backend can, as one relocatable
/// object.
pub fn emit_object(
    bc: &DecodedBytecode,
    symbols: &HashMap<usize, String>,
    data: &ProgramData,
    opts: Options,
) -> ObjectEmitted {
    let program = BytecodeProgram { bc, symbols, data };
    let module = crate::air_pipeline::AshModule::new(bc);
    let mut optimized = Vec::new();
    for f in &bc.functions {
        let Ok(o) = crate::air_pipeline::optimized(&module, f) else {
            continue;
        };
        let Some(sig) = program.signature(f.findex as usize) else {
            continue;
        };
        optimized.push((f.findex as usize, sig, o));
    }
    let inputs: Vec<Input> = optimized
        .iter()
        .map(|(findex, sig, o)| Input {
            findex: *findex,
            sig: sig.clone(),
            body: &o.ir,
        })
        .collect();
    ash_wasm_emit::emit_object(&program, &inputs, opts)
}
