use crate::bytecode::{BytecodeDecoder, DecodedBytecode};
use crate::hl::*;
use crate::native_lib::{init_std_library, NativeFunctionResolver};
use crate::types::{HLType, HLTypeFun, HLTypeObj, TypeRef, ValueTypeKind};
use anyhow::{anyhow, Result};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::types::{
    AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType,
};
use inkwell::values::{
    AnyValue, AnyValueEnum, ArrayValue, AsValueRef, BasicValue, BasicValueEnum, FunctionValue, GenericValue, GlobalValue, IntValue, PointerValue, StructValue
};
use inkwell::{AddressSpace, OptimizationLevel};
use num_enum::TryFromPrimitive;
use std::cell::RefCell;
use std::collections::btree_map::IntoValues;
use std::collections::HashMap;
use std::ffi::{c_void, CStr};
use std::io::{stderr, Write};
use std::mem;
use std::ops::Add;
use std::path::Path;
use std::rc::Rc;
use std::slice;

use super::function::{FuncPtr, FunctionBuilder};

use ash_macro::load_symbol;

#[load_symbol]
extern "C" {
    fn hlp_init_virtual(vt: *mut hl_type, _ctx: *mut hl_module_context);
    fn hlp_init_enum(vt: *mut hl_type, _ctx: *mut hl_module_context);
    fn hlp_obj_field_fetch(t: *mut hl_type, fid: i32) -> *mut hl_obj_field;
}

pub struct JITModule<'ctx> {
    pub(crate) context: &'ctx Context,
    pub(crate) module: Module<'ctx>,
    pub(crate) builder: Builder<'ctx>,
    pub(crate) execution_engine: ExecutionEngine<'ctx>,
    pub(crate) bytecode: DecodedBytecode,
    pub(crate) types_: Vec<HLType>,
    pub(crate) type_cache: HashMap<usize, AnyTypeEnum<'ctx>>,
    pub(crate) initialized_type_cache: HashMap<usize, BasicValueEnum<'ctx>>,
    pub(crate) type_info_globals: HashMap<usize, GlobalValue<'ctx>>,
    pub(crate) findexes: HashMap<usize, FuncPtr>,
    pub(crate) func_types: Vec<*mut hl_type>,
    pub(crate) func_cache: HashMap<usize, FunctionValue<'ctx>>,
    pub(crate) native_function_resolver: NativeFunctionResolver,
    pub(crate) int_globals: Vec<GlobalValue<'ctx>>,
    pub(crate) float_globals: Vec<GlobalValue<'ctx>>,
    pub(crate) string_globals: Vec<GlobalValue<'ctx>>,
    pub(crate) bytes_globals: Vec<GlobalValue<'ctx>>,
    /// C-side globals: inttoptr constants pointing into globals_data.
    /// Both JIT code and native stdlib access the same memory.
    pub(crate) globals: HashMap<usize, PointerValue<'ctx>>,
    /// Backing memory for globals — shared between JIT and native code.
    pub(crate) globals_data: Vec<*mut c_void>,
    pub(crate) pending_compilations: Vec<usize>,
    pub(crate) c_ptr_to_type_index: HashMap<usize, usize>,
    pub(crate) hl_type_struct_type: Option<StructType<'ctx>>,
    /// Function pointer table indexed by findex, used by hl_module_context.
    pub(crate) functions_ptrs: Vec<*mut c_void>,
}

impl<'ctx> JITModule<'ctx> {
    pub fn new(context: &'ctx Context, path: &Path) -> Self {
        init_std_library();

        let bytecode = BytecodeDecoder::decode(path).expect("Failed to decode bytecode");

        let module = context.create_module("Hashlink");
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .expect("Failed to initialize execution engine");

        let native_function_resolver = NativeFunctionResolver::new();

        let types_ = bytecode.types.clone();

        let mut module = JITModule {
            context,
            module,
            builder: context.create_builder(),
            execution_engine,
            bytecode,
            type_cache: HashMap::new(),
            initialized_type_cache: HashMap::new(),
            findexes: HashMap::new(),
            func_cache: HashMap::new(),
            native_function_resolver,
            types_,
            int_globals: Vec::new(),
            float_globals: Vec::new(),
            string_globals: Vec::new(),
            bytes_globals: Vec::new(),
            type_info_globals: HashMap::new(),
            pending_compilations: Vec::new(),
            globals: HashMap::new(),
            globals_data: Vec::new(),
            c_ptr_to_type_index: HashMap::new(),
            func_types: Vec::new(),
            hl_type_struct_type: None,
            functions_ptrs: Vec::new(),
        };

        module.string_globals = module
            .bytecode
            .strings
            .iter()
            .enumerate()
            .map(|(i, s)| {
                // Convert UTF-8 string to UTF-16 (HashLink uses UTF-16 internally)
                let utf16: Vec<u16> = s.encode_utf16().chain(std::iter::once(0)).collect(); // null-terminated
                let utf16_bytes: Vec<u8> = utf16.iter().flat_map(|c| c.to_le_bytes()).collect();
                let string_val = module.context.const_string(&utf16_bytes, false);
                let global = module.module.add_global(
                    module
                        .context
                        .i8_type()
                        .array_type(utf16_bytes.len() as u32),
                    None,
                    &format!("String_{}", i),
                );
                global.set_initializer(&string_val);
                global.set_constant(true);
                // Ensure 2-byte alignment for UTF-16
                global.set_alignment(2);
                global
            })
            .collect();

        module.int_globals = module
            .bytecode
            .ints
            .iter()
            .enumerate()
            .map(|(i, v)| {
                let int_val = module.context.i32_type().const_int(*v as u64, false);
                let global = module.module.add_global(
                    module.context.i32_type(),
                    None,
                    &format!("Int_{}", i),
                );
                global.set_initializer(&int_val);
                global.set_constant(true);
                global
            })
            .collect();

        module.float_globals = module
            .bytecode
            .floats
            .iter()
            .enumerate()
            .map(|(i, v)| {
                let float_val = module.context.f64_type().const_float(*v);
                let global = module.module.add_global(
                    module.context.f64_type(),
                    None,
                    &format!("Float_{}", i),
                );
                global.set_initializer(&float_val);
                global.set_constant(true);
                global
            })
            .collect();

        module.bytes_globals = module
            .bytecode
            .bytes_pos
            .iter()
            .enumerate()
            .map(|(i, &pos)| {
                let end = module
                    .bytecode
                    .bytes_pos
                    .get(i + 1)
                    .copied()
                    .unwrap_or(module.bytecode.bytes_data.len());
                let slice = &module.bytecode.bytes_data[pos..end];
                let val = module.context.const_string(slice, false);
                let global = module.module.add_global(
                    module.context.i8_type().array_type(slice.len() as u32),
                    None,
                    &format!("Bytes_{}", i),
                );
                global.set_initializer(&val);
                global.set_constant(true);
                global
            })
            .collect();

        module
            .initialize_globals()
            .expect("Failed to initialize globals");

        module
            .init_natives()
            .expect("Failed to initialize native functions");

        module.init_indexes().expect("Failed to initialie indexes");

        module
            .init_constants()
            .expect("Failed to initialize constants");

        module
    }

    pub fn initialize_globals(&mut self) -> Result<()> {
        let nglobals = self.bytecode.globals.len();
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        // Allocate C-side memory for all globals (pointer-sized slots, zeroed).
        // Both JIT code and native stdlib will access this same memory.
        self.globals_data = vec![std::ptr::null_mut(); nglobals];

        for (index, _global_type) in self.bytecode.globals.clone().iter().enumerate() {
            // Create an inttoptr constant pointing to the C-side slot
            let slot_addr = unsafe { self.globals_data.as_ptr().add(index) } as u64;
            let addr_int = self.context.i64_type().const_int(slot_addr, false);
            let slot_ptr = addr_int.const_to_pointer(ptr_type);
            self.globals.insert(index, slot_ptr);
        }

        Ok(())
    }

    fn init_natives(&mut self) -> Result<()> {
        let natives = self.bytecode.natives.clone();
        let len = natives.len();
        for i in 0..len {
            let native_f = &natives[i];
            let fun_value = self.init_native_func(native_f)?;
            self.func_cache.insert(native_f.findex as usize, fun_value);
        }
        Ok(())
    }

    fn init_indexes(&mut self) -> Result<()> {
        let natives = self.bytecode.natives.clone();
        let native_len = natives.len();

        let funs = self.bytecode.functions.clone();
        let funs_len = funs.len();

        self.func_types = vec![std::ptr::null_mut(); funs_len + native_len];

        // Pre-allocate functions_ptrs — will be filled with actual addresses before execution
        let max_findex = std::cmp::max(
            funs.iter().map(|f| f.findex as usize).max().unwrap_or(0),
            natives.iter().map(|n| n.findex as usize).max().unwrap_or(0),
        ) + 1;
        self.functions_ptrs = vec![std::ptr::null_mut(); max_findex];

        let cache: Rc<RefCell<HashMap<usize, *mut hl_type>>> =
            Rc::new(RefCell::new(HashMap::new()));

        for i in 0..funs_len {
            let findex = (&funs[i]).findex as usize;
            self.findexes.insert(findex, FuncPtr::Fun(funs[i].clone()));
            let types = self.types_.clone();
            let tindex = funs[i].type_.clone();
            let type_fun = types[tindex.0.clone()].fun.as_ref().unwrap();
            self.func_types[findex] = unsafe {
                Box::into_raw(Box::new(hl_type {
                    kind: hl_type_kind_HFUN,
                    __bindgen_anon_1: hl_type__bindgen_ty_1 {
                        fun: Box::into_raw(Box::new(hl_type_fun {
                            args: self.convert_type_refs_to_c(&type_fun.args, Rc::clone(&cache))?,
                            ret: self.convert_type_ref_to_c_cached(
                                &type_fun.ret.clone(),
                                Rc::clone(&cache),
                            )?,
                            nargs: type_fun.args.len() as i32,
                            parent: if let Some(parent) = &type_fun.parent {
                                self.convert_type_ref_to_c_cached(
                                    &parent.clone(),
                                    Rc::clone(&cache),
                                )?
                            } else {
                                std::ptr::null_mut()
                            },
                            closure_type: hl_type_fun__bindgen_ty_1 {
                                kind: 0,
                                p: std::ptr::null_mut(),
                            },
                            closure: hl_type_fun__bindgen_ty_2 {
                                args: std::ptr::null_mut(),
                                ret: std::ptr::null_mut(),
                                nargs: 0,
                                parent: std::ptr::null_mut(),
                            },
                        })),
                    },
                    vobj_proto: std::ptr::null_mut(),
                    mark_bits: std::ptr::null_mut(),
                }))
            };
        }

        for i in 0..native_len {
            let findex = (&natives[i]).findex as usize;
            self.findexes
                .insert(findex, FuncPtr::Native(natives[i].clone()));
            let types = self.types_.clone();
            let tindex = natives[i].type_.clone();
            let type_fun = types[tindex.0.clone()].fun.as_ref().unwrap();
            self.func_types[findex] = unsafe {
                Box::into_raw(Box::new(hl_type {
                    kind: hl_type_kind_HFUN,
                    __bindgen_anon_1: hl_type__bindgen_ty_1 {
                        fun: Box::into_raw(Box::new(hl_type_fun {
                            args: self.convert_type_refs_to_c(&type_fun.args, Rc::clone(&cache))?,
                            ret: self.convert_type_ref_to_c_cached(
                                &type_fun.ret.clone(),
                                Rc::clone(&cache),
                            )?,
                            nargs: type_fun.args.len() as i32,
                            parent: if let Some(parent) = &type_fun.parent {
                                self.convert_type_ref_to_c_cached(
                                    &parent.clone(),
                                    Rc::clone(&cache),
                                )?
                            } else {
                                std::ptr::null_mut()
                            },
                            closure_type: hl_type_fun__bindgen_ty_1 {
                                kind: 0,
                                p: std::ptr::null_mut(),
                            },
                            closure: hl_type_fun__bindgen_ty_2 {
                                args: std::ptr::null_mut(),
                                ret: std::ptr::null_mut(),
                                nargs: 0,
                                parent: std::ptr::null_mut(),
                            },
                        })),
                    },
                    vobj_proto: std::ptr::null_mut(),
                    mark_bits: std::ptr::null_mut(),
                }))
            };
        }

        for (i, type_) in self.types_.clone().iter().enumerate() {
            match type_.kind {
                hl_type_kind_HOBJ | hl_type_kind_HSTRUCT => {
                    let obj = type_.obj.as_ref().expect("Expected to get object type");
                    let global_value_index = obj.global_value.wrapping_sub(1) as usize;
                    for proto in &obj.proto {
                        let pfindex = proto.findex as usize;
                        if let Some(f) = self.findexes.get_mut(&pfindex) {
                            match f {
                                FuncPtr::Fun(fun) => fun.field_name = Some(proto.name.clone()),
                                _ => {}
                            }
                        }
                    }
                    let len = (obj.bindings.len() / 2) as i32;

                    let native_type =
                        self.convert_type_ref_to_c_cached(&TypeRef(i), Rc::clone(&cache))?;

                    assert!(!native_type.is_null());
                    unsafe {
                        let t = native_type.read();
                        (*t.__bindgen_anon_1.obj).m = Box::into_raw(Box::new(hl_module_context {
                            alloc: unsafe { mem::zeroed() },
                            functions_ptrs: self.functions_ptrs.as_mut_ptr() as *mut *mut c_void,
                            functions_types: self.func_types.as_mut_ptr(),
                        }));
                        let obj = t.__bindgen_anon_1.obj.read();

                        for j in 0..obj.nbindings {
                            let fid = *obj.bindings.add((j << 1) as usize) as usize;
                            let mid = *obj.bindings.add(((j << 1) | 1) as usize) as usize;

                            let __field = unsafe { __hlp_obj_field_fetch(native_type, fid as i32) };

                            if !__field.is_null() {
                                let ff = unsafe { __field.read() };
                                let name = unsafe {
                                    CStr::from_ptr(ff.name as *const i8).to_string_lossy()
                                };

                                match (*ff.t).kind {
                                    hl_type_kind_HFUN | hl_type_kind_HDYN => {
                                        if let Some(f) = self.findexes.get_mut(&mid) {
                                            match f {
                                                FuncPtr::Fun(fun) => {
                                                    fun.field_name = Some(name.to_string())
                                                }
                                                _ => {}
                                            }
                                        }
                                    }
                                    _ => {}
                                }
                            }

                            // Note: binding functions will be populated in functions_ptrs
                            // during setup_functions_ptrs (if compiled).
                        }
                    }

                    let type_struct = self.get_hl_type_struct_type()?;

                    // Create an integer constant from the pointer address
                    let ptr_as_int = self.context.i64_type().const_int(native_type as u64, false);

                    // Cast the integer to a pointer
                    let ptr_to_type =
                        ptr_as_int.const_to_pointer(type_struct.ptr_type(AddressSpace::default()));

                    // println!("{:?}", ptr_to_type.print_to_string().to_string());

                    self.initialized_type_cache.insert(i, ptr_to_type.into());
                }
                hl_type_kind_HENUM => {
                    let enum_type =
                        self.convert_type_ref_to_c_cached(&TypeRef(i), Rc::clone(&cache))?;
                    unsafe {
                        __hlp_init_enum(enum_type, std::ptr::null_mut());
                        self.convert_from_c_type(enum_type)?;
                    }
                    let type_struct = self.get_hl_type_struct_type()?;

                    // Create an integer constant from the pointer address
                    let ptr_as_int = self.context.i64_type().const_int(enum_type as u64, false);

                    // Cast the integer to a pointer
                    let ptr_to_type =
                        ptr_as_int.const_to_pointer(type_struct.ptr_type(AddressSpace::default()));

                    // println!("{:?}", ptr_to_type.print_to_string().to_string());

                    self.initialized_type_cache.insert(i, ptr_to_type.into());
                }
                hl_type_kind_HVIRTUAL => {
                    let mut _hl_type =
                        self.convert_type_ref_to_c_cached(&TypeRef(i), Rc::clone(&cache))?;

                    unsafe {
                        __hlp_init_virtual(_hl_type, std::ptr::null_mut());
                        self.convert_from_c_type(_hl_type)?;
                    }

                    let virt_type = self.get_or_create_any_type(i)?.into_struct_type();
                    // Create an integer constant from the pointer address
                    let ptr_as_int = self.context.i64_type().const_int(_hl_type as u64, false);

                    // Cast the integer to a pointer
                    let ptr_to_type =
                        ptr_as_int.const_to_pointer(virt_type.ptr_type(AddressSpace::default()));

                    // println!("{:?}", ptr_to_type.print_to_string().to_string());

                    self.initialized_type_cache.insert(i, ptr_to_type.into());
                }
                _ => {}
            }
        }

        let mut main_obj = HLTypeObj::default();
        if let FuncPtr::Fun(entry_function) = self
            .findexes
            .get_mut(&(self.bytecode.entrypoint as usize))
            .filter(|f| matches!(**f, FuncPtr::Fun(_)))
            .expect("Expected to get entrypoint function")
        {
            main_obj.name = "".to_owned();
            entry_function.obj = Some(main_obj);
            entry_function.field_name = Some(String::from("init"));

            let index = self.bytecode.entrypoint as usize;
            let (_, is_pending) = self.get_or_create_function_value(index)?;
            if is_pending {
                // Compile main function
                self.compile_function(index)?;
            }
        }

        Ok(())
    }

    /// Materialize bytecode constants into globals_data.
    /// Constants are pre-allocated objects (typically String literals) stored in globals.
    /// Each constant specifies a global index and field values to populate.
    /// Must be called AFTER setup_functions_ptrs (needs native function addresses).
    pub(crate) fn init_constants(&mut self) -> Result<()> {
        if self.bytecode.constants.is_empty() {
            return Ok(());
        }

        // Build type_index -> c_type_ptr mapping from c_ptr_to_type_index
        let type_to_c_ptr: HashMap<usize, *mut hl_type> = self
            .c_ptr_to_type_index
            .iter()
            .map(|(&ptr, &idx)| (idx, ptr as *mut hl_type))
            .collect();

        // Resolve native functions we need
        type FnAllocObj = unsafe extern "C" fn(*mut hl_type) -> *mut vdynamic;
        type FnGetObjRt = unsafe extern "C" fn(*mut hl_type) -> *mut hl_runtime_obj;

        let fn_alloc_obj: FnAllocObj = unsafe {
            let ptr = self
                .native_function_resolver
                .resolve_function("std", "hlp_alloc_obj")
                .map_err(|e| anyhow!("Cannot resolve hlp_alloc_obj: {}", e))?;
            std::mem::transmute(ptr)
        };
        let fn_get_obj_rt: FnGetObjRt = unsafe {
            let ptr = self
                .native_function_resolver
                .resolve_function("std", "hlp_get_obj_rt")
                .map_err(|e| anyhow!("Cannot resolve hlp_get_obj_rt: {}", e))?;
            std::mem::transmute(ptr)
        };

        let bytecode = self.bytecode.clone();

        for constant in &bytecode.constants {
            let global_idx = constant.global as usize;
            if global_idx >= bytecode.globals.len() || global_idx >= self.globals_data.len() {
                continue;
            }

            let type_idx = bytecode.globals[global_idx].0;
            let hl_type_rust = &bytecode.types[type_idx];

            let c_type_ptr = match type_to_c_ptr.get(&type_idx) {
                Some(&ptr) => ptr,
                None => continue,
            };

            if c_type_ptr.is_null() {
                continue;
            }

            let kind = hl_type_rust.kind;

            if kind == hl_type_kind_HOBJ || kind == hl_type_kind_HSTRUCT {
                let obj_data = match hl_type_rust.obj.as_ref() {
                    Some(o) => o,
                    None => continue,
                };

                // Allocate the object
                let obj_ptr = unsafe { fn_alloc_obj(c_type_ptr) };
                if obj_ptr.is_null() {
                    continue;
                }

                // Store in globals_data
                self.globals_data[global_idx] = obj_ptr as *mut c_void;

                // Also update global_value in hl_type_obj so stdlib can find it
                unsafe {
                    let obj = (*c_type_ptr).__bindgen_anon_1.obj;
                    if !obj.is_null() && !(*obj).global_value.is_null() {
                        *(*obj).global_value = obj_ptr as *mut c_void;
                    }
                }

                // Get runtime object for field offsets
                let rt = unsafe { fn_get_obj_rt(c_type_ptr) };
                if rt.is_null() || constant.fields.is_empty() {
                    continue;
                }

                // Calculate field start offset (skip parent fields)
                let start = unsafe { (*rt).nfields as usize - obj_data.fields.len() };

                // Fill in constant fields
                for (j, &field_value) in constant.fields.iter().enumerate() {
                    if j >= obj_data.fields.len() {
                        break;
                    }

                    let field_type_idx = obj_data.fields[j].type_.0;
                    let field_kind = bytecode.types[field_type_idx].kind;

                    let field_offset = unsafe { *(*rt).fields_indexes.add(j + start) };
                    let field_addr =
                        unsafe { (obj_ptr as *mut u8).add(field_offset as usize) };

                    match field_kind {
                        hl_type_kind_HBYTES => {
                            // field_value is a string index → convert to UTF-16
                            let str_idx = field_value as usize;
                            if str_idx < bytecode.strings.len() {
                                let s = &bytecode.strings[str_idx];
                                let mut utf16: Vec<u16> = s.encode_utf16().collect();
                                utf16.push(0); // null terminator
                                let ptr = utf16.as_ptr();
                                std::mem::forget(utf16);
                                unsafe {
                                    *(field_addr as *mut *const u16) = ptr;
                                }
                            }
                        }
                        hl_type_kind_HI32 | hl_type_kind_HBOOL => {
                            // field_value is an index into the ints table
                            let int_val = bytecode.ints.get(field_value as usize)
                                .copied()
                                .unwrap_or(field_value);
                            unsafe {
                                *(field_addr as *mut i32) = int_val;
                            }
                        }
                        _ => {
                            // For other types, try ints table lookup too
                            let int_val = bytecode.ints.get(field_value as usize)
                                .copied()
                                .unwrap_or(field_value);
                            unsafe {
                                *(field_addr as *mut i32) = int_val;
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    // Helper function to ensure we have a valid insertion block
    fn ensure_valid_insert_block(&self, builder: &Builder<'ctx>) -> Result<BasicBlock<'ctx>> {
        if let Some(block) = builder.get_insert_block() {
            Ok(block)
        } else {
            let void_type = self.context.void_type();
            let function_type = void_type.fn_type(&[], false);
            let function = self
                .module
                .add_function("temp_function", function_type, None);
            let basic_block = self.context.append_basic_block(function, "entry");

            Ok(basic_block)
        }
    }

    pub fn get_or_create_any_type(&mut self, type_idx: usize) -> Result<AnyTypeEnum<'ctx>> {
        let types = self.types_.clone();
        if let Some(type_) = self.type_cache.get(&type_idx) {
            Ok(type_.clone())
        } else {
            let t = self.convert_hl_type_to_llvm_type(&types[type_idx])?;
            self.type_cache.insert(type_idx, t);
            Ok(t)
        }
    }
    pub(crate) fn convert_hl_type_to_llvm_type(
        &mut self,
        ty: &HLType,
    ) -> Result<AnyTypeEnum<'ctx>> {
        let types = self.bytecode.types.clone();
        let strings = self.bytecode.strings.clone();
        match ty.kind {
            hl_type_kind_HVOID => Ok(self.context.void_type().into()),
            hl_type_kind_HUI8 => Ok(self.context.i8_type().into()),
            hl_type_kind_HUI16 => Ok(self.context.i16_type().into()),
            hl_type_kind_HI32 => Ok(self.context.i32_type().into()),
            hl_type_kind_HI64 => Ok(self.context.i64_type().into()),
            hl_type_kind_HF32 => Ok(self.context.f32_type().into()),
            hl_type_kind_HF64 => Ok(self.context.f64_type().into()),
            hl_type_kind_HBOOL => Ok(self.context.bool_type().into()),
            hl_type_kind_HBYTES => Ok(self
                .context
                .i8_type()
                .ptr_type(inkwell::AddressSpace::default())
                .into()),
            hl_type_kind_HOBJ | hl_type_kind_HSTRUCT => {
                self.create_obj_type(ty.obj.as_ref().expect("expected to get object type"))
            }
            hl_type_kind_HARRAY => {
                let array_type = self.context.opaque_struct_type("varray");
                let ptr_type = self
                    .context
                    .i8_type()
                    .ptr_type(inkwell::AddressSpace::default());

                array_type.set_body(
                    &[
                        ptr_type.into(),                // pointer to type
                        ptr_type.into(),                // pointer to array element type
                        self.context.i32_type().into(), // array size
                        self.context.i32_type().into(), // __pad: #force align on 16 bytes for double
                    ],
                    false,
                );
                Ok(array_type.into())
            }
            hl_type_kind_HTYPE => {
                let hl_type_struct = self.get_hl_type_struct_type()?;
                Ok(hl_type_struct.into())
            }
            hl_type_kind_HREF | hl_type_kind_HPACKED => {
                let tparam_idx = ty.tparam.as_ref().expect("Expected type parameter").0;
                // Recursively ensure the referenced type is converted first
                let referenced_type = self.get_or_create_any_type(tparam_idx)?;
                Ok(match referenced_type {
                    AnyTypeEnum::ArrayType(t) => {
                        t.ptr_type(inkwell::AddressSpace::default()).into()
                    }
                    AnyTypeEnum::FloatType(t) => {
                        t.ptr_type(inkwell::AddressSpace::default()).into()
                    }
                    AnyTypeEnum::FunctionType(t) => {
                        t.ptr_type(inkwell::AddressSpace::default()).into()
                    }
                    AnyTypeEnum::IntType(t) => {
                        t.ptr_type(inkwell::AddressSpace::default()).into()
                    }
                    AnyTypeEnum::PointerType(t) => {
                        t.ptr_type(inkwell::AddressSpace::default()).into()
                    }
                    AnyTypeEnum::StructType(t) => {
                        t.ptr_type(inkwell::AddressSpace::default()).into()
                    }
                    AnyTypeEnum::VectorType(t) => {
                        t.ptr_type(inkwell::AddressSpace::default()).into()
                    }
                    AnyTypeEnum::VoidType(_) => {
                        self.context.ptr_type(AddressSpace::default()).into()
                    }
                })
            }
            hl_type_kind_HVIRTUAL => {
                let v = ty.virt.as_ref().expect("Expected to get virtual type");
                self.create_virtual_type(v)
            }
            hl_type_kind_HDYN => {
                let dyn_type = self.context.opaque_struct_type("vdynamic");
                dyn_type.set_body(
                    &[
                        self.context.ptr_type(AddressSpace::default()).into(), // type
                        self.context.ptr_type(AddressSpace::default()).into(), // union
                    ],
                    false,
                );
                Ok(dyn_type.into())
            }
            hl_type_kind_HDYNOBJ => self.create_dynobj_type(ty),
            hl_type_kind_HFUN | hl_type_kind_HMETHOD => Ok(self
                .create_function_type(ty.fun.as_ref().expect("Expected to get function type"))?
                .into()),
            hl_type_kind_HABSTRACT => self.handle_abstract_type(
                ty.abs_name
                    .as_ref()
                    .expect("Expected to get abstract type name")
                    .clone(),
            ),
            hl_type_kind_HENUM => {
                let tenum = ty.tenum.as_ref().expect("Expected to get enum type");
                self.handle_enum_type(tenum)
            }
            hl_type_kind_HNULL => {
                let null = ty
                    .tparam
                    .as_ref()
                    .expect("Expected to get underlying Null type parameter");
                self.handle_null_type(null)
            }
            // Add more cases for other Type variants as needed
            _ => Err(anyhow!("Unsupported type {:?}", ty)),
        }
    }

    fn create_obj_type(
        &self,
        ty: &crate::types::HLTypeObj,
    ) -> std::result::Result<AnyTypeEnum<'ctx>, anyhow::Error> {
        let obj_type = self.context.opaque_struct_type(&ty.name);

        let i32_type = self.context.i32_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let name = self.context.const_string(ty.name.as_bytes(), false);

        obj_type.set_body(
            &[
                i32_type.into(),                                      // nfields
                i32_type.into(),                                      // nproto
                i32_type.into(),                                      // nbindings
                name.get_type().into(),                               // name
                ptr_type.into(),                                      // _super type
                ptr_type.into(),                                      // fields
                ptr_type.into(),                                      // proto
                i32_type.array_type(ty.bindings.len() as u32).into(), // bindings
                ptr_type.into(),                                      // global_value
                ptr_type.into(),                                      // module_context
                ptr_type.into(),                                      // *mut hl_runtime_obj
            ],
            false,
        );

        Ok(obj_type.as_any_type_enum())
    }

    fn create_dynobj_type(
        &self,
        ty: &HLType,
    ) -> std::result::Result<AnyTypeEnum<'ctx>, anyhow::Error> {
        let dynobj_type = self.context.opaque_struct_type("vdynobj");
        let i32_type = self.context.i32_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        dynobj_type.set_body(
            &[
                ptr_type.into(), // type
                ptr_type.into(), // *mut hl_field_lookup
                ptr_type.into(), // raw_data
                ptr_type.into(), // values
                i32_type.into(), // nfields
                i32_type.into(), // nvalues
                ptr_type.into(), //virtuals
            ],
            false,
        );

        Ok(dynobj_type.as_any_type_enum())
    }

    fn create_virtual_type(
        &self,
        v: &crate::types::HLTypeVirtual,
    ) -> std::result::Result<AnyTypeEnum<'ctx>, anyhow::Error> {
        let virtual_signature: String = format!(
            "haxe.Virtual<{}>",
            v.fields
                .iter()
                .map(|f| self.get_type_name_by_index(f.type_.0))
                .collect::<Vec<String>>()
                .join(",")
        );
        let virt_type = self.context.opaque_struct_type(&virtual_signature);
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i32_type = self.context.i32_type();
        virt_type.set_body(
            &[
                ptr_type.into(),                                    // fields
                i32_type.into(),                                    // nfields
                i32_type.into(),                                    // data_size
                i32_type.array_type(v.indexes.len() as u32).into(), // indexes
                ptr_type.into(),                                    // *mut hl_field_lookup
            ],
            false,
        );
        Ok(virt_type.as_any_type_enum())
    }

    fn handle_abstract_type(
        &self,
        name: String,
    ) -> std::result::Result<AnyTypeEnum<'ctx>, anyhow::Error> {
        let name = format!("haxe.Abstract<{}>", name);
        let abs_type = self.context.opaque_struct_type(&name);
        Ok(abs_type.as_any_type_enum())
    }

    fn handle_null_type(
        &self,
        _null: &TypeRef,
    ) -> std::result::Result<AnyTypeEnum<'ctx>, anyhow::Error> {
        // Null types are always heap-allocated (nullable pointer wrappers)
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        Ok(ptr_type.as_any_type_enum())
    }

    fn handle_enum_type(
        &self,
        tenum: &crate::types::HLTypeEnum,
    ) -> std::result::Result<AnyTypeEnum<'ctx>, anyhow::Error> {
        let enum_type = self.context.opaque_struct_type(&tenum.name);

        let name = self.context.const_string(tenum.name.as_bytes(), false);
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        enum_type.set_body(
            &[
                name.get_type().into(),         // name
                self.context.i32_type().into(), //nconstructs
                ptr_type.into(),                // constructs
                ptr_type.into(),
            ],
            false,
        );

        Ok(enum_type.as_any_type_enum())
    }

    // pub(crate) fn get_type_index(&self, llvm_type: AnyTypeEnum<'ctx>) -> Result<usize> {
    //     match llvm_type {
    //         AnyTypeEnum::IntType(int_type) => match int_type.get_bit_width() {
    //             1 => Ok(self
    //                 .bytecode
    //                 .types
    //                 .iter()
    //                 .position(|t| matches!(t, Type::Bool))
    //                 .unwrap()),
    //             8 => Ok(self
    //                 .bytecode
    //                 .types
    //                 .iter()
    //                 .position(|t| matches!(t, Type::UI8))
    //                 .unwrap()),
    //             16 => Ok(self
    //                 .bytecode
    //                 .types
    //                 .iter()
    //                 .position(|t| matches!(t, Type::UI16))
    //                 .unwrap()),
    //             32 => Ok(self
    //                 .bytecode
    //                 .types
    //                 .iter()
    //                 .position(|t| matches!(t, Type::I32))
    //                 .unwrap()),
    //             64 => Ok(self
    //                 .bytecode
    //                 .types
    //                 .iter()
    //                 .position(|t| matches!(t, Type::I64))
    //                 .unwrap()),
    //             _ => Err(anyhow!("Unsupported integer bit width")),
    //         },
    //         AnyTypeEnum::FloatType(float_type) => {
    //             // match float_type {
    //             //     32 => Ok(self.bytecode.types.iter().position(|t| matches!(t, Type::F32)).unwrap()),
    //             //     64 => Ok(self.bytecode.types.iter().position(|t| matches!(t, Type::F64)).unwrap()),
    //             //     _ => Err(anyhow!("Unsupported float bit width")),
    //             // }
    //             // In LLVM, we can compare the float type directly with the context's float types
    //             if float_type == self.context.f32_type() {
    //                 Ok(self
    //                     .bytecode
    //                     .types
    //                     .iter()
    //                     .position(|t| matches!(t, Type::F32))
    //                     .unwrap())
    //             } else if float_type == self.context.f64_type() {
    //                 Ok(self
    //                     .bytecode
    //                     .types
    //                     .iter()
    //                     .position(|t| matches!(t, Type::F64))
    //                     .unwrap())
    //             } else {
    //                 Err(anyhow!("Unsupported float type"))
    //             }
    //         }
    //         AnyTypeEnum::PointerType(ptr_type) => {
    //             match ptr_type.as_any_type_enum() {
    //                 AnyTypeEnum::IntType(int_type) if int_type.get_bit_width() == 8 => {
    //                     // This is likely a string or bytes
    //                     Ok(self
    //                         .bytecode
    //                         .types
    //                         .iter()
    //                         .position(|t| matches!(t, Type::Bytes))
    //                         .unwrap())
    //                 }
    //                 AnyTypeEnum::StructType(struct_type) => {
    //                     // This could be an object or array
    //                     if let Some(name) = struct_type.get_name() {
    //                         let type_name = name.to_str().unwrap();
    //                         if type_name == "Array" {
    //                             Ok(self
    //                                 .bytecode
    //                                 .types
    //                                 .iter()
    //                                 .position(|t| matches!(t, Type::Array))
    //                                 .unwrap())
    //                         } else {
    //                             self.bytecode.types.iter()
    //                                 .position(|t| matches!(t, Type::Obj(obj) if self.get_obj_type_name(obj.clone()) == type_name))
    //                                 .ok_or_else(|| anyhow!("Object type not found: {}", type_name))
    //                         }
    //                     } else {
    //                         Err(anyhow!("Anonymous struct types are not supported"))
    //                     }
    //                 }
    //                 _ => Err(anyhow!("Unsupported pointer element type")),
    //             }
    //         }
    //         AnyTypeEnum::StructType(struct_type) => {
    //             if let Some(name) = struct_type.get_name() {
    //                 let type_name = name.to_str().unwrap();

    //                 // self.bytecode.types.iter()
    //                 //     .position(|t| matches!(t, Type::Obj(obj) if self.get_obj_type_name(obj.clone()) == type_name))
    //                 //     .ok_or_else(|| anyhow!("Struct type not found: {}", type_name))
    //             } else {
    //                 Err(anyhow!("Anonymous struct types are not supported"))
    //             }
    //         }
    //         AnyTypeEnum::VoidType(_) => Ok(self
    //             .bytecode
    //             .types
    //             .iter()
    //             .position(|t| matches!(t, Type::Void))
    //             .unwrap()),
    //         // Add more cases as needed
    //         _ => Err(anyhow!("Unsupported type for GetType operation")),
    //     }
    // }

    fn get_type_name_by_index(&self, type_index: usize) -> String {
        self.get_type_name(self.bytecode.types.get(type_index).expect("Unknown type"))
    }

    fn get_type_name(&self, ty: &HLType) -> String {
        let type_name = match ty.kind {
            hl_type_kind_HVOID => String::from("haxe.Void"),
            hl_type_kind_HUI8 | hl_type_kind_HUI16 | hl_type_kind_HI32 | hl_type_kind_HI64 => {
                String::from("haxe.Number")
            }
            hl_type_kind_HF32 | hl_type_kind_HF64 => String::from("haxe.Float"),
            hl_type_kind_HBOOL => String::from("haxe.Bool"),
            hl_type_kind_HBYTES => String::from("haxe.Bytes"),
            hl_type_kind_HDYN => String::from("haxe.Dynamic"),
            hl_type_kind_HDYNOBJ => String::from("haxe.DynObject"),
            hl_type_kind_HFUN | hl_type_kind_HMETHOD => {
                let t = ty.fun.as_ref().expect("expected to get function type");
                let args: Vec<String> = t
                    .args
                    .iter()
                    .map(|a| self.get_type_name_by_index(a.0))
                    .collect();
                let args = args.join(",");
                let ret = self.get_type_name_by_index(t.ret.0);
                format!("Func<({}):{}>", args, ret)
            }
            hl_type_kind_HOBJ | hl_type_kind_HSTRUCT => {
                let t = ty.obj.as_ref().expect("expected to get object type");
                format!("haxe.ClassObject<{}>", t.name)
            }
            hl_type_kind_HARRAY => String::from("haxe.Array"),
            hl_type_kind_HTYPE => String::from("haxe.Type"),
            hl_type_kind_HREF => {
                let typ = self
                    .get_type_name_by_index(
                        ty.tparam.as_ref().expect("expect to get type parameter").0,
                    )
                    .clone();
                format!("haxe.Ref<{}>", typ)
            }
            hl_type_kind_HVIRTUAL => String::from("haxe.Virtual"),
            hl_type_kind_HABSTRACT => {
                format!(
                    "haxe.Abstract<{}>",
                    ty.abs_name
                        .as_ref()
                        .expect("expected to get abstract type name")
                )
            }
            hl_type_kind_HENUM => String::from(format!(
                "haxe.Enum<{}>",
                ty.tenum.as_ref().expect("expected to get enum type").name
            )),
            hl_type_kind_HNULL => format!(
                "haxe.Null<{}>",
                self.get_type_name_by_index(
                    ty.tparam.as_ref().expect("expect to get type parameter").0
                )
            ),
            hl_type_kind_HPACKED => format!(
                "haxe.Packed<{}>",
                self.get_type_name_by_index(
                    ty.tparam.as_ref().expect("expect to get type parameter").0
                )
            ),

            _ => {
                unreachable!()
            }
        };

        type_name
    }

    fn create_type_info_global(&mut self, type_index: usize) -> Result<()> {
        let type_ = &self.bytecode.types[type_index];
        let llvmtype = self
            .type_cache
            .get(&type_index)
            .ok_or_else(|| anyhow!("Type not found for index {}", type_index))?;

        let type_info_struct = self.context.opaque_struct_type("TypeInfo");
        type_info_struct.set_body(
            &[
                self.context.i32_type().into(), // Type kind (enum, struct, etc.)
                self.context.i32_type().into(), // Type index
                self.context
                    .i8_type()
                    .ptr_type(inkwell::AddressSpace::default())
                    .into(), // Type name
                self.context
                    .i8_type()
                    .ptr_type(inkwell::AddressSpace::default())
                    .into(), // Pointer to the actual type (as void*)
            ],
            false,
        );

        let type_name = self.get_type_name_by_index(type_index);

        let global_name = format!("type_info_{}", type_index);
        let global = self.module.add_global(type_info_struct, None, &global_name);
        global.set_linkage(inkwell::module::Linkage::External);

        let type_ptr = match llvmtype {
            AnyTypeEnum::ArrayType(t) => t.ptr_type(inkwell::AddressSpace::default()).const_null(),
            AnyTypeEnum::FloatType(t) => t.ptr_type(inkwell::AddressSpace::default()).const_null(),
            AnyTypeEnum::FunctionType(t) => {
                t.ptr_type(inkwell::AddressSpace::default()).const_null()
            }
            AnyTypeEnum::IntType(t) => t.ptr_type(inkwell::AddressSpace::default()).const_null(),
            AnyTypeEnum::PointerType(t) => t.const_null(),
            AnyTypeEnum::StructType(t) => t.ptr_type(inkwell::AddressSpace::default()).const_null(),
            AnyTypeEnum::VectorType(t) => t.ptr_type(inkwell::AddressSpace::default()).const_null(),
            AnyTypeEnum::VoidType(_) => self
                .context
                .i8_type()
                .ptr_type(inkwell::AddressSpace::default())
                .const_null(),
        };

        let init = type_info_struct.const_named_struct(&[
            self.context
                .i32_type()
                .const_int(type_.kind as u64, false)
                .into(),
            self.context
                .i32_type()
                .const_int(type_index as u64, false)
                .into(),
            self.create_type_info_string_constant(&type_name).into(),
            type_ptr.into(),
        ]);

        global.set_initializer(&init);

        self.type_info_globals.insert(type_index, global);

        Ok(())
    }

    fn create_type_info_string_constant(&self, s: &str) -> PointerValue<'ctx> {
        let string_type = self.context.i8_type().array_type(s.len() as u32 + 1);
        let string_global = self.module.add_global(string_type, None, "type_info_name");
        string_global.set_linkage(inkwell::module::Linkage::Internal);
        string_global.set_constant(true);

        let string_const = self.context.const_string(s.as_bytes(), true);
        string_global.set_initializer(&string_const);

        string_global.as_pointer_value()
    }

    pub fn get_int_global(&self, index: usize) -> Option<GlobalValue<'ctx>> {
        self.int_globals.get(index).cloned()
    }

    pub fn get_float_global(&self, index: usize) -> Option<GlobalValue<'ctx>> {
        self.float_globals.get(index).cloned()
    }

    pub fn get_string_global(&self, index: usize) -> Option<GlobalValue<'ctx>> {
        self.string_globals.get(index).cloned()
    }

    pub fn get_bytes_global(&self, index: usize) -> Option<GlobalValue<'ctx>> {
        self.bytes_globals.get(index).cloned()
    }

    pub fn struct_value_to_pointer(
        &self,
        struct_value: StructValue<'ctx>,
    ) -> Result<PointerValue<'ctx>> {
        let struct_type = struct_value.get_type();
        let function = self
            .module
            .get_first_function()
            .expect("No function available");
        let block = self.ensure_valid_insert_block(&self.builder)?;
        // let entry = function
        //     .get_first_basic_block()
        //     .expect("No entry block in function");

        // Create a new builder and position it at the start of the entry block

        self.builder
            .position_before(block.get_first_instruction().as_ref().unwrap());

        // Allocate memory for the struct
        let alloca = self.builder.build_alloca(struct_type, "struct_ptr")?;

        // Store the struct value in the allocated memory
        self.builder.build_store(alloca, struct_value);

        Ok(alloca)
    }
}
