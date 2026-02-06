use anyhow::{anyhow, Result};
use hlbc::types::{EnumConstruct, ObjField, Type, TypeObj};
use hlbc::Bytecode;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::types::{
    AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType,
};
use inkwell::values::{AnyValue, FunctionValue, GlobalValue, PointerValue};
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::path::Path;

use crate::native_lib::{init_std_library, NativeFunctionResolver};
use crate::types::{FunPtr, Str, ValueTypeKind};
use crate::values::Object;
pub struct AshModule<'ctx> {
    pub(crate) context: &'ctx Context,
    pub(crate) module: Module<'ctx>,
    pub(crate) builder: Builder<'ctx>,
    pub(crate) execution_engine: ExecutionEngine<'ctx>,
    pub(crate) bytecode: Bytecode,
    pub(crate) int_globals: Vec<GlobalValue<'ctx>>,
    pub(crate) float_globals: Vec<GlobalValue<'ctx>>,
    pub(crate) string_globals: Vec<GlobalValue<'ctx>>,
    pub(crate) type_cache: HashMap<usize, AnyTypeEnum<'ctx>>,
    pub(crate) dynamic_type: Option<StructType<'ctx>>,
    pub(crate) dynamic_obj_type: Option<StructType<'ctx>>,
    pub(crate) function_type: Option<StructType<'ctx>>,
    pub(crate) abstract_types: HashMap<Str, AnyTypeEnum<'ctx>>,
    pub(crate) enum_types: HashMap<Str, StructType<'ctx>>,
    pub(crate) type_info_globals: HashMap<usize, GlobalValue<'ctx>>,
    pub(crate) function_indexes: HashMap<usize, FunPtr>,
    pub(crate) function_values: HashMap<usize, FunctionValue<'ctx>>,
    pub(crate) native_function_resolver: NativeFunctionResolver,
}

impl<'ctx> AshModule<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .expect("Failed to initialize execution engine");

        let native_function_resolver = NativeFunctionResolver::new();

        AshModule {
            context,
            module,
            builder: context.create_builder(),
            execution_engine,
            bytecode: Bytecode::default(),
            int_globals: Vec::new(),
            float_globals: Vec::new(),
            string_globals: Vec::new(),
            type_cache: HashMap::new(),
            dynamic_type: None,
            dynamic_obj_type: None,
            function_type: None,
            abstract_types: HashMap::new(),
            enum_types: HashMap::new(),
            type_info_globals: HashMap::new(),
            function_indexes: HashMap::new(),
            function_values: HashMap::new(),
            native_function_resolver,
        }
    }

    pub fn load_bytecode<P: AsRef<Path>>(&mut self, path: P) -> Result<()> {
        let bc = Bytecode::from_file(path).map_err(|e| anyhow::Error::new(e))?;
        self.bytecode = bc;
        Ok(())
    }

    pub fn load_libraries(&mut self, libraries: HashMap<Str, &Path>) -> Result<()> {
        for (name, path) in libraries {
            self.native_function_resolver.load_library(&name, path)?
        }
        Ok(())
    }

    pub fn initialize(&mut self) -> Result<()> {
        // Preload std library
        init_std_library()?;
        
        // Store ints as global variables
        self.int_globals = self
            .bytecode
            .ints
            .iter()
            .enumerate()
            .map(|(i, &int_value)| {
                let int_type = self.context.i32_type();
                let global = self
                    .module
                    .add_global(int_type, None, &format!("int_{}", i));
                global.set_initializer(&int_type.const_int(int_value as u64, false));
                global
            })
            .collect();

        // Store floats as global variables
        self.float_globals = self
            .bytecode
            .floats
            .iter()
            .enumerate()
            .map(|(i, &float_value)| {
                let float_type = self.context.f32_type();
                let global = self
                    .module
                    .add_global(float_type, None, &format!("float_{}", i));
                global.set_initializer(&float_type.const_float(float_value as f64));
                global
            })
            .collect();

        // Store strings as global variables
        self.string_globals = self
            .bytecode
            .strings
            .iter()
            .enumerate()
            .map(|(i, string_value)| {
                let string_type = self.context.i8_type().array_type(string_value.len() as u32);
                let global = self
                    .module
                    .add_global(string_type, None, &format!("string_{}", i));
                let string_value = self.context.const_string(string_value.as_bytes(), false);
                global.set_initializer(&string_value);
                global
            })
            .collect();

        // Initialize types
        self.initialize_types()?;
        self.initialize_indexes()?;

        // Build entrypoint function
        self.create_function_value(self.bytecode.entrypoint.0)?;

        Ok(())
    }

    fn initialize_types(&mut self) -> Result<()> {
        let types = self.bytecode.types.clone();
        for (index, ty) in types.iter().enumerate() {
            // println!("{:?}", ty);
            let llvm_type = self.convert_hlbc_type_to_llvm(ty)?;
            self.type_cache.insert(index, llvm_type);
            self.create_type_info_global(index, ty, llvm_type)?;
        }
        Ok(())
    }

    fn initialize_indexes(&mut self) -> Result<()> {
        let functions = self.bytecode.functions.clone();
        let natives = self.bytecode.natives.clone();
        let types = self.bytecode.types.clone();

        for f in functions.iter() {
            // Normal functions
            self.function_indexes
                .insert(f.findex.0, FunPtr::Fun(f.clone()));
        }
        for n in natives.iter() {
            // Native extern functions
            self.function_indexes
                .insert(n.findex.0, FunPtr::Native(n.clone()));

            self.create_function_value(n.findex.0)?;
        }

        for (i, _type) in types.iter().enumerate() {
            if let Some(__type) = self.type_cache.get(&i) {
                match _type {
                    Type::Obj(obj) | Type::Struct(obj) => {
                        let struct_type = __type.into_struct_type();
                        Object::create_type(self, struct_type, obj)?;
                    }
                    _ => {}
                }
            }
            // let llvm_type = &self.type_cache[&i];
            // Todo: store global values
        }

        Ok(())
    }

    pub(crate) fn convert_hlbc_type_to_llvm(&mut self, ty: &Type) -> Result<AnyTypeEnum<'ctx>> {
        let types = self.bytecode.types.clone();
        let strings = self.bytecode.strings.clone();
        match ty {
            Type::Void => Ok(self.context.void_type().into()),
            Type::UI8 => Ok(self.context.i8_type().into()),
            Type::UI16 => Ok(self.context.i16_type().into()),
            Type::I32 => Ok(self.context.i32_type().into()),
            Type::I64 => Ok(self.context.i64_type().into()),
            Type::F32 => Ok(self.context.f32_type().into()),
            Type::F64 => Ok(self.context.f64_type().into()),
            Type::Bool => Ok(self.context.bool_type().into()),
            Type::Bytes => Ok(self
                .context
                .i8_type()
                .ptr_type(inkwell::AddressSpace::default())
                .into()),
            Type::Obj(type_obj) | Type::Struct(type_obj) => {
                let obj_type = self.context.opaque_struct_type(&strings[type_obj.name.0]); // Object::create_type(self, type_obj)?;
                Ok(obj_type.as_any_type_enum())
            }
            Type::Array => {
                let array_type = self.context.opaque_struct_type("haxe.lang.Array");
                Ok(array_type.into())
            }
            Type::Type => {
                let type_type = self.context.opaque_struct_type("haxe.lang.Type");
                Ok(type_type.into())
            }
            Type::Ref(type_index) | Type::Packed(type_index) => {
                if let Some(referenced_type) = self.type_cache.get(&type_index.0) {
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
                        AnyTypeEnum::VoidType(_) => self
                            .context
                            .i8_type()
                            .ptr_type(inkwell::AddressSpace::default())
                            .into(),
                    })
                } else {
                    Err(anyhow!("Referenced type not found in cache"))
                }
            }
            Type::Virtual { fields } => self.create_virtual_type(fields.as_slice()),
            Type::Dyn => Ok(self.get_or_create_dynamic_type().into()),
            Type::DynObj => Ok(self.get_or_create_dynamic_obj_type().into()),
            Type::Fun(f) | Type::Method(f) => Ok(self.create_function_type(f)?.into()),
            Type::Abstract { name } => self.handle_abstract_type(name.0),
            Type::Enum {
                name,
                global,
                constructs,
            } => self.handle_enum_type(name.0, global.0, constructs),
            Type::Null(t) => {
                let typ = self.bytecode.types[t.0].clone();
                self.handle_null_type(&typ)
            }
            // Add more cases for other Type variants as needed
            _ => Err(anyhow!("Unsupported type {:?}", ty)),
        }
    }

    pub fn get_or_create_any_type(&mut self, type_idx: usize) -> Result<AnyTypeEnum<'ctx>> {
        let types = self.bytecode.types.clone();
        if let Some(type_) = self.type_cache.get(&type_idx) {
            Ok(type_.clone())
        } else {
            self.convert_hlbc_type_to_llvm(&types[type_idx])
        }
    }

    fn create_type_info_global(
        &mut self,
        type_index: usize,
        ty: &Type,
        llvm_type: AnyTypeEnum<'ctx>,
    ) -> Result<()> {
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

        let global_name = format!("type_info_{}", type_index);
        let global = self.module.add_global(type_info_struct, None, &global_name);
        global.set_linkage(inkwell::module::Linkage::External);

        let type_kind = self.get_type_kind(ty);
        let type_name = self.get_type_name(ty);

        let type_ptr = match llvm_type {
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
                .const_int(type_kind as u64, false)
                .into(),
            self.context
                .i32_type()
                .const_int(type_index as u64, false)
                .into(),
            self.create_string_constant(&type_name).into(),
            type_ptr.into(),
        ]);

        global.set_initializer(&init);

        self.type_info_globals.insert(type_index, global);

        Ok(())
    }

    fn get_type_kind(&self, ty: &Type) -> u32 {
        match ty {
            Type::Void => ValueTypeKind::HVOID.into(),
            Type::UI8 => ValueTypeKind::HUI8.into(),
            Type::UI16 => ValueTypeKind::HUI16.into(),
            Type::I32 => ValueTypeKind::HI32.into(),
            Type::I64 => ValueTypeKind::HI64.into(),
            Type::F32 => ValueTypeKind::HF32.into(),
            Type::F64 => ValueTypeKind::HF64.into(),
            Type::Bool => ValueTypeKind::HBOOL.into(),
            Type::Bytes => ValueTypeKind::HBYTES.into(),
            Type::Dyn => ValueTypeKind::HDYN.into(),
            Type::Fun(_) => ValueTypeKind::HFUN.into(),
            Type::Method(_) => ValueTypeKind::HMETHOD.into(),
            Type::Obj(_) => ValueTypeKind::HOBJ.into(),
            Type::Struct(_) => ValueTypeKind::HSTRUCT.into(),
            Type::Array => ValueTypeKind::HARRAY.into(),
            Type::Type => ValueTypeKind::HTYPE.into(),
            Type::Ref(_) => ValueTypeKind::HREF.into(),
            Type::Virtual { .. } => ValueTypeKind::HVIRTUAL.into(),
            Type::Abstract { .. } => ValueTypeKind::HABSTRACT.into(),
            Type::Enum { .. } => ValueTypeKind::HENUM.into(),
            Type::Null(_) => ValueTypeKind::HNULL.into(),
            Type::DynObj => ValueTypeKind::HDYNOBJ.into(),
            Type::Packed(_) => ValueTypeKind::HPACKED.into(),
            // Add more cases as needed
        }
    }

    pub(crate) fn get_type_index(&self, llvm_type: AnyTypeEnum<'ctx>) -> Result<usize> {
        match llvm_type {
            AnyTypeEnum::IntType(int_type) => match int_type.get_bit_width() {
                1 => Ok(self
                    .bytecode
                    .types
                    .iter()
                    .position(|t| matches!(t, Type::Bool))
                    .unwrap()),
                8 => Ok(self
                    .bytecode
                    .types
                    .iter()
                    .position(|t| matches!(t, Type::UI8))
                    .unwrap()),
                16 => Ok(self
                    .bytecode
                    .types
                    .iter()
                    .position(|t| matches!(t, Type::UI16))
                    .unwrap()),
                32 => Ok(self
                    .bytecode
                    .types
                    .iter()
                    .position(|t| matches!(t, Type::I32))
                    .unwrap()),
                64 => Ok(self
                    .bytecode
                    .types
                    .iter()
                    .position(|t| matches!(t, Type::I64))
                    .unwrap()),
                _ => Err(anyhow!("Unsupported integer bit width")),
            },
            AnyTypeEnum::FloatType(float_type) => {
                // match float_type {
                //     32 => Ok(self.bytecode.types.iter().position(|t| matches!(t, Type::F32)).unwrap()),
                //     64 => Ok(self.bytecode.types.iter().position(|t| matches!(t, Type::F64)).unwrap()),
                //     _ => Err(anyhow!("Unsupported float bit width")),
                // }
                // In LLVM, we can compare the float type directly with the context's float types
                if float_type == self.context.f32_type() {
                    Ok(self
                        .bytecode
                        .types
                        .iter()
                        .position(|t| matches!(t, Type::F32))
                        .unwrap())
                } else if float_type == self.context.f64_type() {
                    Ok(self
                        .bytecode
                        .types
                        .iter()
                        .position(|t| matches!(t, Type::F64))
                        .unwrap())
                } else {
                    Err(anyhow!("Unsupported float type"))
                }
            }
            AnyTypeEnum::PointerType(ptr_type) => {
                match ptr_type.as_any_type_enum() {
                    AnyTypeEnum::IntType(int_type) if int_type.get_bit_width() == 8 => {
                        // This is likely a string or bytes
                        Ok(self
                            .bytecode
                            .types
                            .iter()
                            .position(|t| matches!(t, Type::Bytes))
                            .unwrap())
                    }
                    AnyTypeEnum::StructType(struct_type) => {
                        // This could be an object or array
                        if let Some(name) = struct_type.get_name() {
                            let type_name = name.to_str().unwrap();
                            if type_name == "Array" {
                                Ok(self
                                    .bytecode
                                    .types
                                    .iter()
                                    .position(|t| matches!(t, Type::Array))
                                    .unwrap())
                            } else {
                                self.bytecode.types.iter()
                                    .position(|t| matches!(t, Type::Obj(obj) if self.get_obj_type_name(obj.clone()) == type_name))
                                    .ok_or_else(|| anyhow!("Object type not found: {}", type_name))
                            }
                        } else {
                            Err(anyhow!("Anonymous struct types are not supported"))
                        }
                    }
                    _ => Err(anyhow!("Unsupported pointer element type")),
                }
            }
            AnyTypeEnum::StructType(struct_type) => {
                if let Some(name) = struct_type.get_name() {
                    let type_name = name.to_str().unwrap();
                    self.bytecode.types.iter()
                        .position(|t| matches!(t, Type::Obj(obj) if self.get_obj_type_name(obj.clone()) == type_name))
                        .ok_or_else(|| anyhow!("Struct type not found: {}", type_name))
                } else {
                    Err(anyhow!("Anonymous struct types are not supported"))
                }
            }
            AnyTypeEnum::VoidType(_) => Ok(self
                .bytecode
                .types
                .iter()
                .position(|t| matches!(t, Type::Void))
                .unwrap()),
            // Add more cases as needed
            _ => Err(anyhow!("Unsupported type for GetType operation")),
        }
    }

    fn get_type_name(&self, ty: &Type) -> Str {
        let type_name = match ty {
            Type::Void => Str::from("Void"),
            Type::UI8 => Str::from("UInt8"),
            Type::UI16 => Str::from("UInt16"),
            Type::I32 => Str::from("Int32"),
            Type::I64 => Str::from("Int64"),
            Type::F32 => Str::from("Float32"),
            Type::F64 => Str::from("Float64"),
            Type::Bool => Str::from("Bool"),
            Type::Bytes => Str::from("Bytes"),
            Type::Dyn => Str::from("Dynamic"),
            Type::Fun(t) | Type::Method(t) => {
                let args: Vec<Str> = t
                    .args
                    .iter()
                    .map(|a| self.get_type_name_by_index(a.0))
                    .collect();
                let args = args.join(",");
                let ret = self.get_type_name_by_index(t.ret.0);
                Str::from(format!("Func<({}):{}>", args, ret))
            }
            Type::Obj(type_obj) | Type::Struct(type_obj) => Str::from(format!(
                "Object<{}>",
                self.get_obj_type_name(type_obj.clone())
            )),
            Type::Array => Str::from("Array"),
            Type::Type => Str::from("Type"),
            Type::Ref(type_index) => {
                let typ = self.get_type_name_by_index(type_index.0).clone();
                Str::from(format!("Ref<{}>", typ.as_str()))
            }
            Type::Virtual { .. } => Str::from("Virtual"),
            Type::Abstract { name } => {
                Str::from(format!("abstract<{}>", self.get_abstract_type_name(name.0)))
            }
            Type::Enum { name, .. } => {
                Str::from(format!("enum<{}>", self.get_enum_type_name(name.0)))
            }
            Type::Null(underlying) => Str::from(&format!(
                "Null<{}>",
                self.get_type_name(&self.bytecode.types[underlying.0])
            )),
            Type::DynObj => Str::from("DynamicObject"),
            Type::Packed(underlying) => Str::from(&format!(
                "Packed<{}>",
                self.get_type_name(&self.bytecode.types[underlying.0])
            )),
            // Add more cases as needed
        };

        type_name
    }

    fn get_type_name_by_index(&self, type_index: usize) -> Str {
        self.get_type_name(self.bytecode.types.get(type_index).expect("Unknown type"))
    }

    pub(crate) fn get_obj_type_name(&self, type_obj: TypeObj) -> Str {
        self.bytecode
            .strings
            .get(type_obj.name.0)
            .expect("UnknownObj")
            .clone()
    }

    fn get_abstract_type_name(&self, name_ref: usize) -> Str {
        self.bytecode
            .strings
            .get(name_ref)
            .expect("UnknownAbstract")
            .clone()
    }

    fn get_enum_type_name(&self, name_ref: usize) -> Str {
        self.bytecode
            .strings
            .get(name_ref)
            .expect("UnknownEnum")
            .clone()
    }

    fn create_string_constant(&self, s: &str) -> PointerValue<'ctx> {
        let string_type = self.context.i8_type().array_type(s.len() as u32 + 1);
        let string_global = self.module.add_global(string_type, None, "type_info_name");
        string_global.set_linkage(inkwell::module::Linkage::Internal);
        string_global.set_constant(true);

        let string_const = self.context.const_string(s.as_bytes(), true);
        string_global.set_initializer(&string_const);

        string_global.as_pointer_value()
    }

    fn create_virtual_type(&mut self, fields: &[ObjField]) -> Result<AnyTypeEnum<'ctx>> {
        let types = self.bytecode.types.clone();
        let field_types: Vec<_> = fields
            .iter()
            .map(|field| {
                self.convert_hlbc_type_to_llvm(&types[field.t.0])
                    .expect("expected to resolve type from bytecode")
            })
            .collect();

        // Create an opaque struct type
        let struct_type = self.context.opaque_struct_type("virtual_type");

        // Convert AnyTypeEnum to BasicTypeEnum where possible, use pointers for complex types
        let basic_field_types: Vec<BasicTypeEnum> = field_types
            .iter()
            .map(|t| match t {
                AnyTypeEnum::ArrayType(at) => at.ptr_type(inkwell::AddressSpace::default()).into(),
                AnyTypeEnum::FloatType(ft) => (*ft).into(),
                AnyTypeEnum::IntType(it) => (*it).into(),
                AnyTypeEnum::PointerType(pt) => (*pt).into(),
                AnyTypeEnum::StructType(st) => st.ptr_type(inkwell::AddressSpace::default()).into(),
                AnyTypeEnum::VectorType(vt) => vt.ptr_type(inkwell::AddressSpace::default()).into(),
                _ => self
                    .context
                    .i8_type()
                    .ptr_type(inkwell::AddressSpace::default())
                    .into(), // Use a pointer for unsupported types
            })
            .collect();

        // Set the body of the struct type
        struct_type.set_body(&basic_field_types, false);

        Ok(struct_type.into())
    }

    fn handle_abstract_type(&mut self, name_ref: usize) -> Result<AnyTypeEnum<'ctx>> {
        let abstract_name = &self.bytecode.strings[name_ref];

        // Check if we've already created this abstract type
        if let Some(existing_type) = self.abstract_types.get(abstract_name) {
            return Ok(*existing_type);
        }

        // For now, we'll represent abstract types as opaque structs
        // In a more complete implementation, you might want to look up
        // the underlying type and use that instead
        let abstract_type = self.context.opaque_struct_type(abstract_name);

        // Store the new abstract type
        self.abstract_types
            .insert(abstract_name.clone(), abstract_type.into());

        Ok(abstract_type.into())
    }

    fn handle_enum_type(
        &mut self,
        name_ref: usize,
        global: usize,
        constructs: &[EnumConstruct],
    ) -> Result<AnyTypeEnum<'ctx>> {
        let enum_name = self.bytecode.strings[name_ref].clone();

        // Check if we've already created this enum type
        if let Some(existing_type) = self.enum_types.get(&enum_name) {
            return Ok((*existing_type).into());
        }

        // Create a new struct type for the enum
        let enum_struct = self.context.opaque_struct_type(&enum_name);

        // Create the enum body
        let tag_type = self.context.i32_type(); // Tag to identify which construct
                                                // let max_params = constructs.iter().map(|c| c.params.len()).max().unwrap_or(0);
        let mut param_types: Vec<BasicTypeEnum> =
            vec![self.context.i8_type().into(); constructs.len()];

        // Create global constructor structs for each construct
        for construct in constructs.iter() {
            let construct_name = self.bytecode.strings[construct.name.0].clone();
            let types = self.bytecode.types.clone();
            let _param_types: Vec<_> = construct
                .params
                .iter()
                .map(|param_type| {
                    self.convert_hlbc_type_to_llvm(&types[param_type.0])
                        .expect("expected to tranform type")
                })
                .collect();

            let _param_types = _param_types
                .iter()
                .map(|t| match t {
                    AnyTypeEnum::ArrayType(at) => {
                        at.ptr_type(inkwell::AddressSpace::default()).into()
                    }
                    AnyTypeEnum::FloatType(ft) => (*ft).into(),
                    AnyTypeEnum::IntType(it) => (*it).into(),
                    AnyTypeEnum::PointerType(pt) => (*pt).into(),
                    AnyTypeEnum::StructType(st) => {
                        st.ptr_type(inkwell::AddressSpace::default()).into()
                    }
                    AnyTypeEnum::VectorType(vt) => {
                        vt.ptr_type(inkwell::AddressSpace::default()).into()
                    }
                    _ => self
                        .context
                        .i8_type()
                        .ptr_type(inkwell::AddressSpace::default())
                        .into(), // Use a pointer for unsupported types
                })
                .collect::<Vec<BasicTypeEnum>>();

            let construct_struct = self
                .context
                .opaque_struct_type(&format!("{}_{}", enum_name, construct_name));
            construct_struct.set_body(&_param_types, false);

            param_types.push(construct_struct.into());
        }

        enum_struct.set_body(param_types.clone().as_slice(), false);

        // Store the new enum type
        self.enum_types.insert(enum_name.clone(), enum_struct);

        Ok(enum_struct.into())
    }

    fn create_enum_constructor(
        &mut self,
        enum_name: &str,
        enum_struct: StructType<'ctx>,
        // tag: u32,
        construct: &EnumConstruct,
    ) -> Result<BasicTypeEnum> {
        let construct_name = self.bytecode.strings[construct.name.0].clone();
        let types = self.bytecode.types.clone();
        let param_types: Result<Vec<_>, _> = construct
            .params
            .iter()
            .map(|param_type| self.convert_hlbc_type_to_llvm(&types[param_type.0]))
            .collect();
        let param_types = param_types?;
        let param_types = param_types
            .iter()
            .map(|t| match t {
                AnyTypeEnum::ArrayType(at) => at.ptr_type(inkwell::AddressSpace::default()).into(),
                AnyTypeEnum::FloatType(ft) => (*ft).into(),
                AnyTypeEnum::IntType(it) => (*it).into(),
                AnyTypeEnum::PointerType(pt) => (*pt).into(),
                AnyTypeEnum::StructType(st) => st.ptr_type(inkwell::AddressSpace::default()).into(),
                AnyTypeEnum::VectorType(vt) => vt.ptr_type(inkwell::AddressSpace::default()).into(),
                _ => self
                    .context
                    .i8_type()
                    .ptr_type(inkwell::AddressSpace::default())
                    .into(), // Use a pointer for unsupported types
            })
            .collect::<Vec<BasicTypeEnum>>();

        let construct_struct = self
            .context
            .opaque_struct_type(&format!("{}_{}", enum_name, construct_name));
        construct_struct.set_body(&param_types, false);

        Ok(construct_struct.into())
    }

    pub(crate) fn get_or_create_function_type(&mut self) -> Result<StructType<'ctx>> {
        if let Some(fun_type) = self.function_type {
            Ok(fun_type)
        } else {
            let fun_type = self.context.opaque_struct_type("haxe.lang.Function");
            self.function_type = Some(fun_type);
            Ok(fun_type)
        }
    }

    fn get_or_create_dynamic_type(&mut self) -> StructType<'ctx> {
        if let Some(dyn_type) = self.dynamic_type {
            dyn_type
        } else {
            let s = self.context.opaque_struct_type("haxe.lang.Dynamic");
            self.dynamic_type = Some(s.clone());
            s
        }

        // Define the structure of the dynamic type
        // let type_info_type = self.context.i32_type(); // You might want to use a more complex type for type info
        // let data_ptr_type = self.context.i8_type().ptr_type(inkwell::AddressSpace::default());

        // dyn_type.set_body(&[type_info_type.into(), data_ptr_type.into()], false);

        // Cache the dynamic type for future use

        // dyn_type
    }
    fn get_or_create_dynamic_obj_type(&mut self) -> StructType<'ctx> {
        if let Some(dyn_type) = self.dynamic_type {
            dyn_type
        } else {
            let s = self.context.opaque_struct_type("haxe.lang.DynamicObject");
            self.dynamic_obj_type = Some(s.clone());
            s
        }

        // Define the structure of the dynamic obj type
        // let type_info_type = self.context.i32_type(); // You might want to use a more complex type for type info
        // let data_ptr_type = self.context.i8_type().ptr_type(inkwell::AddressSpace::default());

        // dyn_type.set_body(&[type_info_type.into(), data_ptr_type.into()], false);

        // Cache the dynamic type for future use
        // self.dynamic_type = Some(dyn_type);

        // dyn_type
    }

    fn handle_null_type(&mut self, underlying_type: &Type) -> Result<AnyTypeEnum<'ctx>> {
        // Convert the underlying type
        let llvm_type = self.convert_hlbc_type_to_llvm(underlying_type)?;

        // Create a nullable version of the type
        match llvm_type {
            AnyTypeEnum::IntType(int_type) => {
                Ok(int_type.ptr_type(inkwell::AddressSpace::default()).into())
            }
            AnyTypeEnum::FloatType(float_type) => {
                Ok(float_type.ptr_type(inkwell::AddressSpace::default()).into())
            }
            AnyTypeEnum::PointerType(ptr_type) => Ok(ptr_type.into()),
            AnyTypeEnum::StructType(struct_type) => Ok(struct_type
                .ptr_type(inkwell::AddressSpace::default())
                .into()),
            AnyTypeEnum::ArrayType(array_type) => {
                Ok(array_type.ptr_type(inkwell::AddressSpace::default()).into())
            }
            AnyTypeEnum::VectorType(vector_type) => Ok(vector_type
                .ptr_type(inkwell::AddressSpace::default())
                .into()),
            AnyTypeEnum::VoidType(_) => Err(anyhow!("Cannot create nullable void type")),
            AnyTypeEnum::FunctionType(fn_type) => {
                Ok(fn_type.ptr_type(inkwell::AddressSpace::default()).into())
            }
        }
    }

    // fn get_type_name(&self, type_obj: &TypeObj) -> Str {
    //     let bytecode = &self.bytecode;
    //     let name_index = type_obj.name.0;
    //     bytecode.strings[name_index].clone()
    // }

    pub fn get_int_global(&self, index: usize) -> Option<GlobalValue<'ctx>> {
        self.int_globals.get(index).cloned()
    }

    pub fn get_float_global(&self, index: usize) -> Option<GlobalValue<'ctx>> {
        self.float_globals.get(index).cloned()
    }

    pub fn get_string_global(&self, index: usize) -> Option<GlobalValue<'ctx>> {
        self.string_globals.get(index).cloned()
    }

    pub fn print_llvm_ir(&self) {
        if let Err(err) = self.module.verify() {
            println!("Error: {}", err);
        } else {
            self.module.print_to_stderr();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Result;
    use std::env;
    use std::path::PathBuf;
    use std::str::FromStr;

    #[test]
    fn test_ash_module() -> Result<()> {
        let context = Context::create();
        let mut libraries = HashMap::new();
     
        let mut ash_module = AshModule::new(&context, "test_module");

        let mut cwd = PathBuf::from_str(env!("CARGO_MANIFEST_DIR"))?;
        cwd.push("test/test.hl");

        // Note: This test will fail unless you provide a valid bytecode file
        ash_module.load_bytecode(&cwd)?;
        // Load libraries before initializing
        ash_module.load_libraries(libraries)?;
        ash_module.initialize()?;
        //
        // Test global lookups
        assert!(ash_module.get_int_global(0).is_some());
        assert!(ash_module.get_float_global(0).is_some());
        assert!(ash_module.get_string_global(0).is_some());
        //

        ash_module.print_llvm_ir();
     
        if let Some(main) = ash_module
            .function_values
            .get(&ash_module.bytecode.entrypoint.0)
        {
            println!("Main ==> {}", main.print_to_string());
        }

        Ok(())
    }
}
