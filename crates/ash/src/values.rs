use crate::{module::AshModule, types::Str};
use anyhow::*;
use hlbc::types::{Type, TypeObj};
use inkwell::{
    types::{ AnyTypeEnum, BasicType, BasicTypeEnum,  StructType},
    values::{AnyValueEnum, FunctionValue, StructValue, BasicValueEnum},
    AddressSpace
};

#[derive(Debug, Clone)]
pub struct Object<'ctx> {
    pub _type: StructType<'ctx>,
    pub super_type: Option<StructType<'ctx>>,
    pub(crate) name: Str,
    pub(crate) fields: Vec<ObjectField<'ctx>>,
}

#[derive(Debug, Clone)]
pub struct ObjectField<'ctx> {
    pub _type: AnyTypeEnum<'ctx>,
    pub(crate) name: Str,
    pub(crate) value: Option<AnyValueEnum<'ctx>>,
}

impl<'ctx> Object<'ctx> {
    pub fn create_type(module: &mut AshModule<'ctx>, struct_type:StructType<'ctx>, obj: &TypeObj) -> Result<BasicTypeEnum<'ctx>> {
        let strings = module.bytecode.strings.clone();
        let types = module.bytecode.types.clone();
        let type_cache =  module.type_cache.clone();
        
        let name = module.get_obj_type_name(obj.clone());
        // let struct_type = module.context.opaque_struct_type(&name);
        let mut super_type = None;
      
        if let Some(super_type_ref) = obj.super_ {
            let super_type_obj = module.bytecode.types[super_type_ref.0].clone();
            super_type = match super_type_obj {
                Type::Obj(t) | Type::Struct(t) => {
                    if let Some(__type) = module.type_cache.get(&super_type_ref.0) {
                        Some(Self::create_type(module, __type.into_struct_type(), &t)?)
                    } else {
                        None
                    }
                },
                _ => None,
            }
        }
      

        let mut field_types: Vec<BasicTypeEnum<'ctx>> = obj
            .fields
            .iter()
            .map(|field| {
                let field_type =module.get_or_create_any_type(field.t.0).expect("expected to get field type");
                let type_ = match field_type {
                    AnyTypeEnum::ArrayType(t) => t.as_basic_type_enum(),
                    AnyTypeEnum::FloatType(t) => t.as_basic_type_enum(),
                    AnyTypeEnum::FunctionType(t) => module
                        .get_or_create_function_type()
                        .expect("expected to get opaque type for function")
                        .into(),
                    AnyTypeEnum::IntType(t) => t.as_basic_type_enum(),
                    AnyTypeEnum::PointerType(t) => t.as_basic_type_enum(),
                    AnyTypeEnum::StructType(t) => t.as_basic_type_enum(),
                    AnyTypeEnum::VectorType(t) => t.as_basic_type_enum(),
                    AnyTypeEnum::VoidType(t) => panic!("Unsupported field type"),
                };
                return type_;
            })
            .collect();

        // let mut methods: Vec<FunctionValue<'ctx>> = obj
        //     .protos
        //     .iter()
        //     .enumerate()
        //     .map(|(i, field)| {
        //         if let Some(f) = module.function_values.get(&field.findex.0) {
        //             f.clone()
        //         } else {
        //             module.create_function_value(field.findex.0).expect("expect to create function value")
        //         }
                
        //     })
        //     .collect();

        if let Some(super_) = super_type {
            field_types.insert(0, super_)
        } else {
            field_types.insert(0, module.context.struct_type(&[], false).into())
        }

        struct_type.set_body(&field_types, false);

        // // Add a vtable pointer for methods
        // let vtable_type = module.context.i8_type().ptr_type(AddressSpace::default());
        // field_types.insert(1, vtable_type.into());

        // // Create and populate the vtable
        // let vtable = Self::create_vtable(module, name.as_str(), methods.as_slice());
        // module
        //     .module
        //     .add_global(vtable.get_type(), None, &format!("{}_vtable", name))
        //     .set_initializer(&vtable);

        Ok(struct_type.into())
    }
    pub fn create_value(
        module: &mut AshModule<'ctx>,
        _type: StructType<'ctx>,
        obj: TypeObj,
    ) -> Result<StructValue<'ctx>> {
        let name = module.get_obj_type_name(obj.clone());
        // let mut super_type = None;
        // if let Some(super_type_ref) = obj.super_ {
        //     let super_type_obj = module.bytecode.types[super_type_ref.0].clone();
        //     super_type = match super_type_obj {
        //         Type::Obj(t) | Type::Struct(t) => Some(Self::create_type(module, &t)?),
        //         _ => None,
        //     }
        // }

        Ok(_type.const_named_struct(&[]))
    }

    fn create_vtable(
        module: &mut AshModule<'ctx>,
        class_name: &str,
        methods: &[FunctionValue<'ctx>],
    ) -> BasicValueEnum<'ctx> {
        let vtable_type = module.context.struct_type(
            &methods
                .iter()
                .map(|_| {
                    module.context
                        .i8_type()
                        .ptr_type(AddressSpace::default())
                        .into()
                })
                .collect::<Vec<_>>(),
            false,
        );
        let vtable = vtable_type.const_named_struct(
            &methods
                .iter()
                .map(|func| func.as_global_value().as_pointer_value().into())
                .collect::<Vec<_>>(),
        );
        vtable.into()
    }

    pub fn create_method(
        module: &mut AshModule<'ctx>,
        class_name: &str,
        method_name: &str,
        arg_count: usize,
        register_count: usize,
    ) -> Result<FunctionValue<'ctx>> {
        let i8_ptr_type = module.context.i8_type().ptr_type(AddressSpace::default());
        let function_type = i8_ptr_type.fn_type(&[i8_ptr_type.into()], false);
        let function = module.module.add_function(method_name, function_type, None);

        let basic_block = module.context.append_basic_block(function, "entry");

        // self.registers.clear();

        // Create registers
        for _ in 0..register_count {
            // self.create_register()?;

            // let reg_type = module.context.i8_type().ptr_type(AddressSpace::default());
            // let alloca = module
            //     .context
            //     .build_alloca(reg_type, &format!("reg_{}", self.registers.len()))?;
        }

        // 'this' pointer
        let this_ptr = function.get_nth_param(0).unwrap().into_pointer_value();
        // self.variables.insert("this".to_string(), this_ptr);

        // The first argument is a pointer to the register array
        let reg_array_ptr = function.get_nth_param(0).unwrap().into_pointer_value();

        // Load arguments into the first `arg_count` registers
        // for i in 0..arg_count {
        //     let arg_ptr = unsafe {
        //         self.builder.build_gep(
        //             i8_ptr_type,
        //             reg_array_ptr,
        //             &[self.context.i32_type().const_int(i as u64, false)],
        //             &format!("arg_ptr_{}", i),
        //         )?
        //     };

        //     let arg_value =
        //         self.builder
        //             .build_load(arg_ptr.get_type(), arg_ptr, &format!("arg_{}", i))?;
        //     self.store_in_register(self.get_register(i), arg_value);
        // }

        Ok(function)
    }
}
