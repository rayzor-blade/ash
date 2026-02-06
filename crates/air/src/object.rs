use crate::prelude::*;
use anyhow::Result;

impl<'ctx> AIR<'ctx> {
    pub fn create_object_type(
        &mut self,
        name: &str,
        fields: &[(&str, BasicTypeEnum<'ctx>)],
        methods: &[(&str, FunctionValue<'ctx>)],
    ) -> StructType<'ctx> {
        let struct_type = self.context.opaque_struct_type(name);
        // let mut field_types: Vec<BasicTypeEnum> = fields.iter().map(|(_, ty)| *ty).collect();
        let mut method_types: Vec<BasicTypeEnum> =
            fields.iter().map(|(_, ty)| *ty).collect();

        // Add a vtable pointer for methods
        let vtable_type = self.context.i8_type().ptr_type(AddressSpace::default());
        method_types.insert(0, vtable_type.into());

        struct_type.set_body(&method_types, false);
        // struct_type.set_body(&field_types, false);
        self.struct_types.insert(name.to_string(), struct_type);

        // Create and populate the vtable
        let vtable = self.create_vtable(name, methods);
        self.module
            .add_global(vtable.get_type(), None, &format!("{}_vtable", name))
            .set_initializer(&vtable);


        struct_type
    }

    fn create_vtable(
        &self,
        class_name: &str,
        methods: &[(&str, FunctionValue<'ctx>)],
    ) -> BasicValueEnum<'ctx> {
        let vtable_type = self.context.struct_type(
            &methods
                .iter()
                .map(|_| {
                    self.context
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
                .map(|(_, func)| func.as_global_value().as_pointer_value().into())
                .collect::<Vec<_>>(),
        );
        vtable.into()
    }

    pub fn create_method(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_count: usize,
        register_count: usize,
    ) -> Result<FunctionValue<'ctx>> {
        let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let function_type = i8_ptr_type.fn_type(&[i8_ptr_type.into()], false);
        let function = self.module.add_function(method_name, function_type, None);
        self.function = Some(function);
        
        let basic_block = self.create_basic_block("entry");
        self.builder.position_at_end(basic_block);

        
        self.registers.clear();

        // Create registers
        for _ in 0..register_count {
            self.create_register()?;
        }

        // 'this' pointer
        let this_ptr = function.get_nth_param(0).unwrap().into_pointer_value();
        self.variables.insert("this".to_string(), this_ptr);

        // The first argument is a pointer to the register array
        let reg_array_ptr = function.get_nth_param(0).unwrap().into_pointer_value();

        // Load arguments into the first `arg_count` registers
        for i in 0..arg_count {
            let arg_ptr = unsafe {
                self.builder.build_gep(
                    i8_ptr_type,
                    reg_array_ptr,
                    &[self.context.i32_type().const_int(i as u64, false)],
                    &format!("arg_ptr_{}", i),
                )?
            };

            let arg_value =
                self.builder
                    .build_load(arg_ptr.get_type(), arg_ptr, &format!("arg_{}", i))?;
            self.store_in_register(self.get_register(i), arg_value);
        }

        Ok(function)
    }
}

#[cfg(test)]
mod test {
    use super::AIR;
    use anyhow::Result;
    use inkwell::context::Context;
    use inkwell::IntPredicate;

    #[test]
    fn compile_simple_object() -> Result<()> {
        let context = Context::create();
        let mut air = AIR::new(&context, "module");

        let function = air.create_method("hl.Class", "getName", 1, 1)?;

        let object = air.create_object_type(
            "hl.Class",
            &[("_field", air.context.i32_type().into())],
            &[(function.get_name().to_str()?, function)],
        );

        println!("\n{}", air.module.print_to_string().to_string());

        Ok(())
    }
}
