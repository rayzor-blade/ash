use crate::prelude::*;
use anyhow::Result;
use inkwell::AddressSpace;

impl<'ctx> AIR<'ctx> {
    // Create a register (pointer to a generic value)
    pub(crate) fn create_register(&mut self) -> Result<PointerValue<'ctx>> {
        let reg_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let alloca = self
            .builder
            .build_alloca(reg_type, &format!("reg_{}", self.registers.len()))?;
        self.registers.push(alloca);
        Ok(alloca)
    }

    // Get a register by index
    pub(crate) fn get_register(&self, index: usize) -> PointerValue<'ctx> {
        self.registers[index]
    }

    // Store a value in a register
    pub(crate) fn store_in_register(&self, reg: PointerValue<'ctx>, value: BasicValueEnum<'ctx>) {
        self.builder.build_store(reg, value);
    }

    // Load a value from a register
    pub(crate) fn load_from_register(
        &self,
        ty: impl BasicType<'ctx>,
        reg: PointerValue<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>> {
        self.builder
            .build_load(ty, reg, "load")
            .map_err(|err| anyhow::Error::new(err))
    }

    // Create a function with HashLink-like calling convention
    pub fn create_function(
        &mut self,
        name: &str,
        arg_count: usize,
        register_count: usize,
    ) -> Result<FunctionValue<'ctx>> {
        let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let function_type = i8_ptr_type.fn_type(&[i8_ptr_type.into()], false);
        let function = self.module.add_function(name, function_type, None);

        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        self.function = Some(function);
        self.registers.clear();

        // Create registers
        for _ in 0..register_count {
            self.create_register()?;
        }

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

    // Call a function with HashLink-like calling convention
    pub fn call_function(
        &self,
        function: FunctionValue<'ctx>,
        args: &[usize],
    ) -> Result<BasicValueEnum<'ctx>> {
        let reg_array_type = self
            .context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .array_type(args.len() as u32);
        let reg_array = self.builder.build_alloca(reg_array_type, "reg_array")?;
        let default_type = self.context.i8_type().ptr_type(AddressSpace::default());
        for (i, &arg_reg) in args.iter().enumerate() {
            let arg_value =
                self.load_from_register(default_type, self.get_register(arg_reg))?;
            let arg_ptr = unsafe {
                self.builder.build_gep(
                    default_type,
                    reg_array,
                    &[
                        self.context.i32_type().const_int(0, false),
                        self.context.i32_type().const_int(i as u64, false),
                    ],
                    &format!("arg_ptr_{}", i),
                )?
            };
            self.builder.build_store(arg_ptr, arg_value);
        }

        let reg_array_ptr = self.builder.build_bit_cast(
            reg_array,
            self.context.i8_type().ptr_type(AddressSpace::default()),
            "reg_array_ptr",
        )?;

        self.build_call(function, &[reg_array_ptr.into()], "call")
    }

    pub fn build_return(&self, value: Option<BasicValueEnum<'ctx>>) {
        match value {
            Some(val) => self.builder.build_return(Some(&val)),
            None => self.builder.build_return(None),
        };
    }

    pub fn build_call(
        &self,
        function: FunctionValue<'ctx>,
        args: &[BasicMetadataValueEnum<'ctx>],
        name: &str,
    ) -> Result<BasicValueEnum<'ctx>> {
        Ok(self
            .builder
            .build_call(function, args, name)?
            .try_as_basic_value()
            .left()
            .unwrap())
    }
}

#[cfg(test)]
mod test {
    use super::AIR;
    use anyhow::Result;
    use inkwell::context::Context;
    use inkwell::IntPredicate;

    #[test]
    fn compile_simple_function() -> Result<()> {
        // let context = Context::create();
        // let mut air = AIR::new(&context, "example");
        // let i64_type = context.i64_type();
        // let function = air.create_function(
        //     "simple_function",
        //     &[
        //         ("x".to_string(), context.i64_type().into()),
        //         ("y".to_string(), context.i64_type().into()),
        //     ],
        //     context.i64_type().into(),
        // )?;

        // let i64_ptr_type = i64_type.ptr_type(inkwell::AddressSpace::default());
        // let x = air.variables["x"];
        // let y = air.variables["y"];

        // let x_val = air.builder.build_load(i64_type, x, "x_val")?;
        // let y_val = air.builder.build_load(i64_type, y, "y_val")?;

        // let sum = air.build_int_add(x_val.into(), y_val.into(), "sum")?;
        // let condition = air.build_int_compare(
        //     IntPredicate::SGT,
        //     sum.into(),
        //     air.context.i64_type().const_int(10, false).into(),
        //     "cmp",
        // )?;

        // let then_block = air.context.append_basic_block(function, "then");
        // let else_block = air.context.append_basic_block(function, "else");
        // let cont_block = air.context.append_basic_block(function, "cont");

        // air.build_conditional_branch(condition, then_block, else_block);

        // air.builder.position_at_end(then_block);
        // let then_value = air.build_int_mul(
        //     sum.into(),
        //     air.context.i64_type().const_int(2, false).into(),
        //     "then_value",
        // )?;
        // air.build_unconditional_branch(cont_block);

        // air.builder.position_at_end(else_block);
        // let else_value = sum;
        // air.build_unconditional_branch(cont_block);

        // air.builder.position_at_end(cont_block);
        // let phi = air.build_phi(
        //     &[(&then_value, then_block), (&else_value, else_block)],
        //     "result",
        // )?;

        // air.build_return(Some(phi));

        // // Verify the generated code
        // if function.verify(true) {
        //     println!("Function verified successfully!");
        // } else {
        //     unsafe { function.delete() };
        //     panic!("Invalid generated function");
        // }

        // // Print the LLVM IR
        // println!("\n{}", air.module.print_to_string().to_string());

        Ok(())
    }
}
