use crate::prelude::*;
use anyhow::Result;
use std::collections::HashMap;

/// ASH Immediate Representation
/// ===
/// This struct provides a higher level LLVM IR wrapper for representation and construction of the
/// immediate language which will be used for JIT code building and compilation.
pub struct AIR<'ctx> {
    pub(crate) context: &'ctx Context,
    pub(crate) module: Module<'ctx>,
    pub(crate) builder: Builder<'ctx>,
    pub(crate) function: Option<FunctionValue<'ctx>>,
    pub(crate) registers: Vec<PointerValue<'ctx>>,
    pub(crate) struct_types: HashMap<String, StructType<'ctx>>,
    pub(crate) variables: HashMap<String, PointerValue<'ctx>>,
}

impl<'ctx> AIR<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        AIR {
            context,
            module,
            builder,
            function: None,
            registers: Vec::new(),
            struct_types: HashMap::new(),
            variables: HashMap::new()
        }
    }

    pub fn create_basic_block(&self, name: &str) -> BasicBlock<'ctx> {
        let function = self.function.unwrap();
        let mut unique_name = name.to_string();
        let mut counter = 0;

        while function
            .get_basic_blocks()
            .iter()
            .any(|block| block.get_name().to_str().unwrap() == unique_name)
        {
            counter += 1;
            unique_name = format!("{}_{}", name, counter);
        }
        self.context.append_basic_block(function, &unique_name)
    }

    pub(crate) fn create_entry_block_alloca(
        &self,
        name: &str,
        ty: BasicTypeEnum<'ctx>,
    ) -> Result<PointerValue<'ctx>> {
        let builder = self.context.create_builder();
        let entry = self.function.unwrap().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder
            .build_alloca(ty, name)
            .map_err(|err| anyhow::Error::new(err))
    }
}
