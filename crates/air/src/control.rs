use crate::prelude::*;
use anyhow::Result;

impl<'ctx> AIR<'ctx> {
    pub fn build_conditional_branch(&self, condition: BasicValueEnum<'ctx>, then_block: inkwell::basic_block::BasicBlock<'ctx>, else_block: inkwell::basic_block::BasicBlock<'ctx>) {
        self.builder.build_conditional_branch(condition.into_int_value(), then_block, else_block);
    }

    pub fn build_unconditional_branch(&self, block: inkwell::basic_block::BasicBlock<'ctx>) -> Result<InstructionValue<'ctx>> {
        self.builder.build_unconditional_branch(block).map_err(|err| anyhow::Error::new(err))
    }

    pub fn build_phi(&self, incoming_values: &[(&dyn BasicValue<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)], name: &str) -> Result<BasicValueEnum<'ctx>> {
        let phi_type = incoming_values[0].0.as_basic_value_enum().get_type();
        let phi = self.builder.build_phi(phi_type, name)?;
        phi.add_incoming(incoming_values);
        Ok(phi.as_basic_value())
    }
}