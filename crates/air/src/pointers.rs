use anyhow::Result;

use crate::prelude::*;

impl<'ctx> AIR<'ctx> {
    pub fn build_global_string_ptr(&self, string: &str, name: &str) -> Result<GlobalValue<'ctx>> {
        self.builder.build_global_string_ptr(string, name).map_err(|err| anyhow::Error::new(err))
    }

    pub fn build_int_to_ptr(&self, int_value: BasicMetadataValueEnum<'ctx>, dest_type: inkwell::types::PointerType<'ctx>, name: &str) ->  Result<BasicValueEnum<'ctx>> {
        Ok(self.builder.build_int_to_ptr(int_value.into_int_value(), dest_type, name)?.into())
    }

    pub fn build_ptr_to_int(&self, ptr_value: BasicMetadataValueEnum<'ctx>, dest_type: IntType<'ctx>, name: &str) -> Result<BasicValueEnum<'ctx>> {
        Ok(self.builder.build_ptr_to_int(ptr_value.into_pointer_value(), dest_type, name)?.into())
    }
}