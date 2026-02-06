use crate::prelude::*;
use anyhow::Result;

impl<'ctx> AIR<'ctx> {
    pub fn build_int_add(&self, lhs: BasicMetadataValueEnum<'ctx>, rhs: BasicMetadataValueEnum<'ctx>, name: &str) -> Result<BasicValueEnum<'ctx>> {
        Ok(self.builder.build_int_add(lhs.into_int_value(), rhs.into_int_value(), name)?.into())
    }

    pub fn build_int_sub(&self, lhs: BasicMetadataValueEnum<'ctx>, rhs: BasicMetadataValueEnum<'ctx>, name: &str) -> Result<BasicValueEnum<'ctx>> {
        Ok(self.builder.build_int_sub(lhs.into_int_value(), rhs.into_int_value(), name)?.into())
    }

    pub fn build_int_mul(&self, lhs: BasicMetadataValueEnum<'ctx>, rhs: BasicMetadataValueEnum<'ctx>, name: &str) -> Result<BasicValueEnum<'ctx>> {
        Ok(self.builder.build_int_mul(lhs.into_int_value(), rhs.into_int_value(), name)?.into())
    }

    pub fn build_int_div(&self, lhs: BasicMetadataValueEnum<'ctx>, rhs: BasicMetadataValueEnum<'ctx>, name: &str) -> Result<BasicValueEnum<'ctx>> {
        Ok(self.builder.build_int_signed_div(lhs.into_int_value(), rhs.into_int_value(), name)?.into())
    }

    pub fn build_float_add(&self, lhs: BasicMetadataValueEnum<'ctx>, rhs: BasicMetadataValueEnum<'ctx>, name: &str) -> Result<BasicValueEnum<'ctx>> {
       Ok(self.builder.build_float_add(lhs.into_float_value(), rhs.into_float_value(), name)?.into())
    }

    pub fn build_float_sub(&self, lhs: BasicMetadataValueEnum<'ctx>, rhs: BasicMetadataValueEnum<'ctx>, name: &str) -> Result<BasicValueEnum<'ctx>> {
        Ok(self.builder.build_float_sub(lhs.into_float_value(), rhs.into_float_value(), name)?.into())
    }

    pub fn build_float_mul(&self, lhs: BasicMetadataValueEnum<'ctx>, rhs: BasicMetadataValueEnum<'ctx>, name: &str) -> Result<BasicValueEnum<'ctx>> {
        Ok(self.builder.build_float_mul(lhs.into_float_value(), rhs.into_float_value(), name)?.into())
    }

    pub fn build_float_div(&self, lhs: BasicMetadataValueEnum<'ctx>, rhs: BasicMetadataValueEnum<'ctx>, name: &str) -> Result<BasicValueEnum<'ctx>> {
        Ok(self.builder.build_float_div(lhs.into_float_value(), rhs.into_float_value(), name)?.into())
    }

    pub fn build_int_compare(&self, op: inkwell::IntPredicate, lhs: BasicMetadataValueEnum<'ctx>, rhs: BasicMetadataValueEnum<'ctx>, name: &str) -> Result<BasicValueEnum<'ctx>> {
        Ok(self.builder.build_int_compare(op, lhs.into_int_value(), rhs.into_int_value(), name)?.into())
    }

    pub fn build_float_compare(&self, op: inkwell::FloatPredicate, lhs: BasicMetadataValueEnum<'ctx>, rhs: BasicMetadataValueEnum<'ctx>, name: &str) -> Result<BasicValueEnum<'ctx>> {
        Ok(self.builder.build_float_compare(op, lhs.into_float_value(), rhs.into_float_value(), name)?.into())
    }


    pub fn build_bitcast(&self, value: BasicValueEnum<'ctx>, dest_type: BasicTypeEnum<'ctx>, name: &str) -> Result<BasicValueEnum<'ctx>> {
        Ok(self.builder.build_bit_cast(value, dest_type, name).map_err(|err| anyhow::Error::new(err))?.into())
    }
}