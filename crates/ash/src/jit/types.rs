use std::ops::Add;

use super::module::JITModule;
use crate::hl;
use crate::types::{HLObjField, HLObjProto, HLType, HLTypeObj};
use anyhow::{anyhow, Result};
use inkwell::{
    types::StructType,
    values::{BasicValueEnum, PointerValue, StructValue},
    AddressSpace,
};

impl<'ctx> JITModule<'ctx> {
    pub fn create_hl_type_struct_value(
        &mut self,
        hl_type: &HLType,
        union_value: BasicValueEnum<'ctx>,
    ) -> Result<StructValue<'ctx>> {
        // Create the struct value with null pointers
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let struct_value = self.get_hl_type_struct_type()?.const_named_struct(&[
            self.context
                .i32_type()
                .const_int(hl_type.kind as u64, false)
                .into(),
            union_value.into(),           // union field (initially null)
            ptr_type.const_null().into(), // vobj_proto
            ptr_type.const_null().into(), // mark_bits
        ]);

        Ok(struct_value)
    }

    pub(crate) fn get_hl_type_struct_type(&mut self) -> Result<StructType<'ctx>> {
        if let Some(hl_type_struct) = self.hl_type_struct_type {
            return Ok(hl_type_struct);
        }

        let hl_type_struct = self.context.opaque_struct_type("hl_type");
        let kind_type = self.context.i32_type();
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());

        hl_type_struct.set_body(
            &[
                kind_type.into(),
                ptr_type.into(), // This replaces the union
                ptr_type.into(), // vobj_proto
                ptr_type.into(), // mark_bits
            ],
            false,
        );

        self.hl_type_struct_type = Some(hl_type_struct);

        Ok(hl_type_struct)
    }

    //     pub fn set_hl_type_union_value(
    //         &self,
    //         hl_type_value: StructValue<'ctx>,
    //         union_value: BasicValueEnum<'ctx>
    //     ) -> Result<()> {
    //         // Create a new builder
    //         let builder = self.context.create_builder();

    //         // Get the current insert block
    //         let current_block = self.builder.get_insert_block()
    //             .ok_or_else(|| anyhow!("No current block"))?;

    //         // Position the builder at the end of the current block
    //         builder.position_at_end(current_block);

    //         // Create a pointer to the struct
    //         let ptr = builder.build_alloca(hl_type_value.get_type(), "hl_type_ptr")?;
    //         builder.build_store(ptr, hl_type_value)?;

    //         // Get a pointer to the union field (index 1)
    //         let union_ptr = unsafe {
    //             builder.build_struct_gep(ptr, 1, "union_ptr")?
    //                 .map_err(|_| anyhow!("Failed to create GEP for union field"))?
    //         };

    //         // Store the new union value
    //         builder.build_store(union_ptr, union_value);

    //         // Load the updated struct
    //         let updated_hl_type = builder.build_load(ptr.get_type(), ptr, "updated_hl_type")?;

    //         // Replace all uses of the old struct value with the new one
    //         hl_type_value.replace_all_uses_with(updated_hl_type);

    //         Ok(())
    //     }

    //     // Helper functions (you'll need to implement these)
    //     fn create_hl_type_fun_struct_value(&self, fun: &HLTypeFun) -> Result<StructValue<'ctx>> {
    //         // Implement this based on your HLTypeFun structure
    //         unimplemented!()
    //     }

    //     fn create_hl_type_enum_struct_value(&self, tenum: &HLTypeEnum) -> Result<StructValue<'ctx>> {
    //         // Implement this based on your HLTypeEnum structure
    //         unimplemented!()
    //     }

    //     fn create_hl_type_virtual_struct_value(&self, virt: &HLTypeVirtual) -> Result<StructValue<'ctx>> {
    //         // Implement this based on your HLTypeVirtual structure
    //         unimplemented!()
    //     }
}
