//! Target ABI facts shared by bytecode layout and LLVM AOT lowering.
//!
//! These are properties of the program being produced, not of the compiler
//! process.  Keeping them here prevents a cross build from quietly inheriting
//! the host's pointer width through `size_of` or `offset_of`.

use anyhow::{anyhow, Result};
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use inkwell::types::IntType;
use inkwell::{AddressSpace, OptimizationLevel};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TargetAbi {
    triple: String,
    pointer_bytes: u32,
    pub wasi: bool,
    pub threads: bool,
    pub sjlj_eh: bool,
    pub native_dynamic_loading: bool,
    /// Whether a reference to an undefined DATA symbol can be relocated.
    ///
    /// Every native object format can: the linker resolves it, or the dynamic
    /// loader does. WebAssembly cannot -- its memory-address relocations
    /// require a defined symbol, and `--allow-undefined` imports functions
    /// only -- so generated code must reach such a word through a pointer the
    /// module's own initializer fills.
    pub direct_data_relocations: bool,
}

impl TargetAbi {
    pub fn host() -> Result<Self> {
        let triple = TargetMachine::get_default_triple()
            .as_str()
            .to_string_lossy()
            .into_owned();
        Self::for_triple(&triple)
    }

    pub fn for_triple(triple: &str) -> Result<Self> {
        let (_, machine) = target_machine(triple, OptimizationLevel::Aggressive)?;
        let pointer_bytes = machine.get_target_data().get_pointer_byte_size(None);
        if pointer_bytes != 4 && pointer_bytes != 8 {
            return Err(anyhow!(
                "unsupported target pointer width: {pointer_bytes} bytes for {triple}"
            ));
        }
        let lower = triple.to_ascii_lowercase();
        let wasm = lower.starts_with("wasm32-") || lower.starts_with("wasm64-");
        let wasi = wasm && (lower.contains("wasi") || lower.contains("wasip"));
        Ok(Self {
            triple: triple.to_owned(),
            pointer_bytes,
            wasi,
            // The first wasm artifact is deliberately single-threaded.
            threads: !wasm,
            sjlj_eh: !wasm,
            native_dynamic_loading: !wasm,
            direct_data_relocations: !wasm,
        })
    }

    pub fn triple(&self) -> &str {
        &self.triple
    }

    pub fn pointer_bytes(&self) -> u32 {
        self.pointer_bytes
    }

    pub fn pointer_align(&self) -> u32 {
        self.pointer_bytes
    }

    pub fn pointer_int_type<'ctx>(&self, context: &'ctx Context) -> IntType<'ctx> {
        context.custom_width_int_type(self.pointer_bytes * 8)
    }

    /// Install the final target before any target-dependent type or body is
    /// emitted into `module`.
    pub fn apply_to_module(&self, module: &Module<'_>) -> Result<()> {
        let (triple, machine) = self.target_machine(OptimizationLevel::Aggressive)?;
        module.set_triple(&triple);
        module.set_data_layout(&machine.get_target_data().get_data_layout());
        Ok(())
    }

    pub fn target_machine(&self, opt: OptimizationLevel) -> Result<(TargetTriple, TargetMachine)> {
        target_machine(&self.triple, opt)
    }

    pub fn varray_size_offset(&self) -> u64 {
        (self.pointer_bytes * 2) as u64
    }

    pub fn varray_data_offset(&self) -> u64 {
        (self.pointer_bytes * 2 + 8) as u64
    }

    pub fn vvirtual_value_offset(&self) -> u64 {
        self.pointer_bytes as u64
    }

    pub fn vvirtual_fields_offset(&self) -> u64 {
        (self.pointer_bytes * 3) as u64
    }

    pub fn vclosure_fun_offset(&self) -> u64 {
        self.pointer_bytes as u64
    }

    pub fn vclosure_has_value_offset(&self) -> u64 {
        (self.pointer_bytes * 2) as u64
    }

    pub fn vclosure_value_offset(&self) -> u64 {
        if self.pointer_bytes == 8 {
            24
        } else {
            12
        }
    }

    pub fn vclosure_size(&self) -> u64 {
        if self.pointer_bytes == 8 {
            32
        } else {
            16
        }
    }

    pub fn vclosure_wrapper_fun_offset(&self) -> u64 {
        self.vclosure_size()
    }

    pub fn venum_index_offset(&self) -> u64 {
        self.pointer_bytes as u64
    }

    pub fn venum_payload_offset(&self) -> u64 {
        self.pointer_bytes as u64 + 4
    }

    pub fn hl_type_vobj_proto_offset(&self) -> u64 {
        (self.pointer_bytes * 2) as u64
    }

    pub fn hl_runtime_obj_methods_offset(&self) -> u64 {
        if self.pointer_bytes == 8 {
            32
        } else {
            28
        }
    }

    pub fn hl_runtime_obj_fields_indexes_offset(&self) -> u64 {
        self.hl_runtime_obj_methods_offset() + self.pointer_bytes as u64
    }

    pub fn hl_runtime_obj_size(&self) -> u64 {
        if self.pointer_bytes == 8 {
            112
        } else {
            76
        }
    }
}

pub(crate) fn target_machine(
    triple: &str,
    opt: OptimizationLevel,
) -> Result<(TargetTriple, TargetMachine)> {
    Target::initialize_all(&InitializationConfig::default());
    let tt = TargetTriple::create(triple);
    let target = Target::from_triple(&tt).map_err(|e| anyhow!("no target for {triple}: {e}"))?;
    let host = TargetMachine::get_default_triple();
    let native = tt == host;
    let cpu = if native {
        TargetMachine::get_host_cpu_name().to_string()
    } else {
        "generic".to_string()
    };
    let features = if native {
        TargetMachine::get_host_cpu_features().to_string()
    } else {
        String::new()
    };
    let machine = target
        .create_target_machine(
            &tt,
            &cpu,
            &features,
            opt,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .ok_or_else(|| anyhow!("could not create a TargetMachine for {triple}"))?;
    Ok((tt, machine))
}

#[cfg(test)]
mod tests {
    use super::TargetAbi;

    #[test]
    fn wasm32_hashlink_layouts() {
        let abi = TargetAbi::for_triple("wasm32-wasip1").unwrap();
        assert_eq!(abi.pointer_bytes(), 4);
        assert_eq!(abi.varray_size_offset(), 8);
        assert_eq!(abi.varray_data_offset(), 16);
        assert_eq!(abi.vvirtual_fields_offset(), 12);
        assert_eq!(abi.vclosure_fun_offset(), 4);
        assert_eq!(abi.vclosure_has_value_offset(), 8);
        assert_eq!(abi.vclosure_value_offset(), 12);
        assert_eq!(abi.vclosure_size(), 16);
        assert_eq!(abi.venum_index_offset(), 4);
        assert_eq!(abi.venum_payload_offset(), 8);
        assert_eq!(abi.hl_type_vobj_proto_offset(), 8);
        assert_eq!(abi.hl_runtime_obj_methods_offset(), 28);
        assert_eq!(abi.hl_runtime_obj_fields_indexes_offset(), 32);
        assert_eq!(abi.hl_runtime_obj_size(), 76);
        assert!(abi.wasi);
        assert!(!abi.threads);
        assert!(!abi.native_dynamic_loading);
        assert!(!abi.direct_data_relocations);
    }

    #[test]
    fn native_64_hashlink_layouts() {
        let abi = TargetAbi::for_triple("x86_64-unknown-linux-gnu").unwrap();
        assert_eq!(abi.pointer_bytes(), 8);
        assert_eq!(abi.varray_size_offset(), 16);
        assert_eq!(abi.varray_data_offset(), 24);
        assert_eq!(abi.vvirtual_fields_offset(), 24);
        assert_eq!(abi.vclosure_value_offset(), 24);
        assert_eq!(abi.vclosure_size(), 32);
        assert_eq!(abi.hl_runtime_obj_fields_indexes_offset(), 40);
        assert_eq!(abi.hl_runtime_obj_size(), 112);
    }
}
