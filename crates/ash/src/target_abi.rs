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
    /// Whether compiled functions keep the runtime's shadow call stack.
    ///
    /// A native frame has a frame pointer and a return address, so the
    /// runtime walks the machine stack to name the frames of a trace and
    /// `dladdr` or the registered table names each one. WebAssembly's call
    /// stack is not addressable at all: no walker can see it, so every Haxe
    /// function instead pushes a frame at entry, records its source position
    /// as it goes, and pops on return, and the runtime reads that.
    pub shadow_call_stack: bool,
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
            shadow_call_stack: wasm,
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

fn lower_triple(triple: &str) -> String {
    triple.to_ascii_lowercase()
}

/// Turn on the backend's setjmp/longjmp lowering, once per process.
///
/// These are LLVM command-line options rather than target-machine settings,
/// which is why this reaches for the option parser: there is no other way in.
/// Harmless on a native build that never asks for a wasm target machine,
/// because nothing else consults them.
///
/// The second one is the difference between two encodings of the same
/// exceptions proposal. LLVM still defaults to the withdrawn `try`/`catch`
/// instructions, which every current engine refuses -- wasmtime asks for
/// `legacy_exceptions` by name and Chrome dropped them -- while the proposal
/// as standardised is `try_table` and `exnref`. A module built the default
/// way is valid to nobody, so ash always asks for the standard one.
fn enable_wasm_sjlj() {
    use std::sync::Once;
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        let args = [
            c"ash".as_ptr(),
            c"-wasm-enable-sjlj".as_ptr(),
            c"-wasm-use-legacy-eh=false".as_ptr(),
        ];
        let overview = c"ash wasm codegen";
        unsafe {
            inkwell::llvm_sys::support::LLVMParseCommandLineOptions(
                args.len() as i32,
                args.as_ptr(),
                overview.as_ptr(),
            );
        }
    });
}

/// Put the machine into the exception model wasm setjmp lowering needs.
///
/// The option alone gets half the rewrite: see
/// `crates/ash/cpp/wasm_exception_model.cpp`, which explains why the other
/// half is unreachable from the C API and what the object looks like without
/// it -- it links, it runs, and the first throw escapes the program.
#[cfg(not(no_wasm_exception_shim))]
fn force_wasm_exception_model(machine: &TargetMachine) -> Result<()> {
    extern "C" {
        fn ash_force_wasm_exception_model(
            machine: inkwell::llvm_sys::target_machine::LLVMTargetMachineRef,
        );
    }
    // Safety: the pointer is this machine's, and the call only assigns two
    // of its fields.
    unsafe { ash_force_wasm_exception_model(machine.as_mut_ptr()) };
    Ok(())
}

/// Refuse rather than emit an object whose throws escape the program.
#[cfg(no_wasm_exception_shim)]
fn force_wasm_exception_model(_machine: &TargetMachine) -> Result<()> {
    Err(anyhow!(
        "this ash was built without the wasm exception-model shim (no llvm-config \
         was found at build time), and a wasm object built without it cannot catch \
         what it throws"
    ))
}

/// The features a cross target needs to be linkable, as opposed to merely
/// faster.
///
/// An empty feature string means the bare base ISA, and on most targets that
/// costs performance and nothing else. On RISC-V it decides the calling
/// convention: with no F or D the object is emitted soft-float, and a
/// soft-float object cannot be linked against a distribution's libraries at
/// all -- `can't link soft-float modules with double-float modules`, with
/// ELF flags 0x0 against their 0x5.
///
/// So `riscv64-unknown-linux-gnu` is given the profile that name means
/// everywhere it is actually used: `gc`, which is what every Linux
/// distribution builds for and what Rust spells `riscv64gc`. LLVM derives
/// the ABI from the features when none is named, so F and D being present is
/// what makes it `lp64d`.
///
/// `ASH_TARGET_FEATURES` replaces this for a target that wants something
/// else -- a bare embedded RISC-V, say -- because the right answer there is
/// the operator's, not a default's.
fn default_features(triple: &str) -> String {
    if let Ok(explicit) = std::env::var("ASH_TARGET_FEATURES") {
        return explicit;
    }
    if triple.starts_with("riscv64") || triple.starts_with("riscv32") {
        return "+m,+a,+f,+d,+c".to_string();
    }
    String::new()
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
    } else if lower_triple(triple).starts_with("wasm") {
        // ash's trap model is `setjmp`, and on WebAssembly a `setjmp` IS
        // exception handling: the backend rewrites the call into
        // `__wasm_setjmp` and friends, and refuses to do it without the
        // feature enabled ("is using setjmp/longjmp but does not have
        // +exception-handling target feature"). The runtime is built with the
        // same two switches, and both halves have to agree or the program's
        // calls stay unlowered while the runtime's are rewritten.
        enable_wasm_sjlj();
        "+exception-handling".to_string()
    } else {
        default_features(&lower_triple(triple))
    };
    let wasm = lower_triple(triple).starts_with("wasm");
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
    if wasm {
        force_wasm_exception_model(&machine)?;
    }
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
