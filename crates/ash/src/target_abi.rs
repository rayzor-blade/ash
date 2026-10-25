//! Target ABI facts shared by bytecode layout and LLVM AOT lowering.
//!
//! These are properties of the program being produced, not of the compiler
//! process.  Keeping them here prevents a cross build from quietly inheriting
//! the host's pointer width through `size_of` or `offset_of`.

use anyhow::{Result, anyhow};
#[cfg(feature = "llvm")]
use inkwell::context::Context;
#[cfg(feature = "llvm")]
use inkwell::module::Module;
#[cfg(feature = "llvm")]
use inkwell::targets::{
    CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
#[cfg(feature = "llvm")]
use inkwell::types::IntType;
#[cfg(feature = "llvm")]
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
    /// `_setjmp` takes the frame to unwind to as a second argument.
    ///
    /// Win64's `setjmp` is `_setjmp(env, _AddressOfReturnAddress())`, and
    /// `longjmp` there reads that frame to decide whether to unwind with SEH.
    /// Called with one argument the field holds whatever was in the register,
    /// and the unwind walks it. Ash abandons the frames between the trap and
    /// the throw deliberately -- it restores the GC lock depth and the shadow
    /// stack itself -- so the frame it wants is the null one, which is Win64's
    /// own spelling for "do not unwind".
    pub setjmp_takes_frame: bool,
    /// Whether a register that can hold a heap pointer must live in memory.
    ///
    /// The collector finds roots by scanning memory it can address. On every
    /// native target that is enough: a value live across a call is either in
    /// a callee-saved register, which the mutator publishes at a safepoint,
    /// or spilled to the machine stack, which is memory. WebAssembly has
    /// neither. A value LLVM leaves in a wasm local lives in the engine's own
    /// frame storage, which is not in linear memory and cannot be scanned by
    /// anything -- so an object whose only reference is there is collected
    /// while it is still in use.
    ///
    /// Keeping those registers in memory is the difference between a program
    /// that is correct and one that is correct until it collects at the wrong
    /// moment. Turning it off puts the failure straight back: the array in
    /// `crates/ash/test/tests/wasm/RootLost.hx` comes back holding another
    /// object's bytes, three runs of three.
    ///
    /// It is not free. On a compute loop that allocates nothing, four calls
    /// run in 1704ms against 764ms unpinned -- and the hot loop's wasm is
    /// IDENTICAL either way, same instructions in the same order, so the cost
    /// is not where it looks. Whatever it is has not been found; the
    /// measurement is recorded rather than explained.
    pub pointer_registers_in_memory: bool,
}

impl TargetAbi {
    #[cfg(feature = "llvm")]
    pub fn host() -> Result<Self> {
        crate::llvm_init::ensure();
        let triple = TargetMachine::get_default_triple()
            .as_str()
            .to_string_lossy()
            .into_owned();
        Self::for_triple(&triple)
    }

    /// The ABI as LLVM describes the triple. `for_triple_str` is the same
    /// answer from the name alone, for a build without LLVM.
    #[cfg(feature = "llvm")]
    pub fn for_triple(triple: &str) -> Result<Self> {
        let (_, machine) = target_machine(triple, OptimizationLevel::Aggressive)?;
        let pointer_bytes = machine.get_target_data().get_pointer_byte_size(None);
        Self::from_pointer_bytes(triple, pointer_bytes)
    }

    /// The ABI from the triple's architecture prefix, with no LLVM.
    pub fn for_triple_str(triple: &str) -> Result<Self> {
        let pointer_bytes = pointer_bytes_for_triple(triple)
            .ok_or_else(|| anyhow!("unknown architecture in target triple {triple}"))?;
        Self::from_pointer_bytes(triple, pointer_bytes)
    }

    pub fn from_pointer_bytes(triple: &str, pointer_bytes: u32) -> Result<Self> {
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
            setjmp_takes_frame: lower.contains("windows") && pointer_bytes == 8,
            pointer_registers_in_memory: wasm,
        })
    }

    /// Whether the runtime on this target collects generationally, so the
    /// write barrier has minors to serve. Unix only, as in `ash_std::gc`.
    pub fn generational_gc(&self) -> bool {
        let t = self.triple.to_ascii_lowercase();
        !t.starts_with("wasm") && !t.contains("windows")
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

    #[cfg(feature = "llvm")]
    pub fn pointer_int_type<'ctx>(&self, context: &'ctx Context) -> IntType<'ctx> {
        context.custom_width_int_type(self.pointer_bytes * 8)
    }

    /// Install the final target before any target-dependent type or body is
    /// emitted into `module`.
    #[cfg(feature = "llvm")]
    pub fn apply_to_module(&self, module: &Module<'_>) -> Result<()> {
        let (triple, machine) = self.target_machine(OptimizationLevel::Aggressive)?;
        module.set_triple(&triple);
        module.set_data_layout(&machine.get_target_data().get_data_layout());
        Ok(())
    }

    #[cfg(feature = "llvm")]
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
        if self.pointer_bytes == 8 { 24 } else { 12 }
    }

    pub fn vclosure_size(&self) -> u64 {
        if self.pointer_bytes == 8 { 32 } else { 16 }
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
        if self.pointer_bytes == 8 { 32 } else { 28 }
    }

    pub fn hl_runtime_obj_fields_indexes_offset(&self) -> u64 {
        self.hl_runtime_obj_methods_offset() + self.pointer_bytes as u64
    }

    pub fn hl_runtime_obj_size(&self) -> u64 {
        if self.pointer_bytes == 8 { 112 } else { 76 }
    }
}

fn lower_triple(triple: &str) -> String {
    triple.to_ascii_lowercase()
}

/// Pointer width by architecture prefix, for the triples ash emits for.
/// `table_agrees_with_llvm` keeps this in step with LLVM's own answer.
const POINTER_BYTES_BY_ARCH: &[(&str, u32)] = &[
    ("aarch64", 8),
    ("arm64", 8),
    ("x86_64", 8),
    ("amd64", 8),
    ("riscv64", 8),
    ("s390x", 8),
    ("powerpc64", 8),
    ("loongarch64", 8),
    ("mips64", 8),
    ("wasm64", 8),
    ("wasm32", 4),
    ("i686", 4),
    ("i586", 4),
    ("i386", 4),
    ("riscv32", 4),
    ("armv7", 4),
    ("thumbv7", 4),
    ("arm", 4),
    ("powerpc", 4),
    ("mips", 4),
];

fn pointer_bytes_for_triple(triple: &str) -> Option<u32> {
    let lower = lower_triple(triple);
    let arch = lower.split('-').next().unwrap_or("");
    // Longest prefix first, so `powerpc64` is not answered by `powerpc`.
    POINTER_BYTES_BY_ARCH
        .iter()
        .filter(|(name, _)| arch.starts_with(name))
        .max_by_key(|(name, _)| name.len())
        .map(|&(_, bytes)| bytes)
}

/// Turn on the backend's setjmp/longjmp lowering and its exception handling,
/// once per process: a trap in compiled code is an exception handler
/// (`llvm::function::wasm_traps`), and without `-wasm-enable-eh` the backend
/// drops every `invoke`'s unwind edge; the runtime's own `setjmp`s still
/// take the setjmp lowering.
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
#[cfg(feature = "llvm")]
fn enable_wasm_sjlj() {
    use std::sync::Once;
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        parse_llvm_options(&[
            c"-wasm-enable-sjlj",
            c"-wasm-enable-eh",
            c"-wasm-use-legacy-eh=false",
        ]);
    });
}

/// Hand LLVM command-line options to its global option registry.
///
/// The registry is process-wide and not thread-safe, and compiles run on
/// several threads, so every caller goes through one lock. Each option may
/// be given once per process: callers guard themselves with a `Once`.
#[cfg(feature = "llvm")]
pub(crate) fn parse_llvm_options(options: &[&std::ffi::CStr]) {
    static LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());
    let _held = LOCK.lock().unwrap_or_else(|e| e.into_inner());
    let args: Vec<*const std::ffi::c_char> = std::iter::once(c"ash".as_ptr())
        .chain(options.iter().map(|o| o.as_ptr()))
        .collect();
    unsafe {
        inkwell::llvm_sys::support::LLVMParseCommandLineOptions(
            args.len() as i32,
            args.as_ptr(),
            c"ash".as_ptr(),
        );
    }
}

/// Put the machine into the exception model wasm setjmp lowering needs.
///
/// The option alone gets half the rewrite: see
/// `crates/ash/cpp/wasm_exception_model.cpp`, which explains why the other
/// half is unreachable from the C API and what the object looks like without
/// it -- it links, it runs, and the first throw escapes the program.
#[cfg(feature = "llvm")]
#[cfg(not(no_wasm_exception_shim))]
fn force_wasm_exception_model(machine: &TargetMachine) -> Result<()> {
    unsafe extern "C" {
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
#[cfg(feature = "llvm")]
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
#[cfg(feature = "llvm")]
fn default_features(triple: &str) -> String {
    if let Ok(explicit) = std::env::var("ASH_TARGET_FEATURES") {
        return explicit;
    }
    if triple.starts_with("riscv64") || triple.starts_with("riscv32") {
        return "+m,+a,+f,+d,+c".to_string();
    }
    String::new()
}

/// Register the backends ash emits for: the hosts it runs on (x86-64,
/// AArch64), wasm32, and the objects-only targets `cross_targets` gates --
/// 32-bit x86 and ARM, RISC-V 32 and 64, PowerPC64LE, s390x.
///
/// Only these, not `Target::initialize_all`: a static archive member is
/// linked only when something references it, and `initialize_all`
/// references every backend LLVM was built with, so the tables and code of
/// targets ash never names -- Hexagon, AMDGPU, NVPTX, MIPS, Sparc and the
/// rest -- were a third of the release binary. A triple outside this set
/// fails in `Target::from_triple`, and the error names the build that has
/// it: the `all-targets` feature registers everything, which is what the
/// `-dev` release tarballs are built with. `table_agrees_with_llvm` pins
/// that every triple ash claims is inside the default set.
#[cfg(feature = "llvm")]
pub(crate) fn initialize_targets() {
    crate::llvm_init::ensure();
    let config = InitializationConfig::default();
    #[cfg(feature = "all-targets")]
    Target::initialize_all(&config);
    #[cfg(not(feature = "all-targets"))]
    {
        Target::initialize_x86(&config);
        Target::initialize_aarch64(&config);
        Target::initialize_arm(&config);
        Target::initialize_riscv(&config);
        Target::initialize_power_pc(&config);
        Target::initialize_system_z(&config);
        Target::initialize_webassembly(&config);
    }
}

/// Whether this build registers every LLVM backend (the `-dev` tarballs) or
/// the supported set alone.
#[cfg(feature = "llvm")]
pub fn all_targets_build() -> bool {
    cfg!(feature = "all-targets")
}

#[cfg(feature = "llvm")]
static GENERIC_CPU: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Restore the previous CPU policy after an AOT request, including errors.
#[cfg(feature = "llvm")]
pub(crate) struct CpuPolicy(bool);

#[cfg(feature = "llvm")]
impl CpuPolicy {
    pub(crate) fn enter(generic: bool) -> Self {
        Self(GENERIC_CPU.swap(generic, std::sync::atomic::Ordering::Relaxed))
    }
}

#[cfg(feature = "llvm")]
impl Drop for CpuPolicy {
    fn drop(&mut self) {
        GENERIC_CPU.store(self.0, std::sync::atomic::Ordering::Relaxed);
    }
}

#[cfg(feature = "llvm")]
fn generic_cpu_requested() -> bool {
    GENERIC_CPU.load(std::sync::atomic::Ordering::Relaxed)
}

/// Accept release asset names and Rust's RISC-V architecture spelling.
pub fn normalize_triple(triple: &str) -> String {
    let triple = match triple {
        "linux-x86_64" => "x86_64-unknown-linux-gnu",
        "linux-aarch64" => "aarch64-unknown-linux-gnu",
        "macos-x86_64" => "x86_64-apple-darwin",
        "macos-aarch64" => "aarch64-apple-darwin",
        "windows-x86_64" => "x86_64-pc-windows-msvc",
        "windows-aarch64" => "aarch64-pc-windows-msvc",
        other => other,
    };
    triple
        .replace("riscv64gc-", "riscv64-")
        .replace("riscv32gc-", "riscv32-")
}

#[cfg(feature = "llvm")]
pub(crate) fn machine_cache_key(triple: &str) -> String {
    format!(
        "{triple}\0{}\0{:?}\0{:?}",
        generic_cpu_requested(),
        std::env::var("ASH_TARGET_FEATURES").ok(),
        std::env::var("ASH_WASM_SIMD").ok()
    )
}

/// Native runtime compatibility ignores vendor and Darwin version spelling.
pub(crate) fn same_native_abi(a: &str, b: &str) -> bool {
    fn key(triple: &str) -> Option<(String, String)> {
        let arch = match triple.split('-').next()? {
            "arm64" => "aarch64",
            "amd64" => "x86_64",
            other => other,
        }
        .to_string();
        let platform = if triple.contains("darwin") || triple.contains("apple-macos") {
            "darwin".to_string()
        } else if triple.contains("linux") || triple.contains("windows") {
            let os = if triple.contains("linux") {
                "linux"
            } else {
                "windows"
            };
            format!("{os}-{}", triple.rsplit('-').next()?)
        } else {
            return None;
        };
        Some((arch, platform))
    }
    key(a).is_some() && key(a) == key(b)
}

#[cfg(feature = "llvm")]
pub(crate) fn is_host_triple(triple: &str) -> bool {
    same_native_abi(
        triple,
        &TargetMachine::get_default_triple()
            .as_str()
            .to_string_lossy(),
    )
}

/// The CPU a build for `triple` targets: the host's when `triple` is this
/// machine's and no target was named, otherwise `generic`.
#[cfg(feature = "llvm")]
pub fn cpu_for(triple: &str) -> String {
    initialize_targets();
    let native = is_host_triple(triple) && !generic_cpu_requested();
    if native {
        TargetMachine::get_host_cpu_name().to_string()
    } else {
        "generic".to_string()
    }
}

#[cfg(feature = "llvm")]
pub(crate) fn target_machine(
    triple: &str,
    opt: OptimizationLevel,
) -> Result<(TargetTriple, TargetMachine)> {
    target_machine_for(triple, opt, generic_cpu_requested())
}

#[cfg(feature = "llvm")]
pub(crate) fn target_machine_for(
    triple: &str,
    opt: OptimizationLevel,
    generic: bool,
) -> Result<(TargetTriple, TargetMachine)> {
    initialize_targets();
    let tt = TargetTriple::create(triple);
    let target = Target::from_triple(&tt).map_err(|e| {
        if all_targets_build() {
            anyhow!("no target for {triple}: {e}")
        } else {
            anyhow!(
                "no target for {triple}: {e}. This build registers x86, AArch64, ARM, RISC-V, \
                 PowerPC, SystemZ and WebAssembly; the -dev release (built with \
                 --features all-targets) registers every backend LLVM has"
            )
        }
    })?;
    let native = is_host_triple(triple) && !generic;
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
        // SIMD128 lets the vector IR the vectorizer and the `simd` primitives
        // emit become v128 instructions instead of being scalarised lane by
        // lane. `ASH_WASM_SIMD=0` leaves it off, for an engine without it.
        if std::env::var("ASH_WASM_SIMD").is_ok_and(|v| v == "0") {
            "+exception-handling".to_string()
        } else {
            "+exception-handling,+simd128".to_string()
        }
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

    /// Naming the host's own triple still builds for the host CPU unless the
    /// caller asked for a generic one.
    #[cfg(feature = "llvm")]
    #[test]
    fn a_named_host_triple_can_be_built_for_a_generic_cpu() {
        use inkwell::OptimizationLevel::Aggressive;
        use inkwell::targets::TargetMachine;
        let host = TargetMachine::get_default_triple()
            .as_str()
            .to_string_lossy()
            .into_owned();
        let (_, native) = super::target_machine_for(&host, Aggressive, false).unwrap();
        let (_, generic) = super::target_machine_for(&host, Aggressive, true).unwrap();
        assert_eq!(
            native.get_cpu().to_string(),
            TargetMachine::get_host_cpu_name().to_string()
        );
        assert_eq!(generic.get_cpu().to_string(), "generic");
        assert_eq!(generic.get_feature_string().to_string_lossy(), "");
    }

    #[test]
    fn distribution_names_and_rust_riscv_names_normalize() {
        assert!(super::same_native_abi(
            "arm64-apple-darwin25.6.0",
            "aarch64-apple-darwin"
        ));
        assert!(!super::same_native_abi(
            "x86_64-unknown-linux-gnu",
            "x86_64-unknown-linux-musl"
        ));
        assert_eq!(
            super::normalize_triple("linux-x86_64"),
            "x86_64-unknown-linux-gnu"
        );
        assert_eq!(
            super::normalize_triple("macos-aarch64"),
            "aarch64-apple-darwin"
        );
        assert_eq!(
            super::normalize_triple("windows-x86_64"),
            "x86_64-pc-windows-msvc"
        );
        assert_eq!(
            super::normalize_triple("riscv64gc-unknown-linux-gnu"),
            "riscv64-unknown-linux-gnu"
        );
    }

    #[cfg(feature = "llvm")]
    #[test]
    fn cpu_policy_is_restored_and_separates_cached_machines() {
        const CHILD: &str = "ASH_TEST_CPU_POLICY";
        if std::env::var_os(CHILD).is_none() {
            let out = std::process::Command::new(std::env::current_exe().unwrap())
                .args([
                    "--exact",
                    "target_abi::tests::cpu_policy_is_restored_and_separates_cached_machines",
                ])
                .env(CHILD, "1")
                .output()
                .unwrap();
            assert!(
                out.status.success(),
                "{}",
                String::from_utf8_lossy(&out.stdout)
            );
            return;
        }
        let host = inkwell::targets::TargetMachine::get_default_triple()
            .as_str()
            .to_string_lossy()
            .into_owned();
        let key = super::machine_cache_key(&host);
        let attempt = || -> anyhow::Result<()> {
            let _policy = super::CpuPolicy::enter(true);
            assert_eq!(super::cpu_for(&host), "generic");
            assert_ne!(key, super::machine_cache_key(&host));
            anyhow::bail!("leave through error")
        };
        assert!(attempt().is_err());
        assert_eq!(key, super::machine_cache_key(&host));
    }

    #[test]
    fn wasm32_hashlink_layouts() {
        let abi = TargetAbi::for_triple_str("wasm32-wasip1").unwrap();
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
        let abi = TargetAbi::for_triple_str("x86_64-unknown-linux-gnu").unwrap();
        assert_eq!(abi.pointer_bytes(), 8);
        assert_eq!(abi.varray_size_offset(), 16);
        assert_eq!(abi.varray_data_offset(), 24);
        assert_eq!(abi.vvirtual_fields_offset(), 24);
        assert_eq!(abi.vclosure_value_offset(), 24);
        assert_eq!(abi.vclosure_size(), 32);
        assert_eq!(abi.hl_runtime_obj_fields_indexes_offset(), 40);
        assert_eq!(abi.hl_runtime_obj_size(), 112);
    }

    #[test]
    fn unknown_architecture_is_refused() {
        assert!(TargetAbi::for_triple_str("z80-unknown-none").is_err());
    }

    /// The hand-written width table must say what LLVM says.
    #[cfg(feature = "llvm")]
    #[test]
    fn table_agrees_with_llvm() {
        for triple in [
            "aarch64-apple-darwin",
            "aarch64-unknown-linux-gnu",
            "x86_64-unknown-linux-gnu",
            "x86_64-pc-windows-msvc",
            "riscv64-unknown-linux-gnu",
            "s390x-unknown-linux-gnu",
            "powerpc64le-unknown-linux-gnu",
            "wasm32-wasip1",
            "wasm32-unknown-unknown",
            "i686-unknown-linux-gnu",
            "armv7-unknown-linux-gnueabihf",
            "riscv32-unknown-elf",
        ] {
            let llvm = TargetAbi::for_triple(triple).unwrap();
            let table = TargetAbi::for_triple_str(triple).unwrap();
            assert_eq!(llvm, table, "{triple}");
        }
    }
}
