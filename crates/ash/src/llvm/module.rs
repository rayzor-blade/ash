use crate::bytecode::{BytecodeDecoder, DecodedBytecode};
use crate::hl::*;
use crate::native_lib::{init_std_library, NativeFunctionResolver};
use crate::opcodes::Opcode;
use crate::types::{HLType, HLTypeFun, HLTypeObj, TypeRef, ValueTypeKind};
use anyhow::{anyhow, Result};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::types::{
    AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType,
};
use inkwell::values::{
    AnyValue, AnyValueEnum, ArrayValue, AsValueRef, BasicValue, BasicValueEnum, FunctionValue,
    GenericValue, GlobalValue, IntValue, PointerValue, StructValue,
};
use inkwell::{AddressSpace, OptimizationLevel};
use num_enum::TryFromPrimitive;
use std::cell::RefCell;
use std::collections::btree_map::IntoValues;
use std::collections::HashMap;
use std::ffi::{c_void, CStr};
use std::mem;
use std::ops::Add;
use std::path::Path;
use std::rc::Rc;
use std::slice;

use super::function::{FuncPtr, FunctionBuilder};

use ash_macro::load_symbol;

#[load_symbol]
extern "C" {
    fn hlp_init_virtual(vt: *mut hl_type, _ctx: *mut hl_module_context);
    fn hlp_init_enum(vt: *mut hl_type, _ctx: *mut hl_module_context);
    fn hlp_obj_field_fetch(t: *mut hl_type, fid: i32) -> *mut hl_obj_field;
}

#[derive(Debug, Clone)]
pub struct SharedRuntimeHandles {
    pub globals_data_ptr: *mut *mut c_void,
    pub nglobals: usize,
    pub c_types: Vec<*mut hl_type>,
    pub module_ctx: *mut hl_module_context,
}

// SharedRuntimeHandles carries runtime pointers that are process-global for an HL module
// and are read by the background JIT worker. Synchronization remains the caller's responsibility.
unsafe impl Send for SharedRuntimeHandles {}
unsafe impl Sync for SharedRuntimeHandles {}

#[derive(Debug, Clone)]
pub struct CompiledFunctionMeta {
    pub findex: usize,
    pub fn_addr: usize,
    pub arg_kinds: Vec<hl_type_kind>,
    pub ret_kind: hl_type_kind,
}

impl CompiledFunctionMeta {
    /// An AOT lowering has no address: the function is a symbol in the module
    /// and gets one when the object is linked. `fn_addr == 0` is the marker,
    /// and nothing in the AOT path dispatches through it.
    pub fn aot_placeholder(findex: usize) -> Self {
        CompiledFunctionMeta {
            findex,
            fn_addr: 0,
            arg_kinds: Vec::new(),
            ret_kind: 0,
        }
    }
}

pub struct JITModule<'ctx> {
    pub(crate) context: &'ctx Context,
    /// Alias metadata for emitted loads and stores; see [`super::tbaa`].
    pub(crate) tbaa: super::tbaa::TbaaTree<'ctx>,
    pub(crate) module: Module<'ctx>,
    pub(crate) builder: Builder<'ctx>,
    pub(crate) execution_engine: ExecutionEngine<'ctx>,
    pub(crate) bytecode: DecodedBytecode,
    pub(crate) types_: Vec<HLType>,
    pub(crate) type_cache: HashMap<usize, AnyTypeEnum<'ctx>>,
    pub(crate) initialized_type_cache: HashMap<usize, BasicValueEnum<'ctx>>,
    /// `Class.method` -> findex, built once on first use. A loaded
    /// profile names functions that way rather than by position, so
    /// resolving one back needs this direction.
    pub(crate) name_to_findex: Option<HashMap<String, u32>>,
    pub(crate) findex_to_name: Option<HashMap<u32, String>>,
    /// HDLL primitives this object calls, as (library, primitive).
    /// Each has a slot the startup routine fills by dlopen/dlsym,
    /// because there is no symbol to bind at emit time.
    pub(crate) aot_hdll_natives: Vec<(String, String)>,
    /// Whether this object must take the runtime as a shared library.
    ///
    /// Decided from the bytecode's natives BEFORE anything is lowered,
    /// because it changes how every runtime symbol is declared and that
    /// cannot be revised afterwards: hidden visibility makes codegen address
    /// a symbol directly, and a direct reference to a dylib's data fails the
    /// link with "does not have address" on arm64. Clearing the flag later
    /// does not undo the addressing already chosen.
    pub(crate) aot_shared_runtime: bool,
    pub(crate) type_info_globals: HashMap<usize, GlobalValue<'ctx>>,
    pub(crate) findexes: HashMap<usize, FuncPtr>,
    pub(crate) func_types: Vec<*mut hl_type>,
    pub(crate) func_cache: HashMap<usize, FunctionValue<'ctx>>,
    /// Functions the middle end has already optimized. `run_passes` is a
    /// module operation, so this is what lets a promotion pay for the function
    /// it is promoting instead of for the whole module again; see
    /// `park_optimized_functions`.
    pub(crate) optimized_fns: std::collections::HashSet<FunctionValue<'ctx>>,
    pub(crate) native_function_resolver: NativeFunctionResolver,
    pub(crate) int_globals: Vec<Option<GlobalValue<'ctx>>>,
    pub(crate) float_globals: Vec<Option<GlobalValue<'ctx>>>,
    pub(crate) string_globals: Vec<Option<GlobalValue<'ctx>>>,
    pub(crate) bytes_globals: Vec<Option<GlobalValue<'ctx>>>,
    /// C-side globals: inttoptr constants pointing into globals_data.
    /// Both JIT code and native stdlib access the same memory.
    pub(crate) globals: HashMap<usize, PointerValue<'ctx>>,
    /// Backing memory for globals — shared between JIT and native code.
    pub(crate) globals_data: Vec<*mut c_void>,
    pub(crate) pending_compilations: Vec<usize>,
    pub(crate) c_ptr_to_type_index: HashMap<usize, usize>,
    pub(crate) hl_type_struct_type: Option<StructType<'ctx>>,
    /// AOT only: the object-data counterparts of the pointers a JIT bakes in
    /// as integer constants. Keyed by the compiler-side address, so a lookup
    /// answers "what symbol names this pointer in the emitted object".
    pub(crate) aot_types: HashMap<usize, GlobalValue<'ctx>>,
    pub(crate) aot_strings: HashMap<usize, GlobalValue<'ctx>>,
    pub(crate) aot_globals: Option<GlobalValue<'ctx>>,
    pub(crate) aot_functions: Option<GlobalValue<'ctx>>,
    pub(crate) aot_function_types: Option<GlobalValue<'ctx>>,
    pub(crate) aot_module_ctx: Option<GlobalValue<'ctx>>,
    /// Function pointer table indexed by findex, used by hl_module_context.
    pub(crate) functions_ptrs: Vec<*mut c_void>,
    pub(crate) shared_runtime: Option<SharedRuntimeHandles>,
    /// When true, direct calls to bytecode functions become indirect dispatch
    /// through functions_ptrs, so a reloaded function is picked up by its
    /// callers instead of them holding the old address.
    pub(crate) hot_reload: bool,
    /// Ahead-of-time mode: emit an object file rather than JIT into this
    /// process. Natives become `External` declarations the linker resolves
    /// against `libash_std.a` instead of absolute addresses baked into the
    /// IR, because an address valid in THIS process means nothing in the one
    /// that will run the object. See `docs/wasm-target.md`.
    pub(crate) aot: bool,
    /// Compile reached functions into independent modules and dispatch calls
    /// through `functions_ptrs`. This is the LLVM half of compiled-only JIT:
    /// MCJIT modules cannot accept new function bodies after finalization.
    pub(crate) lazy_compilation: bool,
    /// Findex currently being lowered. Stub calls carry this across the Rust
    /// lazy-compilation bridge so logical Haxe stack traces remain intact.
    pub(crate) current_findex: usize,
}

/// Per-phase init timing: printed inline when ASH_TIERED_TIMING=1, and always
/// charged to the profiler's phase tree (which is itself inert unless
/// ASH_PROFILE is set).
macro_rules! phase_timer {
    ($enabled:expr, $label:expr, $start:expr) => {
        let __elapsed = $start.elapsed();
        crate::profile::record($label, __elapsed);
        if $enabled {
            eprintln!(
                "[jit-init] {:<24} {:>8.1}ms",
                $label,
                __elapsed.as_secs_f64() * 1000.0
            );
        }
    };
}

/// Register LLVM's MCJIT implementation before an execution engine is asked for.
///
/// `create_jit_execution_engine` initializes the native target but does not do
/// this, leaving it to the static initializer inside LLVM's MCJIT library.
/// Nothing in this crate references that object, so a link-time-optimized build
/// garbage-collects it and every engine creation then fails at runtime with
/// "JIT has not been linked in." — a release-only failure that does not
/// reproduce under `cargo build`. Calling it explicitly makes the binary
/// independent of how aggressively the linker prunes.
fn link_in_mcjit() {
    static ONCE: std::sync::Once = std::sync::Once::new();
    ONCE.call_once(inkwell::execution_engine::ExecutionEngine::link_in_mc_jit);
}

/// Run LLVM's middle-end over the module before MCJIT emits code.
///
/// `create_jit_execution_engine(OptimizationLevel::Aggressive)` sets only the
/// *codegen* level — instruction selection, scheduling, machine peepholes. It
/// runs no IR passes at all, and ash never called `run_passes`, so until now
/// nothing had ever promoted an HL register out of its `alloca`: every one
/// stayed a stack slot with loads and stores around it, and the backend's
/// load/store optimizer was the only cleanup. That is the shape the lowering
/// produces on purpose — one alloca per register makes lowering simple — but it
/// depends on mem2reg to be worth anything, and mem2reg was never running.
///
/// `ASH_LLVM_PASSES` overrides the pipeline; `off` skips it, which is the
/// bisect switch when a miscompile is suspected.
pub(crate) fn run_middle_end(module: &inkwell::module::Module<'_>) -> Result<()> {
    run_middle_end_at(module, "default<O2>")
}

/// The same pipeline at a caller-chosen strength.
///
/// The O2 default below is a JIT tradeoff: compilation sits on the critical
/// path there, and O3 costs more than it returns. AOT inherited that tradeoff
/// without inheriting the reason -- its compile is a build step, reported as
/// `build_ms` and excluded from the bar by construction, so strength is free.
/// Measured on the AOT lane, O3 against O2: closure_call 0.13 -> 0.11s, which
/// is where clang-built HashLink/C lands on the same machine; method_call
/// 0.13 -> 0.12; mandelbrot, nbody, binary_trees and fib unchanged. Output is
/// identical on all ten programs checked.
pub(crate) fn run_middle_end_at(
    module: &inkwell::module::Module<'_>,
    default_spec: &str,
) -> Result<()> {
    use inkwell::passes::PassBuilderOptions;
    use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target};
    use inkwell::OptimizationLevel;

    // O2 rather than O3: measured on this corpus O3 costs more compile time
    // than it returns (nbody 1.57s at O2 against 1.70s at O3, and it is slower
    // still on short programs where compilation dominates).
    //
    // Functions that can catch are excluded upstream — see
    // `shield_trap_functions_from_optimization`, which is what makes running
    // this safe at all.
    let spec =
        std::env::var("ASH_LLVM_PASSES").unwrap_or_else(|_| default_spec.to_string());
    if spec == "off" {
        return Ok(());
    }

    Target::initialize_native(&InitializationConfig::default())
        .map_err(|e| anyhow!("target init for middle-end: {e}"))?;
    let triple = inkwell::targets::TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).map_err(|e| anyhow!("target from triple: {}", e))?;
    let machine = target
        .create_target_machine(
            &triple,
            &inkwell::targets::TargetMachine::get_host_cpu_name().to_string_lossy(),
            &inkwell::targets::TargetMachine::get_host_cpu_features().to_string_lossy(),
            OptimizationLevel::Aggressive,
            RelocMode::Default,
            CodeModel::JITDefault,
        )
        .ok_or_else(|| anyhow!("could not create target machine for middle-end"))?;

    module
        .run_passes(&spec, &machine, PassBuilderOptions::create())
        .map_err(|e| anyhow!("run_passes({spec}): {}", e))
}

fn timing_enabled() -> bool {
    static CELL: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *CELL.get_or_init(|| std::env::var("ASH_TIERED_TIMING").is_ok())
}

impl<'ctx> JITModule<'ctx> {
    pub fn new(context: &'ctx Context, path: &Path) -> Self {
        Self::build(context, path, false).expect("Failed to build JIT module")
    }

    /// The same construction, with every pointer the lowering needs expressed
    /// as a symbol rather than as an address in this process.
    ///
    /// AOT has to be decided here rather than switched on afterwards: the
    /// entrypoint is compiled during construction, and a body lowered before
    /// the switch would already have an address baked into it.
    pub fn new_aot(context: &'ctx Context, path: &Path) -> Result<Self> {
        Self::build(context, path, true)
    }

    fn build(context: &'ctx Context, path: &Path, aot: bool) -> Result<Self> {
        let timing = timing_enabled();
        let mut t = std::time::Instant::now();
        crate::native_lib::choose_std_linkage(path);
        init_std_library();

        let bytecode = BytecodeDecoder::decode(path).expect("Failed to decode bytecode");
        // Any non-std native means an HDLL, which brings its own copy of the
        // runtime unless this object shares one. Known here, before a single
        // symbol is declared, because declaring them is what commits to a
        // linkage.
        let aot_shared_runtime = aot
            && bytecode
                .natives
                .iter()
                .any(|n| n.lib.strip_prefix('?').unwrap_or(&n.lib) != "std");
        phase_timer!(timing, "decode", t);
        t = std::time::Instant::now();

        link_in_mcjit();
        let module = context.create_module("Hashlink");
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .expect("Failed to initialize execution engine");
        phase_timer!(timing, "engine", t);
        t = std::time::Instant::now();

        let native_function_resolver = NativeFunctionResolver::new();

        let types_ = bytecode.types.clone();
        phase_timer!(timing, "resolver+types clone", t);
        t = std::time::Instant::now();

        let mut module = JITModule {
            context,
            tbaa: super::tbaa::TbaaTree::new(context),
            module,
            builder: context.create_builder(),
            execution_engine,
            bytecode,
            type_cache: HashMap::new(),
            initialized_type_cache: HashMap::new(),
            name_to_findex: None,
            findex_to_name: None,
            aot_hdll_natives: Vec::new(),
            aot_shared_runtime,
            findexes: HashMap::new(),
            func_cache: HashMap::new(),
            optimized_fns: std::collections::HashSet::new(),
            native_function_resolver,
            types_,
            int_globals: Vec::new(),
            float_globals: Vec::new(),
            string_globals: Vec::new(),
            bytes_globals: Vec::new(),
            type_info_globals: HashMap::new(),
            pending_compilations: Vec::new(),
            globals: HashMap::new(),
            globals_data: Vec::new(),
            c_ptr_to_type_index: HashMap::new(),
            func_types: Vec::new(),
            hl_type_struct_type: None,
            aot_types: HashMap::new(),
            aot_strings: HashMap::new(),
            aot_globals: None,
            aot_functions: None,
            aot_function_types: None,
            aot_module_ctx: None,
            functions_ptrs: Vec::new(),
            shared_runtime: None,
            hot_reload: false,
            aot,
            lazy_compilation: false,
            current_findex: usize::MAX,
        };

        module.create_constant_pool_globals();
        phase_timer!(timing, "const globals", t);
        t = std::time::Instant::now();

        module
            .initialize_globals()
            .expect("Failed to initialize globals");
        phase_timer!(timing, "initialize_globals", t);
        t = std::time::Instant::now();

        // Discover and load external HDLL libraries
        let search_dir = path.parent().unwrap_or(Path::new("."));
        module
            .native_function_resolver
            .discover_and_load_libraries(search_dir, &module.bytecode.natives)
            .expect("Failed to discover HDLL libraries");
        phase_timer!(timing, "discover_libraries", t);
        t = std::time::Instant::now();

        module
            .init_natives()
            .expect("Failed to initialize native functions");
        phase_timer!(timing, "init_natives", t);
        t = std::time::Instant::now();

        // Set up dynamic call callbacks (needed by hlp_call_method for Type.createInstance etc.)
        module.setup_callbacks();

        module.init_indexes().expect("Failed to initialie indexes");
        phase_timer!(timing, "init_indexes", t);
        t = std::time::Instant::now();

        // The type table, the global slots and the function tables have to
        // exist as object data before any body is lowered, because lowering
        // is what asks for pointers into them.
        if aot {
            module.emit_aot_data()?;
            phase_timer!(timing, "emit_aot_data", t);
            t = std::time::Instant::now();
        }

        module
            .compile_entrypoint()
            .expect("Failed to compile entrypoint");
        phase_timer!(timing, "compile entrypoint (AIR v2 -> LLVM)", t);
        t = std::time::Instant::now();

        // A JIT can allocate the constant pool now, because the program runs
        // in this process. An object file cannot: the allocation has to
        // happen wherever the object eventually runs, so AOT emits the same
        // work as startup code instead of performing it.
        if aot {
            module.emit_module_init()?;
            phase_timer!(timing, "emit_module_init", t);
        } else {
            module
                .init_constants()
                .expect("Failed to initialize constants");
            phase_timer!(timing, "init_constants", t);
        }

        module.register_profile_names();
        Ok(module)
    }

    /// Teach the profiler this module's findex → name mapping, so sampled
    /// frames in generated code print as function names.
    ///
    /// A no-op unless profiling is on; the map is only built when it will be
    /// read. `set_name_resolver` keeps the first registration, so a resolver
    /// the embedder installed earlier wins over this one.
    pub(crate) fn register_profile_names(&self) {
        if !crate::profile::enabled() {
            return;
        }
        let mut names: HashMap<u32, String> = HashMap::new();
        for f in &self.bytecode.functions {
            names.insert(f.findex as u32, f.name().to_string());
        }
        for n in &self.bytecode.natives {
            names.insert(n.findex as u32, format!("{}@{}", n.lib, n.name));
        }
        crate::profile::set_name_resolver(move |fx| names.get(&fx).cloned());
    }

    /// Create LLVM module globals for the bytecode constant pools
    /// (strings, ints, floats, bytes). Compiled code references these
    /// directly via Opcode::String/Int/Float/Bytes.
    pub(crate) fn create_constant_pool_globals(&mut self) {
        self.create_constant_pool_globals_selected(None);
    }

    /// Materialize only the constants referenced by one raw bytecode body.
    /// Lazy LLVM modules withhold callee bodies from AIR V2, so optimization
    /// can remove these references but cannot introduce an index from a
    /// different function. Keeping holes in the index-aligned vectors avoids
    /// cloning a game's entire constant pool into every reached function.
    pub(crate) fn create_constant_pool_globals_for(&mut self, findex: usize) {
        let mut ints = std::collections::HashSet::new();
        let mut floats = std::collections::HashSet::new();
        let mut strings = std::collections::HashSet::new();
        let mut bytes = std::collections::HashSet::new();
        if let Some(function) = self
            .bytecode
            .functions
            .iter()
            .find(|function| function.findex as usize == findex)
        {
            for op in &function.ops {
                match op {
                    Opcode::Int { ptr, .. } => {
                        ints.insert(ptr.0);
                    }
                    Opcode::Float { ptr, .. } => {
                        floats.insert(ptr.0);
                    }
                    Opcode::String { ptr, .. } => {
                        strings.insert(ptr.0);
                    }
                    Opcode::Bytes { ptr, .. } => {
                        bytes.insert(ptr.0);
                    }
                    _ => {}
                }
            }
        }
        self.create_constant_pool_globals_selected(Some((&ints, &floats, &strings, &bytes)));
    }

    fn create_constant_pool_globals_selected(
        &mut self,
        selected: Option<(
            &std::collections::HashSet<usize>,
            &std::collections::HashSet<usize>,
            &std::collections::HashSet<usize>,
            &std::collections::HashSet<usize>,
        )>,
    ) {
        self.string_globals = self
            .bytecode
            .strings
            .iter()
            .enumerate()
            .map(|(i, s)| {
                if selected.is_some_and(|(_, _, strings, _)| !strings.contains(&i)) {
                    return None;
                }
                // Convert UTF-8 string to UTF-16 (HashLink uses UTF-16 internally)
                let utf16: Vec<u16> = s.encode_utf16().chain(std::iter::once(0)).collect(); // null-terminated
                let utf16_bytes: Vec<u8> = utf16.iter().flat_map(|c| c.to_le_bytes()).collect();
                let string_val = self.context.const_string(&utf16_bytes, false);
                let global = self.module.add_global(
                    self.context.i8_type().array_type(utf16_bytes.len() as u32),
                    None,
                    &format!("String_{}", i),
                );
                global.set_initializer(&string_val);
                global.set_constant(true);
                // Ensure 2-byte alignment for UTF-16
                global.set_alignment(2);
                Some(global)
            })
            .collect();

        self.int_globals = self
            .bytecode
            .ints
            .iter()
            .enumerate()
            .map(|(i, v)| {
                if selected.is_some_and(|(ints, _, _, _)| !ints.contains(&i)) {
                    return None;
                }
                let int_val = self.context.i32_type().const_int(*v as u64, false);
                let global =
                    self.module
                        .add_global(self.context.i32_type(), None, &format!("Int_{}", i));
                global.set_initializer(&int_val);
                global.set_constant(true);
                Some(global)
            })
            .collect();

        self.float_globals = self
            .bytecode
            .floats
            .iter()
            .enumerate()
            .map(|(i, v)| {
                if selected.is_some_and(|(_, floats, _, _)| !floats.contains(&i)) {
                    return None;
                }
                let float_val = self.context.f64_type().const_float(*v);
                let global =
                    self.module
                        .add_global(self.context.f64_type(), None, &format!("Float_{}", i));
                global.set_initializer(&float_val);
                global.set_constant(true);
                Some(global)
            })
            .collect();

        self.bytes_globals = self
            .bytecode
            .bytes_pos
            .iter()
            .enumerate()
            .map(|(i, &pos)| {
                if selected.is_some_and(|(_, _, _, bytes)| !bytes.contains(&i)) {
                    return None;
                }
                let end = self
                    .bytecode
                    .bytes_pos
                    .get(i + 1)
                    .copied()
                    .unwrap_or(self.bytecode.bytes_data.len());
                let slice = &self.bytecode.bytes_data[pos..end];
                let val = self.context.const_string(slice, false);
                let global = self.module.add_global(
                    self.context.i8_type().array_type(slice.len() as u32),
                    None,
                    &format!("Bytes_{}", i),
                );
                global.set_initializer(&val);
                global.set_constant(true);
                Some(global)
            })
            .collect();
    }

    /// Emit for ahead-of-time compilation. Must be set before any function is
    /// lowered: it changes how natives are referenced, and a module that has
    /// already baked an address cannot be un-baked.
    /// The module's bytecode functions, for a driver that wants to lower all
    /// of them.
    pub fn bytecode_functions(&self) -> &[crate::types::HLFunction] {
        &self.bytecode.functions
    }

    /// The findex the module starts at.
    pub fn entrypoint_findex(&self) -> u32 {
        self.bytecode.entrypoint
    }

    /// The symbol the entrypoint is emitted under. Functions take their Haxe
    /// name when they have one and `Fun_<findex>` only as a fallback, so a
    /// driver cannot assume the latter.
    pub fn entrypoint_symbol(&self) -> String {
        let fx = self.bytecode.entrypoint as i32;
        self.bytecode
            .functions
            .iter()
            .find(|f| f.findex == fx)
            .map(|f| f.name().to_string())
            .unwrap_or_else(|| format!("Fun_{fx}"))
    }

    /// Write this module as an object file for `triple`, the AOT counterpart
    /// of `execution_engine.get_function_address`. The lowering above is
    /// target-independent; only this tail and `declare_native` are not.
    /// Whether this object needs the runtime as a shared library.
    ///
    /// An HDLL imports the runtime by name and brings its own copy if the
    /// executable does not share one -- two GCs in one process, which crashes
    /// as soon as the collector meets an object the other allocated. So a
    /// program that loads an HDLL must link the runtime dynamically and both
    /// halves must bind to the same image.
    pub fn aot_needs_shared_runtime(&self) -> bool {
        self.aot_shared_runtime
    }

    pub fn emit_object(&self, triple: &str, path: &std::path::Path) -> Result<u64> {
        use inkwell::targets::FileType;
        let (tt, machine) = super::aot_shard::object_target_machine(triple)?;
        self.module.set_triple(&tt);
        machine
            .write_to_file(&self.module, FileType::Object, path)
            .map_err(|e| anyhow!("emit {}: {e}", path.display()))?;
        Ok(std::fs::metadata(path)?.len())
    }

    pub fn set_hot_reload(&mut self, enabled: bool) {
        self.hot_reload = enabled;
    }

    pub fn set_lazy_compilation(&mut self, enabled: bool) {
        self.lazy_compilation = enabled;
    }

    /// `bytecode` is the caller's already-decoded module. Decoding is not
    /// cheap -- var-ints, UTF-16 conversion and a `__hlp_hash_gen` for every
    /// field and proto name -- and the interpreter has always held a decoded
    /// copy by the time it pre-warms, so this used to parse the same file a
    /// second time. Cloning it instead is a memcpy.
    pub fn new_with_shared_runtime(
        context: &'ctx Context,
        path: &Path,
        bytecode: &DecodedBytecode,
        shared: SharedRuntimeHandles,
    ) -> Self {
        Self::new_for_tiered(context, path, bytecode, shared)
    }

    /// Minimal constructor for tiered promotion (hybrid pre-warm) and
    /// hot-reload. Builds ONLY what `promote_function_strict` touches:
    ///
    /// - LLVM module + MCJIT execution engine
    /// - constant-pool globals (Opcode::String/Int/Float/Bytes load them)
    /// - HDLL discovery (dlopen is process-global; native symbols then
    ///   resolve lazily per compiled function through
    ///   `get_or_create_function_value` -> `init_native_func`, off the
    ///   startup path)
    /// - `findexes` plus `func_types`/`functions_ptrs` mirrored from the
    ///   interpreter's shared module context
    /// - dynamic-call callbacks (process-global, idempotent)
    ///
    /// Everything else the full `Self::new()` builds is either discarded by
    /// `apply_shared_runtime_overrides` (own globals_data wiring, own C type
    /// graph + initialized_type_cache, own boxed HFUN types) or never reached
    /// by single-function promotion (entrypoint compilation, constants
    /// materialization — the interpreter owns program init in hybrid mode).
    /// Unlike the full constructor, nothing here allocates from the GC, so no
    /// GC lock is needed and it is safe to run mid-program (hot-reload
    /// callback) without stalling collections.
    fn new_for_tiered(
        context: &'ctx Context,
        path: &Path,
        bytecode: &DecodedBytecode,
        shared: SharedRuntimeHandles,
    ) -> Self {
        let timing = timing_enabled();
        let mut t = std::time::Instant::now();
        // Process-global setup is NOT done here. It is `prepare_process_globals`,
        // which the host calls on the main thread before this runs -- see that
        // function for why the split exists.
        let bytecode = bytecode.clone();
        phase_timer!(timing, "tiered clone", t);
        t = std::time::Instant::now();

        link_in_mcjit();
        let llvm_module = context.create_module("Hashlink");
        let execution_engine = llvm_module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .expect("Failed to initialize execution engine");
        phase_timer!(timing, "tiered engine", t);
        t = std::time::Instant::now();

        let native_function_resolver = NativeFunctionResolver::new();
        let types_ = bytecode.types.clone();

        let mut module = JITModule {
            context,
            tbaa: super::tbaa::TbaaTree::new(context),
            module: llvm_module,
            builder: context.create_builder(),
            execution_engine,
            bytecode,
            type_cache: HashMap::new(),
            initialized_type_cache: HashMap::new(),
            name_to_findex: None,
            findex_to_name: None,
            aot_hdll_natives: Vec::new(),
            aot_shared_runtime: false,
            findexes: HashMap::new(),
            func_cache: HashMap::new(),
            optimized_fns: std::collections::HashSet::new(),
            native_function_resolver,
            types_,
            int_globals: Vec::new(),
            float_globals: Vec::new(),
            string_globals: Vec::new(),
            bytes_globals: Vec::new(),
            type_info_globals: HashMap::new(),
            pending_compilations: Vec::new(),
            globals: HashMap::new(),
            globals_data: Vec::new(),
            c_ptr_to_type_index: HashMap::new(),
            func_types: Vec::new(),
            hl_type_struct_type: None,
            aot_types: HashMap::new(),
            aot_strings: HashMap::new(),
            aot_globals: None,
            aot_functions: None,
            aot_function_types: None,
            aot_module_ctx: None,
            functions_ptrs: Vec::new(),
            shared_runtime: Some(shared.clone()),
            hot_reload: false,
            aot: false,
            lazy_compilation: false,
            current_findex: usize::MAX,
        };

        // NOT the whole pool. Promotion is per function, and both paths that
        // need a constant already materialize it: `create_constant_pool_
        // globals_for` seeds the one function's own, and `ensure_*_global`
        // creates anything the optimized body turns out to reference. The
        // eager version built globals for a program's entire constant table
        // on the main thread before any bytecode ran -- cold start a
        // developer pays on every CLI call, including one that promotes
        // nothing.
        //
        // This is not the GC-allocating init the main-thread prewarm exists
        // for (constants as HL objects, obj runtimes, enum marks); it is
        // `add_global` plus `set_initializer`, which touches no GC.
        phase_timer!(timing, "tiered const globals", t);
        t = std::time::Instant::now();

        // The HDLL dlopen and callback registration that used to sit here are
        // in `prepare_process_globals`. Both are process-global, both are
        // already done by the host's own startup, and doing them again from
        // here is what made this constructor unsafe to run anywhere but the
        // main thread.

        // The translate path resolves natives lazily through
        // get_or_create_function_value with ONE exception: Opcode::New for
        // HOBJ/HSTRUCT looks up "std_hlp_alloc_obj_caller" in func_cache by
        // name. Pre-create just that caller so object allocation compiles.
        module
            .init_required_natives()
            .expect("Failed to initialize required natives");
        phase_timer!(timing, "tiered required natives", t);
        t = std::time::Instant::now();

        module
            .init_findexes_only()
            .expect("Failed to initialize findexes");
        module.apply_shared_runtime_overrides(&shared);
        phase_timer!(timing, "tiered findexes+shared", t);

        // Seed the compile queue with the entrypoint. MCJIT code-generates a
        // module exactly once: functions added after the first
        // get_function_address are never compiled, so everything that might
        // later be promoted must be IN the module before that first
        // finalization. The first promotion's strict drain pops this seed and
        // transitively compiles the entrypoint's direct-call closure — the
        // same compiled set the full constructor produced by compiling the
        // entrypoint during init — but on the broker thread, off the
        // main-thread startup path. Later promotions then resolve addresses
        // from the already-finalized object.
        module
            .pending_compilations
            .push(module.bytecode.entrypoint as usize);

        module
    }

    /// Process-global setup the JIT module needs, done once on the main
    /// thread before any module is constructed.
    ///
    /// Split out because everything else in the constructor is per-module
    /// LLVM work that can run anywhere, while these four are global and
    /// ordered: `choose_std_linkage` and `init_std_library` pick and
    /// initialize the runtime, `discover_and_load_libraries` dlopens the
    /// HDLLs so lazily resolved natives find their symbols, and
    /// `setup_callbacks` registers the dynamic-call hooks `hlp_call_method`
    /// needs.
    ///
    /// The host's own startup already performs the first three. Repeating
    /// them inside the constructor made it unsafe to build a module off the
    /// main thread -- running the two concurrently raced the global runtime
    /// init and truncated programs silently, with no crash to point at. With
    /// them here, module construction is pure LLVM and can move off the
    /// startup path.
    pub fn prepare_process_globals(
        path: &Path,
        natives: &[crate::types::HLNative],
        resolver: &mut NativeFunctionResolver,
    ) -> Result<()> {
        crate::native_lib::choose_std_linkage(path);
        init_std_library();
        let search_dir = path.parent().unwrap_or(Path::new("."));
        resolver.discover_and_load_libraries(search_dir, natives)?;
        Self::setup_callbacks_global(resolver);
        Ok(())
    }

    /// Pre-create the native callers the translate path expects to find in
    /// `func_cache` by generated name instead of resolving lazily. Currently
    /// only std/alloc_obj (Opcode::New emits a call to
    /// "std_hlp_alloc_obj_caller" fetched from func_cache).
    pub(crate) fn init_required_natives(&mut self) -> Result<()> {
        let required: Vec<_> = self
            .bytecode
            .natives
            .iter()
            .filter(|n| n.lib == "std" && n.name == "alloc_obj")
            .cloned()
            .collect();
        for native_f in &required {
            let fun_value = self.init_native_func(native_f)?;
            self.func_cache.insert(native_f.findex as usize, fun_value);
        }
        Ok(())
    }

    /// Build findexes and func_types tables without initializing HOBJ/HENUM types.
    /// Copies func_types and functions_ptrs from the shared runtime if available.
    /// Safe to call from the worker thread (no GC allocation).
    fn init_findexes_only(&mut self) -> Result<()> {
        let max_findex = std::cmp::max(
            self.bytecode
                .functions
                .iter()
                .map(|f| f.findex as usize)
                .max()
                .unwrap_or(0),
            self.bytecode
                .natives
                .iter()
                .map(|n| n.findex as usize)
                .max()
                .unwrap_or(0),
        ) + 1;

        // Copy func_types and functions_ptrs from the shared runtime (main thread)
        // instead of building them from scratch (which requires GC allocation).
        if let Some(shared) = &self.shared_runtime {
            if !shared.module_ctx.is_null() {
                let ctx = unsafe { &*shared.module_ctx };
                self.func_types = Vec::with_capacity(max_findex);
                self.functions_ptrs = Vec::with_capacity(max_findex);
                for i in 0..max_findex {
                    self.func_types.push(if !ctx.functions_types.is_null() {
                        unsafe { *ctx.functions_types.add(i) }
                    } else {
                        std::ptr::null_mut()
                    });
                    self.functions_ptrs.push(if !ctx.functions_ptrs.is_null() {
                        unsafe { *ctx.functions_ptrs.add(i) }
                    } else {
                        std::ptr::null_mut()
                    });
                }
            } else {
                self.func_types = vec![std::ptr::null_mut(); max_findex];
                self.functions_ptrs = vec![std::ptr::null_mut(); max_findex];
            }
        } else {
            self.func_types = vec![std::ptr::null_mut(); max_findex];
            self.functions_ptrs = vec![std::ptr::null_mut(); max_findex];
        }

        // Register bytecode functions
        for fun in &self.bytecode.functions {
            self.findexes
                .insert(fun.findex as usize, FuncPtr::Fun(fun.clone()));
        }

        // Register native functions
        for nat in &self.bytecode.natives {
            self.findexes
                .insert(nat.findex as usize, FuncPtr::Native(nat.clone()));
        }

        Ok(())
    }

    fn apply_shared_runtime_overrides(&mut self, shared: &SharedRuntimeHandles) {
        // Rewire global slot pointers to interpreter-owned globals_data.
        self.globals.clear();
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        for index in 0..shared.nglobals {
            let slot_addr = unsafe { shared.globals_data_ptr.add(index) } as u64;
            let addr_int = self.context.i64_type().const_int(slot_addr, false);
            let slot_ptr = addr_int.const_to_pointer(ptr_type);
            self.globals.insert(index, slot_ptr);
        }

        // Rewire type identity cache to interpreter-owned hl_type pointers.
        self.initialized_type_cache.clear();
        self.c_ptr_to_type_index.clear();
        for (i, &c_type_ptr) in shared.c_types.iter().enumerate() {
            if c_type_ptr.is_null() {
                continue;
            }
            self.c_ptr_to_type_index.insert(c_type_ptr as usize, i);
            let ptr_as_int = self.context.i64_type().const_int(c_type_ptr as u64, false);
            let ptr_to_type = ptr_as_int.const_to_pointer(ptr_type);
            self.initialized_type_cache.insert(i, ptr_to_type.into());
        }

        // Mirror shared function pointer/type tables when module context is available.
        if !shared.module_ctx.is_null() {
            unsafe {
                let fptrs = (*shared.module_ctx).functions_ptrs;
                let ftypes = (*shared.module_ctx).functions_types;
                for i in 0..self.functions_ptrs.len() {
                    if !fptrs.is_null() {
                        self.functions_ptrs[i] = *fptrs.add(i);
                    }
                    if i < self.func_types.len() && !ftypes.is_null() {
                        self.func_types[i] = *ftypes.add(i);
                    }
                }
            }
        }
    }

    pub fn initialize_globals(&mut self) -> Result<()> {
        let nglobals = self.bytecode.globals.len();
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        // Allocate C-side memory for all globals (pointer-sized slots, zeroed).
        // Both JIT code and native stdlib will access this same memory.
        self.globals_data = vec![std::ptr::null_mut(); nglobals];

        for (index, _global_type) in self.bytecode.globals.clone().iter().enumerate() {
            // Create an inttoptr constant pointing to the C-side slot
            let slot_addr = unsafe { self.globals_data.as_ptr().add(index) } as u64;
            let addr_int = self.context.i64_type().const_int(slot_addr, false);
            let slot_ptr = addr_int.const_to_pointer(ptr_type);
            self.globals.insert(index, slot_ptr);
        }

        Ok(())
    }

    /// The dynamic-call hook `hlp_call_method` needs (Type.createInstance and
    /// friends), registered once per process.
    ///
    /// Deliberately NOT the closure runner that `setup_callbacks` also
    /// installs: which runner is correct depends on the host. The interpreter
    /// installs its own; only standalone JIT execution wants the typed native
    /// bridge. Registering that from a shared startup path would override the
    /// interpreter's.
    fn setup_callbacks_global(resolver: &NativeFunctionResolver) {
        if let (Ok(setup_fn_ptr), Ok(static_call_ptr)) = (
            resolver.resolve_function("std", "hl_setup_callbacks2"),
            resolver.resolve_function("std", "ash_static_call"),
        ) {
            type FnSetupCallbacks2 = unsafe extern "C" fn(*mut c_void, *mut c_void, i32);
            let setup: FnSetupCallbacks2 = unsafe { std::mem::transmute(setup_fn_ptr) };
            // flags=0: fun arg is the direct function pointer (not double-indirection)
            // wrapper=null: we don't use the wrapper mechanism
            unsafe { setup(static_call_ptr, std::ptr::null_mut(), 0) };
        }
    }

    fn setup_callbacks(&mut self) {
        // Resolve hl_setup_callbacks2 and ash_static_call
        if let Ok(setup_fn_ptr) = self
            .native_function_resolver
            .resolve_function("std", "hl_setup_callbacks2")
        {
            if let Ok(static_call_ptr) = self
                .native_function_resolver
                .resolve_function("std", "ash_static_call")
            {
                type FnSetupCallbacks2 = unsafe extern "C" fn(*mut c_void, *mut c_void, i32);
                let setup: FnSetupCallbacks2 = unsafe { std::mem::transmute(setup_fn_ptr) };
                // flags=0: fun arg is the direct function pointer (not double-indirection)
                // wrapper=null: we don't use the wrapper mechanism
                unsafe {
                    setup(static_call_ptr, std::ptr::null_mut(), 0);
                }
            }
        }

        // Native event-loop and thread code enters Haxe closures through this
        // runner. The interpreter installs its own runner; standalone JIT
        // execution must install the typed native bridge instead.
        if let (Ok(set_runner_ptr), Ok(jit_runner_ptr)) = (
            self.native_function_resolver
                .resolve_function("std", "hlp_set_closure_runner"),
            self.native_function_resolver
                .resolve_function("std", "hlp_jit_closure_runner"),
        ) {
            type FnSetClosureRunner = unsafe extern "C" fn(
                unsafe extern "C" fn(*mut vclosure, *mut *mut vdynamic, i32) -> *mut vdynamic,
            );
            let set_runner: FnSetClosureRunner = unsafe { std::mem::transmute(set_runner_ptr) };
            let runner = unsafe {
                std::mem::transmute::<
                    *mut c_void,
                    unsafe extern "C" fn(*mut vclosure, *mut *mut vdynamic, i32) -> *mut vdynamic,
                >(jit_runner_ptr)
            };
            unsafe { set_runner(runner) };
        }
    }

    fn init_natives(&mut self) -> Result<()> {
        // Tree-shaking: only resolve natives that are actually referenced by
        // bytecode functions. Bytecodes declare natives for all possible functions
        // (process, socket, etc.) but only a subset are actually called.
        let mut needed: std::collections::HashSet<usize> = std::collections::HashSet::new();

        // Scan all bytecode functions for call opcodes referencing native findexes
        let native_findex_set: std::collections::HashSet<usize> = self
            .bytecode
            .natives
            .iter()
            .map(|n| n.findex as usize)
            .collect();

        for func in &self.bytecode.functions {
            for op in &func.ops {
                // Extract referenced findex from call opcodes
                let findex = match op {
                    Opcode::Call0 { fun, .. } => Some(fun.0),
                    Opcode::Call1 { fun, .. } => Some(fun.0),
                    Opcode::Call2 { fun, .. } => Some(fun.0),
                    Opcode::Call3 { fun, .. } => Some(fun.0),
                    Opcode::Call4 { fun, .. } => Some(fun.0),
                    Opcode::CallN { fun, .. } => Some(fun.0),
                    Opcode::StaticClosure { fun, .. } => Some(fun.0),
                    _ => None,
                };
                if let Some(fi) = findex {
                    if native_findex_set.contains(&fi) {
                        needed.insert(fi);
                    }
                }
            }
        }

        let natives = self.bytecode.natives.clone();
        let mut resolved = 0;
        let mut skipped = 0;
        for native_f in &natives {
            let fi = native_f.findex as usize;
            if !needed.contains(&fi) {
                if std::env::var("ASH_JIT_NATIVE_LOG").is_ok() {
                    eprintln!(
                        "[jit-native] SHAKEN findex={} {}@{}",
                        fi, native_f.lib, native_f.name
                    );
                }
                skipped += 1;
                continue; // Tree-shaken: not referenced by any bytecode function
            }
            match self.init_native_func(native_f) {
                Ok(fun_value) => {
                    self.func_cache.insert(fi, fun_value);
                    resolved += 1;
                }
                Err(_) => {
                    // Missing symbol — skip silently
                    skipped += 1;
                }
            }
        }
        eprintln!(
            "[ash] JIT natives: {} resolved, {} tree-shaken/skipped (of {} total)",
            resolved,
            skipped,
            natives.len()
        );
        Ok(())
    }

    fn init_indexes(&mut self) -> Result<()> {
        let natives = self.bytecode.natives.clone();
        let native_len = natives.len();

        let funs = self.bytecode.functions.clone();
        let funs_len = funs.len();

        self.func_types = vec![std::ptr::null_mut(); funs_len + native_len];

        // Pre-allocate functions_ptrs — will be filled with actual addresses before execution
        let max_findex = std::cmp::max(
            funs.iter().map(|f| f.findex as usize).max().unwrap_or(0),
            natives.iter().map(|n| n.findex as usize).max().unwrap_or(0),
        ) + 1;
        self.functions_ptrs = vec![std::ptr::null_mut(); max_findex];

        let cache: Rc<RefCell<HashMap<usize, *mut hl_type>>> =
            Rc::new(RefCell::new(HashMap::new()));

        for i in 0..funs_len {
            let findex = (&funs[i]).findex as usize;
            self.findexes.insert(findex, FuncPtr::Fun(funs[i].clone()));
            // Clone the one small HLTypeFun, not the entire type table. This
            // sat inside the per-function loop, so it was O(functions x types)
            // deep clones; perf put HLType/HLTypeObj/HLTypeFun::clone and the
            // malloc+memmove churn behind them at the top of the profile.
            let tindex = funs[i].type_.clone();
            let type_fun = self.types_[tindex.0]
                .fun
                .clone()
                .expect("Expected function type");
            let type_fun = &type_fun;
            self.func_types[findex] = unsafe {
                Box::into_raw(Box::new(hl_type {
                    kind: hl_type_kind_HFUN,
                    __bindgen_anon_1: hl_type__bindgen_ty_1 {
                        fun: Box::into_raw(Box::new(hl_type_fun {
                            args: self.convert_type_refs_to_c(&type_fun.args, Rc::clone(&cache))?,
                            ret: self.convert_type_ref_to_c_cached(
                                &type_fun.ret.clone(),
                                Rc::clone(&cache),
                            )?,
                            nargs: type_fun.args.len() as i32,
                            parent: if let Some(parent) = &type_fun.parent {
                                self.convert_type_ref_to_c_cached(
                                    &parent.clone(),
                                    Rc::clone(&cache),
                                )?
                            } else {
                                std::ptr::null_mut()
                            },
                            closure_type: hl_type_fun__bindgen_ty_1 {
                                kind: 0,
                                p: std::ptr::null_mut(),
                            },
                            closure: hl_type_fun__bindgen_ty_2 {
                                args: std::ptr::null_mut(),
                                ret: std::ptr::null_mut(),
                                nargs: 0,
                                parent: std::ptr::null_mut(),
                            },
                        })),
                    },
                    vobj_proto: std::ptr::null_mut(),
                    mark_bits: std::ptr::null_mut(),
                }))
            };
        }

        for i in 0..native_len {
            let findex = (&natives[i]).findex as usize;
            self.findexes
                .insert(findex, FuncPtr::Native(natives[i].clone()));
            // Same as the funs loop above: one HLTypeFun, not the table.
            let tindex = natives[i].type_.clone();
            let type_fun = self.types_[tindex.0]
                .fun
                .clone()
                .expect("Expected function type");
            let type_fun = &type_fun;
            self.func_types[findex] = unsafe {
                Box::into_raw(Box::new(hl_type {
                    kind: hl_type_kind_HFUN,
                    __bindgen_anon_1: hl_type__bindgen_ty_1 {
                        fun: Box::into_raw(Box::new(hl_type_fun {
                            args: self.convert_type_refs_to_c(&type_fun.args, Rc::clone(&cache))?,
                            ret: self.convert_type_ref_to_c_cached(
                                &type_fun.ret.clone(),
                                Rc::clone(&cache),
                            )?,
                            nargs: type_fun.args.len() as i32,
                            parent: if let Some(parent) = &type_fun.parent {
                                self.convert_type_ref_to_c_cached(
                                    &parent.clone(),
                                    Rc::clone(&cache),
                                )?
                            } else {
                                std::ptr::null_mut()
                            },
                            closure_type: hl_type_fun__bindgen_ty_1 {
                                kind: 0,
                                p: std::ptr::null_mut(),
                            },
                            closure: hl_type_fun__bindgen_ty_2 {
                                args: std::ptr::null_mut(),
                                ret: std::ptr::null_mut(),
                                nargs: 0,
                                parent: std::ptr::null_mut(),
                            },
                        })),
                    },
                    vobj_proto: std::ptr::null_mut(),
                    mark_bits: std::ptr::null_mut(),
                }))
            };
        }

        for (i, type_) in self.types_.clone().iter().enumerate() {
            match type_.kind {
                hl_type_kind_HOBJ | hl_type_kind_HSTRUCT => {
                    let obj = type_.obj.as_ref().expect("Expected to get object type");
                    let global_value_index = obj.global_value.wrapping_sub(1) as usize;
                    for proto in &obj.proto {
                        let pfindex = proto.findex as usize;
                        if let Some(f) = self.findexes.get_mut(&pfindex) {
                            match f {
                                FuncPtr::Fun(fun) => fun.field_name = Some(proto.name.clone()),
                                _ => {}
                            }
                        }
                    }
                    let len = (obj.bindings.len() / 2) as i32;

                    let native_type =
                        self.convert_type_ref_to_c_cached(&TypeRef(i), Rc::clone(&cache))?;

                    assert!(!native_type.is_null());
                    unsafe {
                        let t = native_type.read();
                        (*t.__bindgen_anon_1.obj).m = Box::into_raw(Box::new(hl_module_context {
                            alloc: unsafe { mem::zeroed() },
                            functions_ptrs: self.functions_ptrs.as_mut_ptr() as *mut *mut c_void,
                            functions_types: self.func_types.as_mut_ptr(),
                        }));
                        let obj = t.__bindgen_anon_1.obj.read();

                        for j in 0..obj.nbindings {
                            let fid = *obj.bindings.add((j << 1) as usize) as usize;
                            let mid = *obj.bindings.add(((j << 1) | 1) as usize) as usize;

                            let __field = unsafe { __hlp_obj_field_fetch(native_type, fid as i32) };

                            if !__field.is_null() {
                                let ff = unsafe { __field.read() };
                                let name = unsafe {
                                    CStr::from_ptr(ff.name as *const i8).to_string_lossy()
                                };

                                match (*ff.t).kind {
                                    hl_type_kind_HFUN | hl_type_kind_HDYN => {
                                        if let Some(f) = self.findexes.get_mut(&mid) {
                                            match f {
                                                FuncPtr::Fun(fun) => {
                                                    fun.field_name = Some(name.to_string())
                                                }
                                                _ => {}
                                            }
                                        }
                                    }
                                    _ => {}
                                }
                            }

                            // Note: binding functions will be populated in functions_ptrs
                            // during setup_functions_ptrs (if compiled).
                        }
                    }

                    let type_struct = self.get_hl_type_struct_type()?;

                    // Create an integer constant from the pointer address
                    let ptr_as_int = self.context.i64_type().const_int(native_type as u64, false);

                    // Cast the integer to a pointer
                    let ptr_to_type =
                        ptr_as_int.const_to_pointer(type_struct.ptr_type(AddressSpace::default()));

                    // println!("{:?}", ptr_to_type.print_to_string().to_string());

                    self.initialized_type_cache.insert(i, ptr_to_type.into());
                }
                hl_type_kind_HENUM => {
                    let enum_type =
                        self.convert_type_ref_to_c_cached(&TypeRef(i), Rc::clone(&cache))?;
                    unsafe {
                        __hlp_init_enum(enum_type, std::ptr::null_mut());
                        self.convert_from_c_type(enum_type)?;
                    }
                    let type_struct = self.get_hl_type_struct_type()?;

                    // Create an integer constant from the pointer address
                    let ptr_as_int = self.context.i64_type().const_int(enum_type as u64, false);

                    // Cast the integer to a pointer
                    let ptr_to_type =
                        ptr_as_int.const_to_pointer(type_struct.ptr_type(AddressSpace::default()));

                    // println!("{:?}", ptr_to_type.print_to_string().to_string());

                    self.initialized_type_cache.insert(i, ptr_to_type.into());
                }
                hl_type_kind_HVIRTUAL => {
                    let mut _hl_type =
                        self.convert_type_ref_to_c_cached(&TypeRef(i), Rc::clone(&cache))?;

                    unsafe {
                        __hlp_init_virtual(_hl_type, std::ptr::null_mut());
                        self.convert_from_c_type(_hl_type)?;
                    }

                    let virt_type = self.get_or_create_any_type(i)?.into_struct_type();
                    // Create an integer constant from the pointer address
                    let ptr_as_int = self.context.i64_type().const_int(_hl_type as u64, false);

                    // Cast the integer to a pointer
                    let ptr_to_type =
                        ptr_as_int.const_to_pointer(virt_type.ptr_type(AddressSpace::default()));

                    // println!("{:?}", ptr_to_type.print_to_string().to_string());

                    self.initialized_type_cache.insert(i, ptr_to_type.into());
                }
                _ => {}
            }
        }

        Ok(())
    }

    /// Compile the entrypoint after runtime type indexes have been prepared.
    ///
    /// This used to live inside `init_indexes`, which made that timing bucket
    /// include AIR V2 lowering and LLVM IR construction for the whole entry
    /// function while appearing to measure only table setup.
    fn compile_entrypoint(&mut self) -> Result<()> {
        let mut main_obj = HLTypeObj::default();
        if let FuncPtr::Fun(entry_function) = self
            .findexes
            .get_mut(&(self.bytecode.entrypoint as usize))
            .filter(|f| matches!(**f, FuncPtr::Fun(_)))
            .expect("Expected to get entrypoint function")
        {
            main_obj.name = "".to_owned();
            entry_function.obj = Some(main_obj);
            entry_function.field_name = Some(String::from("init"));

            let index = self.bytecode.entrypoint as usize;
            let (_, is_pending) = self.get_or_create_function_value(index)?;
            if is_pending {
                self.compile_function(index)?;
            }
        }
        Ok(())
    }

    /// Materialize bytecode constants into globals_data.
    /// Constants are pre-allocated objects (typically String literals) stored in globals.
    /// Each constant specifies a global index and field values to populate.
    /// Must be called AFTER setup_functions_ptrs (needs native function addresses).
    pub(crate) fn init_constants(&mut self) -> Result<()> {
        if self.bytecode.constants.is_empty() {
            return Ok(());
        }

        // Build type_index -> c_type_ptr mapping from c_ptr_to_type_index
        let type_to_c_ptr: HashMap<usize, *mut hl_type> = self
            .c_ptr_to_type_index
            .iter()
            .map(|(&ptr, &idx)| (idx, ptr as *mut hl_type))
            .collect();

        // Resolve native functions we need
        type FnAllocObj = unsafe extern "C" fn(*mut hl_type) -> *mut vdynamic;
        type FnGetObjRt = unsafe extern "C" fn(*mut hl_type) -> *mut hl_runtime_obj;
        type FnGcRegisterRoot = unsafe extern "C" fn(*mut vdynamic);

        let fn_alloc_obj: FnAllocObj = unsafe {
            let ptr = self
                .native_function_resolver
                .resolve_function("std", "hlp_alloc_obj")
                .map_err(|e| anyhow!("Cannot resolve hlp_alloc_obj: {}", e))?;
            std::mem::transmute(ptr)
        };
        let fn_gc_register_root: FnGcRegisterRoot = unsafe {
            let ptr = self
                .native_function_resolver
                .resolve_function("std", "hlp_gc_register_root")
                .map_err(|e| anyhow!("Cannot resolve hlp_gc_register_root: {}", e))?;
            std::mem::transmute(ptr)
        };
        let fn_get_obj_rt: FnGetObjRt = unsafe {
            let ptr = self
                .native_function_resolver
                .resolve_function("std", "hlp_get_obj_rt")
                .map_err(|e| anyhow!("Cannot resolve hlp_get_obj_rt: {}", e))?;
            std::mem::transmute(ptr)
        };

        let bytecode = self.bytecode.clone();

        for constant in &bytecode.constants {
            let global_idx = constant.global as usize;
            if global_idx >= bytecode.globals.len() || global_idx >= self.globals_data.len() {
                continue;
            }

            let type_idx = bytecode.globals[global_idx].0;
            let hl_type_rust = &bytecode.types[type_idx];
            let c_type_ptr = match type_to_c_ptr.get(&type_idx) {
                Some(&ptr) => ptr,
                None => continue,
            };

            if c_type_ptr.is_null() {
                continue;
            }

            let kind = hl_type_rust.kind;

            if kind == hl_type_kind_HOBJ || kind == hl_type_kind_HSTRUCT {
                let obj_data = match hl_type_rust.obj.as_ref() {
                    Some(o) => o,
                    None => continue,
                };

                // Allocate the object
                let obj_ptr = unsafe { fn_alloc_obj(c_type_ptr) };
                if obj_ptr.is_null() {
                    continue;
                }

                // Store in globals_data and register as GC root
                self.globals_data[global_idx] = obj_ptr as *mut c_void;
                unsafe { fn_gc_register_root(obj_ptr) };

                // NOTE: Do NOT update the type's global_value slot here.
                // The global_value pointer for type T points to where T's class
                // descriptor lives (e.g., String's global_value -> globals_data[2]).
                // Writing each constant's obj_ptr there would overwrite the class
                // descriptor slot with the last String literal object.

                // Get runtime object for field offsets
                let rt = unsafe { fn_get_obj_rt(c_type_ptr) };
                if rt.is_null() || constant.fields.is_empty() {
                    continue;
                }

                // Calculate field start offset (skip parent fields)
                let start = unsafe { (*rt).nfields as usize - obj_data.fields.len() };

                // Fill in constant fields
                let type_name = &obj_data.name;
                for (j, &field_value) in constant.fields.iter().enumerate() {
                    if j >= obj_data.fields.len() {
                        break;
                    }

                    let field_type_idx = obj_data.fields[j].type_.0;
                    let field_kind = bytecode.types[field_type_idx].kind;

                    let field_offset = unsafe { *(*rt).fields_indexes.add(j + start) };
                    let field_addr = unsafe { (obj_ptr as *mut u8).add(field_offset as usize) };

                    match field_kind {
                        hl_type_kind_HFUN | hl_type_kind_HMETHOD => {
                            // field_value is a findex — create a closure
                            let findex = field_value as usize;
                            if findex < self.functions_ptrs.len() {
                                let func_ptr = self.functions_ptrs[findex];
                                let field_c_type = match type_to_c_ptr.get(&field_type_idx) {
                                    Some(&ptr) if !ptr.is_null() => ptr,
                                    _ => continue,
                                };
                                type FnAllocClosureVoid =
                                    unsafe extern "C" fn(*mut c_void, *mut c_void) -> *mut c_void;
                                let fn_alloc_cl: FnAllocClosureVoid = unsafe {
                                    std::mem::transmute(
                                        self.native_function_resolver
                                            .resolve_function("std", "hlp_alloc_closure_void")
                                            .map_err(|e| {
                                                anyhow!(
                                                    "Cannot resolve hlp_alloc_closure_void: {}",
                                                    e
                                                )
                                            })?,
                                    )
                                };
                                let closure =
                                    unsafe { fn_alloc_cl(field_c_type as *mut c_void, func_ptr) };
                                if !closure.is_null() {
                                    unsafe {
                                        *(field_addr as *mut *mut c_void) = closure;
                                    }
                                }
                            }
                        }
                        hl_type_kind_HTYPE => {
                            // field_value is a type index — store the C type pointer
                            let ref_type_idx = field_value as usize;
                            if let Some(&type_ptr) = type_to_c_ptr.get(&ref_type_idx) {
                                unsafe {
                                    *(field_addr as *mut usize) = type_ptr as usize;
                                }
                            }
                        }
                        hl_type_kind_HOBJ | hl_type_kind_HSTRUCT => {
                            // field_value is a global index — store the global's pointer
                            let ref_global = field_value as usize;
                            if ref_global < self.globals_data.len() {
                                let ref_val = self.globals_data[ref_global];
                                unsafe {
                                    *(field_addr as *mut usize) = if ref_val.is_null() {
                                        0
                                    } else {
                                        ref_val as usize
                                    };
                                }
                            }
                        }
                        hl_type_kind_HBYTES => {
                            // field_value is a string index → convert to UTF-16
                            let str_idx = field_value as usize;
                            if str_idx < bytecode.strings.len() {
                                let s = &bytecode.strings[str_idx];
                                let mut utf16: Vec<u16> = s.encode_utf16().collect();
                                utf16.push(0); // null terminator
                                let ptr = utf16.as_ptr();
                                std::mem::forget(utf16);
                                unsafe {
                                    *(field_addr as *mut *const u16) = ptr;
                                }
                            }
                        }
                        hl_type_kind_HI32 | hl_type_kind_HBOOL => {
                            // field_value is an index into the ints table
                            let int_val = bytecode
                                .ints
                                .get(field_value as usize)
                                .copied()
                                .unwrap_or(field_value);
                            unsafe {
                                *(field_addr as *mut i32) = int_val;
                            }
                        }
                        _ => {
                            // For other types, try ints table lookup too
                            let int_val = bytecode
                                .ints
                                .get(field_value as usize)
                                .copied()
                                .unwrap_or(field_value);
                            unsafe {
                                *(field_addr as *mut i32) = int_val;
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Wire up class descriptor global slots.
    /// The global_value pointers in C type structs are already wired to
    /// globals_data slots by convert_type_ref_to_c_cached. The bytecode
    /// init code creates fully-populated Class descriptors (with __name__,
    /// __constructor__, etc.) and stores them via SetGlobal. We must NOT
    /// pre-allocate bare descriptors here, as the bytecode init code checks
    /// if the global is already non-null and skips full initialization.
    pub(crate) fn init_class_descriptors(&mut self) -> Result<()> {
        // No-op: global_value pointers are already wired by convert_type_ref_to_c_cached.
        // The bytecode init code handles class descriptor creation and population.
        Ok(())
    }

    // Helper function to ensure we have a valid insertion block
    fn ensure_valid_insert_block(&self, builder: &Builder<'ctx>) -> Result<BasicBlock<'ctx>> {
        if let Some(block) = builder.get_insert_block() {
            Ok(block)
        } else {
            let void_type = self.context.void_type();
            let function_type = void_type.fn_type(&[], false);
            let function = self
                .module
                .add_function("temp_function", function_type, None);
            let basic_block = self.context.append_basic_block(function, "entry");

            Ok(basic_block)
        }
    }

    pub fn get_or_create_any_type(&mut self, type_idx: usize) -> Result<AnyTypeEnum<'ctx>> {
        // The cache lookup below is the whole point of this function, so the
        // table must NOT be cloned in front of it: this used to deep-clone
        // every HLType in the module on every call, including the calls that
        // then hit the cache and never touched the clone. Only the miss path
        // needs a type, and it needs exactly one -- cloned to release the
        // borrow before the `&mut self` conversion call.
        if let Some(type_) = self.type_cache.get(&type_idx) {
            Ok(type_.clone())
        } else {
            let one = self.types_[type_idx].clone();
            let t = self.convert_hl_type_to_llvm_type(&one)?;
            self.type_cache.insert(type_idx, t);
            Ok(t)
        }
    }
    pub(crate) fn convert_hl_type_to_llvm_type(
        &mut self,
        ty: &HLType,
    ) -> Result<AnyTypeEnum<'ctx>> {
        let types = self.bytecode.types.clone();
        let strings = self.bytecode.strings.clone();
        match ty.kind {
            hl_type_kind_HVOID => Ok(self.context.void_type().into()),
            hl_type_kind_HUI8 => Ok(self.context.i8_type().into()),
            hl_type_kind_HUI16 => Ok(self.context.i16_type().into()),
            hl_type_kind_HI32 => Ok(self.context.i32_type().into()),
            hl_type_kind_HI64 => Ok(self.context.i64_type().into()),
            hl_type_kind_HF32 => Ok(self.context.f32_type().into()),
            hl_type_kind_HF64 => Ok(self.context.f64_type().into()),
            hl_type_kind_HBOOL => Ok(self.context.bool_type().into()),
            hl_type_kind_HBYTES => Ok(self
                .context
                .i8_type()
                .ptr_type(inkwell::AddressSpace::default())
                .into()),
            hl_type_kind_HOBJ | hl_type_kind_HSTRUCT => {
                self.create_obj_type(ty.obj.as_ref().expect("expected to get object type"))
            }
            hl_type_kind_HARRAY => {
                let array_type = self.context.opaque_struct_type("varray");
                let ptr_type = self
                    .context
                    .i8_type()
                    .ptr_type(inkwell::AddressSpace::default());

                array_type.set_body(
                    &[
                        ptr_type.into(),                // pointer to type
                        ptr_type.into(),                // pointer to array element type
                        self.context.i32_type().into(), // array size
                        self.context.i32_type().into(), // __pad: #force align on 16 bytes for double
                    ],
                    false,
                );
                Ok(array_type.into())
            }
            hl_type_kind_HTYPE => {
                let hl_type_struct = self.get_hl_type_struct_type()?;
                Ok(hl_type_struct.into())
            }
            hl_type_kind_HREF | hl_type_kind_HPACKED => {
                let tparam_idx = ty.tparam.as_ref().expect("Expected type parameter").0;
                // Recursively ensure the referenced type is converted first
                let referenced_type = self.get_or_create_any_type(tparam_idx)?;
                Ok(match referenced_type {
                    AnyTypeEnum::ArrayType(t) => {
                        t.ptr_type(inkwell::AddressSpace::default()).into()
                    }
                    AnyTypeEnum::FloatType(t) => {
                        t.ptr_type(inkwell::AddressSpace::default()).into()
                    }
                    AnyTypeEnum::FunctionType(t) => {
                        t.ptr_type(inkwell::AddressSpace::default()).into()
                    }
                    AnyTypeEnum::IntType(t) => t.ptr_type(inkwell::AddressSpace::default()).into(),
                    AnyTypeEnum::PointerType(t) => {
                        t.ptr_type(inkwell::AddressSpace::default()).into()
                    }
                    AnyTypeEnum::StructType(t) => {
                        t.ptr_type(inkwell::AddressSpace::default()).into()
                    }
                    AnyTypeEnum::VectorType(t) => {
                        t.ptr_type(inkwell::AddressSpace::default()).into()
                    }
                    AnyTypeEnum::ScalableVectorType(t) => {
                        t.ptr_type(inkwell::AddressSpace::default()).into()
                    }
                    AnyTypeEnum::VoidType(_) => {
                        self.context.ptr_type(AddressSpace::default()).into()
                    }
                })
            }
            hl_type_kind_HVIRTUAL => {
                let v = ty.virt.as_ref().expect("Expected to get virtual type");
                self.create_virtual_type(v)
            }
            hl_type_kind_HDYN => {
                let dyn_type = self.context.opaque_struct_type("vdynamic");
                dyn_type.set_body(
                    &[
                        self.context.ptr_type(AddressSpace::default()).into(), // type
                        self.context.ptr_type(AddressSpace::default()).into(), // union
                    ],
                    false,
                );
                Ok(dyn_type.into())
            }
            hl_type_kind_HDYNOBJ => self.create_dynobj_type(ty),
            hl_type_kind_HFUN | hl_type_kind_HMETHOD => Ok(self
                .create_function_type(ty.fun.as_ref().expect("Expected to get function type"))?
                .into()),
            hl_type_kind_HABSTRACT => self.handle_abstract_type(
                ty.abs_name
                    .as_ref()
                    .expect("Expected to get abstract type name")
                    .clone(),
            ),
            hl_type_kind_HENUM => {
                let tenum = ty.tenum.as_ref().expect("Expected to get enum type");
                self.handle_enum_type(tenum)
            }
            hl_type_kind_HNULL => {
                let null = ty
                    .tparam
                    .as_ref()
                    .expect("Expected to get underlying Null type parameter");
                self.handle_null_type(null)
            }
            // Add more cases for other Type variants as needed
            _ => Err(anyhow!("Unsupported type {:?}", ty)),
        }
    }

    fn create_obj_type(
        &self,
        ty: &crate::types::HLTypeObj,
    ) -> std::result::Result<AnyTypeEnum<'ctx>, anyhow::Error> {
        let obj_type = self.context.opaque_struct_type(&ty.name);

        let i32_type = self.context.i32_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let name = self.context.const_string(ty.name.as_bytes(), false);

        obj_type.set_body(
            &[
                i32_type.into(),                                      // nfields
                i32_type.into(),                                      // nproto
                i32_type.into(),                                      // nbindings
                name.get_type().into(),                               // name
                ptr_type.into(),                                      // _super type
                ptr_type.into(),                                      // fields
                ptr_type.into(),                                      // proto
                i32_type.array_type(ty.bindings.len() as u32).into(), // bindings
                ptr_type.into(),                                      // global_value
                ptr_type.into(),                                      // module_context
                ptr_type.into(),                                      // *mut hl_runtime_obj
            ],
            false,
        );

        Ok(obj_type.as_any_type_enum())
    }

    fn create_dynobj_type(
        &self,
        ty: &HLType,
    ) -> std::result::Result<AnyTypeEnum<'ctx>, anyhow::Error> {
        let dynobj_type = self.context.opaque_struct_type("vdynobj");
        let i32_type = self.context.i32_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        dynobj_type.set_body(
            &[
                ptr_type.into(), // type
                ptr_type.into(), // *mut hl_field_lookup
                ptr_type.into(), // raw_data
                ptr_type.into(), // values
                i32_type.into(), // nfields
                i32_type.into(), // nvalues
                ptr_type.into(), //virtuals
            ],
            false,
        );

        Ok(dynobj_type.as_any_type_enum())
    }

    fn create_virtual_type(
        &self,
        v: &crate::types::HLTypeVirtual,
    ) -> std::result::Result<AnyTypeEnum<'ctx>, anyhow::Error> {
        let virtual_signature: String = format!(
            "haxe.Virtual<{}>",
            v.fields
                .iter()
                .map(|f| self.get_type_name_by_index(f.type_.0))
                .collect::<Vec<String>>()
                .join(",")
        );
        let virt_type = self.context.opaque_struct_type(&virtual_signature);
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i32_type = self.context.i32_type();
        virt_type.set_body(
            &[
                ptr_type.into(),                                    // fields
                i32_type.into(),                                    // nfields
                i32_type.into(),                                    // data_size
                i32_type.array_type(v.indexes.len() as u32).into(), // indexes
                ptr_type.into(),                                    // *mut hl_field_lookup
            ],
            false,
        );
        Ok(virt_type.as_any_type_enum())
    }

    fn handle_abstract_type(
        &self,
        name: String,
    ) -> std::result::Result<AnyTypeEnum<'ctx>, anyhow::Error> {
        let name = format!("haxe.Abstract<{}>", name);
        let abs_type = self.context.opaque_struct_type(&name);
        Ok(abs_type.as_any_type_enum())
    }

    fn handle_null_type(
        &self,
        _null: &TypeRef,
    ) -> std::result::Result<AnyTypeEnum<'ctx>, anyhow::Error> {
        // Null types are always heap-allocated (nullable pointer wrappers)
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        Ok(ptr_type.as_any_type_enum())
    }

    fn handle_enum_type(
        &self,
        tenum: &crate::types::HLTypeEnum,
    ) -> std::result::Result<AnyTypeEnum<'ctx>, anyhow::Error> {
        let enum_type = self.context.opaque_struct_type(&tenum.name);

        let name = self.context.const_string(tenum.name.as_bytes(), false);
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        enum_type.set_body(
            &[
                name.get_type().into(),         // name
                self.context.i32_type().into(), //nconstructs
                ptr_type.into(),                // constructs
                ptr_type.into(),
            ],
            false,
        );

        Ok(enum_type.as_any_type_enum())
    }

    fn get_type_name_by_index(&self, type_index: usize) -> String {
        self.get_type_name(self.bytecode.types.get(type_index).expect("Unknown type"))
    }

    fn get_type_name(&self, ty: &HLType) -> String {
        let type_name = match ty.kind {
            hl_type_kind_HVOID => String::from("haxe.Void"),
            hl_type_kind_HUI8 | hl_type_kind_HUI16 | hl_type_kind_HI32 | hl_type_kind_HI64 => {
                String::from("haxe.Number")
            }
            hl_type_kind_HF32 | hl_type_kind_HF64 => String::from("haxe.Float"),
            hl_type_kind_HBOOL => String::from("haxe.Bool"),
            hl_type_kind_HBYTES => String::from("haxe.Bytes"),
            hl_type_kind_HDYN => String::from("haxe.Dynamic"),
            hl_type_kind_HDYNOBJ => String::from("haxe.DynObject"),
            hl_type_kind_HFUN | hl_type_kind_HMETHOD => {
                let t = ty.fun.as_ref().expect("expected to get function type");
                let args: Vec<String> = t
                    .args
                    .iter()
                    .map(|a| self.get_type_name_by_index(a.0))
                    .collect();
                let args = args.join(",");
                let ret = self.get_type_name_by_index(t.ret.0);
                format!("Func<({}):{}>", args, ret)
            }
            hl_type_kind_HOBJ | hl_type_kind_HSTRUCT => {
                let t = ty.obj.as_ref().expect("expected to get object type");
                format!("haxe.ClassObject<{}>", t.name)
            }
            hl_type_kind_HARRAY => String::from("haxe.Array"),
            hl_type_kind_HTYPE => String::from("haxe.Type"),
            hl_type_kind_HREF => {
                let typ = self
                    .get_type_name_by_index(
                        ty.tparam.as_ref().expect("expect to get type parameter").0,
                    )
                    .clone();
                format!("haxe.Ref<{}>", typ)
            }
            hl_type_kind_HVIRTUAL => String::from("haxe.Virtual"),
            hl_type_kind_HABSTRACT => {
                format!(
                    "haxe.Abstract<{}>",
                    ty.abs_name
                        .as_ref()
                        .expect("expected to get abstract type name")
                )
            }
            hl_type_kind_HENUM => String::from(format!(
                "haxe.Enum<{}>",
                ty.tenum.as_ref().expect("expected to get enum type").name
            )),
            hl_type_kind_HNULL => format!(
                "haxe.Null<{}>",
                self.get_type_name_by_index(
                    ty.tparam.as_ref().expect("expect to get type parameter").0
                )
            ),
            hl_type_kind_HPACKED => format!(
                "haxe.Packed<{}>",
                self.get_type_name_by_index(
                    ty.tparam.as_ref().expect("expect to get type parameter").0
                )
            ),

            _ => {
                unreachable!()
            }
        };

        type_name
    }

    fn create_type_info_global(&mut self, type_index: usize) -> Result<()> {
        let type_ = &self.bytecode.types[type_index];
        let llvmtype = self
            .type_cache
            .get(&type_index)
            .ok_or_else(|| anyhow!("Type not found for index {}", type_index))?;

        let type_info_struct = self.context.opaque_struct_type("TypeInfo");
        type_info_struct.set_body(
            &[
                self.context.i32_type().into(), // Type kind (enum, struct, etc.)
                self.context.i32_type().into(), // Type index
                self.context
                    .i8_type()
                    .ptr_type(inkwell::AddressSpace::default())
                    .into(), // Type name
                self.context
                    .i8_type()
                    .ptr_type(inkwell::AddressSpace::default())
                    .into(), // Pointer to the actual type (as void*)
            ],
            false,
        );

        let type_name = self.get_type_name_by_index(type_index);

        let global_name = format!("type_info_{}", type_index);
        let global = self.module.add_global(type_info_struct, None, &global_name);
        global.set_linkage(inkwell::module::Linkage::External);

        let type_ptr = match llvmtype {
            AnyTypeEnum::ArrayType(t) => t.ptr_type(inkwell::AddressSpace::default()).const_null(),
            AnyTypeEnum::FloatType(t) => t.ptr_type(inkwell::AddressSpace::default()).const_null(),
            AnyTypeEnum::FunctionType(t) => {
                t.ptr_type(inkwell::AddressSpace::default()).const_null()
            }
            AnyTypeEnum::IntType(t) => t.ptr_type(inkwell::AddressSpace::default()).const_null(),
            AnyTypeEnum::PointerType(t) => t.const_null(),
            AnyTypeEnum::StructType(t) => t.ptr_type(inkwell::AddressSpace::default()).const_null(),
            AnyTypeEnum::VectorType(t) => t.ptr_type(inkwell::AddressSpace::default()).const_null(),
            AnyTypeEnum::ScalableVectorType(t) => {
                t.ptr_type(inkwell::AddressSpace::default()).const_null()
            }
            AnyTypeEnum::VoidType(_) => self
                .context
                .i8_type()
                .ptr_type(inkwell::AddressSpace::default())
                .const_null(),
        };

        let init = type_info_struct.const_named_struct(&[
            self.context
                .i32_type()
                .const_int(type_.kind as u64, false)
                .into(),
            self.context
                .i32_type()
                .const_int(type_index as u64, false)
                .into(),
            self.create_type_info_string_constant(&type_name).into(),
            type_ptr.into(),
        ]);

        global.set_initializer(&init);

        self.type_info_globals.insert(type_index, global);

        Ok(())
    }

    fn create_type_info_string_constant(&self, s: &str) -> PointerValue<'ctx> {
        let string_type = self.context.i8_type().array_type(s.len() as u32 + 1);
        let string_global = self.module.add_global(string_type, None, "type_info_name");
        string_global.set_linkage(inkwell::module::Linkage::Internal);
        string_global.set_constant(true);

        let string_const = self.context.const_string(s.as_bytes(), true);
        string_global.set_initializer(&string_const);

        string_global.as_pointer_value()
    }

    /// Materialize one constant global on first use.
    ///
    /// Pre-scanning a function's raw opcodes cannot see constants that AIR V2
    /// introduces by inlining a callee, and a missing global fails the compile
    /// and drops the function silently back to the interpreter. Creating them
    /// where they are actually referenced makes the set exact by construction,
    /// so a module holds only what it uses no matter how the body was
    /// optimized -- a game's whole constant pool no longer rides along with
    /// every promoted function.
    pub fn ensure_int_global(&mut self, index: usize) -> Option<GlobalValue<'ctx>> {
        if let Some(g) = self.int_globals.get(index).copied().flatten() {
            return Some(g);
        }
        let v = *self.bytecode.ints.get(index)?;
        let global = self
            .module
            .add_global(self.context.i32_type(), None, &format!("Int_{index}"));
        global.set_initializer(&self.context.i32_type().const_int(v as u64, false));
        global.set_constant(true);
        if self.int_globals.len() <= index {
            self.int_globals.resize(index + 1, None);
        }
        self.int_globals[index] = Some(global);
        Some(global)
    }

    pub fn ensure_float_global(&mut self, index: usize) -> Option<GlobalValue<'ctx>> {
        if let Some(g) = self.float_globals.get(index).copied().flatten() {
            return Some(g);
        }
        let v = *self.bytecode.floats.get(index)?;
        let global = self
            .module
            .add_global(self.context.f64_type(), None, &format!("Float_{index}"));
        global.set_initializer(&self.context.f64_type().const_float(v));
        global.set_constant(true);
        if self.float_globals.len() <= index {
            self.float_globals.resize(index + 1, None);
        }
        self.float_globals[index] = Some(global);
        Some(global)
    }

    pub fn ensure_string_global(&mut self, index: usize) -> Option<GlobalValue<'ctx>> {
        if let Some(g) = self.string_globals.get(index).copied().flatten() {
            return Some(g);
        }
        let s = self.bytecode.strings.get(index)?.clone();
        let utf16: Vec<u16> = s.encode_utf16().chain(std::iter::once(0)).collect();
        let utf16_bytes: Vec<u8> = utf16.iter().flat_map(|c| c.to_le_bytes()).collect();
        let global = self.module.add_global(
            self.context.i8_type().array_type(utf16_bytes.len() as u32),
            None,
            &format!("String_{index}"),
        );
        global.set_initializer(&self.context.const_string(&utf16_bytes, false));
        global.set_constant(true);
        global.set_alignment(2);
        if self.string_globals.len() <= index {
            self.string_globals.resize(index + 1, None);
        }
        self.string_globals[index] = Some(global);
        Some(global)
    }

    pub fn ensure_bytes_global(&mut self, index: usize) -> Option<GlobalValue<'ctx>> {
        if let Some(g) = self.bytes_globals.get(index).copied().flatten() {
            return Some(g);
        }
        let pos = *self.bytecode.bytes_pos.get(index)?;
        let end = self
            .bytecode
            .bytes_pos
            .get(index + 1)
            .copied()
            .unwrap_or(self.bytecode.bytes_data.len());
        let slice = self.bytecode.bytes_data.get(pos..end)?.to_vec();
        let global = self.module.add_global(
            self.context.i8_type().array_type(slice.len() as u32),
            None,
            &format!("Bytes_{index}"),
        );
        global.set_initializer(&self.context.const_string(&slice, false));
        global.set_constant(true);
        if self.bytes_globals.len() <= index {
            self.bytes_globals.resize(index + 1, None);
        }
        self.bytes_globals[index] = Some(global);
        Some(global)
    }

    pub fn get_int_global(&self, index: usize) -> Option<GlobalValue<'ctx>> {
        self.int_globals.get(index).copied().flatten()
    }

    pub fn get_float_global(&self, index: usize) -> Option<GlobalValue<'ctx>> {
        self.float_globals.get(index).copied().flatten()
    }

    pub fn get_string_global(&self, index: usize) -> Option<GlobalValue<'ctx>> {
        self.string_globals.get(index).copied().flatten()
    }

    pub fn get_bytes_global(&self, index: usize) -> Option<GlobalValue<'ctx>> {
        self.bytes_globals.get(index).copied().flatten()
    }

    pub fn struct_value_to_pointer(
        &self,
        struct_value: StructValue<'ctx>,
    ) -> Result<PointerValue<'ctx>> {
        let struct_type = struct_value.get_type();
        let function = self
            .module
            .get_first_function()
            .expect("No function available");
        let block = self.ensure_valid_insert_block(&self.builder)?;
        // let entry = function
        //     .get_first_basic_block()
        //     .expect("No entry block in function");

        // Create a new builder and position it at the start of the entry block

        self.builder
            .position_before(block.get_first_instruction().as_ref().unwrap());

        // Allocate memory for the struct
        let alloca = self.builder.build_alloca(struct_type, "struct_ptr")?;

        // Store the struct value in the allocated memory
        self.builder.build_store(alloca, struct_value);

        Ok(alloca)
    }
}
