//! Source lines for code the LLVM tier compiles.
//!
//! The Cranelift tier answers "which line is this pc on" from srclocs it
//! carries through emission; LLVM carries DWARF locations the same way and
//! writes a line table beside the code. The table is used as a pc map and
//! nothing else: each location's "line" is the id of the frame chain the
//! AIR position marker stands for ([`crate::jit_map::intern_chain`]), and
//! the file is one per module, so no debug-info reader has to resolve
//! inlined scopes back to functions -- the chain already is that answer.
//!
//! MCJIT loads the table when told to (`ash_llvm_track_sections`, in
//! `cpp/jit_sections.cpp`) and reports where; the rows are then attached
//! to the bodies the jit map has registered, as runs of offsets, which is
//! the form every trace reads. JIT only: an AOT object carries its table
//! to the linker, and the AOT tier reads nothing of this.

use std::cell::RefCell;
use std::ffi::c_void;

use inkwell::debug_info::{
    AsDIScope, DICompileUnit, DIFile, DIFlags, DIFlagsConstants, DISubprogram, DWARFEmissionKind,
    DWARFSourceLanguage, DebugInfoBuilder,
};
use inkwell::module::FlagBehavior;
use inkwell::values::FunctionValue;

use super::module::JITModule;
use crate::jit_map::SourceFrame;
use crate::types::HLFunction;
use air::v2::ir::Function as AirFunction;

/// The debug-info state of one LLVM module.
pub(crate) struct ModuleLines<'ctx> {
    builder: DebugInfoBuilder<'ctx>,
    unit: DICompileUnit<'ctx>,
    file: DIFile<'ctx>,
    /// The function being lowered, while one is.
    scope: Option<DISubprogram<'ctx>>,
}

impl<'ctx> JITModule<'ctx> {
    /// Open the body of `f`: give the function a subprogram and put every
    /// instruction from here on at its entry position, until
    /// [`Self::lines_end_function`].
    pub(crate) fn lines_begin_function(&mut self, function: FunctionValue<'ctx>, f: &HLFunction) {
        if !self.trace_lines {
            return;
        }
        if self.lines.is_none() {
            let (builder, unit) = self.module.create_debug_info_builder(
                true,
                DWARFSourceLanguage::C,
                "ash",
                "",
                "ash",
                true,
                "",
                0,
                "",
                DWARFEmissionKind::LineTablesOnly,
                0,
                false,
                false,
                "",
                "",
            );
            // Without the version flag the metadata is dropped as stale
            // before codegen sees it.
            self.module.add_basic_value_flag(
                "Debug Info Version",
                FlagBehavior::Warning,
                self.context
                    .i32_type()
                    .const_int(u64::from(inkwell::debug_info::debug_metadata_version()), false),
            );
            let file = unit.get_file();
            self.lines = Some(ModuleLines {
                builder,
                unit,
                file,
                scope: None,
            });
        }
        let entry = crate::jit_map::intern_chain(vec![SourceFrame {
            findex: f.findex as u32,
            file: f.debug.first().copied().map_or(u32::MAX, |v| v as u32),
            line: f.debug.get(1).copied().map_or(0, |v| v as u32),
        }]);
        let lines = self.lines.as_mut().expect("lines just created");
        let ty = lines
            .builder
            .create_subroutine_type(lines.file, None, &[], DIFlags::ZERO);
        let scope = lines.builder.create_function(
            lines.unit.as_debug_info_scope(),
            &format!("f{}", f.findex),
            None,
            lines.file,
            entry,
            ty,
            true,
            true,
            entry,
            DIFlags::ZERO,
            true,
        );
        function.set_subprogram(scope);
        lines.scope = Some(scope);
        let location =
            lines
                .builder
                .create_debug_location(self.context, entry, 0, scope.as_debug_info_scope(), None);
        self.builder.set_current_debug_location(location);
    }

    /// An AIR position marker: everything emitted from here to the next one
    /// is at this position, in the function it was written in, reached
    /// through the inline sites the marker names.
    pub(crate) fn lines_mark(&mut self, air: &AirFunction, file: u32, line: u32, site: Option<u32>) {
        let Some(lines) = self.lines.as_ref() else {
            return;
        };
        let Some(scope) = lines.scope else {
            return;
        };
        let root = self.current_findex as u32;
        let sites = &air.inline_sites;
        let callee_of = |s: Option<u32>| match s {
            Some(i) => sites
                .get(i as usize)
                .map(|st| st.callee)
                .filter(|c| *c != u32::MAX)
                .unwrap_or(root),
            None => root,
        };
        let mut frames = vec![SourceFrame {
            findex: callee_of(site),
            file,
            line,
        }];
        let mut cur = site;
        while let Some(i) = cur {
            let Some(st) = sites.get(i as usize) else {
                break;
            };
            frames.push(SourceFrame {
                findex: callee_of(st.parent),
                file: st.file,
                line: st.line,
            });
            cur = st.parent;
        }
        let id = crate::jit_map::intern_chain(frames);
        let location =
            lines
                .builder
                .create_debug_location(self.context, id, 0, scope.as_debug_info_scope(), None);
        self.builder.set_current_debug_location(location);
    }

    /// Close the body: nothing emitted after this carries a location, which
    /// matters because the builder is shared and a location names a scope.
    pub(crate) fn lines_end_function(&mut self) {
        if let Some(lines) = self.lines.as_mut() {
            lines.scope = None;
        }
        self.builder.unset_current_debug_location();
    }

    /// Finish the module's debug info. Once per module, before it is
    /// optimized, verified or handed to the engine.
    pub(crate) fn lines_finalize(&mut self) {
        if let Some(lines) = self.lines.take() {
            lines.builder.finalize();
        }
    }
}

/// One section MCJIT loaded, as the listener reported it: its line table
/// bytes when it is one, relocated, copied out during the call.
struct LoadedSection {
    name: String,
    address: usize,
    size: usize,
    line_table: Option<Vec<u8>>,
}

thread_local! {
    /// Objects loaded on this thread and not yet read: the listener runs
    /// inside the load, on the thread that asked for the function address,
    /// and that thread attaches the rows once the bodies are registered.
    static LOADED: RefCell<Vec<Vec<LoadedSection>>> = const { RefCell::new(Vec::new()) };
}

#[repr(C)]
struct AshLoadedSection {
    name: *const std::ffi::c_char,
    address: u64,
    size: u64,
    data: *const u8,
}

#[cfg(not(no_wasm_exception_shim))]
unsafe extern "C" {
    fn ash_llvm_track_sections(
        engine: inkwell::llvm_sys::execution_engine::LLVMExecutionEngineRef,
        callback: unsafe extern "C" fn(*mut c_void, *const AshLoadedSection, usize),
        context: *mut c_void,
    );
}

unsafe extern "C" fn on_object_loaded(_: *mut c_void, sections: *const AshLoadedSection, count: usize) {
    let mut loaded = Vec::with_capacity(count);
    for i in 0..count {
        // SAFETY: the listener hands `count` entries whose names live for
        // the call.
        let section = unsafe { &*sections.add(i) };
        let name = unsafe { std::ffi::CStr::from_ptr(section.name) }
            .to_string_lossy()
            .into_owned();
        // ELF names it `.debug_line`, Mach-O `__debug_line`. A table the
        // loader placed is read where it lies, once the load is finished and
        // its relocations are in; one it did not place comes relocated by
        // the listener and lives only for this call.
        let line_table = (name.ends_with("debug_line")
            && section.address == 0
            && !section.data.is_null())
        .then(|| {
            // SAFETY: `data` holds `size` bytes for the duration of the call.
            unsafe { std::slice::from_raw_parts(section.data, section.size as usize) }.to_vec()
        });
        loaded.push(LoadedSection {
            name,
            address: section.address as usize,
            size: section.size as usize,
            line_table,
        });
    }
    LOADED.with(|l| l.borrow_mut().push(loaded));
}

/// Ask the engine to load every section and report where each one went.
pub(crate) fn track(engine: &inkwell::execution_engine::ExecutionEngine<'_>) {
    #[cfg(not(no_wasm_exception_shim))]
    unsafe {
        ash_llvm_track_sections(engine.as_mut_ptr(), on_object_loaded, std::ptr::null_mut());
    }
    #[cfg(no_wasm_exception_shim)]
    let _ = engine;
}

/// Attach the rows of every object loaded on this thread since the last
/// call to the bodies the jit map holds. Called after those bodies are
/// registered, which is after the load that produced the rows.
pub(crate) fn attach_loaded() {
    let objects = LOADED.with(|l| std::mem::take(&mut *l.borrow_mut()));
    let log = std::env::var_os("ASH_TIER_LOG").is_some();
    for sections in objects {
        let Some(table) = sections.iter().find(|s| s.name.ends_with("debug_line")) else {
            continue;
        };
        let bytes: &[u8] = match (&table.line_table, table.address) {
            (Some(copy), _) => copy,
            // SAFETY: the engine keeps a loaded section mapped for its life,
            // and the engine outlives the process here.
            (None, address) if address != 0 => unsafe {
                std::slice::from_raw_parts(address as *const u8, table.size)
            },
            (None, _) => continue,
        };
        let runs = match line_runs(bytes) {
            Ok(runs) => runs,
            Err(e) => {
                if std::env::var_os("ASH_TIER_LOG").is_some() {
                    eprintln!("[tier] line table unreadable: {e}");
                }
                continue;
            }
        };
        if log {
            eprintln!("[tier] line table: {} runs", runs.len());
        }
        crate::jit_map::attach_runs(&runs);
    }
}

/// The address runs of a `.debug_line` section: `(start, end, chain id)`,
/// one per row, over every program in it.
fn line_runs(bytes: &[u8]) -> Result<Vec<(usize, usize, u32)>, gimli::Error> {
    use gimli::{DebugLine, DebugLineOffset, RunTimeEndian};
    let endian = if cfg!(target_endian = "little") {
        RunTimeEndian::Little
    } else {
        RunTimeEndian::Big
    };
    let debug_line = DebugLine::new(bytes, endian);
    let mut runs = Vec::new();
    let mut offset = 0usize;
    while offset + 4 <= bytes.len() {
        let program = debug_line.program(
            DebugLineOffset(offset),
            std::mem::size_of::<usize>() as u8,
            None,
            None,
        )?;
        let header = program.header().clone();
        let mut rows = program.rows();
        let mut open: Option<(usize, u32)> = None;
        while let Some((_, row)) = rows.next_row()? {
            let address = row.address() as usize;
            if let Some((start, chain)) = open.take()
                && address > start
                && chain != 0
            {
                runs.push((start, address, chain));
            }
            if !row.end_sequence() {
                open = Some((address, row.line().map_or(0, |l| l.get() as u32)));
            }
        }
        let initial = match header.format() {
            gimli::Format::Dwarf32 => 4,
            gimli::Format::Dwarf64 => 12,
        };
        offset += initial + header.unit_length();
    }
    Ok(runs)
}
