//! Capturing and symbolicating the call stack.
//!
//! Exception traces have to name frames from three worlds at once: the
//! interpreter's own frame stack, functions the tiers compiled, and native
//! images reached through an HDLL. A child module of `interpreter` so it
//! reaches `HLInterpreter`'s private fields without widening them.

use anyhow::Result;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::c_void;

use ash_core::bytecode::DecodedBytecode;
use ash_core::hl_bindings as hl;
use ash_core::types::HLFunction;

use crate::values::NanBoxedValue;

use super::{func_of, HLInterpreter};

impl HLInterpreter {
    /// The interpreted call stack as HashLink reports it: innermost first,
    /// `Class.method(file:line)` per frame, using the debug info the bytecode
    /// already carries.
    pub(super) fn capture_call_stack(&self, bytecode: &DecodedBytecode) -> Vec<String> {
        let bc = self.reloaded_bytecode.unwrap_or(bytecode);
        let names = self.function_name_table(bc);
        self.stack
            .iter()
            .rev()
            .map(|frame| {
                // Never drop a frame: a trace that silently omits the frames
                // it could not name is worse than one that admits them, since
                // the gap is invisible and the caller looks like the callee.
                let Some(func) = bc.functions.get(frame.function_index) else {
                    return format!("<unresolved findex {}>", frame.function_index);
                };
                let name = names
                    .get(&(func.findex as usize))
                    .cloned()
                    .unwrap_or_else(|| func.name());
                let debug_pc = frame.pc.min(func.ops.len().saturating_sub(1));
                let file_idx = func.debug.get(debug_pc * 2).copied().unwrap_or(-1);
                let line = func.debug.get(debug_pc * 2 + 1).copied().unwrap_or(0);
                match usize::try_from(file_idx)
                    .ok()
                    .and_then(|i| bc.debug_files.get(i))
                {
                    Some(file) => format!("{name}({file}:{line})"),
                    // No debug info (a release build): the name alone still
                    // says which function, which beats printing nothing.
                    None => name,
                }
            })
            .collect()
    }

    pub(super) fn stack_symbol_for_function(
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        pc: usize,
    ) -> Box<[u16]> {
        let debug_pc = pc.min(func.ops.len().saturating_sub(1));
        let file_idx = func.debug.get(debug_pc * 2).copied().unwrap_or(-1);
        let line = func.debug.get(debug_pc * 2 + 1).copied().unwrap_or(0);
        let file = usize::try_from(file_idx)
            .ok()
            .and_then(|idx| bytecode.debug_files.get(idx))
            .map(String::as_str)
            .unwrap_or("unknown");
        let mut symbol: Vec<u16> = format!("fun${}({file}:{line})", func.findex)
            .encode_utf16()
            .collect();
        symbol.push(0);
        symbol.into_boxed_slice()
    }

    pub(super) fn stack_symbol(
        bytecode: &DecodedBytecode,
        function_index: usize,
        pc: usize,
    ) -> Option<Box<[u16]>> {
        let func = bytecode.functions.get(function_index)?;
        Some(Self::stack_symbol_for_function(bytecode, func, pc))
    }

    pub(super) fn interpreter_stack_symbol(
        &self,
        bytecode: &DecodedBytecode,
        function_index: usize,
        pc: usize,
    ) -> Option<Box<[u16]>> {
        bytecode.functions.get(function_index)?;
        // AIR V2's serializer renumbers opcodes. Cache::prepare builds a
        // matching debug table for that optimized body, so frame.pc must be
        // resolved against the body the interpreter actually executes rather
        // than the original bytecode function at the same numeric index.
        let func = self.air.body(bytecode, function_index);
        Some(Self::stack_symbol_for_function(bytecode, func, pc))
    }

    /// Return true when the loader owns `pc` as part of the executable or a
    /// shared library. JIT code lives in anonymous executable mappings, so a
    /// loader-owned address must never be fed to the nearest-JIT-entry
    /// fallback: doing so made ASLR occasionally report an unrelated Haxe
    /// function for one of Ash's own native stack frames.
    #[cfg(unix)]
    pub(super) fn native_image_owns_pc(pc: usize) -> bool {
        if pc == 0 {
            return false;
        }
        // Answered per PAGE, because `dladdr` is not cheap: it walks the
        // loaded images to find the closest symbol, and this runs for every
        // frame of every captured stack. On MBHaxe, which throws while
        // loading a level, dyld's findClosestSymbol was the busiest non-idle
        // leaf in the whole process. Ownership is a property of the mapping,
        // so every address in a page shares one answer, and a stack walk
        // revisits the same handful of pages over and over.
        const PAGE: usize = 4096;
        thread_local! {
            static OWNED: RefCell<HashMap<usize, bool>> =
                RefCell::new(HashMap::new());
        }
        let page = pc / PAGE;
        if let Some(hit) = OWNED.with(|c| c.borrow().get(&page).copied()) {
            return hit;
        }
        let owned = unsafe {
            let mut info: libc::Dl_info = std::mem::zeroed();
            libc::dladdr(pc as *const c_void, &mut info) != 0 && !info.dli_fbase.is_null()
        };
        OWNED.with(|c| {
            let mut m = c.borrow_mut();
            // JIT code is mapped and unmapped over a run, so the map is a
            // cache and not a registry: bound it rather than let a long run
            // accumulate a page entry per compiled function.
            if m.len() > 8192 {
                m.clear();
            }
            m.insert(page, owned);
        });
        owned
    }

    #[cfg(not(unix))]
    pub(super) fn native_image_owns_pc(_pc: usize) -> bool {
        false
    }

    /// Capture return addresses from the native stack. Generated code ranges
    /// are registered by both AIR V2 backends, so this works for Cranelift,
    /// LLVM promotion, and a stack containing frames from both tiers.
    pub(super) fn compiled_stack_functions(&self, _frame_hint: *const usize) -> Vec<usize> {
        const MAX_FRAMES: usize = 256;
        let mut functions = Vec::new();

        #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
        if !_frame_hint.is_null() {
            unsafe {
                let mut attr: libc::pthread_attr_t = std::mem::zeroed();
                let mut stack_base = std::ptr::null_mut::<c_void>();
                let mut stack_size = 0usize;
                let have_attr = libc::pthread_getattr_np(libc::pthread_self(), &mut attr) == 0;
                let have_bounds = have_attr
                    && libc::pthread_attr_getstack(&attr, &mut stack_base, &mut stack_size) == 0;
                if have_attr {
                    libc::pthread_attr_destroy(&mut attr);
                }

                if have_bounds {
                    let stack_low = stack_base as usize;
                    let stack_high = stack_low.saturating_add(stack_size);
                    let mut frame = _frame_hint as usize;
                    for _ in 0..MAX_FRAMES {
                        if frame < stack_low
                            || frame > stack_high.saturating_sub(2 * std::mem::size_of::<usize>())
                            || !frame.is_multiple_of(std::mem::align_of::<usize>())
                        {
                            break;
                        }
                        let words = frame as *const usize;
                        let caller = *words;
                        let return_pc = *words.add(1);
                        if !Self::native_image_owns_pc(return_pc) {
                            if let Some((findex, _, _)) =
                                ash_core::profile::describe_jit_pc(return_pc)
                            {
                                if let Some(function_index) =
                                    func_of(&self.targets, findex as usize)
                                {
                                    if functions.last().copied() != Some(function_index) {
                                        functions.push(function_index);
                                    }
                                }
                            }
                        }
                        if caller <= frame
                            || caller >= stack_high
                            || caller - frame > stack_size
                            || !caller.is_multiple_of(std::mem::align_of::<usize>())
                        {
                            break;
                        }
                        frame = caller;
                    }
                }
            }
        }

        if !functions.is_empty() {
            return functions;
        }

        let mut pcs = [std::ptr::null_mut::<c_void>(); MAX_FRAMES];

        #[cfg(unix)]
        let count = unsafe { libc::backtrace(pcs.as_mut_ptr(), MAX_FRAMES as i32).max(0) as usize };

        #[cfg(windows)]
        let count = unsafe {
            windows_sys::Win32::System::Diagnostics::Debug::RtlCaptureStackBackTrace(
                0,
                MAX_FRAMES as u32,
                pcs.as_mut_ptr(),
                std::ptr::null_mut(),
            ) as usize
        };

        #[cfg(not(any(unix, windows)))]
        let count = 0;

        for pc in pcs.iter().take(count) {
            if Self::native_image_owns_pc(*pc as usize) {
                continue;
            }
            let Some((findex, _, _)) = ash_core::profile::describe_jit_pc(*pc as usize) else {
                continue;
            };
            let Some(function_index) = func_of(&self.targets, findex as usize) else {
                continue;
            };
            if functions.last().copied() != Some(function_index) {
                functions.push(function_index);
            }
        }
        functions
    }

    /// Render the live interpreter and generated-code frames as HashLink
    /// `hl_symbol` tokens.
    ///
    /// The public ABI treats a symbol as opaque until `resolve_symbol`; using
    /// a stable UTF-16 buffer address as the token lets that second call return
    /// the already-rendered value without exposing Rust frame storage to Haxe.
    pub(super) fn stack_symbols(
        &self,
        bytecode: &DecodedBytecode,
        frame_hint: *const usize,
    ) -> Vec<Box<[u16]>> {
        let compiled = self.compiled_stack_functions(frame_hint);
        let mut symbols: Vec<Box<[u16]>> = compiled
            .iter()
            // Cranelift does not currently expose per-instruction native PC
            // offsets. Use the function's first debug position; the opaque
            // token remains structurally valid and identifies the exact Haxe
            // function while source-map plumbing is added independently.
            .filter_map(|&function_index| Self::stack_symbol(bytecode, function_index, 0))
            .collect();
        let mut last = compiled.last().copied();
        for &function_index in self.jit_bridge_callers.iter().rev() {
            if last == Some(function_index) {
                continue;
            }
            if let Some(symbol) = Self::stack_symbol(bytecode, function_index, 0) {
                symbols.push(symbol);
                last = Some(function_index);
            }
        }
        for frame in self.stack.iter().rev() {
            if last == Some(frame.function_index) {
                continue;
            }
            if let Some(symbol) =
                self.interpreter_stack_symbol(bytecode, frame.function_index, frame.pc)
            {
                symbols.push(symbol);
                last = Some(frame.function_index);
            }
        }

        // NativeStackTrace deliberately discards the outermost raw entry.
        // HashLink's platform unwinders naturally include a C runtime frame;
        // append an equivalent opaque terminator so the last Haxe frame is
        // retained even when Ash filters all non-JIT PCs above.
        if !symbols.is_empty() {
            let mut terminator: Vec<u16> = "fun$0(unknown:0)".encode_utf16().collect();
            terminator.push(0);
            symbols.push(terminator.into_boxed_slice());
        }
        symbols
    }

    pub(super) fn prepare_call_stack(
        &mut self,
        bytecode: &DecodedBytecode,
        frame_hint: *const usize,
    ) -> usize {
        let symbols = self.stack_symbols(bytecode, frame_hint);
        self.call_stack_symbols = symbols
            .iter()
            .map(|symbol| symbol.as_ptr() as usize)
            .collect();
        self.stack_symbol_arena.extend(symbols);
        self.call_stack_symbols.len()
    }

    pub(super) unsafe fn write_call_stack(&mut self, output: *mut *mut c_void, capacity: i32) -> i32 {
        if !output.is_null() {
            for (index, symbol) in self
                .call_stack_symbols
                .iter()
                .take(capacity.max(0) as usize)
                .enumerate()
            {
                *output.add(index) = *symbol as *mut c_void;
            }
        }
        self.call_stack_symbols.len() as i32
    }

    pub(super) fn capture_exception_stack(&mut self, bytecode: &DecodedBytecode) {
        self.prepare_call_stack(bytecode, std::ptr::null());
        self.exception_stack_symbols = self.call_stack_symbols.clone();
    }

    pub(super) fn stack_raw_native(
        &mut self,
        bytecode: &DecodedBytecode,
        args: &[NanBoxedValue],
        exception: bool,
    ) -> Result<NanBoxedValue> {
        if exception {
            if self.exception_stack_symbols.is_empty() {
                self.capture_exception_stack(bytecode);
            }
        } else {
            self.prepare_call_stack(bytecode, std::ptr::null());
        }

        let symbols = if exception {
            &self.exception_stack_symbols
        } else {
            &self.call_stack_symbols
        };
        if let Some(arr) = args.first().filter(|v| !v.is_null() && !v.is_void()) {
            let arr = arr.as_ptr() as *mut hl::varray;
            if !arr.is_null() {
                let capacity = unsafe { (*arr).size.max(0) as usize };
                let data = unsafe {
                    (arr as *mut u8).add(std::mem::size_of::<hl::varray>()) as *mut *const u16
                };
                for (i, symbol) in symbols.iter().take(capacity).enumerate() {
                    unsafe { *data.add(i) = *symbol as *const u16 };
                }
            }
        }
        Ok(NanBoxedValue::from_i32(symbols.len() as i32))
    }

}
