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
use std::sync::Arc;

use ash_core::bytecode::DecodedBytecode;
use ash_core::hl_bindings as hl;
use ash_core::types::HLFunction;

use crate::values::NanBoxedValue;

use super::{func_of, HLInterpreter};

/// One symbolicated frame, with its pieces kept apart.
///
/// The flat `Name(file:line)` label is what a terminal trace prints, but a
/// renderer that wants to show the source line needs the file and the line
/// as themselves. Formatting first and parsing back is how that goes wrong,
/// so the label is `Display` over the parts rather than the stored form.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraceFrame {
    pub symbol: Arc<str>,
    pub file: Option<Arc<str>>,
    pub line: i32,
}

impl std::fmt::Display for TraceFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.file {
            Some(file) => write!(f, "{}({}:{})", self.symbol, file, self.line),
            None => write!(f, "{}", self.symbol),
        }
    }
}

impl HLInterpreter {
    pub(super) fn capture_call_stack(&self, bytecode: &DecodedBytecode) -> Vec<Arc<TraceFrame>> {
        let bc = self.reloaded_bytecode.unwrap_or(bytecode);
        let names = self.function_name_table(bc);
        // Same reasoning as the UTF-16 symbols this module interns for the
        // native side: a frame's label is a pure function of its findex and
        // source position, a throw renders every frame on the stack, and
        // a game throws continuously while loading. Rendering them afresh put
        // `format!` at 96% of this function's time in a sampled freeze.
        // Keyed per bytecode, matching `function_name_table`, so a hot reload
        // does not serve labels built against the old program.
        /// Interned frame labels for one bytecode, keyed by
        /// `(findex, file, line)`.
        type FrameLabels = RefCell<Option<(usize, HashMap<(usize, i32, i32), Arc<TraceFrame>>)>>;
        thread_local! {
            static FRAMES: FrameLabels = const { RefCell::new(None) };
        }
        let bc_key = bc as *const DecodedBytecode as usize;
        FRAMES.with(|cell| {
            let mut slot = cell.borrow_mut();
            if !matches!(slot.as_ref(), Some((k, _)) if *k == bc_key) {
                *slot = Some((bc_key, HashMap::new()));
            }
            let cache = &mut slot.as_mut().expect("just populated").1;
            self.stack
                .iter()
                .rev()
                .map(|frame| {
                    // Never drop a frame: a trace that silently omits the
                    // frames it could not name is worse than one that admits
                    // them, since the gap is invisible and the caller looks
                    // like the callee.
                    let Some(func) = bc.functions.get(frame.function_index) else {
                        return Arc::new(TraceFrame {
                            symbol: Arc::from(
                                format!("<unresolved findex {}>", frame.function_index).as_str(),
                            ),
                            file: None,
                            line: 0,
                        });
                    };
                    let key = Self::stack_symbol_key(func, frame.pc);
                    Arc::clone(cache.entry(key).or_insert_with(|| {
                        let (findex, file_idx, line) = key;
                        let name = names.get(&findex).cloned().unwrap_or_else(|| func.name());
                        Arc::new(TraceFrame {
                            symbol: Arc::from(name.as_str()),
                            // No debug info (a release build): the name alone
                            // still says which function, which beats nothing.
                            file: usize::try_from(file_idx)
                                .ok()
                                .and_then(|i| bc.debug_files.get(i))
                                .map(|file| Arc::from(file.as_str())),
                            line,
                        })
                    }))
                })
                .collect()
        })
    }

    /// The `(findex, file, line)` a frame symbolicates to. Two reads off the
    /// debug table and no allocation, so it can key the symbol cache.
    fn stack_symbol_key(func: &HLFunction, pc: usize) -> (usize, i32, i32) {
        let debug_pc = pc.min(func.ops.len().saturating_sub(1));
        let file_idx = func.debug.get(debug_pc * 2).copied().unwrap_or(-1);
        let line = func.debug.get(debug_pc * 2 + 1).copied().unwrap_or(0);
        (func.findex as usize, file_idx, line)
    }

    /// Pointer to the interned UTF-16 symbol for `key`, built once.
    ///
    /// `write_call_stack` hands these pointers to Haxe, so a symbol has to
    /// outlive the capture that produced it. The symbol is also a pure
    /// function of its key, so interning serves both ends: the map owns every
    /// box for the life of the interpreter, and a frame that recurs costs a
    /// lookup rather than a `format!` and a UTF-16 re-encode. Its size is
    /// bounded by the distinct source positions a trace ever names, where
    /// appending per capture is bounded by nothing -- a game throws while
    /// loading a level, and each throw symbolicated every frame afresh.
    fn intern_stack_symbol(&mut self, bytecode: &DecodedBytecode, key: (usize, i32, i32)) -> usize {
        if let Some(symbol) = self.stack_symbols_interned.get(&key) {
            return symbol.as_ptr() as usize;
        }
        let (findex, file_idx, line) = key;
        let file = usize::try_from(file_idx)
            .ok()
            .and_then(|idx| bytecode.debug_files.get(idx))
            .map(String::as_str)
            .unwrap_or("unknown");
        let mut symbol: Vec<u16> = format!("fun${findex}({file}:{line})")
            .encode_utf16()
            .collect();
        symbol.push(0);
        self.stack_symbols_interned
            .entry(key)
            .or_insert_with(|| symbol.into_boxed_slice())
            .as_ptr() as usize
    }

    fn stack_symbol(
        &mut self,
        bytecode: &DecodedBytecode,
        function_index: usize,
        pc: usize,
    ) -> Option<usize> {
        let key = Self::stack_symbol_key(bytecode.functions.get(function_index)?, pc);
        Some(self.intern_stack_symbol(bytecode, key))
    }

    fn interpreter_stack_symbol(
        &mut self,
        bytecode: &DecodedBytecode,
        function_index: usize,
        pc: usize,
    ) -> Option<usize> {
        bytecode.functions.get(function_index)?;
        // AIR V2's serializer renumbers opcodes. Cache::prepare builds a
        // matching debug table for that optimized body, so frame.pc must be
        // resolved against the body the interpreter actually executes rather
        // than the original bytecode function at the same numeric index.
        // The walker's own body first: it publishes pcs into the serialized
        // opcodes and carries the debug table built for them. `air.body` is
        // the right answer only for the path that executes those opcodes.
        let key = match self.ssa.body(function_index) {
            Some(prep) if !prep.shim.debug.is_empty() => Self::stack_symbol_key(prep.shim, pc),
            _ => Self::stack_symbol_key(self.air.body(bytecode, function_index), pc),
        };
        Some(self.intern_stack_symbol(bytecode, key))
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
        // frame of every captured stack. In a game that throws while
        // loading a level, dyld's findClosestSymbol was the busiest non-idle
        // leaf in the whole process. Ownership is a property of the mapping,
        // so every address in a page shares one answer, and a stack walk
        // revisits the same handful of pages over and over.
        use std::cell::RefCell;
        use std::collections::HashMap;
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
    /// Deepest trace ash will render, matching the 256 the native walks in
    /// `compiled_stack_functions` already impose.
    ///
    /// Those caps covered only the native PCs; the bridge-caller and
    /// interpreter-frame walks below were unbounded, so a deep Haxe chain
    /// produced an unbounded symbol list -- rebuilt, and cloned, on EVERY
    /// throw. Nothing reads past a few dozen frames of a trace, and the cost
    /// of the tail is paid whether or not anyone looks at it.
    pub(super) const MAX_TRACE_FRAMES: usize = 256;

    pub(super) fn stack_symbols(
        &mut self,
        bytecode: &DecodedBytecode,
        frame_hint: *const usize,
    ) -> Vec<usize> {
        let compiled = self.compiled_stack_functions(frame_hint);
        let mut symbols: Vec<usize> = Vec::with_capacity(compiled.len() + self.stack.len() + 1);
        for &function_index in &compiled {
            // Cranelift does not currently expose per-instruction native PC
            // offsets. Use the function's first debug position; the opaque
            // token remains structurally valid and identifies the exact Haxe
            // function while source-map plumbing is added independently.
            if let Some(symbol) = self.stack_symbol(bytecode, function_index, 0) {
                symbols.push(symbol);
            }
        }
        let mut last = compiled.last().copied();
        // Indexed rather than iterated: symbolicating borrows the interpreter
        // mutably to fill the cache.
        for i in (0..self.jit_bridge_callers.len()).rev() {
            if symbols.len() >= Self::MAX_TRACE_FRAMES {
                break;
            }
            let function_index = self.jit_bridge_callers[i];
            if last == Some(function_index) {
                continue;
            }
            if let Some(symbol) = self.stack_symbol(bytecode, function_index, 0) {
                symbols.push(symbol);
                last = Some(function_index);
            }
        }
        for i in (0..self.stack.len()).rev() {
            if symbols.len() >= Self::MAX_TRACE_FRAMES {
                break;
            }
            let (function_index, pc) = (self.stack[i].function_index, self.stack[i].pc);
            if last == Some(function_index) {
                continue;
            }
            if let Some(symbol) = self.interpreter_stack_symbol(bytecode, function_index, pc) {
                symbols.push(symbol);
                last = Some(function_index);
            }
        }

        // NativeStackTrace deliberately discards the outermost raw entry.
        // HashLink's platform unwinders naturally include a C runtime frame;
        // append an equivalent opaque terminator so the last Haxe frame is
        // retained even when Ash filters all non-JIT PCs above. Findex 0 with
        // no debug file interns to exactly "fun$0(unknown:0)".
        if !symbols.is_empty() {
            symbols.push(self.intern_stack_symbol(bytecode, (0, -1, 0)));
        }
        symbols
    }

    pub(super) fn prepare_call_stack(
        &mut self,
        bytecode: &DecodedBytecode,
        frame_hint: *const usize,
    ) -> usize {
        self.call_stack_symbols = self.stack_symbols(bytecode, frame_hint);
        self.call_stack_symbols.len()
    }

    pub(super) unsafe fn write_call_stack(
        &mut self,
        output: *mut *mut c_void,
        capacity: i32,
    ) -> i32 {
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
