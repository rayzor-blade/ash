use crate::array::hlp_alloc_array;
use crate::fun::hlp_dyn_call;
use crate::gc::ImmixAllocator;
use crate::hl::{self, uchar, varray, vbyte, vclosure, vdynamic};
use crate::strings::str_to_uchar_ptr;
use crate::types::hl_aptr;
use anyhow::Result;
use std::ffi::c_void;
use std::fmt::{self, Formatter};
use std::mem;
use std::panic;
use std::sync::atomic::{AtomicUsize, Ordering};

type ResolveSymbol = unsafe extern "C" fn(*mut c_void, *mut u8, *mut i32) -> *mut u8;
type CaptureStack = unsafe extern "C" fn(*mut *mut c_void, i32) -> i32;

static RESOLVE_SYMBOL: AtomicUsize = AtomicUsize::new(0);
static CAPTURE_STACK: AtomicUsize = AtomicUsize::new(0);

thread_local! {
    static EXCEPTION_STACK: std::cell::RefCell<Vec<usize>> = const {
        std::cell::RefCell::new(Vec::new())
    };
    static CALL_STACK_FRAME: std::cell::Cell<usize> = const {
        std::cell::Cell::new(0)
    };
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
extern "C" {
    /// Keeps its own frame pointer so the walker has a place to start; see
    /// stack_boundary.c. Rust owns the primitive's name, because a cdylib
    /// exports Rust symbols and hides the rest.
    fn ash_call_stack_boundary(arr: *mut varray) -> i32;
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct HLException {
    pub t: *mut hl::hl_type,
    pub value: *mut hl::vdynamic,
    pub stack_trace: *mut StackTrace,
}

#[repr(C)]
pub struct StackTrace {
    pub frames: Vec<StackFrame>,
}

#[repr(C)]
pub struct StackFrame {
    pub file_name: String,
    pub function_name: String,
    pub line_number: i32,
}

impl Default for StackTrace {
    fn default() -> Self {
        Self::new()
    }
}

impl StackTrace {
    pub fn new() -> Self {
        StackTrace { frames: Vec::new() }
    }

    pub fn add_frame(&mut self, file: String, function: String, line: i32) {
        self.frames.push(StackFrame {
            file_name: file,
            function_name: function,
            line_number: line,
        });
    }
}

#[derive(Clone)]
pub struct VDynamicException(Box<vdynamic>);

impl VDynamicException {
    pub fn new(vd: Box<vdynamic>) -> Self {
        VDynamicException(vd)
    }

    pub fn into_raw(self) -> *mut vdynamic {
        Box::into_raw(self.0)
    }

    pub unsafe fn from_raw(ptr: *mut vdynamic) -> Self {
        VDynamicException(Box::from_raw(ptr))
    }
}

impl std::fmt::Debug for VDynamicException {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "VDynamic({:?})", self.0.as_ref() as *const vdynamic)
    }
}

unsafe impl Send for VDynamicException {}

/// Best-effort rendering of a thrown value for the uncaught-exception path.
///
/// Deliberately defensive rather than complete: this runs on the way to
/// abort, possibly with a heap in a bad state, so every dereference is
/// guarded and anything unrecognized falls back to kind+pointer — the old
/// output, as a floor rather than a ceiling.
unsafe fn describe_exception(v: *mut hl::vdynamic) -> String {
    if v.is_null() {
        return "null".into();
    }
    let t = (*v).t;
    if t.is_null() || (t as usize) < 0x10000 {
        return format!("<corrupt type> ptr={v:p}");
    }
    let kind = (*t).kind;
    let utf16z = |p: *const hl::uchar| -> String {
        if p.is_null() {
            return "null".into();
        }
        let mut n = 0usize;
        while n < 4096 && *p.add(n) != 0 {
            n += 1;
        }
        String::from_utf16_lossy(std::slice::from_raw_parts(p, n))
    };
    if kind == hl::hl_type_kind_HBYTES {
        // A thrown bytes value is a message string in every case the stdlib
        // produces (hl_error goes through here).
        return format!("\"{}\"", utf16z((*v).v.bytes as *const hl::uchar));
    }
    if kind == hl::hl_type_kind_HOBJ {
        let obj = (*t).__bindgen_anon_1.obj;
        if !obj.is_null() && (obj as usize) >= 0x10000 {
            let name = utf16z((*obj).name);
            // A String object's payload is worth printing whole; for any
            // other class the name alone locates the throw site. Exactly
            // "String": the interpreter's old first-character truncation made
            // an `"S"` alternative look necessary once, and matching it here
            // would read an arbitrary S-named class's fields as bytes/length.
            if name == "String" {
                let bytes = *((v as *const u8).add(8) as *const *const hl::uchar);
                return format!("String \"{}\"", utf16z(bytes));
            }
            return format!("instance of {name} ({v:p})");
        }
    }
    format!("kind={kind} ptr={v:p}")
}

/// Print an exception caught by a VM-level safe-call boundary.
///
/// HashLink runs its bytecode entrypoint through `hl_dyn_call_safe`; Ash's
/// whole-module JIT uses an equivalent generated wrapper and calls back here
/// once the longjmp has landed. Keeping the defensive value decoding beside
/// `hlp_throw` also prevents the JIT runner from dereferencing GC objects.
#[no_mangle]
pub unsafe extern "C" fn hlp_print_uncaught_exception(v: *mut hl::vdynamic) {
    eprintln!("[ash] uncaught exception: {}", describe_exception(v));
}

pub struct TrapContext {
    pub buf: hl::jmp_buf,
    pub has_jmpbuf: bool,
    pub prev: *mut TrapContext,
    pub exception_value: Option<VDynamicException>,
    pub caught: bool,

    /// GC-lock depth held by this thread at the setjmp site. hlp_throw
    /// restores the lock to this depth before longjmp, releasing guards
    /// held by the frames being jumped over (their Drop never runs).
    pub saved_lock_depth: usize,
    /// Shadow call-stack depth at the setjmp site, restored the same way:
    /// the frames being jumped over never reach their pop. Always 0 on a
    /// target that walks its machine stack instead (see `shadow`).
    pub saved_shadow_depth: usize,
}

impl Default for TrapContext {
    fn default() -> Self {
        Self::new()
    }
}

impl TrapContext {
    pub fn new() -> Self {
        TrapContext {
            // jmp_buf layout is target-dependent; keep it opaque and initialize storage.
            buf: unsafe { mem::zeroed() },
            has_jmpbuf: false,
            prev: std::ptr::null_mut(),
            exception_value: None,
            caught: false,
            saved_lock_depth: 0,
            saved_shadow_depth: 0,
        }
    }
}

impl ImmixAllocator {
    pub fn setup_trap(&self) -> *mut TrapContext {
        setup_trap()
    }

    pub fn remove_trap(&self) {
        remove_trap()
    }

    pub fn throw(&self, exception: VDynamicException) -> ! {
        panic::panic_any(exception);
    }

    pub fn run_with_trap<F, R>(&self, f: F) -> Result<R, VDynamicException>
    where
        F: FnOnce() -> R + panic::UnwindSafe,
    {
        let trap = self.setup_trap();
        let result = panic::catch_unwind(f);
        unsafe {
            if (*trap).caught {
                let exception = (*trap).exception_value.take().unwrap();
                self.remove_trap();
                Err(exception)
            } else {
                self.remove_trap();
                match result {
                    Ok(value) => Ok(value),
                    Err(e) => {
                        if let Some(vdynamic_exception) = e.downcast_ref::<VDynamicException>() {
                            Err(vdynamic_exception.clone())
                        } else {
                            // Handle other panic types if needed
                            panic!("Unexpected panic type")
                        }
                    }
                }
            }
        }
    }

    // pub fn throw_exception(&mut self, exception: HLException) {
    //     let mut boxed_exception = Box::new(exception);
    //     if let Some(handler) = &self.exception_handler {
    //         handler(&mut boxed_exception);
    //     } else {
    //         self.current_exception = Some(boxed_exception);
    //     }
    // }

    pub fn set_exception_handler(
        &mut self,
        handler: Box<dyn Fn(&mut HLException) -> Result<*mut vdynamic, VDynamicException>>,
    ) {
        self.exception_handler = Some(handler);
    }

    pub fn clear_exception(&mut self) {
        self.current_exception = None;
    }

    pub fn get_current_exception(&self) -> Option<&HLException> {
        self.current_exception.as_deref()
    }

    pub fn mark_exception(&mut self) {
        if let Some(exception) = self.current_exception.clone() {
            self.mark_vdynamic(exception.value);
            self.mark_stack_trace(exception.stack_trace);
        }
    }

    fn mark_stack_trace(&mut self, stack_trace: *mut StackTrace) {
        if !stack_trace.is_null() {
            unsafe {
                let trace = &*stack_trace;
                for frame in &trace.frames {
                    self.mark_memory(frame.file_name.as_ptr() as *mut u8, frame.file_name.len());
                    self.mark_memory(
                        frame.function_name.as_ptr() as *mut u8,
                        frame.function_name.len(),
                    );
                }
            }
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_exception_stack() -> *mut varray {
    let gc = crate::gc::gc_locked();

    if let Some(exception) = gc.get_current_exception() {
        let stack_trace = &*exception.stack_trace;
        let frame_count = stack_trace.frames.len();

        // Allocate a varray to hold the stack frames
        let varray_ptr: *mut varray =
            hlp_alloc_array(crate::types::hlt_bytes(), frame_count as i32);

        // Fill the array with stack frame information
        for (i, frame) in stack_trace.frames.iter().enumerate() {
            *(hl_aptr::<*const vbyte>(varray_ptr).add(i)) = str_to_uchar_ptr(&format!(
                "{}:{} {}",
                frame.file_name, frame.line_number, frame.function_name
            )) as *const vbyte;
        }

        varray_ptr
    } else {
        std::ptr::null_mut()
    }
}

/// Install the platform/JIT symbolizer and stack unwinder used by HashLink's
/// public NativeStackTrace API. This is the same callback contract as
/// `hl_setup_exception` in upstream HashLink; Ash's host installs callbacks
/// after loading bytecode so generated PCs can be mapped back to findices.
/// Symbols an ahead-of-time binary registers at startup: the `ash_functions`
/// table (one code address per findex) and a parallel table of names. The JIT
/// hands the runtime a symbolizer and a stack walker that live in the ash
/// crate; a standalone binary links only this library, so without these two
/// defaults every `haxe.CallStack.exceptionStack()` came back empty -- and
/// heaps' own error reporter then faulted on the null while printing.
static AOT_SYMBOLS: std::sync::Mutex<Vec<(usize, &'static str)>> = std::sync::Mutex::new(Vec::new());
#[cfg(not(target_family = "wasm"))]
static AOT_SYMBOL_TEXT: std::sync::Mutex<Vec<(usize, &'static [u16])>> = std::sync::Mutex::new(Vec::new());
/// The same names by position: entry `findex` is that function's name, or
/// "" where the emitter had none. A shadow frame is keyed by findex rather
/// than by address, so this is how it is named.
static AOT_NAMES_BY_FINDEX: std::sync::Mutex<Vec<&'static str>> = std::sync::Mutex::new(Vec::new());
/// The module's debug-file table, by the index a shadow frame's position
/// carries; see `hlp_register_aot_debug_files`.
static AOT_DEBUG_FILES: std::sync::Mutex<Vec<&'static str>> = std::sync::Mutex::new(Vec::new());

#[no_mangle]
pub unsafe extern "C" fn hlp_register_aot_symbols(
    starts: *const *const c_void,
    names: *const *const std::os::raw::c_char,
    count: usize,
) {
    if starts.is_null() || names.is_null() {
        return;
    }
    let mut table = Vec::with_capacity(count);
    let mut by_findex = Vec::with_capacity(count);
    for i in 0..count {
        let start = *starts.add(i) as usize;
        let name = *names.add(i);
        let text: &'static str = if name.is_null() {
            ""
        } else {
            Box::leak(
                std::ffi::CStr::from_ptr(name)
                    .to_string_lossy()
                    .into_owned()
                    .into_boxed_str(),
            )
        };
        by_findex.push(text);
        if start == 0 || name.is_null() {
            continue;
        }
        table.push((start, text));
    }
    table.sort_by_key(|(start, _)| *start);
    table.dedup_by_key(|(start, _)| *start);
    *AOT_SYMBOLS.lock().unwrap_or_else(|e| e.into_inner()) = table;
    *AOT_NAMES_BY_FINDEX
        .lock()
        .unwrap_or_else(|e| e.into_inner()) = by_findex;
    // A wasm module has no machine stack to walk, so its pair reads the
    // shadow stack its functions maintain; everywhere else the frame-pointer
    // walker and the address table do. Neither displaces a pair the JIT
    // installed first.
    #[cfg(target_family = "wasm")]
    let (resolve, capture): (ResolveSymbol, CaptureStack) =
        (shadow::resolve_symbol, shadow::capture_stack);
    #[cfg(not(target_family = "wasm"))]
    let (resolve, capture): (ResolveSymbol, CaptureStack) = (aot_resolve_symbol, aot_capture_stack);
    if RESOLVE_SYMBOL.load(Ordering::Acquire) == 0 {
        RESOLVE_SYMBOL.store(resolve as usize, Ordering::Release);
    }
    if CAPTURE_STACK.load(Ordering::Acquire) == 0 {
        CAPTURE_STACK.store(capture as usize, Ordering::Release);
    }
}

/// The debug-file table of an ahead-of-time module, registered by the
/// emitter after its symbols. A shadow frame records `(file, line)` with
/// `file` an index into this table, which is how HashLink's bytecode itself
/// spells positions; the emitter only hands the table over for a target whose
/// frames record positions at all.
#[no_mangle]
pub unsafe extern "C" fn hlp_register_aot_debug_files(
    files: *const *const std::os::raw::c_char,
    count: usize,
) {
    if files.is_null() {
        return;
    }
    let table: Vec<&'static str> = (0..count)
        .map(|i| {
            let file = *files.add(i);
            if file.is_null() {
                ""
            } else {
                Box::leak(
                    std::ffi::CStr::from_ptr(file)
                        .to_string_lossy()
                        .into_owned()
                        .into_boxed_str(),
                )
            }
        })
        .collect();
    *AOT_DEBUG_FILES.lock().unwrap_or_else(|e| e.into_inner()) = table;
}

/// No shadow stack on a target whose machine stack the runtime can walk: a
/// trap records depth 0 and unwinding to it is nothing.
#[cfg(not(target_family = "wasm"))]
mod shadow {
    pub fn depth() -> usize {
        0
    }

    pub fn unwind_to(_depth: usize) {}
}

/// The shadow call stack of a WebAssembly module.
///
/// A wasm call stack is not addressable: no frame pointer, no return address,
/// nothing a walker could read. So on that target every compiled Haxe
/// function opens a frame here at entry (`hlp_shadow_push`), stores its
/// source position into the frame as it executes (the emitter's `Pos`
/// markers), and closes it on return (`hlp_shadow_pop`). A throw abandons the
/// frames between the throw and the catch, so `hlp_setup_trap_jit` records
/// the depth and `throw_impl` unwinds to it, alongside the GC lock depth.
///
/// Fixed capacity and no allocation: a push runs in every prologue, and a
/// stack this deep is a runaway recursion that fails on its own terms. Past
/// the capacity a push still counts -- the pops have to balance -- and writes
/// into a scratch slot; those innermost frames are simply missing from a
/// trace.
///
/// wasm32-wasip1 here is single-threaded, which is what makes one static
/// stack sound. Fibers would share it and interleave their frames.
#[cfg(target_family = "wasm")]
mod shadow {
    use std::cell::UnsafeCell;
    use std::collections::BTreeMap;
    use std::ffi::c_void;
    use std::sync::Mutex;

    const CAP: usize = 1 << 15;

    #[repr(C)]
    #[derive(Clone, Copy)]
    struct Frame {
        /// `(file << 32) | line`, as the emitter stores it; 0 until the
        /// function reaches its first marker.
        pos: u64,
        findex: u32,
    }

    struct Stack {
        frames: UnsafeCell<[Frame; CAP]>,
        depth: UnsafeCell<usize>,
        scratch: UnsafeCell<u64>,
    }

    // One thread; see the module docs.
    unsafe impl Sync for Stack {}

    static STACK: Stack = Stack {
        frames: UnsafeCell::new([Frame { pos: 0, findex: 0 }; CAP]),
        depth: UnsafeCell::new(0),
        scratch: UnsafeCell::new(0),
    };

    /// Open a frame for `findex`; the returned slot receives its positions.
    pub unsafe fn push(findex: u32) -> *mut u64 {
        let depth = &mut *STACK.depth.get();
        let at = *depth;
        *depth = at + 1;
        if at < CAP {
            let frame = &mut (*STACK.frames.get())[at];
            *frame = Frame { pos: 0, findex };
            &mut frame.pos
        } else {
            STACK.scratch.get()
        }
    }

    /// Close the innermost frame.
    pub unsafe fn pop() {
        let depth = &mut *STACK.depth.get();
        *depth = depth.saturating_sub(1);
    }

    pub fn depth() -> usize {
        unsafe { *STACK.depth.get() }
    }

    /// Drop the frames a longjmp is about to abandon.
    pub fn unwind_to(depth: usize) {
        unsafe {
            let current = &mut *STACK.depth.get();
            if depth < *current {
                *current = depth;
            }
        }
    }

    /// What a raw stack entry points at. Haxe holds the entries as opaque
    /// words and hands them back to `resolve_symbol`; on wasm32 a word is 32
    /// bits, too small for the frame itself, so each distinct (function,
    /// position) is leaked once and named by its address.
    #[repr(C)]
    struct Symbol {
        findex: u32,
        pos: u64,
    }

    static SYMBOLS: Mutex<BTreeMap<(u32, u64), &'static Symbol>> = Mutex::new(BTreeMap::new());
    static TEXT: Mutex<BTreeMap<usize, &'static [u16]>> = Mutex::new(BTreeMap::new());

    fn symbol_for(findex: u32, pos: u64) -> *mut c_void {
        let mut table = SYMBOLS.lock().unwrap_or_else(|e| e.into_inner());
        let symbol = table
            .entry((findex, pos))
            .or_insert_with(|| Box::leak(Box::new(Symbol { findex, pos })));
        *symbol as *const Symbol as *mut c_void
    }

    /// The `CaptureStack` callback: the frames innermost first, the way the
    /// machine-stack walkers report them, so `haxe.NativeStackTrace`'s
    /// arithmetic holds unchanged. With a null `output` it only counts.
    pub unsafe extern "C" fn capture_stack(output: *mut *mut c_void, capacity: i32) -> i32 {
        let depth = depth().min(CAP);
        if output.is_null() {
            return depth as i32;
        }
        let frames = &*STACK.frames.get();
        let written = depth.min(capacity.max(0) as usize);
        for (i, frame) in frames[..depth].iter().rev().take(written).enumerate() {
            *output.add(i) = symbol_for(frame.findex, frame.pos);
        }
        written as i32
    }

    /// The `ResolveSymbol` callback: `Class.method(file:line)` for a symbol
    /// from `capture_stack`, the shape `haxe.NativeStackTrace.toHaxe` parses.
    /// NUL-terminated UTF-16, cached per symbol; the caller reads
    /// `*buffer_len` code units from the returned pointer.
    pub unsafe extern "C" fn resolve_symbol(
        symbol: *mut c_void,
        _buffer: *mut u8,
        buffer_len: *mut i32,
    ) -> *mut u8 {
        if symbol.is_null() {
            return std::ptr::null_mut();
        }
        let mut cache = TEXT.lock().unwrap_or_else(|e| e.into_inner());
        let text = *cache.entry(symbol as usize).or_insert_with(|| {
            let Symbol { findex, pos } = *(symbol as *const Symbol);
            let mut units: Vec<u16> = super::format_frame(findex, pos).encode_utf16().collect();
            units.push(0);
            Box::leak(units.into_boxed_slice())
        });
        if !buffer_len.is_null() {
            *buffer_len = (text.len() - 1) as i32;
        }
        text.as_ptr() as *mut u8
    }
}

/// Open a shadow frame for `findex`: every compiled prologue of a wasm module
/// calls this, and stores the function's positions into the slot it returns.
/// The cfg sits on the export itself so the compiler's symbol-table scanner
/// (crates/ash/build.rs) leaves it out of a native binary, where it does not
/// exist.
#[cfg(target_family = "wasm")]
#[no_mangle]
pub unsafe extern "C" fn hlp_shadow_push(findex: u32) -> *mut u64 {
    shadow::push(findex)
}

/// Close the innermost shadow frame; every `Ret` of a wasm module calls it.
#[cfg(target_family = "wasm")]
#[no_mangle]
pub unsafe extern "C" fn hlp_shadow_pop() {
    shadow::pop()
}

/// The text of one shadow frame, in the shape Haxe's parser expects:
/// `Class.method(file:line)`, or `fun$<findex>(file:line)` for a closure or
/// the entrypoint, which it reads as a `LocalFunction`. A frame that never
/// reached a position marker is named alone, as HashLink names a frame
/// without debug info.
#[cfg(target_family = "wasm")]
fn format_frame(findex: u32, pos: u64) -> String {
    let names = AOT_NAMES_BY_FINDEX
        .lock()
        .unwrap_or_else(|e| e.into_inner());
    let name = match names.get(findex as usize).copied() {
        // The emitter's `#<hash>` key marks a function no class declares. A
        // static's declaring type is the `$Class` object; HashLink names the
        // frame by the class, so the marker goes.
        Some(name) if !name.is_empty() && !name.starts_with('#') => {
            name.strip_prefix('$').unwrap_or(name).to_owned()
        }
        _ => format!("fun${findex}"),
    };
    drop(names);
    if pos == 0 {
        return name;
    }
    let (file, line) = ((pos >> 32) as usize, pos as u32);
    let files = AOT_DEBUG_FILES.lock().unwrap_or_else(|e| e.into_inner());
    let file = files.get(file).copied().unwrap_or("?");
    format!("{name}({file}:{line})")
}

/// The largest a single compiled body is assumed to be, used to bound the
/// last entry of the table below. Bodies run to a few tens of kilobytes; a
/// megabyte is generous and keeps a return address in some unrelated library
/// from being attributed to the last Haxe function in the image.
#[cfg(not(target_family = "wasm"))]
const AOT_MAX_BODY_BYTES: usize = 1 << 20;

/// The registered name of the body containing `pc`, by address.
///
/// The table holds every body's entry address, sorted, so the body that owns
/// `pc` is the last entry at or below it -- bounded by the next entry, or by
/// `AOT_MAX_BODY_BYTES` for the final one. Address is the only key that
/// works everywhere: a sharded body is a hidden symbol, hidden symbols are
/// not in `.dynsym`, and glibc's `dladdr` reads nothing else, so on Linux it
/// names no program frame at all. Asking it first was the bug -- it made the
/// table reachable only for frames that did not need it.
#[cfg(not(target_family = "wasm"))]
fn aot_symbol_for_pc(pc: usize) -> Option<&'static str> {
    let table = AOT_SYMBOLS.lock().unwrap_or_else(|e| e.into_inner());
    if table.is_empty() {
        return None;
    }
    let i = match table.binary_search_by_key(&pc, |(s, _)| *s) {
        Ok(i) => i,
        Err(0) => return None,
        Err(i) => i - 1,
    };
    let (start, name) = table[i];
    let end = table
        .get(i + 1)
        .map(|(next, _)| *next)
        .unwrap_or(start.saturating_add(AOT_MAX_BODY_BYTES));
    if pc < end {
        Some(name)
    } else {
        None
    }
}

/// Whether the symbol `dladdr` gives a frame says it belongs to the runtime
/// rather than to the program. The runtime may be a separate library or
/// linked statically into the same image, so image identity cannot tell them
/// apart; names can. `ash_h*` is a helper thunk the emitter generates, which
/// the single-module build hides too, so a stack reads the same either way.
#[cfg(unix)]
fn aot_name_is_runtime(name: &str) -> bool {
    let name = name.trim_start_matches('_');
    name.starts_with("hlp_")
        || name.starts_with("hl_")
        || name.starts_with("ash_h")
        || name.starts_with("ash_")
        || name.starts_with("RNv")
        || name.starts_with("ZN")
        || name.starts_with("std_")
        || name == "main"
        || name == "start"
}

/// Whether a return address belongs to program code rather than to the
/// runtime.
///
/// The table decides, because it is the only source that covers every body.
/// `dladdr` still gets a veto where it can see: the table's ranges are
/// inferred from entry addresses, so a runtime function sitting between two
/// bodies would otherwise be attributed to the body before it.
#[cfg(unix)]
unsafe fn aot_frame_in_program(pc: usize) -> bool {
    // Answered once per address and remembered. `dladdr` is not a lookup: it
    // walks the containing image's symbol table linearly, and an AOT build of
    // a game carries tens of thousands of symbols. A throw walks up to 256
    // frames and asks about every one, so the cost is scanned-symbols x
    // frames x throws.
    //
    // MBHaxe throws inside collision search on every physics tick. That put
    // 98% of the process in dyld's findClosestSymbol and read to the player
    // as a hard freeze -- the AOT twin of the symbol-arena freeze fixed in
    // aa7dda2, which only ever covered the JIT walker.
    //
    // Caching is sound because the answer cannot change: an address either
    // lies in program text or it does not. The table is bounded by the code
    // actually appearing in a stack, not by the number of throws.
    if let Some(known) = aot_frame_class_cached(pc) {
        return known;
    }
    let verdict = aot_frame_in_program_uncached(pc);
    aot_frame_class_remember(pc, verdict);
    verdict
}

/// Sorted by address, like [`AOT_SYMBOLS`], and read the same way.
#[cfg(unix)]
static AOT_FRAME_CLASS: std::sync::Mutex<Vec<(usize, bool)>> = std::sync::Mutex::new(Vec::new());

#[cfg(unix)]
fn aot_frame_class_cached(pc: usize) -> Option<bool> {
    let table = AOT_FRAME_CLASS.lock().unwrap_or_else(|e| e.into_inner());
    table.binary_search_by_key(&pc, |(addr, _)| *addr).ok().map(|i| table[i].1)
}

#[cfg(unix)]
fn aot_frame_class_remember(pc: usize, verdict: bool) {
    let mut table = AOT_FRAME_CLASS.lock().unwrap_or_else(|e| e.into_inner());
    if let Err(i) = table.binary_search_by_key(&pc, |(addr, _)| *addr) {
        table.insert(i, (pc, verdict));
    }
}

#[cfg(unix)]
unsafe fn aot_frame_in_program_uncached(pc: usize) -> bool {
    let mut info: libc::Dl_info = std::mem::zeroed();
    let named = libc::dladdr(pc as *const c_void, &mut info) != 0 && !info.dli_sname.is_null();
    // Borrowed, not owned: `to_string_lossy` only allocates for a name that is
    // not valid UTF-8, and nothing here outlives the call.
    let dl_name = if named {
        Some(std::ffi::CStr::from_ptr(info.dli_sname).to_string_lossy())
    } else {
        None
    };
    if aot_symbol_for_pc(pc).is_some() {
        return match &dl_name {
            // `ash_f*` is what the sharded emitter calls a body, so the name
            // agreeing with the table is not a veto.
            Some(n) => n.trim_start_matches('_').starts_with("ash_f") || !aot_name_is_runtime(n),
            None => true,
        };
    }
    match dl_name {
        Some(n) => !aot_name_is_runtime(&n),
        None => false,
    }
}

/// Windows has no `dladdr`. The registered table is still enough to retain
/// program frames; it just cannot veto a runtime function that the linker
/// happened to place in a gap between two adjacent program bodies.
#[cfg(not(any(unix, target_family = "wasm")))]
unsafe fn aot_frame_in_program(pc: usize) -> bool {
    aot_symbol_for_pc(pc).is_some()
}

#[cfg(unix)]
unsafe fn aot_symbol_via_dladdr(pc: usize) -> Option<String> {
    // The table first, and by the pc itself: where dladdr is blind -- every
    // hidden body on Linux -- it is the only thing that can name the frame.
    if let Some(name) = aot_symbol_for_pc(pc) {
        return Some(name.to_string());
    }
    let mut info: libc::Dl_info = std::mem::zeroed();
    if libc::dladdr(pc as *const c_void, &mut info) == 0 || info.dli_sname.is_null() {
        return None;
    }
    // dladdr knows which function CONTAINS the address (inlining moves a
    // return address into whatever body absorbed the call); the registered
    // table knows that function's Haxe name. The emitter's own LLVM symbols
    // are abbreviations (`t`, `o.1234`), so they are only the last resort.
    if let Some(name) = aot_symbol_for_pc(info.dli_saddr as usize) {
        return Some(name.to_string());
    }
    let raw = std::ffi::CStr::from_ptr(info.dli_sname).to_string_lossy().into_owned();
    let name = raw.trim_start_matches('_');
    let name = match name.rfind('.') {
        Some(i) if name[i + 1..].chars().all(|c| c.is_ascii_digit()) => &name[..i],
        _ => name,
    };
    Some(name.to_string())
}

/// The emitter registers every program body and its Haxe name, so platforms
/// without `dladdr` can still resolve AOT frames directly from that table.
#[cfg(not(any(unix, target_family = "wasm")))]
unsafe fn aot_symbol_via_dladdr(pc: usize) -> Option<String> {
    aot_symbol_for_pc(pc).map(str::to_owned)
}

/// Walk the frame-pointer chain from the caller. Every body the emitter
/// produces keeps a frame pointer, and so does the runtime; the walk stops at
/// the first frame that does not (the C entry), or at anything that fails a
/// sanity check. With a null `output` it only counts, as the JIT walker does.
#[cfg(not(target_family = "wasm"))]
unsafe extern "C" fn aot_capture_stack(output: *mut *mut c_void, capacity: i32) -> i32 {
    let mut fp: usize;
    #[cfg(target_arch = "aarch64")]
    core::arch::asm!("mov {}, x29", out(reg) fp, options(nomem, nostack, preserves_flags));
    #[cfg(target_arch = "x86_64")]
    core::arch::asm!("mov {}, rbp", out(reg) fp, options(nomem, nostack, preserves_flags));
    #[cfg(not(any(target_arch = "aarch64", target_arch = "x86_64")))]
    {
        fp = 0;
    }
    let mut written = 0i32;
    let mut depth = 0;
    while fp != 0 && fp.is_multiple_of(8) && depth < 256 {
        let next = *(fp as *const usize);
        let ret = *((fp + 8) as *const usize);
        if ret == 0 {
            break;
        }
        // Keep the frames that belong to the program: dladdr says which
        // image a return address lives in, and the runtime's own frames on
        // the way to the throw are the ones in THIS library.
        let known = aot_frame_in_program(ret);
        if known {
            if !output.is_null() {
                if written >= capacity {
                    break;
                }
                *output.add(written as usize) = ret as *mut c_void;
            }
            written += 1;
        }
        if next <= fp || next - fp > (1 << 24) {
            break;
        }
        fp = next;
        depth += 1;
    }
    written
}

/// Map a return address back to the function that contains it and hand back
/// its name as a NUL-terminated UTF-16 string, cached per address: the
/// JIT's resolver returns a pointer into text it owns, and the caller reads
/// `*buffer_len` code units from it.
#[cfg(not(target_family = "wasm"))]
unsafe extern "C" fn aot_resolve_symbol(
    symbol: *mut c_void,
    _buffer: *mut u8,
    buffer_len: *mut i32,
) -> *mut u8 {
    let pc = symbol as usize;
    if let Some((_, text)) = AOT_SYMBOL_TEXT
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .iter()
        .find(|(addr, _)| *addr == pc)
    {
        if !buffer_len.is_null() {
            *buffer_len = (text.len() - 1) as i32;
        }
        return text.as_ptr() as *mut u8;
    }
    // The exact enclosing symbol first: with inlining, a return address sits
    // in whatever body absorbed the call, which is not necessarily a table
    // entry, and "nearest table start below" then names a neighbour. The
    // linker's symbol table knows every body, local ones included on
    // Darwin; the registered table is the fallback where dladdr only sees
    // exported symbols.
    let name: Option<String> = aot_symbol_via_dladdr(pc).or_else(|| {
        let table = AOT_SYMBOLS.lock().unwrap_or_else(|e| e.into_inner());
        match table.binary_search_by_key(&pc, |(start, _)| *start) {
            Ok(i) => Some(table[i].1.to_string()),
            Err(0) => None,
            Err(i) => (pc - table[i - 1].0 < (1 << 22)).then(|| table[i - 1].1.to_string()),
        }
    });
    let Some(name) = name else {
        return std::ptr::null_mut();
    };
    let mut units: Vec<u16> = name.encode_utf16().collect();
    units.push(0);
    let text: &'static [u16] = Box::leak(units.into_boxed_slice());
    AOT_SYMBOL_TEXT
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .push((pc, text));
    if !buffer_len.is_null() {
        *buffer_len = (text.len() - 1) as i32;
    }
    text.as_ptr() as *mut u8
}

#[no_mangle]
pub unsafe extern "C" fn hlp_setup_exception(
    resolve_symbol: Option<ResolveSymbol>,
    capture_stack: Option<CaptureStack>,
) {
    RESOLVE_SYMBOL.store(
        resolve_symbol.map_or(0, |callback| callback as usize),
        Ordering::Release,
    );
    CAPTURE_STACK.store(
        capture_stack.map_or(0, |callback| callback as usize),
        Ordering::Release,
    );
}

/// Resolve an opaque `hl_symbol` returned by the raw stack APIs.
#[no_mangle]
pub unsafe extern "C" fn hlp_resolve_symbol(
    symbol: *mut c_void,
    buffer: *mut u8,
    buffer_len: *mut i32,
) -> *mut u8 {
    let callback = RESOLVE_SYMBOL.load(Ordering::Acquire);
    if callback == 0 {
        return std::ptr::null_mut();
    }
    let callback: ResolveSymbol = std::mem::transmute(callback);
    callback(symbol, buffer, buffer_len)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_exception_stack_raw(arr: *mut varray) -> i32 {
    EXCEPTION_STACK.with(|saved| {
        let saved = saved.borrow();
        if !arr.is_null() {
            let capacity = (*arr).size.max(0) as usize;
            let output = hl_aptr::<*mut c_void>(arr);
            for (index, symbol) in saved.iter().take(capacity).enumerate() {
                *output.add(index) = *symbol as *mut c_void;
            }
        }
        saved.len() as i32
    })
}

unsafe fn call_stack_raw(arr: *mut varray) -> i32 {
    let callback = CAPTURE_STACK.load(Ordering::Acquire);
    if callback == 0 {
        return 0;
    }
    let callback: CaptureStack = std::mem::transmute(callback);
    if arr.is_null() {
        callback(std::ptr::null_mut(), 0)
    } else {
        callback(hl_aptr::<*mut c_void>(arr), (*arr).size)
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_call_stack_raw(arr: *mut varray) -> i32 {
    #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
    {
        ash_call_stack_boundary(arr)
    }
    #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
    {
        call_stack_raw(arr)
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_call_stack_raw_from_frame(
    arr: *mut varray,
    frame: *mut *mut c_void,
) -> i32 {
    CALL_STACK_FRAME.with(|saved| {
        let previous = saved.replace(frame as usize);
        let result = call_stack_raw(arr);
        saved.set(previous);
        result
    })
}

#[no_mangle]
pub extern "C" fn hlp_call_stack_frame() -> *const usize {
    CALL_STACK_FRAME.with(|frame| frame.get() as *const usize)
}

unsafe fn capture_exception_stack() {
    let callback = CAPTURE_STACK.load(Ordering::Acquire);
    if callback == 0 {
        return;
    }
    let callback: CaptureStack = std::mem::transmute(callback);
    let count = callback(std::ptr::null_mut(), 0).max(0) as usize;
    let mut frames = vec![std::ptr::null_mut(); count];
    let written = if count == 0 {
        0
    } else {
        callback(frames.as_mut_ptr(), count as i32).clamp(0, count as i32) as usize
    };
    frames.truncate(written);
    EXCEPTION_STACK.with(|saved| {
        *saved.borrow_mut() = frames.into_iter().map(|frame| frame as usize).collect();
    });
}

/// `ASH_TRACE_THROW=1`: log every hlp_throw. Read once, gc.rs-style.
fn throw_trace_enabled() -> bool {
    static V: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *V.get_or_init(|| std::env::var("ASH_TRACE_THROW").is_ok())
}

unsafe fn throw_impl(v: *mut vdynamic, capture_stack: bool) {
    if capture_stack {
        capture_exception_stack();
    }
    // Trace throws only on request: an unconditional line here differs
    // between engines (the interpreter throws through its own machinery)
    // and broke every jit-vs-interp output diff that exercised exceptions.
    if throw_trace_enabled() {
        if !v.is_null() {
            let t = (*v).t;
            let kind = if !t.is_null() { (*t).kind } else { 999 };
            // A raw HBYTES throw is the runtime's own hlp_error, and the
            // message is the only thing that says which one. Without it a
            // storm of these is just a count.
            if kind == hl::hl_type_kind_HBYTES && !(*v).v.bytes.is_null() {
                let mut units = Vec::new();
                let mut p = (*v).v.bytes as *const u16;
                while *p != 0 && units.len() < 200 {
                    units.push(*p);
                    p = p.add(1);
                }
                eprintln!(
                    "[ash] hlp_throw: kind={} ptr={:p} msg={:?}",
                    kind,
                    v,
                    String::from_utf16_lossy(&units)
                );
                // The first few get a stack. A message alone says which
                // hlp_error fired, not who called it, and a storm of these
                // is only diagnosable from the caller.
                #[cfg(unix)]
                {
                    static SHOWN: std::sync::atomic::AtomicUsize =
                        std::sync::atomic::AtomicUsize::new(0);
                    if SHOWN.fetch_add(1, Ordering::Relaxed) < 5 {
                        EXCEPTION_STACK.with(|saved| {
                            for pc in saved.borrow().iter().take(12) {
                                match aot_symbol_via_dladdr(*pc) {
                                    Some(name) => eprintln!("[ash]     at {name}"),
                                    None => eprintln!("[ash]     at {pc:#x}"),
                                }
                            }
                        });
                    }
                }
            } else {
                eprintln!("[ash] hlp_throw: kind={} ptr={:p}", kind, v);
            }
        } else {
            eprintln!("[ash] hlp_throw: null");
        }
    }
    let mut buf_copy: hl::jmp_buf = mem::zeroed();
    // Read and pop the trap chain without the GC lock: it is this thread's
    // state, and a longjmp cannot leave the thread that set it up.
    let (saved_lock_depth, saved_shadow_depth) = crate::gc::with_exc(|st| {
        let current = st.current_trap;
        if throw_trace_enabled() {
            let prev = if current.is_null() {
                std::ptr::null_mut()
            } else {
                (*current).prev
            };
            eprintln!("[ash] hlp_throw chain: current={current:p} prev={prev:p} value={v:p}");
        }
        if !current.is_null() && (*current).has_jmpbuf {
            // JIT path: store exception, pop trap, longjmp back to setjmp site
            st.exc_value = v;
            let depth = (*current).saved_lock_depth;
            let shadow_depth = (*current).saved_shadow_depth;
            // Copy jmp_buf to stack BEFORE retiring the TrapContext — longjmp
            // reads from it, and a retired context may be handed straight back
            // out by the next setup_trap.
            std::ptr::copy_nonoverlapping(
                &(*current).buf as *const hl::jmp_buf,
                &mut buf_copy as *mut hl::jmp_buf,
                1,
            );
            st.current_trap = (*current).prev;
            (*current).exception_value = None;
            retire_trap(st, current);
            (depth, shadow_depth)
        } else {
            // No active setjmp trap: this is an uncaught exception. Say WHAT
            // was thrown before dying — the value is right here, and "kind=8
            // ptr=0x..." sent a real bug report back for another round trip
            // when the message string it pointed at would have named the bug.
            st.exc_value = v;
            eprintln!("[ash] uncaught exception: {}", describe_exception(v));
            eprintln!("hlp_throw called without active trap; aborting");
            std::process::abort();
        }
    });

    // The frames between the setjmp site and this longjmp are abandoned, so
    // any GcGuards they hold never run Drop. Restore the lock depth recorded
    // at trap setup (= the depth held at the setjmp site).
    crate::gc::gc_lock_unwind_to(saved_lock_depth);
    // The same frames never reach their shadow-stack pop either.
    shadow::unwind_to(saved_shadow_depth);
    // darwin and glibc export `_longjmp` (the no-signal-mask variant); MSVC's
    // setjmp.h declares only `longjmp`, so the generated bindings differ by
    // exactly this underscore per platform. Windows longjmp never touches
    // signal masks, so the two calls are the same operation.
    #[cfg(all(not(windows), not(target_family = "wasm")))]
    hl::_longjmp(buf_copy.as_mut_ptr(), 1);
    // WASI declares both spellings, and bindgen emits neither: `setjmp` there
    // is exception handling rather than a function, so the header's
    // declarations do not survive into the bindings. The symbol is real and
    // `libsetjmp` provides it, so name it directly.
    #[cfg(all(not(windows), target_family = "wasm"))]
    {
        extern "C" {
            fn longjmp(env: *mut hl::__jmp_buf_tag, val: i32) -> !;
        }
        longjmp(buf_copy.as_mut_ptr(), 1);
    }
    #[cfg(windows)]
    hl::longjmp(buf_copy.as_mut_ptr(), 1);
}

#[no_mangle]
pub unsafe extern "C" fn hlp_throw(v: *mut vdynamic) {
    throw_impl(v, true)
}

/// Rethrow the current exception without changing its value. HashLink keeps a
/// distinct entry point so stack-trace capture can distinguish the original
/// throw site; ash's stack metadata is already retained separately, while
/// trap unwinding is identical for both operations.
#[no_mangle]
pub unsafe extern "C" fn hlp_rethrow(v: *mut vdynamic) {
    throw_impl(v, false)
}

/// Arm a trap on this thread, reusing a retired context when one is available.
///
/// Every interpreter call into compiled code arms one, so this was a malloc
/// and a free per call, under the global GC lock taken twice. Both are gone:
/// the state is thread-local (a `longjmp` cannot cross threads) and the
/// contexts are pooled. Traps nest strictly, so the pool never grows past the
/// nesting depth.
pub(crate) fn setup_trap() -> *mut TrapContext {
    crate::gc::with_exc(|st| {
        let prev = st.current_trap;
        let trap_ptr = match st.trap_pool.pop() {
            Some(reused) => {
                // A reused context must look exactly like a fresh one; a stale
                // `caught` or a leftover exception would be read by the next
                // throw as though it belonged to this trap.
                unsafe {
                    *reused = TrapContext::new();
                    (*reused).prev = prev;
                }
                reused
            }
            None => {
                let mut fresh = Box::new(TrapContext::new());
                fresh.prev = prev;
                Box::into_raw(fresh)
            }
        };
        st.current_trap = trap_ptr;
        trap_ptr
    })
}

/// Pop the innermost trap and retire its context.
pub(crate) fn remove_trap() {
    crate::gc::with_exc(|st| {
        let current = st.current_trap;
        if current.is_null() {
            return;
        }
        unsafe {
            st.current_trap = (*current).prev;
            // Drop what the context owns before retiring it, so a caught
            // exception value is not kept alive by the pool.
            (*current).exception_value = None;
            retire_trap(st, current);
        }
    })
}

/// Return a context to the pool, or free it if the pool is full.
unsafe fn retire_trap(st: &mut crate::gc::ExcState, trap: *mut TrapContext) {
    if st.trap_pool.len() < 64 {
        st.trap_pool.push(trap);
    } else {
        drop(Box::from_raw(trap));
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_setup_trap_jit() -> *mut c_void {
    // Depth held by this thread OUTSIDE this call — i.e. at the setjmp site
    // the caller (JIT code) is about to establish.
    let outer_depth = crate::gc::gc_lock_held_depth();
    let trap = setup_trap();
    (*trap).has_jmpbuf = true;
    (*trap).saved_lock_depth = outer_depth;
    (*trap).saved_shadow_depth = shadow::depth();
    (*trap).buf.as_mut_ptr().cast()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_remove_trap_jit() {
    remove_trap();
}

#[no_mangle]
pub unsafe extern "C" fn hlp_get_exc_value() -> *mut vdynamic {
    crate::gc::with_exc(|st| {
        if throw_trace_enabled() {
            eprintln!(
                "[ash] get_exc: current={:p} value={:p}",
                st.current_trap, st.exc_value
            );
        }
        st.exc_value
    })
}

#[no_mangle]
pub unsafe extern "C" fn hlp_clear_exc_value() {
    crate::gc::with_exc(|st| {
        if throw_trace_enabled() {
            eprintln!(
                "[ash] clear_exc: current={:p} value={:p}",
                st.current_trap, st.exc_value
            );
        }
        st.exc_value = std::ptr::null_mut();
    });
}

#[no_mangle]
pub unsafe extern "C" fn hlp_error(msg: *const uchar, mut _args: ...) {
    let d = crate::gc::gc_locked()
        .allocate(mem::size_of::<hl::vdynamic>())
        .unwrap()
        .as_ptr() as *mut vdynamic;
    (*d).v.bytes = msg as *mut u8;
    (*d).t = crate::types::hlt_bytes();

    hlp_throw(d)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_set_error_handler(handler: *mut vclosure) {
    let mut gc = crate::gc::gc_locked();

    gc.set_exception_handler(Box::new(move |exp: &mut HLException| {
        let gc = crate::gc::gc_locked();
        let mut value = exp.value;
        gc.run_with_trap(move || hlp_dyn_call(handler, &mut value, 1))
    }));
}
