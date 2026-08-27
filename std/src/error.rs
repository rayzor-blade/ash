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
    fn hlp_call_stack_raw(arr: *mut varray) -> i32;
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[used]
static KEEP_CALL_STACK_BOUNDARY: unsafe extern "C" fn(*mut varray) -> i32 = hlp_call_stack_raw;

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

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[no_mangle]
pub unsafe extern "C" fn hlp_call_stack_raw(arr: *mut varray) -> i32 {
    call_stack_raw(arr)
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
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
            eprintln!("[ash] hlp_throw: kind={} ptr={:p}", kind, v);
        } else {
            eprintln!("[ash] hlp_throw: null");
        }
    }
    let mut buf_copy: hl::jmp_buf = mem::zeroed();
    // Read and pop the trap chain without the GC lock: it is this thread's
    // state, and a longjmp cannot leave the thread that set it up.
    let saved_lock_depth = crate::gc::with_exc(|st| {
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
            depth
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
    // darwin and glibc export `_longjmp` (the no-signal-mask variant); MSVC's
    // setjmp.h declares only `longjmp`, so the generated bindings differ by
    // exactly this underscore per platform. Windows longjmp never touches
    // signal masks, so the two calls are the same operation.
    #[cfg(not(windows))]
    hl::_longjmp(buf_copy.as_mut_ptr(), 1);
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
