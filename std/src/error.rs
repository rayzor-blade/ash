use crate::array::hlp_alloc_array;
use crate::fun::hlp_dyn_call;
use crate::gc::{ImmixAllocator, GC};
use crate::hl::{
    self, hl_type, hl_type__bindgen_ty_1, hl_type_kind_HBYTES, uchar, varray, vbyte, vclosure,
    vdynamic,
};
use crate::strings::str_to_uchar_ptr;
use crate::types::hl_aptr;
use anyhow::Result;
use std::fmt::{self, Formatter};
use std::mem;
use std::os::raw::c_int;
use std::panic;

extern "C" {
    fn _longjmp(buf: *mut c_int, val: c_int) -> !;
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

pub struct TrapContext {
    pub buf: [c_int; 48],
    pub has_jmpbuf: bool,
    pub prev: *mut TrapContext,
    pub exception_value: Option<VDynamicException>,
    pub caught: bool,
}

impl TrapContext {
    pub fn new() -> Self {
        TrapContext {
            buf: [0; 48],
            has_jmpbuf: false,
            prev: std::ptr::null_mut(),
            exception_value: None,
            caught: false,
        }
    }
}

impl ImmixAllocator {
    pub fn setup_trap(&self) -> *mut TrapContext {
        let mut new_trap = Box::new(TrapContext::new());
        new_trap.prev = self.current_trap.borrow().clone();
        let trap_ptr = Box::into_raw(new_trap);
        *self.current_trap.borrow_mut() = trap_ptr;
        trap_ptr
    }

    pub fn remove_trap(&self) {
        let current = *self.current_trap.borrow();
        if !current.is_null() {
            unsafe {
                *self.current_trap.borrow_mut() = (*current).prev;
                let _ = Box::from_raw(current);
            }
        }
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
    let gc = GC.get_mut().expect("expected to get GC");

    if let Some(exception) = gc.get_current_exception() {
        let stack_trace = &*exception.stack_trace;
        let frame_count = stack_trace.frames.len();

        // Allocate a varray to hold the stack frames
        let varray_ptr: *mut varray = hlp_alloc_array(crate::types::hlt_bytes(), frame_count as i32);

        // Fill the array with stack frame information
        for (i, frame) in stack_trace.frames.iter().enumerate() {
            *((hl_aptr(varray_ptr) as *mut *const vbyte).add(i as usize)) =
                str_to_uchar_ptr(&format!(
                    "{}:{} {}",
                    frame.file_name, frame.line_number, frame.function_name
                )) as *const vbyte;
        }

        varray_ptr
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_exception_stack_raw(arr: *mut varray) -> i32 {
    unimplemented!()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_call_stack_raw(arr: *mut varray) -> i32 {
    unimplemented!()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_throw(v: *mut vdynamic) {
    let gc = GC.get_mut().expect("expected GC");
    let current = *gc.current_trap.borrow();

    if !current.is_null() && (*current).has_jmpbuf {
        // JIT path: store exception, pop trap, longjmp back to setjmp site
        *gc.exc_value.borrow_mut() = v;
        // Copy jmp_buf to stack BEFORE freeing the TrapContext — longjmp reads from it
        let mut buf_copy: [c_int; 48] = [0; 48];
        buf_copy.copy_from_slice(&(*current).buf);
        *gc.current_trap.borrow_mut() = (*current).prev;
        let _ = Box::from_raw(current);
        _longjmp(buf_copy.as_mut_ptr(), 1);
    } else {
        // Interpreter path: use Rust panic
        gc.throw(VDynamicException(Box::from_raw(v)));
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_setup_trap_jit() -> *mut c_int {
    let gc = GC.get_mut().expect("expected GC");
    let trap = gc.setup_trap();
    (*trap).has_jmpbuf = true;
    (*trap).buf.as_mut_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_remove_trap_jit() {
    let gc = GC.get_mut().expect("expected GC");
    gc.remove_trap();
}

#[no_mangle]
pub unsafe extern "C" fn hlp_get_exc_value() -> *mut vdynamic {
    let gc = GC.get_mut().expect("expected GC");
    *gc.exc_value.borrow()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_error(msg: *const uchar, mut _args: ...) {
    let d = GC
        .get_mut()
        .expect("expeted to call GC")
        .allocate(mem::size_of::<hl::vdynamic>())
        .unwrap()
        .as_ptr() as *mut vdynamic;
    (*d).v.bytes = msg as *mut u8;
    (*d).t = crate::types::hlt_bytes();

    hlp_throw(d)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_set_error_handler(handler: *mut vclosure) {
    let gc = GC.get_mut().expect("expeted to call GC");

    gc.set_exception_handler(Box::new(move |exp: &mut HLException| {
        let gc = GC.get_mut().expect("expeted to call GC");
        let mut value = exp.value.clone();
        gc.run_with_trap(move || hlp_dyn_call(handler, &mut value, 1))
    }));
}
