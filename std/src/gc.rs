use crate::error::{HLException, TrapContext, VDynamicException};
use crate::hl::{self, hl_type, hl_type_obj, vclosure, vdynamic, HL_WSIZE};
use std::cell::{Cell, RefCell};
use std::ptr::{self, NonNull};
use std::rc::Rc;
use std::sync::OnceLock;
use std::{collections::HashSet, mem};
use anyhow::Result;

const HEAP_SIZE: usize = 1024 * 1024 * 100; // 100 MB
const BLOCK_SIZE: usize = 32 * 1024; // 32 KB
const LINE_SIZE: usize = 128; // 128 bytes
const LINES_PER_BLOCK: usize = BLOCK_SIZE / LINE_SIZE;

pub static mut GC: OnceLock<ImmixAllocator> = OnceLock::new();
pub static HL_GLOBAL_LOCK: OnceLock<std::sync::Mutex<()>> = OnceLock::new();

struct ImmixHeap {
    memory: Box<[u8; HEAP_SIZE]>,
    free_blocks: Vec<usize>,
    used_blocks: HashSet<usize>,
    allocation_point: usize,
}
#[derive(Debug, Clone)]
struct Block {
    mark_bits: [bool; LINES_PER_BLOCK],
    evacuation_candidate: bool,
}

struct RootSet {
    globals: Vec<*mut hl::vdynamic>,
    stack_roots: Vec<*mut hl::vdynamic>,
    persistent_roots: HashSet<*mut hl::vdynamic>,
}

pub struct ImmixAllocator {
    heap: ImmixHeap,
    blocks: Vec<Block>,
    roots: Rc<RefCell<RootSet>>,
    pub(crate) current_exception: Option<Box<HLException>>,
    pub(crate) exception_handler: Option<Box<dyn Fn(&mut HLException) -> Result<*mut vdynamic, VDynamicException>>>,
    pub(crate) current_trap: RefCell<*mut TrapContext>,
    pub(crate) exc_value: RefCell<*mut vdynamic>,
}

impl ImmixAllocator {
    pub fn new() -> Self {
        let mut heap = ImmixHeap {
            memory: Box::new([0; HEAP_SIZE]),
            free_blocks: Vec::new(),
            used_blocks: HashSet::new(),
            allocation_point: 0,
        };

        for i in (0..HEAP_SIZE).step_by(BLOCK_SIZE) {
            heap.free_blocks.push(i);
        }

        let blocks = vec![
            Block {
                mark_bits: [false; LINES_PER_BLOCK],
                evacuation_candidate: false,
            };
            HEAP_SIZE / BLOCK_SIZE
        ];

        ImmixAllocator {
            heap,
            blocks,
            roots: Rc::new(RefCell::new(RootSet {
                globals: Vec::new(),
                stack_roots: Vec::new(),
                persistent_roots: HashSet::new(),
            })),
            current_exception: None,
            exception_handler: None,
            current_trap: RefCell::new(std::ptr::null_mut()),
            exc_value: RefCell::new(std::ptr::null_mut()),
        }
    }

    pub fn allocate(&mut self, size: usize) -> Option<NonNull<u8>> {
        if size > LINE_SIZE {
            return self.allocate_large(size);
        }

        if self.heap.allocation_point == 0 || self.heap.allocation_point + size > BLOCK_SIZE {
            if let Some(new_block) = self.heap.free_blocks.pop() {
                self.heap.used_blocks.insert(new_block);
                self.heap.allocation_point = new_block;
            } else {
                self.collect_garbage();
                if self.heap.free_blocks.is_empty() {
                    return None; // Out of memory
                }
                let new_block = self.heap.free_blocks.pop().unwrap();
                self.heap.used_blocks.insert(new_block);
                self.heap.allocation_point = new_block;
            }
        }

        let result = unsafe {
            NonNull::new_unchecked(
                self.heap
                    .memory
                    .as_mut_ptr()
                    .add(self.heap.allocation_point),
            )
        };
        self.heap.allocation_point += size;
        Some(result)
    }

    pub fn allocate_large(&mut self, size: usize) -> Option<NonNull<u8>> {
        let blocks_needed = (size + BLOCK_SIZE - 1) / BLOCK_SIZE;
        let mut start_block = None;

        for _ in 0..blocks_needed {
            if let Some(block) = self.heap.free_blocks.pop() {
                if start_block.is_none() {
                    start_block = Some(block);
                }
                self.heap.used_blocks.insert(block);
            } else {
                // Not enough contiguous blocks, return allocated blocks and trigger GC
                if let Some(start) = start_block {
                    for i in 0..blocks_needed {
                        if let Some(block) = self.heap.used_blocks.take(&(start + i * BLOCK_SIZE)) {
                            self.heap.free_blocks.push(block);
                        }
                    }
                }
                self.collect_garbage();
                return self.allocate_large(size); // Try again after GC
            }
        }

        start_block.map(|block| unsafe {
            NonNull::new_unchecked(self.heap.memory.as_mut_ptr().add(block))
        })
    }

    pub unsafe fn allocate_closure_ptr(
        &mut self,
        t: *mut hl_type,
        fun: *mut std::ffi::c_void,
        ptr: *mut std::ffi::c_void,
    ) -> *mut vclosure {
        // Allocate memory for the closure
        let closure = self
            .allocate(mem::size_of::<vclosure>())
            .expect("Failed to allocate memory for closure")
            .as_ptr() as *mut vclosure;

        let stack = 0;

        // Initialize the closure fields
        ptr::write(
            closure,
            vclosure {
                t: t,
                fun: fun,
                hasValue: 1,
                stackCount: stack,
                value: ptr,
            },
        );

        closure
    }

    pub unsafe fn is_gc_ptr<T>(&self, ptr: *const T) -> bool {
        // Cast the pointer to a usize for address arithmetic
        let addr = ptr as usize;

        // Check if the address is within the heap
        if addr < self.heap.memory.as_ptr() as usize
            || addr >= (self.heap.memory.as_ptr() as usize + HEAP_SIZE)
        {
            return false;
        }

        // Calculate the block index
        let block_index = (addr - self.heap.memory.as_ptr() as usize) / BLOCK_SIZE;

        // Check if the block is in use
        if !self.heap.used_blocks.contains(&(block_index * BLOCK_SIZE)) {
            return false;
        }

        // Calculate the line index within the block
        let line_index = ((addr % BLOCK_SIZE) / LINE_SIZE) as usize;

        // Check if the line is marked (i.e., in use)
        if !self.blocks[block_index].mark_bits[line_index] {
            return false;
        }

        // If it's a vdynamic pointer, we need to check its internal pointer as well
        if std::mem::size_of::<T>() == std::mem::size_of::<hl::vdynamic>() {
            // Safety: We've already checked that this pointer is within our heap
            let vd = unsafe { &*(ptr as *const hl::vdynamic) };

            // Check the type pointer
            if !vd.t.is_null() && !self.is_gc_ptr(vd.t) {
                return false;
            }

            // Check the value pointer for certain types
            match unsafe { (*vd.t).kind } {
                hl::hl_type_kind_HOBJ
                | hl::hl_type_kind_HFUN
                | hl::hl_type_kind_HARRAY
                | hl::hl_type_kind_HVIRTUAL
                | hl::hl_type_kind_HDYNOBJ
                | hl::hl_type_kind_HBYTES => {
                    if !self.is_gc_ptr(vd.v.ptr) {
                        return false;
                    }
                }
                _ => {} // Other types don't have additional pointers to check
            }
        }

        true
    }

    pub fn collect_garbage(&mut self) {
        self.mark_roots();
        self.sweep();
    }

    pub fn mark_roots(&mut self) {
        let roots = self.roots.clone();
        let root_set = roots.borrow();

        // Mark global variables
        for &global_ptr in &root_set.globals {
            self.mark_vdynamic(global_ptr);
        }

        // Mark stack-allocated variables
        for &stack_ptr in &root_set.stack_roots {
            self.mark_vdynamic(stack_ptr);
        }

        // Mark persistent roots (e.g., long-lived objects, caches)
        for &persistent_ptr in &root_set.persistent_roots {
            self.mark_vdynamic(persistent_ptr);
        }

        // Mark the current exception, if any
        self.mark_exception();

        // // Mark hl_hb_map objects
        // self.mark_hb_maps();

        // // Mark rnd objects
        // self.mark_rnd_objects();
    }

    pub fn mark_memory(&mut self, ptr: *mut u8, size: usize) {
        let heap_start = self.heap.memory.as_ptr() as usize;
        let heap_end = heap_start + HEAP_SIZE;
        let addr = ptr as usize;

        // Only mark memory within the heap range
        if addr < heap_start || addr >= heap_end {
            return;
        }

        let end_addr = (addr + size).min(heap_end);
        let mut current_addr = addr;

        while current_addr < end_addr {
            let offset = current_addr - heap_start;
            let block_index = offset / BLOCK_SIZE;
            let line_index = (offset % BLOCK_SIZE) / LINE_SIZE;

            if block_index < self.blocks.len() {
                self.blocks[block_index].mark_bits[line_index] = true;
            }

            current_addr += LINE_SIZE;
        }
    }

    pub fn mark_object(&mut self, ptr: *mut hl::hl_type) {
        if ptr.is_null() {
            return;
        }

        let heap_start = self.heap.memory.as_ptr() as usize;
        let addr = ptr as usize;

        // Only mark objects within the heap
        if addr < heap_start || addr >= heap_start + HEAP_SIZE {
            return;
        }

        let offset = addr - heap_start;
        let block_index = offset / BLOCK_SIZE;
        let line_index = (offset % BLOCK_SIZE) / LINE_SIZE;

        if block_index < self.blocks.len() && !self.blocks[block_index].mark_bits[line_index] {
            self.blocks[block_index].mark_bits[line_index] = true;

            // Mark children based on the type of object
            unsafe {
                match (*ptr).kind {
                    hl::hl_type_kind_HOBJ => {
                        let obj_ptr = (*ptr).__bindgen_anon_1.obj;
                        if !obj_ptr.is_null() {
                            let obj: &hl_type_obj = &*obj_ptr;
                            for i in 0..obj.nfields as usize {
                                if !obj.fields.is_null() {
                                    let field = &*obj.fields.add(i);
                                    self.mark_object(field.t);
                                }
                            }
                            if !obj.super_.is_null() {
                                self.mark_object(obj.super_);
                            }
                        }
                    }
                    hl::hl_type_kind_HFUN => {
                        let fun_ptr = (*ptr).__bindgen_anon_1.fun;
                        if !fun_ptr.is_null() {
                            let fun = &*fun_ptr;
                            for i in 0..fun.nargs as usize {
                                if !fun.args.is_null() {
                                    let arg = *fun.args.add(i);
                                    self.mark_object(arg);
                                }
                            }
                            if !fun.ret.is_null() {
                                self.mark_object(fun.ret);
                            }
                        }
                    }
                    hl::hl_type_kind_HENUM => {
                        let enum_ptr = (*ptr).__bindgen_anon_1.tenum;
                        if !enum_ptr.is_null() {
                            let enum_ = &*enum_ptr;
                            for i in 0..enum_.nconstructs as usize {
                                if !enum_.constructs.is_null() {
                                    let construct = &*enum_.constructs.add(i);
                                    for j in 0..construct.nparams as usize {
                                        if !construct.params.is_null() {
                                            let param = *construct.params.add(j);
                                            self.mark_object(param);
                                        }
                                    }
                                }
                            }
                        }
                    }
                    hl::hl_type_kind_HNULL => {
                        let inner_type = (*ptr).__bindgen_anon_1.tparam;
                        if !inner_type.is_null() {
                            self.mark_object(inner_type);
                        }
                    }
                    _ => {} // Other types might not have child pointers
                }
            }
        }
    }

    pub fn mark_vdynamic(&mut self, vd_ptr: *mut hl::vdynamic) {
        if vd_ptr.is_null() {
            return;
        }

        unsafe {
            let vd = &*vd_ptr;
            self.mark_memory(vd_ptr as *mut u8, mem::size_of::<hl::vdynamic>());

            // Mark the type
            if !vd.t.is_null() {
                self.mark_object(vd.t);
            }

            // Depending on the type, we might need to mark more data
            // Mark the value based on its type
            match vd.t.as_ref().unwrap().kind {
                hl::hl_type_kind_HOBJ => {
                    let obj_ptr = vd.v.ptr as *mut hl::vobj;
                    if !obj_ptr.is_null() {
                        self.mark_object((*obj_ptr).t);
                    }
                }
                hl::hl_type_kind_HFUN => {
                    let fun_ptr = vd.v.ptr as *mut hl::vclosure;
                    if !fun_ptr.is_null() {
                        self.mark_object((*fun_ptr).t);
                        // Mark the function value and environment if present
                        if !(*fun_ptr).fun.is_null() {
                            self.mark_memory(
                                (*fun_ptr).fun as *mut u8,
                                mem::size_of::<*mut ::std::os::raw::c_void>(),
                            );
                        }
                        if (*fun_ptr).hasValue != 0 && !(*fun_ptr).value.is_null() {
                            self.mark_vdynamic((*fun_ptr).value as *mut hl::vdynamic);
                        }
                    }
                }
                hl::hl_type_kind_HARRAY => {
                    let array_ptr = vd.v.ptr as *mut hl::varray;
                    if !array_ptr.is_null() {
                        self.mark_object((*array_ptr).t);
                        // Mark array elements
                        let size = (*array_ptr).size;
                        for i in 0..size {
                            let element_ptr = (*array_ptr).at.add(i as usize);
                            self.mark_vdynamic(element_ptr as *mut hl::vdynamic);
                        }
                    }
                }
                // Add more cases for other types as needed
                _ => {}
            }
        }
    }

    pub fn sweep(&mut self) {
        for (block_index, block) in self.blocks.iter_mut().enumerate() {
            let mut is_empty = true;
            for (line_index, &is_marked) in block.mark_bits.clone().iter().enumerate() {
                if is_marked {
                    is_empty = false;
                    block.mark_bits[line_index] = false; // Reset for next GC cycle
                } else {
                    let start_addr = block_index * BLOCK_SIZE + line_index * LINE_SIZE;
                    unsafe {
                        std::ptr::write_bytes(
                            self.heap.memory.as_mut_ptr().add(start_addr),
                            0,
                            LINE_SIZE,
                        );
                    }
                }
            }

            if is_empty {
                let block_addr = block_index * BLOCK_SIZE;
                self.heap.used_blocks.remove(&block_addr);
                self.heap.free_blocks.push(block_addr);
            } else {
                block.evacuation_candidate =
                    block.mark_bits.iter().filter(|&&x| x).count() < LINES_PER_BLOCK / 2;
            }
        }
    }

    pub fn register_global(&mut self, ptr: *mut hl::vdynamic) {
        self.roots.borrow_mut().globals.push(ptr);
    }

    pub fn push_stack_root(&mut self, ptr: *mut hl::vdynamic) {
        self.roots.borrow_mut().stack_roots.push(ptr);
    }

    pub fn pop_stack_root(&mut self) {
        self.roots.borrow_mut().stack_roots.pop();
    }

    pub fn register_persistent(&mut self, ptr: *mut hl::vdynamic) {
        self.roots.borrow_mut().persistent_roots.insert(ptr);
    }

    pub fn unregister_persistent(&mut self, ptr: *mut hl::vdynamic) {
        self.roots.borrow_mut().persistent_roots.remove(&ptr);
    }


    pub fn alloc_virtual(&mut self, t: *mut hl::hl_type) -> Option<NonNull<hl::vvirtual>> {
        unsafe {
            let virt = (*t).__bindgen_anon_1.virt;
            if virt.is_null() {
                return None;
            }

            let data_size = (*virt).dataSize;
            let nfields = (*virt).nfields;
            let total_size = std::mem::size_of::<hl::vvirtual>() + 
                             (nfields as usize * std::mem::size_of::<*mut std::os::raw::c_void>()) + 
                             (data_size as usize);

            let ptr = self.allocate(total_size)?;
            let v = ptr.as_ptr() as *mut hl::vvirtual;

            // Initialize vvirtual struct
            (*v).t = t;
            (*v).value = std::ptr::null_mut();
            (*v).next = std::ptr::null_mut();

            // Calculate pointers to fields and vdata
            let fields = v.offset(1) as *mut *mut std::os::raw::c_void;
            let vdata = fields.add(nfields as usize) as *mut u8;

            // Initialize fields
            for i in 0..nfields as usize {
                *fields.add(i) = (v as *mut u8).add((*virt).indexes.add(i) as usize) as *mut std::os::raw::c_void;
            }

            // Zero out vdata
            std::ptr::write_bytes(vdata, 0, data_size as usize);

            Some(NonNull::new_unchecked(v))
        }
    }
}


#[no_mangle]
pub unsafe extern "C" fn hlp_zalloc(size: i32) -> *mut std::os::raw::c_void {
    if size < 0 {
        return ptr::null_mut();
    }

    let size_usize = size as usize;

    match GC
        .get_mut()
        .expect("expected to call garbage collector")
        .allocate(size_usize)
    {
        Some(ptr) => {
            // Zero out the allocated memory
            ptr::write_bytes(ptr.as_ptr() as *mut u8, 0, size_usize);
            ptr.as_ptr() as *mut std::os::raw::c_void
        }
        None => ptr::null_mut(),
    }
}

#[no_mangle]
pub extern "C" fn hlp_mark_size(data_size: i32) -> i32 {
    let data_size = data_size as usize;
    let ptr_count = (data_size + HL_WSIZE as usize - 1) / HL_WSIZE as usize;
    (((ptr_count + 31) >> 5) * std::mem::size_of::<i32>() as usize)
        .try_into()
        .unwrap()
}

/// Initialize the garbage collector. Must be called before any allocation.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_init() {
    GC.get_mut_or_init(|| ImmixAllocator::new());
}
