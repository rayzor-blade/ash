use crate::error::{HLException, TrapContext, VDynamicException};
use crate::hl::{self, hl_type, hl_type_obj, vclosure, vdynamic, HL_WSIZE};
use std::cell::{Cell, RefCell};
use std::os::raw::c_void;
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
    current_block_end: usize,
    alloc_count: usize,
    /// For each line in the heap, stores the number of lines this allocation
    /// occupies if this is an allocation start, or 0 for continuation lines.
    /// Enables the GC to mark all lines of a multi-line object.
    alloc_sizes: Vec<u32>,
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
    stack_top: usize,
    globals_range: Option<(*const *mut c_void, usize)>,
}

impl ImmixAllocator {
    pub fn new() -> Self {
        let mut heap = ImmixHeap {
            memory: Box::new([0; HEAP_SIZE]),
            free_blocks: Vec::new(),
            used_blocks: HashSet::new(),
            allocation_point: 0,
            current_block_end: 0,
            alloc_count: 0,
            alloc_sizes: vec![0u32; HEAP_SIZE / LINE_SIZE],
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
            stack_top: 0,
            globals_range: None,
        }
    }

    pub fn allocate(&mut self, size: usize) -> Option<NonNull<u8>> {
        let size = size.max(8);
        // Round up to LINE_SIZE (128 bytes) — Immix line-granularity allocation.
        // Each object gets its own line(s), preventing cross-object corruption.
        let aligned_size = (size + LINE_SIZE - 1) & !(LINE_SIZE - 1);

        if aligned_size > BLOCK_SIZE {
            return self.allocate_large(size);
        }

        let needs_new_block = self.heap.allocation_point + aligned_size > self.heap.current_block_end;

        if needs_new_block {
            if let Some(new_block) = self.heap.free_blocks.pop() {
                self.heap.used_blocks.insert(new_block);
                self.heap.allocation_point = new_block;
                self.heap.current_block_end = new_block + BLOCK_SIZE;
            } else {
                self.collect_garbage();
                if self.heap.free_blocks.is_empty() {
                    return None; // Out of memory
                }
                let new_block = self.heap.free_blocks.pop().unwrap();
                self.heap.used_blocks.insert(new_block);
                self.heap.allocation_point = new_block;
                self.heap.current_block_end = new_block + BLOCK_SIZE;
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

        // Record allocation size in line table for GC multi-line marking
        let start_line = self.heap.allocation_point / LINE_SIZE;
        let num_lines = aligned_size / LINE_SIZE;
        self.heap.alloc_sizes[start_line] = num_lines as u32;
        for i in 1..num_lines {
            self.heap.alloc_sizes[start_line + i] = 0;
        }

        self.heap.allocation_point += aligned_size;
        self.heap.alloc_count += 1;
        Some(result)
    }

    pub fn allocate_large(&mut self, size: usize) -> Option<NonNull<u8>> {
        let blocks_needed = (size + BLOCK_SIZE - 1) / BLOCK_SIZE;
        // Find contiguous free blocks by sorting the free list and scanning for a run.
        self.heap.free_blocks.sort_unstable();

        let mut run_start = None;
        let mut run_len = 0;
        for i in 0..self.heap.free_blocks.len() {
            let block = self.heap.free_blocks[i];
            if run_len == 0 {
                run_start = Some(i);
                run_len = 1;
            } else {
                let prev = self.heap.free_blocks[i - 1];
                if block == prev + BLOCK_SIZE {
                    run_len += 1;
                } else {
                    run_start = Some(i);
                    run_len = 1;
                }
            }
            if run_len >= blocks_needed {
                // Found a contiguous run — remove these blocks from free list
                let start_idx = run_start.unwrap();
                let start_addr = self.heap.free_blocks[start_idx];
                let removed: Vec<usize> = self.heap.free_blocks
                    .drain(start_idx..start_idx + blocks_needed)
                    .collect();
                for block in removed {
                    self.heap.used_blocks.insert(block);
                }
                // Record allocation size for GC multi-line marking
                let num_lines = (size + LINE_SIZE - 1) / LINE_SIZE;
                let start_line = start_addr / LINE_SIZE;
                self.heap.alloc_sizes[start_line] = num_lines as u32;
                for j in 1..num_lines {
                    self.heap.alloc_sizes[start_line + j] = 0;
                }
                return Some(unsafe {
                    NonNull::new_unchecked(self.heap.memory.as_mut_ptr().add(start_addr))
                });
            }
        }

        // No contiguous run found — trigger GC and retry
        self.collect_garbage();
        if self.heap.free_blocks.len() >= blocks_needed {
            return self.allocate_large(size);
        }
        None // Out of memory
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

    /// Mark all lines belonging to the allocation that contains `line`.
    /// Walks backwards to find the allocation start (line with alloc_sizes > 0),
    /// then marks all lines from start to start+size.
    /// Returns newly-marked (block_idx, line_idx) pairs.
    fn mark_allocation_at_line(&mut self, line: usize) -> Vec<(usize, usize)> {
        // Find allocation start by walking backwards
        let mut start = line;
        while start > 0 && self.heap.alloc_sizes[start] == 0 {
            start -= 1;
        }
        let num_lines = self.heap.alloc_sizes[start] as usize;
        let num_lines = if num_lines == 0 { 1 } else { num_lines };

        let mut newly_marked = Vec::new();
        for l in start..start + num_lines {
            let block_idx = l / LINES_PER_BLOCK;
            let line_idx = l % LINES_PER_BLOCK;
            if block_idx < self.blocks.len() && !self.blocks[block_idx].mark_bits[line_idx] {
                self.blocks[block_idx].mark_bits[line_idx] = true;
                newly_marked.push((block_idx, line_idx));
            }
        }
        newly_marked
    }

    /// Conservative mark: scan a memory range for values that look like heap pointers.
    /// For each match, mark ALL lines of the containing allocation.
    /// Returns list of newly-marked (block, line) pairs.
    fn conservative_scan_range(&mut self, start: usize, end: usize) -> Vec<(usize, usize)> {
        let heap_start = self.heap.memory.as_ptr() as usize;
        let heap_end = heap_start + HEAP_SIZE;
        let mut newly_marked = Vec::new();

        let mut addr = start;
        while addr + 8 <= end {
            let val = unsafe { *(addr as *const usize) };
            if val >= heap_start && val < heap_end {
                let offset = val - heap_start;
                let line = offset / LINE_SIZE;
                let block_idx = line / LINES_PER_BLOCK;
                let line_idx = line % LINES_PER_BLOCK;
                // Only process if the pointed-to line isn't already marked
                if block_idx < self.blocks.len() && !self.blocks[block_idx].mark_bits[line_idx] {
                    let alloc_marks = self.mark_allocation_at_line(line);
                    newly_marked.extend(alloc_marks);
                }
            }
            addr += 8;
        }
        newly_marked
    }

    /// Transitively scan all newly-marked heap lines for more heap pointers.
    /// When a new heap pointer is found, marks ALL lines of that allocation.
    fn conservative_trace(&mut self, initial: Vec<(usize, usize)>) {
        let heap_start = self.heap.memory.as_ptr() as usize;
        let heap_end = heap_start + HEAP_SIZE;
        let mut worklist = initial;

        while let Some((block_idx, line_idx)) = worklist.pop() {
            let line_start = heap_start + block_idx * BLOCK_SIZE + line_idx * LINE_SIZE;
            for off in (0..LINE_SIZE).step_by(8) {
                let val = unsafe { *((line_start + off) as *const usize) };
                if val >= heap_start && val < heap_end {
                    let offset = val - heap_start;
                    let child_line = offset / LINE_SIZE;
                    let child_block_idx = child_line / LINES_PER_BLOCK;
                    let child_line_idx = child_line % LINES_PER_BLOCK;
                    if child_block_idx < self.blocks.len() && !self.blocks[child_block_idx].mark_bits[child_line_idx] {
                        let alloc_marks = self.mark_allocation_at_line(child_line);
                        worklist.extend(alloc_marks);
                    }
                }
            }
        }
    }

    pub fn collect_garbage(&mut self) {
        let used_before = self.heap.used_blocks.len();
        self.mark_roots();
        self.sweep();
        let freed = self.heap.free_blocks.len();
        let retained = self.heap.used_blocks.len();
        eprintln!("[GC] collected: freed={freed} retained={retained} (was {used_before} used)");

        self.heap.alloc_count = 0;
        // Reset so next allocation picks a fresh free block
        self.heap.allocation_point = 0;
        self.heap.current_block_end = 0;
    }

    pub fn mark_roots(&mut self) {
        let roots = self.roots.clone();
        let root_set = roots.borrow();

        // Mark explicit roots using conservative approach:
        // Just mark the memory lines, then conservative_trace will follow pointers.
        let heap_start = self.heap.memory.as_ptr() as usize;
        let heap_end = heap_start + HEAP_SIZE;
        let mut all_newly_marked = Vec::new();

        for &global_ptr in &root_set.globals {
            let addr = global_ptr as usize;
            if addr >= heap_start && addr < heap_end {
                let line = (addr - heap_start) / LINE_SIZE;
                all_newly_marked.extend(self.mark_allocation_at_line(line));
            }
        }
        for &stack_ptr in &root_set.stack_roots {
            let addr = stack_ptr as usize;
            if addr >= heap_start && addr < heap_end {
                let line = (addr - heap_start) / LINE_SIZE;
                all_newly_marked.extend(self.mark_allocation_at_line(line));
            }
        }
        for &persistent_ptr in &root_set.persistent_roots {
            let addr = persistent_ptr as usize;
            if addr >= heap_start && addr < heap_end {
                let line = (addr - heap_start) / LINE_SIZE;
                all_newly_marked.extend(self.mark_allocation_at_line(line));
            }
        }
        drop(root_set);

        // Conservative scan of globals_data
        if let Some((globals_ptr, count)) = self.globals_range {
            let start = globals_ptr as usize;
            let end = start + count * 8;
            let newly_marked = self.conservative_scan_range(start, end);
            all_newly_marked.extend(newly_marked);
        }

        // Conservative scan of thread stack
        if self.stack_top > 0 {
            let sp: usize;
            unsafe { core::arch::asm!("mov {}, sp", out(reg) sp); }
            // Stack grows downward on ARM64: SP < stack_top
            if sp < self.stack_top {
                let newly_marked = self.conservative_scan_range(sp, self.stack_top);
                all_newly_marked.extend(newly_marked);
            }
        }

        // Transitive conservative marking
        if !all_newly_marked.is_empty() {
            self.conservative_trace(all_newly_marked);
        }
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

        // Only dereference pointers within the GC heap
        let heap_start = self.heap.memory.as_ptr() as usize;
        let heap_end = heap_start + HEAP_SIZE;
        let addr = vd_ptr as usize;
        if addr < heap_start || addr >= heap_end {
            return; // Not a GC-managed pointer, skip
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
            if vd.t.is_null() {
                return;
            }
            match (*vd.t).kind {
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
        // Block-level collection: only reclaim entirely empty blocks.
        // Partially-occupied blocks are retained intact — we do NOT zero individual
        // dead lines, because conservative marking may miss some live objects whose
        // data would be destroyed by zeroing.
        let used_block_addrs: Vec<usize> = self.heap.used_blocks.iter().copied().collect();
        for block_addr in used_block_addrs {
            let block_index = block_addr / BLOCK_SIZE;
            let block = &mut self.blocks[block_index];
            let mut is_empty = true;
            for line_index in 0..LINES_PER_BLOCK {
                if block.mark_bits[line_index] {
                    is_empty = false;
                }
                block.mark_bits[line_index] = false; // Reset for next GC cycle
            }

            if is_empty {
                self.heap.used_blocks.remove(&block_addr);
                self.heap.free_blocks.push(block_addr);
                // Clear alloc_sizes for all lines in this freed block
                let base_line = block_index * LINES_PER_BLOCK;
                for l in base_line..base_line + LINES_PER_BLOCK {
                    self.heap.alloc_sizes[l] = 0;
                }
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

/// Record the stack top for conservative scanning.
/// Called once at JIT entry before running user code.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_set_stack_top(top: usize) {
    let gc = GC.get_mut().expect("expected GC");
    gc.stack_top = top;
}

/// Register the globals_data array for conservative scanning.
/// Called after init_constants with pointer to globals array and count.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_set_globals(ptr: *const *mut c_void, count: usize) {
    let gc = GC.get_mut().expect("expected GC");
    gc.globals_range = Some((ptr, count));
}
