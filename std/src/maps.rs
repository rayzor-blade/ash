use std::{
    mem,
    os::raw::c_void,
    ptr::{self, NonNull},
};

use crate::{
    gc::ImmixAllocator,
    hl::{self, hl_hb_map},
};

// pub type HLBytesMap = HashMap<flexstr::SharedStr, HLVDynamic>;

impl ImmixAllocator {
    fn allocate_map(&mut self, initial_capacity: usize) -> Option<NonNull<hl::hl_hb_map>> {
        let map_size = mem::size_of::<hl::hl_hb_map>();
        let map_ptr = self.allocate(map_size)?;

        // Initialize the map struct to zero first
        unsafe {
            std::ptr::write_bytes(map_ptr.as_ptr(), 0, map_size);
            let map = &mut *(map_ptr.as_ptr() as *mut hl::hl_hb_map);

            // When initial_capacity is 0, leave all pointers null.
            // hbset checks values.is_null() and goes to the resize path.
            if initial_capacity > 0 {
                let cells_size = initial_capacity * mem::size_of::<*mut ::std::os::raw::c_void>();
                let nexts_size = initial_capacity * mem::size_of::<*mut ::std::os::raw::c_void>();
                let entries_size = initial_capacity * mem::size_of::<hl::hl_hb_entry>();
                let values_size = initial_capacity * mem::size_of::<hl::hl_hb_value>();

                let cells_ptr = self.allocate(cells_size)?;
                let nexts_ptr = self.allocate(nexts_size)?;
                let entries_ptr = self.allocate(entries_size)?;
                let values_ptr = self.allocate(values_size)?;

                map.cells = cells_ptr.as_ptr() as *mut c_void;
                map.nexts = nexts_ptr.as_ptr() as *mut c_void;
                map.entries = entries_ptr.as_ptr() as *mut hl::hl_hb_entry;
                map.values = values_ptr.as_ptr() as *mut hl::hl_hb_value;
                map.ncells = initial_capacity as i32;
                map.maxentries = initial_capacity as i32;

                std::ptr::write_bytes(map.cells, 0, cells_size);
                std::ptr::write_bytes(map.nexts, 0, nexts_size);
            }

            map.lfree = hl::hl_free_list {
                buckets: std::ptr::null_mut(),
                head: 0,
                nbuckets: 0,
            };
            map.nentries = 0;
        }

        NonNull::new(map_ptr.as_ptr() as *mut hl::hl_hb_map)
    }
}

unsafe fn hl_freelist_add_range(f: *mut hl::hl_free_list, pos: i32, count: i32) {
    if (*f).buckets.is_null() {
        // Special handling for continuous space
        if (*f).nbuckets == 0 {
            (*f).head = pos;
            (*f).nbuckets = count;
            return;
        } else if (*f).head + (*f).nbuckets == pos {
            (*f).nbuckets += count;
            return;
        } else if pos + count == (*f).head {
            (*f).head -= count;
            (*f).nbuckets += count;
            return;
        } else {
            let cur_pos = (*f).head;
            let cur_count = (*f).nbuckets;
            (*f).head = 0;
            (*f).nbuckets = 0;
            hl_freelist_resize(f, 2);
            if cur_count != 0 {
                hl_freelist_add_range(f, cur_pos, cur_count);
            }
        }
    }

    let mut b = (*f).buckets;
    let mut prev: *mut hl::hl_free_bucket = ptr::null_mut();

    while b < (*f).buckets.offset((*f).head as isize) {
        if (*b).pos > pos {
            break;
        }
        prev = b;
        b = b.offset(1);
    }

    if b < (*f).buckets.offset((*f).head as isize) && (*b).pos == pos + count {
        (*b).pos -= count;
        (*b).count += count;

        // Merge
        if !prev.is_null() && (*prev).pos + (*prev).count == (*b).pos {
            (*prev).count += (*b).count;
            ptr::copy(
                b.offset(1),
                b,
                ((*f)
                    .buckets
                    .offset((*f).head as isize)
                    .offset_from(b.offset(1))) as usize,
            );
            (*f).head -= 1;
        }
        return;
    }

    if !prev.is_null() && (*prev).pos + (*prev).count == pos {
        (*prev).count += count;
        return;
    }

    // Insert
    if (*f).head == (*f).nbuckets {
        let pos = b.offset_from((*f).buckets) as i32;
        hl_freelist_resize(f, (((*f).nbuckets * 3) + 1) >> 1);
        b = (*f).buckets.offset(pos as isize);
    }

    ptr::copy(
        b,
        b.offset(1),
        ((*f).buckets.offset((*f).head as isize).offset_from(b)) as usize,
    );
    (*b).pos = pos;
    (*b).count = count;
    (*f).head += 1;
}

unsafe fn hl_freelist_add(f: *mut hl::hl_free_list, pos: i32) {
    hl_freelist_add_range(f, pos, 1);
}

unsafe fn hl_freelist_get(f: *mut hl::hl_free_list) -> i32 {
    if (*f).buckets.is_null() {
        if (*f).nbuckets == 0 {
            return -1;
        }
        (*f).nbuckets -= 1;
        (*f).head += 1;
        return (*f).head - 1;
    }

    if (*f).head == 0 {
        return -1;
    }

    let b = (*f).buckets.offset(((*f).head - 1) as isize);
    (*b).count -= 1;
    let p = (*b).pos + (*b).count;
    if (*b).count == 0 {
        (*f).head -= 1;
        if (*f).head < ((*f).nbuckets >> 1) {
            hl_freelist_resize(f, (*f).nbuckets >> 1);
        }
    }
    p
}

unsafe fn hl_freelist_init(f: *mut hl::hl_free_list) {
    ptr::write_bytes(f, 0, 1);
}

unsafe fn hl_freelist_resize(f: *mut hl::hl_free_list, new_size: i32) {
    let new_buckets = crate::gc::gc_locked()
        .allocate(mem::size_of::<hl::hl_free_bucket>() * new_size as usize)
        .unwrap_or_else(|| crate::gc::out_of_memory("a hash map"))
        .as_ptr() as *mut hl::hl_free_bucket;

    ptr::copy_nonoverlapping((*f).buckets, new_buckets, (*f).head as usize);

    (*f).buckets = new_buckets;
    (*f).nbuckets = new_size;
}

pub mod hl_hb {
    use crate::{obj::hlp_hash_gen, ucs2::ucmp};

    use super::*;

    pub type MKeyType = *mut hl::uchar;

    pub fn hb_filter(key: MKeyType) -> MKeyType {
        key
    }

    pub fn hb_hash(key: MKeyType) -> u32 {
        unsafe { hlp_hash_gen(key, false) as u32 }
    }

    pub trait HbMap {
        fn match_entry(&self, c: usize, hash: u32, key: MKeyType) -> bool;
        fn get_key(&self, c: usize) -> MKeyType;
        fn set_entry(&mut self, c: usize, hash: u32, key: MKeyType);
        fn erase_entry(&mut self, c: usize);
    }

    impl HbMap for *mut hl::hl_hb_map {
        fn match_entry(&self, c: usize, hash: u32, key: MKeyType) -> bool {
            unsafe {
                (**self).entries.add(c).read().hash == hash
                    && ucmp((**self).values.add(c).read().key, key) == 0
            }
        }

        fn get_key(&self, c: usize) -> MKeyType {
            unsafe { (**self).values.add(c).read().key }
        }

        fn set_entry(&mut self, c: usize, hash: u32, key: MKeyType) {
            unsafe {
                (**self).entries.add(c).write(hl::hl_hb_entry { hash });
                (**self).values.add(c).write(hl::hl_hb_value {
                    key,
                    value: std::ptr::null_mut(),
                });
            }
        }

        fn erase_entry(&mut self, c: usize) {
            unsafe {
                (**self).values.add(c).write(hl::hl_hb_value {
                    key: std::ptr::null_mut(),
                    value: std::ptr::null_mut(),
                });
            }
        }
    }
}

const _MLIMIT: i32 = 128;

pub trait HbMapExt {
    fn m_index(&self, ckey: u32) -> i32;
    fn m_next(&self, ckey: u32) -> i32;
}

impl HbMapExt for *mut hl::hl_hb_map {
    fn m_index(&self, ckey: u32) -> i32 {
        unsafe {
            if (**self).maxentries < _MLIMIT {
                ((*(*self)).cells as *const std::ffi::c_char)
                    .add(ckey as usize)
                    .read() as i32
            } else {
                ((*(*self)).cells as *const i32).add(ckey as usize).read()
            }
        }
    }

    fn m_next(&self, ckey: u32) -> i32 {
        unsafe {
            if (**self).maxentries < _MLIMIT {
                ((*(*self)).nexts as *const std::ffi::c_char)
                    .add(ckey as usize)
                    .read() as i32
            } else {
                ((*(*self)).nexts as *const i32).add(ckey as usize).read()
            }
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_hballoc() -> *mut hl::hl_hb_map {
    let mut allocator = crate::gc::gc_locked();
    let allocated_map = allocator
        .allocate_map(0)
        .expect("could not allocate bytes map");
    // Deliberately NOT pre-marked. Setting mark bits at allocation time
    // poisoned the first collection after it: the tracer only scans lines
    // whose mark bit it just flipped, so a line marked here was treated as
    // already-visited and its interior pointers were never followed — the
    // map struct survived while every array it pointed to was swept. A
    // 16k-entry StringMap then answered null for ~5k keys after hbget
    // SIGSEGV'd on the freed entries (unit-suite Null access, "hang" at
    // 20k). Maps are reached like everything else: conservatively, through
    // whatever references the map struct.
    allocated_map.as_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_hbset(
    mut m: *mut hl::hl_hb_map,
    key: *mut hl::uchar,
    value: *mut hl::vdynamic,
) {
    use hl_hb::HbMap;
    if env_flag!("ASH_MAP_TRACE") {
        let k = if key.is_null() {
            String::new()
        } else {
            let mut out = String::new();
            let mut p = key;
            while *p != 0 {
                out.push(char::from_u32(*p as u32).unwrap_or('?'));
                p = p.add(1);
            }
            out
        };
        eprintln!(
            "[hbset] map={:#x} key={k:?} val={:#x}",
            m as usize, value as usize
        );
    }

    let mut c;
    let hash = hl_hb::hb_hash(key);
    let mut ckey = 0u32;
    if !(*m).values.is_null() {
        ckey = hash % (*m).ncells as u32;
        c = m.m_index(ckey);
        while c >= 0 {
            if m.match_entry(c as usize, hash, key) {
                (*(*m).values.wrapping_add(c as usize)).value = value;
                return;
            }
            c = m.m_next(c as u32);
        }
    }

    c = hl_freelist_get(&mut (*m).lfree);
    if c < 0 {
        hl_hb_resize(m);
        ckey = hash % (*m).ncells as u32;
        c = hl_freelist_get(&mut (*m).lfree);
    }
    m.set_entry(c as usize, hash, key);
    // nexts[c] = cells[ckey] (old head of chain), then cells[ckey] = c
    if (*m).maxentries < _MLIMIT {
        let src = ((*m).cells as *const std::ffi::c_char).wrapping_add(ckey as usize);
        let dst = ((*m).nexts as *mut std::ffi::c_char).wrapping_add(c as usize);
        ptr::write(dst, ptr::read(src));
        ptr::write(
            ((*m).cells as *mut std::ffi::c_char).wrapping_add(ckey as usize),
            c as std::ffi::c_char,
        );
    } else {
        let src = ((*m).cells as *const i32).wrapping_add(ckey as usize);
        let dst = ((*m).nexts as *mut i32).wrapping_add(c as usize);
        ptr::write(dst, ptr::read(src));
        ptr::write(((*m).cells as *mut i32).wrapping_add(ckey as usize), c);
    }
    (*(*m).values.wrapping_add(c as usize)).value = value;
    (*m).nentries += 1;
}

pub static H_SIZE_INIT: i32 = 3;

// successive primes that double every time
pub static H_PRIMES: [u32; 28] = [
    7, 17, 37, 79, 163, 331, 673, 1361, 2729, 5471, 10949, 21911, 43853, 87613, 175229, 350459,
    700919, 1401857, 2803727, 5607457, 11214943, 22429903, 44859823, 89719661, 179424673,
    373587883, 776531401, 1611623773,
];

unsafe fn hl_hb_resize(m: *mut hl::hl_hb_map) {
    // save
    let mut old = ptr::read(m);
    let resize_trace = env_flag!("ASH_MAP_RESIZE_TRACE");

    if (*m).nentries != (*m).maxentries {
        panic!("assert");
    }

    // resize
    let mut i = 0;
    let nentries = if (*m).maxentries != 0 {
        (((*m).maxentries * 3) + 1) >> 1
    } else {
        H_SIZE_INIT
    };
    let mut ncells = nentries >> 2;

    while H_PRIMES[i] < ncells as u32 {
        i += 1;
    }
    ncells = H_PRIMES[i] as i32;

    let ksize = if nentries < _MLIMIT {
        1
    } else {
        mem::size_of::<i32>()
    };
    (*m).entries = crate::gc::gc_locked()
        .allocate(nentries as usize * mem::size_of::<hl::hl_hb_entry>())
        .unwrap_or_else(|| crate::gc::out_of_memory("a hash map"))
        .as_ptr() as *mut hl::hl_hb_entry;
    (*m).values = crate::gc::gc_locked()
        .allocate(nentries as usize * mem::size_of::<hl::hl_hb_value>())
        .unwrap_or_else(|| crate::gc::out_of_memory("a hash map"))
        .as_ptr() as *mut hl::hl_hb_value;
    (*m).maxentries = nentries;

    if old.ncells == ncells && (nentries < _MLIMIT || old.maxentries >= _MLIMIT) {
        // simply expand
        (*m).nexts = crate::gc::gc_locked()
            .allocate(nentries as usize * ksize)
            .unwrap_or_else(|| crate::gc::out_of_memory("a hash map"))
            .as_ptr() as *mut c_void;
        ptr::copy_nonoverlapping(old.entries, (*m).entries, old.maxentries as usize);
        ptr::copy_nonoverlapping(old.values, (*m).values, old.maxentries as usize);
        ptr::copy_nonoverlapping(old.nexts, (*m).nexts, old.maxentries as usize * ksize);
        ptr::write_bytes(
            (*m).values.add(old.maxentries as usize),
            0,
            (nentries - old.maxentries) as usize,
        );
        hl_freelist_add_range(
            &mut (*m).lfree,
            old.maxentries,
            (*m).maxentries - old.maxentries,
        );
    } else {
        // expand and remap
        (*m).cells = crate::gc::gc_locked()
            .allocate((ncells + nentries) as usize * ksize)
            .unwrap_or_else(|| crate::gc::out_of_memory("a hash map"))
            .as_ptr() as *mut c_void;
        (*m).nexts = (*m).cells.add(ncells as usize * ksize);
        (*m).ncells = ncells;
        (*m).nentries = 0;
        ptr::write_bytes((*m).cells, 0xFF, ncells as usize * ksize);
        // Zero the values array — count is in ELEMENTS, not bytes
        // (write_bytes multiplies by size_of::<T>() internally)
        ptr::write_bytes((*m).values, 0, nentries as usize);
        hl_freelist_init(&mut (*m).lfree);
        hl_freelist_add_range(&mut (*m).lfree, 0, (*m).maxentries);
        for i in 0..old.ncells {
            let mut c = if old.maxentries < _MLIMIT {
                *(old.cells as *const std::ffi::c_char).add(i as usize) as i32
            } else {
                *(old.cells as *const i32).add(i as usize)
            };
            while c >= 0 {
                let _old: *mut hl_hb_map = &mut old;
                hlp_hbset(m, get_key(_old, c), (*old.values.add(c as usize)).value);
                c = _old.m_next(c as u32);
            }
        }
    }
    if resize_trace {
        eprintln!(
            "[hb-resize] map={:#x} maxentries {}->{} ncells {}->{} entries={:#x}+{:#x} values={:#x}+{:#x} cells={:#x} nexts={:#x}",
            m as usize,
            old.maxentries,
            (*m).maxentries,
            old.ncells,
            (*m).ncells,
            (*m).entries as usize,
            (*m).maxentries as usize * mem::size_of::<hl::hl_hb_entry>(),
            (*m).values as usize,
            (*m).maxentries as usize * mem::size_of::<hl::hl_hb_value>(),
            (*m).cells as usize,
            (*m).nexts as usize,
        );
    }
}

unsafe fn get_key(m: *mut hl::hl_hb_map, c: i32) -> *mut hl::uchar {
    (*((*m).values.add(c as usize))).key
}

#[no_mangle]
pub unsafe extern "C" fn hlp_hbget(
    m: *mut hl::hl_hb_map,
    key: *mut hl::uchar,
) -> *mut hl::vdynamic {
    use hl_hb::HbMap;

    if m.is_null() || (*m).values.is_null() {
        return ptr::null_mut();
    }
    let hash = hl_hb::hb_hash(key);
    let ckey = hash % (*m).ncells as u32;
    let mut c = m.m_index(ckey);
    while c >= 0 {
        if m.match_entry(c as usize, hash, key) {
            return (*(*m).values.add(c as usize)).value;
        }
        c = m.m_next(c as u32);
    }
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_hbexists(m: *mut hl::hl_hb_map, key: *mut hl::uchar) -> bool {
    use hl_hb::HbMap;

    if m.is_null() || (*m).values.is_null() {
        return false;
    }
    let hash = hl_hb::hb_hash(key);
    let ckey = hash % (*m).ncells as u32;
    let mut c = m.m_index(ckey);
    while c >= 0 {
        if m.match_entry(c as usize, hash, key) {
            return true;
        }
        c = m.m_next(c as u32);
    }
    false
}

#[no_mangle]
pub unsafe extern "C" fn hlp_hbremove(mut m: *mut hl::hl_hb_map, key: *mut hl::uchar) -> bool {
    use hl_hb::HbMap;

    if m.is_null() || (*m).values.is_null() {
        return false;
    }
    let hash = hl_hb::hb_hash(key);
    let ckey = hash % (*m).ncells as u32;
    let mut c = m.m_index(ckey);
    let mut prev: i32 = -1;
    while c >= 0 {
        if m.match_entry(c as usize, hash, key) {
            // Unlink from chain
            let next = m.m_next(c as u32);
            if prev < 0 {
                // Head of chain: cells[ckey] = next
                if (*m).maxentries < _MLIMIT {
                    ptr::write(
                        ((*m).cells as *mut std::ffi::c_char).wrapping_add(ckey as usize),
                        next as std::ffi::c_char,
                    );
                } else {
                    ptr::write(((*m).cells as *mut i32).wrapping_add(ckey as usize), next);
                }
            } else {
                // Middle/end of chain: nexts[prev] = next
                if (*m).maxentries < _MLIMIT {
                    ptr::write(
                        ((*m).nexts as *mut std::ffi::c_char).wrapping_add(prev as usize),
                        next as std::ffi::c_char,
                    );
                } else {
                    ptr::write(((*m).nexts as *mut i32).wrapping_add(prev as usize), next);
                }
            }
            m.erase_entry(c as usize);
            hl_freelist_add(&mut (*m).lfree, c);
            (*m).nentries -= 1;
            return true;
        }
        prev = c;
        c = m.m_next(c as u32);
    }
    false
}

#[no_mangle]
pub unsafe extern "C" fn hlp_hbkeys(m: *mut hl::hl_hb_map) -> *mut hl::varray {
    let count = if m.is_null() { 0 } else { (*m).nentries };
    let a = crate::array::hlp_alloc_array(crate::types::hlt_bytes(), count);
    if m.is_null() || count == 0 {
        return a;
    }
    let mut p = 0;
    for i in 0..(*m).ncells {
        let mut c = if (*m).maxentries < _MLIMIT {
            *((*m).cells as *const std::ffi::c_char).add(i as usize) as i32
        } else {
            *((*m).cells as *const i32).add(i as usize)
        };
        while c >= 0 {
            let key = (*(*m).values.add(c as usize)).key;
            *(crate::types::hl_aptr::<*mut hl::vbyte>(a)).add(p) = key as *mut hl::vbyte;
            p += 1;
            c = m.m_next(c as u32);
        }
    }
    a
}

#[no_mangle]
pub unsafe extern "C" fn hlp_hbvalues(m: *mut hl::hl_hb_map) -> *mut hl::varray {
    let count = if m.is_null() { 0 } else { (*m).nentries };
    let a = crate::array::hlp_alloc_array(crate::types::hlt_dyn(), count);
    if m.is_null() || count == 0 {
        return a;
    }
    let mut p = 0;
    for i in 0..(*m).ncells {
        let mut c = if (*m).maxentries < _MLIMIT {
            *((*m).cells as *const std::ffi::c_char).add(i as usize) as i32
        } else {
            *((*m).cells as *const i32).add(i as usize)
        };
        while c >= 0 {
            let val = (*(*m).values.add(c as usize)).value;
            *(crate::types::hl_aptr::<*mut hl::vdynamic>(a)).add(p) = val;
            p += 1;
            c = m.m_next(c as u32);
        }
    }
    a
}

/// Upstream _MNAME(clear) (maps.h): zero the whole map header. That is also
/// the shape `allocate_map(0)` hands back, so the cleared map takes the same
/// resize-on-first-set path a fresh one does; the cell/entry/value arrays it
/// dropped are ordinary GC allocations and are reclaimed by the collector.
#[no_mangle]
pub unsafe extern "C" fn hlp_hbclear(m: *mut hl::hl_hb_map) {
    if m.is_null() {
        return;
    }
    ptr::write_bytes(m as *mut u8, 0, mem::size_of::<hl::hl_hb_map>());
}

/// Upstream _MNAME(size) (maps.h).
#[no_mangle]
pub unsafe extern "C" fn hlp_hbsize(m: *mut hl::hl_hb_map) -> i32 {
    if m.is_null() {
        0
    } else {
        (*m).nentries
    }
}

// ============================================================================
// IntMap (hi*), Int64Map (hi64*) and ObjectMap (ho*)
//
// The index stays a Rust HashMap, but it must never be the only holder of a
// GC pointer: the collector does not scan the malloc heap, so a GC object
// reachable only from there has no root and the next collection takes it.
//
// Every GC pointer therefore lives in `slots`, a GC-allocated array hanging
// off a GC-allocated `RootedMap` header — the pointer handed back to the Haxe
// Map object. conservative_trace reaches the header from the Map object, the
// array from the header, and each key and value from the array. The HashMap
// maps a key to a slot index and holds no GC pointer of its own; object maps
// take two slots per entry so the key is rooted alongside its value.
//
// ============================================================================

use std::collections::HashMap;

#[repr(C)]
struct RootedMap {
    /// GC array of GC pointers. Int maps use one slot per entry (the value);
    /// object maps use two (key, then value).
    slots: *mut *mut hl::vdynamic,
    capacity: usize,
    /// Box<SlotIndex<K>> on the malloc heap. Deliberately holds no GC pointer;
    /// the tracer bounds-checks this word and ignores it.
    index: *mut c_void,
}

/// Slot bookkeeping, kept off the GC heap.
struct SlotIndex<K> {
    slot_of: HashMap<K, usize>,
    free: Vec<usize>,
    high: usize,
}

impl<K: std::hash::Hash + Eq> SlotIndex<K> {
    fn new() -> Self {
        SlotIndex {
            slot_of: HashMap::new(),
            free: Vec::new(),
            high: 0,
        }
    }
}

/// GC memory for a map's header and slots. `gc_alloc` already returns zeroed
/// memory and takes the TLAB fast path for the small sizes, so this must not
/// memset again -- doing so outside the allocator's lock would also leave a
/// window where a collection could sweep the block before it was written.
unsafe fn gc_alloc_zeroed(bytes: usize) -> *mut u8 {
    match crate::gc::gc_alloc(bytes) {
        Some(nn) => nn.as_ptr(),
        None => ptr::null_mut(),
    }
}

unsafe fn rooted_alloc<K: std::hash::Hash + Eq>() -> *mut c_void {
    let hdr = gc_alloc_zeroed(mem::size_of::<RootedMap>()) as *mut RootedMap;
    if hdr.is_null() {
        return ptr::null_mut();
    }
    (*hdr).index = Box::into_raw(Box::new(SlotIndex::<K>::new())) as *mut c_void;
    hdr as *mut c_void
}

/// Grow `slots` to hold at least `need` pointers. The old array stays reachable
/// through `rm.slots` until the new one is installed, so a collection triggered
/// by this allocation cannot reclaim the entries being copied.
unsafe fn slots_reserve(rm: *mut RootedMap, need: usize) -> bool {
    if need <= (*rm).capacity {
        return true;
    }
    let mut cap = if (*rm).capacity == 0 {
        8
    } else {
        (*rm).capacity
    };
    while cap < need {
        cap *= 2;
    }
    let fresh =
        gc_alloc_zeroed(cap * mem::size_of::<*mut hl::vdynamic>()) as *mut *mut hl::vdynamic;
    if fresh.is_null() {
        return false;
    }
    if !(*rm).slots.is_null() {
        ptr::copy_nonoverlapping((*rm).slots, fresh, (*rm).capacity);
    }
    (*rm).slots = fresh;
    (*rm).capacity = cap;
    true
}

/// Reserve a slot run of `stride` pointers for a key not yet in the map.
unsafe fn slot_claim<K: std::hash::Hash + Eq>(
    rm: *mut RootedMap,
    idx: &mut SlotIndex<K>,
    stride: usize,
) -> Option<usize> {
    let slot = match idx.free.pop() {
        Some(s) => s,
        None => {
            let s = idx.high;
            idx.high += 1;
            s
        }
    };
    if !slots_reserve(rm, (slot + 1) * stride) {
        idx.free.push(slot);
        return None;
    }
    Some(slot)
}

type IntIndex = SlotIndex<i32>;

#[no_mangle]
pub unsafe extern "C" fn hlp_hialloc() -> *mut c_void {
    rooted_alloc::<i32>()
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hiset(m: *mut c_void, key: i32, value: *mut hl::vdynamic) {
    if m.is_null() {
        return;
    }
    let rm = m as *mut RootedMap;
    let idx = &mut *((*rm).index as *mut IntIndex);
    if let Some(&slot) = idx.slot_of.get(&key) {
        *(*rm).slots.add(slot) = value;
        return;
    }
    if let Some(slot) = slot_claim(rm, idx, 1) {
        *(*rm).slots.add(slot) = value;
        idx.slot_of.insert(key, slot);
    }
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hiexists(m: *mut c_void, key: i32) -> bool {
    if m.is_null() {
        return false;
    }
    let rm = m as *const RootedMap;
    let idx = &*((*rm).index as *const IntIndex);
    idx.slot_of.contains_key(&key)
}
#[no_mangle]
pub unsafe extern "C" fn hlp_higet(m: *mut c_void, key: i32) -> *mut hl::vdynamic {
    if m.is_null() {
        return ptr::null_mut();
    }
    let rm = m as *const RootedMap;
    match (*((*rm).index as *const IntIndex)).slot_of.get(&key) {
        Some(&slot) => *(*rm).slots.add(slot),
        None => ptr::null_mut(),
    }
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hiremove(m: *mut c_void, key: i32) -> bool {
    if m.is_null() {
        return false;
    }
    let rm = m as *mut RootedMap;
    let idx = &mut *((*rm).index as *mut IntIndex);
    match idx.slot_of.remove(&key) {
        Some(slot) => {
            // Drop the reference so the value becomes collectable.
            *(*rm).slots.add(slot) = ptr::null_mut();
            idx.free.push(slot);
            true
        }
        None => false,
    }
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hikeys(m: *mut c_void) -> *mut hl::varray {
    if m.is_null() {
        return ptr::null_mut();
    }
    let rm = m as *const RootedMap;
    let idx = &*((*rm).index as *const IntIndex);
    let arr = crate::array::hlp_alloc_array(crate::types::hlt_i32(), idx.slot_of.len() as i32);
    for (i, &key) in idx.slot_of.keys().enumerate() {
        *(crate::types::hl_aptr::<i32>(arr)).add(i) = key;
    }
    arr
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hivalues(m: *mut c_void) -> *mut hl::varray {
    if m.is_null() {
        return ptr::null_mut();
    }
    let rm = m as *const RootedMap;
    let idx = &*((*rm).index as *const IntIndex);
    let arr = crate::array::hlp_alloc_array(crate::types::hlt_dyn(), idx.slot_of.len() as i32);
    for (i, &slot) in idx.slot_of.values().enumerate() {
        *(crate::types::hl_aptr::<*mut hl::vdynamic>(arr)).add(i) = *(*rm).slots.add(slot);
    }
    arr
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hiclear(m: *mut c_void) {
    if m.is_null() {
        return;
    }
    let rm = m as *mut RootedMap;
    let idx = &mut *((*rm).index as *mut IntIndex);
    for &slot in idx.slot_of.values() {
        *(*rm).slots.add(slot) = ptr::null_mut();
    }
    idx.slot_of.clear();
    idx.free.clear();
    idx.high = 0;
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hisize(m: *mut c_void) -> i32 {
    if m.is_null() {
        return 0;
    }
    let rm = m as *const RootedMap;
    (*((*rm).index as *const IntIndex)).slot_of.len() as i32
}

// ============================================================================
// Int64Map (hi64*) -- the int map above with a 64-bit key.
//
// Upstream separates the two only by key width and by hash: maps.c defines
// hl_hi64hash as the two halves of the key xored together, because its cell
// index is `hash % ncells`. A Rust HashMap hashes the whole i64 itself, so
// that fold has no counterpart here and the key type is the only difference.
// The key is a scalar and stays in the off-heap index; only the value is a GC
// pointer, so one slot per entry roots everything the collector must see.
// ============================================================================
type Int64Index = SlotIndex<i64>;

#[no_mangle]
pub unsafe extern "C" fn hlp_hi64alloc() -> *mut c_void {
    rooted_alloc::<i64>()
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hi64set(m: *mut c_void, key: i64, value: *mut hl::vdynamic) {
    if m.is_null() {
        return;
    }
    let rm = m as *mut RootedMap;
    let idx = &mut *((*rm).index as *mut Int64Index);
    if let Some(&slot) = idx.slot_of.get(&key) {
        *(*rm).slots.add(slot) = value;
        return;
    }
    if let Some(slot) = slot_claim(rm, idx, 1) {
        *(*rm).slots.add(slot) = value;
        idx.slot_of.insert(key, slot);
    }
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hi64exists(m: *mut c_void, key: i64) -> bool {
    if m.is_null() {
        return false;
    }
    let rm = m as *const RootedMap;
    let idx = &*((*rm).index as *const Int64Index);
    idx.slot_of.contains_key(&key)
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hi64get(m: *mut c_void, key: i64) -> *mut hl::vdynamic {
    if m.is_null() {
        return ptr::null_mut();
    }
    let rm = m as *const RootedMap;
    match (*((*rm).index as *const Int64Index)).slot_of.get(&key) {
        Some(&slot) => *(*rm).slots.add(slot),
        None => ptr::null_mut(),
    }
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hi64remove(m: *mut c_void, key: i64) -> bool {
    if m.is_null() {
        return false;
    }
    let rm = m as *mut RootedMap;
    let idx = &mut *((*rm).index as *mut Int64Index);
    match idx.slot_of.remove(&key) {
        Some(slot) => {
            // Drop the reference so the value becomes collectable.
            *(*rm).slots.add(slot) = ptr::null_mut();
            idx.free.push(slot);
            true
        }
        None => false,
    }
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hi64keys(m: *mut c_void) -> *mut hl::varray {
    if m.is_null() {
        return ptr::null_mut();
    }
    let rm = m as *const RootedMap;
    let idx = &*((*rm).index as *const Int64Index);
    // hlt_i64, matching `#define hlt_key hlt_i64` for this map in maps.c:
    // the caller indexes the result with an 8-byte stride.
    let arr = crate::array::hlp_alloc_array(crate::types::hlt_i64(), idx.slot_of.len() as i32);
    for (i, &key) in idx.slot_of.keys().enumerate() {
        *(crate::types::hl_aptr::<i64>(arr)).add(i) = key;
    }
    arr
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hi64values(m: *mut c_void) -> *mut hl::varray {
    if m.is_null() {
        return ptr::null_mut();
    }
    let rm = m as *const RootedMap;
    let idx = &*((*rm).index as *const Int64Index);
    let arr = crate::array::hlp_alloc_array(crate::types::hlt_dyn(), idx.slot_of.len() as i32);
    for (i, &slot) in idx.slot_of.values().enumerate() {
        *(crate::types::hl_aptr::<*mut hl::vdynamic>(arr)).add(i) = *(*rm).slots.add(slot);
    }
    arr
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hi64clear(m: *mut c_void) {
    if m.is_null() {
        return;
    }
    let rm = m as *mut RootedMap;
    let idx = &mut *((*rm).index as *mut Int64Index);
    for &slot in idx.slot_of.values() {
        *(*rm).slots.add(slot) = ptr::null_mut();
    }
    idx.slot_of.clear();
    idx.free.clear();
    idx.high = 0;
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hi64size(m: *mut c_void) -> i32 {
    if m.is_null() {
        return 0;
    }
    let rm = m as *const RootedMap;
    (*((*rm).index as *const Int64Index)).slot_of.len() as i32
}

// ============================================================================
// ObjectMap (ho*) -- two slots per entry so the key object is rooted too.
// ============================================================================
type ObjIndex = SlotIndex<usize>;

#[no_mangle]
pub unsafe extern "C" fn hlp_hoalloc() -> *mut c_void {
    rooted_alloc::<usize>()
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hoset(m: *mut c_void, key: *mut hl::vdynamic, val: *mut hl::vdynamic) {
    if m.is_null() || key.is_null() {
        return;
    }
    let rm = m as *mut RootedMap;
    let idx = &mut *((*rm).index as *mut ObjIndex);
    if let Some(&slot) = idx.slot_of.get(&(key as usize)) {
        *(*rm).slots.add(slot * 2) = key;
        *(*rm).slots.add(slot * 2 + 1) = val;
        return;
    }
    if let Some(slot) = slot_claim(rm, idx, 2) {
        *(*rm).slots.add(slot * 2) = key;
        *(*rm).slots.add(slot * 2 + 1) = val;
        idx.slot_of.insert(key as usize, slot);
    }
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hoexists(m: *mut c_void, key: *mut hl::vdynamic) -> bool {
    if m.is_null() || key.is_null() {
        return false;
    }
    let rm = m as *const RootedMap;
    let idx = &*((*rm).index as *const ObjIndex);
    idx.slot_of.contains_key(&(key as usize))
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hoget(m: *mut c_void, key: *mut hl::vdynamic) -> *mut hl::vdynamic {
    if m.is_null() || key.is_null() {
        return ptr::null_mut();
    }
    let rm = m as *const RootedMap;
    let idx = &*((*rm).index as *const ObjIndex);
    match idx.slot_of.get(&(key as usize)) {
        Some(&slot) => *(*rm).slots.add(slot * 2 + 1),
        None => ptr::null_mut(),
    }
}
#[no_mangle]
pub unsafe extern "C" fn hlp_horemove(m: *mut c_void, key: *mut hl::vdynamic) -> bool {
    if m.is_null() || key.is_null() {
        return false;
    }
    let rm = m as *mut RootedMap;
    let idx = &mut *((*rm).index as *mut ObjIndex);
    match idx.slot_of.remove(&(key as usize)) {
        Some(slot) => {
            *(*rm).slots.add(slot * 2) = ptr::null_mut();
            *(*rm).slots.add(slot * 2 + 1) = ptr::null_mut();
            idx.free.push(slot);
            true
        }
        None => false,
    }
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hokeys(m: *mut c_void) -> *mut hl::varray {
    if m.is_null() {
        return ptr::null_mut();
    }
    let rm = m as *const RootedMap;
    let idx = &*((*rm).index as *const ObjIndex);
    let arr = crate::array::hlp_alloc_array(crate::types::hlt_dyn(), idx.slot_of.len() as i32);
    for (i, &slot) in idx.slot_of.values().enumerate() {
        *(crate::types::hl_aptr::<*mut hl::vdynamic>(arr)).add(i) = *(*rm).slots.add(slot * 2);
    }
    arr
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hovalues(m: *mut c_void) -> *mut hl::varray {
    if m.is_null() {
        return ptr::null_mut();
    }
    let rm = m as *const RootedMap;
    let idx = &*((*rm).index as *const ObjIndex);
    let arr = crate::array::hlp_alloc_array(crate::types::hlt_dyn(), idx.slot_of.len() as i32);
    for (i, &slot) in idx.slot_of.values().enumerate() {
        *(crate::types::hl_aptr::<*mut hl::vdynamic>(arr)).add(i) = *(*rm).slots.add(slot * 2 + 1);
    }
    arr
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hoclear(m: *mut c_void) {
    if m.is_null() {
        return;
    }
    let rm = m as *mut RootedMap;
    let idx = &mut *((*rm).index as *mut ObjIndex);
    for &slot in idx.slot_of.values() {
        *(*rm).slots.add(slot * 2) = ptr::null_mut();
        *(*rm).slots.add(slot * 2 + 1) = ptr::null_mut();
    }
    idx.slot_of.clear();
    idx.free.clear();
    idx.high = 0;
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hosize(m: *mut c_void) -> i32 {
    if m.is_null() {
        return 0;
    }
    let rm = m as *const RootedMap;
    (*((*rm).index as *const ObjIndex)).slot_of.len() as i32
}

// ============================================================================
// hi64* contract tests
//
// These nine prims are reached by NAME from the primitive resolver as
// `unsafe extern "C"`, so nothing about them is checked at build time: a
// narrowed key or a dropped argument links and then misbehaves. What follows
// pins the contract the callers rely on -- round-trip, miss values, overwrite,
// size, and the index alignment `keys`/`values` promise -- and deliberately
// does not pin iteration ORDER, which is a HashMap's to choose.
// ============================================================================
#[cfg(test)]
mod hi64_tests {
    use super::*;
    use crate::hl::{varray, vdynamic};

    /// Every `#[test]` runs on its own harness thread and every prim below
    /// allocates from the process-wide collector. A thread the collector has
    /// not been told about is left out of the conservative stack scan, so a
    /// collection forced by another test -- `gc::tests` forces one -- would
    /// find no root for the map header, its slot array, or the boxed values,
    /// and could hand their block back to the free list underneath us.
    ///
    /// Registration uses the platform's real stack boundary rather than
    /// `hlp_gc_set_stack_top`'s `&local + 8`: the latter only covers frames
    /// below that local, and which locals the optimiser places above it is not
    /// ours to predict. Unregistering from `Drop` is the part that matters on
    /// the failure path -- a panicking assertion must not leave a phantom
    /// mutator behind for the next `stop_mutator_world` to wait on.
    struct Mutator;

    impl Mutator {
        fn enter() -> Mutator {
            // Must precede registration: a registered thread allocates through
            // the TLAB, whose refill takes `gc_locked()` and expects the
            // singleton to already exist.
            unsafe { crate::gc::hlp_gc_init() };
            crate::gc::gc_register_current_os_thread();
            Mutator
        }
    }

    impl Drop for Mutator {
        fn drop(&mut self) {
            crate::gc::gc_unregister_current_os_thread();
        }
    }

    /// Box an i64 the way the runtime does. Fabricating a pointer instead
    /// would put a non-heap word in a GC-visible slot, which is a crash rather
    /// than a failed assertion.
    unsafe fn dyn_i64(v: i64) -> *mut vdynamic {
        let mut raw = v;
        let d =
            crate::cast::hlp_make_dyn(&mut raw as *mut i64 as *mut c_void, crate::types::hlt_i64());
        assert!(!d.is_null(), "hlp_make_dyn returned null for {v}");
        d
    }

    unsafe fn unbox_i64(d: *mut vdynamic) -> i64 {
        assert!(!d.is_null(), "expected a boxed value, got null");
        (*d).v.i64_
    }

    unsafe fn keys_of(m: *mut c_void) -> Vec<i64> {
        let a = hlp_hi64keys(m);
        assert!(!a.is_null(), "hi64keys returned null for a live map");
        let n = (*a).size as usize;
        (0..n)
            .map(|i| *(crate::types::hl_aptr::<i64>(a)).add(i))
            .collect()
    }

    unsafe fn values_of(m: *mut c_void) -> Vec<*mut vdynamic> {
        let a = hlp_hi64values(m);
        assert!(!a.is_null(), "hi64values returned null for a live map");
        let n = (*a).size as usize;
        (0..n)
            .map(|i| *(crate::types::hl_aptr::<*mut vdynamic>(a)).add(i))
            .collect()
    }

    /// Keys chosen to cover the width: a 64-bit map that quietly truncated to
    /// i32 would collide `1 << 40` with 0, and `i64::MIN` with `i64::MAX + 1`.
    const WIDE_KEYS: [i64; 8] = [
        0,
        -1,
        1,
        i64::MIN,
        i64::MAX,
        i32::MAX as i64 + 1,
        i32::MIN as i64 - 1,
        1i64 << 40,
    ];

    /// Injective, so a key/value pairing that drifted by one slot shows up.
    fn payload(key: i64) -> i64 {
        key.wrapping_mul(31).wrapping_add(7)
    }

    #[test]
    fn set_then_get_round_trips_extreme_keys() {
        let _m = Mutator::enter();
        unsafe {
            let map = hlp_hi64alloc();
            assert!(!map.is_null());

            for &k in &WIDE_KEYS {
                hlp_hi64set(map, k, dyn_i64(payload(k)));
            }
            assert_eq!(hlp_hi64size(map), WIDE_KEYS.len() as i32);

            for &k in &WIDE_KEYS {
                assert_eq!(
                    unbox_i64(hlp_hi64get(map, k)),
                    payload(k),
                    "round trip for key {k}"
                );
            }
        }
    }

    /// The implementation documents a miss as a null `vdynamic*`, not a
    /// sentinel and not a panic.
    #[test]
    fn get_on_a_missing_key_returns_null() {
        let _m = Mutator::enter();
        unsafe {
            let map = hlp_hi64alloc();
            assert!(hlp_hi64get(map, 7).is_null(), "miss on an empty map");

            hlp_hi64set(map, 7, dyn_i64(70));
            assert!(hlp_hi64get(map, 8).is_null(), "miss beside a hit");
            assert!(hlp_hi64get(map, -7).is_null(), "sign is part of the key");
            // A truncating implementation would answer this one with 7's value.
            assert!(
                hlp_hi64get(map, 7 + (1i64 << 32)).is_null(),
                "the high half of the key must participate"
            );
            assert!(!hlp_hi64get(map, 7).is_null(), "the hit still hits");
        }
    }

    #[test]
    fn set_on_an_existing_key_overwrites_without_growing() {
        let _m = Mutator::enter();
        unsafe {
            let map = hlp_hi64alloc();
            hlp_hi64set(map, i64::MIN, dyn_i64(1));
            assert_eq!(hlp_hi64size(map), 1);

            hlp_hi64set(map, i64::MIN, dyn_i64(2));
            assert_eq!(hlp_hi64size(map), 1, "overwrite must not duplicate");
            assert_eq!(unbox_i64(hlp_hi64get(map, i64::MIN)), 2);

            hlp_hi64set(map, i64::MIN, dyn_i64(3));
            assert_eq!(hlp_hi64size(map), 1);
            assert_eq!(unbox_i64(hlp_hi64get(map, i64::MIN)), 3);
            assert_eq!(keys_of(map), vec![i64::MIN]);
        }
    }

    #[test]
    fn exists_follows_set_and_remove() {
        let _m = Mutator::enter();
        unsafe {
            let map = hlp_hi64alloc();
            assert!(!hlp_hi64exists(map, i64::MAX));

            hlp_hi64set(map, i64::MAX, dyn_i64(1));
            assert!(hlp_hi64exists(map, i64::MAX));
            assert!(!hlp_hi64exists(map, i64::MIN));

            // A key whose value is null is still present.
            hlp_hi64set(map, i64::MIN, ptr::null_mut());
            assert!(hlp_hi64exists(map, i64::MIN), "presence is not value-ness");
            assert!(hlp_hi64get(map, i64::MIN).is_null());
            assert_eq!(hlp_hi64size(map), 2);

            assert!(hlp_hi64remove(map, i64::MAX));
            assert!(!hlp_hi64exists(map, i64::MAX));
            assert!(hlp_hi64exists(map, i64::MIN));
        }
    }

    #[test]
    fn remove_reports_whether_it_removed_and_the_key_then_misses() {
        let _m = Mutator::enter();
        unsafe {
            let map = hlp_hi64alloc();
            assert!(
                !hlp_hi64remove(map, 5),
                "removing from an empty map is false"
            );

            hlp_hi64set(map, 5, dyn_i64(50));
            assert!(hlp_hi64remove(map, 5), "removing a present key is true");
            assert!(!hlp_hi64remove(map, 5), "a second remove is false");
            assert!(hlp_hi64get(map, 5).is_null(), "the key misses afterwards");
            assert!(!hlp_hi64exists(map, 5));
            assert_eq!(hlp_hi64size(map), 0);

            // The freed slot is reused; the new occupant must not inherit the
            // old value.
            hlp_hi64set(map, 6, dyn_i64(60));
            assert_eq!(unbox_i64(hlp_hi64get(map, 6)), 60);
            assert!(hlp_hi64get(map, 5).is_null());
            assert_eq!(hlp_hi64size(map), 1);
        }
    }

    #[test]
    fn size_follows_a_set_remove_clear_sequence() {
        let _m = Mutator::enter();
        unsafe {
            let map = hlp_hi64alloc();
            assert_eq!(hlp_hi64size(map), 0);

            for (n, &k) in WIDE_KEYS.iter().enumerate() {
                hlp_hi64set(map, k, dyn_i64(payload(k)));
                assert_eq!(hlp_hi64size(map), n as i32 + 1);
            }
            // Re-setting every key changes nothing.
            for &k in &WIDE_KEYS {
                hlp_hi64set(map, k, dyn_i64(payload(k)));
            }
            assert_eq!(hlp_hi64size(map), WIDE_KEYS.len() as i32);

            let mut expected = WIDE_KEYS.len() as i32;
            for &k in &WIDE_KEYS[..3] {
                assert!(hlp_hi64remove(map, k));
                expected -= 1;
                assert_eq!(hlp_hi64size(map), expected);
            }
            assert!(!hlp_hi64remove(map, WIDE_KEYS[0]));
            assert_eq!(hlp_hi64size(map), expected);

            hlp_hi64clear(map);
            assert_eq!(hlp_hi64size(map), 0);
        }
    }

    /// The one property the implementation promises about the two arrays:
    /// same length, and `key[i]` belongs with `value[i]`. Order is
    /// deliberately unspecified, so nothing here depends on it.
    #[test]
    fn keys_and_values_are_index_aligned() {
        let _m = Mutator::enter();
        unsafe {
            let map = hlp_hi64alloc();

            assert!(keys_of(map).is_empty(), "empty map yields an empty array");
            assert!(values_of(map).is_empty());

            for &k in &WIDE_KEYS {
                hlp_hi64set(map, k, dyn_i64(payload(k)));
            }
            // Removing first exercises a slot table with a hole in it, which
            // is where a positional (rather than paired) walk goes wrong.
            assert!(hlp_hi64remove(map, WIDE_KEYS[2]));
            hlp_hi64set(map, 999, dyn_i64(payload(999)));

            let keys = keys_of(map);
            let values = values_of(map);
            assert_eq!(keys.len(), values.len(), "arrays must be the same length");
            assert_eq!(keys.len(), hlp_hi64size(map) as usize);

            for (i, &k) in keys.iter().enumerate() {
                assert_eq!(
                    unbox_i64(values[i]),
                    payload(k),
                    "values[{i}] does not belong to keys[{i}] = {k}"
                );
            }

            // Same contents as the map, order aside.
            let mut got = keys;
            got.sort_unstable();
            let mut want: Vec<i64> = WIDE_KEYS
                .iter()
                .copied()
                .filter(|&k| k != WIDE_KEYS[2])
                .chain(std::iter::once(999))
                .collect();
            want.sort_unstable();
            assert_eq!(got, want);
        }
    }

    /// The 64-bit key width is the whole difference from `hi*`. An array
    /// tagged `hlt_i32` here would be half the size the caller indexes and
    /// would truncate every key.
    #[test]
    fn keys_and_values_arrays_carry_the_upstream_element_types() {
        let _m = Mutator::enter();
        unsafe {
            let map = hlp_hi64alloc();
            let big = 1i64 << 40;
            hlp_hi64set(map, big, dyn_i64(payload(big)));

            let ka = hlp_hi64keys(map);
            assert_eq!((*ka).at, crate::types::hlt_i64(), "keys element type");
            assert_eq!(crate::types::hlp_type_size((*ka).at), 8, "key stride");
            assert_eq!((*ka).size, 1);
            assert_eq!(*crate::types::hl_aptr::<i64>(ka), big);

            let va = hlp_hi64values(map);
            assert_eq!((*va).at, crate::types::hlt_dyn(), "values element type");
            assert_eq!((*va).size, 1);
        }
    }

    #[test]
    fn clear_empties_the_map_and_leaves_it_usable() {
        let _m = Mutator::enter();
        unsafe {
            let map = hlp_hi64alloc();
            for &k in &WIDE_KEYS {
                hlp_hi64set(map, k, dyn_i64(payload(k)));
            }

            hlp_hi64clear(map);
            assert_eq!(hlp_hi64size(map), 0);
            for &k in &WIDE_KEYS {
                assert!(!hlp_hi64exists(map, k), "key {k} survived clear");
                assert!(
                    hlp_hi64get(map, k).is_null(),
                    "value for {k} survived clear"
                );
            }
            assert!(keys_of(map).is_empty());
            assert!(values_of(map).is_empty());

            // Still usable, and the recycled slots hold the new values.
            for &k in &WIDE_KEYS {
                hlp_hi64set(map, k, dyn_i64(payload(k) + 1));
            }
            assert_eq!(hlp_hi64size(map), WIDE_KEYS.len() as i32);
            for &k in &WIDE_KEYS {
                assert_eq!(unbox_i64(hlp_hi64get(map, k)), payload(k) + 1);
            }

            hlp_hi64clear(map);
            assert_eq!(hlp_hi64size(map), 0);
        }
    }

    /// Every prim guards a null handle. The resolver hands these out to
    /// bytecode, so an unset Map field must return the documented default
    /// rather than fault.
    #[test]
    fn null_map_handles_are_inert() {
        let _m = Mutator::enter();
        unsafe {
            let nil = ptr::null_mut();
            hlp_hi64set(nil, 1, dyn_i64(1));
            hlp_hi64clear(nil);
            assert!(!hlp_hi64exists(nil, 1));
            assert!(hlp_hi64get(nil, 1).is_null());
            assert!(!hlp_hi64remove(nil, 1));
            assert_eq!(hlp_hi64size(nil), 0);
            assert!(hlp_hi64keys(nil).is_null());
            assert!(hlp_hi64values(nil).is_null());
        }
    }

    /// A compile-time contract test. The prims are resolved by name with no
    /// prototype on the other side, so a key narrowed to `i32`, a dropped
    /// argument or a changed return would build and link cleanly and only show
    /// up as corruption at runtime. Naming all nine through their upstream
    /// signatures turns that into a compile error.
    #[test]
    fn every_prim_matches_its_upstream_signature() {
        let table: [*const c_void; 9] = [
            (hlp_hi64alloc as unsafe extern "C" fn() -> *mut c_void) as *const c_void,
            (hlp_hi64set as unsafe extern "C" fn(*mut c_void, i64, *mut vdynamic)) as *const c_void,
            (hlp_hi64exists as unsafe extern "C" fn(*mut c_void, i64) -> bool) as *const c_void,
            (hlp_hi64get as unsafe extern "C" fn(*mut c_void, i64) -> *mut vdynamic)
                as *const c_void,
            (hlp_hi64remove as unsafe extern "C" fn(*mut c_void, i64) -> bool) as *const c_void,
            (hlp_hi64keys as unsafe extern "C" fn(*mut c_void) -> *mut varray) as *const c_void,
            (hlp_hi64values as unsafe extern "C" fn(*mut c_void) -> *mut varray) as *const c_void,
            (hlp_hi64clear as unsafe extern "C" fn(*mut c_void)) as *const c_void,
            (hlp_hi64size as unsafe extern "C" fn(*mut c_void) -> i32) as *const c_void,
        ];
        assert!(table.iter().all(|p| !p.is_null()));
    }
}
