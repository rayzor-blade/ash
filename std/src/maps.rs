use std::{
    mem,
    os::raw::c_void,
    ptr::{self, NonNull},
};

use crate::{
    gc::{ImmixAllocator, GC},
    hl::{self, hl_hb_map, uchar},
    strings,
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

    fn mark_map(&mut self, map_ptr: *mut hl::hl_hb_map) {
        if map_ptr.is_null() {
            return;
        }

        // Mark the hl_hb_map struct itself
        self.mark_memory(map_ptr as *mut u8, std::mem::size_of::<hl::hl_hb_map>());

        unsafe {
            let map = &*map_ptr;

            // Mark the cells and nexts arrays
            if !map.cells.is_null() {
                self.mark_memory(
                    map.cells as *mut u8,
                    map.ncells as usize * std::mem::size_of::<*mut std::os::raw::c_void>(),
                );
            }
            if !map.nexts.is_null() {
                self.mark_memory(
                    map.nexts as *mut u8,
                    map.ncells as usize * std::mem::size_of::<*mut std::os::raw::c_void>(),
                );
            }

            // Mark the entries array
            if !map.entries.is_null() {
                self.mark_memory(
                    map.entries as *mut u8,
                    map.maxentries as usize * std::mem::size_of::<hl::hl_hb_entry>(),
                );
            }

            // Mark the values array and its contents
            if !map.values.is_null() {
                self.mark_memory(
                    map.values as *mut u8,
                    map.maxentries as usize * std::mem::size_of::<hl::hl_hb_value>(),
                );

                for i in 0..map.nentries {
                    let value = &*map.values.add(i as usize);

                    // Mark the key (assuming it's a string)
                    if !value.key.is_null() {
                        let key_len = strings::hlp_utf16_length(value.key);
                        self.mark_memory(
                            value.key as *mut u8,
                            key_len * std::mem::size_of::<hl::uchar>(),
                        );
                    }

                    // Mark the value (assuming it's a vdynamic)
                    if !value.value.is_null() {
                        self.mark_vdynamic(value.value);
                    }
                }
            }

            // Mark the free list
            if !map.lfree.buckets.is_null() {
                self.mark_memory(
                    map.lfree.buckets as *mut u8,
                    map.lfree.nbuckets as usize * std::mem::size_of::<hl::hl_free_bucket>(),
                );
            }
        }
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
    let new_buckets = GC
        .get_mut()
        .expect("Expected to get GC")
        .allocate(mem::size_of::<hl::hl_free_bucket>() * new_size as usize)
        .expect("Out of memory")
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
                ((*(*self)).cells as *const i8).add(ckey as usize).read() as i32
            } else {
                ((*(*self)).cells as *const i32).add(ckey as usize).read()
            }
        }
    }

    fn m_next(&self, ckey: u32) -> i32 {
        unsafe {
            if (**self).maxentries < _MLIMIT {
                ((*(*self)).nexts as *const i8).add(ckey as usize).read() as i32
            } else {
                ((*(*self)).nexts as *const i32).add(ckey as usize).read()
            }
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_hballoc() -> *mut hl::hl_hb_map {
    let allocator = GC.get_mut().expect("expected to get garbage collector");
    let allocated_map = allocator
        .allocate_map(0)
        .expect("could not allocate bytes map");
    allocator.mark_map(allocated_map.as_ptr());
    allocated_map.as_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_hbset(
    mut m: *mut hl::hl_hb_map,
    key: *mut hl::uchar,
    value: *mut hl::vdynamic,
) {
    use hl_hb::HbMap;

    let mut c = 0;
    let mut ckey = 0;
    let hash = hl_hb::hb_hash(key);
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
        let src = ((*m).cells as *const i8).wrapping_add(ckey as usize);
        let dst = ((*m).nexts as *mut i8).wrapping_add(c as usize);
        ptr::write(dst, ptr::read(src));
        ptr::write(((*m).cells as *mut i8).wrapping_add(ckey as usize), c as i8);
    } else {
        let src = ((*m).cells as *const i32).wrapping_add(ckey as usize);
        let dst = ((*m).nexts as *mut i32).wrapping_add(c as usize);
        ptr::write(dst, ptr::read(src));
        ptr::write(((*m).cells as *mut i32).wrapping_add(ckey as usize), c as i32);
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
    (*m).entries = GC
        .get_mut()
        .expect("expected to get garbage collector")
        .allocate(nentries as usize * mem::size_of::<hl::hl_hb_entry>())
        .expect("Out of memory")
        .as_ptr() as *mut hl::hl_hb_entry;
    (*m).values = GC
        .get_mut()
        .expect("expected to get garbage collector")
        .allocate(nentries as usize * mem::size_of::<hl::hl_hb_value>())
        .expect("Out of memory")
        .as_ptr() as *mut hl::hl_hb_value;
    (*m).maxentries = nentries;

    if old.ncells == ncells && (nentries < _MLIMIT || old.maxentries >= _MLIMIT) {
        // simply expand
        (*m).nexts = GC
            .get_mut()
            .expect("expected to get garbage collector")
            .allocate(nentries as usize * ksize)
            .expect("Out of memory")
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
        (*m).cells = GC
            .get_mut()
            .expect("expected to get garbage collector")
            .allocate((ncells + nentries) as usize * ksize)
            .expect("Out of memory")
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
                *(old.cells as *const i8).add(i as usize) as i32
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
}

unsafe fn get_key(m: *mut hl::hl_hb_map, c: i32) -> *mut hl::uchar {
    (*((*m).values.add(c as usize))).key
}

// #[no_mangle]
// pub unsafe extern "C" fn hlp_hb_get(m:*mut hl::hl_hb_map, key:*mut hl::uchar) -> *mut hl::vdynamic  {

// }
