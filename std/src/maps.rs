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
    if std::env::var("ASH_MAP_TRACE").is_ok() {
        let k = if key.is_null() { String::new() } else {
            let mut out = String::new(); let mut p = key;
            while *p != 0 { out.push(char::from_u32(*p as u32).unwrap_or('?')); p = p.add(1); }
            out
        };
        eprintln!("[hbset] map={:#x} key={k:?} val={:#x}", m as usize, value as usize);
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
        let src = ((*m).cells as *const i8).wrapping_add(ckey as usize);
        let dst = ((*m).nexts as *mut i8).wrapping_add(c as usize);
        ptr::write(dst, ptr::read(src));
        ptr::write(((*m).cells as *mut i8).wrapping_add(ckey as usize), c as i8);
    } else {
        let src = ((*m).cells as *const i32).wrapping_add(ckey as usize);
        let dst = ((*m).nexts as *mut i32).wrapping_add(c as usize);
        ptr::write(dst, ptr::read(src));
        ptr::write(
            ((*m).cells as *mut i32).wrapping_add(ckey as usize),
            c,
        );
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
    let resize_trace = std::env::var("ASH_MAP_RESIZE_TRACE").is_ok();

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
        .expect("Out of memory")
        .as_ptr() as *mut hl::hl_hb_entry;
    (*m).values = crate::gc::gc_locked()
        .allocate(nentries as usize * mem::size_of::<hl::hl_hb_value>())
        .expect("Out of memory")
        .as_ptr() as *mut hl::hl_hb_value;
    (*m).maxentries = nentries;

    if old.ncells == ncells && (nentries < _MLIMIT || old.maxentries >= _MLIMIT) {
        // simply expand
        (*m).nexts = crate::gc::gc_locked()
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
        (*m).cells = crate::gc::gc_locked()
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
                        ((*m).cells as *mut i8).wrapping_add(ckey as usize),
                        next as i8,
                    );
                } else {
                    ptr::write(((*m).cells as *mut i32).wrapping_add(ckey as usize), next);
                }
            } else {
                // Middle/end of chain: nexts[prev] = next
                if (*m).maxentries < _MLIMIT {
                    ptr::write(
                        ((*m).nexts as *mut i8).wrapping_add(prev as usize),
                        next as i8,
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
            *((*m).cells as *const i8).add(i as usize) as i32
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
            *((*m).cells as *const i8).add(i as usize) as i32
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
// IntMap (hi*) — HashMap<i32, *mut vdynamic>
// ============================================================================

use std::collections::HashMap;
type IntMap = HashMap<i32, *mut hl::vdynamic>;

#[no_mangle]
pub unsafe extern "C" fn hlp_hialloc() -> *mut c_void {
    Box::into_raw(Box::new(IntMap::new())) as *mut c_void
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hiset(m: *mut c_void, key: i32, value: *mut hl::vdynamic) {
    if !m.is_null() {
        (*(m as *mut IntMap)).insert(key, value);
    }
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hiexists(m: *mut c_void, key: i32) -> bool {
    if m.is_null() {
        return false;
    }
    (*(m as *const IntMap)).contains_key(&key)
}
#[no_mangle]
pub unsafe extern "C" fn hlp_higet(m: *mut c_void, key: i32) -> *mut hl::vdynamic {
    if m.is_null() {
        return ptr::null_mut();
    }
    (*(m as *const IntMap))
        .get(&key)
        .copied()
        .unwrap_or(ptr::null_mut())
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hiremove(m: *mut c_void, key: i32) -> bool {
    if m.is_null() {
        return false;
    }
    (*(m as *mut IntMap)).remove(&key).is_some()
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hikeys(m: *mut c_void) -> *mut hl::varray {
    if m.is_null() {
        return ptr::null_mut();
    }
    let map = &*(m as *const IntMap);
    let arr = crate::array::hlp_alloc_array(crate::types::hlt_i32(), map.len() as i32);
    for (i, &key) in map.keys().enumerate() {
        *(crate::types::hl_aptr::<i32>(arr)).add(i) = key;
    }
    arr
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hivalues(m: *mut c_void) -> *mut hl::varray {
    if m.is_null() {
        return ptr::null_mut();
    }
    let map = &*(m as *const IntMap);
    let hlt_dyn = crate::types::hlt_dyn();
    let arr = crate::array::hlp_alloc_array(hlt_dyn, map.len() as i32);
    for (i, &val) in map.values().enumerate() {
        *(crate::types::hl_aptr::<*mut hl::vdynamic>(arr)).add(i) = val;
    }
    arr
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hiclear(m: *mut c_void) {
    if !m.is_null() {
        (*(m as *mut IntMap)).clear();
    }
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hisize(m: *mut c_void) -> i32 {
    if m.is_null() {
        0
    } else {
        (*(m as *const IntMap)).len() as i32
    }
}

// ============================================================================
// ObjectMap (ho*) — HashMap<usize, *mut vdynamic>
// ============================================================================
type ObjMap = HashMap<usize, *mut hl::vdynamic>;

#[no_mangle]
pub unsafe extern "C" fn hlp_hoalloc() -> *mut c_void {
    Box::into_raw(Box::new(ObjMap::new())) as *mut c_void
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hoset(m: *mut c_void, key: *mut hl::vdynamic, val: *mut hl::vdynamic) {
    if !m.is_null() && !key.is_null() {
        (*(m as *mut ObjMap)).insert(key as usize, val);
    }
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hoexists(m: *mut c_void, key: *mut hl::vdynamic) -> bool {
    if m.is_null() || key.is_null() {
        return false;
    }
    (*(m as *const ObjMap)).contains_key(&(key as usize))
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hoget(m: *mut c_void, key: *mut hl::vdynamic) -> *mut hl::vdynamic {
    if m.is_null() || key.is_null() {
        return ptr::null_mut();
    }
    (*(m as *const ObjMap))
        .get(&(key as usize))
        .copied()
        .unwrap_or(ptr::null_mut())
}
#[no_mangle]
pub unsafe extern "C" fn hlp_horemove(m: *mut c_void, key: *mut hl::vdynamic) -> bool {
    if m.is_null() || key.is_null() {
        return false;
    }
    (*(m as *mut ObjMap)).remove(&(key as usize)).is_some()
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hokeys(m: *mut c_void) -> *mut hl::varray {
    if m.is_null() {
        return ptr::null_mut();
    }
    let map = &*(m as *const ObjMap);
    let hlt_dyn = crate::types::hlt_dyn();
    let arr = crate::array::hlp_alloc_array(hlt_dyn, map.len() as i32);
    for (i, &key) in map.keys().enumerate() {
        *(crate::types::hl_aptr::<*mut hl::vdynamic>(arr)).add(i) = key as *mut hl::vdynamic;
    }
    arr
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hovalues(m: *mut c_void) -> *mut hl::varray {
    if m.is_null() {
        return ptr::null_mut();
    }
    let map = &*(m as *const ObjMap);
    let hlt_dyn = crate::types::hlt_dyn();
    let arr = crate::array::hlp_alloc_array(hlt_dyn, map.len() as i32);
    for (i, &val) in map.values().enumerate() {
        *(crate::types::hl_aptr::<*mut hl::vdynamic>(arr)).add(i) = val;
    }
    arr
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hoclear(m: *mut c_void) {
    if !m.is_null() {
        (*(m as *mut ObjMap)).clear();
    }
}
#[no_mangle]
pub unsafe extern "C" fn hlp_hosize(m: *mut c_void) -> i32 {
    if m.is_null() {
        0
    } else {
        (*(m as *const ObjMap)).len() as i32
    }
}
