use std::{ffi::c_int, mem, ptr::NonNull};

use crate::{gc::{ImmixAllocator, GC}, hl};


impl ImmixAllocator {
    fn allocate_rnd(&mut self) -> Option<NonNull<hl::rnd>> {
        let rnd_size = mem::size_of::<hl::rnd>();
        let rnd_ptr = self.allocate(rnd_size)?;

        // Initialize the rnd struct
        unsafe {
            let rnd = &mut *(rnd_ptr.as_ptr() as *mut hl::rnd);
            rnd.seeds = [0; 25]; // Initialize all seeds to 0
            rnd.cur = 0; // Initialize cur to 0
        }

        // Cast the pointer to the correct type and return
        NonNull::new(rnd_ptr.as_ptr() as *mut hl::rnd)
    }

    fn mark_rnd(&mut self, rnd_ptr: *mut hl::rnd) {
        if rnd_ptr.is_null() {
            return;
        }

        // Mark the memory occupied by the rnd struct
        self.mark_memory(rnd_ptr as *mut u8, mem::size_of::<hl::rnd>());
    }
}


pub static MAG01: &[::std::os::raw::c_ulong] = &[
    0x0, 0x8ebfd028, // magic, don't change
];

pub static INIT_SEEDS: &[::std::os::raw::c_ulong] = &[
    0x95f24dab, 0x0b685215, 0xe76ccae7, 0xaf3ec239, 0x715fad23, 0x24a590ad, 0x69e4b5ef, 0xbf456141,
    0x96bc1b7b, 0xa7bdf825, 0xc1de75b7, 0x8858a9c9, 0x2da87693, 0xb657f9dd, 0xffdc8a9f, 0x8121da71,
    0x8b823ecb, 0x885d05f5, 0x4e20cd47, 0x5a9ad5d9, 0x512c0c03, 0xea857ccd, 0x4cc1d30f, 0x8891a8a1,
    0xa6b7aadb,
];

#[no_mangle]
pub unsafe extern "C" fn hlp_rnd_alloc() -> *mut hl::rnd {
    let allocator = GC.get_mut().expect("expected to get garbage collector");
    let allocated_rnd = allocator.allocate_rnd().expect("could not allocate hl::rnd");
    allocator.mark_rnd(allocated_rnd.as_ptr());
    allocated_rnd.as_ptr()
}
#[no_mangle]
pub unsafe extern "C" fn hlp_rnd_init_system() -> *mut hl::rnd {
    let r = hlp_rnd_alloc();
    let pid = std::process::id();
    let now = std::time::SystemTime::now();
    let elapsed  = now.elapsed().expect("expected to get elapsed system time");
    let time:u32 = (elapsed.as_secs() as u32 * 1000000 * elapsed.as_micros() as u32).into();
    hlp_rnd_set_seed(r, (time ^ (pid | (pid << 16))) as i32);
    r
}
#[no_mangle]
pub unsafe extern "C" fn hlp_rnd_set_seed(r: *mut hl::rnd, s:c_int) {
    if r.is_null() {
        return;
    }

    let _r = &mut *r;
    _r.cur = 0;
    _r.seeds.copy_from_slice(&INIT_SEEDS[..]);
    for i in 0..hl::NSEEDS {
        _r.seeds[i as usize] ^= s as u64;
    }
}