use std::{
    ffi::{c_int, c_uint},
    mem,
    ptr::NonNull,
};

use crate::{gc::ImmixAllocator, hl};

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
    let mut allocator = crate::gc::gc_locked();
    let allocated_rnd = allocator
        .allocate_rnd()
        .expect("could not allocate hl::rnd");
    allocator.mark_rnd(allocated_rnd.as_ptr());
    allocated_rnd.as_ptr()
}
#[no_mangle]
pub unsafe extern "C" fn hlp_rnd_init_system() -> *mut hl::rnd {
    let r = hlp_rnd_alloc();
    let pid = std::process::id();
    // Upstream mixes gettimeofday's microsecond clock with the pid; SystemTime
    // is the portable spelling of that same wall clock. A clock reading before
    // the epoch seeds with 0 rather than raising: every value is a legal seed,
    // and this call has no failure channel back to the VM.
    let time = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| {
            (d.as_secs() as u32)
                .wrapping_mul(1_000_000)
                .wrapping_add(d.subsec_micros())
        })
        .unwrap_or(0);
    hlp_rnd_set_seed(r, (time ^ (pid | (pid << 16))) as i32);
    r
}
#[no_mangle]
pub unsafe extern "C" fn hlp_rnd_set_seed(r: *mut hl::rnd, s: c_int) {
    if r.is_null() {
        return;
    }

    let _r = &mut *r;
    _r.cur = 0;
    _r.seeds.copy_from_slice(INIT_SEEDS);
    for i in 0..hl::NSEEDS {
        // seeds is `unsigned long`, which is 32-bit on MSVC and 64-bit on unix;
        // widen through c_ulong so the seed mixes at the platform's width like
        // the C `r->seeds[i] ^= s` does, instead of a hardcoded u64.
        _r.seeds[i as usize] ^= s as ::std::os::raw::c_ulong;
    }
}

/// Upstream hl_rnd_int (random.c): TGFSR step over the 25-word seed table,
/// tempered into a 32-bit result. `unsigned long` on the seeds is the C
/// declaration, so the table mixes at 64 bits on unix and 32 on MSVC exactly
/// as upstream does; only the tempering is fixed at 32 bits.
#[no_mangle]
pub unsafe extern "C" fn hlp_rnd_int(r: *mut hl::rnd) -> c_uint {
    if r.is_null() {
        return 0;
    }
    const N: usize = hl::NSEEDS as usize;
    const M: usize = hl::MAX as usize;

    let _r = &mut *r;
    let mut pos = _r.cur as usize;
    _r.cur = _r.cur.wrapping_add(1);
    if pos >= N {
        let mut kk = 0;
        while kk < N - M {
            _r.seeds[kk] =
                _r.seeds[kk + M] ^ (_r.seeds[kk] >> 1) ^ MAG01[(_r.seeds[kk] % 2) as usize];
            kk += 1;
        }
        while kk < N {
            _r.seeds[kk] =
                _r.seeds[kk + M - N] ^ (_r.seeds[kk] >> 1) ^ MAG01[(_r.seeds[kk] % 2) as usize];
            kk += 1;
        }
        _r.cur = 1;
        pos = 0;
    }

    let mut y = _r.seeds[pos] as c_uint;
    y ^= (y << 7) & 0x2b5b_2500;
    y ^= (y << 15) & 0xdb8b_0000;
    y ^= y >> 16;
    y
}

/// Upstream hl_rnd_float (random.c): three 32-bit draws folded into the
/// [0,1) mantissa, most significant draw last.
#[no_mangle]
pub unsafe extern "C" fn hlp_rnd_float(r: *mut hl::rnd) -> f64 {
    const BIG: f64 = 4294967296.0;
    let a = hlp_rnd_int(r) as f64;
    let b = hlp_rnd_int(r) as f64;
    let c = hlp_rnd_int(r) as f64;
    ((a / BIG + b) / BIG + c) / BIG
}
