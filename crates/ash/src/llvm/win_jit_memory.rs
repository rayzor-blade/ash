//! One ascending region for every JIT section, on Windows x86-64.
//!
//! RuntimeDyld has no real image, so it fakes `__ImageBase` as the lowest
//! section address it has seen and memoises it on first use
//! (`RuntimeDyldCOFFX86_64.h`). Every JIT function carries `.pdata`/`.xdata`,
//! and each of those is three `IMAGE_REL_AMD64_ADDR32NB` relocations against
//! that value, which is fatal for an address below it or more than 4GB above.
//! Since the tiered JIT adds an object to the same MCJIT per promotion, and
//! LLVM's default manager lets the OS place their sections, a later object can
//! land under the frozen base.
//!
//! Serving every section from one reserved region, upward, is the layout
//! RuntimeDyld's own comment asks the memory manager to provide: the first
//! section sits at the region base, so that is what gets cached, and every
//! later section is above it and within the reservation.
//!
//! `place` is where that invariant lives, so it is plain arithmetic over the
//! region bounds and is compiled and tested on every platform. Only the calls
//! that reserve, commit and protect are Windows-only.

/// Sections are page-granular here, so the finalize pass can flip protection
/// on one without touching its neighbour.
const PAGE: usize = 4096;

/// Address space to reserve per engine. Reserving is not committing, so this
/// costs nothing until sections are actually allocated; it only has to exceed
/// the code a run will ever JIT, and stay under the 4GB the relocation can
/// encode. `ASH_JIT_REGION_MB` overrides it.
const DEFAULT_REGION_BYTES: usize = 512 * 1024 * 1024;

/// Below this a reservation is not worth retrying; see `reserve`.
const MIN_REGION_BYTES: usize = 16 * 1024 * 1024;

fn region_bytes() -> usize {
    match std::env::var("ASH_JIT_REGION_MB") {
        Ok(v) => match v.trim().parse::<usize>() {
            Ok(mb) if mb > 0 => (mb * 1024 * 1024).min(u32::MAX as usize),
            _ => DEFAULT_REGION_BYTES,
        },
        Err(_) => DEFAULT_REGION_BYTES,
    }
}

fn align_up(value: usize, align: usize) -> usize {
    debug_assert!(align.is_power_of_two());
    (value + align - 1) & !(align - 1)
}

/// What a section's pages become once the object is finalized.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Protect {
    ReadWrite,
    ReadOnly,
    Execute,
}

/// A bump allocator over one reservation, handed to MCJIT as its section
/// allocator.
#[derive(Debug)]
pub struct OrderedJitMemory {
    base: usize,
    reserved: usize,
    cursor: usize,
    /// Sections whose protection still has to be applied, as
    /// `(address, length, protection)`. Drained by each `finalize_memory`,
    /// which LLVM calls once per object rather than once per engine.
    pending: Vec<(usize, usize, Protect)>,
}

impl OrderedJitMemory {
    /// Where the next section goes, as `(address, committed length)`, or
    /// `None` when it would leave the region.
    ///
    /// Refusing is deliberate. Falling back to an allocation the OS places
    /// would put a section outside the region, which is the layout that
    /// aborts — and it would abort on some later run rather than this one.
    /// A `None` here reaches LLVM as an allocation failure against the module.
    fn place(&mut self, size: usize, alignment: u32) -> Option<(usize, usize)> {
        // At least a page, so the finalize pass can protect this section
        // without catching part of the next one. LLVM asks for alignments well
        // under a page, so this is the binding constraint.
        let align = (alignment as usize).max(PAGE).next_power_of_two();
        let start = align_up(self.cursor, align);
        let len = align_up(size.max(1), PAGE);
        let end = start.checked_add(len)?;
        if end > self.base.checked_add(self.reserved)? {
            return None;
        }
        self.cursor = end;
        Some((start, len))
    }

    /// A region at a known address, for tests that exercise `place` without
    /// asking the OS for anything.
    #[cfg(test)]
    fn at(base: usize, reserved: usize) -> Self {
        Self {
            base,
            reserved,
            cursor: base,
            pending: Vec::new(),
        }
    }
}

#[cfg(all(windows, target_arch = "x86_64"))]
mod imp {
    use super::{
        region_bytes, OrderedJitMemory, Protect, MIN_REGION_BYTES, PAGE,
    };
    use inkwell::memory_manager::McjitMemoryManager;
    use windows_sys::Win32::System::Diagnostics::Debug::FlushInstructionCache;
    use windows_sys::Win32::System::Memory::{
        VirtualAlloc, VirtualFree, VirtualProtect, MEM_COMMIT, MEM_RELEASE, MEM_RESERVE,
        PAGE_EXECUTE_READ, PAGE_NOACCESS, PAGE_PROTECTION_FLAGS, PAGE_READONLY, PAGE_READWRITE,
    };
    use windows_sys::Win32::System::Threading::GetCurrentProcess;

    fn flags(protect: Protect) -> PAGE_PROTECTION_FLAGS {
        match protect {
            Protect::ReadWrite => PAGE_READWRITE,
            Protect::ReadOnly => PAGE_READONLY,
            Protect::Execute => PAGE_EXECUTE_READ,
        }
    }

    impl OrderedJitMemory {
        /// Reserve a region, halving on failure. `None` means even the
        /// smallest attempt failed.
        pub fn reserve() -> Option<Self> {
            let mut bytes = region_bytes();
            while bytes >= MIN_REGION_BYTES {
                let base =
                    unsafe { VirtualAlloc(std::ptr::null(), bytes, MEM_RESERVE, PAGE_NOACCESS) };
                if !base.is_null() {
                    return Some(Self {
                        base: base as usize,
                        reserved: bytes,
                        cursor: base as usize,
                        pending: Vec::new(),
                    });
                }
                bytes /= 2;
            }
            None
        }

        fn allocate(&mut self, size: usize, alignment: u32, protect: Protect) -> *mut u8 {
            let Some((start, len)) = self.place(size, alignment) else {
                return std::ptr::null_mut();
            };
            let got = unsafe { VirtualAlloc(start as *const _, len, MEM_COMMIT, PAGE_READWRITE) };
            if got.is_null() {
                return std::ptr::null_mut();
            }
            if protect != Protect::ReadWrite {
                self.pending.push((start, len, protect));
            }
            got as *mut u8
        }
    }

    impl McjitMemoryManager for OrderedJitMemory {
        fn allocate_code_section(
            &mut self,
            size: libc::uintptr_t,
            alignment: libc::c_uint,
            _section_id: libc::c_uint,
            _section_name: &str,
        ) -> *mut u8 {
            self.allocate(size, alignment, Protect::Execute)
        }

        fn allocate_data_section(
            &mut self,
            size: libc::uintptr_t,
            alignment: libc::c_uint,
            _section_id: libc::c_uint,
            _section_name: &str,
            is_read_only: bool,
        ) -> *mut u8 {
            let protect = if is_read_only {
                Protect::ReadOnly
            } else {
                Protect::ReadWrite
            };
            self.allocate(size, alignment, protect)
        }

        fn finalize_memory(&mut self) -> Result<(), String> {
            for (addr, len, protect) in self.pending.drain(..) {
                let mut previous: PAGE_PROTECTION_FLAGS = 0;
                let ok = unsafe {
                    VirtualProtect(addr as *const _, len, flags(protect), &mut previous)
                };
                if ok == 0 {
                    return Err(format!(
                        "VirtualProtect({addr:#x}, {len}, {protect:?}) failed: {}",
                        std::io::Error::last_os_error()
                    ));
                }
            }
            // These pages were written as data and are about to be executed.
            unsafe { FlushInstructionCache(GetCurrentProcess(), std::ptr::null(), 0) };
            Ok(())
        }

        fn destroy(&mut self) {
            // Only reached when LLVM tears the engine down, at which point the
            // code in here is already unreachable.
            if self.base != 0 {
                unsafe { VirtualFree(self.base as *mut _, 0, MEM_RELEASE) };
                self.base = 0;
                self.cursor = 0;
                self.reserved = 0;
            }
        }
    }

    // Referenced only by the allocation paths above; named here so a build
    // that changes the page size notices both places.
    const _: () = assert!(PAGE == 4096);
}

#[cfg(test)]
mod tests {
    use super::*;

    const BASE: usize = 0x0000_4000_0000_0000;
    const SIZE: usize = 512 * 1024 * 1024;

    /// The invariant the abort turns on: whatever RuntimeDyld caches as
    /// `__ImageBase` from the first section stays the minimum, and every later
    /// section lands within a 32-bit offset of it.
    #[test]
    fn sections_only_ever_go_up_and_stay_in_ADDR32NB_range() {
        let mut mm = OrderedJitMemory::at(BASE, SIZE);
        let (image_base, _) = mm.place(64, 16).expect("first section");
        assert_eq!(image_base, BASE, "the first section must sit at the base");

        let mut previous = image_base;
        for id in 1..2048 {
            // A promotion's worth: code, the unwind data that carries the
            // relocations, and a writable section.
            for (size, align) in [(4096, 16), (128, 8), (128, 8)] {
                let (addr, len) = mm.place(size, align).unwrap_or_else(|| {
                    panic!("section {id} refused with {} bytes left", SIZE - (previous - BASE))
                });
                assert!(addr >= image_base, "section {id} landed below ImageBase");
                assert!(addr >= previous, "section {id} did not advance");
                assert!(
                    addr - image_base <= u32::MAX as usize,
                    "section {id} is out of ADDR32NB range"
                );
                assert_eq!(addr % PAGE, 0, "section {id} is not page aligned");
                assert!(len >= size, "section {id} was given less than it asked");
                previous = addr + len;
            }
        }
    }

    /// Two sections never share a page, or protecting one would change the
    /// other's.
    #[test]
    fn sections_never_share_a_page() {
        let mut mm = OrderedJitMemory::at(BASE, SIZE);
        let (a, a_len) = mm.place(1, 1).unwrap();
        let (b, _) = mm.place(1, 1).unwrap();
        assert_eq!(a_len, PAGE, "a one-byte section still takes a whole page");
        assert!(b >= a + a_len, "the second section overlapped the first");
    }

    /// Exhaustion refuses rather than escaping the region, since a section
    /// placed outside it is the layout that aborts.
    #[test]
    fn a_full_region_refuses_instead_of_escaping() {
        let mut mm = OrderedJitMemory::at(BASE, 4 * PAGE);
        assert!(mm.place(PAGE, 16).is_some());
        assert!(mm.place(PAGE, 16).is_some());
        assert!(mm.place(PAGE, 16).is_some());
        assert!(mm.place(PAGE, 16).is_some());
        assert!(
            mm.place(1, 16).is_none(),
            "allocated past the end of the region"
        );
    }

    /// An alignment larger than a page still cannot walk backwards or out.
    #[test]
    fn an_oversized_alignment_stays_inside() {
        let mut mm = OrderedJitMemory::at(BASE, SIZE);
        let (first, _) = mm.place(64, 16).unwrap();
        let (aligned, _) = mm.place(64, 1 << 20).unwrap();
        assert!(aligned > first);
        assert_eq!(aligned % (1 << 20), 0);
        assert!(aligned - BASE <= u32::MAX as usize);
    }

    /// A size that would overflow the address space is refused, not wrapped.
    #[test]
    fn an_absurd_size_is_refused() {
        let mut mm = OrderedJitMemory::at(BASE, SIZE);
        assert!(mm.place(usize::MAX - PAGE, 16).is_none());
    }

    /// The override is bounded by what the relocation can encode.
    #[test]
    fn the_region_never_exceeds_addr32nb_range() {
        assert!(region_bytes() <= u32::MAX as usize);
        assert!(DEFAULT_REGION_BYTES <= u32::MAX as usize);
    }
}
