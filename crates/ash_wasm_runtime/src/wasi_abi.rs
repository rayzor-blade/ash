//! WASI preview 1, the parts that are arithmetic rather than I/O.
//!
//! Decoding an `iovec` array, laying out an `fdstat`, knowing that `spipe` is
//! 70: none of that depends on what a host can actually do, and all of it is
//! easy to get subtly wrong in a way that shows up as a program misbehaving
//! rather than as an error. So it lives here, outside any host, where it is
//! ordinary Rust that ordinary tests can reach -- rather than inside the
//! browser host, where checking it would mean a browser.
//!
//! Every layout here is the one preview 1 specifies: little-endian, and
//! padded to the alignment of its widest field.

/// The errno numbers preview 1 assigns, in its own order. A guest compares
/// against these values, so they are the platform's rather than ours.
pub mod errno {
    pub const SUCCESS: i32 = 0;
    pub const BADF: i32 = 8;
    pub const EXIST: i32 = 20;
    pub const FAULT: i32 = 21;
    pub const INVAL: i32 = 28;
    pub const IO: i32 = 29;
    pub const ISDIR: i32 = 31;
    pub const NOENT: i32 = 44;
    pub const NOSYS: i32 = 52;
    pub const NOTDIR: i32 = 54;
    pub const NOTSUP: i32 = 58;
    pub const PERM: i32 = 63;
    /// Seeking a pipe, which is what the standard streams are.
    pub const SPIPE: i32 = 70;
}

/// `filetype`, of which a host without a filesystem needs three.
pub mod filetype {
    pub const UNKNOWN: u8 = 0;
    pub const CHARACTER_DEVICE: u8 = 2;
    pub const DIRECTORY: u8 = 3;
}

/// `clockid`.
pub mod clock {
    pub const REALTIME: u32 = 0;
    pub const MONOTONIC: u32 = 1;
}

/// One `iovec`: a pointer and a length, both `u32`, in guest memory.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IoVec {
    pub ptr: u32,
    pub len: u32,
}

/// Size of one `iovec`, and of one `ciovec`, which is the same shape.
pub const IOVEC_SIZE: usize = 8;

/// Read an `iovec` array from the bytes the array itself occupies.
///
/// The region rather than the whole of linear memory, because a host that has
/// to copy out of the guest should copy the eight bytes per vector it needs
/// and not the heap. `None` means the region is not the size it claims.
pub fn parse_iovecs(region: &[u8], count: u32) -> Option<Vec<IoVec>> {
    let bytes = (count as usize).checked_mul(IOVEC_SIZE)?;
    if region.len() < bytes {
        return None;
    }
    let mut out = Vec::with_capacity(count as usize);
    for i in 0..count as usize {
        let base = i * IOVEC_SIZE;
        out.push(IoVec {
            ptr: u32::from_le_bytes(region[base..base + 4].try_into().ok()?),
            len: u32::from_le_bytes(region[base + 4..base + 8].try_into().ok()?),
        });
    }
    Some(out)
}

/// Everything an `iovec` array points at, end to end.
///
/// A write is one write however many pieces it arrived in, which is what
/// makes a line of output a line rather than several. `read` fetches one
/// range out of the guest and answers `None` if it is not inside it, which is
/// the guest's mistake and reported as `EFAULT`.
pub fn gather(vecs: &[IoVec], mut read: impl FnMut(u32, u32) -> Option<Vec<u8>>) -> Option<Vec<u8>> {
    let mut out = Vec::new();
    for v in vecs {
        out.extend_from_slice(&read(v.ptr, v.len)?);
    }
    Some(out)
}

/// Size of an `fdstat`, whose widest field is a `u64`.
pub const FDSTAT_SIZE: usize = 24;

/// An `fdstat`: what kind of thing a descriptor is and what may be done with
/// it. `libc` reads this to decide whether a descriptor is a terminal, which
/// decides whether output is line-buffered.
pub fn fdstat(filetype: u8, flags: u16, rights_base: u64, rights_inheriting: u64) -> [u8; 24] {
    let mut out = [0u8; FDSTAT_SIZE];
    out[0] = filetype;
    out[2..4].copy_from_slice(&flags.to_le_bytes());
    out[8..16].copy_from_slice(&rights_base.to_le_bytes());
    out[16..24].copy_from_slice(&rights_inheriting.to_le_bytes());
    out
}

/// Size of a `filestat`.
/// One `subscription` handed to `poll_oneoff`, and one `event` given back.
///
/// The event is the half that matters and the half easiest to get wrong: a
/// host that reports "n events" without writing them leaves the caller
/// reading whatever was in that memory as the event's errno. wasi-libc's
/// `nanosleep` does exactly that, and Rust's `thread::sleep` asserts the
/// result is either success or `EINTR` -- so uninitialised memory becomes a
/// panic in the guest with a number that means nothing.
pub mod poll {
    /// Bytes per `subscription`. `userdata` at 0, the union's tag at 8, and
    /// the union's contents from 16.
    pub const SUBSCRIPTION_SIZE: usize = 48;
    /// Bytes per `event`.
    pub const EVENT_SIZE: usize = 32;

    /// `eventtype`: what a subscription is waiting for.
    pub const CLOCK: u8 = 0;
    pub const FD_READ: u8 = 1;
    pub const FD_WRITE: u8 = 2;

    /// `subclockflags`: the timeout is an absolute time rather than a delay.
    pub const ABSTIME: u16 = 1;

    /// What one subscription asks for.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Subscription {
        pub userdata: u64,
        pub eventtype: u8,
        /// Which clock, for a clock subscription.
        pub clock_id: u32,
        /// Nanoseconds: a delay, or an absolute time when `ABSTIME` is set.
        pub timeout: u64,
        pub flags: u16,
    }

    /// Read one, or `None` if the bytes are short.
    pub fn subscription(bytes: &[u8]) -> Option<Subscription> {
        let at8 = |at: usize| -> Option<u64> {
            Some(u64::from_le_bytes(bytes.get(at..at + 8)?.try_into().ok()?))
        };
        let at4 = |at: usize| -> Option<u32> {
            Some(u32::from_le_bytes(bytes.get(at..at + 4)?.try_into().ok()?))
        };
        let at2 = |at: usize| -> Option<u16> {
            Some(u16::from_le_bytes(bytes.get(at..at + 2)?.try_into().ok()?))
        };
        Some(Subscription {
            userdata: at8(0)?,
            eventtype: *bytes.get(8)?,
            clock_id: at4(16)?,
            timeout: at8(24)?,
            flags: at2(40)?,
        })
    }

    /// The event answering one subscription. `error` of zero is what a caller
    /// reads as "this happened".
    pub fn event(userdata: u64, error: u16, eventtype: u8) -> [u8; EVENT_SIZE] {
        let mut out = [0u8; EVENT_SIZE];
        out[0..8].copy_from_slice(&userdata.to_le_bytes());
        out[8..10].copy_from_slice(&error.to_le_bytes());
        out[10] = eventtype;
        out
    }
}

pub const FILESTAT_SIZE: usize = 64;

/// A `filestat` for something that is not a file: no device, no inode, no
/// size, and the three timestamps zero.
pub fn filestat(filetype: u8) -> [u8; 64] {
    let mut out = [0u8; FILESTAT_SIZE];
    // dev 0..8, ino 8..16, filetype at 16, nlink 24..32, size 32..40,
    // atim 40..48, mtim 48..56, ctim 56..64.
    out[16] = filetype;
    out[24..32].copy_from_slice(&1u64.to_le_bytes());
    out
}

/// Lay out `args_get`-style vectors: the pointers, then the bytes they point
/// at, each NUL-terminated.
///
/// Returns the pointer array and the byte block, which the caller writes to
/// the two addresses `args_get` was given. `args_sizes_get` must have
/// answered with `items.len()` and the block's length, or the guest will have
/// allocated the wrong amount -- so both come from here.
pub fn vectors(items: &[String], block_at: u32) -> (Vec<u8>, Vec<u8>) {
    let mut pointers = Vec::with_capacity(items.len() * 4);
    let mut block = Vec::new();
    for item in items {
        pointers.extend_from_slice(&(block_at + block.len() as u32).to_le_bytes());
        block.extend_from_slice(item.as_bytes());
        block.push(0);
    }
    (pointers, block)
}

/// The two numbers `args_sizes_get` and `environ_sizes_get` answer.
pub fn vector_sizes(items: &[String]) -> (u32, u32) {
    let bytes: usize = items.iter().map(|i| i.len() + 1).sum();
    (items.len() as u32, bytes as u32)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A reader over a byte vector, standing in for the guest's memory.
    fn reader(memory: &[u8]) -> impl FnMut(u32, u32) -> Option<Vec<u8>> + '_ {
        move |ptr, len| {
            let start = ptr as usize;
            let end = start.checked_add(len as usize)?;
            memory.get(start..end).map(<[u8]>::to_vec)
        }
    }

    #[test]
    fn an_iovec_array_is_read_little_endian() {
        let mut region = vec![0u8; 16];
        region[0..4].copy_from_slice(&16u32.to_le_bytes());
        region[4..8].copy_from_slice(&3u32.to_le_bytes());
        region[8..12].copy_from_slice(&20u32.to_le_bytes());
        region[12..16].copy_from_slice(&2u32.to_le_bytes());
        assert_eq!(
            parse_iovecs(&region, 2).unwrap(),
            vec![IoVec { ptr: 16, len: 3 }, IoVec { ptr: 20, len: 2 }]
        );
    }

    /// A region shorter than it claims is the guest's mistake, and saying so
    /// is the difference between EFAULT and reading someone else's bytes.
    #[test]
    fn a_short_iovec_region_is_refused() {
        assert!(parse_iovecs(&[0u8; 8], 2).is_none());
        assert!(parse_iovecs(&[0u8; 8], u32::MAX).is_none());
    }

    /// Several pieces are one write, or a line arrives as several lines.
    #[test]
    fn gather_joins_the_pieces() {
        let mut memory = vec![0u8; 32];
        memory[16..19].copy_from_slice(b"hi ");
        memory[20..22].copy_from_slice(b"yo");
        let vecs = [IoVec { ptr: 16, len: 3 }, IoVec { ptr: 20, len: 2 }];
        assert_eq!(gather(&vecs, reader(&memory)).unwrap(), b"hi yo");
    }

    #[test]
    fn gather_refuses_a_piece_outside_memory() {
        let memory = vec![0u8; 8];
        assert!(gather(&[IoVec { ptr: 4, len: 99 }], reader(&memory)).is_none());
    }

    /// The fields libc actually reads, at the offsets it reads them from.
    #[test]
    fn an_fdstat_puts_its_fields_where_preview_one_says() {
        let s = fdstat(filetype::CHARACTER_DEVICE, 1, 0x2A, 0x2B);
        assert_eq!(s[0], filetype::CHARACTER_DEVICE);
        assert_eq!(u16::from_le_bytes([s[2], s[3]]), 1);
        assert_eq!(u64::from_le_bytes(s[8..16].try_into().unwrap()), 0x2A);
        assert_eq!(u64::from_le_bytes(s[16..24].try_into().unwrap()), 0x2B);
    }

    #[test]
    fn a_filestat_reports_its_type_at_offset_sixteen() {
        let s = filestat(filetype::DIRECTORY);
        assert_eq!(s[16], filetype::DIRECTORY);
        assert_eq!(s.len(), FILESTAT_SIZE);
    }

    /// The sizes and the layout have to agree, or the guest allocates one
    /// amount and is handed another.
    #[test]
    fn vectors_match_the_sizes_reported_for_them() {
        let items = vec!["prog".to_string(), "a".to_string()];
        let (count, bytes) = vector_sizes(&items);
        let (pointers, block) = vectors(&items, 100);
        assert_eq!(count as usize * 4, pointers.len());
        assert_eq!(bytes as usize, block.len());
        assert_eq!(u32::from_le_bytes(pointers[0..4].try_into().unwrap()), 100);
        assert_eq!(u32::from_le_bytes(pointers[4..8].try_into().unwrap()), 105);
        assert_eq!(block, b"prog\0a\0");
    }

    #[test]
    fn no_vectors_is_no_bytes() {
        assert_eq!(vector_sizes(&[]), (0, 0));
    }
}

#[cfg(test)]
mod poll_tests {
    use super::poll::*;

    /// The offsets preview 1 fixes for a clock subscription: `userdata` at 0,
    /// the tag at 8, the clock id at 16, the timeout at 24 and the flags at
    /// 40. Read from anywhere else and a sleep asks for the wrong duration.
    #[test]
    fn a_clock_subscription_is_read_where_preview_one_puts_it() {
        let mut bytes = [0u8; SUBSCRIPTION_SIZE];
        bytes[0..8].copy_from_slice(&0xfeed_u64.to_le_bytes());
        bytes[8] = CLOCK;
        bytes[16..20].copy_from_slice(&1u32.to_le_bytes());
        bytes[24..32].copy_from_slice(&1_500_000u64.to_le_bytes());
        bytes[40..42].copy_from_slice(&ABSTIME.to_le_bytes());

        assert_eq!(
            subscription(&bytes),
            Some(Subscription {
                userdata: 0xfeed,
                eventtype: CLOCK,
                clock_id: 1,
                timeout: 1_500_000,
                flags: ABSTIME,
            })
        );
    }

    /// An event carries the subscription's userdata back, and an error of
    /// zero. Everything else has to be written, not left: the caller reads
    /// all of it.
    #[test]
    fn an_event_says_which_subscription_and_that_it_worked() {
        let event = event(0xfeed, 0, CLOCK);
        assert_eq!(&event[0..8], &0xfeed_u64.to_le_bytes());
        assert_eq!(u16::from_le_bytes([event[8], event[9]]), 0);
        assert_eq!(event[10], CLOCK);
        assert!(event[11..].iter().all(|b| *b == 0));
    }

    #[test]
    fn a_short_subscription_is_refused() {
        assert_eq!(subscription(&[0u8; 12]), None);
    }
}
