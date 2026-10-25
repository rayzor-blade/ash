//! The block a program shares with a page agent, and the two rings in it.
//!
//! One block of shared memory, 8-byte aligned, laid out as a header and then
//! two rings. Each ring has one writer and one reader: the page writes the
//! event ring and the program reads it; the program writes the command ring
//! and the page reads it. `head` and `tail` are byte counters that only grow
//! (modulo 2^32); a position in a ring is the counter modulo its capacity, and
//! the space in use is `head - tail`. A writer stores its record, then
//! publishes it by storing the new head with release ordering; a reader loads
//! the head with acquire ordering, reads up to it, then stores the new tail.
//!
//! A record is a `u32` header, `kind | (length << 8)`, where `length` is the
//! whole record in bytes (a multiple of 4, at most 2^24 - 4), followed by its
//! payload. A record never wraps: when it does not fit before the end of the
//! ring the writer fills the rest with a pad record (kind 0) and starts again
//! at the beginning.

use core::sync::atomic::{AtomicU32, Ordering};

pub const MAGIC: u32 = 0x4348_5341; // "ASHC"
pub const VERSION: u32 = 1;

/// Byte offsets of the header words, all little-endian `u32`.
pub const MAGIC_AT: usize = 0;
pub const VERSION_AT: usize = 4;
pub const EVENT_CAP_AT: usize = 8;
pub const COMMAND_CAP_AT: usize = 12;
pub const EVENT_HEAD_AT: usize = 16;
pub const EVENT_TAIL_AT: usize = 20;
pub const COMMAND_HEAD_AT: usize = 24;
pub const COMMAND_TAIL_AT: usize = 28;
pub const DROPPED_AT: usize = 32;
pub const ATTACHED_AT: usize = 36;
/// The address of the word the page adds one to and notifies after it bumps
/// the event head, so a scheduler asleep on it wakes at once.
pub const WAKE_AT: usize = 40;
pub const FLAGS_AT: usize = 44;
pub const NAME_LEN_AT: usize = 48;
pub const NAME_AT: usize = 56;
pub const NAME_MAX: usize = 64;
pub const HEADER: usize = 128;

/// `FLAGS_AT` bits.
pub const FLAG_OPEN: u32 = 1;
pub const FLAG_CLOSED_BY_TRANSPORT: u32 = 2;
pub const FLAG_CLOSED_BY_PROGRAM: u32 = 4;

/// Record kinds. 0 is padding; 1 is a message in either direction.
pub const PAD: u8 = 0;
pub const MESSAGE: u8 = 1;
/// Page to program: the transport is open.
pub const OPENED: u8 = 2;
/// Page to program: the transport closed. Program to page: close it.
pub const CLOSED: u8 = 3;

pub const MIN_CAPACITY: u32 = 4096;
pub const MAX_CAPACITY: u32 = 1 << 26;

/// A view of a block. The pointer must stay valid for the view's lifetime and
/// the block must follow the layout above.
#[derive(Clone, Copy)]
pub struct Block {
    base: *mut u8,
    event_cap: u32,
    command_cap: u32,
}

unsafe impl Send for Block {}
unsafe impl Sync for Block {}

fn word<'a>(base: *mut u8, at: usize) -> &'a AtomicU32 {
    unsafe { AtomicU32::from_ptr(base.add(at) as *mut u32) }
}

/// The size in bytes a block with these capacities needs.
pub fn block_size(event_cap: u32, command_cap: u32) -> usize {
    HEADER + event_cap as usize + command_cap as usize
}

/// Whether `capacity` is a size a ring may have.
pub fn valid_capacity(capacity: u32) -> bool {
    capacity.is_power_of_two() && (MIN_CAPACITY..=MAX_CAPACITY).contains(&capacity)
}

impl Block {
    /// Write the header of a zeroed block.
    ///
    /// # Safety
    /// `base` points at `block_size(event_cap, command_cap)` zeroed bytes,
    /// 8-byte aligned, that outlive every use of the returned view.
    pub unsafe fn init(base: *mut u8, event_cap: u32, command_cap: u32, name: &[u8]) -> Block {
        let name = &name[..name.len().min(NAME_MAX)];
        word(base, MAGIC_AT).store(MAGIC, Ordering::Relaxed);
        word(base, VERSION_AT).store(VERSION, Ordering::Relaxed);
        word(base, EVENT_CAP_AT).store(event_cap, Ordering::Relaxed);
        word(base, COMMAND_CAP_AT).store(command_cap, Ordering::Relaxed);
        word(base, NAME_LEN_AT).store(name.len() as u32, Ordering::Relaxed);
        unsafe { core::ptr::copy_nonoverlapping(name.as_ptr(), base.add(NAME_AT), name.len()) };
        Block {
            base,
            event_cap,
            command_cap,
        }
    }

    pub fn address(&self) -> *mut u8 {
        self.base
    }

    pub fn word(&self, at: usize) -> &AtomicU32 {
        word(self.base, at)
    }

    pub fn flags(&self) -> u32 {
        self.word(FLAGS_AT).load(Ordering::Acquire)
    }

    fn event_ring(&self) -> *mut u8 {
        unsafe { self.base.add(HEADER) }
    }

    fn command_ring(&self) -> *mut u8 {
        unsafe { self.base.add(HEADER + self.event_cap as usize) }
    }

    /// Append a record to the command ring. `false` when it does not fit now
    /// or can never fit (larger than half the ring).
    pub fn send_command(&self, kind: u8, payload: &[u8]) -> bool {
        write(
            self.command_ring(),
            self.command_cap,
            self.word(COMMAND_HEAD_AT),
            self.word(COMMAND_TAIL_AT),
            kind,
            payload,
        )
    }

    /// Whether a record with this payload can ever be sent.
    pub fn fits(&self, payload_len: usize) -> bool {
        record_len(payload_len) <= (self.command_cap / 2) as usize
            && record_len(payload_len) < (1 << 24)
    }

    /// Append a record to the event ring. This is the page's side; the
    /// program uses it in tests.
    pub fn send_event(&self, kind: u8, payload: &[u8]) -> bool {
        write(
            self.event_ring(),
            self.event_cap,
            self.word(EVENT_HEAD_AT),
            self.word(EVENT_TAIL_AT),
            kind,
            payload,
        )
    }

    /// Hand every record in the event ring to `f`, oldest first, and release
    /// its space. Pad records are skipped.
    pub fn drain_events(&self, mut f: impl FnMut(u8, &[u8])) {
        read(
            self.event_ring(),
            self.event_cap,
            self.word(EVENT_HEAD_AT),
            self.word(EVENT_TAIL_AT),
            &mut f,
        );
    }

    /// The command ring's side of [`Block::drain_events`], for the page's mirror in tests.
    pub fn drain_commands(&self, mut f: impl FnMut(u8, &[u8])) {
        read(
            self.command_ring(),
            self.command_cap,
            self.word(COMMAND_HEAD_AT),
            self.word(COMMAND_TAIL_AT),
            &mut f,
        );
    }
}

/// The bytes a record with `payload_len` payload bytes takes.
pub fn record_len(payload_len: usize) -> usize {
    payload_len.saturating_add(7) & !3
}

fn write(
    ring: *mut u8,
    cap: u32,
    head: &AtomicU32,
    tail: &AtomicU32,
    kind: u8,
    payload: &[u8],
) -> bool {
    let total = record_len(payload.len());
    if total > (cap / 2) as usize || total >= (1 << 24) {
        return false;
    }
    let total = total as u32;
    let h = head.load(Ordering::Relaxed);
    let t = tail.load(Ordering::Acquire);
    let used = h.wrapping_sub(t);
    let pos = h % cap;
    let contiguous = cap - pos;
    let (pad, advance) = if total > contiguous {
        (contiguous, contiguous + total)
    } else {
        (0, total)
    };
    if used as u64 + advance as u64 > cap as u64 {
        return false;
    }
    unsafe {
        if pad != 0 {
            (ring.add(pos as usize) as *mut u32).write_unaligned((PAD as u32) | (pad << 8));
        }
        let at = if pad != 0 { 0 } else { pos as usize };
        let rec = ring.add(at);
        (rec as *mut u32).write_unaligned((kind as u32) | (total << 8));
        core::ptr::copy_nonoverlapping(payload.as_ptr(), rec.add(4), payload.len());
        let padding = total as usize - 4 - payload.len();
        core::ptr::write_bytes(rec.add(4 + payload.len()), 0, padding);
    }
    head.store(h.wrapping_add(advance), Ordering::Release);
    true
}

fn read(
    ring: *mut u8,
    cap: u32,
    head: &AtomicU32,
    tail: &AtomicU32,
    f: &mut impl FnMut(u8, &[u8]),
) {
    let h = head.load(Ordering::Acquire);
    let mut t = tail.load(Ordering::Relaxed);
    while t != h {
        let pos = t % cap;
        let header = unsafe { (ring.add(pos as usize) as *const u32).read_unaligned() };
        let kind = (header & 0xff) as u8;
        let len = header >> 8;
        if h.wrapping_sub(t) > cap
            || len < 4
            || len % 4 != 0
            || len > cap - pos
            || len > h.wrapping_sub(t)
        {
            // A corrupt ring: drop everything rather than read past it.
            t = h;
            break;
        }
        if kind != PAD {
            let payload = unsafe {
                core::slice::from_raw_parts(ring.add(pos as usize + 4), len as usize - 4)
            };
            f(kind, payload);
        }
        t = t.wrapping_add(len);
    }
    tail.store(t, Ordering::Release);
}

/// A message payload: its exact byte length, then the bytes. A record is
/// rounded up to a multiple of 4, so the length cannot be read off the record.
pub fn message_payload(bytes: &[u8]) -> Vec<u8> {
    let mut payload = Vec::with_capacity(4 + bytes.len());
    payload.extend_from_slice(&(bytes.len() as u32).to_le_bytes());
    payload.extend_from_slice(bytes);
    payload
}

/// The bytes of a message payload, if it is well formed.
pub fn message_bytes(payload: &[u8]) -> Option<&[u8]> {
    let n = u32::from_le_bytes(payload.get(..4)?.try_into().ok()?) as usize;
    payload.get(4..4 + n)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn block(cap: u32) -> (Vec<u64>, Block) {
        let size = block_size(cap, cap);
        let mut mem = vec![0u64; size.div_ceil(8)];
        let b = unsafe { Block::init(mem.as_mut_ptr() as *mut u8, cap, cap, b"test") };
        (mem, b)
    }

    #[test]
    fn a_message_comes_out_as_it_went_in() {
        let (_mem, b) = block(4096);
        assert!(b.send_event(MESSAGE, &message_payload(b"hello")));
        let mut got = Vec::new();
        b.drain_events(|k, p| got.push((k, message_bytes(p).unwrap().to_vec())));
        assert_eq!(got, vec![(MESSAGE, b"hello".to_vec())]);
    }

    #[test]
    fn records_survive_the_ring_wrapping_many_times() {
        let (_mem, b) = block(4096);
        for round in 0u32..2000 {
            let len = 1 + (round as usize * 37) % 900;
            let body: Vec<u8> = (0..len).map(|i| (i as u32 + round) as u8).collect();
            assert!(
                b.send_event(MESSAGE, &message_payload(&body)),
                "round {round}"
            );
            let mut seen = None;
            b.drain_events(|_, p| seen = Some(message_bytes(p).unwrap().to_vec()));
            assert_eq!(seen.as_deref(), Some(&body[..]), "round {round}");
        }
    }

    #[test]
    fn a_full_ring_refuses_and_frees_after_a_drain() {
        let (_mem, b) = block(4096);
        let body = vec![7u8; 1000];
        let mut sent = 0;
        while b.send_event(MESSAGE, &message_payload(&body)) {
            sent += 1;
            assert!(sent < 100);
        }
        assert!(sent >= 3);
        let mut n = 0;
        b.drain_events(|_, _| n += 1);
        assert_eq!(n, sent);
        assert!(b.send_event(MESSAGE, &message_payload(&body)));
    }

    #[test]
    fn a_record_over_half_the_ring_can_never_be_sent() {
        let (_mem, b) = block(4096);
        assert!(!b.fits(4096));
        assert!(!b.send_command(MESSAGE, &vec![0u8; 3000]));
        assert!(b.fits(2000));
    }

    #[test]
    fn size_check_matches_the_record_header_limit() {
        let (_mem, b) = block(1 << 26);
        assert!(b.fits((1 << 24) - 8));
        assert!(!b.fits((1 << 24) - 4));
        assert!(!b.fits(usize::MAX));
    }

    #[test]
    fn an_unpublished_record_is_not_read() {
        let (_mem, b) = block(4096);
        assert!(b.send_event(MESSAGE, &message_payload(b"hello")));
        b.word(EVENT_HEAD_AT).store(4, Ordering::Release);
        b.drain_events(|_, _| panic!("read beyond the published head"));
        assert_eq!(b.word(EVENT_TAIL_AT).load(Ordering::Acquire), 4);
    }

    #[test]
    fn the_command_ring_is_separate_from_the_event_ring() {
        let (_mem, b) = block(4096);
        assert!(b.send_command(MESSAGE, &message_payload(b"up")));
        let mut events = 0;
        b.drain_events(|_, _| events += 1);
        assert_eq!(events, 0);
        let mut got = Vec::new();
        b.drain_commands(|k, p| got.push((k, message_bytes(p).unwrap().to_vec())));
        assert_eq!(got, vec![(MESSAGE, b"up".to_vec())]);
    }

    #[test]
    fn the_counters_wrap_at_two_to_the_thirty_second() {
        let (_mem, b) = block(4096);
        // Start a few records short of the wrap.
        let start = u32::MAX - 4096 * 3 + 1;
        b.word(EVENT_HEAD_AT).store(start & !3, Ordering::Relaxed);
        b.word(EVENT_TAIL_AT).store(start & !3, Ordering::Relaxed);
        for round in 0..200u32 {
            let body = vec![round as u8; 300];
            assert!(
                b.send_event(MESSAGE, &message_payload(&body)),
                "round {round}"
            );
            let mut seen = None;
            b.drain_events(|_, p| seen = Some(message_bytes(p).unwrap().to_vec()));
            assert_eq!(seen.unwrap(), body, "round {round}");
        }
    }
}
