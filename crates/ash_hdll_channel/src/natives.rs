//! The primitives the Haxe side binds, and the state behind them.

use std::collections::VecDeque;
use std::ffi::c_void;
use std::os::raw::{c_char, c_int};
use std::sync::Mutex;

use ash_future_abi::{AshFuture, hlp_future_create, hlp_future_reject, hlp_future_resolve};
use hl_abi::{hlp_alloc_bytes, hlp_alloc_dynamic, hlp_type_bytes, hlp_type_i32, vdynamic};

use crate::ring::{self, Block};

/// This library allocates through the program. See [`hl_abi::ProgramAllocator`].
#[global_allocator]
static ALLOCATOR: hl_abi::ProgramAllocator = hl_abi::ProgramAllocator;

// Rust's threaded WASI std addresses errno as TLS. A dylink side module cannot
// import a TLS datum, and this library performs no errno-reporting libc calls,
// so it owns the otherwise-unused slot.
#[unsafe(no_mangle)]
#[cfg_attr(target_feature = "atomics", thread_local)]
static mut errno: i32 = 0;

unsafe extern "C" {
    fn hl_add_root(slot: *mut c_void);
    fn hl_remove_root(slot: *mut c_void);
    fn ash_host_agent(name: *const u8, name_len: u32, address: u32) -> i32;
    fn ash_host_watch(
        word: *const u32,
        handler: unsafe extern "C" fn(*mut c_void),
        context: *mut c_void,
    ) -> *const u32;
}

/// What `send` answers.
const SENT: c_int = 0;
const FULL: c_int = 1;
const TOO_LARGE: c_int = 2;
const CLOSED: c_int = 3;

/// The error a rejected `recv` carries: an Int the Haxe side turns into its
/// own exception.
const ERR_CLOSED: i32 = 1;

struct Channel {
    block: Block,
    /// Frames that arrived while no `recv` was waiting.
    queue: VecDeque<Vec<u8>>,
    /// Futures of `recv` calls waiting for a frame, oldest first. The runtime
    /// keeps a pending future alive until it settles.
    pending: VecDeque<usize>,
    /// The page told us the transport closed.
    closed: bool,
}

static CHANNELS: Mutex<Vec<Option<Channel>>> = Mutex::new(Vec::new());

fn lock() -> std::sync::MutexGuard<'static, Vec<Option<Channel>>> {
    CHANNELS.lock().unwrap_or_else(|e| e.into_inner())
}

fn slot(channels: &mut [Option<Channel>], handle: c_int) -> Option<&mut Channel> {
    channels
        .get_mut((handle as usize).checked_sub(1)?)
        .and_then(Option::as_mut)
}

unsafe fn boxed_i32(value: i32) -> *mut vdynamic {
    unsafe {
        let d = hlp_alloc_dynamic(hlp_type_i32());
        (*d).v.i = value;
        d
    }
}

/// A frame as the Haxe side reads it: bytes whose first four are the frame's
/// length (little-endian), boxed so a future can carry it.
unsafe fn boxed_frame(frame: &[u8]) -> *mut vdynamic {
    unsafe {
        let mut d = hlp_alloc_dynamic(hlp_type_bytes());
        (*d).v.bytes = core::ptr::null_mut();
        hl_add_root(core::ptr::addr_of_mut!(d).cast());
        let buf = hlp_alloc_bytes((4 + frame.len()) as c_int);
        core::ptr::copy_nonoverlapping((frame.len() as u32).to_le_bytes().as_ptr(), buf, 4);
        core::ptr::copy_nonoverlapping(frame.as_ptr(), buf.add(4), frame.len());
        (*d).v.bytes = buf;
        hl_remove_root(core::ptr::addr_of_mut!(d).cast());
        d
    }
}

/// Read what the page has published, hand frames to the oldest waiting
/// `recv`s and queue the rest. Safe to call at any time on any thread.
fn pump(handle: c_int) {
    let mut resolve: Vec<(usize, Vec<u8>)> = Vec::new();
    let mut reject: Vec<usize> = Vec::new();
    {
        let mut channels = lock();
        let Some(ch) = slot(&mut channels, handle) else {
            return;
        };
        let mut frames = Vec::new();
        let mut closed = false;
        ch.block.drain_events(|kind, payload| match kind {
            ring::MESSAGE => {
                if let Some(bytes) = ring::message_bytes(payload) {
                    frames.push(bytes.to_vec());
                }
            }
            ring::CLOSED => closed = true,
            _ => {}
        });
        for frame in frames {
            match ch.pending.pop_front() {
                Some(future) => resolve.push((future, frame)),
                None => ch.queue.push_back(frame),
            }
        }
        if closed {
            ch.closed = true;
            reject.extend(ch.pending.drain(..));
        }
    }
    // Outside the lock: settling a future wakes a fiber.
    for (future, frame) in resolve {
        unsafe { hlp_future_resolve(future as *mut AshFuture, boxed_frame(&frame)) };
    }
    for future in reject {
        unsafe { hlp_future_reject(future as *mut AshFuture, boxed_i32(ERR_CLOSED)) };
    }
}

/// Run by the runtime's scheduler, on the program's thread, when the watched
/// event head changed. `context` is the channel's handle.
unsafe extern "C" fn on_events(context: *mut c_void) {
    pump(context as usize as c_int);
}

fn plain(name: &[u8]) -> bool {
    !name.is_empty()
        && name.len() <= ring::NAME_MAX
        && name
            .iter()
            .all(|b| b.is_ascii_alphanumeric() || *b == b'_' || *b == b'-')
}

/// Open a channel served by the page agent `name`: a UTF-8 identifier of
/// letters, digits, `_` and `-`. `capacity` is the size of each ring in bytes,
/// a power of two from 4 KiB to 64 MiB; a message may take at most half a
/// ring. Returns a handle, or 0 when the page would not start the agent (no
/// hook, memory not shared) or an argument was refused.
///
/// # Safety
/// `name` points at `name_len` bytes.
pub unsafe extern "C" fn channel_open(name: *const u8, name_len: c_int, capacity: c_int) -> c_int {
    let capacity = capacity as u32;
    if name.is_null() || name_len <= 0 || !ring::valid_capacity(capacity) {
        return 0;
    }
    let name = unsafe { core::slice::from_raw_parts(name, name_len as usize) };
    if !plain(name) {
        return 0;
    }
    let words = ring::block_size(capacity, capacity).div_ceil(8);
    // Never freed: the page agent may still hold the address.
    let memory: &'static mut [u64] = Box::leak(vec![0u64; words].into_boxed_slice());
    let base = memory.as_mut_ptr() as *mut u8;
    let block = unsafe { Block::init(base, capacity, capacity, name) };
    let handle = {
        let mut channels = lock();
        channels.push(Some(Channel {
            block,
            queue: VecDeque::new(),
            pending: VecDeque::new(),
            closed: false,
        }));
        channels.len() as c_int
    };
    // The watch first: the agent may publish the moment it starts, and the
    // wake word has to be in the header when it looks.
    let wake = unsafe {
        ash_host_watch(
            block.word(ring::EVENT_HEAD_AT).as_ptr() as *const u32,
            on_events,
            handle as usize as *mut c_void,
        )
    };
    block
        .word(ring::WAKE_AT)
        .store(wake as usize as u32, core::sync::atomic::Ordering::Release);
    let started = unsafe { ash_host_agent(name.as_ptr(), name.len() as u32, base as usize as u32) };
    if started == 0 {
        lock()[handle as usize - 1] = None;
        return 0;
    }
    handle
}

/// Queue the `len` bytes at `data + pos` as one message for the page. 0 sent;
/// 1 the ring is full, try again; 2 the message can never fit (more than half
/// a ring); 3 the channel is closed or the handle unknown.
///
/// # Safety
/// `data + pos` points at `len` readable bytes.
pub unsafe extern "C" fn channel_send(
    handle: c_int,
    data: *const u8,
    pos: c_int,
    len: c_int,
) -> c_int {
    if data.is_null() || pos < 0 || len < 0 {
        return TOO_LARGE;
    }
    let bytes = unsafe { core::slice::from_raw_parts(data.add(pos as usize), len as usize) };
    let mut channels = lock();
    let Some(ch) = slot(&mut channels, handle) else {
        return CLOSED;
    };
    let flags = ch.block.flags();
    if ch.closed || flags & (ring::FLAG_CLOSED_BY_TRANSPORT | ring::FLAG_CLOSED_BY_PROGRAM) != 0 {
        return CLOSED;
    }
    if !ch.block.fits(4 + bytes.len()) {
        return TOO_LARGE;
    }
    if !ch
        .block
        .send_command(ring::MESSAGE, &ring::message_payload(bytes))
    {
        return FULL;
    }
    notify(ch.block.word(ring::COMMAND_HEAD_AT).as_ptr());
    SENT
}

/// Wake the page agent if it is waiting on the command head.
fn notify(word: *mut u32) {
    #[cfg(target_feature = "atomics")]
    unsafe {
        core::arch::wasm32::memory_atomic_notify(word as *mut i32, 1);
    }
    #[cfg(not(target_feature = "atomics"))]
    let _ = word;
}

/// A future of the next frame: resolves with a boxed `hl.Bytes` whose first
/// four bytes are the frame's length (little-endian), rejects with the Int 1
/// once the channel is closed and nothing is queued.
pub unsafe extern "C" fn channel_recv(handle: c_int) -> *mut AshFuture {
    pump(handle);
    let future = unsafe { hlp_future_create() };
    enum Next {
        Frame(Vec<u8>),
        Closed,
        Wait,
    }
    let next = {
        let mut channels = lock();
        match slot(&mut channels, handle) {
            None => Next::Closed,
            Some(ch) => match ch.queue.pop_front() {
                Some(frame) => Next::Frame(frame),
                None if ch.closed => Next::Closed,
                None => {
                    ch.pending.push_back(future as usize);
                    Next::Wait
                }
            },
        }
    };
    match next {
        Next::Frame(frame) => unsafe {
            hlp_future_resolve(future, boxed_frame(&frame));
        },
        Next::Closed => unsafe {
            hlp_future_reject(future, boxed_i32(ERR_CLOSED));
        },
        Next::Wait => {}
    }
    future
}

/// The header's flag bits (1 open, 2 closed by the transport, 4 closed by the
/// program), plus 8 once frames are queued for `recv`; 0 for an unknown handle.
pub unsafe extern "C" fn channel_state(handle: c_int) -> c_int {
    pump(handle);
    let mut channels = lock();
    match slot(&mut channels, handle) {
        None => 0,
        Some(ch) => (ch.block.flags() | if ch.queue.is_empty() { 0 } else { 8 }) as c_int,
    }
}

/// Close the channel: tell the page to close its transport and reject every
/// waiting `recv`. Frames already queued can still be taken.
pub unsafe extern "C" fn channel_close(handle: c_int) {
    let waiting: Vec<usize> = {
        let mut channels = lock();
        let Some(ch) = slot(&mut channels, handle) else {
            return;
        };
        if ch.block.flags() & ring::FLAG_CLOSED_BY_PROGRAM == 0 {
            ch.block.word(ring::FLAGS_AT).fetch_or(
                ring::FLAG_CLOSED_BY_PROGRAM,
                core::sync::atomic::Ordering::AcqRel,
            );
            ch.block.send_command(ring::CLOSED, &[]);
            notify(ch.block.word(ring::COMMAND_HEAD_AT).as_ptr());
        }
        ch.closed = true;
        ch.pending.drain(..).collect()
    };
    for future in waiting {
        unsafe { hlp_future_reject(future as *mut AshFuture, boxed_i32(ERR_CLOSED)) };
    }
}

/// Export a primitive as the resolver `DEFINE_PRIM` writes:
///
/// ```c
/// EXPORT void *hlp_<name>(const char **sign) {
///     *sign = <signature>; return (void*)&<name>;
/// }
/// ```
///
/// A resolver reports the signature through an out-parameter and returns the
/// real function; the program finds `hlp_<name>` among this module's exports
/// and calls it to get the function. The letters are HashLink's: arguments
/// first, then the return type; `_` void, `i` i32, `b` bytes, `?` an abstract
/// pointer.
macro_rules! define_prim {
    ($resolver:ident, $function:ident, $signature:literal) => {
        /// # Safety
        /// `sign` is writable, as it is for any `DEFINE_PRIM` resolver.
        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn $resolver(sign: *mut *const c_char) -> *mut c_void {
            unsafe {
                if !sign.is_null() {
                    *sign = concat!($signature, "\0").as_ptr() as *const c_char;
                }
                $function as *mut c_void
            }
        }
    };
}

define_prim!(hlp_channel_open, channel_open, "biii");
define_prim!(hlp_channel_send, channel_send, "ibiii");
define_prim!(hlp_channel_recv, channel_recv, "i?");
define_prim!(hlp_channel_state, channel_state, "ii");
define_prim!(hlp_channel_close, channel_close, "i_");
