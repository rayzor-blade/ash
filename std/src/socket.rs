//! BSD sockets and host lookup, the `std@hlp_socket_*` and `std@hlp_host_*` natives.
//!
//! Primitive-for-primitive port of `hashlink/src/std/socket.c`, including its
//! failure sentinels: the `_I32` primitives return -1 for "would block" and -2
//! for "hard error or peer closed", the `_BOOL` ones return false, and the
//! `_BYTES` host lookups return NULL. `sys.net.Socket` and `sys.net.Host`
//! branch on those exact values, so they are part of the ABI.
//!
//! The `hl_socket` handle is `_ABSTRACT(hl_socket)` on the bytecode side: an
//! opaque GC pointer whose layout only this module sees.

use std::ffi::c_int;
use std::ptr;

use crate::bytes::hlp_alloc_bytes;
use crate::hl::{varray, vbyte};
use crate::hl_compat::hl_blocking;
use crate::types::hl_aptr;

// ============================================================================
// Platform layer
//
// Everything below the `sys` boundary is written once against these wrappers.
// The two implementations expose the same Rust-level signatures; the socket
// handle, the error channel and the fd_set layout are what actually differ.
// ============================================================================

#[cfg(unix)]
mod sys {
    use std::ffi::{c_int, c_void};
    use std::mem;
    use std::ptr;

    pub type Sock = c_int;
    pub const INVALID: Sock = -1;

    /// Upstream compiles the Darwin build with `MSG_NOSIGNAL` defined to 0 and
    /// suppresses SIGPIPE through `SO_NOSIGPIPE` instead. Darwin only grew the
    /// send flag in recent SDKs, so keep upstream's split rather than trusting
    /// the constant libc happens to expose.
    #[cfg(target_vendor = "apple")]
    const NOSIGNAL: c_int = 0;
    #[cfg(not(target_vendor = "apple"))]
    const NOSIGNAL: c_int = libc::MSG_NOSIGNAL;

    pub type SockAddrIn = libc::sockaddr_in;

    /// `struct hostent`, identical on glibc and Darwin. Declared here because
    /// the `libc` crate does not export `gethostbyaddr` for every unix target.
    #[repr(C)]
    pub struct Hostent {
        pub h_name: *mut libc::c_char,
        pub h_aliases: *mut *mut libc::c_char,
        pub h_addrtype: c_int,
        pub h_length: c_int,
        pub h_addr_list: *mut *mut libc::c_char,
    }

    extern "C" {
        fn gethostbyaddr(addr: *const c_void, len: libc::socklen_t, ty: c_int) -> *mut Hostent;
    }

    /// Winsock needs a one-time startup; POSIX does not.
    pub fn startup() {}

    pub fn is_valid(s: Sock) -> bool {
        s >= 0
    }

    /// Ordering key for select's `nfds`, which POSIX wants as `max fd + 1`.
    pub fn sock_key(s: Sock) -> u64 {
        s as u64
    }

    /// -1 when the operation merely has to be retried, -2 for a real failure.
    pub fn block_error() -> c_int {
        match std::io::Error::last_os_error().raw_os_error() {
            Some(e)
                if e == libc::EAGAIN
                    || e == libc::EWOULDBLOCK
                    || e == libc::EINPROGRESS
                    || e == libc::EALREADY =>
            {
                -1
            }
            _ => -2,
        }
    }

    /// Only Winsock reports a bounced datagram back to the reader.
    pub fn conn_reset() -> bool {
        false
    }

    pub fn sockaddr_in(host: c_int, port: c_int) -> SockAddrIn {
        let mut addr: SockAddrIn = unsafe { mem::zeroed() };
        addr.sin_family = libc::AF_INET as libc::sa_family_t;
        addr.sin_port = (port as u16).to_be();
        // `host` already carries the address in network byte order: it is the
        // int `host_resolve` handed to Haxe, which is a raw `s_addr`.
        addr.sin_addr.s_addr = host as u32;
        addr
    }

    /// Splits a bound address back into the `(host, port)` pair Haxe expects,
    /// host staying in network order and port coming back to host order.
    pub fn addr_parts(addr: &SockAddrIn) -> (c_int, c_int) {
        (
            addr.sin_addr.s_addr as c_int,
            u16::from_be(addr.sin_port) as c_int,
        )
    }

    pub unsafe fn create(udp: bool) -> Sock {
        let ty = if udp {
            libc::SOCK_DGRAM
        } else {
            libc::SOCK_STREAM
        };
        let s = libc::socket(libc::AF_INET, ty, 0);
        if s == INVALID {
            return INVALID;
        }
        // Upstream passes NULL/0 as the option value here, which Darwin
        // rejects with EFAULT and so leaves SIGPIPE armed; pass the real
        // value so writing to a closed peer returns EPIPE instead of
        // killing the process.
        #[cfg(target_vendor = "apple")]
        {
            let one: c_int = 1;
            libc::setsockopt(
                s,
                libc::SOL_SOCKET,
                libc::SO_NOSIGPIPE,
                &one as *const c_int as *const c_void,
                mem::size_of::<c_int>() as libc::socklen_t,
            );
        }
        // Sockets must not be inherited across exec.
        let old = libc::fcntl(s, libc::F_GETFD, 0);
        if old >= 0 {
            libc::fcntl(s, libc::F_SETFD, old | libc::FD_CLOEXEC);
        }
        s
    }

    pub unsafe fn close(s: Sock) {
        libc::close(s);
    }

    pub unsafe fn send(s: Sock, buf: *const u8, len: c_int) -> isize {
        libc::send(s, buf as *const c_void, len as usize, NOSIGNAL)
    }

    pub unsafe fn recv(s: Sock, buf: *mut u8, len: c_int) -> isize {
        libc::recv(s, buf as *mut c_void, len as usize, NOSIGNAL)
    }

    pub unsafe fn send_to(s: Sock, buf: *const u8, len: c_int, addr: &SockAddrIn) -> isize {
        libc::sendto(
            s,
            buf as *const c_void,
            len as usize,
            NOSIGNAL,
            addr as *const SockAddrIn as *const libc::sockaddr,
            mem::size_of::<SockAddrIn>() as libc::socklen_t,
        )
    }

    pub unsafe fn recv_from(s: Sock, buf: *mut u8, len: c_int, addr: &mut SockAddrIn) -> isize {
        let mut alen = mem::size_of::<SockAddrIn>() as libc::socklen_t;
        libc::recvfrom(
            s,
            buf as *mut c_void,
            len as usize,
            NOSIGNAL,
            addr as *mut SockAddrIn as *mut libc::sockaddr,
            &mut alen,
        )
    }

    pub unsafe fn connect(s: Sock, addr: &SockAddrIn) -> bool {
        libc::connect(
            s,
            addr as *const SockAddrIn as *const libc::sockaddr,
            mem::size_of::<SockAddrIn>() as libc::socklen_t,
        ) == 0
    }

    pub unsafe fn bind(s: Sock, addr: &SockAddrIn) -> bool {
        // Upstream sets SO_REUSEADDR on POSIX only; on Windows the option
        // means "steal a live listener", which is not what is wanted.
        let opt: c_int = 1;
        libc::setsockopt(
            s,
            libc::SOL_SOCKET,
            libc::SO_REUSEADDR,
            &opt as *const c_int as *const c_void,
            mem::size_of::<c_int>() as libc::socklen_t,
        );
        libc::bind(
            s,
            addr as *const SockAddrIn as *const libc::sockaddr,
            mem::size_of::<SockAddrIn>() as libc::socklen_t,
        ) != -1
    }

    pub unsafe fn listen(s: Sock, n: c_int) -> bool {
        libc::listen(s, n) != -1
    }

    pub unsafe fn accept(s: Sock) -> Sock {
        let mut addr: SockAddrIn = mem::zeroed();
        let mut alen = mem::size_of::<SockAddrIn>() as libc::socklen_t;
        libc::accept(
            s,
            &mut addr as *mut SockAddrIn as *mut libc::sockaddr,
            &mut alen,
        )
    }

    pub unsafe fn sock_name(s: Sock) -> Option<SockAddrIn> {
        let mut addr: SockAddrIn = mem::zeroed();
        let mut alen = mem::size_of::<SockAddrIn>() as libc::socklen_t;
        if libc::getsockname(
            s,
            &mut addr as *mut SockAddrIn as *mut libc::sockaddr,
            &mut alen,
        ) == -1
        {
            return None;
        }
        Some(addr)
    }

    pub unsafe fn peer_name(s: Sock) -> Option<SockAddrIn> {
        let mut addr: SockAddrIn = mem::zeroed();
        let mut alen = mem::size_of::<SockAddrIn>() as libc::socklen_t;
        if libc::getpeername(
            s,
            &mut addr as *mut SockAddrIn as *mut libc::sockaddr,
            &mut alen,
        ) == -1
        {
            return None;
        }
        Some(addr)
    }

    pub unsafe fn shutdown(s: Sock, read: bool, write: bool) -> bool {
        let how = if read {
            if write {
                libc::SHUT_RDWR
            } else {
                libc::SHUT_RD
            }
        } else {
            libc::SHUT_WR
        };
        libc::shutdown(s, how) == 0
    }

    pub unsafe fn set_blocking(s: Sock, b: bool) -> bool {
        let rights = libc::fcntl(s, libc::F_GETFL);
        if rights == -1 {
            return false;
        }
        let rights = if b {
            rights & !libc::O_NONBLOCK
        } else {
            rights | libc::O_NONBLOCK
        };
        libc::fcntl(s, libc::F_SETFL, rights) != -1
    }

    pub unsafe fn set_timeout(s: Sock, t: f64) -> bool {
        let time = timeval_of(t);
        let val = &time as *const libc::timeval as *const c_void;
        let len = mem::size_of::<libc::timeval>() as libc::socklen_t;
        libc::setsockopt(s, libc::SOL_SOCKET, libc::SO_SNDTIMEO, val, len) == 0
            && libc::setsockopt(s, libc::SOL_SOCKET, libc::SO_RCVTIMEO, val, len) == 0
    }

    pub unsafe fn set_flag(s: Sock, level: c_int, name: c_int, b: bool) -> bool {
        let flag: c_int = b as c_int;
        libc::setsockopt(
            s,
            level,
            name,
            &flag as *const c_int as *const c_void,
            mem::size_of::<c_int>() as libc::socklen_t,
        ) == 0
    }

    pub const TCP_LEVEL: c_int = libc::IPPROTO_TCP;
    pub const TCP_NODELAY: c_int = libc::TCP_NODELAY;
    pub const SOCKET_LEVEL: c_int = libc::SOL_SOCKET;
    pub const SO_BROADCAST: c_int = libc::SO_BROADCAST;

    /// Upstream's `init_timeval`: seconds truncated toward zero, the fraction
    /// carried in microseconds.
    fn timeval_of(t: f64) -> libc::timeval {
        libc::timeval {
            tv_sec: t as libc::time_t,
            tv_usec: ((t - (t as i64) as f64) * 1_000_000.0) as libc::suseconds_t,
        }
    }

    /// Bytes one fd_set costs inside the caller-supplied scratch buffer, or -1
    /// when `count` cannot be represented. POSIX fd_sets are fixed-size, so the
    /// count only decides whether the request is legal at all.
    pub fn fd_size(count: c_int) -> c_int {
        if count < 0 || count as usize > libc::FD_SETSIZE {
            return -1;
        }
        mem::size_of::<libc::fd_set>() as c_int
    }

    /// A borrowed fd_set living inside the VM's scratch bytes.
    pub struct FdSet {
        ptr: *mut libc::fd_set,
    }

    impl FdSet {
        pub unsafe fn init(region: *mut u8, _count: usize) -> FdSet {
            let ptr = region as *mut libc::fd_set;
            libc::FD_ZERO(ptr);
            FdSet { ptr }
        }

        /// False when the descriptor is outside the range an fd_set indexes;
        /// setting it anyway would scribble past the bitmap.
        pub unsafe fn add(&mut self, s: Sock) -> bool {
            if s < 0 || s as usize >= libc::FD_SETSIZE {
                return false;
            }
            libc::FD_SET(s, self.ptr);
            true
        }

        pub unsafe fn contains(&self, s: Sock) -> bool {
            if s < 0 || s as usize >= libc::FD_SETSIZE {
                return false;
            }
            libc::FD_ISSET(s, self.ptr)
        }
    }

    pub unsafe fn select(
        nfds: u64,
        read: Option<&FdSet>,
        write: Option<&FdSet>,
        except: Option<&FdSet>,
        timeout: Option<f64>,
    ) -> c_int {
        let mut time;
        let tp = match timeout {
            Some(t) => {
                time = timeval_of(t);
                &mut time as *mut libc::timeval
            }
            None => ptr::null_mut(),
        };
        libc::select(
            nfds as c_int,
            read.map_or(ptr::null_mut(), |s| s.ptr),
            write.map_or(ptr::null_mut(), |s| s.ptr),
            except.map_or(ptr::null_mut(), |s| s.ptr),
            tp,
        )
    }

    /// First IPv4 address for a NUL-terminated host name, in network order.
    ///
    /// Upstream calls `gethostbyname`/`gethostbyname_r`; `getaddrinfo` is the
    /// POSIX replacement, is thread-safe, and returns the same `s_addr`.
    pub unsafe fn resolve_ipv4(name: *const u8) -> Option<c_int> {
        let mut hints: libc::addrinfo = mem::zeroed();
        hints.ai_family = libc::AF_INET;
        hints.ai_socktype = libc::SOCK_STREAM;
        let mut res: *mut libc::addrinfo = ptr::null_mut();
        if libc::getaddrinfo(name as *const libc::c_char, ptr::null(), &hints, &mut res) != 0 {
            return None;
        }
        let mut out = None;
        let mut cur = res;
        while !cur.is_null() {
            let ai = &*cur;
            if ai.ai_family == libc::AF_INET
                && !ai.ai_addr.is_null()
                && ai.ai_addrlen as usize >= mem::size_of::<SockAddrIn>()
            {
                out = Some((*(ai.ai_addr as *const SockAddrIn)).sin_addr.s_addr as c_int);
                break;
            }
            cur = ai.ai_next;
        }
        libc::freeaddrinfo(res);
        out
    }

    /// Reverse lookup, mirroring upstream's `gethostbyaddr` branch. The
    /// returned name is copied out immediately because it lives in the
    /// resolver's static buffer.
    pub unsafe fn reverse_ipv4(ip: c_int) -> Option<Vec<u8>> {
        let h = gethostbyaddr(&ip as *const c_int as *const c_void, 4, libc::AF_INET);
        if h.is_null() || (*h).h_name.is_null() {
            return None;
        }
        Some(copy_cstr((*h).h_name as *const u8))
    }

    pub unsafe fn local_name() -> Option<Vec<u8>> {
        let mut buf = [0u8; 256];
        if libc::gethostname(buf.as_mut_ptr() as *mut libc::c_char, buf.len()) == -1 {
            return None;
        }
        // gethostname may leave the buffer unterminated on truncation.
        buf[255] = 0;
        Some(copy_cstr(buf.as_ptr()))
    }

    unsafe fn copy_cstr(p: *const u8) -> Vec<u8> {
        let mut len = 0usize;
        while *p.add(len) != 0 {
            len += 1;
        }
        std::slice::from_raw_parts(p, len).to_vec()
    }
}

// WASI preview 1 has half a socket API, and this is that half.
//
// A module cannot create a socket, connect one, bind, listen, resolve a name
// or select across a set: preview 1 has no call for any of it, and Rust's own
// `std::net` compiles on this target only to answer `Unsupported` at run time
// (measured, including under `wasmtime -S inherit-network`). Those operations
// refuse here, and refuse honestly.
//
// What preview 1 does have is `sock_accept`, `sock_recv`, `sock_send` and
// `sock_shutdown`, on a descriptor the HOST opened and passed in -- the shape
// a server takes when the runtime owns the listening socket. Those are
// implemented, so a Haxe program handed a listening descriptor accepts
// connections and talks over them.
//
// The natives above exist either way: a program that never opens a socket
// runs, and one that does gets a failure at the call rather than a missing
// symbol at load.
#[cfg(not(any(unix, windows)))]
mod sys {
    use std::ffi::c_int;

    /// The preview 1 calls this module uses. Declared rather than taken from
    /// a crate: it is five functions, and the alternative is a dependency
    /// that exists to spell them.
    #[link(wasm_import_module = "wasi_snapshot_preview1")]
    extern "C" {
        fn sock_accept(fd: u32, flags: u16, result: *mut u32) -> u16;
        fn sock_recv(
            fd: u32,
            ri_data: *const IoVec,
            ri_data_len: usize,
            ri_flags: u16,
            ro_datalen: *mut usize,
            ro_flags: *mut u16,
        ) -> u16;
        fn sock_send(
            fd: u32,
            si_data: *const IoVec,
            si_data_len: usize,
            si_flags: u16,
            so_datalen: *mut usize,
        ) -> u16;
        fn sock_shutdown(fd: u32, how: u8) -> u16;
        fn fd_close(fd: u32) -> u16;
    }

    /// `__wasi_ciovec_t` and `__wasi_iovec_t` have the same shape, so one
    /// type serves both directions.
    #[repr(C)]
    struct IoVec {
        buf: *const u8,
        len: usize,
    }

    /// The two operations preview 1 is missing, offered to whatever host
    /// wants to supply them.
    ///
    /// Everything else a socket does -- send, receive, shut down, close --
    /// preview 1 already names, so a host that implements those imports has
    /// implemented most of a socket already. It cannot open one or connect
    /// it, because preview 1 has no call for either, and that is the entire
    /// gap these two fill.
    ///
    /// A browser is the case this exists for. `new WebSocket(url)` opens a
    /// stream to a peer that speaks WebSocket, which is the only kind of
    /// connection a page may make; the host hands back a descriptor, and the
    /// WASI calls above then carry bytes over it. What a page cannot do at
    /// all is listen, so `bind` and `listen` still refuse.
    ///
    /// Declared weak-by-convention: a host that does not implement them is
    /// the ordinary case, and `open` returning a negative descriptor is how
    /// it says so.
    #[link(wasm_import_module = "env")]
    extern "C" {
        /// Open a socket, returning a descriptor or a negative errno.
        fn ash_host_socket_open(udp: i32) -> i32;
        /// Connect `fd` to an IPv4 address in host byte order. Returns 0, or
        /// a positive errno -- `EAGAIN` while a connection is still being
        /// established, which is how an asynchronous host reports progress.
        fn ash_host_socket_connect(fd: i32, ip: i32, port: i32) -> i32;
    }

    pub type Sock = c_int;
    pub const INVALID: Sock = -1;

    /// The shape of an IPv4 address, kept so the code above compiles and
    /// indexes it identically. Nothing here ever reads one back from a host.
    #[repr(C)]
    #[derive(Clone, Copy, Default)]
    pub struct SockAddrIn {
        pub sin_family: u16,
        pub sin_port: u16,
        pub sin_addr: u32,
        pub sin_zero: [u8; 8],
    }

    #[allow(dead_code)]
    pub struct Hostent {
        pub h_name: *mut i8,
        pub h_aliases: *mut *mut i8,
        pub h_addrtype: c_int,
        pub h_length: c_int,
        pub h_addr_list: *mut *mut i8,
    }

    pub fn startup() {}

    pub fn is_valid(s: Sock) -> bool {
        s >= 0
    }

    pub fn sock_key(s: Sock) -> u64 {
        s as u64
    }

    /// `EAGAIN` in WASI's errno numbering, which is what a would-block read
    /// or write reports.
    pub fn block_error() -> c_int {
        6
    }

    pub fn conn_reset() -> bool {
        false
    }

    pub fn sockaddr_in(host: c_int, port: c_int) -> SockAddrIn {
        SockAddrIn {
            sin_family: 2,
            sin_port: (port as u16).to_be(),
            sin_addr: host as u32,
            sin_zero: [0; 8],
        }
    }

    pub fn addr_parts(addr: &SockAddrIn) -> (c_int, c_int) {
        (addr.sin_addr as c_int, u16::from_be(addr.sin_port) as c_int)
    }

    /// Preview 1 has no call that creates a socket, so this asks the host.
    /// A host with nothing to offer returns a negative descriptor, and the
    /// caller sees the same refusal it would have seen from a kernel.
    pub unsafe fn create(udp: bool) -> Sock {
        let fd = ash_host_socket_open(i32::from(udp));
        if fd < 0 {
            INVALID
        } else {
            fd
        }
    }

    pub unsafe fn close(s: Sock) {
        if s >= 0 {
            fd_close(s as u32);
        }
    }

    pub unsafe fn send(s: Sock, buf: *const u8, len: c_int) -> isize {
        if s < 0 {
            return -1;
        }
        let iov = IoVec {
            buf,
            len: len as usize,
        };
        let mut sent: usize = 0;
        if sock_send(s as u32, &iov, 1, 0, &mut sent) != 0 {
            return -1;
        }
        sent as isize
    }

    pub unsafe fn recv(s: Sock, buf: *mut u8, len: c_int) -> isize {
        if s < 0 {
            return -1;
        }
        let iov = IoVec {
            buf: buf as *const u8,
            len: len as usize,
        };
        let mut got: usize = 0;
        let mut flags: u16 = 0;
        if sock_recv(s as u32, &iov, 1, 0, &mut got, &mut flags) != 0 {
            return -1;
        }
        got as isize
    }

    pub unsafe fn send_to(s: Sock, buf: *const u8, len: c_int, addr: &SockAddrIn) -> isize {
        let _ = (s, buf, len, addr);
        -1
    }

    pub unsafe fn recv_from(s: Sock, buf: *mut u8, len: c_int, addr: &mut SockAddrIn) -> isize {
        let _ = (s, buf, len, addr);
        -1
    }

    pub unsafe fn connect(s: Sock, addr: &SockAddrIn) -> bool {
        if s < 0 {
            return false;
        }
        let (ip, port) = addr_parts(addr);
        ash_host_socket_connect(s, ip, port) == 0
    }

    /// A page cannot listen, and preview 1 cannot bind. Both stay refused
    /// however capable the host is.
    pub unsafe fn bind(s: Sock, addr: &SockAddrIn) -> bool {
        let _ = (s, addr);
        false
    }

    pub unsafe fn listen(s: Sock, n: c_int) -> bool {
        let _ = (s, n);
        false
    }

    /// Accept on a descriptor the host opened. This is the one operation
    /// preview 1 offers that creates a socket, and it is why a server works
    /// here at all.
    pub unsafe fn accept(s: Sock) -> Sock {
        if s < 0 {
            return INVALID;
        }
        let mut accepted: u32 = 0;
        if sock_accept(s as u32, 0, &mut accepted) != 0 {
            return INVALID;
        }
        accepted as Sock
    }

    pub unsafe fn sock_name(s: Sock) -> Option<SockAddrIn> {
        let _ = s;
        None
    }

    pub unsafe fn peer_name(s: Sock) -> Option<SockAddrIn> {
        let _ = s;
        None
    }

    pub unsafe fn shutdown(s: Sock, read: bool, write: bool) -> bool {
        if s < 0 {
            return false;
        }
        // The `sdflags` bits: 1 = read, 2 = write.
        let how = u8::from(read) | (u8::from(write) << 1);
        sock_shutdown(s as u32, how) == 0
    }

    pub unsafe fn set_blocking(s: Sock, b: bool) -> bool {
        let _ = (s, b);
        false
    }

    pub unsafe fn set_timeout(s: Sock, t: f64) -> bool {
        let _ = (s, t);
        false
    }

    pub unsafe fn set_flag(s: Sock, level: c_int, name: c_int, b: bool) -> bool {
        let _ = (s, level, name, b);
        false
    }

    // The option names the code above passes to `set_flag`, which refuses
    // them all. Values match the BSD numbering so a trace reads the same.
    pub const TCP_LEVEL: c_int = 6;
    pub const TCP_NODELAY: c_int = 1;
    pub const SOCKET_LEVEL: c_int = 0xffff;
    pub const SO_BROADCAST: c_int = 0x0020;

    pub fn fd_size(count: c_int) -> c_int {
        let _ = count;
        0
    }

    pub struct FdSet;

    impl FdSet {
        /// # Safety
        /// Takes the caller's region without reading it; there is no set to
        /// build.
        pub unsafe fn init(region: *mut u8, _count: usize) -> FdSet {
            let _ = region;
            FdSet
        }

        /// # Safety
        /// Mirrors the platform signature; adds nothing.
        pub unsafe fn add(&mut self, s: Sock) -> bool {
            let _ = s;
            false
        }

        /// # Safety
        /// Mirrors the platform signature; contains nothing.
        pub unsafe fn contains(&self, s: Sock) -> bool {
            let _ = s;
            false
        }
    }

    /// # Safety
    /// Mirrors the platform signature. Selecting over no sockets is an error
    /// rather than an immediate timeout, so a caller does not spin.
    pub unsafe fn select(
        nfds: u64,
        read: Option<&FdSet>,
        write: Option<&FdSet>,
        except: Option<&FdSet>,
        timeout: Option<f64>,
    ) -> c_int {
        let _ = (nfds, read, write, except, timeout);
        -1
    }

    /// # Safety
    /// Mirrors the platform signature. There is no resolver.
    pub unsafe fn resolve_ipv4(name: *const u8) -> Option<c_int> {
        let _ = name;
        None
    }

    /// # Safety
    /// Mirrors the platform signature. There is no resolver.
    pub unsafe fn reverse_ipv4(ip: c_int) -> Option<Vec<u8>> {
        let _ = ip;
        None
    }

    /// # Safety
    /// Mirrors the platform signature. A sandbox has no host name.
    pub unsafe fn local_name() -> Option<Vec<u8>> {
        None
    }
}

#[cfg(windows)]
mod sys {
    use std::ffi::c_int;
    use std::mem;
    use std::ptr;
    use std::sync::Once;

    use windows_sys::Win32::Networking::WinSock as ws;

    pub type Sock = ws::SOCKET;
    pub const INVALID: Sock = ws::INVALID_SOCKET;

    /// Upstream defines `MSG_NOSIGNAL` to 0 on Windows; Winsock has no such
    /// flag and never raises SIGPIPE.
    const NOSIGNAL: i32 = 0;

    pub type SockAddrIn = ws::SOCKADDR_IN;

    /// Winsock refuses every call until `WSAStartup` has run. `socket_init`
    /// drives this, but `sys.net.Host` reaches the resolver without going
    /// through `sys.net.Socket`, so the host lookups call it too.
    pub fn startup() {
        static INIT: Once = Once::new();
        INIT.call_once(|| unsafe {
            let mut data: ws::WSADATA = mem::zeroed();
            // MAKEWORD(2,0), the version upstream requests.
            ws::WSAStartup(0x0002, &mut data);
        });
    }

    pub fn is_valid(s: Sock) -> bool {
        s != INVALID
    }

    /// Winsock ignores select's `nfds`; the key only has to order handles.
    pub fn sock_key(s: Sock) -> u64 {
        s as u64
    }

    pub fn block_error() -> c_int {
        let err = unsafe { ws::WSAGetLastError() };
        if err == ws::WSAEWOULDBLOCK || err == ws::WSAEALREADY || err == ws::WSAETIMEDOUT {
            -1
        } else {
            -2
        }
    }

    /// A UDP read fails with WSAECONNRESET when an *earlier* datagram drew
    /// an ICMP port-unreachable; the read itself is not in error.
    pub fn conn_reset() -> bool {
        unsafe { ws::WSAGetLastError() == ws::WSAECONNRESET }
    }

    pub fn sockaddr_in(host: c_int, port: c_int) -> SockAddrIn {
        let mut addr: SockAddrIn = unsafe { mem::zeroed() };
        addr.sin_family = ws::AF_INET;
        addr.sin_port = (port as u16).to_be();
        // `host` already carries the address in network byte order.
        addr.sin_addr = ws::IN_ADDR {
            S_un: ws::IN_ADDR_0 {
                S_addr: host as u32,
            },
        };
        addr
    }

    pub fn addr_parts(addr: &SockAddrIn) -> (c_int, c_int) {
        // The union is plain storage; every arm aliases the same four bytes.
        let ip = unsafe { addr.sin_addr.S_un.S_addr };
        (ip as c_int, u16::from_be(addr.sin_port) as c_int)
    }

    pub unsafe fn create(udp: bool) -> Sock {
        let ty = if udp { ws::SOCK_DGRAM } else { ws::SOCK_STREAM };
        ws::socket(ws::AF_INET as i32, ty, 0)
    }

    pub unsafe fn close(s: Sock) {
        ws::closesocket(s);
    }

    pub unsafe fn send(s: Sock, buf: *const u8, len: c_int) -> isize {
        ws::send(s, buf, len, NOSIGNAL) as isize
    }

    pub unsafe fn recv(s: Sock, buf: *mut u8, len: c_int) -> isize {
        ws::recv(s, buf, len, NOSIGNAL) as isize
    }

    pub unsafe fn send_to(s: Sock, buf: *const u8, len: c_int, addr: &SockAddrIn) -> isize {
        ws::sendto(
            s,
            buf,
            len,
            NOSIGNAL,
            addr as *const SockAddrIn as *const ws::SOCKADDR,
            mem::size_of::<SockAddrIn>() as i32,
        ) as isize
    }

    pub unsafe fn recv_from(s: Sock, buf: *mut u8, len: c_int, addr: &mut SockAddrIn) -> isize {
        let mut alen = mem::size_of::<SockAddrIn>() as i32;
        ws::recvfrom(
            s,
            buf,
            len,
            NOSIGNAL,
            addr as *mut SockAddrIn as *mut ws::SOCKADDR,
            &mut alen,
        ) as isize
    }

    pub unsafe fn connect(s: Sock, addr: &SockAddrIn) -> bool {
        ws::connect(
            s,
            addr as *const SockAddrIn as *const ws::SOCKADDR,
            mem::size_of::<SockAddrIn>() as i32,
        ) == 0
    }

    pub unsafe fn bind(s: Sock, addr: &SockAddrIn) -> bool {
        // No SO_REUSEADDR here: upstream skips it on Windows, where the option
        // lets a second process hijack a live listener.
        ws::bind(
            s,
            addr as *const SockAddrIn as *const ws::SOCKADDR,
            mem::size_of::<SockAddrIn>() as i32,
        ) != ws::SOCKET_ERROR
    }

    pub unsafe fn listen(s: Sock, n: c_int) -> bool {
        ws::listen(s, n) != ws::SOCKET_ERROR
    }

    pub unsafe fn accept(s: Sock) -> Sock {
        let mut addr: SockAddrIn = mem::zeroed();
        let mut alen = mem::size_of::<SockAddrIn>() as i32;
        ws::accept(
            s,
            &mut addr as *mut SockAddrIn as *mut ws::SOCKADDR,
            &mut alen,
        )
    }

    pub unsafe fn sock_name(s: Sock) -> Option<SockAddrIn> {
        let mut addr: SockAddrIn = mem::zeroed();
        let mut alen = mem::size_of::<SockAddrIn>() as i32;
        if ws::getsockname(
            s,
            &mut addr as *mut SockAddrIn as *mut ws::SOCKADDR,
            &mut alen,
        ) == ws::SOCKET_ERROR
        {
            return None;
        }
        Some(addr)
    }

    pub unsafe fn peer_name(s: Sock) -> Option<SockAddrIn> {
        let mut addr: SockAddrIn = mem::zeroed();
        let mut alen = mem::size_of::<SockAddrIn>() as i32;
        if ws::getpeername(
            s,
            &mut addr as *mut SockAddrIn as *mut ws::SOCKADDR,
            &mut alen,
        ) == ws::SOCKET_ERROR
        {
            return None;
        }
        Some(addr)
    }

    pub unsafe fn shutdown(s: Sock, read: bool, write: bool) -> bool {
        let how = if read {
            if write {
                ws::SD_BOTH
            } else {
                ws::SD_RECEIVE
            }
        } else {
            ws::SD_SEND
        };
        ws::shutdown(s, how) == 0
    }

    pub unsafe fn set_blocking(s: Sock, b: bool) -> bool {
        let mut arg: u32 = if b { 0 } else { 1 };
        ws::ioctlsocket(s, ws::FIONBIO, &mut arg) == 0
    }

    pub unsafe fn set_timeout(s: Sock, t: f64) -> bool {
        // Winsock's SO_*TIMEO take milliseconds as a DWORD, not a timeval.
        let time: c_int = (t * 1000.0) as c_int;
        let val = &time as *const c_int as *const u8;
        let len = mem::size_of::<c_int>() as i32;
        ws::setsockopt(s, ws::SOL_SOCKET, ws::SO_SNDTIMEO, val, len) == 0
            && ws::setsockopt(s, ws::SOL_SOCKET, ws::SO_RCVTIMEO, val, len) == 0
    }

    pub unsafe fn set_flag(s: Sock, level: c_int, name: c_int, b: bool) -> bool {
        let flag: c_int = b as c_int;
        ws::setsockopt(
            s,
            level,
            name,
            &flag as *const c_int as *const u8,
            mem::size_of::<c_int>() as i32,
        ) == 0
    }

    pub const TCP_LEVEL: c_int = ws::IPPROTO_TCP;
    pub const TCP_NODELAY: c_int = ws::TCP_NODELAY;
    pub const SOCKET_LEVEL: c_int = ws::SOL_SOCKET;
    pub const SO_BROADCAST: c_int = ws::SO_BROADCAST;

    /// Upstream raises `FD_SETSIZE` to 65536 before including winsock2.h and
    /// hands `select` a hand-sized fd_set; the layout below is that same
    /// `{ u_int fd_count; SOCKET fd_array[n]; }`, whose array starts one
    /// pointer in because of alignment padding.
    const FD_SETSIZE_CAP: usize = 65536;
    const FD_ARRAY_OFFSET: usize = mem::size_of::<usize>();

    pub fn fd_size(count: c_int) -> c_int {
        if count < 0 || count as usize > FD_SETSIZE_CAP {
            return -1;
        }
        (FD_ARRAY_OFFSET + count as usize * mem::size_of::<Sock>()) as c_int
    }

    /// A borrowed variable-length fd_set living inside the VM's scratch bytes.
    pub struct FdSet {
        ptr: *mut u8,
        cap: usize,
    }

    impl FdSet {
        pub unsafe fn init(region: *mut u8, count: usize) -> FdSet {
            ptr::write_unaligned(region as *mut u32, 0);
            FdSet {
                ptr: region,
                cap: count,
            }
        }

        fn count(&self) -> usize {
            unsafe { ptr::read_unaligned(self.ptr as *const u32) as usize }
        }

        unsafe fn slot(&self, i: usize) -> *mut Sock {
            (self.ptr.add(FD_ARRAY_OFFSET) as *mut Sock).add(i)
        }

        /// False only when more descriptors arrive than the region was sized
        /// for, which the caller's `fd_size` accounting rules out.
        pub unsafe fn add(&mut self, s: Sock) -> bool {
            let n = self.count();
            if n >= self.cap {
                return false;
            }
            ptr::write_unaligned(self.slot(n), s);
            ptr::write_unaligned(self.ptr as *mut u32, (n + 1) as u32);
            true
        }

        pub unsafe fn contains(&self, s: Sock) -> bool {
            (0..self.count()).any(|i| ptr::read_unaligned(self.slot(i)) == s)
        }
    }

    pub unsafe fn select(
        _nfds: u64,
        read: Option<&FdSet>,
        write: Option<&FdSet>,
        except: Option<&FdSet>,
        timeout: Option<f64>,
    ) -> c_int {
        let mut time;
        let tp = match timeout {
            Some(t) => {
                time = ws::TIMEVAL {
                    tv_sec: t as i32,
                    tv_usec: ((t - (t as i64) as f64) * 1_000_000.0) as i32,
                };
                &mut time as *mut ws::TIMEVAL
            }
            None => ptr::null_mut(),
        };
        let raw = |s: Option<&FdSet>| s.map_or(ptr::null_mut(), |s| s.ptr as *mut ws::FD_SET);
        ws::select(0, raw(read), raw(write), raw(except), tp)
    }

    pub unsafe fn resolve_ipv4(name: *const u8) -> Option<c_int> {
        startup();
        let mut hints: ws::ADDRINFOA = mem::zeroed();
        hints.ai_family = ws::AF_INET as i32;
        hints.ai_socktype = ws::SOCK_STREAM;
        let mut res: *mut ws::ADDRINFOA = ptr::null_mut();
        if ws::getaddrinfo(name, ptr::null(), &hints, &mut res) != 0 {
            return None;
        }
        let mut out = None;
        let mut cur = res;
        while !cur.is_null() {
            let ai = &*cur;
            if ai.ai_family == ws::AF_INET as i32
                && !ai.ai_addr.is_null()
                && ai.ai_addrlen >= mem::size_of::<SockAddrIn>()
            {
                let sa = &*(ai.ai_addr as *const SockAddrIn);
                out = Some(sa.sin_addr.S_un.S_addr as c_int);
                break;
            }
            cur = ai.ai_next;
        }
        ws::freeaddrinfo(res);
        out
    }

    pub unsafe fn reverse_ipv4(ip: c_int) -> Option<Vec<u8>> {
        startup();
        let h = ws::gethostbyaddr(&ip as *const c_int as *const u8, 4, ws::AF_INET as i32);
        if h.is_null() || (*h).h_name.is_null() {
            return None;
        }
        Some(copy_cstr((*h).h_name))
    }

    pub unsafe fn local_name() -> Option<Vec<u8>> {
        startup();
        let mut buf = [0u8; 256];
        if ws::gethostname(buf.as_mut_ptr(), buf.len() as i32) == ws::SOCKET_ERROR {
            return None;
        }
        buf[255] = 0;
        Some(copy_cstr(buf.as_ptr()))
    }

    unsafe fn copy_cstr(p: *const u8) -> Vec<u8> {
        let mut len = 0usize;
        while *p.add(len) != 0 {
            len += 1;
        }
        std::slice::from_raw_parts(p, len).to_vec()
    }
}

// ============================================================================
// Handle
// ============================================================================

/// The `_ABSTRACT(hl_socket)` payload. Upstream allocates it with
/// `hl_gc_alloc_noptr`; ash's collector is conservative and has no
/// pointer-free variant, so a plain GC block carries it.
#[repr(C)]
pub struct hl_socket {
    sock: sys::Sock,
}

/// Copies `src` into GC memory with a trailing NUL. The host lookups return
/// `_BYTES` that Haxe reads with `String.fromUTF8`, so the terminator is part
/// of the contract, and the storage must be the VM's, not Rust's.
unsafe fn gc_cstring(src: &[u8]) -> *mut vbyte {
    let out = hlp_alloc_bytes((src.len() + 1) as c_int);
    ptr::copy_nonoverlapping(src.as_ptr(), out, src.len());
    *out.add(src.len()) = 0;
    out
}

/// Parses the strict dotted-quad form `inet_addr` accepts, returning the
/// address in network byte order. Anything else (including the legacy short
/// forms) falls through to the resolver, which handles them.
fn parse_ipv4(text: &[u8]) -> Option<c_int> {
    let mut octets = [0u8; 4];
    let mut parts = text.split(|&b| b == b'.');
    for slot in octets.iter_mut() {
        let part = parts.next()?;
        if part.is_empty() || part.len() > 3 || !part.iter().all(|b| b.is_ascii_digit()) {
            return None;
        }
        let mut value = 0u32;
        for &b in part {
            value = value * 10 + (b - b'0') as u32;
        }
        if value > 255 {
            return None;
        }
        *slot = value as u8;
    }
    if parts.next().is_some() {
        return None;
    }
    // The int Haxe carries is a raw `s_addr`, i.e. the four bytes in wire
    // order reinterpreted as a host int.
    Some(u32::from_ne_bytes(octets) as c_int)
}

unsafe fn cstr_slice<'a>(p: *const u8) -> &'a [u8] {
    let mut len = 0usize;
    while *p.add(len) != 0 {
        len += 1;
    }
    std::slice::from_raw_parts(p, len)
}

// ============================================================================
// Primitives
// ============================================================================

/// `DEFINE_PRIM(_VOID, socket_init, _NO_ARG)`
#[no_mangle]
pub extern "C" fn hlp_socket_init() {
    sys::startup();
}

/// `DEFINE_PRIM(_SOCK, socket_new, _BOOL)`
#[no_mangle]
pub unsafe extern "C" fn hlp_socket_new(udp: bool) -> *mut hl_socket {
    let s = sys::create(udp);
    if !sys::is_valid(s) {
        return ptr::null_mut();
    }
    let hs = hlp_alloc_bytes(std::mem::size_of::<hl_socket>() as c_int) as *mut hl_socket;
    (*hs).sock = s;
    hs
}

/// `DEFINE_PRIM(_VOID, socket_close, _SOCK)`
#[no_mangle]
pub unsafe extern "C" fn hlp_socket_close(s: *mut hl_socket) {
    if s.is_null() {
        return;
    }
    sys::close((*s).sock);
    // Poisoning the handle is what makes a double close, and any later send
    // on a closed socket, return an error instead of hitting a recycled fd.
    (*s).sock = sys::INVALID;
}

/// `DEFINE_PRIM(_I32, socket_send_char, _SOCK _I32)`
#[no_mangle]
pub unsafe extern "C" fn hlp_socket_send_char(s: *mut hl_socket, c: c_int) -> c_int {
    if s.is_null() {
        return -2;
    }
    let byte = c as u8;
    hl_blocking(true);
    let sent = sys::send((*s).sock, &byte, 1);
    hl_blocking(false);
    if sent < 0 {
        let e = sys::block_error();
        trace::io("send", e);
        return e;
    }
    trace::io("send", 1);
    1
}

/// `DEFINE_PRIM(_I32, socket_send, _SOCK _BYTES _I32 _I32)`
#[no_mangle]
pub unsafe extern "C" fn hlp_socket_send(
    s: *mut hl_socket,
    buf: *mut vbyte,
    pos: c_int,
    len: c_int,
) -> c_int {
    if s.is_null() {
        return -2;
    }
    // Declared blocking for the same reason `recv` is: a full send buffer
    // parks this thread inside the kernel, and a collector that does not know
    // that waits for it to reach a safe point it will never reach.
    hl_blocking(true);
    let r = sys::send((*s).sock, buf.wrapping_offset(pos as isize), len);
    hl_blocking(false);
    if r < 0 {
        let e = sys::block_error();
        trace::io("send", e);
        return e;
    }
    trace::io("send", r as c_int);
    trace::message(
        "send",
        buf.wrapping_offset(pos as isize) as *const u8,
        r as i32,
    );
    // Upstream returns the requested `len` rather than `r`, which reports a
    // short write on a non-blocking socket as a complete one and silently
    // drops the tail. Returning the count actually sent can only ever be
    // lower, so callers written against upstream still behave.
    r as c_int
}

/// `DEFINE_PRIM(_I32, socket_recv, _SOCK _BYTES _I32 _I32)`
#[no_mangle]
pub unsafe extern "C" fn hlp_socket_recv(
    s: *mut hl_socket,
    buf: *mut vbyte,
    pos: c_int,
    len: c_int,
) -> c_int {
    if s.is_null() {
        return -2;
    }
    hl_blocking(true);
    let ret = sys::recv((*s).sock, buf.wrapping_offset(pos as isize), len);
    hl_blocking(false);
    if ret < 0 {
        let e = sys::block_error();
        trace::io("recv", e);
        return e;
    }
    trace::io("recv", ret as c_int);
    trace::message(
        "recv",
        buf.wrapping_offset(pos as isize) as *const u8,
        ret as i32,
    );
    // 0 is end-of-stream here, which `sys.net.Socket` turns into Eof.
    ret as c_int
}

/// `DEFINE_PRIM(_I32, socket_recv_char, _SOCK)`
#[no_mangle]
pub unsafe extern "C" fn hlp_socket_recv_char(s: *mut hl_socket) -> c_int {
    if s.is_null() {
        return -2;
    }
    let mut byte: u8 = 0;
    hl_blocking(true);
    let ret = sys::recv((*s).sock, &mut byte, 1);
    hl_blocking(false);
    if ret < 0 {
        let e = sys::block_error();
        trace::io("recv", e);
        return e;
    }
    if ret == 0 {
        trace::io("recv", 0);
        return -2;
    }
    trace::io("recv", 1);
    byte as c_int
}

/// `DEFINE_PRIM(_I32, host_resolve, _BYTES)`
///
/// `host` is a NUL-terminated UTF-8 char*, not a uchar*: `sys.net.Host` calls
/// `utf16ToUtf8` before crossing. Returns -1 when the name does not resolve,
/// which Haxe reports as "Unresolved host".
#[no_mangle]
pub unsafe extern "C" fn hlp_host_resolve(host: *mut vbyte) -> c_int {
    if host.is_null() {
        return -1;
    }
    if let Some(ip) = parse_ipv4(cstr_slice(host)) {
        return ip;
    }
    hl_blocking(true);
    let ip = sys::resolve_ipv4(host);
    hl_blocking(false);
    ip.unwrap_or(-1)
}

/// `DEFINE_PRIM(_BYTES, host_to_string, _I32)`
#[no_mangle]
pub unsafe extern "C" fn hlp_host_to_string(ip: c_int) -> *mut vbyte {
    // The int is a raw `s_addr`, so its native byte order already is the
    // wire order; upstream reaches the same digits through inet_ntoa.
    let o = ip.to_ne_bytes();
    let text = format!("{}.{}.{}.{}", o[0], o[1], o[2], o[3]);
    gc_cstring(text.as_bytes())
}

/// `DEFINE_PRIM(_BYTES, host_reverse, _I32)`
#[no_mangle]
pub unsafe extern "C" fn hlp_host_reverse(ip: c_int) -> *mut vbyte {
    hl_blocking(true);
    let name = sys::reverse_ipv4(ip);
    hl_blocking(false);
    match name {
        Some(n) => gc_cstring(&n),
        None => ptr::null_mut(),
    }
}

/// `DEFINE_PRIM(_BYTES, host_local, _NO_ARG)`
#[no_mangle]
pub unsafe extern "C" fn hlp_host_local() -> *mut vbyte {
    match sys::local_name() {
        Some(n) => gc_cstring(&n),
        None => ptr::null_mut(),
    }
}

/// Socket activity tracing, behind `ASH_TRACE_SOCKET=1`.
///
/// A native library's connection failing tells you nothing on its own: the
/// question is always whether the connect succeeded, whether bytes moved, and
/// which direction stopped first. Totals are reported per socket so a stalled
/// stream is distinguishable from one that never opened.
mod trace {
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::sync::OnceLock;

    pub fn on() -> bool {
        static ON: OnceLock<bool> = OnceLock::new();
        *ON.get_or_init(|| std::env::var("ASH_TRACE_SOCKET").is_ok())
    }

    static SENT: AtomicU64 = AtomicU64::new(0);
    static RECVD: AtomicU64 = AtomicU64::new(0);
    static OPS: AtomicU64 = AtomicU64::new(0);

    pub fn connect(host: i32, port: i32, ok: bool) {
        if !on() {
            return;
        }
        let b = host.to_le_bytes();
        eprintln!(
            "[sock] connect {}.{}.{}.{}:{} -> {}",
            b[0],
            b[1],
            b[2],
            b[3],
            port,
            if ok { "ok" } else { "FAILED" }
        );
    }

    /// Report a frame's protocol `type` field, and nothing else.
    ///
    /// These frames carry the join password and the SDP, so the payload is
    /// never printed. Scanning for `"type":"..."` says which step of the
    /// handshake moved -- whether a `turn_credentials` reply ever comes back,
    /// for one -- without putting a credential in a log. Server frames are
    /// unmasked and readable; client frames are masked, so an unreadable
    /// buffer simply yields nothing.
    ///
    /// # Safety
    /// `buf` must be valid for `len` bytes.
    pub unsafe fn message(dir: &str, buf: *const u8, len: i32) {
        if !on() || buf.is_null() || len <= 0 {
            return;
        }
        let bytes = std::slice::from_raw_parts(buf, len as usize);
        let Some(at) = bytes.windows(6).position(|w| w == b"\"type\"") else {
            return;
        };
        // Past `"type"`, its colon and opening quote, to the value.
        let rest = &bytes[at + 6..];
        let Some(open) = rest.iter().position(|&c| c == b'"') else {
            return;
        };
        let value = &rest[open + 1..];
        let Some(end) = value.iter().position(|&c| c == b'"') else {
            return;
        };
        let name: String = value[..end]
            .iter()
            .take(48)
            .map(|&c| if c.is_ascii_graphic() { c as char } else { '.' })
            .collect();
        if !name.is_empty() {
            eprintln!("[sock] {dir} type=\"{name}\"");
        }
    }

    /// `n` is the native return: >0 bytes, -1 would-block, -2 error, 0 eof.
    pub fn io(dir: &str, n: i32) {
        if !on() {
            return;
        }
        if n > 0 {
            let total = if dir == "send" {
                SENT.fetch_add(n as u64, Ordering::Relaxed) + n as u64
            } else {
                RECVD.fetch_add(n as u64, Ordering::Relaxed) + n as u64
            };
            // The opening exchange in full -- that is where a handshake goes
            // wrong -- then sparsely, so a busy stream stays readable.
            let ops = OPS.fetch_add(1, Ordering::Relaxed) + 1;
            if ops <= 24 || ops.is_multiple_of(256) {
                eprintln!("[sock] {dir} {n} bytes (total {total}, op {ops})");
            }
        } else if n == 0 || n == -2 {
            eprintln!(
                "[sock] {dir} {} (sent {} recvd {})",
                if n == 0 { "EOF" } else { "ERROR" },
                SENT.load(Ordering::Relaxed),
                RECVD.load(Ordering::Relaxed)
            );
        }
    }
}

/// `DEFINE_PRIM(_BOOL, socket_connect, _SOCK _I32 _I32)`
#[no_mangle]
pub unsafe extern "C" fn hlp_socket_connect(s: *mut hl_socket, host: c_int, port: c_int) -> bool {
    if s.is_null() {
        return false;
    }
    let addr = sys::sockaddr_in(host, port);
    hl_blocking(true);
    let ok = sys::connect((*s).sock, &addr);
    let blocked = !ok && sys::block_error() == -1;
    hl_blocking(false);
    // A non-blocking connect reports "in progress"; upstream calls that a
    // success and lets select decide when the handshake finished.
    trace::connect(host, port, ok || blocked);
    ok || blocked
}

/// `DEFINE_PRIM(_BOOL, socket_listen, _SOCK _I32)`
#[no_mangle]
pub unsafe extern "C" fn hlp_socket_listen(s: *mut hl_socket, n: c_int) -> bool {
    if s.is_null() {
        return false;
    }
    sys::listen((*s).sock, n)
}

/// `DEFINE_PRIM(_BOOL, socket_bind, _SOCK _I32 _I32)`
#[no_mangle]
pub unsafe extern "C" fn hlp_socket_bind(s: *mut hl_socket, host: c_int, port: c_int) -> bool {
    if s.is_null() {
        return false;
    }
    let addr = sys::sockaddr_in(host, port);
    sys::bind((*s).sock, &addr)
}

/// `DEFINE_PRIM(_SOCK, socket_accept, _SOCK)`
#[no_mangle]
pub unsafe extern "C" fn hlp_socket_accept(s: *mut hl_socket) -> *mut hl_socket {
    if s.is_null() {
        return ptr::null_mut();
    }
    hl_blocking(true);
    let nsock = sys::accept((*s).sock);
    hl_blocking(false);
    if !sys::is_valid(nsock) {
        return ptr::null_mut();
    }
    let hs = hlp_alloc_bytes(std::mem::size_of::<hl_socket>() as c_int) as *mut hl_socket;
    (*hs).sock = nsock;
    hs
}

/// `DEFINE_PRIM(_BOOL, socket_peer, _SOCK _REF(_I32) _REF(_I32))`
#[no_mangle]
pub unsafe extern "C" fn hlp_socket_peer(
    s: *mut hl_socket,
    host: *mut c_int,
    port: *mut c_int,
) -> bool {
    if s.is_null() {
        return false;
    }
    match sys::peer_name((*s).sock) {
        Some(addr) => {
            let (h, p) = sys::addr_parts(&addr);
            if !host.is_null() {
                *host = h;
            }
            if !port.is_null() {
                *port = p;
            }
            true
        }
        None => false,
    }
}

/// `DEFINE_PRIM(_BOOL, socket_host, _SOCK _REF(_I32) _REF(_I32))`
#[no_mangle]
pub unsafe extern "C" fn hlp_socket_host(
    s: *mut hl_socket,
    host: *mut c_int,
    port: *mut c_int,
) -> bool {
    if s.is_null() {
        return false;
    }
    match sys::sock_name((*s).sock) {
        Some(addr) => {
            let (h, p) = sys::addr_parts(&addr);
            if !host.is_null() {
                *host = h;
            }
            if !port.is_null() {
                *port = p;
            }
            true
        }
        None => false,
    }
}

/// `DEFINE_PRIM(_BOOL, socket_set_timeout, _SOCK _F64)`
#[no_mangle]
pub unsafe extern "C" fn hlp_socket_set_timeout(s: *mut hl_socket, t: f64) -> bool {
    if s.is_null() {
        return false;
    }
    sys::set_timeout((*s).sock, t)
}

/// `DEFINE_PRIM(_BOOL, socket_shutdown, _SOCK _BOOL _BOOL)`
#[no_mangle]
pub unsafe extern "C" fn hlp_socket_shutdown(s: *mut hl_socket, r: bool, w: bool) -> bool {
    if s.is_null() {
        return false;
    }
    if !r && !w {
        return true;
    }
    sys::shutdown((*s).sock, r, w)
}

/// `DEFINE_PRIM(_BOOL, socket_set_blocking, _SOCK _BOOL)`
#[no_mangle]
pub unsafe extern "C" fn hlp_socket_set_blocking(s: *mut hl_socket, b: bool) -> bool {
    if s.is_null() {
        return false;
    }
    sys::set_blocking((*s).sock, b)
}

/// `DEFINE_PRIM(_BOOL, socket_set_fast_send, _SOCK _BOOL)`
#[no_mangle]
pub unsafe extern "C" fn hlp_socket_set_fast_send(s: *mut hl_socket, b: bool) -> bool {
    if s.is_null() {
        return false;
    }
    sys::set_flag((*s).sock, sys::TCP_LEVEL, sys::TCP_NODELAY, b)
}

/// `socket_set_broadcast(_SOCK _BOOL) : _BOOL`
///
/// Not in socket.c, but `sys.net.UdpSocket.setBroadcast` binds it and upstream
/// therefore fails to resolve it. Implemented so UDP broadcast works here.
#[no_mangle]
pub unsafe extern "C" fn hlp_socket_set_broadcast(s: *mut hl_socket, b: bool) -> bool {
    if s.is_null() {
        return false;
    }
    sys::set_flag((*s).sock, sys::SOCKET_LEVEL, sys::SO_BROADCAST, b)
}

/// `DEFINE_PRIM(_I32, socket_send_to, _SOCK _BYTES _I32 _I32 _I32)`
///
/// No `pos` argument here: `UdpSocket` offsets the bytes on the Haxe side.
#[no_mangle]
pub unsafe extern "C" fn hlp_socket_send_to(
    s: *mut hl_socket,
    data: *mut vbyte,
    len: c_int,
    host: c_int,
    port: c_int,
) -> c_int {
    if s.is_null() {
        return -2;
    }
    let addr = sys::sockaddr_in(host, port);
    let r = sys::send_to((*s).sock, data, len, &addr);
    if r < 0 {
        return sys::block_error();
    }
    r as c_int
}

/// `DEFINE_PRIM(_I32, socket_recv_from, _SOCK _BYTES _I32 _REF(_I32) _REF(_I32))`
#[no_mangle]
pub unsafe extern "C" fn hlp_socket_recv_from(
    s: *mut hl_socket,
    data: *mut vbyte,
    len: c_int,
    host: *mut c_int,
    port: *mut c_int,
) -> c_int {
    if s.is_null() {
        return -2;
    }
    let mut addr = sys::sockaddr_in(0, 0);
    hl_blocking(true);
    let r = sys::recv_from((*s).sock, data, len, &mut addr);
    hl_blocking(false);
    let received = if r < 0 {
        // Upstream folds Windows' WSAECONNRESET (an earlier datagram bounced)
        // into an empty read rather than an error.
        let err = sys::block_error();
        if err == -2 && sys::conn_reset() {
            0
        } else {
            return err;
        }
    } else {
        r as c_int
    };
    let (h, p) = sys::addr_parts(&addr);
    if !host.is_null() {
        *host = h;
    }
    if !port.is_null() {
        *port = p;
    }
    received
}

/// `DEFINE_PRIM(_I32, socket_fd_size, _I32)`
#[no_mangle]
pub extern "C" fn hlp_socket_fd_size(size: c_int) -> c_int {
    sys::fd_size(size)
}

/// Carves one fd_set for `a` out of the caller's scratch buffer and fills it,
/// advancing the cursor exactly as upstream's `make_socket_set` does so the
/// Haxe-side accounting (one `socket_fd_size` per array) still lines up.
///
/// `Ok(None)` means the array was NULL and select must see a NULL set.
/// `Err(())` means the buffer was too small, or a descriptor sits outside the
/// range an fd_set can address — upstream scribbles past the bitmap in that
/// second case, this fails the call instead.
unsafe fn make_socket_set(
    a: *mut varray,
    cursor: &mut *mut u8,
    left: &mut c_int,
    max: &mut u64,
) -> Result<Option<sys::FdSet>, ()> {
    if a.is_null() {
        return Ok(None);
    }
    let count = (*a).size.max(0);
    let req = sys::fd_size(count);
    if req < 0 || *left < req {
        return Err(());
    }
    *left -= req;
    let region = *cursor;
    *cursor = (*cursor).add(req as usize);

    let mut set = sys::FdSet::init(region, count as usize);
    let aptr = hl_aptr::<*mut hl_socket>(a);
    for i in 0..count {
        let s = *aptr.add(i as usize);
        if s.is_null() {
            break;
        }
        let fd = (*s).sock;
        // A socket closed while still sitting in a select set is simply never
        // ready; upstream would pass the poisoned handle straight to FD_SET.
        if !sys::is_valid(fd) {
            continue;
        }
        if !set.add(fd) {
            return Err(());
        }
        let key = sys::sock_key(fd);
        if key > *max {
            *max = key;
        }
    }
    Ok(Some(set))
}

/// Compacts `a` down to the sockets that came back ready, NUL-terminated when
/// there is room. `sys.net.Socket.outArray` stops at that first null.
unsafe fn make_array_result(set: &sys::FdSet, a: *mut varray) {
    if a.is_null() {
        return;
    }
    let count = (*a).size.max(0);
    let aptr = hl_aptr::<*mut hl_socket>(a);
    let mut pos = 0usize;
    for i in 0..count {
        let s = *aptr.add(i as usize);
        if s.is_null() {
            break;
        }
        if set.contains((*s).sock) {
            *aptr.add(pos) = s;
            pos += 1;
        }
    }
    if pos < count as usize {
        *aptr.add(pos) = ptr::null_mut();
    }
}

/// `DEFINE_PRIM(_BOOL, socket_select, _ARR _ARR _ARR _BYTES _I32 _F64)`
#[no_mangle]
pub unsafe extern "C" fn hlp_socket_select(
    ra: *mut varray,
    wa: *mut varray,
    ea: *mut varray,
    tmp: *mut vbyte,
    tmp_size: c_int,
    timeout: f64,
) -> bool {
    let mut cursor = tmp;
    // Without scratch space no set can be built; the all-NULL case still
    // works, since select then just sleeps out the timeout.
    let mut left = if tmp.is_null() { 0 } else { tmp_size.max(0) };
    let mut max: u64 = 0;

    let rs = match make_socket_set(ra, &mut cursor, &mut left, &mut max) {
        Ok(set) => set,
        Err(()) => return false,
    };
    let ws = match make_socket_set(wa, &mut cursor, &mut left, &mut max) {
        Ok(set) => set,
        Err(()) => return false,
    };
    let es = match make_socket_set(ea, &mut cursor, &mut left, &mut max) {
        Ok(set) => set,
        Err(()) => return false,
    };

    // A negative timeout means "block indefinitely".
    let deadline = if timeout < 0.0 { None } else { Some(timeout) };
    hl_blocking(true);
    let rc = sys::select(max + 1, rs.as_ref(), ws.as_ref(), es.as_ref(), deadline);
    hl_blocking(false);
    if rc < 0 {
        return false;
    }

    if let Some(set) = rs.as_ref() {
        make_array_result(set, ra);
    }
    if let Some(set) = ws.as_ref() {
        make_array_result(set, wa);
    }
    if let Some(set) = es.as_ref() {
        make_array_result(set, ea);
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Once;

    /// The primitives hand results back in GC memory, so the test process has
    /// to stand the VM heap up before calling any of them.
    fn vm_heap() {
        static INIT: Once = Once::new();
        INIT.call_once(|| unsafe { crate::gc::hlp_gc_init() });
    }

    /// Marshal a host name the way `sys.net.Host` does: NUL-terminated UTF-8,
    /// not the UTF-16 the string primitives take.
    fn pc(s: &str) -> Vec<u8> {
        let mut v = s.as_bytes().to_vec();
        v.push(0);
        v
    }

    unsafe fn unpc(p: *const vbyte) -> String {
        assert!(!p.is_null());
        String::from_utf8_lossy(cstr_slice(p)).into_owned()
    }

    unsafe fn loopback() -> c_int {
        let mut name = pc("127.0.0.1");
        hlp_host_resolve(name.as_mut_ptr())
    }

    /// Drives the real `socket_select` path, scratch buffer and all, and
    /// reads the answer out of the in-place compaction the Haxe side expects.
    unsafe fn wait_readable(s: *mut hl_socket) -> bool {
        let a = crate::array::hlp_alloc_array(crate::types::hlt_bytes(), 1);
        let slots = hl_aptr::<*mut hl_socket>(a);
        *slots = s;
        let size = hlp_socket_fd_size(1);
        assert!(size > 0);
        let tmp = hlp_alloc_bytes(size);
        if !hlp_socket_select(a, ptr::null_mut(), ptr::null_mut(), tmp, size, 2.0) {
            return false;
        }
        // A socket that was not ready gets overwritten with the NULL sentinel.
        !(*slots).is_null()
    }

    #[test]
    fn host_int_is_wire_ordered() {
        vm_heap();
        unsafe {
            let ip = loopback();
            // `sys.net.Host.ip` is a raw s_addr: octets in wire order.
            assert_eq!(ip.to_ne_bytes(), [127, 0, 0, 1]);
            assert_eq!(unpc(hlp_host_to_string(ip)), "127.0.0.1");

            // Unresolvable is -1, which `sys.net.Host` turns into
            // "Unresolved host". The empty name fails in the resolver without
            // putting a query on the wire, so this stays offline.
            let mut bad = pc("");
            assert_eq!(hlp_host_resolve(bad.as_mut_ptr()), -1);
            assert_eq!(hlp_host_resolve(ptr::null_mut()), -1);

            assert!(!hlp_host_local().is_null());
        }
    }

    #[test]
    fn null_handle_returns_sentinels() {
        unsafe {
            let n: *mut hl_socket = ptr::null_mut();
            assert_eq!(hlp_socket_recv_char(n), -2);
            assert_eq!(hlp_socket_send_char(n, 0), -2);
            assert_eq!(hlp_socket_send(n, ptr::null_mut(), 0, 0), -2);
            assert_eq!(hlp_socket_recv(n, ptr::null_mut(), 0, 0), -2);
            assert!(!hlp_socket_connect(n, 0, 0));
            assert!(!hlp_socket_listen(n, 1));
            assert!(!hlp_socket_bind(n, 0, 0));
            assert!(hlp_socket_accept(n).is_null());
            assert!(!hlp_socket_set_blocking(n, true));
            assert!(!hlp_socket_set_timeout(n, 1.0));
            assert!(!hlp_socket_set_fast_send(n, true));
            // The null check precedes the no-op case, so even a shutdown
            // that asks for nothing fails on a null handle.
            assert!(!hlp_socket_shutdown(n, false, false));
            hlp_socket_close(n);
        }
    }

    #[test]
    fn loopback_stream_round_trip() {
        vm_heap();
        unsafe {
            hlp_socket_init();
            let host = loopback();

            let server = hlp_socket_new(false);
            assert!(!server.is_null());
            assert!(hlp_socket_bind(server, host, 0));
            assert!(hlp_socket_listen(server, 4));

            let (mut bound_host, mut bound_port) = (0, 0);
            assert!(hlp_socket_host(server, &mut bound_host, &mut bound_port));
            assert_eq!(bound_host, host);
            assert!(bound_port > 0);

            let client = hlp_socket_new(false);
            assert!(!client.is_null());
            assert!(hlp_socket_set_blocking(client, false));
            assert!(hlp_socket_set_fast_send(client, true));
            // A non-blocking connect reports "in progress" as success.
            assert!(hlp_socket_connect(client, host, bound_port));

            assert!(wait_readable(server));
            let accepted = hlp_socket_accept(server);
            assert!(!accepted.is_null());
            assert!(hlp_socket_set_timeout(accepted, 2.0));

            let mut out = b"..ash".to_vec();
            assert_eq!(hlp_socket_send(client, out.as_mut_ptr(), 2, 3), 3);
            assert!(wait_readable(accepted));
            let mut got = [0u8; 8];
            assert_eq!(hlp_socket_recv(accepted, got.as_mut_ptr(), 1, 7), 3);
            assert_eq!(&got[1..4], b"ash");

            assert_eq!(hlp_socket_send_char(accepted, 0x7a), 1);
            assert!(wait_readable(client));
            assert_eq!(hlp_socket_recv_char(client), 0x7a);

            // The accepted socket's peer is whatever the client bound itself to.
            let (mut peer_host, mut peer_port) = (0, 0);
            assert!(hlp_socket_peer(accepted, &mut peer_host, &mut peer_port));
            let (mut own_host, mut own_port) = (0, 0);
            assert!(hlp_socket_host(client, &mut own_host, &mut own_port));
            assert_eq!((peer_host, peer_port), (own_host, own_port));

            // Asking to shut down neither direction is a no-op success.
            assert!(hlp_socket_shutdown(accepted, false, false));
            assert!(hlp_socket_shutdown(accepted, true, true));
            hlp_socket_close(accepted);
            assert!(wait_readable(client));
            // End of stream is -2, the value `sys.net.Socket` turns into Eof.
            assert_eq!(hlp_socket_recv_char(client), -2);

            hlp_socket_close(client);
            hlp_socket_close(server);
        }
    }

    #[test]
    fn loopback_datagram_round_trip() {
        vm_heap();
        unsafe {
            hlp_socket_init();
            let host = loopback();

            let rx = hlp_socket_new(true);
            assert!(!rx.is_null());
            assert!(hlp_socket_bind(rx, host, 0));
            let (mut bound_host, mut bound_port) = (0, 0);
            assert!(hlp_socket_host(rx, &mut bound_host, &mut bound_port));
            assert!(bound_port > 0);

            let tx = hlp_socket_new(true);
            assert!(!tx.is_null());
            let mut out = b"ping".to_vec();
            assert_eq!(
                hlp_socket_send_to(tx, out.as_mut_ptr(), 4, host, bound_port),
                4
            );

            assert!(wait_readable(rx));
            let mut got = [0u8; 8];
            let (mut from_host, mut from_port) = (0, 0);
            assert_eq!(
                hlp_socket_recv_from(rx, got.as_mut_ptr(), 8, &mut from_host, &mut from_port),
                4
            );
            assert_eq!(&got[..4], b"ping");
            assert_eq!(from_host, host);
            assert!(from_port > 0);

            hlp_socket_close(tx);
            hlp_socket_close(rx);
        }
    }
}
