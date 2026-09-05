//! The guest's sockets, over the operating system's.
//!
//! WASI preview 1 cannot carry a socket: it has no call that opens one, and
//! wasmtime's implementation of the four calls it does name answers
//! `ENOTSOCK` to every descriptor. So the guest's socket module
//! (`ash_std`'s `socket.rs`, wasm arm) asks the host for the whole thing
//! through twelve `env.ash_host_socket_*` imports, and this is the host's
//! answer: a table of OS sockets, with a translation on each side of the
//! call. Descriptors the guest sees are indices into that table, a namespace
//! private to these imports that starts at 0 and never meets a WASI fd;
//! errors cross as WASI preview 1 errno numbers, which is the one numbering
//! the guest and every host agree on; and readiness bits are ash's own,
//! because `POLLIN` is a different number on Darwin and on Linux.
//!
//! The imports are plain synchronous host functions on an async linker, and
//! a blocking `accept`, `recv` or `poll` blocks the host thread. `ash-wasm-run`
//! runs one guest and nothing else, so nothing is waiting behind it; a host
//! that multiplexes guests would want these to suspend instead, the way the
//! fiber import does.
//!
//! Readiness is evaluated with `select(2)`, not `poll(2)`. The guest is
//! implementing `select` -- `sys.net.Socket.select`, three sets and a timeout
//! -- and the two do not agree on Darwin: once the peer has closed, `poll`
//! reports the stream as `POLLIN|POLLPRI|POLLHUP` and not writable, while
//! `select` reports it readable and writable and not exceptional, which is
//! also what the unix runtime says and what the Haxe suite checks (measured
//! on this machine with both calls side by side). `select` answers the
//! question the guest asked; the poll-shaped record is just the transport,
//! and it stays because it carries N descriptors in one call regardless of
//! their numbers.
//!
//! On a host without BSD sockets -- Windows, where `libc` does not spell them
//! -- the twelve imports are still installed and every one answers `NOTSUP`,
//! so a program that never opens a socket runs there unchanged.

use anyhow::{anyhow, Result};
use wasmtime::{Caller, Extern, Linker};

use super::Host;

/// WASI preview 1 errno numbers. The guest branches on `AGAIN`, `ALREADY`,
/// `INPROGRESS` and `NOTSUP`; the rest are reported for the message.
mod errno {
    pub const ADDRINUSE: i32 = 3;
    pub const AGAIN: i32 = 6;
    pub const ALREADY: i32 = 7;
    pub const BADF: i32 = 8;
    pub const CONNREFUSED: i32 = 14;
    pub const CONNRESET: i32 = 15;
    pub const INPROGRESS: i32 = 26;
    pub const INVAL: i32 = 28;
    pub const IO: i32 = 29;
    pub const NOTCONN: i32 = 53;
    pub const NOTSOCK: i32 = 57;
    pub const NOTSUP: i32 = 58;
    pub const PIPE: i32 = 64;
    pub const TIMEDOUT: i32 = 73;
}

/// The readiness bits in a poll record, shared with the guest by value.
mod ev {
    pub const RD: u16 = 1;
    pub const WR: u16 = 2;
    pub const PRI: u16 = 4;
    pub const NVAL: u16 = 32;
}

/// One record of a poll request as the guest lays it out: 8 bytes, `fd`
/// then `events` then `revents`, little-endian like everything in wasm
/// memory.
pub struct PollFd {
    pub fd: i32,
    pub events: u16,
    pub revents: u16,
}

const POLLFD_SIZE: usize = 8;

impl PollFd {
    fn read(bytes: &[u8; POLLFD_SIZE]) -> PollFd {
        PollFd {
            fd: i32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
            events: u16::from_le_bytes([bytes[4], bytes[5]]),
            revents: u16::from_le_bytes([bytes[6], bytes[7]]),
        }
    }

    /// Only `revents` is the host's to write; the rest stays as the guest
    /// left it.
    fn write_revents(&self, bytes: &mut [u8; POLLFD_SIZE]) {
        bytes[6..8].copy_from_slice(&self.revents.to_le_bytes());
    }
}

/// An OS socket, or on a host without them the number that would have been
/// one.
type OsFd = i32;

/// The guest's descriptor table: guest descriptor = index, value = OS socket.
/// A closed slot is reused, as a kernel reuses a descriptor number; the guest
/// poisons its own handle on close, so nothing of its reaches a reused slot
/// by accident.
#[derive(Default)]
pub struct Table {
    entries: Vec<Option<OsFd>>,
}

impl Table {
    fn os(&self, fd: i32) -> std::result::Result<OsFd, i32> {
        usize::try_from(fd)
            .ok()
            .and_then(|i| self.entries.get(i).copied().flatten())
            .ok_or(errno::BADF)
    }

    fn insert(&mut self, os: OsFd) -> i32 {
        match self.entries.iter().position(Option::is_none) {
            Some(i) => {
                self.entries[i] = Some(os);
                i as i32
            }
            None => {
                self.entries.push(Some(os));
                (self.entries.len() - 1) as i32
            }
        }
    }

    fn remove(&mut self, fd: i32) -> std::result::Result<OsFd, i32> {
        let i = usize::try_from(fd).map_err(|_| errno::BADF)?;
        self.entries
            .get_mut(i)
            .and_then(Option::take)
            .ok_or(errno::BADF)
    }
}

#[cfg(unix)]
mod os {
    use std::ffi::{c_int, c_void};
    use std::io;
    use std::mem;
    use std::ptr;

    use super::{errno, ev, OsFd, PollFd, Table};

    /// Darwin's `send` only grew `MSG_NOSIGNAL` in recent SDKs; `SO_NOSIGPIPE`
    /// at open does the same job there, and the unix runtime makes the same
    /// split.
    #[cfg(target_vendor = "apple")]
    const NOSIGNAL: c_int = 0;
    #[cfg(not(target_vendor = "apple"))]
    const NOSIGNAL: c_int = libc::MSG_NOSIGNAL;

    /// The kernel's errno, renumbered as WASI preview 1's. Anything without a
    /// name in the guest's list is an I/O error, which is what it reads as.
    fn wasi_errno(e: &io::Error) -> i32 {
        match e.raw_os_error() {
            Some(code) if code == libc::EAGAIN || code == libc::EWOULDBLOCK => errno::AGAIN,
            Some(libc::EINPROGRESS) => errno::INPROGRESS,
            Some(libc::EALREADY) => errno::ALREADY,
            Some(libc::ECONNREFUSED) => errno::CONNREFUSED,
            Some(libc::EADDRINUSE) => errno::ADDRINUSE,
            Some(libc::EBADF) => errno::BADF,
            Some(libc::ENOTSOCK) => errno::NOTSOCK,
            Some(libc::ECONNRESET) => errno::CONNRESET,
            Some(libc::EPIPE) => errno::PIPE,
            Some(libc::ETIMEDOUT) => errno::TIMEDOUT,
            Some(libc::ENOTCONN) => errno::NOTCONN,
            Some(libc::EINVAL) => errno::INVAL,
            Some(libc::ENOTSUP) => errno::NOTSUP,
            _ => errno::IO,
        }
    }

    fn last_errno() -> i32 {
        wasi_errno(&io::Error::last_os_error())
    }

    /// `ip` is the `s_addr` the guest carries -- four octets in wire order
    /// read as an int -- so it goes in as it is; only the port is byte-swapped.
    fn sockaddr_in(ip: i32, port: i32) -> libc::sockaddr_in {
        let mut addr: libc::sockaddr_in = unsafe { mem::zeroed() };
        addr.sin_family = libc::AF_INET as libc::sa_family_t;
        addr.sin_port = (port as u16).to_be();
        addr.sin_addr.s_addr = ip as u32;
        addr
    }

    const ADDR_LEN: libc::socklen_t = mem::size_of::<libc::sockaddr_in>() as libc::socklen_t;

    unsafe fn set_int_opt(s: OsFd, level: c_int, name: c_int, value: c_int) -> c_int {
        libc::setsockopt(
            s,
            level,
            name,
            &value as *const c_int as *const c_void,
            mem::size_of::<c_int>() as libc::socklen_t,
        )
    }

    fn status(rc: c_int) -> i32 {
        if rc < 0 {
            last_errno()
        } else {
            0
        }
    }

    impl Table {
        /// Take ownership of a socket the kernel just handed over: not
        /// inherited by a child the host might spawn, and never a SIGPIPE.
        fn adopt(&mut self, s: OsFd) -> i32 {
            unsafe {
                let old = libc::fcntl(s, libc::F_GETFD, 0);
                if old >= 0 {
                    libc::fcntl(s, libc::F_SETFD, old | libc::FD_CLOEXEC);
                }
                #[cfg(target_vendor = "apple")]
                set_int_opt(s, libc::SOL_SOCKET, libc::SO_NOSIGPIPE, 1);
            }
            self.insert(s)
        }

        pub fn open(&mut self, udp: i32) -> i32 {
            let ty = if udp != 0 {
                libc::SOCK_DGRAM
            } else {
                libc::SOCK_STREAM
            };
            let s = unsafe { libc::socket(libc::AF_INET, ty, 0) };
            if s < 0 {
                return -last_errno();
            }
            self.adopt(s)
        }

        /// `EINPROGRESS` passes through: the guest treats an in-progress
        /// non-blocking connect as success and lets select finish it.
        pub fn connect(&mut self, fd: i32, ip: i32, port: i32) -> i32 {
            let s = match self.os(fd) {
                Ok(s) => s,
                Err(e) => return e,
            };
            let addr = sockaddr_in(ip, port);
            status(unsafe {
                libc::connect(
                    s,
                    &addr as *const libc::sockaddr_in as *const libc::sockaddr,
                    ADDR_LEN,
                )
            })
        }

        /// `SO_REUSEADDR` first, as the unix runtime does, so a listener
        /// restarted on a port still in `TIME_WAIT` binds.
        pub fn bind(&mut self, fd: i32, ip: i32, port: i32) -> i32 {
            let s = match self.os(fd) {
                Ok(s) => s,
                Err(e) => return e,
            };
            let addr = sockaddr_in(ip, port);
            status(unsafe {
                set_int_opt(s, libc::SOL_SOCKET, libc::SO_REUSEADDR, 1);
                libc::bind(
                    s,
                    &addr as *const libc::sockaddr_in as *const libc::sockaddr,
                    ADDR_LEN,
                )
            })
        }

        pub fn listen(&mut self, fd: i32, backlog: i32) -> i32 {
            match self.os(fd) {
                Ok(s) => status(unsafe { libc::listen(s, backlog) }),
                Err(e) => e,
            }
        }

        pub fn accept(&mut self, fd: i32) -> i32 {
            let s = match self.os(fd) {
                Ok(s) => s,
                Err(e) => return -e,
            };
            let a = unsafe { libc::accept(s, ptr::null_mut(), ptr::null_mut()) };
            if a < 0 {
                return -last_errno();
            }
            self.adopt(a)
        }

        pub fn send(&mut self, fd: i32, bytes: &[u8]) -> i32 {
            let s = match self.os(fd) {
                Ok(s) => s,
                Err(e) => return -e,
            };
            let n =
                unsafe { libc::send(s, bytes.as_ptr() as *const c_void, bytes.len(), NOSIGNAL) };
            if n < 0 {
                return -last_errno();
            }
            n as i32
        }

        /// Zero is end of stream, exactly as `recv(2)` reports it.
        pub fn recv(&mut self, fd: i32, bytes: &mut [u8]) -> i32 {
            let s = match self.os(fd) {
                Ok(s) => s,
                Err(e) => return -e,
            };
            let n = unsafe { libc::recv(s, bytes.as_mut_ptr() as *mut c_void, bytes.len(), 0) };
            if n < 0 {
                return -last_errno();
            }
            n as i32
        }

        /// `how` bit 1 is the read side, bit 2 the write side.
        pub fn shutdown(&mut self, fd: i32, how: i32) -> i32 {
            let s = match self.os(fd) {
                Ok(s) => s,
                Err(e) => return e,
            };
            let how = match (how & 1 != 0, how & 2 != 0) {
                (true, true) => libc::SHUT_RDWR,
                (true, false) => libc::SHUT_RD,
                (false, true) => libc::SHUT_WR,
                (false, false) => return 0,
            };
            status(unsafe { libc::shutdown(s, how) })
        }

        pub fn close(&mut self, fd: i32) -> i32 {
            match self.remove(fd) {
                Ok(s) => status(unsafe { libc::close(s) }),
                Err(e) => e,
            }
        }

        /// The bound (`which` 0) or peer (`which` 1) address as the pair the
        /// guest carries: `s_addr` in network order as an int, port in host
        /// order.
        pub fn name(&self, fd: i32, which: i32) -> std::result::Result<(i32, i32), i32> {
            let s = self.os(fd)?;
            let mut addr: libc::sockaddr_in = unsafe { mem::zeroed() };
            let mut len = ADDR_LEN;
            let sa = &mut addr as *mut libc::sockaddr_in as *mut libc::sockaddr;
            let rc = unsafe {
                match which {
                    0 => libc::getsockname(s, sa, &mut len),
                    1 => libc::getpeername(s, sa, &mut len),
                    _ => return Err(errno::INVAL),
                }
            };
            if rc < 0 {
                return Err(last_errno());
            }
            Ok((
                addr.sin_addr.s_addr as i32,
                u16::from_be(addr.sin_port) as i32,
            ))
        }

        /// `opt` 0 blocking, 1 `TCP_NODELAY`, 2 `SO_BROADCAST`, 3 send and
        /// receive timeout in milliseconds.
        pub fn set(&mut self, fd: i32, opt: i32, value: i32) -> i32 {
            let s = match self.os(fd) {
                Ok(s) => s,
                Err(e) => return e,
            };
            unsafe {
                match opt {
                    0 => {
                        let flags = libc::fcntl(s, libc::F_GETFL);
                        if flags < 0 {
                            return last_errno();
                        }
                        let flags = if value != 0 {
                            flags & !libc::O_NONBLOCK
                        } else {
                            flags | libc::O_NONBLOCK
                        };
                        status(libc::fcntl(s, libc::F_SETFL, flags))
                    }
                    1 => status(set_int_opt(
                        s,
                        libc::IPPROTO_TCP,
                        libc::TCP_NODELAY,
                        c_int::from(value != 0),
                    )),
                    2 => status(set_int_opt(
                        s,
                        libc::SOL_SOCKET,
                        libc::SO_BROADCAST,
                        c_int::from(value != 0),
                    )),
                    3 => {
                        let ms = value.max(0);
                        let tv = libc::timeval {
                            tv_sec: (ms / 1000) as _,
                            tv_usec: ((ms % 1000) * 1000) as _,
                        };
                        let p = &tv as *const libc::timeval as *const c_void;
                        let len = mem::size_of::<libc::timeval>() as libc::socklen_t;
                        let rc = libc::setsockopt(s, libc::SOL_SOCKET, libc::SO_SNDTIMEO, p, len);
                        if rc < 0 {
                            return last_errno();
                        }
                        status(libc::setsockopt(
                            s,
                            libc::SOL_SOCKET,
                            libc::SO_RCVTIMEO,
                            p,
                            len,
                        ))
                    }
                    _ => errno::NOTSUP,
                }
            }
        }

        /// Readiness across `fds`, answered with `select(2)` for the reason
        /// the module header gives. Returns how many records came back with
        /// a non-zero `revents`, or `-errno`.
        ///
        /// A record naming a descriptor this table does not hold, or one the
        /// fd_set cannot index, is answered `NVAL` at once without waiting --
        /// `poll(2)`'s behaviour, and what keeps a set built around a stale
        /// handle from sleeping out its timeout.
        pub fn poll(&self, fds: &mut [PollFd], timeout_ms: i32) -> i32 {
            let mut invalid = 0;
            for p in fds.iter_mut() {
                p.revents = 0;
                match self.os(p.fd) {
                    Ok(s) if (s as usize) < libc::FD_SETSIZE => {}
                    _ => {
                        p.revents = ev::NVAL;
                        invalid += 1;
                    }
                }
            }
            if invalid > 0 {
                return invalid;
            }

            let mut read: libc::fd_set = unsafe { mem::zeroed() };
            let mut write: libc::fd_set = unsafe { mem::zeroed() };
            let mut except: libc::fd_set = unsafe { mem::zeroed() };
            let rc = loop {
                // Rebuilt on every attempt: what select leaves in the sets
                // after an interrupted call is unspecified.
                let mut max: c_int = -1;
                unsafe {
                    libc::FD_ZERO(&mut read);
                    libc::FD_ZERO(&mut write);
                    libc::FD_ZERO(&mut except);
                    for p in fds.iter() {
                        let s = self.os(p.fd).unwrap_or(-1);
                        if p.events & ev::RD != 0 {
                            libc::FD_SET(s, &mut read);
                        }
                        if p.events & ev::WR != 0 {
                            libc::FD_SET(s, &mut write);
                        }
                        if p.events & ev::PRI != 0 {
                            libc::FD_SET(s, &mut except);
                        }
                        max = max.max(s);
                    }
                }
                // Linux counts the wait down in `tv`, Darwin leaves it; a
                // fresh value each time makes a signal extend the wait rather
                // than cut it short, which is the safer of the two errors.
                let mut tv = libc::timeval {
                    tv_sec: (timeout_ms / 1000) as _,
                    tv_usec: ((timeout_ms % 1000) * 1000) as _,
                };
                let tp = if timeout_ms < 0 {
                    ptr::null_mut()
                } else {
                    &mut tv as *mut libc::timeval
                };
                let rc = unsafe { libc::select(max + 1, &mut read, &mut write, &mut except, tp) };
                if rc >= 0 {
                    break rc;
                }
                let e = io::Error::last_os_error();
                if e.raw_os_error() != Some(libc::EINTR) {
                    return -wasi_errno(&e);
                }
            };
            if rc == 0 {
                return 0;
            }

            let mut ready = 0;
            for p in fds.iter_mut() {
                let s = self.os(p.fd).unwrap_or(-1);
                unsafe {
                    if libc::FD_ISSET(s, &read) {
                        p.revents |= ev::RD;
                    }
                    if libc::FD_ISSET(s, &write) {
                        p.revents |= ev::WR;
                    }
                    if libc::FD_ISSET(s, &except) {
                        p.revents |= ev::PRI;
                    }
                }
                if p.revents != 0 {
                    ready += 1;
                }
            }
            ready
        }
    }
}

/// A host without BSD sockets. The imports exist so the module instantiates,
/// and every one of them refuses.
#[cfg(not(unix))]
mod os {
    use super::{errno, PollFd, Table};

    impl Table {
        pub fn open(&mut self, _udp: i32) -> i32 {
            -errno::NOTSUP
        }
        pub fn connect(&mut self, _fd: i32, _ip: i32, _port: i32) -> i32 {
            errno::NOTSUP
        }
        pub fn bind(&mut self, _fd: i32, _ip: i32, _port: i32) -> i32 {
            errno::NOTSUP
        }
        pub fn listen(&mut self, _fd: i32, _backlog: i32) -> i32 {
            errno::NOTSUP
        }
        pub fn accept(&mut self, _fd: i32) -> i32 {
            -errno::NOTSUP
        }
        pub fn send(&mut self, _fd: i32, _bytes: &[u8]) -> i32 {
            -errno::NOTSUP
        }
        pub fn recv(&mut self, _fd: i32, _bytes: &mut [u8]) -> i32 {
            -errno::NOTSUP
        }
        pub fn shutdown(&mut self, _fd: i32, _how: i32) -> i32 {
            errno::NOTSUP
        }
        pub fn close(&mut self, fd: i32) -> i32 {
            self.remove(fd).map_or(errno::BADF, |_| 0)
        }
        pub fn name(&self, _fd: i32, _which: i32) -> std::result::Result<(i32, i32), i32> {
            Err(errno::NOTSUP)
        }
        pub fn set(&mut self, _fd: i32, _opt: i32, _value: i32) -> i32 {
            errno::NOTSUP
        }
        pub fn poll(&self, _fds: &mut [PollFd], _timeout_ms: i32) -> i32 {
            -errno::NOTSUP
        }
    }
}

/// The guest's bytes at `[ptr, ptr + len)`, with the store's data alongside
/// so one call can read or fill them and reach the socket table in a single
/// borrow.
///
/// A guest that hands over a bad range gets an error return rather than a
/// trap: a module with no exported memory cannot be served (`BADF`), and a
/// range outside it is the guest's argument error (`INVAL`). Neither is worth
/// taking the whole program down for when the call can simply fail.
fn guest_bytes<'a>(
    caller: &'a mut Caller<'_, Host>,
    ptr: i32,
    len: i32,
) -> std::result::Result<(&'a mut [u8], &'a mut Host), i32> {
    let Some(memory) = caller.get_export("memory").and_then(Extern::into_memory) else {
        return Err(errno::BADF);
    };
    let (data, host) = memory.data_and_store_mut(caller);
    let start = ptr as u32 as usize;
    let len = usize::try_from(len).map_err(|_| errno::INVAL)?;
    let end = start.checked_add(len).ok_or(errno::INVAL)?;
    let bytes = data.get_mut(start..end).ok_or(errno::INVAL)?;
    Ok((bytes, host))
}

/// Register the twelve imports.
pub fn install(linker: &mut Linker<Host>) -> Result<()> {
    fn failed(name: &str) -> impl FnOnce(wasmtime::Error) -> anyhow::Error + '_ {
        move |e| anyhow!("installing env.{name}: {e}")
    }

    linker
        .func_wrap(
            "env",
            "ash_host_socket_open",
            |mut caller: Caller<'_, Host>, udp: i32| -> i32 { caller.data_mut().sockets.open(udp) },
        )
        .map_err(failed("ash_host_socket_open"))?;
    linker
        .func_wrap(
            "env",
            "ash_host_socket_connect",
            |mut caller: Caller<'_, Host>, fd: i32, ip: i32, port: i32| -> i32 {
                caller.data_mut().sockets.connect(fd, ip, port)
            },
        )
        .map_err(failed("ash_host_socket_connect"))?;
    linker
        .func_wrap(
            "env",
            "ash_host_socket_bind",
            |mut caller: Caller<'_, Host>, fd: i32, ip: i32, port: i32| -> i32 {
                caller.data_mut().sockets.bind(fd, ip, port)
            },
        )
        .map_err(failed("ash_host_socket_bind"))?;
    linker
        .func_wrap(
            "env",
            "ash_host_socket_listen",
            |mut caller: Caller<'_, Host>, fd: i32, backlog: i32| -> i32 {
                caller.data_mut().sockets.listen(fd, backlog)
            },
        )
        .map_err(failed("ash_host_socket_listen"))?;
    linker
        .func_wrap(
            "env",
            "ash_host_socket_accept",
            |mut caller: Caller<'_, Host>, fd: i32| -> i32 { caller.data_mut().sockets.accept(fd) },
        )
        .map_err(failed("ash_host_socket_accept"))?;
    linker
        .func_wrap(
            "env",
            "ash_host_socket_send",
            |mut caller: Caller<'_, Host>, fd: i32, buf: i32, len: i32| -> i32 {
                match guest_bytes(&mut caller, buf, len) {
                    Ok((bytes, host)) => host.sockets.send(fd, bytes),
                    Err(e) => -e,
                }
            },
        )
        .map_err(failed("ash_host_socket_send"))?;
    linker
        .func_wrap(
            "env",
            "ash_host_socket_recv",
            |mut caller: Caller<'_, Host>, fd: i32, buf: i32, len: i32| -> i32 {
                match guest_bytes(&mut caller, buf, len) {
                    Ok((bytes, host)) => host.sockets.recv(fd, bytes),
                    Err(e) => -e,
                }
            },
        )
        .map_err(failed("ash_host_socket_recv"))?;
    linker
        .func_wrap(
            "env",
            "ash_host_socket_shutdown",
            |mut caller: Caller<'_, Host>, fd: i32, how: i32| -> i32 {
                caller.data_mut().sockets.shutdown(fd, how)
            },
        )
        .map_err(failed("ash_host_socket_shutdown"))?;
    linker
        .func_wrap(
            "env",
            "ash_host_socket_close",
            |mut caller: Caller<'_, Host>, fd: i32| -> i32 { caller.data_mut().sockets.close(fd) },
        )
        .map_err(failed("ash_host_socket_close"))?;
    linker
        .func_wrap(
            "env",
            "ash_host_socket_name",
            |mut caller: Caller<'_, Host>, fd: i32, which: i32, out: i32| -> i32 {
                match guest_bytes(&mut caller, out, 8) {
                    Ok((bytes, host)) => match host.sockets.name(fd, which) {
                        Ok((ip, port)) => {
                            bytes[..4].copy_from_slice(&ip.to_le_bytes());
                            bytes[4..].copy_from_slice(&port.to_le_bytes());
                            0
                        }
                        Err(e) => e,
                    },
                    Err(e) => e,
                }
            },
        )
        .map_err(failed("ash_host_socket_name"))?;
    linker
        .func_wrap(
            "env",
            "ash_host_socket_set",
            |mut caller: Caller<'_, Host>, fd: i32, opt: i32, value: i32| -> i32 {
                caller.data_mut().sockets.set(fd, opt, value)
            },
        )
        .map_err(failed("ash_host_socket_set"))?;
    linker
        .func_wrap(
            "env",
            "ash_host_socket_poll",
            |mut caller: Caller<'_, Host>, fds: i32, nfds: i32, timeout_ms: i32| -> i32 {
                let Ok(count) = usize::try_from(nfds) else {
                    return -errno::INVAL;
                };
                let Some(len) = count
                    .checked_mul(POLLFD_SIZE)
                    .and_then(|n| i32::try_from(n).ok())
                else {
                    return -errno::INVAL;
                };
                let (bytes, host) = match guest_bytes(&mut caller, fds, len) {
                    Ok(pair) => pair,
                    Err(e) => return -e,
                };
                // `as_chunks` rather than `chunks_exact`: the record size is a
                // constant, so the compiler gets fixed-size arrays and the
                // remainder is split off once instead of checked per record.
                let (records_in, _) = bytes.as_chunks::<POLLFD_SIZE>();
                let mut records: Vec<PollFd> = records_in.iter().map(PollFd::read).collect();
                let rc = host.sockets.poll(&mut records, timeout_ms);
                let (records_out, _) = bytes.as_chunks_mut::<POLLFD_SIZE>();
                for (record, slot) in records.iter().zip(records_out.iter_mut()) {
                    record.write_revents(slot);
                }
                rc
            },
        )
        .map_err(failed("ash_host_socket_poll"))?;
    Ok(())
}
