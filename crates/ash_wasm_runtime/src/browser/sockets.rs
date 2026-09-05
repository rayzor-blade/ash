//! Sockets in a page, over WebSocket.
//!
//! A browser will not open a TCP connection to an arbitrary host and port.
//! That is a security boundary, not a missing feature, and no shim removes
//! it. What a page can open is a `WebSocket` to a peer that speaks the
//! WebSocket protocol, which carries a byte stream once it is up. So this is
//! a real socket layer with a real limit: a program that connects to a
//! WebSocket peer (directly, or through a relay that bridges to TCP) works
//! unchanged, and a program that expects to listen does not.
//!
//! # What the guest asks for
//!
//! The guest's socket module asks the host for every socket operation --
//! WASI preview 1 has no descriptor that can be a socket -- through twelve
//! `env.ash_host_socket_*` imports. Descriptors are the host's own namespace,
//! indices into the table below starting at 0; errors are WASI preview 1
//! errno numbers; and a host that cannot do something answers `NOTSUP`,
//! which the guest reports as the refusal a kernel would have given.
//!
//! | import | here |
//! |---|---|
//! | `env.ash_host_socket_open(udp)` | reserve a slot; UDP is `NOTSUP` |
//! | `env.ash_host_socket_connect(fd, ip, port)` | `new WebSocket(url)`; `AGAIN` until open |
//! | `env.ash_host_socket_bind` | `NOTSUP`: a page cannot listen |
//! | `env.ash_host_socket_listen` | `NOTSUP` |
//! | `env.ash_host_socket_accept` | `-NOTSUP` |
//! | `env.ash_host_socket_send(fd, buf, len)` | `WebSocket.send` |
//! | `env.ash_host_socket_recv(fd, buf, len)` | drain the receive queue; 0 once closed |
//! | `env.ash_host_socket_shutdown(fd, how)` | `WebSocket.close`, whichever side was asked |
//! | `env.ash_host_socket_close(fd)` | close and drop |
//! | `env.ash_host_socket_name(fd, which, out)` | `NOTSUP`: a page sees no addresses |
//! | `env.ash_host_socket_set(fd, opt, value)` | see [`Sockets::set`] |
//! | `env.ash_host_socket_poll(fds, nfds, timeout)` | evaluated now, never waited for |
//!
//! # Where the semantics differ, all visible to a program
//!
//! **Connecting does not block.** A `WebSocket` is open when its `open` event
//! fires, which is after `connect` returns. `connect` therefore reports
//! `EAGAIN` until the socket is up, and a caller polls -- which is what a
//! non-blocking socket does everywhere, so `sys.net.Socket.setBlocking(false)`
//! programs need no change. A program that expects `connect` to block wants
//! the host to suspend it, which is what `ash_host_fiber_yield` is for.
//!
//! **Nothing blocks, including `poll`.** A page has one thread and it is the
//! event loop; a host function that waited on it would wait for the events
//! that can never arrive while it waits. `poll` answers what is ready at the
//! moment of the call and ignores its timeout; a program that wants to wait
//! yields to the host and asks again.
//!
//! **A message is not a byte range.** WebSocket delivers whole messages;
//! `recv` hands back bytes. Messages are queued whole and drained by byte, so
//! a `recv` smaller than the message returns part of it and the rest stays
//! queued. A protocol that assumes a `send` and a `recv` pair up will find
//! they do not, exactly as on TCP.

use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;

use js_sys::Uint8Array;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{BinaryType, MessageEvent, WebSocket};

/// WASI errno values this module reports. The guest compares against these
/// numbers, so they are the platform's rather than ours.
mod errno {
    pub const SUCCESS: i32 = 0;
    pub const AGAIN: i32 = 6;
    pub const BADF: i32 = 8;
    pub const CONNREFUSED: i32 = 14;
    pub const NOTSUP: i32 = 58;
}

/// The readiness bits of a poll record, ash's own numbering shared with the
/// guest by value: `RD` 1, `WR` 2, `PRI` 4, `ERR` 8, `HUP` 16, `NVAL` 32.
mod ev {
    pub const RD: u16 = 1;
    pub const WR: u16 = 2;
    pub const NVAL: u16 = 32;
}

/// One record of a poll request as the guest lays it out: 8 bytes, `fd`
/// then `events` then `revents`, little-endian.
pub struct PollFd {
    pub fd: i32,
    pub events: u16,
    pub revents: u16,
}

/// One socket: the WebSocket itself, and what has arrived on it.
struct Socket {
    ws: WebSocket,
    /// Bytes received and not yet handed to the guest. One queue rather than
    /// a queue of messages, because the guest reads by byte count.
    inbox: Rc<RefCell<VecDeque<u8>>>,
    /// Set by the `close` and `error` handlers. A closed socket reads as end
    /// of stream rather than as an error, which is what a peer hanging up
    /// looks like on TCP.
    closed: Rc<RefCell<bool>>,
    /// Kept alive for as long as the socket is: dropping a closure detaches
    /// the listener.
    _on_message: Closure<dyn FnMut(MessageEvent)>,
    _on_close: Closure<dyn FnMut(JsValue)>,
}

/// The descriptor table. Descriptors are indices into it, starting at 0: the
/// guest's sockets are their own namespace and never meet a WASI fd, so
/// nothing has to be skipped for the standard streams.
#[derive(Default)]
pub struct Sockets {
    entries: Vec<Option<Socket>>,
}

impl Sockets {
    fn index(&self, fd: i32) -> Option<usize> {
        let index = usize::try_from(fd).ok()?;
        (index < self.entries.len()).then_some(index)
    }

    fn slot(&mut self, fd: i32) -> Option<&mut Socket> {
        let index = self.index(fd)?;
        self.entries[index].as_mut()
    }

    /// Reserve a descriptor. Nothing is connected yet: a socket exists before
    /// it has a peer, and the guest may set options on it first. A page has
    /// no datagrams, so a UDP socket is refused here rather than at its first
    /// use.
    pub fn open(&mut self, udp: i32) -> i32 {
        if udp != 0 {
            return -errno::NOTSUP;
        }
        let index = match self.entries.iter().position(Option::is_none) {
            Some(i) => i,
            None => {
                self.entries.push(None);
                self.entries.len() - 1
            }
        };
        // The entry stays empty until `connect` supplies the WebSocket; the
        // slot is what the descriptor names.
        index as i32
    }

    /// Point a descriptor at a peer.
    ///
    /// `ip` and `port` are what the guest resolved, and a page cannot use
    /// them: it has no resolver and no raw addressing. The address is
    /// rendered back into a URL on the assumption that a host wanting real
    /// addressing supplies its own mapping; the common case is a program
    /// connecting to the page's own origin.
    pub fn connect(&mut self, fd: i32, ip: i32, port: i32) -> i32 {
        let Some(index) = self.index(fd) else {
            return errno::BADF;
        };
        if let Some(existing) = &self.entries[index] {
            // Already connecting or connected: report progress rather than
            // opening a second socket.
            return match existing.ws.ready_state() {
                WebSocket::OPEN => errno::SUCCESS,
                WebSocket::CONNECTING => errno::AGAIN,
                _ => errno::CONNREFUSED,
            };
        }

        // `ip` is `s_addr` as it sits in memory -- network byte order -- read
        // back as a little-endian i32 on wasm32, so the first octet is the
        // LOW byte. Shifting from the top rendered every address reversed.
        let octets = (ip as u32).to_le_bytes();
        let url = format!(
            "ws://{}.{}.{}.{}:{}/",
            octets[0], octets[1], octets[2], octets[3], port
        );
        let Ok(ws) = WebSocket::new(&url) else {
            return errno::CONNREFUSED;
        };
        // Binary frames as bytes rather than as a Blob, so a message can be
        // copied out synchronously when the guest asks for it.
        ws.set_binary_type(BinaryType::Arraybuffer);

        let inbox = Rc::new(RefCell::new(VecDeque::new()));
        let closed = Rc::new(RefCell::new(false));

        let queue = inbox.clone();
        let on_message = Closure::<dyn FnMut(MessageEvent)>::new(move |e: MessageEvent| {
            if let Ok(buffer) = e.data().dyn_into::<js_sys::ArrayBuffer>() {
                let bytes = Uint8Array::new(&buffer).to_vec();
                queue.borrow_mut().extend(bytes);
            } else if let Some(text) = e.data().as_string() {
                queue.borrow_mut().extend(text.into_bytes());
            }
        });
        ws.set_onmessage(Some(on_message.as_ref().unchecked_ref()));

        let flag = closed.clone();
        let on_close = Closure::<dyn FnMut(JsValue)>::new(move |_| {
            *flag.borrow_mut() = true;
        });
        ws.set_onclose(Some(on_close.as_ref().unchecked_ref()));
        ws.set_onerror(Some(on_close.as_ref().unchecked_ref()));

        self.entries[index] = Some(Socket {
            ws,
            inbox,
            closed,
            _on_message: on_message,
            _on_close: on_close,
        });
        // Open is an event, not a return value.
        errno::AGAIN
    }

    /// A page cannot listen, so there is nothing to bind to.
    pub fn bind(&mut self, _fd: i32, _ip: i32, _port: i32) -> i32 {
        errno::NOTSUP
    }

    pub fn listen(&mut self, _fd: i32, _backlog: i32) -> i32 {
        errno::NOTSUP
    }

    /// A page cannot listen, so nothing ever accepts.
    pub fn accept(&mut self, _fd: i32) -> i32 {
        -errno::NOTSUP
    }

    /// Send bytes. Returns the count written, or a negative errno.
    pub fn send(&mut self, fd: i32, bytes: &[u8]) -> i32 {
        let Some(socket) = self.slot(fd) else {
            return -errno::BADF;
        };
        match socket.ws.ready_state() {
            WebSocket::CONNECTING => -errno::AGAIN,
            WebSocket::OPEN => match socket.ws.send_with_u8_array(bytes) {
                Ok(()) => bytes.len() as i32,
                Err(_) => -errno::CONNREFUSED,
            },
            _ => -errno::CONNREFUSED,
        }
    }

    /// Take up to `out.len()` received bytes. Zero means end of stream on a
    /// closed socket, and `EAGAIN` means nothing has arrived yet.
    pub fn recv(&mut self, fd: i32, out: &mut [u8]) -> i32 {
        let Some(socket) = self.slot(fd) else {
            return -errno::BADF;
        };
        let mut inbox = socket.inbox.borrow_mut();
        if inbox.is_empty() {
            return if *socket.closed.borrow() {
                0
            } else {
                -errno::AGAIN
            };
        }
        let n = out.len().min(inbox.len());
        for slot in out.iter_mut().take(n) {
            *slot = inbox.pop_front().unwrap_or(0);
        }
        n as i32
    }

    /// WebSocket closes in one direction only, so a half shutdown closes the
    /// whole thing whichever side `how` names (1 read, 2 write). A program
    /// that shuts down its write side and keeps reading will see the peer go
    /// away, which is the one place this leaks.
    pub fn shutdown(&mut self, fd: i32, _how: i32) -> i32 {
        match self.slot(fd) {
            Some(socket) => {
                let _ = socket.ws.close();
                errno::SUCCESS
            }
            None => errno::BADF,
        }
    }

    pub fn close(&mut self, fd: i32) -> i32 {
        let Some(index) = self.index(fd) else {
            return errno::BADF;
        };
        if let Some(socket) = self.entries[index].take() {
            let _ = socket.ws.close();
            errno::SUCCESS
        } else {
            errno::BADF
        }
    }

    /// A page sees neither its own address nor the peer's, so `host()` and
    /// `peer()` come back null in the program rather than invented.
    pub fn name(&self, _fd: i32, _which: i32) -> Result<(i32, i32), i32> {
        Err(errno::NOTSUP)
    }

    /// Socket options, most of which a WebSocket has already decided.
    ///
    /// `opt` 0 is blocking: a WebSocket is non-blocking and cannot be made
    /// otherwise, so asking for non-blocking succeeds and asking for
    /// blocking is `NOTSUP`. 1 (`TCP_NODELAY`) succeeds without doing
    /// anything, since a WebSocket frame goes out when it is sent. 2
    /// (`SO_BROADCAST`) is a datagram option and there are no datagrams. 3
    /// (a timeout) succeeds without recording anything: nothing here ever
    /// blocks, so there is nothing for a timeout to cut short.
    pub fn set(&mut self, fd: i32, opt: i32, value: i32) -> i32 {
        if self.slot(fd).is_none() {
            return errno::BADF;
        }
        match opt {
            0 if value == 0 => errno::SUCCESS,
            0 => errno::NOTSUP,
            1 | 3 => errno::SUCCESS,
            _ => errno::NOTSUP,
        }
    }

    /// What is ready right now. Readable when bytes are queued or the socket
    /// has closed (the read then reports end of stream); writable while the
    /// WebSocket is open; never priority data. A descriptor that is not a
    /// socket is `NVAL`. The timeout is ignored for the reason the module
    /// header gives, and the count of records with a non-zero `revents` is
    /// returned.
    pub fn poll(&mut self, fds: &mut [PollFd], _timeout_ms: i32) -> i32 {
        let mut ready = 0;
        for record in fds.iter_mut() {
            record.revents = match self.slot(record.fd) {
                None => ev::NVAL,
                Some(socket) => {
                    let mut bits = 0;
                    if !socket.inbox.borrow().is_empty() || *socket.closed.borrow() {
                        bits |= ev::RD;
                    }
                    if socket.ws.ready_state() == WebSocket::OPEN {
                        bits |= ev::WR;
                    }
                    bits & record.events
                }
            };
            if record.revents != 0 {
                ready += 1;
            }
        }
        ready
    }
}
