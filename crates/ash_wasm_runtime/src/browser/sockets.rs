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
//! The guest's socket module talks preview 1 wherever preview 1 has a call,
//! and asks the host for the two it does not have:
//!
//! | import | here |
//! |---|---|
//! | `env.ash_host_socket_open` | take a descriptor from the table |
//! | `env.ash_host_socket_connect` | `new WebSocket(url)`, non-blocking |
//! | `wasi_snapshot_preview1.sock_send` | `WebSocket.send` |
//! | `wasi_snapshot_preview1.sock_recv` | drain the receive queue |
//! | `wasi_snapshot_preview1.sock_shutdown` | `WebSocket.close` |
//! | `wasi_snapshot_preview1.fd_close` | close and drop |
//!
//! `bind` and `listen` are refused by the guest before they reach here.
//!
//! # Two places the semantics differ, both visible to a program
//!
//! **Connecting does not block.** A `WebSocket` is open when its `open` event
//! fires, which is after `connect` returns. `connect` therefore reports
//! `EAGAIN` until the socket is up, and a caller polls -- which is what a
//! non-blocking socket does everywhere, so `sys.net.Socket.setBlocking(false)`
//! programs need no change. A program that expects `connect` to block wants
//! the host to suspend it, which is what `ash_host_fiber_yield` is for.
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

/// The descriptor table. Descriptors are indices into it, offset so that zero
/// through two stay with the standard streams.
#[derive(Default)]
pub struct Sockets {
    entries: Vec<Option<Socket>>,
}

const FD_BASE: i32 = 3;

impl Sockets {
    fn slot(&mut self, fd: i32) -> Option<&mut Socket> {
        let index = usize::try_from(fd - FD_BASE).ok()?;
        self.entries.get_mut(index)?.as_mut()
    }

    /// Reserve a descriptor. Nothing is connected yet: a socket exists before
    /// it has a peer, and the guest may set options on it first.
    pub fn open(&mut self) -> i32 {
        let index = match self.entries.iter().position(Option::is_none) {
            Some(i) => i,
            None => {
                self.entries.push(None);
                self.entries.len() - 1
            }
        };
        // The entry stays empty until `connect` supplies the WebSocket; the
        // slot is what the descriptor names.
        index as i32 + FD_BASE
    }

    /// Point a descriptor at a peer.
    ///
    /// `ip` and `port` are what the guest resolved, and a page cannot use
    /// them: it has no resolver and no raw addressing. The address is
    /// rendered back into a URL on the assumption that a host wanting real
    /// addressing supplies its own mapping; the common case is a program
    /// connecting to the page's own origin.
    pub fn connect(&mut self, fd: i32, ip: i32, port: i32) -> i32 {
        let index = match usize::try_from(fd - FD_BASE) {
            Ok(i) if i < self.entries.len() => i,
            _ => return errno::BADF,
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

        let octets = [
            (ip >> 24) as u8,
            (ip >> 16) as u8,
            (ip >> 8) as u8,
            ip as u8,
        ];
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
    /// whole thing. A program that shuts down its write side and keeps
    /// reading will see the peer go away, which is the one place this leaks.
    pub fn shutdown(&mut self, fd: i32) -> i32 {
        match self.slot(fd) {
            Some(socket) => {
                let _ = socket.ws.close();
                errno::SUCCESS
            }
            None => errno::BADF,
        }
    }

    pub fn close(&mut self, fd: i32) -> i32 {
        let index = match usize::try_from(fd - FD_BASE) {
            Ok(i) if i < self.entries.len() => i,
            _ => return errno::BADF,
        };
        if let Some(socket) = self.entries[index].take() {
            let _ = socket.ws.close();
            errno::SUCCESS
        } else {
            errno::BADF
        }
    }

    /// A page cannot listen, so nothing ever accepts.
    pub fn accept(&mut self, _fd: i32) -> i32 {
        -errno::NOTSUP
    }
}
