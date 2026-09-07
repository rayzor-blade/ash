//! Binding a page's answers to a module's imports.
//!
//! A module names 69 imports and every one must be supplied before it can be
//! instantiated -- an import nothing answers is a link error, not a runtime
//! one, so a program fails at load for a function it would never have called.
//! That is why the refusals matter as much as the implementations.
//!
//! # Why the refusals can share one function
//!
//! Thirty of these exist only to say no. A JavaScript function ignores
//! arguments it was not declared to take, so one zero-argument function
//! answering `ENOTSUP` binds correctly to an import of any arity, and the
//! thirty become two: one refusal for the calls that name a path, one for the
//! calls that name a descriptor that a page never opened.
//!
//! # The order of the two halves
//!
//! The host functions need the guest's memory, and the guest does not have
//! one until it is instantiated -- with these imports. So the memory is
//! shared through a cell that is empty while the imports are built and filled
//! the moment the instance exists. A host function called before then, which
//! only a `start` section could manage, answers `EFAULT` rather than reaching
//! into nothing.

use std::cell::RefCell;
use std::rc::Rc;

use js_sys::{Function, Object, Reflect};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;

use crate::wasi_abi::errno;

use super::fibers::Fibers;
use super::memory::Guest;
use super::sockets::Sockets;
use super::threads::Threads;
use super::wasi::Wasi;

/// Everything a page holds on a program's behalf.
pub struct Host {
    pub(super) control: super::control::Control,
    pub wasi: RefCell<Wasi>,
    pub sockets: RefCell<Sockets>,
    /// Set once the module has been instantiated. See the note above.
    pub guest: RefCell<Option<Guest>>,
    /// The globals ash's link-time transform added, if the module carries it.
    /// Read from the same exports as the memory, and for the same reason.
    pub fibers: RefCell<Fibers>,
    /// Where a frame goes, if the page gave this Worker somewhere to put one.
    pub canvas: super::canvas::Canvas,
}

impl Host {
    pub fn new(args: Vec<String>, environ: Vec<String>) -> Rc<Self> {
        Self::with_control(args, environ, super::control::Control::default())
    }

    pub(super) fn with_control(
        args: Vec<String>,
        environ: Vec<String>,
        control: super::control::Control,
    ) -> Rc<Self> {
        Rc::new(Self {
            control,
            wasi: RefCell::new(Wasi::new(args, environ)),
            sockets: RefCell::new(Sockets::default()),
            guest: RefCell::new(None),
            fibers: RefCell::new(Fibers::default()),
            canvas: super::canvas::Canvas::default(),
        })
    }

    /// Point the host at the memory the instance turned out to have.
    pub fn attach(&self, guest: Guest) {
        *self.guest.borrow_mut() = Some(guest);
    }

    /// The same for the transform's globals, which are exports too.
    pub fn attach_fibers(&self, fibers: Fibers) {
        *self.fibers.borrow_mut() = fibers;
    }
}

/// Keep a closure alive for as long as the page is.
///
/// A `Closure` detaches its JavaScript function when it is dropped, and these
/// are reachable for the whole life of the program, so they are deliberately
/// leaked rather than tracked. The alternative is an owner that outlives the
/// instance and has no other purpose.
fn install(object: &Object, name: &str, function: JsValue) {
    // The object is one this module just made, so a failed set is a bug here
    // rather than something a caller can act on.
    let _ = Reflect::set(object, &JsValue::from_str(name), &function);
}

/// Bind a host function of any arity. See the note above about arity.
macro_rules! bind {
    // A closure taking nothing lexes `||` as one token, so it needs its own
    // rule rather than an empty repetition.
    ($object:expr, $name:literal, move || -> i32 $body:expr) => {{
        let closure = Closure::wrap(Box::new(move || -> i32 { $body }) as Box<dyn FnMut() -> i32>);
        install($object, $name, closure.into_js_value());
    }};
    ($object:expr, $name:literal, move |$($arg:ident: $ty:ty),*| -> i32 $body:expr) => {{
        let closure = Closure::wrap(Box::new(move |$($arg: $ty),*| -> i32 { $body })
            as Box<dyn FnMut($($ty),*) -> i32>);
        install($object, $name, closure.into_js_value());
    }};
    ($object:expr, $name:literal, move |$($arg:ident: $ty:ty),*| $body:expr) => {{
        let closure = Closure::wrap(Box::new(move |$($arg: $ty),*| { $body })
            as Box<dyn FnMut($($ty),*)>);
        install($object, $name, closure.into_js_value());
    }};
}

/// The imports object to instantiate a module with.
///
/// Three namespaces, as the module names them: `wasi_snapshot_preview1` for
/// the standard library's own calls, `env` for what ash asks of a host beyond
/// it, and `wasi` for the one function a threads build asks for.
pub fn imports(
    host: &Rc<Host>,
    memory: Option<&js_sys::WebAssembly::Memory>,
    threads: &Rc<Threads>,
) -> Object {
    let imports = Object::new();
    let preview1 = Object::new();
    let env = Object::new();
    // `wasi` and `wasi_snapshot_preview1` are two module names, not one
    // shortened. The threads interface never went into preview 1 and imports
    // its single function from its own namespace.
    let wasi = Object::new();

    install_wasi(&preview1, host);
    install_env(&env, host);
    let h = host.clone();
    let wait32 = Closure::wrap(Box::new(
        move |address: u32, expected: i32, timeout: i64, offset: u64| -> Result<i32, JsValue> {
            let guest = h
                .guest
                .borrow()
                .clone()
                .ok_or_else(|| JsValue::from_str("atomic wait without memory"))?;
            h.control.wait(
                guest.memory(),
                address,
                offset,
                expected as i64,
                timeout,
                false,
            )
        },
    )
        as Box<dyn FnMut(u32, i32, i64, u64) -> Result<i32, JsValue>>);
    install(&env, ash_wasm_link::waits::WAIT32, wait32.into_js_value());
    let h = host.clone();
    let wait64 = Closure::wrap(Box::new(
        move |address: u32, expected: i64, timeout: i64, offset: u64| -> Result<i32, JsValue> {
            let guest = h
                .guest
                .borrow()
                .clone()
                .ok_or_else(|| JsValue::from_str("atomic wait without memory"))?;
            h.control
                .wait(guest.memory(), address, offset, expected, timeout, true)
        },
    )
        as Box<dyn FnMut(u32, i64, i64, u64) -> Result<i32, JsValue>>);
    install(&env, ash_wasm_link::waits::WAIT64, wait64.into_js_value());
    // A threads build imports its memory rather than defining one, so that
    // every thread instantiates against the same one. The host made it; here
    // is where the module is given it.
    if let Some(memory) = memory {
        install(&env, "memory", memory.clone().into());
    }
    // Always bound: an import nothing answers is a link error before a line
    // runs, so a program that never draws still needs this to exist.
    let _ = super::canvas::install(&env, host);
    let starting = Rc::clone(threads);
    bind!(&wasi, "thread-spawn", move |start_arg: i32| -> i32 {
        starting.spawn(start_arg)
    });

    install(
        &imports,
        "wasi_snapshot_preview1",
        preview1.unchecked_into::<JsValue>(),
    );
    install(&imports, "env", env.unchecked_into::<JsValue>());
    install(&imports, "wasi", wasi.unchecked_into::<JsValue>());
    imports
}

/// A guest accessor for the duration of one call, or `EFAULT`.
macro_rules! guest {
    ($host:expr) => {{
        $host.control.check();
        match $host.guest.borrow().clone() {
            Some(g) => g,
            None => return errno::FAULT,
        }
    }};
}

fn install_wasi(wasi: &Object, host: &Rc<Host>) {
    let h = host.clone();
    bind!(wasi, "fd_write", move |fd: i32, iovs: u32, len: u32, out: u32| -> i32 {
        let g = guest!(h);
        h.wasi.borrow_mut().fd_write(&g, fd, iovs, len, out)
    });

    let h = host.clone();
    bind!(wasi, "fd_read", move |fd: i32, iovs: u32, len: u32, out: u32| -> i32 {
        let g = guest!(h);
        h.wasi.borrow_mut().fd_read(&g, fd, iovs, len, out)
    });

    let h = host.clone();
    bind!(wasi, "fd_close", move |fd: i32| -> i32 {
        h.wasi.borrow_mut().fd_close(fd)
    });

    let h = host.clone();
    bind!(wasi, "fd_fdstat_get", move |fd: i32, out: u32| -> i32 {
        let g = guest!(h);
        h.wasi.borrow().fd_fdstat_get(&g, fd, out)
    });

    let h = host.clone();
    bind!(wasi, "fd_filestat_get", move |fd: i32, out: u32| -> i32 {
        let g = guest!(h);
        h.wasi.borrow().fd_filestat_get(&g, fd, out)
    });

    let h = host.clone();
    bind!(wasi, "fd_seek", move |fd: i32| -> i32 {
        h.wasi.borrow().fd_seek(fd)
    });

    let h = host.clone();
    bind!(wasi, "fd_prestat_get", move |fd: i32| -> i32 {
        h.wasi.borrow().fd_prestat_get(fd)
    });

    let h = host.clone();
    bind!(wasi, "clock_time_get", move |id: u32, _precision: i64, out: u32| -> i32 {
        // `precision` is an i64 and has to be declared as one: a wasm i64
        // reaches JavaScript as a BigInt, and a binding that reads it as two
        // i32 halves takes the wrong number of arguments and leaves `out`
        // with no argument at all. It is advisory, and this clock ignores it.
        let g = guest!(h);
        h.wasi.borrow().clock_time_get(&g, id, out)
    });

    let h = host.clone();
    bind!(wasi, "clock_res_get", move |id: u32, out: u32| -> i32 {
        let g = guest!(h);
        h.wasi.borrow().clock_res_get(&g, id, out)
    });

    let h = host.clone();
    bind!(wasi, "random_get", move |ptr: u32, len: u32| -> i32 {
        let g = guest!(h);
        h.wasi.borrow().random_get(&g, ptr, len)
    });

    let h = host.clone();
    bind!(wasi, "args_sizes_get", move |a: u32, b: u32| -> i32 {
        let g = guest!(h);
        h.wasi.borrow().args_sizes_get(&g, a, b)
    });

    let h = host.clone();
    bind!(wasi, "args_get", move |a: u32, b: u32| -> i32 {
        let g = guest!(h);
        h.wasi.borrow().args_get(&g, a, b)
    });

    let h = host.clone();
    bind!(wasi, "environ_sizes_get", move |a: u32, b: u32| -> i32 {
        let g = guest!(h);
        h.wasi.borrow().environ_sizes_get(&g, a, b)
    });

    let h = host.clone();
    bind!(wasi, "environ_get", move |a: u32, b: u32| -> i32 {
        let g = guest!(h);
        h.wasi.borrow().environ_get(&g, a, b)
    });

    let h = host.clone();
    bind!(wasi, "poll_oneoff", move |s: u32, e: u32, n: u32, out: u32| -> i32 {
        let g = guest!(h);
        h.wasi.borrow().poll_oneoff(&g, s, e, n, out)
    });

    let h = host.clone();
    bind!(wasi, "sched_yield", move || -> i32 {
        h.control.check();
        h.wasi.borrow().sched_yield()
    });

    // `proc_exit` does not return. Recording the status and then throwing
    // unwinds the guest, which is how a trap reaches the embedder; letting it
    // return would run the program past its own end.
    let h = host.clone();
    bind!(wasi, "proc_exit", move |code: i32| exit_now(&h, code));

    // A page has no filesystem, so every call that names one is refused, and
    // every call naming a descriptor it never opened says so. See the note at
    // the top about why one function can answer them all.
    let refuse = constant(errno::NOTSUP);
    for name in [
        "path_create_directory",
        "path_filestat_get",
        "path_filestat_set_times",
        "path_link",
        "path_open",
        "path_readlink",
        "path_remove_directory",
        "path_rename",
        "path_symlink",
        "path_unlink_file",
    ] {
        install(wasi, name, refuse.clone().into());
    }

    let bad = constant(errno::BADF);
    for name in [
        "fd_advise",
        "fd_allocate",
        "fd_datasync",
        "fd_fdstat_set_flags",
        "fd_fdstat_set_rights",
        "fd_filestat_set_size",
        "fd_filestat_set_times",
        "fd_pread",
        "fd_prestat_dir_name",
        "fd_pwrite",
        "fd_readdir",
        "fd_renumber",
        "fd_sync",
        "fd_tell",
        // WASI's own socket calls, which ash never uses: its sockets are the
        // `env.ash_host_socket_*` imports, because preview 1 has no
        // descriptor that can be one.
        "sock_accept",
        "sock_recv",
        "sock_send",
        "sock_shutdown",
    ] {
        install(wasi, name, bad.clone().into());
    }
}

/// Record the status, then unwind the guest.
///
/// A named function rather than the closure body, because the throw diverges
/// and a closure whose body diverges is inferred to return `!` rather than
/// the `()` the import needs. Declaring the return type here settles it.
fn exit_now(host: &Rc<Host>, code: i32) {
    host.wasi.borrow_mut().proc_exit(code);
    let memory = host.guest.borrow().as_ref().map(|g| g.memory().clone());
    host.control.finish(
        &super::run::Outcome {
            status: code,
            trapped: None,
        },
        memory.as_ref(),
    );
    wasm_bindgen::throw_str("ash: the program exited");
}

/// One function answering `code` whatever it is passed.
///
/// For the imports a page can only refuse, and for the few that are honestly
/// constant. See the note at the top about why arity does not matter.
fn constant(code: i32) -> Function {
    let closure = Closure::wrap(Box::new(move || -> i32 { code }) as Box<dyn FnMut() -> i32>);
    closure.into_js_value().unchecked_into()
}

/// Bridge `poll` across the boundary: read the guest's records, answer them,
/// write the readiness back.
///
/// A record is eight bytes -- `fd` then `events` then `revents`, all
/// little-endian -- and only the last two of them are written back, because
/// the other six are the caller's and it still owns them.
fn poll_through(host: &Rc<Host>, guest: &Guest, at: u32, count: u32, timeout: i32) -> i32 {
    const RECORD: u32 = 8;
    let Some(bytes) = guest.read(at, count.saturating_mul(RECORD)) else {
        return -errno::FAULT;
    };
    let mut records: Vec<super::sockets::PollFd> = Vec::with_capacity(count as usize);
    for i in 0..count as usize {
        let base = i * RECORD as usize;
        records.push(super::sockets::PollFd {
            fd: i32::from_le_bytes(bytes[base..base + 4].try_into().unwrap_or_default()),
            events: u16::from_le_bytes(bytes[base + 4..base + 6].try_into().unwrap_or_default()),
            revents: 0,
        });
    }
    let ready = host.sockets.borrow_mut().poll(&mut records, timeout);
    for (i, record) in records.iter().enumerate() {
        let offset = at + i as u32 * RECORD + 6;
        if !guest.write(offset, &record.revents.to_le_bytes()) {
            return -errno::FAULT;
        }
    }
    ready
}

fn install_env(env: &Object, host: &Rc<Host>) {
    // Sockets: the twelve, against the WebSocket table.
    let h = host.clone();
    bind!(env, "ash_host_socket_open", move |udp: i32| -> i32 {
        h.sockets.borrow_mut().open(udp)
    });

    let h = host.clone();
    bind!(env, "ash_host_socket_connect", move |fd: i32, ip: i32, port: i32| -> i32 {
        h.sockets.borrow_mut().connect(fd, ip, port)
    });

    let h = host.clone();
    bind!(env, "ash_host_socket_bind", move |fd: i32, ip: i32, port: i32| -> i32 {
        h.sockets.borrow_mut().bind(fd, ip, port)
    });

    let h = host.clone();
    bind!(env, "ash_host_socket_listen", move |fd: i32, backlog: i32| -> i32 {
        h.sockets.borrow_mut().listen(fd, backlog)
    });

    let h = host.clone();
    bind!(env, "ash_host_socket_accept", move |fd: i32| -> i32 {
        h.sockets.borrow_mut().accept(fd)
    });

    let h = host.clone();
    bind!(env, "ash_host_socket_send", move |fd: i32, ptr: u32, len: u32| -> i32 {
        let g = guest!(h);
        let Some(bytes) = g.read(ptr, len) else {
            return -errno::FAULT;
        };
        h.sockets.borrow_mut().send(fd, &bytes)
    });

    let h = host.clone();
    bind!(env, "ash_host_socket_recv", move |fd: i32, ptr: u32, len: u32| -> i32 {
        let g = guest!(h);
        let mut buffer = vec![0u8; len as usize];
        let got = h.sockets.borrow_mut().recv(fd, &mut buffer);
        if got > 0 && !g.write(ptr, &buffer[..got as usize]) {
            return -errno::FAULT;
        }
        got
    });

    let h = host.clone();
    bind!(env, "ash_host_socket_shutdown", move |fd: i32, how: i32| -> i32 {
        h.sockets.borrow_mut().shutdown(fd, how)
    });

    let h = host.clone();
    bind!(env, "ash_host_socket_close", move |fd: i32| -> i32 {
        h.sockets.borrow_mut().close(fd)
    });

    let h = host.clone();
    bind!(env, "ash_host_socket_name", move |fd: i32, which: i32, out: u32| -> i32 {
        let g = guest!(h);
        match h.sockets.borrow().name(fd, which) {
            Ok((ip, port)) => {
                if !g.write_u32(out, ip as u32) || !g.write_u32(out + 4, port as u32) {
                    return errno::FAULT;
                }
                errno::SUCCESS
            }
            Err(e) => e,
        }
    });

    let h = host.clone();
    bind!(env, "ash_host_socket_set", move |fd: i32, opt: i32, value: i32| -> i32 {
        h.sockets.borrow_mut().set(fd, opt, value)
    });

    let h = host.clone();
    bind!(env, "ash_host_socket_poll", move |fds: u32, n: u32, timeout: i32| -> i32 {
        let g = guest!(h);
        poll_through(&h, &g, fds, n, timeout)
    });

    // Fibers, through the globals ash's link-time transform added. A module
    // without the transform has none, every call answers zero, and a fiber
    // runs to completion at the point it would have suspended -- which is
    // what the native host does for the same module.
    let h = host.clone();
    bind!(env, "ash_host_fiber_yield", move || -> i32 {
        h.fibers.borrow().yield_now();
        0
    });

    let h = host.clone();
    bind!(env, "ash_host_fiber_state", move || -> i32 {
        h.fibers.borrow().state()
    });

    let h = host.clone();
    bind!(env, "ash_host_fiber_arm", move |data: i32, rewind: i32, sp: i32| -> i32 {
        h.fibers.borrow().arm(data, rewind, sp)
    });

    // Starting a process is not something a page does, at all. `-1` is what
    // `Sys.command` reports when a shell cannot be started, and what the
    // native host answers when it has not been given permission.
    for name in [
        "ash_host_command",
        "ash_host_process_start",
        "ash_host_process_len",
        "ash_host_process_read",
        "ash_host_process_code",
    ] {
        install(env, name, constant(-1).into());
    }
    bind!(env, "ash_host_process_free", move |_h: i32| {});

    // A page's environment is what the embedder passed in and nothing later
    // reads it back out of a process, so setting one changes only the guest's
    // own view -- which `hlp_sys_put_env` has already done by the time this
    // is called.
    bind!(env, "ash_host_put_env", move |_n: u32, _nl: i32, _v: u32, _vl: i32| {});

    // Loading a native library in a page is the same steps against
    // `WebAssembly.instantiate`, and is not written yet: see
    // `docs/wasm-hdlls.md`. Answering zero is "no such library", which the
    // guest reports only if a primitive is actually reached.
    for name in ["ash_host_dlopen", "ash_host_dlsym"] {
        install(env, name, constant(0).into());
    }
}
