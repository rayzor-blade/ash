//! The native host: `wasmtime`.
//!
//! This is the one the conformance lane uses, and it is deliberately the
//! simpler of the two. `wasmtime` already implements WASI preview 1 properly,
//! and it has real fibers, so the import that must suspend suspends: with
//! async support on, a host function may await, and the engine puts the
//! guest's stack aside while something else runs. That is the same capability
//! JSPI gives a browser, available here without a browser.
//!
//! What WASI preview 1 does not have is sockets, and the guest asks for those
//! through its own `env.ash_host_socket_*` imports; [`sockets`] answers them
//! with the operating system's.
//!
//! It is also useful before the runtime is finished. A module that still
//! imports `hlp_*` -- because `ash_std` has not been linked into it yet --
//! does not fail with a linker's idea of an error; [`Program::missing`]
//! reports exactly which imports nothing satisfies, which during the port is
//! the question being asked.

pub(crate) mod dylink;
mod sockets;

use std::path::{Path, PathBuf};

use anyhow::{anyhow, Result};
use wasmtime::{Caller, Config, Engine, Linker, Module, Store};
use wasmtime_wasi::p1::{self, WasiP1Ctx};
use wasmtime_wasi::{DirPerms, FilePerms, WasiCtxBuilder};

/// What the guest gets to see of the outside world.
pub struct Program {
    engine: Engine,
    module: Module,
    /// Where it was loaded from, because a native library shipped with it is
    /// found beside it -- the same rule an HDLL has always followed.
    path: PathBuf,
}

/// How a run ended.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Outcome {
    /// The program returned, with this status.
    Exited(i32),
    /// The program trapped. The string is the trap, already formatted.
    Trapped(String),
}

/// The store's data: everything a host function reaches through its
/// `Caller`. WASI keeps its own descriptor table in `wasi`; the guest's
/// sockets live in a second table because a socket is not a WASI fd and must
/// never be mistaken for one -- closing guest socket 3 through `fd_close`
/// would close the preopened working directory.
pub(crate) struct Host {
    wasi: WasiP1Ctx,
    sockets: sockets::Table,
    /// What each `sys.io.Process` the guest started said. See
    /// [`install_process`] for why the whole of it is here at once.
    processes: Vec<Option<Finished>>,
    /// Native libraries loaded beside the program. See [`dylink`].
    pub(crate) libraries: dylink::Libraries,
}

/// A child that has already run, and how much of what it said the guest has
/// taken.
#[derive(Default)]
pub(crate) struct Finished {
    stdout: Vec<u8>,
    stderr: Vec<u8>,
    /// How far into each of the two the guest has read.
    taken: [usize; 2],
    code: i32,
}

impl Program {
    /// Load a module, without running it.
    pub fn load(path: &Path) -> Result<Self> {
        let mut config = Config::new();
        // Fibers need a host function that can suspend. In this wasmtime that
        // is the default rather than a switch, so nothing is set for it; the
        // capability is what matters, and `func_wrap_async` below uses it.
        //
        // Exceptions are not the default. ash's trap model is `setjmp`, which
        // the WebAssembly backend lowers into the exception-handling
        // instructions, so a module built from it does not even parse without
        // this: "exceptions proposal not enabled".
        config.wasm_exceptions(true);
        // Compiling a module is the cost of running one: the conformance
        // suite's module is 23MB and takes wasmtime six seconds and every
        // core, and isolation runs it once per case. wasmtime keeps compiled
        // code keyed by module hash in the user's cache directory, the same
        // one its own CLI uses; with the cache a second run of the same
        // module is a load. A cache that cannot be set up is not a reason
        // not to run, so that failure only costs the speed.
        if let Ok(cache) = wasmtime::Cache::from_file(None) {
            config.cache(Some(cache));
        }
        let engine =
            Engine::new(&config).map_err(|e| anyhow!("creating the wasmtime engine: {e}"))?;
        let module = Module::from_file(&engine, path)
            .map_err(|e| anyhow!("loading {}: {e}", path.display()))?;
        Ok(Self {
            engine,
            module,
            path: path.to_path_buf(),
        })
    }

    /// The imports this module needs that neither WASI nor this host supplies.
    ///
    /// Empty is the goal. Anything listed is a symbol the runtime has not
    /// provided yet, and during the port that list IS the work.
    pub fn missing(&self) -> Vec<String> {
        self.module
            .imports()
            .filter(|import| {
                let module = import.module();
                let known_wasi =
                    module.starts_with("wasi_snapshot_preview1") || module.starts_with("wasi_");
                let known_host = module == FIBER_YIELD_MODULE
                    && (import.name() == FIBER_YIELD_NAME
                        || import.name().starts_with("ash_host_"));
                !(known_wasi || known_host)
            })
            .map(|import| format!("{}.{}", import.module(), import.name()))
            .collect()
    }

    /// Run the program to completion.
    pub async fn run(&self, args: &[String], dirs: &[std::path::PathBuf]) -> Result<Outcome> {
        let missing = self.missing();
        if !missing.is_empty() {
            return Err(anyhow!(
                "the module imports {} symbol(s) no host can supply, the first few being {}. \
                 A program linked against the wasm runtime imports only WASI and the \
                 {}.ash_host_* imports this host provides.",
                missing.len(),
                missing
                    .iter()
                    .take(5)
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(", "),
                FIBER_YIELD_MODULE,
            ));
        }

        let mut wasi = WasiCtxBuilder::new();
        wasi.inherit_stdout().inherit_stderr();
        // The working directory, as the program's own. A native ash program
        // can write a file beside itself; a wasm one can only reach what the
        // host preopens, and with nothing preopened every `File.write` failed
        // with "Can't open" while the same program ran natively. The
        // directory is the one the host was started in, nothing above it.
        if let Err(e) = wasi.preopened_dir(".", ".", DirPerms::all(), FilePerms::all()) {
            eprintln!("[ash-wasm-run] the working directory is not available to the program: {e}");
        }
        // Anything else the operator named, on the command line or in
        // ASH_WASM_DIRS. A module can open only what has been opened for it,
        // so a program that legitimately reaches outside its own directory --
        // upwards, most often -- needs the host to say so rather than to be
        // refused at the boundary with nothing to do about it.
        let named: Vec<std::path::PathBuf> = std::env::var("ASH_WASM_DIRS")
            .unwrap_or_default()
            .split(',')
            .map(str::trim)
            .filter(|d| !d.is_empty())
            .map(std::path::PathBuf::from)
            .collect();
        for dir in dirs.iter().chain(named.iter()) {
            let at = dir.to_string_lossy().into_owned();
            if let Err(e) = wasi.preopened_dir(dir, &at, DirPerms::all(), FilePerms::all()) {
                eprintln!("[ash-wasm-run] {at} is not available to the program: {e}");
            }
        }
        for arg in args {
            wasi.arg(arg);
        }
        // The runtime inside the module reads its switches from the
        // environment, exactly as the native one does -- ASH_GC_STRESS and the
        // rest. Without this the guest sees an empty environment and every
        // diagnostic is unreachable, which is the difference between being
        // able to ask a question of a wasm build and not.
        //
        // Only ASH_ names cross by default. A wasm module is a sandbox and
        // the host's environment is not its business; handing over PATH and
        // credentials to get one debugging flag through is not a trade worth
        // making.
        //
        // ASH_WASM_ENV names what else may. A program that legitimately reads
        // a variable -- `Sys.getEnv` means the same thing on every other
        // target -- would otherwise see nothing at all on wasm, and the host
        // is the only party that can say which of its variables the guest is
        // entitled to. Naming them keeps that decision explicit and with the
        // side that owns the secret.
        let allowed: Vec<String> = std::env::var("ASH_WASM_ENV")
            .unwrap_or_default()
            .split([',', ' '])
            .map(str::trim)
            .filter(|n| !n.is_empty())
            .map(str::to_string)
            .collect();
        for (key, value) in std::env::vars() {
            if key.starts_with("ASH_") || allowed.contains(&key) {
                wasi.env(&key, &value);
            }
        }
        let mut store = Store::new(
            &self.engine,
            Host {
                wasi: wasi.build_p1(),
                sockets: sockets::Table::default(),
                processes: Vec::new(),
                libraries: dylink::Libraries::default(),
            },
        );

        let mut linker: Linker<Host> = Linker::new(&self.engine);
        p1::add_to_linker_async(&mut linker, |host: &mut Host| &mut host.wasi)
            .map_err(|e| anyhow!("adding WASI to the linker: {e}"))?;
        install_fiber_yield(&mut linker)?;
        sockets::install(&mut linker)?;

        let instance = linker
            .instantiate_async(&mut store, &self.module)
            .await
            .map_err(|e| anyhow!("instantiating the module: {e}"))?;

        // Before the program initialises, because that is when it resolves
        // its primitives -- and because instantiating a module from inside a
        // call the guest is making is a knot not worth tying.
        let libraries = dylink::load_beside(&mut store, &linker, &instance, &self.path).await?;
        if !libraries.is_empty() {
            eprintln!(
                "[ash] loaded native {}: {}",
                if libraries.names().len() == 1 {
                    "library"
                } else {
                    "libraries"
                },
                libraries.names().join(", ")
            );
        }
        store.data_mut().libraries = libraries;

        // A command module is entered through `_start`; one linked without a
        // command entry is entered through `main`.
        let entry = instance
            .get_typed_func::<(), ()>(&mut store, "_start")
            .ok()
            .map(Entry::Start)
            .or_else(|| {
                instance
                    .get_typed_func::<(i32, i32), i32>(&mut store, "main")
                    .ok()
                    .map(Entry::Main)
            })
            .ok_or_else(|| anyhow!("the module exports neither _start nor main"))?;

        match entry.call(&mut store).await {
            Ok(code) => Ok(Outcome::Exited(code)),
            Err(err) => {
                // `proc_exit` unwinds by trapping, and a status is how it
                // reports itself rather than a failure.
                if let Some(exit) = err.downcast_ref::<wasmtime_wasi::I32Exit>() {
                    return Ok(Outcome::Exited(exit.0));
                }
                Ok(Outcome::Trapped(format!("{err:?}")))
            }
        }
    }
}

enum Entry {
    Start(wasmtime::TypedFunc<(), ()>),
    Main(wasmtime::TypedFunc<(i32, i32), i32>),
}

impl Entry {
    async fn call(self, store: &mut Store<Host>) -> Result<i32> {
        match self {
            Entry::Start(f) => {
                f.call_async(store, ()).await?;
                Ok(0)
            }
            // argc/argv: the program reads its arguments through WASI, so the
            // C-shaped pair is passed empty.
            Entry::Main(f) => Ok(f.call_async(store, (0, 0)).await?),
        }
    }
}

const FIBER_YIELD_MODULE: &str = crate::FIBER_YIELD_IMPORT.0;
const FIBER_YIELD_NAME: &str = crate::FIBER_YIELD_IMPORT.1;

/// The globals the link-time fiber transform adds, if it was applied.
const STATE_GLOBAL: &str = "ash_fiber_state";
const DATA_GLOBAL: &str = "ash_fiber_data";
/// The module's own shadow stack pointer, exported only when the linker
/// instrumented it, because a fiber has to run on a region of its own.
const SP_GLOBAL: &str = "__stack_pointer";

/// State values shared with the transform, which are Asyncify's.
const UNWINDING: i32 = 1;
const REWINDING: i32 = 2;

/// Let the guest run a command, if this host has been told it may.
///
/// This is the one import that leaves the sandbox, so it is off unless
/// `ASH_WASM_ALLOW_COMMAND` says otherwise, and the refusal is the same -1 a
/// native `Sys.command` returns when the shell cannot be started. The guest
/// cannot grant itself the capability; only whoever started the host can.
fn install_command(linker: &mut Linker<Host>) -> Result<()> {
    let allowed = matches!(
        std::env::var("ASH_WASM_ALLOW_COMMAND").as_deref(),
        Ok("1") | Ok("on") | Ok("yes")
    );
    linker
        .func_wrap(
            FIBER_YIELD_MODULE,
            "ash_host_command",
            move |mut caller: Caller<'_, Host>, ptr: i32, len: i32| -> i32 {
                if !allowed {
                    return -1;
                }
                let Some(wasmtime::Extern::Memory(memory)) = caller.get_export("memory") else {
                    return -1;
                };
                let (data, _) = memory.data_and_store_mut(&mut caller);
                let (Ok(start), Ok(len)) = (usize::try_from(ptr), usize::try_from(len)) else {
                    return -1;
                };
                let Some(bytes) = data.get(start..start.saturating_add(len)) else {
                    return -1;
                };
                let line = String::from_utf8_lossy(bytes).into_owned();
                run_command(&line)
            },
        )
        .map_err(|e| anyhow!("installing the command import: {e}"))?;
    Ok(())
}

/// A command line with the guest's idea of an absolute path made into the
/// host's.
///
/// The guest's root is the directory this host preopened for it, which is
/// this host's own working directory, so a path the guest built from its
/// `Sys.getCwd()` arrives looking absolute and names something under `.`.
/// Dropping the leading separator is the whole translation, and it is right
/// for every path the guest can produce, because that root is the only one it
/// has.
///
/// This works on the string rather than on tokens: splitting on whitespace
/// would take `"/temp/two words"` apart and put it back as two arguments. A
/// separator only starts a path where a token does -- at the beginning, or
/// after a space or a quote -- so those are the only places it is dropped.
fn rebase_guest_paths(line: &str) -> String {
    // The first token names the program, and a program is usually somewhere
    // the guest cannot see, so it gets the resolves-or-not test the whole
    // line cannot have -- the rest of the line may well name a path that is
    // about to be created.
    if let Some(cmd) = line.split_whitespace().next() {
        if line.starts_with(cmd) && cmd.starts_with('/') && !Path::new(&rebase_guest_arg(cmd)).exists() {
            let (_, rest) = line.split_at(cmd.len());
            return format!("{cmd}{}", rebase_guest_paths_in(rest));
        }
    }
    rebase_guest_paths_in(line)
}

fn rebase_guest_paths_in(line: &str) -> String {
    let mut out = String::with_capacity(line.len());
    let mut starts_token = true;
    for c in line.chars() {
        if c == '/' && starts_token {
            // The separator is dropped, and what follows is no longer at the
            // start of a token.
            starts_token = false;
            continue;
        }
        starts_token = matches!(c, ' ' | '\t' | '"' | '\'');
        out.push(c);
    }
    out
}

/// The platform's shell, answering the way `Sys.command` does everywhere else.
fn run_command(line: &str) -> i32 {
    let line = &rebase_guest_paths(line);
    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        match std::process::Command::new("/bin/sh")
            .arg("-c")
            .arg(line)
            .status()
        {
            Ok(s) => s.code().unwrap_or(0) | (s.signal().unwrap_or(0) << 8),
            Err(_) => -1,
        }
    }
    #[cfg(windows)]
    {
        use std::os::windows::process::CommandExt;
        let shell =
            std::env::var_os("COMSPEC").unwrap_or_else(|| std::ffi::OsString::from("cmd.exe"));
        match std::process::Command::new(shell)
            .raw_arg("/C")
            .raw_arg(line)
            .status()
        {
            Ok(s) => s.code().unwrap_or(-1),
            Err(_) => -1,
        }
    }
}

/// Let the guest run a `sys.io.Process` -- once, to completion.
///
/// A host function is not concurrent with the guest that called it, so
/// nothing here can hand a guest a live child to interleave with. The guest
/// side ([`ash_std::process`] on wasm) collects the command and everything
/// written to its input, and calls `start` at the first point the answer is
/// actually needed; this runs it and keeps what it said, and the other four
/// imports hand that over.
///
/// Spawning leaves the sandbox, so it obeys the same `ASH_WASM_ALLOW_COMMAND`
/// switch [`install_command`] does, and refuses the same way when it is unset.
fn install_process(linker: &mut Linker<Host>) -> Result<()> {
    let allowed = matches!(
        std::env::var("ASH_WASM_ALLOW_COMMAND").as_deref(),
        Ok("1") | Ok("on") | Ok("yes")
    );
    linker
        .func_wrap(
            FIBER_YIELD_MODULE,
            "ash_host_process_start",
            move |mut caller: Caller<'_, Host>,
                  argv: i32,
                  argv_len: i32,
                  input: i32,
                  input_len: i32,
                  shell: i32|
                  -> i32 {
                if !allowed {
                    return -1;
                }
                let Some(argv) = guest_slice(&mut caller, argv, argv_len) else {
                    return -1;
                };
                let Some(input) = guest_slice(&mut caller, input, input_len) else {
                    return -1;
                };
                if shell != 0 {
                    let line = rebase_guest_paths(&String::from_utf8_lossy(&argv));
                    let (cmd, args) = shell_command(&line);
                    return match run_to_completion(&cmd, &args, &input) {
                        Some(done) => {
                            let table = &mut caller.data_mut().processes;
                            table.push(Some(done));
                            (table.len() - 1) as i32
                        }
                        None => -1,
                    };
                }
                let mut parts = argv.split(|b| *b == 0);
                let Some(cmd) = parts.next().filter(|c| !c.is_empty()) else {
                    return -1;
                };
                let cmd = rebase_command(&String::from_utf8_lossy(cmd));
                let args: Vec<String> = parts
                    .map(|a| rebase_guest_arg(&String::from_utf8_lossy(a)))
                    .collect();
                let Some(done) = run_to_completion(&cmd, &args, &input) else {
                    return -1;
                };
                let table = &mut caller.data_mut().processes;
                table.push(Some(done));
                (table.len() - 1) as i32
            },
        )
        .map_err(|e| anyhow!("installing the process start import: {e}"))?;

    // `Sys.putEnv` in the guest, applied to this process so that a child
    // started later inherits it. Not gated: it changes nothing outside this
    // process, and without it a guest's own environment and its children's
    // disagree.
    linker
        .func_wrap(
            FIBER_YIELD_MODULE,
            "ash_host_put_env",
            |mut caller: Caller<'_, Host>, name: i32, name_len: i32, value: i32, value_len: i32| {
                let Some(name) = guest_slice(&mut caller, name, name_len) else {
                    return;
                };
                let name = String::from_utf8_lossy(&name).into_owned();
                if value_len < 0 {
                    std::env::remove_var(name);
                    return;
                }
                let Some(value) = guest_slice(&mut caller, value, value_len) else {
                    return;
                };
                std::env::set_var(name, String::from_utf8_lossy(&value).as_ref());
            },
        )
        .map_err(|e| anyhow!("installing the environment import: {e}"))?;

    linker
        .func_wrap(
            FIBER_YIELD_MODULE,
            "ash_host_process_len",
            |mut caller: Caller<'_, Host>, handle: i32, which: i32| -> i32 {
                let Some(done) = process_of(&mut caller, handle) else {
                    return 0;
                };
                let (stream, taken) = match which {
                    0 => (&done.stdout, done.taken[0]),
                    1 => (&done.stderr, done.taken[1]),
                    _ => return 0,
                };
                stream.len().saturating_sub(taken).min(i32::MAX as usize) as i32
            },
        )
        .map_err(|e| anyhow!("installing the process length import: {e}"))?;

    linker
        .func_wrap(
            FIBER_YIELD_MODULE,
            "ash_host_process_read",
            |mut caller: Caller<'_, Host>, handle: i32, which: i32, into: i32, len: i32| -> i32 {
                let (Ok(into), Ok(len)) = (usize::try_from(into), usize::try_from(len)) else {
                    return -1;
                };
                // Take a copy, because the memory borrow below and the store
                // data cannot both be held.
                let chunk = {
                    let Some(done) = process_of(&mut caller, handle) else {
                        return -1;
                    };
                    let (stream, taken) = match which {
                        0 => (&done.stdout, &mut done.taken[0]),
                        1 => (&done.stderr, &mut done.taken[1]),
                        _ => return -1,
                    };
                    let n = stream.len().saturating_sub(*taken).min(len);
                    let chunk = stream[*taken..*taken + n].to_vec();
                    *taken += n;
                    chunk
                };
                if chunk.is_empty() {
                    return 0;
                }
                let Some(wasmtime::Extern::Memory(memory)) = caller.get_export("memory") else {
                    return -1;
                };
                let (data, _) = memory.data_and_store_mut(&mut caller);
                let Some(dst) = data.get_mut(into..into.saturating_add(chunk.len())) else {
                    return -1;
                };
                dst.copy_from_slice(&chunk);
                chunk.len() as i32
            },
        )
        .map_err(|e| anyhow!("installing the process read import: {e}"))?;

    linker
        .func_wrap(
            FIBER_YIELD_MODULE,
            "ash_host_process_code",
            |mut caller: Caller<'_, Host>, handle: i32| -> i32 {
                process_of(&mut caller, handle).map_or(-1, |d| d.code)
            },
        )
        .map_err(|e| anyhow!("installing the process code import: {e}"))?;

    linker
        .func_wrap(
            FIBER_YIELD_MODULE,
            "ash_host_process_free",
            |mut caller: Caller<'_, Host>, handle: i32| {
                if let Ok(i) = usize::try_from(handle) {
                    if let Some(slot) = caller.data_mut().processes.get_mut(i) {
                        *slot = None;
                    }
                }
            },
        )
        .map_err(|e| anyhow!("installing the process free import: {e}"))?;
    Ok(())
}

/// A guest pointer and length as host bytes, or nothing if it does not name
/// memory the guest has.
fn guest_slice(caller: &mut Caller<'_, Host>, ptr: i32, len: i32) -> Option<Vec<u8>> {
    let (start, len) = (usize::try_from(ptr).ok()?, usize::try_from(len).ok()?);
    let wasmtime::Extern::Memory(memory) = caller.get_export("memory")? else {
        return None;
    };
    let (data, _) = memory.data_and_store_mut(caller);
    Some(data.get(start..start.checked_add(len)?)?.to_vec())
}

/// One entry of the guest's table, if that handle is live.
fn process_of<'a>(caller: &'a mut Caller<'_, Host>, handle: i32) -> Option<&'a mut Finished> {
    let i = usize::try_from(handle).ok()?;
    caller.data_mut().processes.get_mut(i)?.as_mut()
}

/// A single argument with the guest's idea of an absolute path made into the
/// host's, on the same reasoning as [`rebase_guest_paths`] -- but on an
/// argument that is already one token, so it needs no scanning.
fn rebase_guest_arg(arg: &str) -> String {
    arg.strip_prefix('/').unwrap_or(arg).to_string()
}

/// The same, for the one argument that names a program.
///
/// A program is usually somewhere the guest cannot see at all -- `/bin/sh`,
/// the `hl` on `PATH` -- and rebasing those under the preopened directory
/// names nothing. It is only a guest path when the rebased form exists, so
/// that is the test: rebase what resolves, and otherwise leave it for the
/// host to resolve as it would any other command.
fn rebase_command(cmd: &str) -> String {
    let rebased = rebase_guest_arg(cmd);
    if cmd.starts_with('/') && !Path::new(&rebased).exists() {
        return cmd.to_string();
    }
    rebased
}

/// The platform's shell and the argument that hands it one line, so a
/// `Process` built with no argument array runs the same way `Sys.command`
/// does.
fn shell_command(line: &str) -> (String, Vec<String>) {
    #[cfg(windows)]
    {
        let shell = std::env::var("COMSPEC").unwrap_or_else(|_| "cmd.exe".to_string());
        (shell, vec!["/C".to_string(), line.to_string()])
    }
    #[cfg(not(windows))]
    {
        (
            "/bin/sh".to_string(),
            vec!["-c".to_string(), line.to_string()],
        )
    }
}

/// Run a child with `input` as all of its input, and collect all of its
/// output. Nothing is streamed: the guest is stopped for the whole of it.
fn run_to_completion(cmd: &str, args: &[String], input: &[u8]) -> Option<Finished> {
    use std::io::Write;
    use std::process::{Command, Stdio};

    let mut child = Command::new(cmd)
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .ok()?;
    // Closing input is what tells a filter it may finish, so the write and
    // the drop have to happen before the wait or a child reading to end of
    // input never returns.
    if let Some(mut sink) = child.stdin.take() {
        let _ = sink.write_all(input);
    }
    let out = child.wait_with_output().ok()?;
    Some(Finished {
        stdout: out.stdout,
        stderr: out.stderr,
        taken: [0, 0],
        code: out.status.code().unwrap_or(-1),
    })
}

/// How the guest reaches a native library that was loaded beside it.
///
/// Two imports, and they are `dlopen` and `dlsym` under other names, because
/// that is what `crate::aot_native` on every other target calls at exactly
/// this point. The library is already loaded by the time either is asked --
/// see [`dylink`] -- so "open" is a lookup, and "sym" answers with a table
/// index, which is what a function pointer is in a wasm module.
///
/// Answering zero is not an error. It is the null the `DEFINE_PRIM` resolver
/// protocol already reads as "not in this library", and the call site raises
/// the same "not loaded" a native binary raises for a missing HDLL -- only if
/// the primitive is actually reached.
fn install_dlopen(linker: &mut Linker<Host>) -> Result<()> {
    linker
        .func_wrap(
            FIBER_YIELD_MODULE,
            "ash_host_dlopen",
            |mut caller: Caller<'_, Host>, name: i32, name_len: i32| -> i32 {
                let Some(name) = guest_slice(&mut caller, name, name_len) else {
                    return 0;
                };
                let name = String::from_utf8_lossy(&name).into_owned();
                caller.data().libraries.contains(&name) as i32
            },
        )
        .map_err(|e| anyhow!("installing the library import: {e}"))?;

    linker
        .func_wrap(
            FIBER_YIELD_MODULE,
            "ash_host_dlsym",
            |mut caller: Caller<'_, Host>, lib: i32, lib_len: i32, sym: i32, sym_len: i32| -> i32 {
                let Some(lib) = guest_slice(&mut caller, lib, lib_len) else {
                    return 0;
                };
                let Some(sym) = guest_slice(&mut caller, sym, sym_len) else {
                    return 0;
                };
                let lib = String::from_utf8_lossy(&lib).into_owned();
                let sym = String::from_utf8_lossy(&sym).into_owned();
                dylink::resolve(&mut caller, &lib, &sym).unwrap_or(0)
            },
        )
        .map_err(|e| anyhow!("installing the symbol import: {e}"))?;
    Ok(())
}

/// The transform's state global, if this module has one.
fn fiber_global(caller: &mut Caller<'_, Host>, name: &str) -> Option<wasmtime::Global> {
    match caller.get_export(name) {
        Some(wasmtime::Extern::Global(g)) => Some(g),
        _ => None,
    }
}

/// Supply the imports that have to reach outside the sandbox to suspend.
///
/// Two mechanisms, and the module says which it wants. A module the linker
/// instrumented exports the transform's state global: suspending it means
/// setting that global and letting the instrumented frames unwind themselves
/// back to the scheduler, and the engine is not involved. A module without it
/// has no way to unwind, so the engine has to do the suspending -- which
/// wasmtime's async support can, by parking the guest stack while a host
/// function awaits.
///
/// Both are here because they are not interchangeable. Engine suspension is
/// cheaper and needs no rewrite, and is what to use where the engine has it;
/// the transform is what makes a fiber work where it does not.
fn install_fiber_yield(linker: &mut Linker<Host>) -> Result<()> {
    linker
        .func_wrap_async(
            FIBER_YIELD_MODULE,
            FIBER_YIELD_NAME,
            |mut caller: Caller<'_, Host>, _params: ()| {
                // Setting the state is what makes every instrumented frame
                // between here and the scheduler return on its way out.
                if let Some(state) = fiber_global(&mut caller, STATE_GLOBAL) {
                    let now = state.get(&mut caller).i32().unwrap_or(0);
                    // On a rewind this is the call the fiber stopped at, so
                    // reaching it again means the rewind is over.
                    let next = if now == REWINDING { 0 } else { UNWINDING };
                    let _ = state.set(&mut caller, wasmtime::Val::I32(next));
                    return Box::new(async {}) as _;
                }
                Box::new(async {
                    tokio::task::yield_now().await;
                })
            },
        )
        .map_err(|e| anyhow!("installing the fiber yield import: {e}"))?;

    // What the transform's state global says. The guest cannot read it: the
    // global is added after the guest has been compiled, so there is no name
    // in the guest to refer to it by.
    linker
        .func_wrap(
            FIBER_YIELD_MODULE,
            "ash_host_fiber_state",
            |mut caller: Caller<'_, Host>| -> i32 {
                fiber_global(&mut caller, STATE_GLOBAL)
                    .and_then(|g| g.get(&mut caller).i32())
                    .unwrap_or(0)
            },
        )
        .map_err(|e| anyhow!("installing the fiber state import: {e}"))?;

    install_command(linker)?;
    install_process(linker)?;
    install_dlopen(linker)?;

    // Point the transform at a fiber's side stack and say whether the next
    // entry is a rewind. An uninstrumented module has neither global and
    // needs neither: this is then a no-op and its fibers run to completion.
    linker
        .func_wrap(
            FIBER_YIELD_MODULE,
            "ash_host_fiber_arm",
            |mut caller: Caller<'_, Host>, data: i32, rewind: i32, sp: i32| -> i32 {
                if let Some(g) = fiber_global(&mut caller, DATA_GLOBAL) {
                    let _ = g.set(&mut caller, wasmtime::Val::I32(data));
                }
                if let Some(g) = fiber_global(&mut caller, STATE_GLOBAL) {
                    let next = if rewind != 0 { REWINDING } else { 0 };
                    let _ = g.set(&mut caller, wasmtime::Val::I32(next));
                }
                // Swap in the fiber's own shadow stack and hand back whose it
                // was, so the caller can be put back exactly where it was.
                match fiber_global(&mut caller, SP_GLOBAL) {
                    Some(g) if sp != 0 => {
                        let was = g.get(&mut caller).i32().unwrap_or(0);
                        let _ = g.set(&mut caller, wasmtime::Val::I32(sp));
                        was
                    }
                    Some(g) => g.get(&mut caller).i32().unwrap_or(0),
                    None => 0,
                }
            },
        )
        .map_err(|e| anyhow!("installing the fiber arm import: {e}"))?;
    Ok(())
}
