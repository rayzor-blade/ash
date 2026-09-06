//! Running something else: `Sys.command` and `sys.io.Process`.
//!
//! A wasm module cannot start a process -- WASI preview 1 has no interface
//! for one -- so the guest asks the host through two imports of its own, and
//! the host runs the command with the operating system's own facilities.
//!
//! The guest's idea of where things are is not the host's, which is why so
//! much of this is rebasing: a preopened directory is the guest's `/`, and a
//! path it passes is relative to that. See [`rebase_command`] for the one
//! case where rebasing is wrong.

use std::path::Path;

use anyhow::{anyhow, Result};
use wasmtime::{Caller, Linker};

use super::fibers::YIELD_MODULE;
use super::{guest_memory, guest_slice, Host};

/// Install both: `Sys.command`, and the five that make a `sys.io.Process`.
pub(crate) fn install(linker: &mut Linker<Host>) -> Result<()> {
    install_command(linker)?;
    install_process(linker)
}

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
            YIELD_MODULE,
            "ash_host_command",
            move |mut caller: Caller<'_, Host>, ptr: i32, len: i32| -> i32 {
                if !allowed {
                    return -1;
                }
                let Some((data, _)) = guest_memory(&mut caller) else {
                    return -1;
                };
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
            YIELD_MODULE,
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
            YIELD_MODULE,
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
            YIELD_MODULE,
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
            YIELD_MODULE,
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
                let Some((data, _)) = guest_memory(&mut caller) else {
                    return -1;
                };
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
            YIELD_MODULE,
            "ash_host_process_code",
            |mut caller: Caller<'_, Host>, handle: i32| -> i32 {
                process_of(&mut caller, handle).map_or(-1, |d| d.code)
            },
        )
        .map_err(|e| anyhow!("installing the process code import: {e}"))?;

    linker
        .func_wrap(
            YIELD_MODULE,
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
