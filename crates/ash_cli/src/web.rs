//! `ash run`, `ash serve`, and the page `--build` writes beside a wasm module.

use std::io::{BufRead, BufReader, Write};
use std::net::{TcpListener, TcpStream};
use std::path::{Component, Path, PathBuf};

use anyhow::{Context, Result, bail};

/// Run a wasm module under wasmtime and return its exit status.
///
/// Threads are always allowed: the host only starts one for a module built
/// for `wasm32-wasip1-threads`, so a single-threaded module is unaffected.
pub fn run_module(module: &Path, dirs: &[PathBuf], args: &[String]) -> Result<i32> {
    use ash_wasm_runtime::native::{Outcome, Program};

    let program = Program::load(module)?;
    let mut argv = vec![guest_visible_path(module)];
    argv.extend(args.iter().cloned());
    let runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .context("starting the wasm host")?;
    match runtime.block_on(program.run(&argv, dirs, true))? {
        Outcome::Exited(code) => Ok(code),
        Outcome::Trapped(trap) => {
            eprintln!("{trap}");
            Ok(70)
        }
    }
}

/// The module's path as the program sees it: relative to the working
/// directory the host preopens, or the bare file name when it lies outside.
fn guest_visible_path(module: &Path) -> String {
    let bare = || {
        module
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| "program".to_string())
    };
    let (Ok(cwd), Ok(full)) = (std::env::current_dir(), module.canonicalize()) else {
        return bare();
    };
    match full.strip_prefix(&cwd) {
        Ok(rest) => format!("/{}", rest.to_string_lossy()),
        Err(_) => bare(),
    }
}

/// Serve `dir` on localhost with the headers a threaded module needs.
///
/// A shared memory is a `SharedArrayBuffer`, which a page has only when every
/// response carries COOP and COEP. `no-store` so a rebuilt module is never
/// served stale.
pub fn serve(dir: &Path, port: u16) -> Result<()> {
    let root = dir
        .canonicalize()
        .with_context(|| format!("{} is not a directory", dir.display()))?;
    if !root.is_dir() {
        bail!("{} is not a directory", dir.display());
    }
    let listener = TcpListener::bind(("127.0.0.1", port))
        .with_context(|| format!("listening on port {port}; pass --port to pick another"))?;
    println!("serving {} at http://127.0.0.1:{port}/", root.display());
    for stream in listener.incoming() {
        let Ok(stream) = stream else { continue };
        let root = root.clone();
        std::thread::spawn(move || {
            let _ = respond(stream, &root);
        });
    }
    Ok(())
}

fn respond(mut stream: TcpStream, root: &Path) -> std::io::Result<()> {
    let mut reader = BufReader::new(stream.try_clone()?);
    let mut request = String::new();
    reader.read_line(&mut request)?;
    // Headers are read and ignored; the request line is all this needs.
    let mut line = String::new();
    while reader.read_line(&mut line)? > 2 {
        line.clear();
    }
    let mut parts = request.split_whitespace();
    let (method, target) = (parts.next().unwrap_or(""), parts.next().unwrap_or("/"));
    if method != "GET" && method != "HEAD" {
        return send(
            &mut stream,
            "405 Method Not Allowed",
            "text/plain",
            b"",
            true,
        );
    }
    let Some(path) = resolve(root, target) else {
        return send(
            &mut stream,
            "404 Not Found",
            "text/plain",
            b"not found\n",
            method == "GET",
        );
    };
    match std::fs::read(&path) {
        Ok(body) => send(
            &mut stream,
            "200 OK",
            content_type(&path),
            &body,
            method == "GET",
        ),
        Err(_) => send(
            &mut stream,
            "404 Not Found",
            "text/plain",
            b"not found\n",
            method == "GET",
        ),
    }
}

/// The file a request names, or `None` for one outside `root`.
fn resolve(root: &Path, target: &str) -> Option<PathBuf> {
    let path = target.split(['?', '#']).next().unwrap_or("/");
    let decoded = percent_decode(path)?;
    let mut out = root.to_path_buf();
    for component in Path::new(decoded.trim_start_matches('/')).components() {
        match component {
            Component::Normal(part) => out.push(part),
            Component::CurDir => {}
            _ => return None,
        }
    }
    if out.is_dir() {
        out.push("index.html");
    }
    out.is_file().then_some(out)
}

fn percent_decode(s: &str) -> Option<String> {
    let bytes = s.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' {
            let hex = std::str::from_utf8(bytes.get(i + 1..i + 3)?).ok()?;
            out.push(u8::from_str_radix(hex, 16).ok()?);
            i += 3;
        } else {
            out.push(bytes[i]);
            i += 1;
        }
    }
    String::from_utf8(out).ok()
}

fn content_type(path: &Path) -> &'static str {
    match path.extension().and_then(|e| e.to_str()).unwrap_or("") {
        "html" | "htm" => "text/html; charset=utf-8",
        "js" | "mjs" => "text/javascript; charset=utf-8",
        "wasm" => "application/wasm",
        "css" => "text/css; charset=utf-8",
        "json" => "application/json",
        "png" => "image/png",
        "jpg" | "jpeg" => "image/jpeg",
        "svg" => "image/svg+xml",
        "txt" | "md" => "text/plain; charset=utf-8",
        _ => "application/octet-stream",
    }
}

fn send(
    stream: &mut TcpStream,
    status: &str,
    content_type: &str,
    body: &[u8],
    with_body: bool,
) -> std::io::Result<()> {
    write!(
        stream,
        "HTTP/1.1 {status}\r\n\
         Content-Type: {content_type}\r\n\
         Content-Length: {}\r\n\
         Cross-Origin-Opener-Policy: same-origin\r\n\
         Cross-Origin-Embedder-Policy: require-corp\r\n\
         Cache-Control: no-store\r\n\
         Connection: close\r\n\r\n",
        body.len()
    )?;
    if with_body {
        stream.write_all(body)?;
    }
    stream.flush()
}

/// Write the page that runs `module` in a browser beside it, and say so.
pub fn write_page(module: &Path, quiet: bool) -> Result<()> {
    let written = ash_page::write_page(module)
        .with_context(|| format!("writing the page beside {}", module.display()))?;
    if !quiet {
        let dir = module
            .parent()
            .filter(|d| !d.as_os_str().is_empty())
            .unwrap_or(Path::new("."));
        eprintln!(
            "[ash] wrote a page for it beside {}{}; `ash serve {}` opens it",
            module
                .file_name()
                .map(|n| n.to_string_lossy())
                .unwrap_or_default(),
            if written.index_kept {
                " (your index.html kept)"
            } else {
                ""
            },
            dir.display()
        );
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{percent_decode, resolve};

    #[test]
    fn a_request_resolves_inside_the_served_directory_only() {
        let root = std::env::temp_dir().join(format!("ash-serve-{}", std::process::id()));
        std::fs::create_dir_all(root.join("sub dir")).unwrap();
        std::fs::write(root.join("index.html"), "i").unwrap();
        std::fs::write(root.join("sub dir/a.wasm"), "a").unwrap();

        assert_eq!(resolve(&root, "/"), Some(root.join("index.html")));
        assert_eq!(resolve(&root, "/?demo=x"), Some(root.join("index.html")));
        assert_eq!(
            resolve(&root, "/sub%20dir/a.wasm"),
            Some(root.join("sub dir/a.wasm"))
        );
        assert_eq!(resolve(&root, "/../etc/passwd"), None);
        assert_eq!(resolve(&root, "/sub%20dir/../../x"), None);
        assert_eq!(resolve(&root, "/missing.js"), None);

        std::fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn percent_decoding_refuses_a_truncated_escape() {
        assert_eq!(percent_decode("/a%20b").as_deref(), Some("/a b"));
        assert_eq!(percent_decode("/a%2"), None);
    }
}
