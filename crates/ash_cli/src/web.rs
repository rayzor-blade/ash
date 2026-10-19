//! `ash run`, `ash serve`, and the page `--build` writes beside a wasm module.

use std::io::{BufRead, BufReader, Read, Seek, SeekFrom, Write};
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
    // The request line and the Range header are all this reads.
    let mut range_header = None;
    let mut brotli_accepted = false;
    let mut line = String::new();
    while reader.read_line(&mut line)? > 2 {
        if let Some((name, value)) = line.split_once(':') {
            if name.eq_ignore_ascii_case("range") {
                range_header = Some(value.trim().to_string());
            } else if name.eq_ignore_ascii_case("accept-encoding") {
                brotli_accepted = accepts_brotli(value);
            }
        }
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
    let Ok(mut file) = std::fs::File::open(&path) else {
        return send(
            &mut stream,
            "404 Not Found",
            "text/plain",
            b"not found\n",
            method == "GET",
        );
    };
    let Ok(total) = file.metadata().map(|m| m.len()) else {
        return send(
            &mut stream,
            "404 Not Found",
            "text/plain",
            b"not found\n",
            method == "GET",
        );
    };
    let with_body = method == "GET";
    let kind = content_type(&path);
    match range_header
        .as_deref()
        .map_or(RangeRequest::Whole, |h| parse_range(h, total))
    {
        RangeRequest::Whole => {
            let mut body = Vec::with_capacity(total as usize);
            file.read_to_end(&mut body)?;
            if brotli_accepted && compressible(&path) && body.len() > 1024 {
                let packed = compressed(&path, &body, total);
                return send_with(
                    &mut stream,
                    "200 OK",
                    kind,
                    &packed,
                    with_body,
                    "Content-Encoding: br\r\nVary: Accept-Encoding\r\n",
                );
            }
            send_with(&mut stream, "200 OK", kind, &body, with_body, "")
        }
        RangeRequest::Part(first, last) => {
            // Only the bytes asked for are read, so a seek into a large
            // media file does not load the rest of it.
            let mut body = vec![0u8; (last - first + 1) as usize];
            file.seek(SeekFrom::Start(first))?;
            file.read_exact(&mut body)?;
            let extra = format!("Content-Range: bytes {first}-{last}/{total}\r\n");
            send_with(
                &mut stream,
                "206 Partial Content",
                kind,
                &body,
                with_body,
                &extra,
            )
        }
        RangeRequest::Unsatisfiable => send_with(
            &mut stream,
            "416 Range Not Satisfiable",
            "text/plain",
            b"",
            false,
            &format!("Content-Range: bytes */{total}\r\n"),
        ),
    }
}

/// Whether an `Accept-Encoding` value lists brotli, and not as `br;q=0`.
fn accepts_brotli(value: &str) -> bool {
    value.split(',').any(|item| {
        let mut parts = item.split(';');
        parts
            .next()
            .is_some_and(|c| c.trim().eq_ignore_ascii_case("br"))
            && !parts.any(|p| p.trim().replace(' ', "") == "q=0")
    })
}

/// Files worth compressing: text and wasm. Images and audio already are.
fn compressible(path: &Path) -> bool {
    matches!(
        path.extension().and_then(|e| e.to_str()),
        Some("wasm" | "js" | "mjs" | "html" | "css" | "json" | "svg" | "wgsl" | "hl" | "txt")
    )
}

/// A file's brotli form, made once per version of it: the page is fetched
/// again on every reload, and compressing megabytes costs seconds.
fn compressed(path: &Path, body: &[u8], total: u64) -> std::sync::Arc<Vec<u8>> {
    use std::collections::HashMap;
    use std::sync::{Arc, Mutex, OnceLock};
    // Keyed by path, length and modification time.
    type Packed = Mutex<HashMap<(PathBuf, u64, u64), Arc<Vec<u8>>>>;
    static CACHE: OnceLock<Packed> = OnceLock::new();
    let modified = std::fs::metadata(path)
        .and_then(|m| m.modified())
        .ok()
        .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
        .map_or(0, |d| d.as_nanos() as u64);
    let key = (path.to_path_buf(), total, modified);
    let cache = CACHE.get_or_init(Default::default);
    if let Some(hit) = cache.lock().expect("compression cache poisoned").get(&key) {
        return hit.clone();
    }
    let mut out = Vec::new();
    let params = brotli::enc::BrotliEncoderParams {
        quality: 5,
        ..Default::default()
    };
    let mut input = body;
    brotli::BrotliCompress(&mut input, &mut out, &params).expect("compressing into memory");
    let out = Arc::new(out);
    cache
        .lock()
        .expect("compression cache poisoned")
        .insert(key, out.clone());
    out
}

/// What a request's `Range` header asks of a file of a given length.
#[derive(Debug, PartialEq, Eq)]
enum RangeRequest {
    /// No range, or one this does not honour: the whole file.
    Whole,
    /// Bytes `first..=last`, both inside the file.
    Part(u64, u64),
    /// A range that starts at or past the end of the file.
    Unsatisfiable,
}

/// Read one `bytes=` range. A list of ranges, another unit and a range that
/// is malformed are all answered with the whole file, which RFC 9110 allows;
/// a media element asks for one range at a time.
fn parse_range(header: &str, total: u64) -> RangeRequest {
    let Some(spec) = header.trim().strip_prefix("bytes=") else {
        return RangeRequest::Whole;
    };
    if spec.contains(',') {
        return RangeRequest::Whole;
    }
    let Some((first, last)) = spec.trim().split_once('-') else {
        return RangeRequest::Whole;
    };
    let (first, last) = (first.trim(), last.trim());
    if first.is_empty() {
        // The last `n` bytes.
        return match last.parse::<u64>() {
            Ok(n) if n > 0 && total > 0 => RangeRequest::Part(total.saturating_sub(n), total - 1),
            Ok(_) => RangeRequest::Unsatisfiable,
            Err(_) => RangeRequest::Whole,
        };
    }
    let Ok(first) = first.parse::<u64>() else {
        return RangeRequest::Whole;
    };
    let last = if last.is_empty() {
        None
    } else {
        match last.parse::<u64>() {
            Ok(n) => Some(n),
            Err(_) => return RangeRequest::Whole,
        }
    };
    if last.is_some_and(|last| last < first) {
        return RangeRequest::Whole;
    }
    if first >= total {
        return RangeRequest::Unsatisfiable;
    }
    RangeRequest::Part(first, last.map_or(total - 1, |last| last.min(total - 1)))
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
    let extension = path.extension().and_then(|e| e.to_str()).unwrap_or("");
    match extension.to_ascii_lowercase().as_str() {
        "html" | "htm" => "text/html; charset=utf-8",
        "js" | "mjs" => "text/javascript; charset=utf-8",
        "wasm" => "application/wasm",
        "css" => "text/css; charset=utf-8",
        "json" => "application/json",
        "png" => "image/png",
        "jpg" | "jpeg" => "image/jpeg",
        "gif" => "image/gif",
        "webp" => "image/webp",
        "avif" => "image/avif",
        "ico" => "image/x-icon",
        "svg" => "image/svg+xml",
        "txt" | "md" => "text/plain; charset=utf-8",
        "m4a" => "audio/mp4",
        "mp3" => "audio/mpeg",
        "ogg" | "oga" | "opus" => "audio/ogg",
        "wav" => "audio/wav",
        "flac" => "audio/flac",
        "mp4" | "m4v" => "video/mp4",
        "webm" => "video/webm",
        "ogv" => "video/ogg",
        "woff" => "font/woff",
        "woff2" => "font/woff2",
        "ttf" => "font/ttf",
        "otf" => "font/otf",
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
    send_with(stream, status, content_type, body, with_body, "")
}

/// [`send`] with more header lines, each ending in CRLF. Every response says
/// it accepts byte ranges, so a media element knows it may seek.
fn send_with(
    stream: &mut TcpStream,
    status: &str,
    content_type: &str,
    body: &[u8],
    with_body: bool,
    extra_headers: &str,
) -> std::io::Result<()> {
    write!(
        stream,
        "HTTP/1.1 {status}\r\n\
         Content-Type: {content_type}\r\n\
         Content-Length: {}\r\n\
         Accept-Ranges: bytes\r\n\
         {extra_headers}\
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
    use super::{RangeRequest, accepts_brotli, content_type, parse_range, percent_decode, resolve};
    use std::path::Path;

    #[test]
    fn a_single_byte_range_is_read_against_the_file_length() {
        use RangeRequest::{Part, Unsatisfiable, Whole};
        assert_eq!(parse_range("bytes=1000-1999", 5000), Part(1000, 1999));
        assert_eq!(parse_range("bytes=1000-", 5000), Part(1000, 4999));
        assert_eq!(parse_range("bytes=4500-9999", 5000), Part(4500, 4999));
        assert_eq!(parse_range("bytes=-500", 5000), Part(4500, 4999));
        assert_eq!(parse_range("bytes=-9999", 5000), Part(0, 4999));
        assert_eq!(parse_range("bytes=0-0", 5000), Part(0, 0));
        assert_eq!(parse_range("bytes=5000-", 5000), Unsatisfiable);
        assert_eq!(parse_range("bytes=-0", 5000), Unsatisfiable);
        assert_eq!(parse_range("bytes=0-", 0), Unsatisfiable);
        assert_eq!(parse_range("bytes=-5", 0), Unsatisfiable);
        // Not honoured: the whole file is a valid answer to each.
        assert_eq!(parse_range("bytes=0-1,5-9", 5000), Whole);
        assert_eq!(parse_range("items=0-1", 5000), Whole);
        assert_eq!(parse_range("bytes=9-3", 5000), Whole);
        assert_eq!(parse_range("bytes=a-b", 5000), Whole);
        assert_eq!(parse_range("bytes=", 5000), Whole);
    }

    /// One request to `respond` over a real socket; the raw response.
    fn ask(root: &Path, request: &str) -> Vec<u8> {
        use std::io::{Read, Write};
        let listener = std::net::TcpListener::bind(("127.0.0.1", 0)).unwrap();
        let addr = listener.local_addr().unwrap();
        let root = root.to_path_buf();
        let server = std::thread::spawn(move || {
            let (stream, _) = listener.accept().unwrap();
            super::respond(stream, &root).unwrap();
        });
        let mut client = std::net::TcpStream::connect(addr).unwrap();
        client.write_all(request.as_bytes()).unwrap();
        let mut response = Vec::new();
        client.read_to_end(&mut response).unwrap();
        server.join().unwrap();
        response
    }

    fn split(response: &[u8]) -> (String, &[u8]) {
        let at = response
            .windows(4)
            .position(|w| w == b"\r\n\r\n")
            .expect("a header block")
            + 4;
        (
            String::from_utf8_lossy(&response[..at]).into_owned(),
            &response[at..],
        )
    }

    #[test]
    fn a_module_is_sent_brotli_compressed_to_a_browser_that_accepts_it() {
        let root = std::env::temp_dir().join(format!("ash-brotli-{}", std::process::id()));
        std::fs::create_dir_all(&root).unwrap();
        let data: Vec<u8> = b"asm".iter().cycle().take(50_000).copied().collect();
        std::fs::write(root.join("app.wasm"), &data).unwrap();
        std::fs::write(root.join("song.m4a"), &data).unwrap();

        let packed = ask(
            &root,
            "GET /app.wasm HTTP/1.1\r\nAccept-Encoding: gzip, br\r\n\r\n",
        );
        let (head, body) = split(&packed);
        assert!(head.contains("Content-Encoding: br"), "{head}");
        assert!(head.contains("Vary: Accept-Encoding"), "{head}");
        assert!(body.len() < data.len() / 10, "{} bytes", body.len());
        let mut unpacked = Vec::new();
        brotli::BrotliDecompress(&mut &body[..], &mut unpacked).unwrap();
        assert_eq!(unpacked, data);

        for request in [
            "GET /app.wasm HTTP/1.1\r\n\r\n",
            "GET /app.wasm HTTP/1.1\r\nAccept-Encoding: br;q=0\r\n\r\n",
            "GET /song.m4a HTTP/1.1\r\nAccept-Encoding: br\r\n\r\n",
        ] {
            let plain = ask(&root, request);
            let (head, body) = split(&plain);
            assert!(!head.contains("Content-Encoding"), "{head}");
            assert_eq!(body, &data[..]);
        }
        assert!(accepts_brotli("gzip, deflate, br, zstd"));
        assert!(!accepts_brotli("gzip"));
    }

    #[test]
    fn a_range_request_gets_the_bytes_asked_for() {
        let root = std::env::temp_dir().join(format!("ash-range-{}", std::process::id()));
        std::fs::create_dir_all(&root).unwrap();
        let data: Vec<u8> = (0..100u8).collect();
        std::fs::write(root.join("night-drive.m4a"), &data).unwrap();

        let whole = ask(&root, "GET /night-drive.m4a HTTP/1.1\r\n\r\n");
        let (head, body) = split(&whole);
        assert!(head.starts_with("HTTP/1.1 200 OK"), "{head}");
        assert!(head.contains("Accept-Ranges: bytes"), "{head}");
        assert!(head.contains("Content-Type: audio/mp4"), "{head}");
        assert_eq!(body, &data[..]);

        let part = ask(
            &root,
            "GET /night-drive.m4a HTTP/1.1\r\nRange: bytes=10-19\r\n\r\n",
        );
        let (head, body) = split(&part);
        assert!(head.starts_with("HTTP/1.1 206 Partial Content"), "{head}");
        assert!(head.contains("Content-Range: bytes 10-19/100"), "{head}");
        assert!(head.contains("Content-Length: 10"), "{head}");
        assert_eq!(body, &data[10..20]);

        let head_only = ask(
            &root,
            "HEAD /night-drive.m4a HTTP/1.1\r\nrange: bytes=90-\r\n\r\n",
        );
        let (head, body) = split(&head_only);
        assert!(head.starts_with("HTTP/1.1 206"), "{head}");
        assert!(head.contains("Content-Length: 10"), "{head}");
        assert!(body.is_empty());

        let past = ask(
            &root,
            "GET /night-drive.m4a HTTP/1.1\r\nRange: bytes=100-\r\n\r\n",
        );
        let (head, body) = split(&past);
        assert!(head.starts_with("HTTP/1.1 416"), "{head}");
        assert!(head.contains("Content-Range: bytes */100"), "{head}");
        assert!(body.is_empty());

        std::fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn media_files_get_their_media_types() {
        let kind = |name| content_type(Path::new(name));
        assert_eq!(kind("a/night-drive.m4a"), "audio/mp4");
        assert_eq!(kind("a.MP3"), "audio/mpeg");
        assert_eq!(kind("a.ogg"), "audio/ogg");
        assert_eq!(kind("a.mp4"), "video/mp4");
        assert_eq!(kind("a.webm"), "video/webm");
        assert_eq!(kind("a.unknown"), "application/octet-stream");
    }

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
