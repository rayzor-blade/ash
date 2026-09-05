//! Rendering an uncaught-exception trace against the program's own source.
//!
//! HashLink reports an uncaught exception as a message and a list of
//! `Class.method(file:line)` lines. That names the path but shows none of it,
//! so acting on it means opening each file and counting to the line by hand.
//!
//! When the sources are reachable this renders the same trace as a diagnostic
//! over them: the throw site carried as the primary label, each caller
//! beneath it in order. When they are not -- a shipped `.hl` with no checkout
//! beside it, which is the common case -- nothing is invented and the caller
//! prints the flat list instead.

use std::path::{Path, PathBuf};
use std::sync::Arc;

use std::io::IsTerminal;

use ariadne::{sources, Color, Config, Label, Report, ReportKind};
use ash_interp::interpreter::stack::TraceFrame;

/// How many frames a report shows before it stops helping.
const MAX_LABELS: usize = 12;

/// Where a relative debug path may be rooted.
///
/// Haxe records the std library by absolute path and the program's own files
/// relative to the compile root, so both shapes turn up in one trace.
fn source_roots() -> Vec<PathBuf> {
    let mut roots = Vec::new();
    if let Ok(paths) = std::env::var("ASH_SOURCE_PATH") {
        roots.extend(std::env::split_paths(&paths));
    }
    if let Ok(cwd) = std::env::current_dir() {
        roots.push(cwd);
    }
    roots
}

fn resolve(file: &str, roots: &[PathBuf]) -> Option<PathBuf> {
    let direct = Path::new(file);
    if direct.is_absolute() && direct.is_file() {
        return Some(direct.to_path_buf());
    }
    roots
        .iter()
        .map(|root| root.join(direct))
        .find(|p| p.is_file())
}

/// Byte range of `line` (1-based) in `text`, trimmed of leading indentation
/// so the caret sits under the code rather than under the margin.
fn line_span(text: &str, line: i32) -> Option<std::ops::Range<usize>> {
    let wanted = usize::try_from(line).ok()?.checked_sub(1)?;
    let mut offset = 0usize;
    for (i, raw) in text.split_inclusive('\n').enumerate() {
        if i == wanted {
            let content = raw.trim_end_matches(['\n', '\r']);
            let indent = content.len() - content.trim_start().len();
            let start = offset + indent;
            let end = offset + content.len();
            return Some(if start < end {
                start..end
            } else {
                offset..offset + raw.len()
            });
        }
        offset += raw.len();
    }
    None
}

/// Render `frames` as a diagnostic. Returns false when no frame could be
/// resolved to a file on disk, which is the caller's cue to print the flat
/// trace instead.
pub fn render(message: &str, frames: &[Arc<TraceFrame>]) -> bool {
    render_inner(message, frames).unwrap_or(false)
}

fn render_inner(message: &str, frames: &[Arc<TraceFrame>]) -> Option<bool> {
    let colour = std::io::stderr().is_terminal();
    let roots = source_roots();
    let locate = |frame: &Arc<TraceFrame>| -> Option<(PathBuf, String)> {
        let path = resolve(frame.file.as_deref()?, &roots)?;
        let text = std::fs::read_to_string(&path).ok()?;
        line_span(&text, frame.line)?;
        Some((path, text))
    };

    // The INNERMOST frame decides. Haxe records its own standard library by
    // absolute path, so on a machine with Haxe installed an outer std frame
    // resolves even when none of the program's sources are reachable -- and a
    // report anchored there points confidently at a line that has nothing to
    // do with the failure. Better the flat list than a confident wrong answer.
    let head = frames.first()?;
    let (head_path, head_text) = locate(head)?;

    let mut located: Vec<(&TraceFrame, PathBuf, String)> =
        vec![(head, head_path.clone(), head_text.clone())];
    for frame in frames.iter().take(MAX_LABELS).skip(1) {
        if let Some((path, text)) = locate(frame) {
            located.push((frame, path, text));
        }
    }

    let id = |path: &Path| path.display().to_string();
    let head_span = line_span(&head_text, head.line)?;

    let mut report = Report::build(
        ReportKind::Custom("Uncaught exception", Color::Red),
        (id(&head_path), head_span.clone()),
    )
    // Colour is for a terminal. Piped into a file or a CI log it is noise
    // that hides the text it decorates.
    .with_config(Config::default().with_color(colour))
    .with_message(message)
    .with_label(
        Label::new((id(&head_path), head_span))
            .with_message(format!("thrown in {}", head.symbol))
            .with_color(Color::Red)
            .with_order(0),
    );

    // Callers, outward. `with_order` keeps them in call order rather than in
    // whatever order the spans happen to sort into.
    for (i, (frame, path, text)) in located.iter().enumerate().skip(1) {
        let Some(span) = line_span(text, frame.line) else {
            continue;
        };
        report = report.with_label(
            Label::new((id(path), span))
                .with_message(format!("called from {}", frame.symbol))
                .with_color(Color::Yellow)
                .with_order(i as i32),
        );
    }

    // Frames that could not be shown are still named, so the trace never
    // silently loses its tail.
    let unshown: Vec<String> = frames
        .iter()
        .skip(located.len())
        .map(|frame| frame.to_string())
        .collect();
    if !unshown.is_empty() {
        report = report.with_note(format!("also called from: {}", unshown.join(", ")));
    }

    let cache = sources(
        located
            .iter()
            .map(|(_, path, text)| (id(path), text.clone()))
            .collect::<Vec<_>>(),
    );
    let mut buf: Vec<u8> = Vec::new();
    report.finish().write(cache, &mut buf).ok()?;
    let text = String::from_utf8_lossy(&buf).into_owned();
    // `Config::with_color(false)` covers the body but not the header:
    // `ReportKind::Custom` carries its own colour and applies it regardless
    // of the config. Strip on the way out instead, which also holds if
    // another corner of the renderer picks up the same habit.
    let out = if colour { text } else { strip_ansi(&text) };
    eprint!("{out}");
    Some(true)
}

/// Remove CSI sequences. Enough for what a renderer emits: colour and style,
/// never cursor movement.
fn strip_ansi(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut chars = text.chars();
    while let Some(c) = chars.next() {
        if c != '\u{1b}' {
            out.push(c);
            continue;
        }
        if chars.next() != Some('[') {
            continue;
        }
        for tail in chars.by_ref() {
            if tail.is_ascii_alphabetic() {
                break;
            }
        }
    }
    out
}
