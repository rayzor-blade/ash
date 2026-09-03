//! A one-line progress report, for work that takes long enough to wonder
//! about.
//!
//! Compiling a game ahead of time is a minute of silence, and silence during a
//! minute is indistinguishable from a hang. This draws a single line on
//! stderr, rewritten in place, saying which stage is running and how far in it
//! is.
//!
//! It is off unless stderr is a terminal. A pipe gets the ordinary log lines,
//! unchanged and in order, because that is what a script or a CI job reads;
//! escape codes in a log file help nobody. `ASH_AOT_LOG=1` also forces the log
//! lines, for anyone who wants the detail on a terminal.
//!
//! Two kinds of message, and the difference matters once a bar is on screen:
//! [`note`] is for something the reader must see, so it clears the bar, prints
//! and redraws; [`detail`] is diagnostics, which a bar replaces rather than
//! competes with.

use std::io::{IsTerminal, Write};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Mutex;
use std::time::{Duration, Instant};

static ENABLED: AtomicBool = AtomicBool::new(false);
static VERBOSE: AtomicBool = AtomicBool::new(false);
static TOTAL: AtomicU64 = AtomicU64::new(0);
static DONE: AtomicU64 = AtomicU64::new(0);
static DRAWN: AtomicBool = AtomicBool::new(false);
/// Whether a stage is running. A note printed after the last stage finished
/// must not resurrect its bar, which would leave a finished stage on screen
/// under the summary.
static ACTIVE: AtomicBool = AtomicBool::new(false);

struct State {
    stage: String,
    started: Instant,
    last_draw: Instant,
}

fn state() -> &'static Mutex<State> {
    static STATE: std::sync::OnceLock<Mutex<State>> = std::sync::OnceLock::new();
    STATE.get_or_init(|| {
        Mutex::new(State {
            stage: String::new(),
            started: Instant::now(),
            last_draw: Instant::now() - Duration::from_secs(1),
        })
    })
}

/// Turn the bar on if this terminal and this run want one.
///
/// `wanted` is the caller's own decision -- a quiet run does not get a bar
/// however friendly the terminal is.
pub fn enable(wanted: bool) {
    let verbose = std::env::var_os("ASH_AOT_LOG").is_some();
    VERBOSE.store(verbose, Ordering::Release);
    ENABLED.store(
        wanted && !verbose && std::io::stderr().is_terminal(),
        Ordering::Release,
    );
}

pub fn enabled() -> bool {
    ENABLED.load(Ordering::Acquire)
}

/// Start a stage. `total` of zero means the end is unknown, and the line
/// reports elapsed time only.
pub fn begin(stage: &str, total: u64) {
    if !enabled() {
        return;
    }
    TOTAL.store(total, Ordering::Release);
    DONE.store(0, Ordering::Release);
    ACTIVE.store(true, Ordering::Release);
    if let Ok(mut s) = state().lock() {
        s.stage = stage.to_string();
        s.started = Instant::now();
        s.last_draw = Instant::now() - Duration::from_secs(1);
    }
    draw(true);
}

/// Report `n` more units of the current stage done.
pub fn advance(n: u64) {
    if !enabled() || !ACTIVE.load(Ordering::Acquire) {
        return;
    }
    DONE.fetch_add(n, Ordering::AcqRel);
    draw(false);
}

/// End the current stage, leaving the line clear.
pub fn finish() {
    ACTIVE.store(false, Ordering::Release);
    if !enabled() {
        return;
    }
    clear();
    TOTAL.store(0, Ordering::Release);
    DONE.store(0, Ordering::Release);
}

/// Something the reader must see. Printed either way; with a bar on screen it
/// takes the line above it.
pub fn note(message: &str) {
    if enabled() {
        clear();
        eprintln!("{message}");
        draw(true);
    } else {
        eprintln!("{message}");
    }
}

/// Diagnostics. The bar stands in for these; a pipe or `ASH_AOT_LOG=1` gets
/// them in full.
pub fn detail(message: &str) {
    if !enabled() {
        eprintln!("{message}");
    }
}

/// Whether the caller should bother formatting a [`detail`] at all, for a
/// message that costs something to build.
pub fn detail_wanted() -> bool {
    !enabled()
}

fn clear() {
    if DRAWN.swap(false, Ordering::AcqRel) {
        let mut err = std::io::stderr().lock();
        // Return to the start of the line and erase to its end. Two escapes
        // rather than a row of spaces, which would wrap on a narrow window.
        let _ = write!(err, "\r\x1b[2K");
        let _ = err.flush();
    }
}

fn draw(force: bool) {
    if !enabled() || !ACTIVE.load(Ordering::Acquire) {
        return;
    }
    let Ok(mut s) = state().lock() else { return };
    // Redrawing on every function of nine thousand would cost more than the
    // work it reports.
    if !force && s.last_draw.elapsed() < Duration::from_millis(80) {
        return;
    }
    s.last_draw = Instant::now();
    let done = DONE.load(Ordering::Acquire);
    let total = TOTAL.load(Ordering::Acquire);
    let secs = s.started.elapsed().as_secs();
    let line = if total > 0 {
        let width = 24usize;
        let filled = ((done.min(total) as f64 / total as f64) * width as f64).round() as usize;
        let bar: String = "█".repeat(filled) + &"·".repeat(width.saturating_sub(filled));
        format!(
            "[ash] {:<10} {bar} {done}/{total}  {secs}s",
            s.stage,
            done = done,
            total = total,
            secs = secs
        )
    } else {
        format!("[ash] {:<10} {secs}s", s.stage, secs = secs)
    };
    let mut err = std::io::stderr().lock();
    let _ = write!(err, "\r\x1b[2K{line}");
    let _ = err.flush();
    DRAWN.store(true, Ordering::Release);
}
