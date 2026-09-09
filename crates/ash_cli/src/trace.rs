//! The CLI's side of the trace renderer.
//!
//! The renderer itself is [`ash_trace`], shared with the runtime so an
//! ahead-of-time binary -- which links the runtime and not this crate --
//! reports the same way. Interpreter frames arrive as `TraceFrame`, so all
//! that is left here is handing them over borrowed.

use std::sync::Arc;

use ash_interp::interpreter::stack::TraceFrame;

/// Render `frames` as a diagnostic. Returns false when no frame could be
/// resolved to a file on disk, which is the caller's cue to print the flat
/// trace instead.
pub fn render(message: &str, frames: &[Arc<TraceFrame>]) -> bool {
    let borrowed: Vec<ash_trace::Frame<'_>> = frames
        .iter()
        .map(|frame| ash_trace::Frame {
            symbol: &frame.symbol,
            file: frame.file.as_deref(),
            line: frame.line,
        })
        .collect();
    ash_trace::render(message, &borrowed)
}
