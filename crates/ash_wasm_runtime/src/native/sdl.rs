//! A window and a frame, for a host with no screen.
//!
//! `sdl.wasm` forwards; something has to answer. In a page that is a canvas
//! and WebGL2. Here it is a recorder: every call is counted, every query gets
//! the answer a WebGL2 context would give, and nothing is drawn.
//!
//! That is not a placeholder for the browser host -- it is the only way to
//! check the other one. A page cannot be run in a test lane, but a Heaps
//! program driving these sixty-seven can, and what it asks for in what order
//! is the whole of what the browser host will have to satisfy. A recorded run
//! says the program reached its draw call; a page then has to make the same
//! calls mean pixels.
//!
//! # What a program can tell
//!
//! **It runs for a fixed number of frames.** After `ASH_SDL_FRAMES` swaps the
//! recorder prints what it saw and ends the process. Asking the program to
//! quit would be tidier, but a quit is an event and filling one in means
//! knowing the layout of a Heaps object; a headless program otherwise has no
//! reason to ever stop.
//!
//! **Every object name is invented here.** `gl_create_*` hands out
//! consecutive integers; nothing checks that a name later handed back was
//! ever issued, because a recorder has nothing to check it against.
//!
//! **Every query answers as WebGL2 would**, which is the point: Heaps asks a
//! handful of questions at startup and picks a code path from the answers, so
//! these few replies decide which of the other primitives it ever calls. They
//! are the browser's answers rather than a desktop GL's on purpose -- a trace
//! taken here is then the trace a page would produce.

use std::collections::BTreeMap;

/// One argument, as the guest passed it.
#[derive(Debug, Clone, Copy)]
pub(crate) enum Arg {
    I(i32),
    D(f64),
}

/// GL enum values, which are the same numbers everywhere and are what the
/// program is asking about.
mod gl {
    pub const VENDOR: i32 = 0x1F00;
    pub const RENDERER: i32 = 0x1F01;
    pub const VERSION: i32 = 0x1F02;
    pub const SHADING_LANGUAGE_VERSION: i32 = 0x8B8C;
    /// `COMPILE_STATUS` and `LINK_STATUS`, which a recorder always passes.
    pub const COMPILE_STATUS: i32 = 0x8B81;
    pub const LINK_STATUS: i32 = 0x8B82;
}

/// What a host with no screen answers.
#[derive(Default)]
pub(crate) struct Sdl {
    /// How many times each primitive was called, in name order.
    calls: BTreeMap<String, u32>,
    /// The order they were first called in, which is what a port has to
    /// satisfy and a count cannot show.
    order: Vec<String>,
    /// The next GL object name to hand out. Starts at 1: zero means "no
    /// object" to every GL that has ever existed.
    next_name: i32,
    frames: u32,
    limit: u32,
    trace: bool,
}

impl Sdl {
    pub(crate) fn new() -> Self {
        Self {
            next_name: 1,
            limit: std::env::var("ASH_SDL_FRAMES")
                .ok()
                .and_then(|v| v.parse().ok())
                .unwrap_or(3),
            trace: std::env::var("ASH_SDL_TRACE").is_ok(),
            ..Default::default()
        }
    }

    /// Every primitive arrives here. See `sdl_generated.rs`.
    pub(crate) fn call(&mut self, name: &str, args: &[Arg]) -> i64 {
        if !self.calls.contains_key(name) {
            self.order.push(name.to_string());
        }
        *self.calls.entry(name.to_string()).or_default() += 1;
        if self.trace {
            eprintln!("[sdl] {name}{args:?}");
        }
        self.answer(name, args)
    }

    fn answer(&mut self, qualified: &str, args: &[Arg]) -> i64 {
        // Calls arrive as `lib@name`, because two libraries may name the same
        // primitive and a recorder that merged them would be lying about
        // which was asked.
        let name = qualified.split_once('@').map_or(qualified, |(_, n)| n);
        let arg = |i: usize| match args.get(i) {
            Some(Arg::I(v)) => *v,
            Some(Arg::D(v)) => *v as i32,
            None => 0,
        };
        match name {
            // Starting up. `init_once` and `detect_win32` are asked before
            // there is a window; the second is false everywhere but Windows.
            "init_once" => 1,
            "detect_win32" => 0,
            // A GL that would not initialise sends Heaps down its failure
            // path, which destroys the window and puts up a dialog. It is
            // the first answer that has to be right.
            "gl_init" => 1,

            // A window and its context are opaque to the program, so any
            // non-null value is a window. One is as good as another and
            // there is only ever one.
            "win_create_ex" => WINDOW,
            "win_get_glcontext" => CONTEXT,
            "win_get_size" => 0,
            "win_set_fullscreen" => 1,

            // A frame ended. After enough of them the program is told to
            // quit, which is the only reason a headless run ever stops.
            "win_swap_window" => {
                self.frames += 1;
                if self.frames >= self.limit {
                    // Nothing here can ask the program to stop: the quit that
                    // would do it is an event, and filling one means knowing
                    // the layout of a Heaps object. A recorder has what it
                    // came for by now, so it says so and ends the process.
                    eprint!("{}", self.report());
                    std::process::exit(0);
                }
                0
            }
            // False means "no more events". hlsdl pumps this in a loop until
            // it says so, so answering true is an event queue that never
            // drains and a frame that never starts.
            "event_loop" => 0,

            // Names, invented. Nothing checks them.
            "gl_create_buffer"
            | "gl_create_program"
            | "gl_create_shader"
            | "gl_create_texture"
            | "gl_create_vertex_array"
            | "gl_create_framebuffer"
            | "gl_create_renderbuffer"
            | "gl_create_query" => {
                let name = self.next_name;
                self.next_name += 1;
                i64::from(name)
            }

            // Queries, answered as WebGL2 would. A shader that did not
            // compile or a program that did not link would send Heaps down
            // its error path and stop the run before anything was learned.
            "gl_get_shader_parameter" if arg(1) == gl::COMPILE_STATUS => 1,
            "gl_get_program_parameter" if arg(1) == gl::LINK_STATUS => 1,
            "gl_get_shader_parameter" | "gl_get_program_parameter" => 0,
            "gl_get_error" => 0,
            "gl_is_context_lost" => 0,
            "gl_has_extension" => 0,
            "gl_get_config_parameter" => 0,
            "gl_get_string" => 0,
            "gl_get_attrib_location" | "gl_get_uniform_location" => {
                let name = self.next_name;
                self.next_name += 1;
                i64::from(name)
            }

            _ => 0,
        }
    }

    /// Fill in one of GL's strings, and answer how many bytes that took.
    ///
    /// WebGL2's answers rather than a desktop GL's, so that a trace taken
    /// here is the trace a page would produce -- Heaps parses the version and
    /// picks a code path from it.
    pub(crate) fn gl_string(&self, name: i32) -> Option<&'static [u8]> {
        Some(match name {
            gl::VENDOR => b"ash\0".as_slice(),
            gl::RENDERER => b"ash headless recorder\0".as_slice(),
            gl::VERSION => b"OpenGL ES 3.0 (WebGL 2.0)\0".as_slice(),
            gl::SHADING_LANGUAGE_VERSION => b"OpenGL ES GLSL ES 3.00 (WebGL GLSL ES 3.00)\0".as_slice(),
            _ => return None,
        })
    }

    /// What the run asked for, in the order it first asked.
    pub(crate) fn report(&self) -> String {
        let mut out = format!(
            "[sdl] {} primitives over {} frames\n",
            self.calls.len(),
            self.frames
        );
        for name in &self.order {
            let count = self.calls.get(name).copied().unwrap_or(0);
            out += &format!("  {name} x{count}\n");
        }
        out
    }

    /// Whether a primitive was reached. What a test asserts on: a run that
    /// got as far as `gl_draw_elements` drew something.
    #[allow(dead_code)]
    pub(crate) fn reached(&self, name: &str) -> bool {
        self.calls.contains_key(name)
    }
}

/// The window and its context, as the program sees them: opaque and non-null.
/// Distinct values so that handing one back where the other belongs is
/// visible in a trace rather than silently fine.
const WINDOW: i64 = 0x5D_10_00_01;
const CONTEXT: i64 = 0x5D_10_00_02;
