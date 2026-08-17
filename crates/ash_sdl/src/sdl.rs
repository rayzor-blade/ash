//! SDL core functions: init, window, events.

use crate::resolver::define_prim;
use crate::types::*;
use std::ffi::{c_char, c_int, c_void, CStr};
use std::ptr;

// ============================================================================
// Raw SDL2 FFI bindings (linked via build.rs)
// ============================================================================

#[allow(non_snake_case)]
extern "C" {
    fn SDL_Init(flags: u32) -> c_int;
    fn SDL_Quit();
    fn SDL_SetHint(name: *const c_char, value: *const c_char) -> i32;
    fn SDL_GetError() -> *const c_char;
    fn SDL_PollEvent(event: *mut SDL_Event) -> c_int;
    fn SDL_CreateWindow(
        title: *const c_char,
        x: c_int,
        y: c_int,
        w: c_int,
        h: c_int,
        flags: u32,
    ) -> *mut c_void;
    fn SDL_DestroyWindow(window: *mut c_void);
    fn SDL_SetWindowTitle(window: *mut c_void, title: *const c_char);
    fn SDL_GetWindowSize(window: *mut c_void, w: *mut c_int, h: *mut c_int);
    fn SDL_GetWindowDisplayIndex(window: *mut c_void) -> c_int;
    fn SDL_GL_CreateContext(window: *mut c_void) -> *mut c_void;
    fn SDL_GL_MakeCurrent(window: *mut c_void, ctx: *mut c_void) -> c_int;
    fn SDL_GL_SwapWindow(window: *mut c_void);
    fn SDL_GL_SetAttribute(attr: c_int, value: c_int) -> c_int;
    fn SDL_GL_GetAttribute(attr: c_int, value: *mut c_int) -> c_int;
    fn SDL_GL_SetSwapInterval(interval: c_int) -> c_int;
    fn SDL_GL_DeleteContext(ctx: *mut c_void);
    fn SDL_GetCurrentDisplayMode(display: c_int, mode: *mut SDL_DisplayMode) -> c_int;
    fn SDL_ShowSimpleMessageBox(
        flags: u32,
        title: *const c_char,
        msg: *const c_char,
        win: *mut c_void,
    ) -> c_int;
    fn SDL_Delay(ms: u32);
    fn SDL_SetRelativeMouseMode(enabled: i32) -> c_int;
    fn SDL_GetRelativeMouseMode() -> i32;
    fn SDL_SetWindowFullscreen(window: *mut c_void, flags: u32) -> c_int;
    fn SDL_SetWindowSize(window: *mut c_void, w: c_int, h: c_int);
    fn SDL_SetWindowPosition(window: *mut c_void, x: c_int, y: c_int);
    fn SDL_GetWindowPosition(window: *mut c_void, x: *mut c_int, y: *mut c_int);
    fn SDL_SetWindowMinimumSize(window: *mut c_void, w: c_int, h: c_int);
    fn SDL_SetWindowMaximumSize(window: *mut c_void, w: c_int, h: c_int);
    fn SDL_GetWindowMinimumSize(window: *mut c_void, w: *mut c_int, h: *mut c_int);
    fn SDL_GetWindowMaximumSize(window: *mut c_void, w: *mut c_int, h: *mut c_int);
    fn SDL_GetWindowOpacity(window: *mut c_void, opacity: *mut f32) -> c_int;
    fn SDL_SetWindowOpacity(window: *mut c_void, opacity: f32) -> c_int;
    fn SDL_GetWindowID(window: *mut c_void) -> u32;
    fn SDL_ShowCursor(toggle: c_int) -> c_int;
    fn SDL_StartTextInput();
    fn SDL_StopTextInput();
    fn SDL_WarpMouseGlobal(x: c_int, y: c_int) -> c_int;
    fn SDL_WarpMouseInWindow(window: *mut c_void, x: c_int, y: c_int);
    fn SDL_SetWindowGrab(window: *mut c_void, grabbed: i32);
    fn SDL_GetWindowGrab(window: *mut c_void) -> i32;
    fn SDL_GetGlobalMouseState(x: *mut c_int, y: *mut c_int) -> u32;
}

// SDL constants
const SDL_INIT_EVERYTHING: u32 = 0x0000FFFF;
const SDL_WINDOW_OPENGL: u32 = 0x00000002;
const SDL_WINDOW_RESIZABLE: u32 = 0x00000020;
const SDL_WINDOW_ALLOW_HIGHDPI: u32 = 0x00002000;
const SDL_WINDOWPOS_CENTERED: c_int = 0x2FFF0000;

// SDL_GL attributes
const SDL_GL_CONTEXT_PROFILE_MASK: c_int = 21;
const SDL_GL_CONTEXT_MAJOR_VERSION: c_int = 17;
const SDL_GL_CONTEXT_MINOR_VERSION: c_int = 18;
const SDL_GL_DOUBLEBUFFER: c_int = 5;
const SDL_GL_STENCIL_SIZE: c_int = 7;
const SDL_GL_DEPTH_SIZE: c_int = 6;
const SDL_GL_MULTISAMPLEBUFFERS: c_int = 13;
const SDL_GL_MULTISAMPLESAMPLES: c_int = 14;
const SDL_GL_CONTEXT_PROFILE_CORE: c_int = 0x0001;
const SDL_GL_CONTEXT_PROFILE_COMPATIBILITY: c_int = 0x0002;
const SDL_GL_CONTEXT_PROFILE_ES: c_int = 0x0004;

#[repr(C)]
struct SDL_DisplayMode {
    format: u32,
    w: c_int,
    h: c_int,
    refresh_rate: c_int,
    driverdata: *mut c_void,
}

// SDL_Event is a 56-byte union; we only need the type field and sub-structs
#[repr(C)]
union SDL_Event {
    type_: u32,
    key: SDL_KeyboardEvent,
    motion: SDL_MouseMotionEvent,
    button: SDL_MouseButtonEvent,
    wheel: SDL_MouseWheelEvent,
    window: SDL_WindowEvent,
    text: SDL_TextInputEvent,
    tfinger: SDL_TouchFingerEvent,
    jaxis: SDL_JoyAxisEvent,
    jball: SDL_JoyBallEvent,
    jhat: SDL_JoyHatEvent,
    jbutton: SDL_JoyButtonEvent,
    jdevice: SDL_JoyDeviceEvent,
    cbutton: SDL_ControllerButtonEvent,
    caxis: SDL_ControllerAxisEvent,
    drop: SDL_DropEvent,
    _pad: [u8; 56],
}

#[repr(C)]
#[derive(Copy, Clone)]
struct SDL_KeyboardEvent {
    type_: u32,
    timestamp: u32,
    window_id: u32,
    state: u8,
    repeat: u8,
    _pad: u16,
    scancode: i32,
    sym: i32,
    _mod: u16,
}
#[repr(C)]
#[derive(Copy, Clone)]
struct SDL_MouseMotionEvent {
    type_: u32,
    timestamp: u32,
    window_id: u32,
    which: u32,
    state: u32,
    x: i32,
    y: i32,
    xrel: i32,
    yrel: i32,
}
#[repr(C)]
#[derive(Copy, Clone)]
struct SDL_MouseButtonEvent {
    type_: u32,
    timestamp: u32,
    window_id: u32,
    which: u32,
    button: u8,
    state: u8,
    clicks: u8,
    _pad: u8,
    x: i32,
    y: i32,
}
#[repr(C)]
#[derive(Copy, Clone)]
struct SDL_MouseWheelEvent {
    type_: u32,
    timestamp: u32,
    window_id: u32,
    which: u32,
    x: i32,
    y: i32,
    direction: u32,
}
#[repr(C)]
#[derive(Copy, Clone)]
struct SDL_WindowEvent {
    type_: u32,
    timestamp: u32,
    window_id: u32,
    event: u8,
    _pad: [u8; 3],
    data1: i32,
    data2: i32,
}
#[repr(C)]
#[derive(Copy, Clone)]
struct SDL_TextInputEvent {
    type_: u32,
    timestamp: u32,
    window_id: u32,
    text: [u8; 32],
}
#[repr(C)]
#[derive(Copy, Clone)]
struct SDL_TouchFingerEvent {
    type_: u32,
    timestamp: u32,
    touch_id: i64,
    finger_id: i64,
    x: f32,
    y: f32,
    dx: f32,
    dy: f32,
    pressure: f32,
    window_id: u32,
}
#[repr(C)]
#[derive(Copy, Clone)]
struct SDL_JoyAxisEvent {
    type_: u32,
    timestamp: u32,
    which: i32,
    axis: u8,
    _pad: [u8; 3],
    value: i16,
}
#[repr(C)]
#[derive(Copy, Clone)]
struct SDL_JoyBallEvent {
    type_: u32,
    timestamp: u32,
    which: i32,
    ball: u8,
    _pad: [u8; 3],
    xrel: i16,
    yrel: i16,
}
#[repr(C)]
#[derive(Copy, Clone)]
struct SDL_JoyHatEvent {
    type_: u32,
    timestamp: u32,
    which: i32,
    hat: u8,
    value: u8,
}
#[repr(C)]
#[derive(Copy, Clone)]
struct SDL_JoyButtonEvent {
    type_: u32,
    timestamp: u32,
    which: i32,
    button: u8,
    state: u8,
}
#[repr(C)]
#[derive(Copy, Clone)]
struct SDL_JoyDeviceEvent {
    type_: u32,
    timestamp: u32,
    which: i32,
}
#[repr(C)]
#[derive(Copy, Clone)]
struct SDL_ControllerButtonEvent {
    type_: u32,
    timestamp: u32,
    which: i32,
    button: u8,
    state: u8,
}
#[repr(C)]
#[derive(Copy, Clone)]
struct SDL_ControllerAxisEvent {
    type_: u32,
    timestamp: u32,
    which: i32,
    axis: u8,
    _pad: [u8; 3],
    value: i16,
}
#[repr(C)]
#[derive(Copy, Clone)]
struct SDL_DropEvent {
    type_: u32,
    timestamp: u32,
    file: *mut c_char,
    window_id: u32,
}

// SDL event type constants
const SDL_QUIT: u32 = 0x100;
const SDL_WINDOWEVENT: u32 = 0x200;
const SDL_KEYDOWN: u32 = 0x300;
const SDL_KEYUP: u32 = 0x301;
const SDL_TEXTINPUT: u32 = 0x303;
const SDL_MOUSEMOTION: u32 = 0x400;
const SDL_MOUSEBUTTONDOWN: u32 = 0x401;
const SDL_MOUSEBUTTONUP: u32 = 0x402;
const SDL_MOUSEWHEEL: u32 = 0x403;
const SDL_FINGERDOWN: u32 = 0x700;
const SDL_FINGERUP: u32 = 0x701;
const SDL_FINGERMOTION: u32 = 0x702;
const SDL_CONTROLLERDEVICEADDED: u32 = 0x654;
const SDL_CONTROLLERDEVICEREMOVED: u32 = 0x655;
const SDL_CONTROLLERBUTTONDOWN: u32 = 0x650;
const SDL_CONTROLLERBUTTONUP: u32 = 0x651;
const SDL_CONTROLLERAXISMOTION: u32 = 0x650;
const SDL_DROPBEGIN: u32 = 0x1002;
const SDL_DROPFILE: u32 = 0x1000;
const SDL_DROPTEXT: u32 = 0x1001;
const SDL_DROPCOMPLETE: u32 = 0x1003;

// SDL window event constants
const SDL_WINDOWEVENT_SHOWN: u8 = 1;
const SDL_WINDOWEVENT_HIDDEN: u8 = 2;
const SDL_WINDOWEVENT_EXPOSED: u8 = 3;
const SDL_WINDOWEVENT_MOVED: u8 = 4;
const SDL_WINDOWEVENT_SIZE_CHANGED: u8 = 5;
const SDL_WINDOWEVENT_MINIMIZED: u8 = 7;
const SDL_WINDOWEVENT_MAXIMIZED: u8 = 8;
const SDL_WINDOWEVENT_RESTORED: u8 = 9;
const SDL_WINDOWEVENT_ENTER: u8 = 10;
const SDL_WINDOWEVENT_LEAVE: u8 = 11;
const SDL_WINDOWEVENT_FOCUS_GAINED: u8 = 12;
const SDL_WINDOWEVENT_FOCUS_LOST: u8 = 13;
const SDL_WINDOWEVENT_CLOSE: u8 = 14;

// ============================================================================
// Implementation functions
// ============================================================================

static mut IS_GL_OPTIONS_SET: bool = false;

unsafe extern "C" fn sdl_init_once() -> bool {
    eprintln!("[ash_sdl] init_once called");
    SDL_SetHint(
        b"SDL_JOYSTICK_ALLOW_BACKGROUND_EVENTS\0".as_ptr() as _,
        b"1\0".as_ptr() as _,
    );
    if SDL_Init(SDL_INIT_EVERYTHING) != 0 {
        eprintln!("[ash_sdl] SDL_Init failed");
        return false;
    }
    if !IS_GL_OPTIONS_SET {
        SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
        SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
        SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 2);
        SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
        SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 8);
        SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);
    }
    true
}

unsafe extern "C" fn sdl_gl_options(
    major: i32,
    minor: i32,
    depth: i32,
    stencil: i32,
    flags: i32,
    samples: i32,
) {
    IS_GL_OPTIONS_SET = true;
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, major);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, minor);
    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, depth);
    SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, stencil);
    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, flags & 1);
    if flags & 2 != 0 {
        SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
    } else if flags & 4 != 0 {
        SDL_GL_SetAttribute(
            SDL_GL_CONTEXT_PROFILE_MASK,
            SDL_GL_CONTEXT_PROFILE_COMPATIBILITY,
        );
    } else if flags & 8 != 0 {
        SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_ES);
    } else {
        SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
    }
    if samples > 1 {
        SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 1);
        SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, samples);
    }
}

unsafe extern "C" fn sdl_event_loop(event: *mut EventData) -> bool {
    loop {
        let mut e: SDL_Event = std::mem::zeroed();
        if SDL_PollEvent(&mut e) == 0 {
            break;
        }
        match e.type_ {
            SDL_QUIT => {
                (*event).event_type = EventType::Quit as i32;
            }
            SDL_MOUSEMOTION => {
                (*event).event_type = EventType::MouseMove as i32;
                (*event).window = e.motion.window_id as i32;
                (*event).mouse_x = e.motion.x;
                (*event).mouse_y = e.motion.y;
                (*event).mouse_x_rel = e.motion.xrel;
                (*event).mouse_y_rel = e.motion.yrel;
            }
            SDL_KEYDOWN => {
                (*event).event_type = EventType::KeyDown as i32;
                (*event).window = e.key.window_id as i32;
                (*event).key_code = e.key.sym;
                (*event).scan_code = e.key.scancode;
                (*event).key_repeat = if e.key.repeat != 0 { 1 } else { 0 };
            }
            SDL_KEYUP => {
                (*event).event_type = EventType::KeyUp as i32;
                (*event).window = e.key.window_id as i32;
                (*event).key_code = e.key.sym;
                (*event).scan_code = e.key.scancode;
            }
            SDL_MOUSEBUTTONDOWN => {
                (*event).event_type = EventType::MouseDown as i32;
                (*event).window = e.button.window_id as i32;
                (*event).button = e.button.button as i32;
                (*event).mouse_x = e.button.x;
                (*event).mouse_y = e.button.y;
            }
            SDL_MOUSEBUTTONUP => {
                (*event).event_type = EventType::MouseUp as i32;
                (*event).window = e.button.window_id as i32;
                (*event).button = e.button.button as i32;
                (*event).mouse_x = e.button.x;
                (*event).mouse_y = e.button.y;
            }
            SDL_MOUSEWHEEL => {
                (*event).event_type = EventType::MouseWheel as i32;
                (*event).window = e.wheel.window_id as i32;
                (*event).wheel_delta = e.wheel.y;
                (*event).mouse_x = e.wheel.x;
                (*event).mouse_y = e.wheel.y;
            }
            SDL_WINDOWEVENT => {
                (*event).event_type = EventType::WindowState as i32;
                (*event).window = e.window.window_id as i32;
                (*event).state = match e.window.event {
                    SDL_WINDOWEVENT_SHOWN => WsChange::Show as i32,
                    SDL_WINDOWEVENT_HIDDEN => WsChange::Hide as i32,
                    SDL_WINDOWEVENT_EXPOSED => WsChange::Expose as i32,
                    SDL_WINDOWEVENT_MOVED => WsChange::Move as i32,
                    SDL_WINDOWEVENT_SIZE_CHANGED => WsChange::Resize as i32,
                    SDL_WINDOWEVENT_MINIMIZED => WsChange::Minimize as i32,
                    SDL_WINDOWEVENT_MAXIMIZED => WsChange::Maximize as i32,
                    SDL_WINDOWEVENT_RESTORED => WsChange::Restore as i32,
                    SDL_WINDOWEVENT_ENTER => WsChange::Enter as i32,
                    SDL_WINDOWEVENT_LEAVE => WsChange::Leave as i32,
                    SDL_WINDOWEVENT_FOCUS_GAINED => WsChange::Focus as i32,
                    SDL_WINDOWEVENT_FOCUS_LOST => WsChange::Blur as i32,
                    SDL_WINDOWEVENT_CLOSE => WsChange::Close as i32,
                    _ => continue,
                };
            }
            SDL_TEXTINPUT => {
                (*event).event_type = EventType::TextInput as i32;
                (*event).window = e.text.window_id as i32;
                let t = &e.text.text;
                let mut code = *(t.as_ptr() as *const i32);
                let mask = if t[0] == 0 {
                    0
                } else if t[1] == 0 {
                    0xFF
                } else if t[2] == 0 {
                    0xFFFF
                } else if t[3] == 0 {
                    0xFFFFFF
                } else {
                    0xFFFFFFFFu32 as i32
                };
                code &= mask;
                (*event).key_code = code;
            }
            SDL_FINGERDOWN => {
                (*event).event_type = EventType::TouchDown as i32;
                (*event).mouse_x = (e.tfinger.x * 10000.0) as i32;
                (*event).mouse_y = (e.tfinger.y * 10000.0) as i32;
                (*event).reference = e.tfinger.finger_id as i32;
            }
            SDL_FINGERUP => {
                (*event).event_type = EventType::TouchUp as i32;
                (*event).mouse_x = (e.tfinger.x * 10000.0) as i32;
                (*event).mouse_y = (e.tfinger.y * 10000.0) as i32;
                (*event).reference = e.tfinger.finger_id as i32;
            }
            SDL_FINGERMOTION => {
                (*event).event_type = EventType::TouchMove as i32;
                (*event).mouse_x = (e.tfinger.x * 10000.0) as i32;
                (*event).mouse_y = (e.tfinger.y * 10000.0) as i32;
                (*event).reference = e.tfinger.finger_id as i32;
            }
            _ => continue,
        }
        return true;
    }
    false
}

unsafe extern "C" fn sdl_hint_value(name: *const u8, value: *const u8) -> bool {
    SDL_SetHint(name as _, value as _) == 1
}

unsafe extern "C" fn sdl_quit() {
    SDL_Quit();
}
unsafe extern "C" fn sdl_delay(ms: i32) {
    hl_blocking(true);
    SDL_Delay(ms as u32);
    hl_blocking(false);
}
unsafe extern "C" fn sdl_detect_win32() -> bool {
    false
}
unsafe extern "C" fn sdl_set_vsync(v: bool) {
    SDL_GL_SetSwapInterval(if v { 1 } else { 0 });
}

unsafe extern "C" fn sdl_get_screen_width() -> i32 {
    let mut mode: SDL_DisplayMode = std::mem::zeroed();
    SDL_GetCurrentDisplayMode(0, &mut mode);
    mode.w
}
unsafe extern "C" fn sdl_get_screen_height() -> i32 {
    let mut mode: SDL_DisplayMode = std::mem::zeroed();
    SDL_GetCurrentDisplayMode(0, &mut mode);
    mode.h
}
unsafe extern "C" fn sdl_get_screen_width_of_window(win: *mut c_void) -> i32 {
    let idx = if !win.is_null() {
        SDL_GetWindowDisplayIndex(win)
    } else {
        0
    };
    let mut mode: SDL_DisplayMode = std::mem::zeroed();
    SDL_GetCurrentDisplayMode(idx, &mut mode);
    mode.w
}
unsafe extern "C" fn sdl_get_screen_height_of_window(win: *mut c_void) -> i32 {
    let idx = if !win.is_null() {
        SDL_GetWindowDisplayIndex(win)
    } else {
        0
    };
    let mut mode: SDL_DisplayMode = std::mem::zeroed();
    SDL_GetCurrentDisplayMode(idx, &mut mode);
    mode.h
}
unsafe extern "C" fn sdl_get_framerate(win: *mut c_void) -> i32 {
    let idx = if !win.is_null() {
        SDL_GetWindowDisplayIndex(win)
    } else {
        0
    };
    let mut mode: SDL_DisplayMode = std::mem::zeroed();
    SDL_GetCurrentDisplayMode(idx, &mut mode);
    mode.refresh_rate
}

unsafe extern "C" fn sdl_message_box(title: *const u8, text: *const u8, error: bool) {
    let flags = if error { 0x10 } else { 0 }; // SDL_MESSAGEBOX_ERROR
    hl_blocking(true);
    SDL_ShowSimpleMessageBox(flags, title as _, text as _, ptr::null_mut());
    hl_blocking(false);
}

unsafe extern "C" fn sdl_text_input(enable: bool) {
    if enable {
        SDL_StartTextInput();
    } else {
        SDL_StopTextInput();
    }
}
unsafe extern "C" fn sdl_set_relative_mouse_mode(enable: bool) -> i32 {
    SDL_SetRelativeMouseMode(enable as i32)
}
unsafe extern "C" fn sdl_get_relative_mouse_mode() -> bool {
    SDL_GetRelativeMouseMode() != 0
}
unsafe extern "C" fn sdl_warp_mouse_global(x: i32, y: i32) -> i32 {
    SDL_WarpMouseGlobal(x, y)
}
unsafe extern "C" fn sdl_warp_mouse_in_window(win: *mut c_void, x: i32, y: i32) {
    SDL_WarpMouseInWindow(win, x, y);
}
unsafe extern "C" fn sdl_set_window_grab(win: *mut c_void, grabbed: bool) {
    SDL_SetWindowGrab(win, grabbed as i32);
}
unsafe extern "C" fn sdl_get_window_grab(win: *mut c_void) -> bool {
    SDL_GetWindowGrab(win) != 0
}
unsafe extern "C" fn sdl_get_global_mouse_state(x: *mut i32, y: *mut i32) -> i32 {
    SDL_GetGlobalMouseState(x, y) as i32
}
unsafe extern "C" fn sdl_detect_keyboard_layout() -> *const u8 {
    b"qwerty\0".as_ptr()
}
unsafe extern "C" fn sdl_show_cursor(show: bool) {
    SDL_ShowCursor(if show { 1 } else { 0 });
}
unsafe extern "C" fn sdl_is_cursor_visible() -> bool {
    SDL_ShowCursor(-1) == 1
}

// Window functions
unsafe extern "C" fn sdl_win_create_ex(w: i32, h: i32, x: i32, y: i32, flags: i32) -> *mut c_void {
    eprintln!(
        "[ash_sdl] win_create_ex w={} h={} x={} y={} flags={:#x}",
        w, h, x, y, flags
    );
    let mut sdl_flags = SDL_WINDOW_OPENGL | SDL_WINDOW_ALLOW_HIGHDPI;
    if flags & 1 != 0 {
        sdl_flags |= SDL_WINDOW_RESIZABLE;
    }
    // If w/h look like position values and x/y look like dimensions, swap them.
    // Heaps may pass (x, y, w, h, flags) instead of (w, h, x, y, flags).
    let (w, h, x, y) = if w > 10000 && h > 10000 && x > 0 && x < 10000 && y > 0 && y < 10000 {
        eprintln!(
            "[ash_sdl] Detected swapped args: using x={} y={} as w/h",
            x, y
        );
        (x, y, w, h)
    } else {
        (w, h, x, y)
    };
    let px = if x == -1 || x > 100000 {
        SDL_WINDOWPOS_CENTERED
    } else {
        x
    };
    let py = if y == -1 || y > 100000 {
        SDL_WINDOWPOS_CENTERED
    } else {
        y
    };
    let w = if w <= 0 { 800 } else { w };
    let h = if h <= 0 { 600 } else { h };
    let win = SDL_CreateWindow(b"Ash\0".as_ptr() as _, px, py, w, h, sdl_flags);
    eprintln!("[ash_sdl] SDL_CreateWindow -> {:p} ({}x{})", win, w, h);
    win
}
unsafe extern "C" fn sdl_win_create(w: i32, h: i32) -> *mut c_void {
    sdl_win_create_ex(w, h, -1, -1, 1)
}
extern "C" {
    fn SDL_ShowWindow(window: *mut c_void);
    fn SDL_PumpEvents();
}

unsafe extern "C" fn sdl_win_get_glcontext(win: *mut c_void) -> *mut c_void {
    // macOS Metal backend requires the window to be visible before the
    // GL context's backing texture can be created (otherwise width=0).
    SDL_ShowWindow(win);
    SDL_PumpEvents(); // process the show event so the drawable surface is ready
    SDL_GL_CreateContext(win)
}
unsafe extern "C" fn sdl_win_set_fullscreen(win: *mut c_void, mode: i32) -> bool {
    let flags = match mode {
        1 => 0x00000001,
        2 => 0x00001001,
        _ => 0,
    };
    SDL_SetWindowFullscreen(win, flags) == 0
}
unsafe extern "C" fn sdl_win_set_title(win: *mut c_void, title: *const u8) {
    SDL_SetWindowTitle(win, title as _);
}
unsafe extern "C" fn sdl_win_get_size(win: *mut c_void, w: *mut i32, h: *mut i32) {
    SDL_GetWindowSize(win, w, h);
}
unsafe extern "C" fn sdl_win_set_size(win: *mut c_void, w: i32, h: i32) {
    SDL_SetWindowSize(win, w, h);
}
unsafe extern "C" fn sdl_win_set_position(win: *mut c_void, x: i32, y: i32) {
    SDL_SetWindowPosition(win, x, y);
}
unsafe extern "C" fn sdl_win_get_position(win: *mut c_void, x: *mut i32, y: *mut i32) {
    SDL_GetWindowPosition(win, x, y);
}
unsafe extern "C" fn sdl_win_set_min_size(win: *mut c_void, w: i32, h: i32) {
    SDL_SetWindowMinimumSize(win, w, h);
}
unsafe extern "C" fn sdl_win_set_max_size(win: *mut c_void, w: i32, h: i32) {
    SDL_SetWindowMaximumSize(win, w, h);
}
unsafe extern "C" fn sdl_win_get_min_size(win: *mut c_void, w: *mut i32, h: *mut i32) {
    SDL_GetWindowMinimumSize(win, w, h);
}
unsafe extern "C" fn sdl_win_get_max_size(win: *mut c_void, w: *mut i32, h: *mut i32) {
    SDL_GetWindowMaximumSize(win, w, h);
}
unsafe extern "C" fn sdl_win_get_opacity(win: *mut c_void) -> f64 {
    let mut op: f32 = 1.0;
    SDL_GetWindowOpacity(win, &mut op);
    op as f64
}
unsafe extern "C" fn sdl_win_set_opacity(win: *mut c_void, opacity: f64) -> bool {
    SDL_SetWindowOpacity(win, opacity as f32) == 0
}
unsafe extern "C" fn sdl_win_swap_window(win: *mut c_void) {
    static SWAP_COUNT: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);
    let c = SWAP_COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    if c < 5 {
        eprintln!("[ash_sdl] win_swap_window #{}", c);
    }
    SDL_GL_SwapWindow(win);
}
unsafe extern "C" fn sdl_win_render_to(win: *mut c_void, ctx: *mut c_void) {
    SDL_GL_MakeCurrent(win, ctx);
}
unsafe extern "C" fn sdl_win_destroy(win: *mut c_void, ctx: *mut c_void) {
    // Must detach GL context before destroying the window to avoid
    // recursive lock in macOS OpenGL renderer (GLRDrawable::dealloc).
    if !win.is_null() && !ctx.is_null() {
        SDL_GL_MakeCurrent(win, std::ptr::null_mut());
    }
    if !ctx.is_null() {
        SDL_GL_DeleteContext(ctx);
    }
    if !win.is_null() {
        SDL_DestroyWindow(win);
    }
}
unsafe extern "C" fn sdl_win_get_id(win: *mut c_void) -> i32 {
    SDL_GetWindowID(win) as i32
}
unsafe extern "C" fn sdl_win_resize(win: *mut c_void, mode: i32) { /* stub */
}
unsafe extern "C" fn sdl_win_set_display_mode(
    _w: *mut c_void,
    _ww: i32,
    _hh: i32,
    _r: i32,
) -> bool {
    false
}
unsafe extern "C" fn sdl_win_display_handle(win: *mut c_void) -> i32 {
    SDL_GetWindowDisplayIndex(win)
}

// Stubs for functions not yet needed
unsafe extern "C" fn sdl_event_poll(_e: *mut c_void) -> i32 {
    0
}
unsafe extern "C" fn sdl_set_clipboard_text(_t: *const u8) -> bool {
    false
}
unsafe extern "C" fn sdl_get_clipboard_text() -> *const u8 {
    b"\0".as_ptr()
}
unsafe extern "C" fn sdl_set_drag_and_drop_enabled(_e: bool) {}
unsafe extern "C" fn sdl_get_drag_and_drop_enabled() -> bool {
    false
}
unsafe extern "C" fn sdl_get_displays() -> *mut c_void {
    ptr::null_mut()
}
unsafe extern "C" fn sdl_get_display_modes(_d: i32) -> *mut c_void {
    ptr::null_mut()
}
unsafe extern "C" fn sdl_get_current_display_mode(_d: i32, _desktop: bool) -> *mut c_void {
    ptr::null_mut()
}
unsafe extern "C" fn sdl_get_devices() -> *mut c_void {
    ptr::null_mut()
}

// Controller/joystick/haptic/cursor stubs
unsafe extern "C" fn sdl_gctrl_count() -> i32 {
    0
}
unsafe extern "C" fn sdl_gctrl_open(_i: i32) -> *mut c_void {
    ptr::null_mut()
}
unsafe extern "C" fn sdl_gctrl_close(_c: *mut c_void) {}
unsafe extern "C" fn sdl_gctrl_get_axis(_c: *mut c_void, _a: i32) -> i32 {
    0
}
unsafe extern "C" fn sdl_gctrl_get_button(_c: *mut c_void, _b: i32) -> bool {
    false
}
unsafe extern "C" fn sdl_gctrl_get_id(_c: *mut c_void) -> i32 {
    0
}
unsafe extern "C" fn sdl_gctrl_get_name(_c: *mut c_void) -> *const u8 {
    b"\0".as_ptr()
}
unsafe extern "C" fn sdl_joy_count() -> i32 {
    0
}
unsafe extern "C" fn sdl_joy_open(_i: i32) -> *mut c_void {
    ptr::null_mut()
}
unsafe extern "C" fn sdl_joy_close(_j: *mut c_void) {}
unsafe extern "C" fn sdl_joy_get_axis(_j: *mut c_void, _a: i32) -> i32 {
    0
}
unsafe extern "C" fn sdl_joy_get_hat(_j: *mut c_void, _h: i32) -> i32 {
    0
}
unsafe extern "C" fn sdl_joy_get_button(_j: *mut c_void, _b: i32) -> bool {
    false
}
unsafe extern "C" fn sdl_joy_get_id(_j: *mut c_void) -> i32 {
    0
}
unsafe extern "C" fn sdl_joy_get_name(_j: *mut c_void) -> *const u8 {
    b"\0".as_ptr()
}
unsafe extern "C" fn sdl_haptic_open(_c: *mut c_void) -> *mut c_void {
    ptr::null_mut()
}
unsafe extern "C" fn sdl_haptic_close(_h: *mut c_void) {}
unsafe extern "C" fn sdl_haptic_rumble_init(_h: *mut c_void) -> i32 {
    -1
}
unsafe extern "C" fn sdl_haptic_rumble_play(_h: *mut c_void, _s: f64, _d: i32) -> i32 {
    -1
}
unsafe extern "C" fn sdl_cursor_create(_s: *mut c_void, _x: i32, _y: i32) -> *mut c_void {
    ptr::null_mut()
}
unsafe extern "C" fn sdl_cursor_create_system(_id: i32) -> *mut c_void {
    ptr::null_mut()
}
unsafe extern "C" fn sdl_free_cursor(_c: *mut c_void) {}
unsafe extern "C" fn sdl_set_cursor(_c: *mut c_void) {}
unsafe extern "C" fn sdl_surface_from(
    _b: *const u8,
    _w: i32,
    _h: i32,
    _p: i32,
    _rm: i32,
    _gm: i32,
    _bm: i32,
    _am: i32,
    _dep: i32,
) -> *mut c_void {
    ptr::null_mut()
}
unsafe extern "C" fn sdl_free_surface(_s: *mut c_void) {}

// ============================================================================
// DEFINE_PRIM resolvers
// ============================================================================

define_prim!(hlp_init_once, "Pb", sdl_init_once);
define_prim!(hlp_gl_options, "Piiiiii", sdl_gl_options);
define_prim!(hlp_event_loop, "PD_b", sdl_event_loop);
define_prim!(hlp_event_poll, "Pp_i", sdl_event_poll);
define_prim!(hlp_quit, "P_v", sdl_quit);
define_prim!(hlp_delay, "Pi_v", sdl_delay);
define_prim!(hlp_get_screen_width, "P_i", sdl_get_screen_width);
define_prim!(hlp_get_screen_height, "P_i", sdl_get_screen_height);
define_prim!(
    hlp_get_screen_width_of_window,
    "PXsdl_window__i",
    sdl_get_screen_width_of_window
);
define_prim!(
    hlp_get_screen_height_of_window,
    "PXsdl_window__i",
    sdl_get_screen_height_of_window
);
define_prim!(hlp_get_framerate, "PXsdl_window__i", sdl_get_framerate);
define_prim!(hlp_message_box, "PBBb_v", sdl_message_box);
define_prim!(hlp_set_vsync, "Pb_v", sdl_set_vsync);
define_prim!(hlp_detect_win32, "P_b", sdl_detect_win32);
define_prim!(hlp_text_input, "Pb_v", sdl_text_input);
define_prim!(
    hlp_set_relative_mouse_mode,
    "Pb_i",
    sdl_set_relative_mouse_mode
);
define_prim!(
    hlp_get_relative_mouse_mode,
    "P_b",
    sdl_get_relative_mouse_mode
);
define_prim!(hlp_warp_mouse_global, "Pii_i", sdl_warp_mouse_global);
define_prim!(
    hlp_warp_mouse_in_window,
    "PXsdl_window_ii_v",
    sdl_warp_mouse_in_window
);
define_prim!(hlp_set_window_grab, "PXsdl_window_b_v", sdl_set_window_grab);
define_prim!(hlp_get_window_grab, "PXsdl_window__b", sdl_get_window_grab);
define_prim!(
    hlp_get_global_mouse_state,
    "PRiRi_i",
    sdl_get_global_mouse_state
);
define_prim!(
    hlp_detect_keyboard_layout,
    "P_B",
    sdl_detect_keyboard_layout
);
define_prim!(hlp_hint_value, "PBB_b", sdl_hint_value);
define_prim!(hlp_win_create_ex, "Piiiii_Xsdl_window_", sdl_win_create_ex);
define_prim!(hlp_win_create, "Pii_Xsdl_window_", sdl_win_create);
define_prim!(
    hlp_win_get_glcontext,
    "PXsdl_window__Xsdl_gl_",
    sdl_win_get_glcontext
);
define_prim!(
    hlp_win_set_fullscreen,
    "PXsdl_window_i_b",
    sdl_win_set_fullscreen
);
define_prim!(
    hlp_win_set_display_mode,
    "PXsdl_window_iii_b",
    sdl_win_set_display_mode
);
define_prim!(
    hlp_win_display_handle,
    "PXsdl_window__i",
    sdl_win_display_handle
);
define_prim!(hlp_win_resize, "PXsdl_window_i_v", sdl_win_resize);
define_prim!(hlp_win_set_title, "PXsdl_window_B_v", sdl_win_set_title);
define_prim!(
    hlp_win_set_position,
    "PXsdl_window_ii_v",
    sdl_win_set_position
);
define_prim!(
    hlp_win_get_position,
    "PXsdl_window_RiRi_v",
    sdl_win_get_position
);
define_prim!(hlp_win_set_size, "PXsdl_window_ii_v", sdl_win_set_size);
define_prim!(
    hlp_win_set_min_size,
    "PXsdl_window_ii_v",
    sdl_win_set_min_size
);
define_prim!(
    hlp_win_set_max_size,
    "PXsdl_window_ii_v",
    sdl_win_set_max_size
);
define_prim!(hlp_win_get_size, "PXsdl_window_RiRi_v", sdl_win_get_size);
define_prim!(
    hlp_win_get_min_size,
    "PXsdl_window_RiRi_v",
    sdl_win_get_min_size
);
define_prim!(
    hlp_win_get_max_size,
    "PXsdl_window_RiRi_v",
    sdl_win_get_max_size
);
define_prim!(hlp_win_get_opacity, "PXsdl_window__d", sdl_win_get_opacity);
define_prim!(hlp_win_set_opacity, "PXsdl_window_d_b", sdl_win_set_opacity);
define_prim!(hlp_win_swap_window, "PXsdl_window__v", sdl_win_swap_window);
define_prim!(
    hlp_win_render_to,
    "PXsdl_window_Xsdl_gl__v",
    sdl_win_render_to
);
define_prim!(hlp_win_destroy, "PXsdl_window_Xsdl_gl__v", sdl_win_destroy);
define_prim!(hlp_win_get_id, "PXsdl_window__i", sdl_win_get_id);
define_prim!(hlp_show_cursor, "Pb_v", sdl_show_cursor);
define_prim!(hlp_is_cursor_visible, "P_b", sdl_is_cursor_visible);
define_prim!(
    hlp_cursor_create,
    "PXsdl_surface_ii_Xsdl_cursor_",
    sdl_cursor_create
);
define_prim!(
    hlp_cursor_create_system,
    "Pi_Xsdl_cursor_",
    sdl_cursor_create_system
);
define_prim!(hlp_free_cursor, "PXsdl_cursor__v", sdl_free_cursor);
define_prim!(hlp_set_cursor, "PXsdl_cursor__v", sdl_set_cursor);
define_prim!(hlp_set_clipboard_text, "PB_b", sdl_set_clipboard_text);
define_prim!(hlp_get_clipboard_text, "P_B", sdl_get_clipboard_text);
define_prim!(
    hlp_set_drag_and_drop_enabled,
    "Pb_v",
    sdl_set_drag_and_drop_enabled
);
define_prim!(
    hlp_get_drag_and_drop_enabled,
    "P_b",
    sdl_get_drag_and_drop_enabled
);
define_prim!(hlp_get_displays, "P_A", sdl_get_displays);
define_prim!(hlp_get_display_modes, "Pi_A", sdl_get_display_modes);
define_prim!(
    hlp_get_current_display_mode,
    "Pib_D",
    sdl_get_current_display_mode
);
define_prim!(hlp_get_devices, "P_A", sdl_get_devices);
define_prim!(hlp_gctrl_count, "P_i", sdl_gctrl_count);
define_prim!(hlp_gctrl_open, "Pi_Xsdl_gctrl_", sdl_gctrl_open);
define_prim!(hlp_gctrl_close, "PXsdl_gctrl__v", sdl_gctrl_close);
define_prim!(hlp_gctrl_get_axis, "PXsdl_gctrl_i_i", sdl_gctrl_get_axis);
define_prim!(
    hlp_gctrl_get_button,
    "PXsdl_gctrl_i_b",
    sdl_gctrl_get_button
);
define_prim!(hlp_gctrl_get_id, "PXsdl_gctrl__i", sdl_gctrl_get_id);
define_prim!(hlp_gctrl_get_name, "PXsdl_gctrl__B", sdl_gctrl_get_name);
define_prim!(
    hlp_surface_from,
    "PBiiiiiii_Xsdl_surface_",
    sdl_surface_from
);
define_prim!(hlp_free_surface, "PXsdl_surface__v", sdl_free_surface);
