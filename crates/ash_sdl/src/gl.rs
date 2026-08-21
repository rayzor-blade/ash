//! OpenGL wrapper functions matching HashLink sdl.hdll gl.c API.

use crate::resolver::define_prim;
use crate::types::*;
use std::ffi::{c_char, c_void, CString};
use std::ptr;

// ============================================================================
// OpenGL FFI (linked via -framework OpenGL on macOS)
// ============================================================================

#[allow(non_snake_case)]
extern "C" {
    fn glClear(mask: u32);
    fn glGetError() -> u32;
    fn glScissor(x: i32, y: i32, w: i32, h: i32);
    fn glClearColor(r: f32, g: f32, b: f32, a: f32);
    fn glClearDepth(depth: f64);
    fn glClearStencil(s: i32);
    fn glViewport(x: i32, y: i32, w: i32, h: i32);
    fn glFlush();
    fn glFinish();
    fn glPixelStorei(pname: u32, param: i32);
    fn glGetString(name: u32) -> *const u8;
    fn glEnable(cap: u32);
    fn glDisable(cap: u32);
    fn glCullFace(mode: u32);
    fn glBlendFunc(sfactor: u32, dfactor: u32);
    fn glBlendFuncSeparate(src: u32, dst: u32, srca: u32, dsta: u32);
    fn glBlendEquation(mode: u32);
    fn glBlendEquationSeparate(rgb: u32, a: u32);
    fn glDepthMask(flag: u8);
    fn glDepthFunc(func: u32);
    fn glColorMask(r: u8, g: u8, b: u8, a: u8);
    fn glStencilMaskSeparate(face: u32, mask: u32);
    fn glStencilFuncSeparate(face: u32, func: u32, ref_: i32, mask: u32);
    fn glStencilOpSeparate(face: u32, sfail: u32, dpfail: u32, dppass: u32);
    fn glPolygonMode(face: u32, mode: u32);
    fn glPolygonOffset(factor: f32, units: f32);
    fn glCreateProgram() -> u32;
    fn glDeleteProgram(program: u32);
    fn glAttachShader(program: u32, shader: u32);
    fn glLinkProgram(program: u32);
    fn glGetProgramiv(program: u32, pname: u32, params: *mut i32);
    fn glGetProgramInfoLog(program: u32, bufsize: i32, length: *mut i32, log: *mut u8);
    fn glGetUniformLocation(program: u32, name: *const c_char) -> i32;
    fn glGetAttribLocation(program: u32, name: *const c_char) -> i32;
    fn glUseProgram(program: u32);
    fn glBindFragDataLocation(program: u32, color: u32, name: *const c_char);
    fn glCreateShader(type_: u32) -> u32;
    fn glShaderSource(shader: u32, count: i32, string: *const *const c_char, length: *const i32);
    fn glCompileShader(shader: u32);
    fn glGetShaderiv(shader: u32, pname: u32, params: *mut i32);
    fn glGetShaderInfoLog(shader: u32, bufsize: i32, length: *mut i32, log: *mut u8);
    fn glDeleteShader(shader: u32);
    fn glGenTextures(n: i32, textures: *mut u32);
    fn glActiveTexture(texture: u32);
    fn glBindTexture(target: u32, texture: u32);
    fn glTexParameteri(target: u32, pname: u32, param: i32);
    fn glTexParameterf(target: u32, pname: u32, param: f32);
    fn glTexImage2D(
        target: u32,
        level: i32,
        ifmt: i32,
        w: i32,
        h: i32,
        border: i32,
        fmt: u32,
        type_: u32,
        data: *const u8,
    );
    fn glTexSubImage2D(
        target: u32,
        level: i32,
        x: i32,
        y: i32,
        w: i32,
        h: i32,
        fmt: u32,
        type_: u32,
        data: *const u8,
    );
    fn glTexImage3D(
        target: u32,
        level: i32,
        ifmt: i32,
        w: i32,
        h: i32,
        d: i32,
        border: i32,
        fmt: u32,
        type_: u32,
        data: *const u8,
    );
    fn glTexSubImage3D(
        target: u32,
        level: i32,
        x: i32,
        y: i32,
        z: i32,
        w: i32,
        h: i32,
        d: i32,
        fmt: u32,
        type_: u32,
        data: *const u8,
    );
    fn glTexStorage2D(target: u32, levels: i32, ifmt: u32, w: i32, h: i32);
    fn glTexStorage3D(target: u32, levels: i32, ifmt: u32, w: i32, h: i32, d: i32);
    fn glCompressedTexImage2D(
        target: u32,
        level: i32,
        ifmt: u32,
        w: i32,
        h: i32,
        border: i32,
        size: i32,
        data: *const u8,
    );
    fn glCompressedTexImage3D(
        target: u32,
        level: i32,
        ifmt: u32,
        w: i32,
        h: i32,
        d: i32,
        border: i32,
        size: i32,
        data: *const u8,
    );
    fn glCompressedTexSubImage2D(
        target: u32,
        level: i32,
        x: i32,
        y: i32,
        w: i32,
        h: i32,
        fmt: u32,
        size: i32,
        data: *const u8,
    );
    fn glCompressedTexSubImage3D(
        target: u32,
        level: i32,
        x: i32,
        y: i32,
        z: i32,
        w: i32,
        h: i32,
        d: i32,
        fmt: u32,
        size: i32,
        data: *const u8,
    );
    fn glGenerateMipmap(target: u32);
    fn glDeleteTextures(n: i32, textures: *const u32);
    fn glGenFramebuffers(n: i32, fbs: *mut u32);
    fn glBindFramebuffer(target: u32, fb: u32);
    fn glFramebufferTexture(target: u32, attachment: u32, texture: u32, level: i32);
    fn glFramebufferTexture2D(
        target: u32,
        attachment: u32,
        textarget: u32,
        texture: u32,
        level: i32,
    );
    fn glFramebufferTextureLayer(
        target: u32,
        attachment: u32,
        texture: u32,
        level: i32,
        layer: i32,
    );
    fn glDeleteFramebuffers(n: i32, fbs: *const u32);
    fn glBlitFramebuffer(
        sx0: i32,
        sy0: i32,
        sx1: i32,
        sy1: i32,
        dx0: i32,
        dy0: i32,
        dx1: i32,
        dy1: i32,
        mask: u32,
        filter: u32,
    );
    fn glReadPixels(x: i32, y: i32, w: i32, h: i32, fmt: u32, type_: u32, data: *mut u8);
    fn glReadBuffer(src: u32);
    fn glDrawBuffers(n: i32, bufs: *const u32);
    fn glGenRenderbuffers(n: i32, rbs: *mut u32);
    fn glBindRenderbuffer(target: u32, rb: u32);
    fn glRenderbufferStorage(target: u32, ifmt: u32, w: i32, h: i32);
    fn glRenderbufferStorageMultisample(target: u32, samples: i32, ifmt: u32, w: i32, h: i32);
    fn glFramebufferRenderbuffer(target: u32, attachment: u32, rbtarget: u32, rb: u32);
    fn glDeleteRenderbuffers(n: i32, rbs: *const u32);
    fn glGenBuffers(n: i32, buffers: *mut u32);
    fn glBindBuffer(target: u32, buffer: u32);
    fn glBindBufferBase(target: u32, index: u32, buffer: u32);
    fn glBufferData(target: u32, size: isize, data: *const u8, usage: u32);
    fn glBufferSubData(target: u32, offset: isize, size: isize, data: *const u8);
    fn glEnableVertexAttribArray(index: u32);
    fn glDisableVertexAttribArray(index: u32);
    fn glVertexAttribPointer(
        index: u32,
        size: i32,
        type_: u32,
        normalized: u8,
        stride: i32,
        pointer: *const c_void,
    );
    fn glVertexAttribIPointer(
        index: u32,
        size: i32,
        type_: u32,
        stride: i32,
        pointer: *const c_void,
    );
    fn glDeleteBuffers(n: i32, buffers: *const u32);
    fn glUniform1i(location: i32, v0: i32);
    fn glUniform4fv(location: i32, count: i32, value: *const f32);
    fn glUniformMatrix4fv(location: i32, count: i32, transpose: u8, value: *const f32);
    fn glDrawElements(mode: u32, count: i32, type_: u32, indices: *const c_void);
    fn glDrawElementsInstanced(
        mode: u32,
        count: i32,
        type_: u32,
        indices: *const c_void,
        instancecount: i32,
    );
    fn glDrawArrays(mode: u32, first: i32, count: i32);
    fn glDrawArraysInstanced(mode: u32, first: i32, count: i32, instancecount: i32);
    fn glGenVertexArrays(n: i32, arrays: *mut u32);
    fn glBindVertexArray(array: u32);
    fn glDeleteVertexArrays(n: i32, arrays: *const u32);
    fn glVertexAttribDivisor(index: u32, divisor: u32);
    fn glGenQueries(n: i32, ids: *mut u32);
    fn glDeleteQueries(n: i32, ids: *const u32);
    fn glBeginQuery(target: u32, id: u32);
    fn glEndQuery(target: u32);
    fn glGetQueryObjectuiv(id: u32, pname: u32, params: *mut u32);
    fn glGetStringi(name: u32, index: u32) -> *const u8;
    fn glGetIntegerv(pname: u32, data: *mut i32);
    fn glGetUniformBlockIndex(program: u32, name: *const c_char) -> u32;
    fn glUniformBlockBinding(program: u32, index: u32, binding: u32);
}

extern "C" {
    fn SDL_GL_GetAttribute(attr: i32, value: *mut i32) -> i32;
}

/// Helper: extract i32 from a HNULL(HI32) vdynamic, or 0 if null.
unsafe fn zidx(val: *mut vdynamic) -> u32 {
    if val.is_null() {
        0
    } else {
        (*val).v.i as u32
    }
}

// ============================================================================
// GL implementation functions
// ============================================================================

unsafe extern "C" fn gl_init() -> bool {
    true
} // On macOS, GL functions are linked statically
unsafe extern "C" fn gl_is_context_lost() -> bool {
    false
}
unsafe extern "C" fn gl_clear(bits: i32) {
    static CLEAR_COUNT: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);
    let c = CLEAR_COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    if c < 5 {
        eprintln!("[ash_sdl] gl_clear #{} bits={:#x}", c, bits);
    }
    glClear(bits as u32);
}
unsafe extern "C" fn gl_get_error() -> i32 {
    glGetError() as i32
}
unsafe extern "C" fn gl_scissor(x: i32, y: i32, w: i32, h: i32) {
    glScissor(x, y, w, h);
}
unsafe extern "C" fn gl_clear_color(r: f64, g: f64, b: f64, a: f64) {
    glClearColor(r as f32, g as f32, b as f32, a as f32);
}
unsafe extern "C" fn gl_clear_depth(v: f64) {
    glClearDepth(v);
}
unsafe extern "C" fn gl_clear_stencil(v: i32) {
    glClearStencil(v);
}
unsafe extern "C" fn gl_viewport(x: i32, y: i32, w: i32, h: i32) {
    glViewport(x, y, w, h);
}
unsafe extern "C" fn gl_finish() {
    glFinish();
}
unsafe extern "C" fn gl_flush() {
    glFlush();
}
unsafe extern "C" fn gl_pixel_storei(k: i32, v: i32) {
    glPixelStorei(k as u32, v);
}
unsafe extern "C" fn gl_get_string(name: i32) -> *const u8 {
    glGetString(name as u32)
}
unsafe extern "C" fn gl_polygon_mode(face: i32, mode: i32) {
    glPolygonMode(face as u32, mode as u32);
}
unsafe extern "C" fn gl_polygon_offset(factor: f32, units: f32) {
    glPolygonOffset(factor, units);
}
unsafe extern "C" fn gl_enable(f: i32) {
    glEnable(f as u32);
}
unsafe extern "C" fn gl_disable(f: i32) {
    glDisable(f as u32);
}
unsafe extern "C" fn gl_cull_face(f: i32) {
    glCullFace(f as u32);
}
unsafe extern "C" fn gl_blend_func(s: i32, d: i32) {
    glBlendFunc(s as u32, d as u32);
}
unsafe extern "C" fn gl_blend_func_separate(s: i32, d: i32, sa: i32, da: i32) {
    glBlendFuncSeparate(s as u32, d as u32, sa as u32, da as u32);
}
unsafe extern "C" fn gl_blend_equation(op: i32) {
    glBlendEquation(op as u32);
}
unsafe extern "C" fn gl_blend_equation_separate(op: i32, aop: i32) {
    glBlendEquationSeparate(op as u32, aop as u32);
}
unsafe extern "C" fn gl_depth_mask(m: bool) {
    glDepthMask(m as u8);
}
unsafe extern "C" fn gl_depth_func(f: i32) {
    glDepthFunc(f as u32);
}
unsafe extern "C" fn gl_color_mask(r: bool, g: bool, b: bool, a: bool) {
    glColorMask(r as u8, g as u8, b as u8, a as u8);
}
unsafe extern "C" fn gl_color_maski(i: i32, r: bool, g: bool, b: bool, a: bool) {
    // glColorMaski not available on all macOS GL versions, ignore
}
unsafe extern "C" fn gl_stencil_mask_separate(face: i32, mask: i32) {
    glStencilMaskSeparate(face as u32, mask as u32);
}
unsafe extern "C" fn gl_stencil_func_separate(face: i32, func: i32, ref_: i32, mask: i32) {
    glStencilFuncSeparate(face as u32, func as u32, ref_, mask as u32);
}
unsafe extern "C" fn gl_stencil_op_separate(face: i32, sfail: i32, dpfail: i32, dppass: i32) {
    glStencilOpSeparate(face as u32, sfail as u32, dpfail as u32, dppass as u32);
}

// Programs
unsafe extern "C" fn gl_create_program() -> *mut vdynamic {
    alloc_i32(glCreateProgram() as i32)
}
unsafe extern "C" fn gl_delete_program(p: *mut vdynamic) {
    glDeleteProgram(zidx(p));
}
unsafe extern "C" fn gl_attach_shader(p: *mut vdynamic, s: *mut vdynamic) {
    glAttachShader(zidx(p), zidx(s));
}
unsafe extern "C" fn gl_link_program(p: *mut vdynamic) {
    glLinkProgram(zidx(p));
}
unsafe extern "C" fn gl_get_program_parameter(p: *mut vdynamic, param: i32) -> *mut vdynamic {
    let mut v: i32 = 0;
    glGetProgramiv(zidx(p), param as u32, &mut v);
    alloc_i32(v)
}
unsafe extern "C" fn gl_get_program_info_bytes(p: *mut vdynamic) -> *const u8 {
    let mut len: i32 = 0;
    glGetProgramiv(zidx(p), 0x8B84, &mut len); // GL_INFO_LOG_LENGTH
    if len <= 0 {
        return c"".as_ptr() as *const u8;
    }
    let buf = hl_alloc_bytes(len + 1);
    glGetProgramInfoLog(zidx(p), len + 1, ptr::null_mut(), buf);
    buf
}
unsafe extern "C" fn gl_get_uniform_location(
    p: *mut vdynamic,
    name: *mut vdynamic,
) -> *mut vdynamic {
    if name.is_null() {
        return ptr::null_mut();
    }
    let bytes_ptr = *(name as *const u8).add(8) as *const u16;
    let utf8 = hl_to_utf8(bytes_ptr);
    let loc = glGetUniformLocation(zidx(p), utf8 as *const c_char);
    if loc < 0 {
        return ptr::null_mut();
    }
    alloc_i32(loc)
}
unsafe extern "C" fn gl_get_attrib_location(p: *mut vdynamic, name: *mut vdynamic) -> i32 {
    if name.is_null() {
        return -1;
    }
    let bytes_ptr = *((name as *const u8).add(8) as *const *const u16);
    let utf8 = hl_to_utf8(bytes_ptr);
    glGetAttribLocation(zidx(p), utf8 as *const c_char)
}
unsafe extern "C" fn gl_use_program(p: *mut vdynamic) {
    glUseProgram(zidx(p));
}
unsafe extern "C" fn gl_bind_frag_data_location(p: *mut vdynamic, color: i32, name: *mut vdynamic) {
    if name.is_null() {
        return;
    }
    let bytes_ptr = *((name as *const u8).add(8) as *const *const u16);
    let utf8 = hl_to_utf8(bytes_ptr);
    glBindFragDataLocation(zidx(p), color as u32, utf8 as *const c_char);
}

// Shaders
unsafe extern "C" fn gl_create_shader(type_: i32) -> *mut vdynamic {
    alloc_i32(glCreateShader(type_ as u32) as i32)
}
unsafe extern "C" fn gl_shader_source(s: *mut vdynamic, src: *mut vdynamic) {
    if src.is_null() {
        return;
    }
    // src is a String object: offset 8 = UTF-16 bytes pointer
    let bytes_ptr = *((src as *const u8).add(8) as *const *const u16);
    let utf8 = hl_to_utf8(bytes_ptr);
    // macOS GLSL version patching: 130/140 → 150
    #[cfg(target_os = "macos")]
    {
        let s_str = std::ffi::CStr::from_ptr(utf8 as *const c_char);
        if let Ok(text) = s_str.to_str() {
            if text.starts_with("#version 130") || text.starts_with("#version 140") {
                let patched = text.replacen("#version 130", "#version 150", 1).replacen(
                    "#version 140",
                    "#version 150",
                    1,
                );
                let c = CString::new(patched).unwrap();
                let p = c.as_ptr();
                glShaderSource(zidx(s), 1, &p, ptr::null());
                return;
            }
        }
    }
    let p = utf8 as *const c_char;
    glShaderSource(zidx(s), 1, &p, ptr::null());
}
unsafe extern "C" fn gl_compile_shader(s: *mut vdynamic) {
    glCompileShader(zidx(s));
}
unsafe extern "C" fn gl_get_shader_parameter(s: *mut vdynamic, param: i32) -> *mut vdynamic {
    let mut v: i32 = 0;
    glGetShaderiv(zidx(s), param as u32, &mut v);
    alloc_i32(v)
}
unsafe extern "C" fn gl_get_shader_info_bytes(s: *mut vdynamic) -> *const u8 {
    let mut len: i32 = 0;
    glGetShaderiv(zidx(s), 0x8B84, &mut len); // GL_INFO_LOG_LENGTH
    if len <= 0 {
        return c"".as_ptr() as *const u8;
    }
    let buf = hl_alloc_bytes(len + 1);
    glGetShaderInfoLog(zidx(s), len + 1, ptr::null_mut(), buf);
    buf
}
unsafe extern "C" fn gl_delete_shader(s: *mut vdynamic) {
    glDeleteShader(zidx(s));
}

// Textures
unsafe extern "C" fn gl_create_texture() -> *mut vdynamic {
    let mut id: u32 = 0;
    glGenTextures(1, &mut id);
    alloc_i32(id as i32)
}
unsafe extern "C" fn gl_active_texture(t: i32) {
    glActiveTexture(t as u32);
}
unsafe extern "C" fn gl_bind_texture(target: i32, t: *mut vdynamic) {
    glBindTexture(target as u32, zidx(t));
}
unsafe extern "C" fn gl_tex_parameteri(target: i32, pname: i32, param: i32) {
    glTexParameteri(target as u32, pname as u32, param);
}
unsafe extern "C" fn gl_tex_parameterf(target: i32, pname: i32, param: f32) {
    glTexParameterf(target as u32, pname as u32, param);
}
unsafe extern "C" fn gl_tex_image2d(
    target: i32,
    level: i32,
    ifmt: i32,
    w: i32,
    h: i32,
    border: i32,
    fmt: i32,
    type_: i32,
    data: *const u8,
) {
    glTexImage2D(
        target as u32,
        level,
        ifmt,
        w,
        h,
        border,
        fmt as u32,
        type_ as u32,
        data,
    );
}
unsafe extern "C" fn gl_tex_image3d(
    target: i32,
    level: i32,
    ifmt: i32,
    w: i32,
    h: i32,
    d: i32,
    border: i32,
    fmt: i32,
    type_: i32,
    data: *const u8,
) {
    glTexImage3D(
        target as u32,
        level,
        ifmt,
        w,
        h,
        d,
        border,
        fmt as u32,
        type_ as u32,
        data,
    );
}
unsafe extern "C" fn gl_tex_storage2d(target: i32, levels: i32, ifmt: i32, w: i32, h: i32) {
    glTexStorage2D(target as u32, levels, ifmt as u32, w, h);
}
unsafe extern "C" fn gl_tex_storage3d(target: i32, levels: i32, ifmt: i32, w: i32, h: i32, d: i32) {
    glTexStorage3D(target as u32, levels, ifmt as u32, w, h, d);
}
unsafe extern "C" fn gl_tex_sub_image2d(
    target: i32,
    level: i32,
    x: i32,
    y: i32,
    w: i32,
    h: i32,
    fmt: i32,
    type_: i32,
    data: *const u8,
) {
    glTexSubImage2D(
        target as u32,
        level,
        x,
        y,
        w,
        h,
        fmt as u32,
        type_ as u32,
        data,
    );
}
unsafe extern "C" fn gl_tex_sub_image3d(
    target: i32,
    level: i32,
    x: i32,
    y: i32,
    z: i32,
    w: i32,
    h: i32,
    d: i32,
    fmt: i32,
    type_: i32,
    data: *const u8,
) {
    glTexSubImage3D(
        target as u32,
        level,
        x,
        y,
        z,
        w,
        h,
        d,
        fmt as u32,
        type_ as u32,
        data,
    );
}
unsafe extern "C" fn gl_compressed_tex_image2d(
    target: i32,
    level: i32,
    ifmt: i32,
    w: i32,
    h: i32,
    border: i32,
    size: i32,
    data: *const u8,
) {
    glCompressedTexImage2D(target as u32, level, ifmt as u32, w, h, border, size, data);
}
unsafe extern "C" fn gl_compressed_tex_image3d(
    target: i32,
    level: i32,
    ifmt: i32,
    w: i32,
    h: i32,
    d: i32,
    border: i32,
    size: i32,
    data: *const u8,
) {
    glCompressedTexImage3D(
        target as u32,
        level,
        ifmt as u32,
        w,
        h,
        d,
        border,
        size,
        data,
    );
}
unsafe extern "C" fn gl_compressed_tex_sub_image2d(
    target: i32,
    level: i32,
    x: i32,
    y: i32,
    w: i32,
    h: i32,
    fmt: i32,
    size: i32,
    data: *const u8,
) {
    glCompressedTexSubImage2D(target as u32, level, x, y, w, h, fmt as u32, size, data);
}
unsafe extern "C" fn gl_compressed_tex_sub_image3d(
    target: i32,
    level: i32,
    x: i32,
    y: i32,
    z: i32,
    w: i32,
    h: i32,
    d: i32,
    fmt: i32,
    size: i32,
    data: *const u8,
) {
    glCompressedTexSubImage3D(
        target as u32,
        level,
        x,
        y,
        z,
        w,
        h,
        d,
        fmt as u32,
        size,
        data,
    );
}
unsafe extern "C" fn gl_generate_mipmap(target: i32) {
    glGenerateMipmap(target as u32);
}
unsafe extern "C" fn gl_delete_texture(t: *mut vdynamic) {
    let id = zidx(t);
    glDeleteTextures(1, &id);
}
unsafe extern "C" fn gl_tex_image2d_multisample(
    _target: i32,
    _samples: i32,
    _ifmt: i32,
    _w: i32,
    _h: i32,
    _fixed: bool,
) {
}

// Framebuffers
unsafe extern "C" fn gl_create_framebuffer() -> *mut vdynamic {
    let mut id: u32 = 0;
    glGenFramebuffers(1, &mut id);
    alloc_i32(id as i32)
}
unsafe extern "C" fn gl_bind_framebuffer(target: i32, fb: *mut vdynamic) {
    glBindFramebuffer(target as u32, zidx(fb));
}
unsafe extern "C" fn gl_framebuffer_texture(target: i32, att: i32, tex: *mut vdynamic, level: i32) {
    glFramebufferTexture(target as u32, att as u32, zidx(tex), level);
}
unsafe extern "C" fn gl_framebuffer_texture2d(
    target: i32,
    att: i32,
    textarget: i32,
    tex: *mut vdynamic,
    level: i32,
) {
    glFramebufferTexture2D(
        target as u32,
        att as u32,
        textarget as u32,
        zidx(tex),
        level,
    );
}
unsafe extern "C" fn gl_framebuffer_texture_layer(
    target: i32,
    att: i32,
    tex: *mut vdynamic,
    level: i32,
    layer: i32,
) {
    glFramebufferTextureLayer(target as u32, att as u32, zidx(tex), level, layer);
}
unsafe extern "C" fn gl_delete_framebuffer(fb: *mut vdynamic) {
    let id = zidx(fb);
    glDeleteFramebuffers(1, &id);
}
unsafe extern "C" fn gl_blit_framebuffer(
    sx0: i32,
    sy0: i32,
    sx1: i32,
    sy1: i32,
    dx0: i32,
    dy0: i32,
    dx1: i32,
    dy1: i32,
    mask: i32,
    filter: i32,
) {
    glBlitFramebuffer(
        sx0,
        sy0,
        sx1,
        sy1,
        dx0,
        dy0,
        dx1,
        dy1,
        mask as u32,
        filter as u32,
    );
}
unsafe extern "C" fn gl_read_pixels(
    x: i32,
    y: i32,
    w: i32,
    h: i32,
    fmt: i32,
    type_: i32,
    data: *mut u8,
) {
    glReadPixels(x, y, w, h, fmt as u32, type_ as u32, data);
}
unsafe extern "C" fn gl_read_buffer(src: i32) {
    glReadBuffer(src as u32);
}
unsafe extern "C" fn gl_draw_buffers(n: i32, bufs: *const u8) {
    glDrawBuffers(n, bufs as *const u32);
}

// Renderbuffers
unsafe extern "C" fn gl_create_renderbuffer() -> *mut vdynamic {
    let mut id: u32 = 0;
    glGenRenderbuffers(1, &mut id);
    alloc_i32(id as i32)
}
unsafe extern "C" fn gl_bind_renderbuffer(target: i32, rb: *mut vdynamic) {
    glBindRenderbuffer(target as u32, zidx(rb));
}
unsafe extern "C" fn gl_renderbuffer_storage(target: i32, ifmt: i32, w: i32, h: i32) {
    glRenderbufferStorage(target as u32, ifmt as u32, w, h);
}
unsafe extern "C" fn gl_renderbuffer_storage_multisample(
    target: i32,
    samples: i32,
    ifmt: i32,
    w: i32,
    h: i32,
) {
    glRenderbufferStorageMultisample(target as u32, samples, ifmt as u32, w, h);
}
unsafe extern "C" fn gl_framebuffer_renderbuffer(
    target: i32,
    att: i32,
    rbtarget: i32,
    rb: *mut vdynamic,
) {
    glFramebufferRenderbuffer(target as u32, att as u32, rbtarget as u32, zidx(rb));
}
unsafe extern "C" fn gl_delete_renderbuffer(rb: *mut vdynamic) {
    let id = zidx(rb);
    glDeleteRenderbuffers(1, &id);
}

// Buffers
unsafe extern "C" fn gl_create_buffer() -> *mut vdynamic {
    let mut id: u32 = 0;
    glGenBuffers(1, &mut id);
    alloc_i32(id as i32)
}
unsafe extern "C" fn gl_bind_buffer(target: i32, buf: *mut vdynamic) {
    glBindBuffer(target as u32, zidx(buf));
}
unsafe extern "C" fn gl_bind_buffer_base(target: i32, index: i32, buf: *mut vdynamic) {
    glBindBufferBase(target as u32, index as u32, zidx(buf));
}
unsafe extern "C" fn gl_buffer_data_size(target: i32, size: i32, usage: i32) {
    glBufferData(target as u32, size as isize, ptr::null(), usage as u32);
}
unsafe extern "C" fn gl_buffer_data(target: i32, size: i32, data: *const u8, usage: i32) {
    glBufferData(target as u32, size as isize, data, usage as u32);
}
unsafe extern "C" fn gl_buffer_sub_data(
    target: i32,
    offset: i32,
    data: *const u8,
    src_pos: i32,
    len: i32,
) {
    glBufferSubData(
        target as u32,
        offset as isize,
        len as isize,
        data.add(src_pos as usize),
    );
}
unsafe extern "C" fn gl_enable_vertex_attrib_array(i: i32) {
    glEnableVertexAttribArray(i as u32);
}
unsafe extern "C" fn gl_disable_vertex_attrib_array(i: i32) {
    glDisableVertexAttribArray(i as u32);
}
unsafe extern "C" fn gl_vertex_attrib_pointer(
    i: i32,
    size: i32,
    type_: i32,
    normalized: bool,
    stride: i32,
    offset: i32,
) {
    glVertexAttribPointer(
        i as u32,
        size,
        type_ as u32,
        normalized as u8,
        stride,
        offset as usize as *const c_void,
    );
}
unsafe extern "C" fn gl_vertex_attrib_ipointer(
    i: i32,
    size: i32,
    type_: i32,
    stride: i32,
    offset: i32,
) {
    glVertexAttribIPointer(
        i as u32,
        size,
        type_ as u32,
        stride,
        offset as usize as *const c_void,
    );
}
unsafe extern "C" fn gl_delete_buffer(buf: *mut vdynamic) {
    let id = zidx(buf);
    glDeleteBuffers(1, &id);
}

// Uniforms
unsafe extern "C" fn gl_uniform1i(loc: *mut vdynamic, v: i32) {
    glUniform1i(zidx(loc) as i32, v);
}
unsafe extern "C" fn gl_uniform4fv(loc: *mut vdynamic, data: *const u8, count: i32, _stride: i32) {
    glUniform4fv(zidx(loc) as i32, count, data as *const f32);
}
unsafe extern "C" fn gl_uniform_matrix4fv(
    loc: *mut vdynamic,
    transpose: bool,
    data: *const u8,
    count: i32,
    _stride: i32,
) {
    glUniformMatrix4fv(zidx(loc) as i32, count, transpose as u8, data as *const f32);
}

// Draw
unsafe extern "C" fn gl_draw_elements(mode: i32, count: i32, type_: i32, offset: i32) {
    glDrawElements(
        mode as u32,
        count,
        type_ as u32,
        offset as usize as *const c_void,
    );
}
unsafe extern "C" fn gl_draw_elements_instanced(
    mode: i32,
    count: i32,
    type_: i32,
    offset: i32,
    instances: i32,
) {
    glDrawElementsInstanced(
        mode as u32,
        count,
        type_ as u32,
        offset as usize as *const c_void,
        instances,
    );
}
unsafe extern "C" fn gl_draw_arrays(mode: i32, first: i32, count: i32) {
    glDrawArrays(mode as u32, first, count);
}
unsafe extern "C" fn gl_draw_arrays_instanced(mode: i32, first: i32, count: i32, instances: i32) {
    glDrawArraysInstanced(mode as u32, first, count, instances);
}
unsafe extern "C" fn gl_multi_draw_elements_indirect(
    _mode: i32,
    _type_: i32,
    _indirect: *const u8,
    _count: i32,
    _stride: i32,
) {
}
unsafe extern "C" fn gl_multi_draw_elements_indirect_count(
    _mode: i32,
    _type_: i32,
    _indirect: *const u8,
    _count: *const u8,
    _max: i32,
    _stride: i32,
) {
}

// VAO
unsafe extern "C" fn gl_create_vertex_array() -> *mut vdynamic {
    let mut id: u32 = 0;
    glGenVertexArrays(1, &mut id);
    alloc_i32(id as i32)
}
unsafe extern "C" fn gl_bind_vertex_array(vao: *mut vdynamic) {
    glBindVertexArray(zidx(vao));
}
unsafe extern "C" fn gl_delete_vertex_array(vao: *mut vdynamic) {
    let id = zidx(vao);
    glDeleteVertexArrays(1, &id);
}
unsafe extern "C" fn gl_vertex_attrib_divisor(index: i32, divisor: i32) {
    glVertexAttribDivisor(index as u32, divisor as u32);
}

// Queries
unsafe extern "C" fn gl_create_query() -> *mut vdynamic {
    let mut id: u32 = 0;
    glGenQueries(1, &mut id);
    alloc_i32(id as i32)
}
unsafe extern "C" fn gl_delete_query(q: *mut vdynamic) {
    let id = zidx(q);
    glDeleteQueries(1, &id);
}
unsafe extern "C" fn gl_begin_query(target: i32, q: *mut vdynamic) {
    glBeginQuery(target as u32, zidx(q));
}
unsafe extern "C" fn gl_end_query(target: i32) {
    glEndQuery(target as u32);
}
unsafe extern "C" fn gl_query_result_available(q: *mut vdynamic) -> bool {
    let mut v: u32 = 0;
    glGetQueryObjectuiv(zidx(q), 0x8867, &mut v);
    v != 0
}
unsafe extern "C" fn gl_query_counter(_q: *mut vdynamic, _target: i32) {}
unsafe extern "C" fn gl_query_result(q: *mut vdynamic) -> f64 {
    let mut v: u32 = 0;
    glGetQueryObjectuiv(zidx(q), 0x8866, &mut v);
    v as f64
}

// Misc
unsafe extern "C" fn gl_bind_image_texture(
    _unit: i32,
    _tex: i32,
    _level: i32,
    _layered: bool,
    _layer: i32,
    _access: i32,
    _fmt: i32,
) {
}
unsafe extern "C" fn gl_dispatch_compute(_x: i32, _y: i32, _z: i32) {}
unsafe extern "C" fn gl_memory_barrier(_barriers: i32) {}
unsafe extern "C" fn gl_get_config_parameter(param: i32) -> i32 {
    let mut v: i32 = 0;
    SDL_GL_GetAttribute(param, &mut v);
    v
}
unsafe extern "C" fn gl_has_extension(name: *mut vdynamic) -> bool {
    if name.is_null() {
        return false;
    }
    let bytes_ptr = *((name as *const u8).add(8) as *const *const u16);
    let utf8 = hl_to_utf8(bytes_ptr);
    let target = std::ffi::CStr::from_ptr(utf8 as *const c_char);
    let mut num: i32 = 0;
    glGetIntegerv(0x821D, &mut num); // GL_NUM_EXTENSIONS
    for i in 0..num as u32 {
        let ext = glGetStringi(0x1F03, i); // GL_EXTENSIONS
        if !ext.is_null() {
            let ext_str = std::ffi::CStr::from_ptr(ext as *const c_char);
            if ext_str == target {
                return true;
            }
        }
    }
    false
}

unsafe extern "C" fn gl_get_uniform_block_index(p: *mut vdynamic, name: *mut vdynamic) -> i32 {
    if name.is_null() {
        return -1;
    }
    let bytes_ptr = *((name as *const u8).add(8) as *const *const u16);
    let utf8 = hl_to_utf8(bytes_ptr);
    glGetUniformBlockIndex(zidx(p), utf8 as *const c_char) as i32
}
unsafe extern "C" fn gl_uniform_block_binding(p: *mut vdynamic, index: i32, binding: i32) {
    glUniformBlockBinding(zidx(p), index as u32, binding as u32);
}
unsafe extern "C" fn gl_get_program_resource_index(
    _p: *mut vdynamic,
    _iface: i32,
    _name: *mut vdynamic,
) -> i32 {
    -1
}
unsafe extern "C" fn gl_shader_storage_block_binding(
    _p: *mut vdynamic,
    _index: i32,
    _binding: i32,
) {
}

// ============================================================================
// DEFINE_PRIM resolvers for GL functions
// ============================================================================

define_prim!(hlp_gl_init, "P_b", gl_init);
define_prim!(hlp_gl_is_context_lost, "P_b", gl_is_context_lost);
define_prim!(hlp_gl_clear, "Pi_v", gl_clear);
define_prim!(hlp_gl_get_error, "P_i", gl_get_error);
define_prim!(hlp_gl_scissor, "Piiii_v", gl_scissor);
define_prim!(hlp_gl_clear_color, "Pdddd_v", gl_clear_color);
define_prim!(hlp_gl_clear_depth, "Pd_v", gl_clear_depth);
define_prim!(hlp_gl_clear_stencil, "Pi_v", gl_clear_stencil);
define_prim!(hlp_gl_viewport, "Piiii_v", gl_viewport);
define_prim!(hlp_gl_finish, "P_v", gl_finish);
define_prim!(hlp_gl_flush, "P_v", gl_flush);
define_prim!(hlp_gl_pixel_storei, "Pii_v", gl_pixel_storei);
define_prim!(hlp_gl_get_string, "Pi_B", gl_get_string);
define_prim!(hlp_gl_polygon_mode, "Pii_v", gl_polygon_mode);
define_prim!(hlp_gl_polygon_offset, "Pff_v", gl_polygon_offset);
define_prim!(hlp_gl_enable, "Pi_v", gl_enable);
define_prim!(hlp_gl_disable, "Pi_v", gl_disable);
define_prim!(hlp_gl_cull_face, "Pi_v", gl_cull_face);
define_prim!(hlp_gl_blend_func, "Pii_v", gl_blend_func);
define_prim!(
    hlp_gl_blend_func_separate,
    "Piiii_v",
    gl_blend_func_separate
);
define_prim!(hlp_gl_blend_equation, "Pi_v", gl_blend_equation);
define_prim!(
    hlp_gl_blend_equation_separate,
    "Pii_v",
    gl_blend_equation_separate
);
define_prim!(hlp_gl_depth_mask, "Pb_v", gl_depth_mask);
define_prim!(hlp_gl_depth_func, "Pi_v", gl_depth_func);
define_prim!(hlp_gl_color_mask, "Pbbbb_v", gl_color_mask);
define_prim!(hlp_gl_color_maski, "Pibbbb_v", gl_color_maski);
define_prim!(
    hlp_gl_stencil_mask_separate,
    "Pii_v",
    gl_stencil_mask_separate
);
define_prim!(
    hlp_gl_stencil_func_separate,
    "Piiii_v",
    gl_stencil_func_separate
);
define_prim!(
    hlp_gl_stencil_op_separate,
    "Piiii_v",
    gl_stencil_op_separate
);
define_prim!(hlp_gl_create_program, "P_Ni", gl_create_program);
define_prim!(hlp_gl_delete_program, "PNi_v", gl_delete_program);
define_prim!(
    hlp_gl_bind_frag_data_location,
    "PNiiOBi__v",
    gl_bind_frag_data_location
);
define_prim!(hlp_gl_attach_shader, "PNiNi_v", gl_attach_shader);
define_prim!(hlp_gl_link_program, "PNi_v", gl_link_program);
define_prim!(
    hlp_gl_get_program_parameter,
    "PNii_D",
    gl_get_program_parameter
);
define_prim!(
    hlp_gl_get_program_info_bytes,
    "PNi_B",
    gl_get_program_info_bytes
);
define_prim!(
    hlp_gl_get_uniform_location,
    "PNiOBi__Ni",
    gl_get_uniform_location
);
define_prim!(
    hlp_gl_get_attrib_location,
    "PNiOBi__i",
    gl_get_attrib_location
);
define_prim!(hlp_gl_use_program, "PNi_v", gl_use_program);
define_prim!(hlp_gl_create_shader, "Pi_Ni", gl_create_shader);
define_prim!(hlp_gl_shader_source, "PNiOBi__v", gl_shader_source);
define_prim!(hlp_gl_compile_shader, "PNi_v", gl_compile_shader);
define_prim!(
    hlp_gl_get_shader_info_bytes,
    "PNi_B",
    gl_get_shader_info_bytes
);
define_prim!(
    hlp_gl_get_shader_parameter,
    "PNii_D",
    gl_get_shader_parameter
);
define_prim!(hlp_gl_delete_shader, "PNi_v", gl_delete_shader);
define_prim!(hlp_gl_create_texture, "P_Ni", gl_create_texture);
define_prim!(hlp_gl_active_texture, "Pi_v", gl_active_texture);
define_prim!(hlp_gl_bind_texture, "PiNi_v", gl_bind_texture);
define_prim!(hlp_gl_tex_parameteri, "Piii_v", gl_tex_parameteri);
define_prim!(hlp_gl_tex_parameterf, "Piif_v", gl_tex_parameterf);
define_prim!(hlp_gl_tex_image2d, "PiiiiiiiB_v", gl_tex_image2d);
define_prim!(hlp_gl_tex_image3d, "PiiiiiiiiB_v", gl_tex_image3d);
define_prim!(hlp_gl_tex_storage2d, "Piiii_v", gl_tex_storage2d);
define_prim!(hlp_gl_tex_storage3d, "Piiiii_v", gl_tex_storage3d);
define_prim!(
    hlp_gl_tex_image2d_multisample,
    "Piiiib_v",
    gl_tex_image2d_multisample
);
define_prim!(
    hlp_gl_compressed_tex_image2d,
    "PiiiiiiB_v",
    gl_compressed_tex_image2d
);
define_prim!(
    hlp_gl_compressed_tex_image3d,
    "PiiiiiiiB_v",
    gl_compressed_tex_image3d
);
define_prim!(hlp_gl_tex_sub_image2d, "PiiiiiiiiB_v", gl_tex_sub_image2d);
define_prim!(hlp_gl_tex_sub_image3d, "PiiiiiiiiiB_v", gl_tex_sub_image3d);
define_prim!(
    hlp_gl_compressed_tex_sub_image2d,
    "PiiiiiiiiB_v",
    gl_compressed_tex_sub_image2d
);
define_prim!(
    hlp_gl_compressed_tex_sub_image3d,
    "PiiiiiiiiiiB_v",
    gl_compressed_tex_sub_image3d
);
define_prim!(hlp_gl_generate_mipmap, "Pi_v", gl_generate_mipmap);
define_prim!(hlp_gl_delete_texture, "PNi_v", gl_delete_texture);
define_prim!(
    hlp_gl_blit_framebuffer,
    "Piiiiiiiiii_v",
    gl_blit_framebuffer
);
define_prim!(hlp_gl_create_framebuffer, "P_Ni", gl_create_framebuffer);
define_prim!(hlp_gl_bind_framebuffer, "PiNi_v", gl_bind_framebuffer);
define_prim!(
    hlp_gl_framebuffer_texture,
    "PiiNii_v",
    gl_framebuffer_texture
);
define_prim!(
    hlp_gl_framebuffer_texture2d,
    "PiiiNii_v",
    gl_framebuffer_texture2d
);
define_prim!(
    hlp_gl_framebuffer_texture_layer,
    "PiiNiii_v",
    gl_framebuffer_texture_layer
);
define_prim!(hlp_gl_delete_framebuffer, "PNi_v", gl_delete_framebuffer);
define_prim!(hlp_gl_read_pixels, "PiiiiiiB_v", gl_read_pixels);
define_prim!(hlp_gl_read_buffer, "Pi_v", gl_read_buffer);
define_prim!(hlp_gl_draw_buffers, "PiB_v", gl_draw_buffers);
define_prim!(hlp_gl_create_renderbuffer, "P_Ni", gl_create_renderbuffer);
define_prim!(hlp_gl_bind_renderbuffer, "PiNi_v", gl_bind_renderbuffer);
define_prim!(
    hlp_gl_renderbuffer_storage,
    "Piiii_v",
    gl_renderbuffer_storage
);
define_prim!(
    hlp_gl_renderbuffer_storage_multisample,
    "Piiiii_v",
    gl_renderbuffer_storage_multisample
);
define_prim!(
    hlp_gl_framebuffer_renderbuffer,
    "PiiiNi_v",
    gl_framebuffer_renderbuffer
);
define_prim!(hlp_gl_delete_renderbuffer, "PNi_v", gl_delete_renderbuffer);
define_prim!(hlp_gl_create_buffer, "P_Ni", gl_create_buffer);
define_prim!(hlp_gl_bind_buffer, "PiNi_v", gl_bind_buffer);
define_prim!(hlp_gl_bind_buffer_base, "PiiNi_v", gl_bind_buffer_base);
define_prim!(hlp_gl_buffer_data_size, "Piii_v", gl_buffer_data_size);
define_prim!(hlp_gl_buffer_data, "PiiBi_v", gl_buffer_data);
define_prim!(hlp_gl_buffer_sub_data, "PiiBii_v", gl_buffer_sub_data);
define_prim!(
    hlp_gl_enable_vertex_attrib_array,
    "Pi_v",
    gl_enable_vertex_attrib_array
);
define_prim!(
    hlp_gl_disable_vertex_attrib_array,
    "Pi_v",
    gl_disable_vertex_attrib_array
);
define_prim!(
    hlp_gl_vertex_attrib_pointer,
    "Piiibi_v",
    gl_vertex_attrib_pointer
);
define_prim!(
    hlp_gl_vertex_attrib_ipointer,
    "Piiii_v",
    gl_vertex_attrib_ipointer
);
define_prim!(hlp_gl_delete_buffer, "PNi_v", gl_delete_buffer);
define_prim!(hlp_gl_uniform1i, "PNii_v", gl_uniform1i);
define_prim!(hlp_gl_uniform4fv, "PNiBii_v", gl_uniform4fv);
define_prim!(hlp_gl_uniform_matrix4fv, "PNibBii_v", gl_uniform_matrix4fv);
define_prim!(
    hlp_gl_bind_image_texture,
    "Piiibiii_v",
    gl_bind_image_texture
);
define_prim!(hlp_gl_dispatch_compute, "Piii_v", gl_dispatch_compute);
define_prim!(hlp_gl_memory_barrier, "Pi_v", gl_memory_barrier);
define_prim!(hlp_gl_draw_elements, "Piiii_v", gl_draw_elements);
define_prim!(
    hlp_gl_draw_elements_instanced,
    "Piiiii_v",
    gl_draw_elements_instanced
);
define_prim!(hlp_gl_draw_arrays, "Piii_v", gl_draw_arrays);
define_prim!(
    hlp_gl_draw_arrays_instanced,
    "Piiii_v",
    gl_draw_arrays_instanced
);
define_prim!(
    hlp_gl_multi_draw_elements_indirect,
    "PiiBii_v",
    gl_multi_draw_elements_indirect
);
define_prim!(
    hlp_gl_multi_draw_elements_indirect_count,
    "PiiBBii_v",
    gl_multi_draw_elements_indirect_count
);
define_prim!(hlp_gl_create_vertex_array, "P_Ni", gl_create_vertex_array);
define_prim!(hlp_gl_bind_vertex_array, "PNi_v", gl_bind_vertex_array);
define_prim!(hlp_gl_delete_vertex_array, "PNi_v", gl_delete_vertex_array);
define_prim!(
    hlp_gl_vertex_attrib_divisor,
    "Pii_v",
    gl_vertex_attrib_divisor
);
define_prim!(hlp_gl_create_query, "P_Ni", gl_create_query);
define_prim!(hlp_gl_delete_query, "PNi_v", gl_delete_query);
define_prim!(hlp_gl_begin_query, "PiNi_v", gl_begin_query);
define_prim!(hlp_gl_end_query, "Pi_v", gl_end_query);
define_prim!(
    hlp_gl_query_result_available,
    "PNi_b",
    gl_query_result_available
);
define_prim!(hlp_gl_query_counter, "PNii_v", gl_query_counter);
define_prim!(hlp_gl_query_result, "PNi_d", gl_query_result);
define_prim!(
    hlp_gl_get_uniform_block_index,
    "PNiOBi__i",
    gl_get_uniform_block_index
);
define_prim!(
    hlp_gl_uniform_block_binding,
    "PNiii_v",
    gl_uniform_block_binding
);
define_prim!(
    hlp_gl_get_program_resource_index,
    "PNiiOBi__i",
    gl_get_program_resource_index
);
define_prim!(
    hlp_gl_shader_storage_block_binding,
    "PNiii_v",
    gl_shader_storage_block_binding
);
define_prim!(hlp_gl_get_config_parameter, "Pi_i", gl_get_config_parameter);
define_prim!(hlp_gl_has_extension, "POBi__b", gl_has_extension);
