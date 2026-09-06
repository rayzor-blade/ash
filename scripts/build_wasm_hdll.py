#!/usr/bin/env python3
"""Build a native library as a wasm side module -- an HDLL a program loads.

`docs/wasm-hdlls.md` explains what one is and why it is a `dylink.0` side
module rather than a component. This builds the ones that ship with ash.

    scripts/build_wasm_hdll.py                    # all of them, release
    scripts/build_wasm_hdll.py --only sqlite

Output goes where `ash --build` and the conformance harness look for it:
`target/<profile>/wasm32-wasip1/hdll/<lib>.wasm`. Drop one beside a program
and it is loaded; leave it out and the program still builds and still runs,
raising only if a primitive is actually reached.

Two things about the build are not obvious and are not negotiable:

* **`-Z build-std`.** The toolchain ships `core` and `std` compiled without
  position independence, and a side module is position-independent by
  definition, so linking against the shipped ones fails with a page of
  "recompile with -fPIC". They have to be rebuilt, which needs nightly and
  the `rust-src` component.
* **`--whole-archive`.** `-u`, the usual way to force an archive member in,
  does nothing in a `-shared` link: wasm-ld resolves undefined symbols by
  importing them rather than by pulling members. `--gc-sections` then takes
  back out what the exports do not reach.
"""

import argparse
import os
import pathlib
import subprocess
import sys

TRIPLE = "wasm32-wasip1"
REPO = pathlib.Path(__file__).resolve().parent.parent

# The libraries ash ships, and the primitives each exports. The names are the
# Haxe side's: `@:hlNative("sqlite", "connect")` is answered by `hlp_connect`,
# NOT by `hlp_sqlite_connect`.
# What base3d calls, from ASH_TRACE_NATIVES; see examples/heaps_base3d.
SDL_PRIMITIVES = [
    "cursor_create", "cursor_create_system", "delay", "detect_keyboard_layout",
    "detect_win32", "event_loop", "free_cursor", "free_surface",
    "gctrl_close", "gctrl_count", "gctrl_get_axis", "gctrl_get_button",
    "gctrl_get_id", "gctrl_get_name", "gctrl_open", "get_clipboard_text",
    "get_current_display_mode", "get_devices", "get_display_modes", "get_displays",
    "get_drag_and_drop_enabled", "get_framerate", "get_global_mouse_state", "get_relative_mouse_mode",
    "get_screen_height", "get_screen_height_of_window", "get_screen_width", "get_screen_width_of_window",
    "get_window_grab", "gl_active_texture", "gl_attach_shader", "gl_begin_query",
    "gl_bind_buffer", "gl_bind_buffer_base", "gl_bind_frag_data_location", "gl_bind_framebuffer",
    "gl_bind_image_texture", "gl_bind_renderbuffer", "gl_bind_texture", "gl_bind_vertex_array",
    "gl_blend_equation", "gl_blend_equation_separate", "gl_blend_func", "gl_blend_func_separate",
    "gl_blit_framebuffer", "gl_buffer_data", "gl_buffer_data_size", "gl_buffer_sub_data",
    "gl_clear", "gl_clear_color", "gl_clear_depth", "gl_clear_stencil",
    "gl_color_mask", "gl_color_maski", "gl_compile_shader", "gl_compressed_tex_image2d",
    "gl_compressed_tex_image3d", "gl_compressed_tex_sub_image2d", "gl_compressed_tex_sub_image3d", "gl_create_buffer",
    "gl_create_framebuffer", "gl_create_program", "gl_create_query", "gl_create_renderbuffer",
    "gl_create_shader", "gl_create_texture", "gl_create_vertex_array", "gl_cull_face",
    "gl_delete_buffer", "gl_delete_framebuffer", "gl_delete_query", "gl_delete_renderbuffer",
    "gl_delete_shader", "gl_delete_texture", "gl_delete_vertex_array", "gl_depth_func",
    "gl_depth_mask", "gl_disable", "gl_disable_vertex_attrib_array", "gl_dispatch_compute",
    "gl_draw_arrays", "gl_draw_arrays_instanced", "gl_draw_buffers", "gl_draw_elements",
    "gl_draw_elements_instanced", "gl_enable", "gl_enable_vertex_attrib_array", "gl_end_query",
    "gl_finish", "gl_framebuffer_renderbuffer", "gl_framebuffer_texture", "gl_framebuffer_texture2d",
    "gl_framebuffer_texture_layer", "gl_generate_mipmap", "gl_get_attrib_location", "gl_get_config_parameter",
    "gl_get_error", "gl_get_program_info_bytes", "gl_get_program_parameter", "gl_get_program_resource_index",
    "gl_get_shader_info_bytes", "gl_get_shader_parameter", "gl_get_string", "gl_get_uniform_block_index",
    "gl_get_uniform_location", "gl_has_extension", "gl_init", "gl_is_context_lost",
    "gl_link_program", "gl_memory_barrier", "gl_multi_draw_elements_indirect", "gl_multi_draw_elements_indirect_count",
    "gl_options", "gl_pixel_storei", "gl_polygon_mode", "gl_polygon_offset",
    "gl_query_counter", "gl_query_result", "gl_query_result_available", "gl_read_buffer",
    "gl_read_pixels", "gl_renderbuffer_storage", "gl_renderbuffer_storage_multisample", "gl_scissor",
    "gl_shader_source", "gl_shader_storage_block_binding", "gl_stencil_func_separate", "gl_stencil_mask_separate",
    "gl_stencil_op_separate", "gl_tex_image2d", "gl_tex_image2d_multisample", "gl_tex_image3d",
    "gl_tex_parameterf", "gl_tex_parameteri", "gl_tex_storage2d", "gl_tex_storage3d",
    "gl_tex_sub_image2d", "gl_tex_sub_image3d", "gl_uniform1i", "gl_uniform4fv",
    "gl_uniform_block_binding", "gl_uniform_matrix4fv", "gl_use_program", "gl_vertex_attrib_divisor",
    "gl_vertex_attrib_ipointer", "gl_vertex_attrib_pointer", "gl_viewport", "haptic_close",
    "haptic_open", "haptic_rumble_init", "haptic_rumble_play", "hint_value",
    "init_once", "is_cursor_visible", "message_box", "quit",
    "set_clipboard_text", "set_cursor", "set_drag_and_drop_enabled", "set_relative_mouse_mode",
    "set_vsync", "set_window_grab", "show_cursor", "surface_from",
    "warp_mouse_global", "warp_mouse_in_window", "win_create", "win_create_ex",
    "win_destroy", "win_display_handle", "win_get_glcontext", "win_get_id",
    "win_get_max_size", "win_get_min_size", "win_get_opacity", "win_get_position",
    "win_get_size", "win_render_to", "win_resize", "win_set_display_mode",
    "win_set_fullscreen", "win_set_max_size", "win_set_min_size", "win_set_opacity",
    "win_set_position", "win_set_size", "win_set_title", "win_swap_window",
]

HDLLS = {
    # Every primitive the Heaps base3d example reaches. The library is named
    # `sdl` because that is what `@:hlNative("sdl", ...)` asks for and the file
    # stem is the name; the crate that builds it is `tinysdl`, because it is
    # not SDL. See crates/tinysdl and scripts/generate_sdl_shim.py.
    "sdl": {
        "package": "tinysdl",
        "primitives": SDL_PRIMITIVES,
    },
    # Three no-ops and a watchdog, but a Heaps program calls `ui_init` during
    # startup and stops if it is missing. Built from the same archive as sdl:
    # only the exports differ, and --gc-sections drops the rest.
    "ui": {
        "package": "tinysdl",
        "primitives": ["ui_button_new", "ui_choose_file", "ui_dialog", "ui_init", "ui_loop", "ui_sentinel_is_paused", "ui_sentinel_pause", "ui_sentinel_tick", "ui_start_sentinel", "ui_stop_loop", "ui_win_destroy", "ui_winlog_new", "ui_winlog_set_text"],
    },
    "sqlite": {
        "package": "ash_hdll_sqlite",
        "primitives": [
            "connect",
            "close",
            "request",
            "last_id",
            "result_next",
            "result_get",
            "result_get_int",
            "result_get_float",
            "result_get_length",
            "result_get_nfields",
            "result_get_fields",
        ],
    },
}


def sh(cmd, **kw):
    print("+", " ".join(str(c) for c in cmd), flush=True)
    return subprocess.run(cmd, check=True, text=True, **kw)


def main() -> int:
    # Reuse the runtime build's answers to the same two questions.
    sys.path.insert(0, str(REPO / "scripts"))
    from build_wasm_runtime import find_lld, find_sysroot  # noqa: E402

    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--sysroot", default=None, help="WASI sysroot directory")
    ap.add_argument("--profile", choices=["release", "debug"], default="release")
    ap.add_argument("--only", default=None, help="one library, by name")
    ap.add_argument("--cargo", default=os.environ.get("CARGO", "cargo"))
    ap.add_argument("--toolchain", default="+nightly",
                    help="the toolchain that has -Z build-std and rust-src")
    args = ap.parse_args()

    sysroot = find_sysroot(args.sysroot)
    lld = find_lld()
    out_dir = REPO / "target" / args.profile / TRIPLE / "hdll"
    out_dir.mkdir(parents=True, exist_ok=True)

    wanted = [args.only] if args.only else list(HDLLS)
    unknown = [w for w in wanted if w not in HDLLS]
    if unknown:
        sys.exit(f"no such library: {', '.join(unknown)}")

    env = dict(os.environ, WASI_SYSROOT=str(sysroot))
    env["RUSTFLAGS"] = " ".join([
        env.get("RUSTFLAGS", ""),
        "-C relocation-model=pic",
        "-C target-feature=+mutable-globals",
    ]).strip()
    # A C dependency has to be told the same, and told where its headers are.
    env.setdefault("CC_wasm32_wasip1", "clang")
    env.setdefault("CFLAGS_wasm32_wasip1",
                   f"--target=wasm32-wasi --sysroot={sysroot} -fPIC")

    for name in wanted:
        spec = HDLLS[name]
        package = spec["package"]
        cargo = [args.cargo]
        if args.toolchain:
            cargo.append(args.toolchain)
        cargo += ["build", "-p", package, "--target", TRIPLE,
                  "-Z", "build-std=std,panic_abort"]
        if args.profile == "release":
            cargo.append("--release")
        sh(cargo, cwd=REPO, env=env)

        archive = REPO / "target" / TRIPLE / args.profile / f"lib{package}.a"
        if not archive.is_file():
            sys.exit(f"cargo produced no {archive}")

        out = out_dir / f"{name}.wasm"
        exports = [f"--export=hlp_{p}" for p in spec["primitives"]]
        sh([str(lld), "-flavor", "wasm",
            "--experimental-pic", "-shared", "--no-entry", "--gc-sections",
            # Undefined DATA as well as functions: Rust's std wants the
            # address of `errno`, and --import-undefined covers only calls.
            "--unresolved-symbols=import-dynamic",
            *exports,
            "--whole-archive", str(archive), "--no-whole-archive",
            "-o", str(out)])
        print(f"wrote {out} ({out.stat().st_size} bytes)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
