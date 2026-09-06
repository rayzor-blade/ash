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
    "detect_win32", "event_loop", "gl_active_texture", "gl_attach_shader",
    "gl_bind_buffer", "gl_bind_frag_data_location", "gl_bind_framebuffer", "gl_bind_texture",
    "gl_bind_vertex_array", "gl_blend_equation", "gl_buffer_data_size", "gl_buffer_sub_data",
    "gl_clear", "gl_clear_color", "gl_clear_depth", "gl_clear_stencil",
    "gl_color_mask", "gl_compile_shader", "gl_create_buffer", "gl_create_framebuffer",
    "gl_create_program", "gl_create_shader", "gl_create_texture", "gl_create_vertex_array",
    "gl_cull_face", "gl_delete_shader", "gl_depth_func", "gl_depth_mask",
    "gl_disable", "gl_disable_vertex_attrib_array", "gl_draw_elements", "gl_enable",
    "gl_enable_vertex_attrib_array", "gl_finish", "gl_framebuffer_texture2d", "gl_get_attrib_location",
    "gl_get_config_parameter", "gl_get_error", "gl_get_program_parameter", "gl_get_shader_parameter",
    "gl_get_string", "gl_get_uniform_location", "gl_has_extension", "gl_init",
    "gl_is_context_lost", "gl_link_program", "gl_pixel_storei", "gl_polygon_mode",
    "gl_shader_source", "gl_stencil_func_separate", "gl_stencil_op_separate", "gl_tex_image2d",
    "gl_tex_parameteri", "gl_uniform1i", "gl_uniform4fv", "gl_use_program",
    "gl_vertex_attrib_pointer", "gl_viewport", "init_once", "set_vsync",
    "win_create_ex", "win_get_glcontext", "win_get_size", "win_render_to",
    "win_set_fullscreen", "win_set_title", "win_swap_window",
]

HDLLS = {
    # Every primitive the Heaps base3d example reaches. See
    # crates/ash_hdll_sdl and scripts/generate_sdl_shim.py.
    "sdl": {
        "package": "ash_hdll_sdl",
        "primitives": SDL_PRIMITIVES,
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
