#!/usr/bin/env python3
"""Build the prelinked wasm runtime object that `ash --build --target wasm32-*` links.

`ash_runtime.o` is `ash_std` compiled for wasm32-wasip1, joined once with a
wasi libc and `libsetjmp` into a single relocatable object. ash's own linker
then links a program against exactly that one object, so a Haxe developer
never needs a wasi-sdk on their machine -- but whoever builds ash does, and
nothing in the cargo build produced the object. It went stale silently: a
runtime function added to `ash_std` was missing from the object until someone
noticed an `unknown import` at instantiate time.

    scripts/build_wasm_runtime.py                 # release, sysroot found
    scripts/build_wasm_runtime.py --sysroot ~/wasi-sysroot
    scripts/build_wasm_runtime.py --profile debug

The output lands where `ash` looks for it: `target/<profile>/wasm32-wasip1/
ash_runtime.o`, beside the `ash` binary of the same profile.

`--no-whole-archive` after the archive is load-bearing. Without it libc's
crt1 and its long-double printf are force-included, and the module then
imports `__main_argc_argv` and `__multc3`, which nothing provides.
"""

import argparse
import os
import pathlib
import shutil
import subprocess
import sys

TRIPLE = "wasm32-wasip1"
REPO = pathlib.Path(__file__).resolve().parent.parent


def sh(cmd, **kw):
    print("+", " ".join(str(c) for c in cmd), flush=True)
    return subprocess.run(cmd, check=True, text=True, **kw)


def rustc_info(key: str) -> str:
    out = subprocess.run(["rustc", "-vV"], check=True, capture_output=True, text=True).stdout
    for line in out.splitlines():
        if line.startswith(key + ":"):
            return line.split(":", 1)[1].strip()
    sys.exit(f"rustc -vV printed no '{key}:' line")


def find_lld() -> pathlib.Path:
    """`rust-lld` ships with every Rust toolchain, under the host's rustlib."""
    sysroot = pathlib.Path(
        subprocess.run(["rustc", "--print", "sysroot"], check=True,
                       capture_output=True, text=True).stdout.strip())
    host = rustc_info("host")
    for candidate in [
        sysroot / "lib" / "rustlib" / host / "bin" / "rust-lld",
        sysroot / "lib" / "rustlib" / host / "bin" / "rust-lld.exe",
    ]:
        if candidate.is_file():
            return candidate
    found = shutil.which("rust-lld") or shutil.which("wasm-ld")
    if found:
        return pathlib.Path(found)
    sys.exit(f"no rust-lld under {sysroot} and none on PATH")


def find_sysroot(explicit: str | None) -> pathlib.Path:
    """A WASI sysroot holding lib/wasm32-wasip1/{libc.a,libsetjmp.a}."""
    candidates: list[pathlib.Path] = []
    if explicit:
        candidates.append(pathlib.Path(explicit))
    if os.environ.get("WASI_SYSROOT"):
        candidates.append(pathlib.Path(os.environ["WASI_SYSROOT"]))
    brew = shutil.which("brew")
    if brew:
        r = subprocess.run([brew, "--prefix", "wasi-libc"], capture_output=True, text=True)
        if r.returncode == 0:
            candidates.append(pathlib.Path(r.stdout.strip()) / "share" / "wasi-sysroot")
    candidates += [
        pathlib.Path("/opt/wasi-sdk/share/wasi-sysroot"),
        pathlib.Path("/usr/local/wasi-sdk/share/wasi-sysroot"),
        pathlib.Path("/usr/share/wasi-sysroot"),
    ]
    for c in candidates:
        lib = c / "lib" / TRIPLE
        if (lib / "libc.a").is_file():
            if not (lib / "libsetjmp.a").is_file():
                sys.exit(f"{lib} has libc.a but no libsetjmp.a; ash's exceptions are setjmp "
                         "and need wasi-libc's libsetjmp (wasi-sdk 22 or newer)")
            return c
    sys.exit("no WASI sysroot found. Pass --sysroot, set WASI_SYSROOT, install "
             "wasi-libc (brew) or unpack a wasi-sdk release at /opt/wasi-sdk. "
             f"Looked in: {', '.join(str(c) for c in candidates)}")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--sysroot", default=None, help="WASI sysroot directory")
    ap.add_argument("--profile", choices=["release", "debug"], default="release")
    ap.add_argument("--out", type=pathlib.Path, default=None,
                    help=f"where to write the object (default target/<profile>/{TRIPLE}/ash_runtime.o)")
    ap.add_argument("--cargo", default=os.environ.get("CARGO", "cargo"))
    args = ap.parse_args()

    sysroot = find_sysroot(args.sysroot)
    lld = find_lld()
    print(f"sysroot: {sysroot}\nlinker:  {lld}")

    cargo_cmd = [args.cargo, "rustc", "-p", "ash_std", "--target", TRIPLE,
                 "--crate-type", "staticlib"]
    if args.profile == "release":
        cargo_cmd.append("--release")
    # std/build.rs runs bindgen against the WASI libc headers and reads the
    # sysroot from this variable; the linker below needs the same directory's
    # libraries. One discovery, handed to both.
    env = dict(os.environ, WASI_SYSROOT=str(sysroot))
    sh(cargo_cmd, cwd=REPO, env=env)

    archive = REPO / "target" / TRIPLE / args.profile / "libash_std.a"
    if not archive.is_file():
        sys.exit(f"cargo produced no {archive}")
    out = args.out or (REPO / "target" / args.profile / TRIPLE / "ash_runtime.o")
    out.parent.mkdir(parents=True, exist_ok=True)
    sh([str(lld), "-flavor", "wasm", "-r", "-o", str(out),
        "--whole-archive", str(archive), "--no-whole-archive",
        f"-L{sysroot / 'lib' / TRIPLE}", "-lc", "-lsetjmp"])
    print(f"wrote {out} ({out.stat().st_size} bytes)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
