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

# libc entry points a native library may use that ash_std itself never calls,
# force-included with `-u` so they are IN the runtime object and can be
# exported to a library that asks for one. `-u` rather than `--whole-archive`
# on libc: whole-archiving pulls crt1 and the long-double printf, whose own
# undefined symbols (`__main_argc_argv`, `__multc3`) nothing provides.
#
# They cost nothing when unused. Nothing references them and no library asks
# for them, so tree shaking drops them from the module -- which is why this
# list can be generous.
LIBRARY_LIBC = [
    "pread", "pwrite", "readv", "writev", "preadv", "pwritev",
    "futimens", "utimensat", "fstatat", "mkdirat", "unlinkat", "renameat",
    "readlinkat", "symlinkat", "faccessat", "fdopendir", "readdir", "closedir",
    "qsort", "bsearch", "strtol", "strtoul", "strtoll", "strtoull",
    "strncmp", "strrchr", "strchr", "strstr", "strcspn", "strspn",
    "localtime_r", "gmtime_r", "mktime", "nanosleep",
    "pthread_attr_init", "pthread_attr_destroy", "pthread_attr_setstacksize",
    "pthread_detach", "pthread_create", "pthread_join",
    "pthread_mutex_init", "pthread_mutex_lock", "pthread_mutex_unlock",
    "pthread_mutex_destroy", "pthread_cond_init", "pthread_cond_wait",
    "pthread_cond_signal", "pthread_cond_broadcast", "pthread_cond_destroy",
]
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


def ar_members(path: pathlib.Path):
    """Yield (name, bytes) for each member of a `ar` archive.

    Written out rather than shelled to `llvm-ar`, which is not otherwise
    needed to build ash and would be one more tool to find. The format is a
    magic line, then 60-byte headers; long names live in the `//` member and
    are referenced as `/<offset>`.
    """
    data = path.read_bytes()
    if not data.startswith(b"!<arch>\n"):
        return
    longnames = b""
    p = 8
    while p + 60 <= len(data):
        header = data[p:p + 60]
        name = header[0:16].decode("ascii", "replace").rstrip()
        size = int(header[48:58].decode("ascii", "replace").strip() or 0)
        body = data[p + 60:p + 60 + size]
        p += 60 + size + (size & 1)
        if name == "//":
            longnames = body
            continue
        if name.startswith("/") and name[1:].isdigit():
            start = int(name[1:])
            end = longnames.find(b"/", start)
            name = longnames[start:end].decode("ascii", "replace")
        yield name.rstrip("/"), body


def extract_library_libc(sysroot: pathlib.Path, into: pathlib.Path):
    """Write out the libc members in LIBRARY_LIBC, and return their paths.

    A relocatable link keeps only what is given to it, and `-u` -- the usual
    way to force an archive member in -- is refused alongside `-r`. wasi-libc
    puts one function per member and names it after the function, so naming
    the members directly does the same job.
    """
    archive = sysroot / "lib" / TRIPLE / "libc.a"
    if not archive.is_file():
        return []
    wanted = {f"{name}.c.obj": name for name in LIBRARY_LIBC}
    out_dir = into / "library-libc"
    out_dir.mkdir(parents=True, exist_ok=True)
    written = []
    found = set()
    for member, body in ar_members(archive):
        if member not in wanted:
            continue
        target = out_dir / member
        target.write_bytes(body)
        written.append(target)
        found.add(wanted[member])
    missing = sorted(set(LIBRARY_LIBC) - found)
    if missing:
        print(f"note: this wasi-libc has no member for {', '.join(missing)}")
    return written


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
    # ash_std carries a C dependency on wasm -- SQLite, compiled in because a
    # module cannot dlopen the HDLL. `cc` needs to be told which compiler and
    # which sysroot, since neither is the host's. Set only if the caller has
    # not: a machine with its own wasi-sdk knows better than this guess.
    cc = shutil.which("clang") or "clang"
    env.setdefault("CC_wasm32_wasip1", cc)
    env.setdefault(
        "CFLAGS_wasm32_wasip1",
        f"--target=wasm32-wasi --sysroot={sysroot}",
    )
    sh(cargo_cmd, cwd=REPO, env=env)

    archive = REPO / "target" / TRIPLE / args.profile / "libash_std.a"
    if not archive.is_file():
        sys.exit(f"cargo produced no {archive}")
    out = args.out or (REPO / "target" / args.profile / TRIPLE / "ash_runtime.o")
    out.parent.mkdir(parents=True, exist_ok=True)
    extra = extract_library_libc(sysroot, out.parent)
    sh([str(lld), "-flavor", "wasm", "-r", "-o", str(out),
        "--whole-archive", str(archive), "--no-whole-archive",
        *[str(o) for o in extra],
        f"-L{sysroot / 'lib' / TRIPLE}", "-lc", "-lsetjmp"])
    print(f"wrote {out} ({out.stat().st_size} bytes)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
