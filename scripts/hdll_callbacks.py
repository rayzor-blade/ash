#!/usr/bin/env python3
"""Build and run the HDLL callback corpus.

ash's unit tests link the stdlib as an rlib and call into it directly. That
reaches none of the boundary a real HDLL crosses: primitives resolved by name
from an export table, a C library holding a `vclosure*` across a collection,
and `hl_dyn_call` marshalling arguments back into the VM. Both defects fixed in
e66c795 lived exactly there, and both were reported by a user rather than
caught here.

Each case is a Haxe program plus a C library. The library keeps the closure in
malloc'd memory, the program lets the collector run, and the library then calls
back. `CBn_ROOT` selects how the library declares that reference -- see
cases.toml for what each value means and which rows are contractual.

    scripts/hdll_callbacks.py                    # build + run everything
    scripts/hdll_callbacks.py --cases cb8,cb9    # a subset
    scripts/hdll_callbacks.py --modes interp     # one engine
    scripts/hdll_callbacks.py --update-baseline  # re-record expectations

Exit status is 1 if any row regressed against the baseline, else 0. A row that
starts PASSING is also reported, because an unexplained improvement usually
means the case stopped testing what it was written for.
"""
from __future__ import annotations

import argparse
import json
import os
import platform
import re
import shutil
import subprocess
import sys
import tomllib
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
HDLL_DIR = ROOT / "crates" / "ash" / "test" / "hdll"
BUILD_DIR = HDLL_DIR / "build"
# Per platform. The corpus links real shared libraries and runs real
# collections, so a row's outcome is a property of the OS as much as of ash --
# recording one platform's answers and asserting them on another reports every
# difference as a regression, which is what a macOS baseline did to the first
# Linux run.
BASELINE = HDLL_DIR / "baseline.{}-{}.json".format(
    platform.system().lower(), platform.machine().lower())
IS_DARWIN = platform.system() == "Darwin"
IS_WINDOWS = platform.system() == "Windows"
# What the runtime is called once staged, and what cargo actually produced.
# Windows drops the `lib` prefix and splits the import library out.
SHLIB = "libhl.dylib" if IS_DARWIN else ("hl.dll" if IS_WINDOWS else "libhl.so")
CDYLIB = "libash_std.dylib" if IS_DARWIN else ("ash_std.dll" if IS_WINDOWS else "libash_std.so")

# Addresses differ every run; nothing else in these transcripts does.
PTR_RE = re.compile(r"0x[0-9a-fA-F]{4,}")


def norm(text: str) -> str:
    return PTR_RE.sub("0xPTR", text).strip()


def target_dir() -> Path:
    override = os.environ.get("ASH_TARGET_DIR")
    if override:
        return Path(override)
    return ROOT / "target" / "debug"


def stage_runtime(tdir: Path) -> Path:
    """HDLLs link against a library called libhl; ash's is called ash_std."""
    staged = tdir / SHLIB
    src = tdir / CDYLIB
    if not src.exists():
        sys.exit(f"missing {src}; run: cargo build -p ash_std")
    if not staged.exists() or staged.stat().st_mtime < src.stat().st_mtime:
        shutil.copy2(src, staged)
    if IS_WINDOWS:
        # An hdll links against the import library, not the DLL itself.
        imp = tdir / "ash_std.dll.lib"
        if imp.exists():
            shutil.copy2(imp, tdir / "hl.lib")
    return staged


def c_driver() -> str:
    """clang on Windows: the harnesses are GCC-flavoured C and the flags below
    (-shared, -I) are clang/gcc spellings, not cl.exe's."""
    for cand in ("clang", "cc", "gcc"):
        if shutil.which(cand):
            return cand
    sys.exit("no C driver found (tried clang, cc, gcc)")


def build(cases, tdir: Path, verbose: bool) -> None:
    BUILD_DIR.mkdir(exist_ok=True)
    staged = stage_runtime(tdir)
    CC = c_driver()

    def run(cmd, **kw):
        if verbose:
            print("  $", " ".join(str(c) for c in cmd))
        p = subprocess.run(cmd, cwd=HDLL_DIR, capture_output=True, text=True, **kw)
        if p.returncode != 0:
            sys.exit(f"command failed: {' '.join(str(c) for c in cmd)}\n{p.stdout}{p.stderr}")

    # `cc` is not guaranteed to target the machine ash was built for -- an Intel
    # Homebrew toolchain first on PATH silently produces an x86_64 object and
    # the link fails on the arm64 runtime. Ask the runtime what it is.
    # cb14 spawns a foreign thread. Apple's libc carries pthread, glibc does
    # not, so the Linux link needs asking for it explicitly; -fPIC is required
    # there for a shared object and is a no-op on Darwin. Windows has neither
    # concept.
    extra = [] if (IS_DARWIN or IS_WINDOWS) else ["-pthread", "-fPIC"]
    arch = []
    if IS_DARWIN:
        got = subprocess.run(["lipo", "-archs", str(staged)], capture_output=True, text=True)
        if got.returncode == 0 and got.stdout.split():
            arch = ["-arch", got.stdout.split()[0]]

    link_target = str(tdir / "hl.lib") if IS_WINDOWS else str(staged)
    for lib in sorted({c["lib"] for c in cases}):
        out = BUILD_DIR / f"{lib}.hdll"
        run([CC, *arch, *extra, "-shared", "-o", str(out), f"{lib}.c",
             "-I", str(ROOT / "std"), link_target])
        if IS_DARWIN:
            # The hdll records the absolute path it linked against; point it at
            # the name ash actually loads instead.
            installed = subprocess.run(
                ["otool", "-D", str(staged)], capture_output=True, text=True, check=True
            ).stdout.strip().splitlines()[-1]
            run(["install_name_tool", "-change", installed, "@rpath/libhl.dylib", str(out)])
        print(f"built {out.name}")

    for c in cases:
        run(["haxe", "-main", c["class"], "--hl", str(BUILD_DIR / f"{c['name']}.hl")])
        print(f"built {c['name']}.hl")


# Words that mark the line worth printing. A case's first line is usually its
# own banner, which says nothing about why it failed.
_SIGNAL = re.compile(
    r"error|failed|failure|missing|not found|cannot|unable|abort|panic|"
    r"segmentation|assert|FAIL|exception",
    re.IGNORECASE,
)


def diagnostic_line(out: str) -> str:
    """The line most likely to explain a failure: the last one that reads like
    a complaint, or failing that the last thing the program said at all."""
    lines = [l.strip() for l in out.splitlines() if l.strip()]
    if not lines:
        return ""
    for line in reversed(lines):
        if _SIGNAL.search(line):
            return line
    return lines[-1]


def judge(stdout: str, stderr: str, code: int, timed_out: bool) -> str:
    if timed_out:
        return "TIMEOUT"
    blob = stdout + stderr
    if "FAIL" in blob:
        return "FAIL"
    if "PASS" in blob:
        return "PASS"
    if code != 0:
        return f"EXIT{code}"
    # Cases without an explicit marker are judged purely by their transcript,
    # which the baseline holds. Saying PASS here would be a claim the program
    # never made.
    return "NOMARK"


def run_case(case, root: str, mode: str, ash: Path, timeout: int):
    env = dict(os.environ)
    # From the manifest, not the case name: cb0b shares cb0.c and so reads
    # CB0_ROOT, which no derivation from "cb0b" would produce.
    var = f"{case['env_prefix']}_ROOT"
    # An empty value still reads as set, so "none" must UNSET it.
    env.pop(var, None)
    if root != "none":
        env[var] = root
    try:
        p = subprocess.run(
            [str(ash), "--mode", mode, f"{case['name']}.hl"],
            cwd=BUILD_DIR, env=env, capture_output=True, text=True, timeout=timeout,
        )
        return judge(p.stdout, p.stderr, p.returncode, False), norm(p.stdout + p.stderr)
    except subprocess.TimeoutExpired:
        return "TIMEOUT", ""


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--cases", help="comma-separated case names")
    ap.add_argument("--modes", default="interp,jit,hybrid")
    ap.add_argument("--roots", help="restrict to these CBn_ROOT values")
    ap.add_argument("--timeout", type=int, default=60)
    ap.add_argument("--no-build", action="store_true")
    ap.add_argument("--update-baseline", action="store_true")
    ap.add_argument("--json", help="write the full matrix here")
    ap.add_argument("-v", "--verbose", action="store_true")
    args = ap.parse_args()

    manifest = tomllib.loads((HDLL_DIR / "cases.toml").read_text())["case"]
    if args.cases:
        want = set(args.cases.split(","))
        manifest = [c for c in manifest if c["name"] in want]
        if not manifest:
            sys.exit("no such case")

    tdir = target_dir()
    ash = tdir / ("ash.exe" if IS_WINDOWS else "ash")
    if not ash.exists():
        sys.exit(f"missing {ash}; run: cargo build -p ash")

    if not args.no_build:
        build(manifest, tdir, args.verbose)

    modes = args.modes.split(",")
    only_roots = set(args.roots.split(",")) if args.roots else None

    results, width = {}, max(len(c["name"]) for c in manifest)
    for c in manifest:
        for root in c["roots"]:
            if only_roots and root not in only_roots:
                continue
            row, why = [], []
            for mode in modes:
                status, out = run_case(c, root, mode, ash, args.timeout)
                results[f"{c['name']}/{root}/{mode}"] = {"status": status, "output": out}
                row.append(f"{mode}={status}")
                if status not in ("PASS", "NOMARK") and out:
                    line = diagnostic_line(out)
                    if line and line not in why:
                        why.append(line)
            print(f"{c['name']:<{width}}  root={root:<6}  " + "  ".join(row))
            # A status alone cannot be acted on from a CI log. One line of what
            # the program actually said usually can.
            for line in why[:2]:
                print(f"{'':<{width}}    | {line[:150]}")

    if args.json:
        Path(args.json).write_text(json.dumps(results, indent=2, sort_keys=True))

    if args.update_baseline:
        BASELINE.write_text(json.dumps(
            {k: v["status"] for k, v in sorted(results.items())}, indent=2) + "\n")
        print(f"\nbaseline written: {len(results)} rows -> {BASELINE.name}")
        return 0

    if not BASELINE.exists():
        # A first run on a platform is a measurement, not a verdict -- the same
        # rule scripts/haxe_conformance.py follows. The matrix above is the
        # useful output; a red X asserting one platform's answers on another is
        # not.
        print(f"\nno {BASELINE.name}; this platform has no recorded expectations yet.")
        print("Record them with --update-baseline once the rows are understood.")
        return 0

    base = json.loads(BASELINE.read_text())
    regressed, improved, new = [], [], []
    for key, got in sorted(results.items()):
        was = base.get(key)
        if was is None:
            new.append(key)
        elif was != got["status"]:
            (regressed if was == "PASS" else improved).append((key, was, got["status"]))

    for key, was, now in regressed:
        print(f"REGRESSED {key}: {was} -> {now}")
    for key, was, now in improved:
        print(f"changed   {key}: {was} -> {now}")
    for key in new:
        print(f"new row   {key}: {results[key]['status']} (not in baseline)")

    if regressed:
        print(f"\n{len(regressed)} row(s) regressed")
        return 1
    print(f"\n{len(results)} rows, no regressions")
    return 0


if __name__ == "__main__":
    sys.exit(main())
