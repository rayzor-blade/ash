#!/usr/bin/env python3
"""Run programs against the HDLLs of an official HashLink release.

The callback corpus builds its own HDLLs, and the Heaps demos ship one macOS
`sdl.hdll`. Neither says what happens when ash loads the HDLL set a Windows
user actually has: the `hashlink-<tag>.0-win.zip` from HashLink's releases,
paired with whatever `hlsdl`/`heaps` haxelib versions their project resolved.
Those pairings drift apart, and a program can then name a primitive the
loaded HDLL does not export.

Cells:

  optional-prim  Calls primitives of an optional library (`?sdl`) that no
                 sdl.hdll exports. HashLink starts, skips the first and
                 raises a catchable error from the second.
  haxeui-heaps   A HaxeUI + Heaps window built from the released haxelibs.
                 `hlsdl` 1.16 names `?sdl@win_get_display_scale`, which a
                 1.15 sdl.hdll lacks.

    scripts/hdll_release_probe.py --hashlink 1.15
    scripts/hdll_release_probe.py --hashlink 1.16 --cells optional-prim
    scripts/hdll_release_probe.py --hdll-dir DIR --ash target/release/ash   # local

This reports and does not gate: it exits 0 unless a cell could not be set up.
`--strict` makes an unexpected result exit 1, for once expectations exist.
"""
from __future__ import annotations

import argparse
import os
import platform
import shutil
import subprocess
import sys
import urllib.request
import zipfile
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
FIXTURES = ROOT / "crates" / "ash" / "test" / "hdll_release"
IS_WINDOWS = platform.system() == "Windows"
EXE = ".exe" if IS_WINDOWS else ""
MODES = ("interp", "hybrid", "jit")
CELLS = ("optional-prim", "haxeui-heaps")
HAXELIBS = ("heaps", "hlsdl", "haxeui-core", "haxeui-heaps")

# What a run that behaves like HashLink prints.
OPTIONAL_PRIM_EXPECTED = [
    "start",
    "after-skip",
    "caught: Primitive or library is missing",
    "end",
]


def run(cmd, cwd=None, timeout=None):
    return subprocess.run(
        [str(c) for c in cmd], cwd=cwd, timeout=timeout, text=True,
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT, errors="replace",
    )


def tool(name: str) -> str:
    found = shutil.which(name)
    if not found:
        sys.exit(f"{name} is not on PATH")
    return found


def fetch_release(tag: str, work: Path) -> Path:
    """Download and unpack HashLink's Windows release; return the directory
    holding its HDLLs."""
    url = (f"https://github.com/HaxeFoundation/hashlink/releases/download/"
           f"{tag}/hashlink-{tag}.0-win.zip")
    archive = work / f"hashlink-{tag}.zip"
    if not archive.exists():
        print(f"fetching {url}")
        urllib.request.urlretrieve(url, archive)
    out = work / f"hashlink-{tag}"
    if not out.exists():
        with zipfile.ZipFile(archive) as z:
            z.extractall(out)
    hits = sorted(out.rglob("sdl.hdll"))
    if not hits:
        sys.exit(f"no sdl.hdll in {archive}")
    return hits[0].parent


def stage(hdll_dir: Path, runtime: Path | None, dest: Path) -> None:
    """The release's HDLLs and their DLLs, with ash's runtime standing in for
    the release's own libhl: ash ships its own and the HDLLs bind to that."""
    dest.mkdir(parents=True, exist_ok=True)
    for f in hdll_dir.iterdir():
        if f.is_file() and f.suffix.lower() in (".hdll", ".dll", ".dylib", ".so"):
            if f.name.lower().startswith("libhl"):
                continue
            shutil.copy2(f, dest / f.name)
    if runtime:
        shutil.copy2(runtime, dest / ("libhl.dll" if IS_WINDOWS else runtime.name))


def build_optional_prim(dest: Path) -> Path:
    out = dest / "OptionalPrim.hl"
    p = run([tool("haxe"), "-hl", out, "-cp", FIXTURES, "-main", "OptionalPrim"])
    if p.returncode:
        sys.exit(f"haxe failed:\n{p.stdout}")
    return out


def build_haxeui_heaps(work: Path, dest: Path) -> Path:
    project = work / "haxeui_heaps"
    if project.exists():
        shutil.rmtree(project)
    shutil.copytree(FIXTURES / "haxeui_heaps", project)
    (project / "build" / "heaps" / "hl").mkdir(parents=True)
    haxelib = tool("haxelib")
    # A repository beside the project, so the libraries are whatever haxelib
    # serves today and nothing already on the machine.
    run([haxelib, "newrepo"], cwd=project)
    for lib in HAXELIBS:
        p = run([haxelib, "--always", "install", lib], cwd=project)
        if p.returncode:
            sys.exit(f"haxelib install {lib} failed:\n{p.stdout}")
    print(run([haxelib, "list"], cwd=project).stdout.strip())
    p = run([tool("haxe"), "heaps-hl.hxml"], cwd=project)
    if p.returncode:
        sys.exit(f"haxe failed:\n{p.stdout}")
    out = dest / "Main.hl"
    shutil.copy2(project / "build" / "heaps" / "hl" / "Main.hl", out)
    return out


def summarize(cell: str, text: str, code: int | None) -> tuple[str, str]:
    """(verdict, detail) for one run."""
    lines = [l.strip() for l in text.splitlines() if l.strip()]
    missing = next((l for l in lines if "natives resolved" in l), "")
    problem = next((l for l in lines if l.startswith(("Error:", "Uncaught exception"))
                    or "panicked" in l or "LLVM ERROR" in l), "")
    if cell == "optional-prim":
        got = [l for l in lines if not l.startswith("[ash]")]
        if got == OPTIONAL_PRIM_EXPECTED:
            return "ok", "behaves like HashLink"
        return "DIFF", problem or " | ".join(got[:5]) or "no output"
    if problem:
        return "FAIL", problem
    if code is None:
        return "ran", "until the time limit; " + (missing or "no missing natives")
    return "ran", missing or "no missing natives"


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--hashlink", help="release tag whose Windows HDLLs to use, e.g. 1.15")
    ap.add_argument("--hdll-dir", type=Path, help="use this directory's HDLLs instead of a release")
    ap.add_argument("--ash", type=Path,
                    default=ROOT / "target" / "debug" / f"ash{EXE}")
    ap.add_argument("--runtime", type=Path,
                    default=ROOT / "target" / "debug" / "ash_std.dll" if IS_WINDOWS else None,
                    help="ash's runtime library, staged as libhl")
    ap.add_argument("--cells", default=",".join(CELLS))
    ap.add_argument("--modes", default=",".join(MODES))
    ap.add_argument("--timeout", type=int, default=45)
    ap.add_argument("--work", type=Path, default=ROOT / "target" / "hdll_release_probe")
    ap.add_argument("--no-run", action="store_true", help="build the cells and stop")
    ap.add_argument("--strict", action="store_true")
    args = ap.parse_args()

    if not (args.hashlink or args.hdll_dir):
        ap.error("give --hashlink or --hdll-dir")
    args.ash = args.ash.resolve()
    args.runtime = args.runtime.resolve() if args.runtime else None
    args.work = args.work.resolve()
    cells = args.cells.split(",")
    modes = args.modes.split(",")
    label = args.hashlink or str(args.hdll_dir)
    args.work.mkdir(parents=True, exist_ok=True)

    hdlls = args.hdll_dir or fetch_release(args.hashlink, args.work)
    dest = args.work / f"stage-{(args.hashlink or 'local')}"
    if dest.exists():
        shutil.rmtree(dest)
    stage(hdlls, args.runtime, dest)

    programs = {}
    for cell in cells:
        programs[cell] = (build_optional_prim(dest) if cell == "optional-prim"
                          else build_haxeui_heaps(args.work, dest))
    if args.no_run:
        return 0

    rows = []
    for cell in cells:
        for mode in modes:
            try:
                p = run([args.ash, "--mode", mode, programs[cell].name],
                        cwd=dest, timeout=args.timeout)
                text, code = p.stdout, p.returncode
            except subprocess.TimeoutExpired as e:
                text = (e.stdout.decode(errors="replace") if isinstance(e.stdout, bytes)
                        else e.stdout or "")
                code = None
            verdict, detail = summarize(cell, text, code)
            rows.append((cell, mode, verdict, detail))
            print(f"\n--- {cell} / {mode}: {verdict}\n{text.strip()[-1500:]}")

    table = [f"### HashLink {label} HDLLs", "",
             "| cell | mode | result | detail |", "|---|---|---|---|"]
    table += [f"| {c} | {m} | {v} | {d.replace('|', '/')[:400]} |" for c, m, v, d in rows]
    print("\n" + "\n".join(table))
    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary:
        with open(summary, "a", encoding="utf-8") as f:
            f.write("\n".join(table) + "\n")

    if args.strict and any(v in ("DIFF", "FAIL") for _, _, v, _ in rows):
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
