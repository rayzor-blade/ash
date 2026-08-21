#!/usr/bin/env python3
"""Time official HashLink — JIT and HL/C AOT — on the ash bench corpus.

The published benchmark page normalizes every bar to the HashLink JIT and
shows HashLink/C as the ahead-of-time floor, so both baselines have to come
from the same runner, the same sources, and the same correctness judgment as
the ash rows. This script reads bench/benchmarks.toml (the single source of
truth for the corpus and its accepted answers) and runs two lanes:

  hashlink-jit  `hl <bench>.hl` on the committed bytecode — needs a working
                `hl` on PATH (or --hl).
  hashlink-c    `haxe -main <Main> -hl main.c` recompiled with the system C
                compiler against libhl, then the native binary timed — needs
                `haxe`, a C compiler, and --hashlink-dir pointing at a
                HashLink checkout that has src/ headers and a built libhl.

Each lane degrades independently: a missing toolchain records UNAVAILABLE and
the page renders without that row. A wrong checksum records INVALID and fails
the process — a wrong answer must never publish as a baseline.

Usage:
  scripts/hl_bench.py --benchmarks mandelbrot --out hl.json
  scripts/hl_bench.py --benchmarks fib --hashlink-dir ~/hashlink --out hl.json
"""

from __future__ import annotations

import argparse
import json
import os
import platform
import shutil
import statistics
import subprocess
import sys
import tempfile
import time
from pathlib import Path

try:
    import tomllib
except ModuleNotFoundError:  # pragma: no cover
    sys.exit("hl_bench.py needs Python 3.11+ (tomllib)")


def hl_version(hl: str, env: dict | None = None) -> str | None:
    """The hl version string, or None when the binary does not actually work.
    A binary that spawns but exits non-zero (unresolvable libhl, broken
    cache) must not publish its loader errors as a version."""
    import re
    try:
        out = subprocess.run(
            [hl, "--version"], capture_output=True, text=True, timeout=30,
            env=env,
        )
    except (OSError, subprocess.TimeoutExpired):
        return None
    if out.returncode != 0:
        return None
    v = (out.stdout or out.stderr).strip().splitlines()[0] if (out.stdout or out.stderr).strip() else ""
    return v if re.search(r"\d+\.\d+", v) else None


def judge(bench: dict, stdout: str) -> tuple[bool, str]:
    """Whether the run printed an accepted value, mirroring ash_bench's
    checksum judgment: find the marker line, compare the trailing token."""
    marker = bench.get("checksum_marker", "Checksum:")
    accepted = {c["value"] for c in bench.get("checksums", [])}
    if not accepted:
        return True, "exit only"
    for line in stdout.splitlines():
        if marker in line:
            got = line.split()[-1].strip()
            if got in accepted:
                return True, f"checksum {got}"
            return False, f"checksum {got} not in accepted set"
    return False, f"no '{marker}' line in output"


def time_command(cmd: list[str], bench: dict, iterations: int, warmups: int,
                 timeout: float, env: dict | None = None) -> dict:
    samples: list[float] = []
    detail = ""
    for i in range(warmups + iterations):
        t0 = time.perf_counter()
        try:
            p = subprocess.run(cmd, capture_output=True, text=True,
                               timeout=timeout, env=env)
        except subprocess.TimeoutExpired:
            return {"status": "TIMEOUT", "detail": f"exceeded {timeout:.0f}s"}
        except OSError as e:
            return {"status": "FAIL", "detail": f"spawn failed: {e}"}
        wall = (time.perf_counter() - t0) * 1000.0
        if p.returncode != 0:
            return {
                "status": "FAIL",
                "detail": f"exit {p.returncode}: {(p.stderr or p.stdout)[:300]}",
            }
        ok, detail = judge(bench, p.stdout)
        if not ok:
            return {"status": "INVALID", "detail": detail}
        if i >= warmups:
            samples.append(wall)
    return {
        "status": "OK",
        "detail": detail,
        "samples_ms": samples,
        "wall_ms": {
            "min_ms": min(samples),
            "median_ms": statistics.median(samples),
            "mean_ms": statistics.fmean(samples),
            "max_ms": max(samples),
            "stddev_ms": statistics.stdev(samples) if len(samples) > 1 else 0.0,
            "runs": len(samples),
        },
    }


def build_hlc(bench: dict, tests_dir: Path, hashlink_dir: Path, haxe: str,
              cc: str, workdir: Path) -> tuple[Path | None, str, float]:
    """Compile the bench to native via HL/C. Returns (binary, detail, build_ms)."""
    main = bench.get("main")
    if not main:
        return None, "no Haxe main class recorded for this bench", 0.0
    out_c = workdir / "main.c"
    t0 = time.perf_counter()
    p = subprocess.run(
        [haxe, "--cwd", str(tests_dir), "-main", main, "-hl", str(out_c)],
        capture_output=True, text=True, timeout=300,
    )
    if p.returncode != 0:
        # Haxe splits the real diagnostic and the generic "Build failed"
        # across the two streams; keep both, and put the whole thing in the
        # job log where a truncated JSON detail can't hide it.
        full = (p.stderr or "") + ("\n" + p.stdout if p.stdout else "")
        print(f"[hl-bench] haxe -hl failed for {main}:\n{full}", flush=True)
        return None, f"haxe -hl main.c failed: {full.strip()[:500]}", 0.0
    binary = workdir / "app"
    link = [
        cc, "-O3", "-o", str(binary), str(out_c),
        "-I", str(workdir), "-I", str(hashlink_dir / "src"),
        "-L", str(hashlink_dir), "-lhl", "-lm",
    ]
    if platform.system() == "Linux":
        link += ["-ldl", "-lpthread"]
    p = subprocess.run(link, capture_output=True, text=True, timeout=300)
    if p.returncode != 0:
        full = (p.stderr or "") + ("\n" + p.stdout if p.stdout else "")
        print(f"[hl-bench] cc failed for {main}:\n{full}", flush=True)
        return None, f"cc failed: {full.strip()[:500]}", 0.0
    build_ms = (time.perf_counter() - t0) * 1000.0
    return binary, "", build_ms


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--repo-root", type=Path,
                    default=Path(__file__).resolve().parent.parent)
    ap.add_argument("--manifest", type=Path, default=None)
    ap.add_argument("--hl", default=None,
                    help="hl binary for the JIT lane (default: PATH)")
    ap.add_argument("--haxe", default=None,
                    help="haxe binary for the HL/C lane (default: PATH)")
    ap.add_argument("--cc", default=os.environ.get("CC", "cc"))
    ap.add_argument("--hashlink-dir", type=Path, default=None,
                    help="HashLink checkout with src/ headers and built libhl; "
                         "enables the hashlink-c lane")
    ap.add_argument("--benchmarks", default=None,
                    help="comma-separated names (default: all with checksums)")
    ap.add_argument("--iterations", type=lambda v: max(1, int(v)), default=7)
    ap.add_argument("--warmups", type=int, default=1)
    ap.add_argument("--timeout-scale", type=float, default=1.0)
    ap.add_argument("--out", type=Path, required=True)
    args = ap.parse_args()

    manifest = args.manifest or args.repo_root / "bench" / "benchmarks.toml"
    with manifest.open("rb") as f:
        doc = tomllib.load(f)
    # Same resolution as ash_bench.py: the corpus lives in the test tree, and
    # a bench naming a parity_case inherits its .hl filename, Haxe main class
    # and timeout.
    tests_dir = (args.repo_root / "crates" / "ash" / "test" / "tests").resolve()
    parity = {}
    parity_file = tests_dir / "parity_cases.toml"
    if parity_file.exists():
        with parity_file.open("rb") as f:
            parity = {c["name"]: c for c in tomllib.load(f).get("case", [])}
    for b in doc["bench"]:
        base = parity.get(b.get("parity_case", ""), {})
        b.setdefault("hl", base.get("hl", ""))
        b.setdefault("main", base.get("main", ""))
        b.setdefault("timeout_secs", base.get("timeout_secs", 120))

    benches = {b["name"]: b for b in doc["bench"]}
    if args.benchmarks:
        want = [n.strip() for n in args.benchmarks.split(",") if n.strip()]
        unknown = [n for n in want if n not in benches]
        if unknown:
            sys.exit(f"unknown benchmark(s): {', '.join(unknown)}")
        selected = [benches[n] for n in want]
    else:
        # Only checksum-judged benches can be validated without an
        # interpreter reference, so those are the publishable default.
        selected = [b for b in doc["bench"] if b.get("checksums")]

    run_env = dict(os.environ)
    if args.hashlink_dir:
        # The freshly built libhl lives next to the checkout, for both lanes.
        for var in ("LD_LIBRARY_PATH", "DYLD_LIBRARY_PATH"):
            prev = run_env.get(var, "")
            run_env[var] = f"{args.hashlink_dir}{':' + prev if prev else ''}"

    hl = args.hl or shutil.which("hl")
    version = hl_version(hl, run_env) if hl else None

    haxe = args.haxe or shutil.which("haxe")
    cc = shutil.which(args.cc)
    hlc_ready = bool(
        haxe and cc and args.hashlink_dir
        and (args.hashlink_dir / "src" / "hlc.h").exists()
    )
    hlc_missing = (
        "haxe" if not haxe else
        "C compiler" if not cc else
        "--hashlink-dir with src/hlc.h" if not hlc_ready else ""
    )

    out = {
        "schema_version": 1,
        "tool": "hl_bench",
        "generated_iso": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "hl_binary": hl,
        "hl_version": version,
        "haxe_binary": haxe if hlc_ready else None,
        "system": {
            "os": platform.system().lower(),
            "arch": platform.machine(),
        },
        "results": [],
    }

    for bench in selected:
        if not bench["hl"]:
            sys.exit(f"bench {bench['name']}: no .hl file (parity case missing?)")
        hl_path = tests_dir / bench["hl"]
        timeout = float(bench.get("timeout_secs", 120)) * args.timeout_scale

        # ---- hashlink-jit lane ------------------------------------------
        rec: dict = {"benchmark": bench["name"], "engine": "hashlink-jit",
                     "hl": bench["hl"]}
        if version is None:
            rec.update(status="UNAVAILABLE",
                       detail="no working hl binary on this runner")
        elif not hl_path.exists():
            rec.update(status="SKIP", detail=f"missing {hl_path}")
        else:
            print(f"[hl-bench] {bench['name']} (jit) ...", flush=True)
            rec.update(time_command([hl, str(hl_path)], bench,
                                    args.iterations, args.warmups, timeout,
                                    env=run_env))
            wall = rec.get("wall_ms")
            shown = f"{wall['median_ms']:.1f}ms" if wall else rec["detail"]
            print(f"[hl-bench] {bench['name']} (jit) {rec['status']} {shown}",
                  flush=True)
        out["results"].append(rec)

        # ---- hashlink-c lane --------------------------------------------
        rec = {"benchmark": bench["name"], "engine": "hashlink-c"}
        if not hlc_ready:
            rec.update(status="UNAVAILABLE",
                       detail=f"HL/C lane needs {hlc_missing}")
        else:
            with tempfile.TemporaryDirectory(prefix=f"hlc-{bench['name']}-") as td:
                binary, err, build_ms = build_hlc(
                    bench, tests_dir, args.hashlink_dir, haxe, cc, Path(td)
                )
                if binary is None:
                    rec.update(status="FAIL", detail=err)
                else:
                    print(f"[hl-bench] {bench['name']} (hl/c) ...", flush=True)
                    rec["aot_build_ms"] = round(build_ms, 1)
                    rec.update(time_command([str(binary)], bench,
                                            args.iterations, args.warmups,
                                            timeout, env=run_env))
                    wall = rec.get("wall_ms")
                    shown = f"{wall['median_ms']:.1f}ms" if wall else rec["detail"]
                    print(f"[hl-bench] {bench['name']} (hl/c) "
                          f"{rec['status']} {shown}", flush=True)
        out["results"].append(rec)

    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(json.dumps(out, indent=2, sort_keys=True) + "\n")
    print(f"[hl-bench] wrote {args.out}")
    # A missing toolchain is a degraded publish, not a failure; a wrong
    # answer is a failure — it must never become a published baseline.
    return 1 if any(r.get("status") == "INVALID" for r in out["results"]) else 0


if __name__ == "__main__":
    sys.exit(main())
