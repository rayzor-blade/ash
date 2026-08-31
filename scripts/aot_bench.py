#!/usr/bin/env python3
"""Time ash's AOT lane beside its JIT and HashLink/C.

AOT and HL/C are the two ahead-of-time native lanes, so this is the
comparison that decides whether emitting objects ourselves was worth doing:
same corpus, same accepted answers, same timing, same judgment. It reuses
`hl_bench`'s `time_command`, `judge` and `build_hlc` rather than reimplementing
them, because a lane timed by a different routine is not a comparison.

Lanes:
  ash-aot   the object ash emits, linked by tools/aot/link.sh, run directly.
            No JIT, no interpreter, no bytecode loading, no warm-up.
  ash-jit   `ash --mode jit`, which compiles the whole module and then runs.
  ash       `ash` with its default tiering, which is what CI publishes.
  hlc       `haxe -main M -hl main.c` recompiled against libhl.

Build cost is reported separately from run time for the two lanes that have
one, because they are different questions: `build_ms` is what a developer
waits for once, `median_ms` is what the user waits for every time.

A wrong answer is a failure, never a fast row: every iteration of every lane
is checked against bench/benchmarks.toml's accepted set.

Usage:
  scripts/aot_bench.py --out aot.json
  scripts/aot_bench.py --benchmarks fib,deltablue --hashlink-dir ~/hashlink \
                       --iterations 9 --out aot.json
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
import tempfile
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import hl_bench as hlb  # noqa: E402  (path set above)

try:
    import tomllib
except ModuleNotFoundError:  # pragma: no cover
    sys.exit("aot_bench.py needs Python 3.11+ (tomllib)")


RSS_BSD_RE = re.compile(r"^\s*(\d+)\s+maximum resident set size")
RSS_GNU_RE = re.compile(r"Maximum resident set size \(kbytes\):\s*(\d+)")


def time_wrapper() -> list[str] | None:
    """The argv prefix that yields a peak-RSS line, or None if unavailable."""
    t = "/usr/bin/time"
    if not os.path.exists(t):
        t = shutil.which("time") or ""
        if not t:
            return None
    return [t, "-l"] if sys.platform == "darwin" else [t, "-v"]


def parse_rss_bytes(stderr: str) -> int | None:
    for line in stderr.splitlines():
        m = RSS_BSD_RE.match(line)
        if m:
            return int(m.group(1))  # macOS reports bytes
        m = RSS_GNU_RE.search(line)
        if m:
            return int(m.group(1)) * 1024  # GNU reports kbytes
    return None


def peak_rss(cmd: list[str], timeout: float, env: dict | None) -> int | None:
    """One extra run, outside the timed set, purely to read peak RSS.

    Separate from `time_command` on purpose: wrapping every timed run in
    /usr/bin/time would charge the measurement its own process, and the whole
    point of the AOT lane is that its process is cheap.
    """
    wrapper = time_wrapper()
    if not wrapper:
        return None
    try:
        res = subprocess.run(wrapper + cmd, capture_output=True, text=True,
                             timeout=timeout, env=env)
    except (OSError, subprocess.TimeoutExpired):
        return None
    return parse_rss_bytes(res.stderr)


def build_aot(bench: dict, tests_dir: Path, repo: Path,
              workdir: Path) -> tuple[Path | None, str, float]:
    """Emit and link one bench ahead of time. Returns (binary, detail, ms)."""
    source = tests_dir / bench["hl"]
    if not source.exists():
        return None, f"no bytecode at {source}", 0.0
    spike = repo / "target" / "release" / "examples" / "aot_spike"
    if not spike.exists():
        spike = repo / "target" / "debug" / "examples" / "aot_spike"
    if not spike.exists():
        return None, "no aot_spike (cargo build -p ash_core --example aot_spike)", 0.0

    obj = workdir / "prog.o"
    binary = workdir / "prog"
    t0 = time.perf_counter()
    p = subprocess.run([str(spike), str(source), str(obj)],
                       capture_output=True, text=True, timeout=900)
    if p.returncode != 0:
        return None, f"emit failed: {(p.stderr or p.stdout).strip()[:300]}", 0.0
    refused = [l for l in p.stdout.splitlines() if "refused" in l]
    p = subprocess.run([str(repo / "tools" / "aot" / "link.sh"),
                        str(obj), str(binary)],
                       capture_output=True, text=True, timeout=300)
    if p.returncode != 0:
        return None, f"link failed: {(p.stderr or p.stdout).strip()[:300]}", 0.0
    ms = (time.perf_counter() - t0) * 1000.0
    return binary, (refused[0].strip() if refused else ""), ms


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--repo-root", type=Path,
                    default=Path(__file__).resolve().parent.parent)
    ap.add_argument("--benchmarks", default=None,
                    help="comma-separated names (default: all with checksums)")
    ap.add_argument("--iterations", type=lambda v: max(1, int(v)), default=7)
    ap.add_argument("--warmups", type=int, default=1)
    ap.add_argument("--haxe", default=None)
    ap.add_argument("--cc", default=os.environ.get("CC", "cc"))
    ap.add_argument("--hashlink-dir", type=Path, default=None)
    ap.add_argument("--out", type=Path, required=True)
    args = ap.parse_args()

    repo = args.repo_root.resolve()
    tests_dir = (repo / "crates" / "ash" / "test" / "tests").resolve()

    with (repo / "bench" / "benchmarks.toml").open("rb") as f:
        doc = tomllib.load(f)
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
        selected = [b for b in doc["bench"] if b.get("checksums")]

    ash = repo / "target" / "release" / "ash"
    if not ash.exists():
        ash = repo / "target" / "debug" / "ash"
    if not ash.exists():
        sys.exit("no ash binary (cargo build --release -p ash)")

    haxe = args.haxe or shutil.which("haxe")
    cc = shutil.which(args.cc)
    hlc_ready = bool(haxe and cc and args.hashlink_dir
                     and (args.hashlink_dir / "src" / "hlc.h").exists())
    run_env = dict(os.environ)
    if args.hashlink_dir:
        for var in ("LD_LIBRARY_PATH", "DYLD_LIBRARY_PATH"):
            prev = run_env.get(var, "")
            run_env[var] = f"{args.hashlink_dir}{':' + prev if prev else ''}"

    results = []
    for bench in selected:
        name = bench["name"]
        source = tests_dir / bench["hl"]
        timeout = bench["timeout_secs"] * 4
        row: dict = {"name": name, "lanes": {}}

        with tempfile.TemporaryDirectory(prefix=f"aot-{name}-") as td:
            binary, detail, build_ms = build_aot(bench, tests_dir, repo, Path(td))
            if binary is None:
                row["lanes"]["ash-aot"] = {"status": "FAIL", "detail": detail}
            else:
                rec = hlb.time_command([str(binary)], bench, args.iterations,
                                       args.warmups, timeout, run_env)
                rec["build_ms"] = round(build_ms, 1)
                rec["peak_rss_bytes"] = peak_rss([str(binary)], timeout, run_env)
                if detail:
                    rec["note"] = detail
                row["lanes"]["ash-aot"] = rec

        jit_cmd = [str(ash), "--mode", "jit", str(source)]
        row["lanes"]["ash-jit"] = hlb.time_command(
            jit_cmd, bench, args.iterations, args.warmups, timeout, run_env)
        row["lanes"]["ash-jit"]["peak_rss_bytes"] = peak_rss(jit_cmd, timeout, run_env)
        # Explicitly hybrid: `ash <file>` with no mode is the INTERPRETER,
        # and timing fib(40) under it measures the interpreter, not the
        # engine anyone ships. Hybrid is what the published table runs.
        hybrid_cmd = [str(ash), "--mode", "hybrid", str(source)]
        row["lanes"]["ash-hybrid"] = hlb.time_command(
            hybrid_cmd, bench, args.iterations, args.warmups, timeout, run_env)
        row["lanes"]["ash-hybrid"]["peak_rss_bytes"] = peak_rss(hybrid_cmd, timeout, run_env)

        if not hlc_ready:
            row["lanes"]["hlc"] = {"status": "UNAVAILABLE",
                                   "detail": "needs haxe, a C compiler and --hashlink-dir"}
        else:
            with tempfile.TemporaryDirectory(prefix=f"hlc-{name}-") as td:
                cbin, cdetail, cbuild = hlb.build_hlc(
                    bench, tests_dir, args.hashlink_dir, haxe, cc, Path(td))
                if cbin is None:
                    row["lanes"]["hlc"] = {"status": "FAIL", "detail": cdetail}
                else:
                    rec = hlb.time_command([str(cbin)], bench, args.iterations,
                                           args.warmups, timeout, run_env)
                    rec["build_ms"] = round(cbuild, 1)
                    rec["peak_rss_bytes"] = peak_rss([str(cbin)], timeout, run_env)
                    row["lanes"]["hlc"] = rec

        shown = []
        for lane in ("ash-aot", "ash-jit", "ash-hybrid", "hlc"):
            rec = row["lanes"][lane]
            if rec.get("status") == "OK":
                rss = rec.get("peak_rss_bytes")
                mem = f"/{rss / 1048576:.0f}MB" if rss else ""
                shown.append(f"{lane} {rec['wall_ms']['median_ms']:.1f}ms{mem}")
            else:
                shown.append(f"{lane} {rec['status']}")
        print(f"{name:16s} " + "  ".join(shown), flush=True)
        results.append(row)

    payload = {
        "host": {"platform": platform.platform(), "machine": platform.machine()},
        "iterations": args.iterations,
        "warmups": args.warmups,
        "benchmarks": results,
    }
    args.out.write_text(json.dumps(payload, indent=2))
    print(f"wrote {args.out}")

    invalid = [r["name"] for r in results
               for rec in r["lanes"].values() if rec.get("status") == "INVALID"]
    if invalid:
        print(f"INVALID answers: {', '.join(sorted(set(invalid)))}")
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
