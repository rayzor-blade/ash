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


def build_aot(bench: dict, tests_dir: Path, repo: Path, workdir: Path,
              env: dict | None = None, profile: bool = False,
              ash: Path | None = None) -> tuple[Path | None, str, float]:
    """Emit and link one bench ahead of time. Returns (binary, detail, ms)."""
    source = tests_dir / bench["hl"]
    if not source.exists():
        return None, f"no bytecode at {source}", 0.0
    # Newest, not release-first. A stale release build sitting beside a fresh
    # debug one silently emits an object from older compiler code -- here it
    # produced one with no `main` at all, and the failure surfaced as a link
    # error naming a symbol the emitter had simply stopped writing.
    candidates = [
        repo / "target" / profile / "examples" / "aot_spike"
        for profile in ("release", "debug")
    ]
    existing = [c for c in candidates if c.exists()]
    if not existing:
        return None, "no aot_spike (cargo build -p ash_core --example aot_spike)", 0.0
    spike = max(existing, key=lambda c: c.stat().st_mtime)

    obj = workdir / "prog.o"
    binary = workdir / "prog"
    t0 = time.perf_counter()

    emit_env = dict(env or os.environ)
    pgo_arg: list[str] = []
    if profile:
        # A profiling run, charged to BUILD time where it belongs: this is
        # ordinary PGO, and the cost is paid once by whoever builds, not on
        # every run. Only a tiered mode observes anything -- the record site is
        # gated on a tiered runtime -- so `--mode hybrid`, not interp.
        # The caller's binary, already chosen newest-first. Re-deriving it
        # here release-first picked a stale build that had never heard of
        # ASH_AOT_PROFILE_OUT, so no profile was written and the flag looked
        # like it simply did not work.
        if ash is not None:
            prof = workdir / "callsites.prof"
            run_env = dict(os.environ)
            run_env["ASH_AOT_PROFILE_OUT"] = str(prof)
            subprocess.run([str(ash), "--mode", "hybrid", "--quiet", str(source)],
                           capture_output=True, text=True, timeout=900, env=run_env)
            if prof.exists():
                pgo_arg = ["--pgo=" + str(prof)]

    p = subprocess.run([str(spike), str(source), str(obj), *pgo_arg],
                       capture_output=True, text=True, timeout=900, env=emit_env)
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
    ap.add_argument("--lanes", default="ash-aot,ash-jit,ash-hybrid,hlc",
                    help="comma-separated lanes to run; CI asks for ash-aot "
                         "alone, because ash_bench.py and hl_bench.py already "
                         "produce the others and running them twice would put "
                         "two different measurements of one engine on the page")
    ap.add_argument("--aot-profile", action="store_true",
                    help="give the AOT lane a callsite profile from a hybrid "
                         "run first, and charge that run to build time. This "
                         "makes the lane profile-guided, which the HL/C lane "
                         "is not -- say so wherever the row is published.")
    ap.add_argument("--emit-partial", type=Path, default=None,
                    help="also write a merge_bench_site.py partial carrying "
                         "the ash-aot lane")
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
        b.setdefault("env", {})

    benches = {b["name"]: b for b in doc["bench"]}
    if args.benchmarks:
        want = [n.strip() for n in args.benchmarks.split(",") if n.strip()]
        unknown = [n for n in want if n not in benches]
        if unknown:
            sys.exit(f"unknown benchmark(s): {', '.join(unknown)}")
        selected = [benches[n] for n in want]
    else:
        selected = [b for b in doc["bench"] if b.get("checksums")]

    ash_candidates = [
        repo / "target" / profile / "ash" for profile in ("release", "debug")
    ]
    ash_existing = [c for c in ash_candidates if c.exists()]
    if not ash_existing:
        sys.exit("no ash binary (cargo build --release -p ash)")
    ash = max(ash_existing, key=lambda c: c.stat().st_mtime)

    haxe = args.haxe or shutil.which("haxe")
    cc = shutil.which(args.cc)
    hlc_ready = bool(haxe and cc and args.hashlink_dir
                     and (args.hashlink_dir / "src" / "hlc.h").exists())
    run_env = dict(os.environ)
    if args.hashlink_dir:
        for var in ("LD_LIBRARY_PATH", "DYLD_LIBRARY_PATH"):
            prev = run_env.get(var, "")
            run_env[var] = f"{args.hashlink_dir}{':' + prev if prev else ''}"

    lanes = {l.strip() for l in args.lanes.split(",") if l.strip()}
    results = []
    for bench in selected:
        name = bench["name"]
        source = tests_dir / bench["hl"]
        timeout = bench["timeout_secs"] * 4
        row: dict = {"name": name, "lanes": {}}
        # A bench may pin engine configuration -- see `fib_calls`, which turns
        # off pure-call CSE so the row measures calls. It has to reach the AOT
        # compile too, not only the run: that is where the elimination happens.
        lane_env = dict(run_env)
        lane_env.update({str(k): str(v) for k, v in bench.get("env", {}).items()})

        if "ash-aot" not in lanes:
            row["lanes"]["ash-aot"] = {"status": "SKIP", "detail": "lane not selected"}
        else:
          with tempfile.TemporaryDirectory(prefix=f"aot-{name}-") as td:
            binary, detail, build_ms = build_aot(bench, tests_dir, repo, Path(td), lane_env,
                                                 profile=args.aot_profile, ash=ash)
            if binary is None:
                row["lanes"]["ash-aot"] = {"status": "FAIL", "detail": detail}
            else:
                rec = hlb.time_command([str(binary)], bench, args.iterations,
                                       args.warmups, timeout, lane_env)
                rec["build_ms"] = round(build_ms, 1)
                rec["peak_rss_bytes"] = peak_rss([str(binary)], timeout, lane_env)
                if detail:
                    rec["note"] = detail
                row["lanes"]["ash-aot"] = rec

        jit_cmd = [str(ash), "--mode", "jit", str(source)]
        if "ash-jit" not in lanes:
            row["lanes"]["ash-jit"] = {"status": "SKIP", "detail": "lane not selected"}
        else:
          row["lanes"]["ash-jit"] = hlb.time_command(
            jit_cmd, bench, args.iterations, args.warmups, timeout, lane_env)
        row["lanes"]["ash-jit"]["peak_rss_bytes"] = peak_rss(jit_cmd, timeout, lane_env)
        # Explicitly hybrid: `ash <file>` with no mode is the INTERPRETER,
        # and timing fib(40) under it measures the interpreter, not the
        # engine anyone ships. Hybrid is what the published table runs.
        hybrid_cmd = [str(ash), "--mode", "hybrid", str(source)]
        if "ash-hybrid" not in lanes:
            row["lanes"]["ash-hybrid"] = {"status": "SKIP", "detail": "lane not selected"}
        else:
          row["lanes"]["ash-hybrid"] = hlb.time_command(
            hybrid_cmd, bench, args.iterations, args.warmups, timeout, lane_env)
        row["lanes"]["ash-hybrid"]["peak_rss_bytes"] = peak_rss(hybrid_cmd, timeout, lane_env)

        if "hlc" not in lanes:
            row["lanes"]["hlc"] = {"status": "SKIP", "detail": "lane not selected"}
        elif not hlc_ready:
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

    if args.emit_partial:
        # Same record shape hl_bench.py writes, so the site merger needs no
        # third code path: one engine per record, judged the same way.
        records = []
        for row in results:
            rec = row["lanes"].get("ash-aot") or {}
            if rec.get("status") in (None, "SKIP"):
                continue
            out = {
                "benchmark": row["name"],
                "engine": "ash-aot",
                "status": rec["status"],
                "detail": rec.get("detail", ""),
            }
            if rec.get("wall_ms"):
                out["wall_ms"] = rec["wall_ms"]
            if rec.get("build_ms") is not None:
                # Reported, never timed: the page says "AOT build excluded",
                # and it means it -- this is what a developer waits for once,
                # not what the program costs to run.
                out["aot_build_ms"] = rec["build_ms"]
            if rec.get("peak_rss_bytes") is not None:
                out["peak_rss_bytes"] = rec["peak_rss_bytes"]
            records.append(out)
        args.emit_partial.write_text(json.dumps({
            "tool": "aot_bench",
            "host": payload["host"],
            "results": records,
        }, indent=2))
        print(f"wrote {args.emit_partial}")

    invalid = [r["name"] for r in results
               for rec in r["lanes"].values() if rec.get("status") == "INVALID"]
    if invalid:
        print(f"INVALID answers: {', '.join(sorted(set(invalid)))}")
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
