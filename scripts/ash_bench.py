#!/usr/bin/env python3
"""ASH benchmark runner.

Sweeps the .hl corpus in bench/benchmarks.toml across ASH's execution modes,
checks that each run produced the *right* answer, and writes both a readable
table and a machine-readable JSON baseline.

This is the canonical performance tool for the repo. scripts/run_perf_matrix.py
is a compatibility shim that forwards here.

  ./scripts/ash_bench.py --out-json bench/baselines/local.json
  ./scripts/ash_bench.py --baseline bench/baselines/local.json

See bench/README.md for what the metrics mean and why the numbers move.
"""

from __future__ import annotations

import argparse
import json

import os
import pathlib
import platform
import re
import shutil
import signal
import statistics
import subprocess
import sys
import time

try:
    import tomllib  # Python 3.11+
except ModuleNotFoundError as exc:  # pragma: no cover
    raise SystemExit(f"tomllib unavailable (need Python 3.11+): {exc}")


SCHEMA_VERSION = 1

# Statuses, worst last. Anything at or past INVALID is excluded from speedup
# and regression arithmetic: a configuration that produced the wrong answer has
# no meaningful runtime, and letting it into the comparison is exactly how a
# fast-but-wrong result gets mistaken for a win.
STATUS_OK = "OK"
STATUS_SKIP = "SKIP"
STATUS_INVALID = "INVALID"
STATUS_FAIL = "FAIL"
STATUS_TIMEOUT = "TIMEOUT"

COMPARABLE_STATUSES = {STATUS_OK}


# ── small utilities ─────────────────────────────────────────────────────────


def eprint(*a, **kw):
    print(*a, file=sys.stderr, **kw)


def repo_root_default() -> pathlib.Path:
    return pathlib.Path(__file__).resolve().parent.parent


def find_binary(repo_root: pathlib.Path, name: str) -> pathlib.Path | None:
    """Locate a built binary under target/.

    Nightly cargo drops ash_std (and therefore everything built alongside it)
    in target/debug, while .cargo/config.toml pins a target triple that would
    normally put it under target/<triple>/debug. Check both, newest first, so
    the runner does not silently measure a stale binary.
    """
    target = repo_root / "target"
    candidates: list[pathlib.Path] = []
    for profile in ("debug", "release"):
        candidates.append(target / profile / name)
        candidates.extend(sorted(target.glob(f"*/{profile}/{name}")))

    found = [p for p in candidates if p.is_file() and os.access(p, os.X_OK)]
    if not found:
        return None
    # Newest wins. Two profiles or two triples can both exist after a
    # cross-build; measuring whichever the glob happened to list first would
    # make results depend on directory order.
    found.sort(key=lambda p: p.stat().st_mtime, reverse=True)
    return found[0]


def git_info(repo_root: pathlib.Path) -> dict:
    def git(*args):
        try:
            out = subprocess.run(
                ["git", *args],
                cwd=repo_root,
                capture_output=True,
                text=True,
                timeout=15,
                check=False,
            )
            return out.stdout.strip() if out.returncode == 0 else None
        except (OSError, subprocess.SubprocessError):
            return None

    return {
        "commit": git("rev-parse", "HEAD"),
        "branch": git("rev-parse", "--abbrev-ref", "HEAD"),
        "dirty": bool(git("status", "--porcelain")),
    }


def system_info() -> dict:
    def sysctl(key):
        try:
            out = subprocess.run(
                ["sysctl", "-n", key], capture_output=True, text=True, timeout=10
            )
            return out.stdout.strip() if out.returncode == 0 else ""
        except (OSError, subprocess.SubprocessError):
            return ""

    cpu_model = ""
    ram_mb = 0
    if sys.platform == "darwin":
        cpu_model = sysctl("machdep.cpu.brand_string")
        try:
            ram_mb = int(sysctl("hw.memsize")) // (1024 * 1024)
        except ValueError:
            ram_mb = 0
    elif sys.platform.startswith("linux"):
        try:
            for line in pathlib.Path("/proc/cpuinfo").read_text().splitlines():
                if line.startswith("model name"):
                    cpu_model = line.split(":", 1)[1].strip()
                    break
            for line in pathlib.Path("/proc/meminfo").read_text().splitlines():
                if line.startswith("MemTotal:"):
                    ram_mb = int(line.split()[1]) // 1024
                    break
        except (OSError, ValueError, IndexError):
            pass

    return {
        "os": sys.platform,
        "arch": platform.machine(),
        "cpu_model": cpu_model or "unknown",
        # Physical cores where we can get them: a benchmark's contention story
        # is about cores, not hyperthreads.
        "cpu_count": os.cpu_count() or 1,
        "ram_mb": ram_mb,
        "hostname": platform.node(),
        "python": platform.python_version(),
    }


def load_snapshot() -> dict:
    try:
        one, five, fifteen = os.getloadavg()
    except (OSError, AttributeError):
        return {"available": False}
    return {"available": True, "load1": one, "load5": five, "load15": fifteen}


def summarize(samples: list[float]) -> dict | None:
    """min/median/mean/max/stddev over the measured wall times.

    A median alone cannot tell a noisy machine from a real change, which is
    what makes run-to-run comparison unreadable. Carry the spread so a reader
    can see when min and max straddle the number they are about to trust.
    """
    if not samples:
        return None
    s = sorted(samples)
    return {
        "min_ms": s[0],
        "median_ms": statistics.median(s),
        "mean_ms": statistics.fmean(s),
        "max_ms": s[-1],
        "stddev_ms": statistics.stdev(s) if len(s) > 1 else 0.0,
        "runs": len(s),
    }


# ── output parsing ──────────────────────────────────────────────────────────
#
# Every counter below is one ASH already prints. Parse it; do not re-derive it
# from the outside, because a number the runner computes for itself is a second
# implementation that can disagree with the VM's own.

# crates/ash_cli/src/main.rs, printed under --jit-log after the entrypoint
# returns:
#   [tiered] attempted=2 succeeded=1 failed=0 compiled_calls=19884 \
#            fallbacks=0 cranelift=1 llvm=0
TIERED_SUMMARY_RE = re.compile(
    r"^\[tiered\]\s+attempted=(\d+)\s+succeeded=(\d+)\s+failed=(\d+)\s+"
    r"compiled_calls=(\d+)\s+fallbacks=(\d+)\s+cranelift=(\d+)\s+llvm=(\d+)"
)
TIERED_FIELDS = (
    "attempted",
    "succeeded",
    "failed",
    "compiled_calls",
    "fallbacks",
    "cranelift",
    "llvm",
)

# std/src/gc.rs print_gc_stats_report(), under ASH_GC_STATS=1.
GC_PATTERNS = {
    "collections": re.compile(r"^\[gc\] collections:\s+(\d+)"),
    "blocks_reclaimed": re.compile(r"^\[gc\] blocks reclaimed:\s+(\d+)"),
    "live_blocks": re.compile(r"^\[gc\] live at last gc:\s+(\d+) blocks"),
}
GC_PAUSE_RE = re.compile(
    r"^\[gc\] pause total:\s+([\d.]+)ms, max ([\d.]+)ms, total (\d+)ns"
)
GC_ALLOC_RE = re.compile(
    r"^\[gc\] bytes allocated:\s+([\d.]+)MB \(\+ external ([\d.]+)MB\)"
)

# crates/ash_interp/src/interpreter.rs, one line per installed function under
# --jit-log / ASH_TIER_LOG=1:
#   [tier] install findex=255 tier=cranelift addr=0x... ops=13 in 5.84ms
# `name=` sits between findex and tier since the log started naming functions;
# it is optional here so this keeps parsing older logs too. Losing the match
# is silent -- every row simply reports no installs, which reads as "the tier
# never engaged" and is the one thing this parser exists to tell apart.
TIER_INSTALL_RE = re.compile(
    r"^\[tier\] install findex=(\d+)(?: name=\S+)? tier=(\w+)"
    r"(?:.*?\bin ([\d.]+)ms)?.*$"
)

# /usr/bin/time -l (macOS, bytes) and GNU time -v (Linux, kbytes).
RSS_BSD_RE = re.compile(r"^\s*(\d+)\s+maximum resident set size")
RSS_GNU_RE = re.compile(r"Maximum resident set size \(kbytes\):\s*(\d+)")


def parse_tiered(stderr: str) -> dict | None:
    """Last [tiered] summary wins.

    The summary is printed once, but a background promotion can log after it —
    take the last match rather than the first so a re-run that somehow emits
    two is not read from the stale one.
    """
    found = None
    for line in stderr.splitlines():
        m = TIERED_SUMMARY_RE.match(line.strip())
        if m:
            found = {k: int(v) for k, v in zip(TIERED_FIELDS, m.groups())}
    return found


def parse_tier_installs(stderr: str) -> list[dict]:
    """Per-function installs, including ones that land after the summary line.

    The `[tiered] ... llvm=N` counter is sampled when the entrypoint returns.
    LLVM promotion runs on a background thread, so on a short benchmark it
    routinely finishes *after* that sample and the counter reads 0 while an
    install line for it sits further down stderr. Collecting the install lines
    separately is what distinguishes "the top tier never engaged" from "it
    engaged too late to matter", which are different bugs.
    """
    out = []
    for line in stderr.splitlines():
        m = TIER_INSTALL_RE.match(line.strip())
        if m:
            out.append(
                {
                    "findex": int(m.group(1)),
                    "tier": m.group(2),
                    "compile_ms": float(m.group(3)) if m.group(3) else None,
                }
            )
    return out


def parse_gc(stderr: str) -> dict | None:
    out: dict = {}
    for line in stderr.splitlines():
        line = line.strip()
        for key, pat in GC_PATTERNS.items():
            m = pat.match(line)
            if m:
                out[key] = int(m.group(1))
        m = GC_PAUSE_RE.match(line)
        if m:
            out["pause_total_ms"] = float(m.group(1))
            out["pause_max_ms"] = float(m.group(2))
            out["pause_total_ns"] = int(m.group(3))
        m = GC_ALLOC_RE.match(line)
        if m:
            out["bytes_allocated_mb"] = float(m.group(1))
            out["external_bytes_mb"] = float(m.group(2))
    return out or None


def parse_rss_bytes(stderr: str) -> int | None:
    for line in stderr.splitlines():
        m = RSS_BSD_RE.match(line)
        if m:
            return int(m.group(1))  # macOS reports bytes
        m = RSS_GNU_RE.search(line)
        if m:
            return int(m.group(1)) * 1024  # GNU reports kbytes
    return None


def extract_checksum(stdout: str, marker: str = "Checksum:") -> str | None:
    """Pull the single value a benchmark reduces its work to.

    The marker is per-benchmark because some programs are verbatim copies of an
    upstream corpus — nbody prints `Energy:` — and editing their output to suit
    this runner would cost the comparability that made them worth copying.
    """
    for line in stdout.splitlines():
        if marker in line:
            return line.split(marker, 1)[1].strip()
    return None


def normalize_text(s: str, mode: str) -> str:
    """Mirror of normalize_text in crates/ash_cli/tests/common/mod.rs.

    Kept behaviourally identical on purpose: a benchmark that the parity suite
    calls correct must not be called incorrect here for a whitespace reason.
    """
    unix = s.replace("\r\n", "\n")
    if mode == "none":
        return unix
    if mode == "json_ws":
        return " ".join(unix.split())
    # "default"
    return "\n".join(line.rstrip() for line in unix.split("\n")).rstrip("\n")


# ── process execution ───────────────────────────────────────────────────────


class RunResult:
    __slots__ = ("returncode", "signal", "stdout", "stderr", "wall_ms", "timed_out")

    def __init__(self, returncode, sig, stdout, stderr, wall_ms, timed_out):
        self.returncode = returncode
        self.signal = sig
        self.stdout = stdout
        self.stderr = stderr
        self.wall_ms = wall_ms
        self.timed_out = timed_out

    @property
    def ok(self) -> bool:
        return not self.timed_out and self.returncode == 0 and self.signal is None


def exec_run(cmd: list[str], cwd: pathlib.Path, env: dict, timeout: float) -> RunResult:
    """Run one process and time it.

    Deliberately *not* wrapped in /usr/bin/time: BSD time(1) reports its
    child's status through WEXITSTATUS, which is meaningless for a
    signal-terminated child, so wrapping would erase exactly the SIGSEGV we
    need to record for the full-JIT game.hl case. RSS comes from a separate
    instrumented run instead — see run_instrumented.
    """
    start = time.perf_counter()
    try:
        p = subprocess.run(
            cmd,
            cwd=str(cwd),
            env=env,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=timeout,
            check=False,
        )
    except subprocess.TimeoutExpired as e:
        wall = (time.perf_counter() - start) * 1000.0
        return RunResult(
            None,
            None,
            (e.stdout or b"").decode("utf-8", "replace"),
            (e.stderr or b"").decode("utf-8", "replace"),
            wall,
            True,
        )
    except OSError as e:
        wall = (time.perf_counter() - start) * 1000.0
        return RunResult(None, None, "", f"exec failed: {e}", wall, False)

    wall = (time.perf_counter() - start) * 1000.0
    rc = p.returncode
    sig = None
    if rc is not None and rc < 0:
        sig = -rc
        rc = None
    return RunResult(
        rc,
        sig,
        p.stdout.decode("utf-8", "replace"),
        p.stderr.decode("utf-8", "replace"),
        wall,
        False,
    )


def time_wrapper() -> list[str] | None:
    """The argv prefix that yields a peak-RSS line, or None if unavailable."""
    t = "/usr/bin/time"
    if not os.path.exists(t):
        t = shutil.which("time") or ""
        if not t:
            return None
    return [t, "-l"] if sys.platform == "darwin" else [t, "-v"]


# ── manifest loading ────────────────────────────────────────────────────────


class Mode:
    def __init__(self, raw: dict):
        self.name: str = raw["name"]
        self.binary: str = raw.get("binary", "ash")
        self.args: list[str] = list(raw.get("args", []))
        self.default: bool = bool(raw.get("default", True))
        self.description: str = raw.get("description", "")
        # Per-mode environment overlay, for a configuration the CLI has no
        # flag for. It exists so an A/B of two engine settings can be two ROWS
        # of one sweep rather than two sweeps: the same commit swept twice
        # differs by up to 28% on this corpus, so only within-sweep ratios
        # mean anything. Applied under the per-bench overlay, which wins.
        self.env: dict[str, str] = {
            str(k): str(v) for k, v in (raw.get("env") or {}).items()
        }
        if self.binary != "ash":
            raise SystemExit(
                f"mode {self.name}: unknown binary {self.binary!r} — the CLI "
                f"is unified; every mode is a flag set on the `ash` binary"
            )

    @property
    def supports_flags(self) -> bool:
        """Every mode runs the unified `ash` CLI, which takes the full flag
        set (--quiet, --jit-log, ...) in every --mode."""
        return True


class Bench:
    def __init__(self, raw: dict, parity: dict, tests_dir: pathlib.Path):
        self.name: str = raw["name"]
        self.group: str = raw.get("group", "misc")
        self.description: str = raw.get("description", "")
        self.windowed: bool = bool(raw.get("windowed", False))
        self.parity_case: str | None = raw.get("parity_case")
        # Per-bench environment overlay. A benchmark sometimes has to measure
        # the engine in a configuration the engine does not ship in -- see
        # `fib_calls`, which suppresses pure-call CSE so the row reports call
        # cost rather than how much of the program the optimizer could prove
        # away. Applied on top of `build_env`, so it wins over the defaults.
        self.env: dict[str, str] = {
            str(k): str(v) for k, v in (raw.get("env") or {}).items()
        }

        base: dict = {}
        if self.parity_case:
            base = parity.get(self.parity_case) or {}
            if not base:
                raise SystemExit(
                    f"bench {self.name}: parity_case {self.parity_case!r} not found "
                    f"in parity_cases.toml"
                )

        self.hl_name: str = raw.get("hl") or base.get("hl", "")
        if not self.hl_name:
            raise SystemExit(f"bench {self.name}: no hl file")
        self.hl_path: pathlib.Path = (tests_dir / self.hl_name).resolve()

        self.expectation: str = raw.get("expectation") or base.get(
            "expectation", "exact"
        )
        self.normalize: str = raw.get("normalize") or base.get("normalize", "default")
        self.timeout_secs: float = float(
            raw.get("timeout_secs") or base.get("timeout_secs", 60)
        )
        self.slow: bool = bool(
            raw["slow"] if "slow" in raw else base.get("slow", False)
        )
        self.checksums: list[dict] = list(raw.get("checksums", []))
        self.checksum_marker: str = raw.get("checksum_marker", "Checksum:")
        # Optional allowlist. A benchmark that only says something under one
        # mode should not burn the timeout under the others just to record a
        # result nobody can read.
        self.modes: list[str] | None = raw.get("modes")
        self.modes_reason: str = raw.get("modes_reason", "not applicable to this mode")

        if self.expectation not in ("exact", "checksum", "exit_only"):
            raise SystemExit(
                f"bench {self.name}: bad expectation {self.expectation!r} "
                f"(want exact|checksum|exit_only)"
            )
        if self.expectation == "checksum" and not self.checksums:
            raise SystemExit(
                f"bench {self.name}: expectation=checksum needs a `checksums` list"
            )


def load_parity_cases(path: pathlib.Path) -> dict:
    if not path.exists():
        return {}
    data = tomllib.loads(path.read_text(encoding="utf-8"))
    return {c["name"]: c for c in data.get("case", [])}


def load_manifest(
    manifest: pathlib.Path, parity_file: pathlib.Path, tests_dir: pathlib.Path
) -> tuple[list[Mode], list[Bench]]:
    data = tomllib.loads(manifest.read_text(encoding="utf-8"))
    if data.get("schema_version") != SCHEMA_VERSION:
        raise SystemExit(
            f"{manifest}: schema_version {data.get('schema_version')} != "
            f"{SCHEMA_VERSION}"
        )
    parity = load_parity_cases(parity_file)
    modes = [Mode(m) for m in data.get("mode", [])]
    benches = [Bench(b, parity, tests_dir) for b in data.get("bench", [])]
    if not modes:
        raise SystemExit(f"{manifest}: no [[mode]] entries")
    if not benches:
        raise SystemExit(f"{manifest}: no [[bench]] entries")

    seen = set()
    for coll, kind in ((modes, "mode"), (benches, "bench")):
        for item in coll:
            key = (kind, item.name)
            if key in seen:
                raise SystemExit(f"duplicate {kind} name {item.name!r}")
            seen.add(key)
    return modes, benches


# ── correctness ─────────────────────────────────────────────────────────────


class Reference:
    """What a run's output is judged against."""

    def __init__(self, source: str, stdout: str | None, exit_code: int | None):
        self.source = source
        self.stdout = stdout
        self.exit_code = exit_code


def load_oracle(oracle_dir: pathlib.Path | None) -> dict:
    """Optional HashLink oracle bundle from scripts/generate_parity_oracle.py.

    HashLink itself does not run on Apple M-series, so on this machine the
    oracle is a CI artifact or nothing. When it is absent the runner falls back
    to `interp`, which is the mode the parity suite validates.
    """
    if not oracle_dir:
        return {}
    manifest = oracle_dir / "parity_manifest.json"
    if not manifest.exists():
        raise SystemExit(f"--oracle {oracle_dir}: no parity_manifest.json")
    data = json.loads(manifest.read_text(encoding="utf-8"))
    out = {}
    for case in data.get("cases", []):
        case_dir = oracle_dir / "cases" / case["name"]
        stdout_path = case_dir / "hashlink.stdout"
        out[case["name"]] = {
            "stdout": stdout_path.read_text(encoding="utf-8")
            if stdout_path.exists()
            else None,
            "exit_code": case.get("hashlink_exit_code"),
            "checksum": case.get("checksum"),
        }
    return out


def judge(bench: Bench, res: RunResult, ref: Reference | None) -> tuple[bool, str, dict]:
    """Return (ok, detail, checksum_record) for one completed run."""
    checksum_record: dict = {}

    if bench.expectation == "checksum":
        got = extract_checksum(res.stdout, bench.checksum_marker)
        accepted = {c["value"]: c.get("label", "?") for c in bench.checksums}
        checksum_record = {
            "value": got,
            "label": accepted.get(got),
            "accepted": got in accepted,
        }
        if got is None:
            return (
                False,
                f"no {bench.checksum_marker!r} line in stdout",
                checksum_record,
            )
        if got not in accepted:
            return (
                False,
                f"checksum {got} not in accepted set "
                f"{{{', '.join(f'{v} ({l})' for v, l in accepted.items())}}}",
                checksum_record,
            )
        return True, f"checksum {got} ({accepted[got]})", checksum_record

    if bench.expectation == "exit_only":
        return True, "exit code 0", checksum_record

    # exact
    if ref is None or ref.stdout is None:
        return False, "no reference output available", checksum_record
    got = normalize_text(res.stdout, bench.normalize)
    want = normalize_text(ref.stdout, bench.normalize)
    if got == want:
        return True, f"stdout matches {ref.source}", checksum_record
    return False, first_difference(want, got, ref.source), checksum_record


def fingerprint(bench: Bench, res: RunResult) -> str:
    """What this run produced, reduced to a comparable string.

    Used only to tell whether repeated runs of the same command agree with each
    other — a separate question from whether they agree with the reference. A
    row that is wrong every time in the *same* way is a miscompile; one that is
    wrong in a *different* way each time is reading uninitialized memory.
    """
    if bench.expectation == "checksum":
        return str(extract_checksum(res.stdout, bench.checksum_marker))
    if bench.expectation == "exit_only":
        return f"exit={res.returncode}"
    return normalize_text(res.stdout, bench.normalize)


def first_difference(want: str, got: str, source: str) -> str:
    w = want.split("\n")
    g = got.split("\n")
    for i in range(max(len(w), len(g))):
        a = w[i] if i < len(w) else "<missing>"
        b = g[i] if i < len(g) else "<missing>"
        if a != b:
            return (
                f"stdout differs from {source} at line {i + 1}: "
                f"expected {a[:80]!r}, got {b[:80]!r}"
            )
    return f"stdout differs from {source} (length only)"


# ── the sweep ───────────────────────────────────────────────────────────────


def build_env(args, instrumented: bool, bench=None, mode=None) -> dict:
    env = dict(os.environ)
    # Strip inherited knobs so a shell that happens to export them cannot
    # silently change what is measured.
    for k in ("ASH_GC_STATS", "ASH_TIER_LOG", "ASH_TIER", "ASH_NO_PURE_CSE", "ASH_AIR"):
        env.pop(k, None)
    if args.gc_heap_mb:
        env["ASH_GC_HEAP_MB"] = str(args.gc_heap_mb)
    if instrumented:
        if args.gc_stats:
            env["ASH_GC_STATS"] = "1"
        if args.tier_log:
            env["ASH_TIER_LOG"] = "1"
    if mode is not None:
        env.update(mode.env)
    if bench is not None:
        env.update(bench.env)
    return env


def command_for(mode: Mode, bench: Bench, binaries: dict, jit_log: bool) -> list[str]:
    exe = binaries[mode.binary]
    cmd = [str(exe)]
    if mode.supports_flags:
        cmd.extend(mode.args)
        if jit_log:
            cmd.append("--jit-log")
        cmd.append("--quiet")
    cmd.append(str(bench.hl_path))
    return cmd


def status_for(res: RunResult) -> str:
    if res.timed_out:
        return STATUS_TIMEOUT
    if res.signal is not None or res.returncode != 0:
        return STATUS_FAIL
    return STATUS_OK


def failure_detail(res: RunResult) -> str:
    if res.timed_out:
        return "timed out"
    if res.signal is not None:
        try:
            name = signal.Signals(res.signal).name
        except ValueError:
            name = f"signal {res.signal}"
        return f"killed by {name}"
    last = ""
    for line in reversed(res.stderr.splitlines()):
        if line.strip():
            last = line.strip()[:160]
            break
    return f"exit {res.returncode}" + (f": {last}" if last else "")


def resolve_reference(
    bench: Bench, binaries: dict, oracle: dict, args, cwd: pathlib.Path
) -> Reference | None:
    """Establish what this benchmark's correct output is.

    Order: HashLink oracle bundle > an `interp` run. Only `exact` benchmarks
    need one — `checksum` carries its accepted set in the manifest and
    `exit_only` asserts nothing about stdout — which matters, because the
    reference run is untimed but not free and Mandelbrot takes minutes under
    the interpreter.
    """
    if bench.expectation != "exact":
        return None

    if bench.parity_case and bench.parity_case in oracle:
        entry = oracle[bench.parity_case]
        if entry["stdout"] is not None:
            return Reference("hashlink-oracle", entry["stdout"], entry["exit_code"])

    ref_mode = Mode({"name": "interp", "args": ["--mode", "interp"]})
    cmd = command_for(ref_mode, bench, binaries, jit_log=False)
    res = exec_run(
        cmd, cwd, build_env(args, instrumented=False, bench=bench), bench.timeout_secs * args.timeout_scale
    )
    if not res.ok:
        return Reference("interp(failed)", None, None)
    return Reference("interp", res.stdout, res.returncode)


def run_instrumented(mode: Mode, bench: Bench, binaries: dict, args, cwd: pathlib.Path):
    """One extra run that collects counters and peak RSS.

    Separate from the timed runs on purpose. --jit-log and ASH_GC_STATS both
    add stderr writes on hot paths, and /usr/bin/time adds a process; folding
    any of that into the measured runs would put instrumentation cost inside
    the number being compared across baselines.
    """
    wrapper = time_wrapper()
    jit_log = mode.supports_flags
    cmd = command_for(mode, bench, binaries, jit_log=jit_log)
    if wrapper:
        cmd = wrapper + cmd
    res = exec_run(
        cmd,
        cwd,
        build_env(args, instrumented=True, bench=bench, mode=mode),
        bench.timeout_secs * args.timeout_scale,
    )
    return {
        "peak_rss_bytes": parse_rss_bytes(res.stderr) if wrapper else None,
        "tiered": parse_tiered(res.stderr),
        "tier_installs": parse_tier_installs(res.stderr),
        "gc": parse_gc(res.stderr) if args.gc_stats else None,
        "instrumented_ok": res.ok,
    }


def run_one(
    mode: Mode,
    bench: Bench,
    binaries: dict,
    ref: Reference | None,
    args,
    cwd: pathlib.Path,
) -> dict:
    timeout = bench.timeout_secs * args.timeout_scale
    env = build_env(args, instrumented=False, bench=bench, mode=mode)
    cmd = command_for(mode, bench, binaries, jit_log=False)

    record = {
        "benchmark": bench.name,
        "mode": mode.name,
        "group": bench.group,
        "binary": mode.binary,
        "hl": bench.hl_name,
        "command": " ".join(cmd),
        "expectation": bench.expectation,
        "timeout_secs": timeout,
        "status": STATUS_OK,
        "detail": "",
        "wall_ms": None,
        "samples_ms": [],
        "peak_rss_bytes": None,
        "exit_code": None,
        "signal": None,
        "checksum": None,
        # Whether every timed run agreed with every other. False means the JIT
        # produced a different answer on different runs of the same command.
        "output_stable": None,
        "reference_source": ref.source if ref else "manifest",
        "tiered": None,
        "tier_installs": [],
        "gc": None,
    }

    # Warmups: page cache, dyld, CPU frequency. NOT JIT warmup — every ASH run
    # is a fresh process and the tier ladder restarts from the interpreter each
    # time, so nothing a warmup run compiles survives into the next one.
    for _ in range(max(0, args.warmups)):
        res = exec_run(cmd, cwd, env, timeout)
        if res.timed_out:
            record["status"] = STATUS_TIMEOUT
            record["detail"] = f"warmup timed out after {timeout:.0f}s"
            return record

    # Every iteration is judged, not just the last one. ASH promotes on a
    # background thread, so a JIT mode can legitimately produce a different
    # answer on different runs of the same command — that is precisely the bug
    # worth catching, and checking only the final run would let it through
    # whenever the last iteration happened to be the correct one.
    samples: list[float] = []
    verdicts: list[tuple[bool, str, dict]] = []
    fingerprints: list[str] = []
    last: RunResult | None = None
    for _ in range(max(1, args.iterations)):
        res = exec_run(cmd, cwd, env, timeout)
        last = res
        st = status_for(res)
        if st != STATUS_OK:
            record["status"] = st
            record["detail"] = failure_detail(res)
            record["exit_code"] = res.returncode
            record["signal"] = res.signal
            record["samples_ms"] = samples
            record["wall_ms"] = summarize(samples)
            return record
        samples.append(res.wall_ms)
        verdicts.append(judge(bench, res, ref))
        fingerprints.append(fingerprint(bench, res))

    assert last is not None
    record["exit_code"] = last.returncode
    record["samples_ms"] = samples
    record["wall_ms"] = summarize(samples)

    failures = [v for v in verdicts if not v[0]]
    record["checksum"] = verdicts[-1][2] or None
    record["output_stable"] = len(set(fingerprints)) == 1

    if failures:
        record["status"] = STATUS_INVALID
        if len(failures) == len(verdicts) and record["output_stable"]:
            record["detail"] = failures[0][1]
        elif len(failures) == len(verdicts):
            # Wrong every time *and* differently every time. That is
            # uninitialized memory, not a deterministic miscompile, and the
            # distinction is the first thing anyone debugging it needs.
            record["detail"] = (
                f"wrong on all {len(verdicts)} runs, and differently each time "
                f"(unstable output): {failures[0][1]}"
            )
        else:
            record["detail"] = (
                f"{len(failures)}/{len(verdicts)} runs wrong (nondeterministic): "
                f"{failures[0][1]}"
            )
    else:
        record["detail"] = verdicts[-1][1]
        if not record["output_stable"]:
            record["detail"] += " [WARNING: output varied across runs]"

    inst = run_instrumented(mode, bench, binaries, args, cwd)
    record["peak_rss_bytes"] = inst["peak_rss_bytes"]
    record["tiered"] = inst["tiered"]
    record["tier_installs"] = inst["tier_installs"]
    record["gc"] = inst["gc"]
    return record


# ── reporting ───────────────────────────────────────────────────────────────


def fmt_ms(v) -> str:
    if v is None:
        return "-"
    if v >= 100000:
        return f"{v / 1000:.0f}s"
    if v >= 10000:
        return f"{v / 1000:.1f}s"
    return f"{v:.1f}"


def fmt_rss(v) -> str:
    if not v:
        return "-"
    return f"{v / (1024 * 1024):.0f}M"


def fmt_tier(rec: dict) -> str:
    t = rec.get("tiered")
    installs = rec.get("tier_installs") or []
    if not t and not installs:
        return "-"
    c = t["cranelift"] if t else 0
    l = t["llvm"] if t else 0
    # Installs that landed after the summary was sampled still count as the
    # tier having engaged; mark them so the two are not confused.
    late_c = sum(1 for i in installs if i["tier"] == "cranelift") - c
    late_l = sum(1 for i in installs if i["tier"] == "llvm") - l
    s = f"{c}/{l}"
    if late_c > 0 or late_l > 0:
        s += f"(+{max(late_c, 0)}/{max(late_l, 0)})"
    return s


def print_table(results: list[dict]) -> None:
    """Group by benchmark, preserving the order rows were produced in.

    `results` is already in manifest order (benchmarks outer, modes inner), so
    grouping on first sight reproduces that order without re-sorting.
    """
    grouped: list[tuple[str, list[dict]]] = []
    index: dict[str, list[dict]] = {}
    for r in results:
        if r["benchmark"] not in index:
            index[r["benchmark"]] = []
            grouped.append((r["benchmark"], index[r["benchmark"]]))
        index[r["benchmark"]].append(r)

    w_bench = max(9, max((len(b) for b, _ in grouped), default=9))
    w_mode = max(6, max((len(r["mode"]) for r in results), default=6))

    header = (
        f"{'BENCHMARK':<{w_bench}}  {'MODE':<{w_mode}}  {'STATUS':<8}  "
        f"{'MEDIAN':>9}  {'MIN':>9}  {'MAX':>9}  {'SD%':>5}  {'RSS':>6}  "
        f"{'SPEEDUP':>8}  {'TIER c/l':>10}  {'GC':>4}  NOTE"
    )
    print()
    print(header)
    print("-" * len(header))

    for bench_name, rows in grouped:
        # Speedup baseline is the first mode in manifest order that produced a
        # comparable result — normally `interp`.
        base = next(
            (
                r["wall_ms"]["median_ms"]
                for r in rows
                if r["status"] in COMPARABLE_STATUSES and r["wall_ms"]
            ),
            None,
        )

        for r in rows:
            wm = r["wall_ms"]
            median = wm["median_ms"] if wm else None
            sd = ""
            if wm and wm["median_ms"]:
                sd = f"{100.0 * wm['stddev_ms'] / wm['median_ms']:.1f}"
            speed = "-"
            if base and median and r["status"] in COMPARABLE_STATUSES:
                speed = f"{base / median:.2f}x"
            gc = r.get("gc") or {}
            gcs = str(gc.get("collections", "-")) if gc else "-"

            note = r["detail"]
            label = (r.get("checksum") or {}).get("label")
            if r["status"] == STATUS_OK and label:
                note = f"checksum {label}"
            print(
                f"{bench_name:<{w_bench}}  {r['mode']:<{w_mode}}  {r['status']:<8}  "
                f"{fmt_ms(median):>9}  {fmt_ms(wm['min_ms'] if wm else None):>9}  "
                f"{fmt_ms(wm['max_ms'] if wm else None):>9}  {sd:>5}  "
                f"{fmt_rss(r['peak_rss_bytes']):>6}  {speed:>8}  "
                f"{fmt_tier(r):>10}  {gcs:>4}  {note[:70]}"
            )
        print()


def print_summary(results: list[dict]) -> None:
    counts: dict[str, int] = {}
    for r in results:
        counts[r["status"]] = counts.get(r["status"], 0) + 1
    parts = [f"{k}={counts[k]}" for k in sorted(counts)]
    print(f"runs: {len(results)}  " + "  ".join(parts))

    bad = [r for r in results if r["status"] in (STATUS_INVALID,)]
    if bad:
        print()
        print("INVALID runs (wrong answer — excluded from all comparisons):")
        for r in bad:
            print(f"  {r['benchmark']}/{r['mode']}: {r['detail']}")

    failed = [r for r in results if r["status"] in (STATUS_FAIL, STATUS_TIMEOUT)]
    if failed:
        print()
        print("Failed / timed-out runs:")
        for r in failed:
            print(f"  {r['benchmark']}/{r['mode']}: {r['status']} — {r['detail']}")


# ── baseline comparison ─────────────────────────────────────────────────────


def index_results(doc: dict) -> dict:
    return {(r["benchmark"], r["mode"]): r for r in doc.get("results", [])}


def compare_baseline(
    baseline: dict,
    current: dict,
    threshold: float,
    improve_note: float,
    min_delta_ms: float,
) -> tuple[list[str], list[str]]:
    """Return (regressions, notes). A regression fails the gate.

    A pair must clear *both* the relative threshold and `min_delta_ms` to
    count. A relative-only gate is unusable on this corpus: most benchmarks
    finish in 15-25 ms, nearly all of which is VM startup, so two milliseconds
    of scheduler noise reads as a 10-40% "regression". The first comparison run
    flagged `basic/hybrid-cranelift` at +15.5% (17.9 -> 20.6 ms) and
    `jsonarr/interp` at +39.1% (16.5 -> 23.0 ms) on an otherwise idle machine —
    neither is a change anyone could act on.
    """
    old = index_results(baseline)
    new = index_results(current)
    regressions: list[str] = []
    notes: list[str] = []

    print()
    print(f"Baseline comparison (regression threshold: +{threshold * 100:.0f}%)")
    hdr = (
        f"{'BENCHMARK':<18}  {'MODE':<17}  {'BASE':>10}  {'NEW':>10}  "
        f"{'DELTA':>9}  VERDICT"
    )
    print(hdr)
    print("-" * len(hdr))

    # Current-run order first (which is manifest order), then anything that
    # exists only in the baseline. Sorting alphabetically here would be
    # deterministic but would stop the comparison lining up with the table
    # printed directly above it.
    keys = list(new.keys()) + [k for k in old if k not in new]

    for key in keys:
        bench, mode = key
        o = old.get(key)
        n = new.get(key)
        if o is None:
            notes.append(f"{bench}/{mode}: new run, absent from baseline")
            print(f"{bench:<18}  {mode:<17}  {'-':>10}  "
                  f"{fmt_ms((n['wall_ms'] or {}).get('median_ms') if n else None):>10}  "
                  f"{'-':>9}  NEW")
            continue
        if n is None:
            notes.append(f"{bench}/{mode}: in baseline but not run")
            print(f"{bench:<18}  {mode:<17}  "
                  f"{fmt_ms((o['wall_ms'] or {}).get('median_ms')):>10}  "
                  f"{'-':>10}  {'-':>9}  MISSING")
            continue

        # A status regression is a regression regardless of the clock. A mode
        # that used to answer correctly and now crashes, times out, or answers
        # wrongly has not got faster, whatever its wall time says.
        if o["status"] in COMPARABLE_STATUSES and n["status"] not in COMPARABLE_STATUSES:
            msg = f"{bench}/{mode}: status {o['status']} -> {n['status']} ({n['detail']})"
            regressions.append(msg)
            print(f"{bench:<18}  {mode:<17}  {'':>10}  {'':>10}  {'':>9}  "
                  f"REGRESSED ({o['status']} -> {n['status']})")
            continue
        if n["status"] not in COMPARABLE_STATUSES:
            print(f"{bench:<18}  {mode:<17}  {'':>10}  {'':>10}  {'':>9}  "
                  f"{n['status']} (also in baseline)")
            continue
        if o["status"] not in COMPARABLE_STATUSES:
            notes.append(f"{bench}/{mode}: {o['status']} -> {n['status']} (recovered)")
            print(f"{bench:<18}  {mode:<17}  {'':>10}  "
                  f"{fmt_ms(n['wall_ms']['median_ms']):>10}  {'':>9}  RECOVERED")
            continue

        ob = (o["wall_ms"] or {}).get("median_ms")
        nb = (n["wall_ms"] or {}).get("median_ms")
        if not ob or not nb:
            continue
        delta = (nb - ob) / ob
        abs_delta = nb - ob
        verdict = "ok"
        if delta > threshold and abs_delta > min_delta_ms:
            verdict = "REGRESSED"
            regressions.append(
                f"{bench}/{mode}: median {ob:.1f}ms -> {nb:.1f}ms "
                f"({delta * 100:+.1f}%, {abs_delta:+.1f}ms)"
            )
        elif delta > threshold:
            # Over the relative bar but under the absolute one. Say so rather
            # than printing a bare "ok", so a real regression on a fast
            # benchmark is not invisible while it grows.
            verdict = f"noise (+{abs_delta:.1f}ms < {min_delta_ms:.0f}ms floor)"
        elif delta < -improve_note and -abs_delta > min_delta_ms:
            verdict = "improved"

        # A checksum that changed label means the FP fusion policy moved, which
        # is a behaviour change even when the time is flat.
        oc = (o.get("checksum") or {}).get("label")
        nc = (n.get("checksum") or {}).get("label")
        if oc != nc:
            notes.append(f"{bench}/{mode}: checksum label {oc} -> {nc}")
            verdict += " [checksum label changed]"

        print(
            f"{bench:<18}  {mode:<17}  {fmt_ms(ob):>10}  {fmt_ms(nb):>10}  "
            f"{delta * 100:>+8.1f}%  {verdict}"
        )

    return regressions, notes


# ── main ────────────────────────────────────────────────────────────────────


def parse_args(argv=None):
    p = argparse.ArgumentParser(
        description="ASH benchmark runner",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    p.add_argument("--repo-root", default=str(repo_root_default()))
    p.add_argument(
        "--manifest",
        default=None,
        help="benchmark corpus (default: <repo>/bench/benchmarks.toml)",
    )
    p.add_argument("--out-json", default=None, help="write results here")
    p.add_argument(
        "--baseline",
        default=None,
        help="compare this run against a previous --out-json file and gate on "
        "regressions",
    )
    p.add_argument(
        "--regress-threshold",
        type=float,
        default=0.10,
        help="fractional median slowdown that counts as a regression "
        "(default 0.10 = 10%%)",
    )
    p.add_argument(
        "--improve-threshold",
        type=float,
        default=0.05,
        help="fractional speedup worth labelling 'improved' (default 0.05)",
    )
    p.add_argument(
        "--regress-min-ms",
        type=float,
        default=5.0,
        help="absolute median delta a regression must also clear, in ms "
        "(default 5.0). Guards against scheduler noise on benchmarks that "
        "finish near the ~16ms VM startup floor",
    )
    p.add_argument("--iterations", type=int, default=5)
    p.add_argument("--warmups", type=int, default=1)
    p.add_argument(
        "--benchmarks",
        default=None,
        help="comma-separated benchmark names (default: all non-slow, "
        "non-windowed)",
    )
    p.add_argument(
        "--modes",
        default=None,
        help="comma-separated mode names (default: every mode not marked "
        "default=false)",
    )
    p.add_argument("--group", default=None, help="only benchmarks in this group")
    p.add_argument("--include-slow", action="store_true")
    p.add_argument(
        "--include-windowed",
        action="store_true",
        help="include benchmarks that open a window; see bench/README.md for "
        "why their numbers are not comparable",
    )
    p.add_argument(
        "--timeout-scale",
        type=float,
        default=1.0,
        help="multiply every per-run timeout by this (default 1.0)",
    )
    p.add_argument("--gc-stats", action="store_true", help="ASH_GC_STATS=1")
    p.add_argument("--tier-log", action="store_true", help="ASH_TIER_LOG=1")
    p.add_argument("--gc-heap-mb", type=int, default=None, help="ASH_GC_HEAP_MB")
    p.add_argument(
        "--oracle",
        default=None,
        help="HashLink oracle bundle dir from scripts/generate_parity_oracle.py",
    )
    p.add_argument("--build", action="store_true", help="cargo build before running")
    p.add_argument(
        "--load-threshold",
        type=float,
        default=0.4,
        help="warn when loadavg/cores exceeds this before starting (default 0.4)",
    )
    p.add_argument(
        "--ignore-load",
        action="store_true",
        help="do not warn about machine load (the numbers are still affected)",
    )
    p.add_argument(
        "--min-speedup",
        type=float,
        default=0.0,
        help="fail unless every benchmark's best JIT mode beats interp by this "
        "factor (0 disables)",
    )
    p.add_argument(
        "--no-gate",
        action="store_true",
        help="report regressions but always exit 0",
    )
    p.add_argument("--list", action="store_true", help="list modes/benches and exit")
    return p.parse_args(argv)


def cargo_build(repo_root: pathlib.Path, needed: set[str]) -> None:
    """Build only what the selected modes require.

    ash_std comes first and unconditionally: it is a cdylib embedded into `ash`
    via include_bytes! at build time, so building `ash` against a stale ash_std
    silently measures an old standard library. `ash` itself is skipped unless a
    selected mode uses it — linking LLVM is the expensive part of this build and
    an interpreter-only sweep has no use for it.
    """
    pkgs = ["ash_std"]
    if needed:
        # The unified `ash` package builds the binary; the ash_core
        # library (and its LLVM link) comes in as its dependency.
        pkgs.append("ash")

    build_target = os.environ.get("CARGO_BUILD_TARGET")
    for pkg in pkgs:
        cmd = ["cargo", "build", "-q", "-p", pkg]
        if build_target:
            cmd.extend(["--target", build_target])
        eprint(f"[build] {' '.join(cmd)}")
        r = subprocess.run(cmd, cwd=str(repo_root), check=False)
        if r.returncode != 0:
            raise SystemExit(f"{' '.join(cmd)} failed")


def main(argv=None) -> int:
    args = parse_args(argv)
    repo_root = pathlib.Path(args.repo_root).resolve()
    tests_dir = repo_root / "crates" / "ash" / "test" / "tests"
    manifest_path = (
        pathlib.Path(args.manifest).resolve()
        if args.manifest
        else repo_root / "bench" / "benchmarks.toml"
    )
    parity_file = tests_dir / "parity_cases.toml"

    modes, benches = load_manifest(manifest_path, parity_file, tests_dir)

    if args.modes:
        want = [m.strip() for m in args.modes.split(",") if m.strip()]
        known = {m.name for m in modes}
        unknown = [m for m in want if m not in known]
        if unknown:
            raise SystemExit(f"unknown mode(s): {', '.join(unknown)}")
        modes = [m for m in modes if m.name in want]
    else:
        modes = [m for m in modes if m.default]

    if args.benchmarks:
        want = [b.strip() for b in args.benchmarks.split(",") if b.strip()]
        known = {b.name for b in benches}
        unknown = [b for b in want if b not in known]
        if unknown:
            raise SystemExit(f"unknown benchmark(s): {', '.join(unknown)}")
        benches = [b for b in benches if b.name in want]
    else:
        benches = [
            b
            for b in benches
            if (args.include_slow or not b.slow)
            and (args.include_windowed or not b.windowed)
        ]
    if args.group:
        benches = [b for b in benches if b.group == args.group]

    if args.list:
        print("modes:")
        for m in modes:
            print(f"  {m.name:<18} {m.binary:<8} {' '.join(m.args)}")
            if m.description:
                print(f"  {'':<18} {m.description}")
        print("benchmarks:")
        for b in benches:
            flags = []
            if b.slow:
                flags.append("slow")
            if b.windowed:
                flags.append("windowed")
            print(
                f"  {b.name:<18} {b.hl_name:<34} {b.expectation:<10} "
                f"{'/'.join(flags)}"
            )
        return 0

    if not modes or not benches:
        raise SystemExit("nothing selected")

    if args.build:
        cargo_build(repo_root, {m.binary for m in modes} | {"ash"})

    # One binary serves every mode, including the untimed interp reference
    # run that `exact` benchmarks are judged against.
    binaries: dict[str, pathlib.Path] = {}
    found = find_binary(repo_root, "ash")
    if found is not None:
        binaries["ash"] = found

    needed = {m.binary for m in modes} | {"ash"}
    missing = sorted(needed - set(binaries))
    if missing:
        raise SystemExit(
            f"binary/binaries {', '.join(missing)} not found under "
            f"{repo_root / 'target'}. Run with --build, or:\n"
            f"  cargo build -p ash_std && cargo build -p ash"
        )

    oracle = load_oracle(pathlib.Path(args.oracle).resolve() if args.oracle else None)

    sysinfo = system_info()
    load_start = load_snapshot()
    load_warning = False
    if load_start.get("available") and not args.ignore_load:
        ratio = load_start["load1"] / max(1, sysinfo["cpu_count"])
        if ratio > args.load_threshold:
            load_warning = True
            eprint(
                f"WARNING: 1-minute load average is {load_start['load1']:.2f} on "
                f"{sysinfo['cpu_count']} cores ({ratio * 100:.0f}% busy). Timings "
                f"taken now are not comparable to an idle-machine baseline — a "
                f"6.7s pre-warm on this repo has been observed inflating to 18.5s "
                f"under a concurrent cargo build. Wait for the machine to settle, "
                f"or pass --ignore-load to proceed anyway."
            )

    eprint(f"[bench] ash: {binaries.get('ash', '-')}")
    eprint(
        f"[bench] {len(benches)} benchmark(s) x {len(modes)} mode(s), "
        f"{args.warmups} warmup + {args.iterations} timed run(s) each"
    )

    results: list[dict] = []
    started = time.time()

    # Deterministic order: manifest order for benchmarks, manifest order for
    # modes within each benchmark. Sequential by construction — see
    # bench/README.md on why there is no --jobs.
    for bench in benches:
        if not bench.hl_path.exists():
            eprint(f"[bench] {bench.name}: SKIP (missing {bench.hl_path})")
            results.append(
                {
                    "benchmark": bench.name,
                    "mode": "-",
                    "group": bench.group,
                    "status": STATUS_SKIP,
                    "detail": f"missing {bench.hl_path}",
                    "wall_ms": None,
                    "samples_ms": [],
                    "peak_rss_bytes": None,
                    "checksum": None,
                    "tiered": None,
                    "tier_installs": [],
                    "gc": None,
                }
            )
            continue

        ref = resolve_reference(bench, binaries, oracle, args, tests_dir)
        if ref and ref.stdout is None and bench.expectation == "exact":
            eprint(
                f"[bench] {bench.name}: WARNING no reference output "
                f"({ref.source}); correctness cannot be judged"
            )

        for mode in modes:
            if bench.modes is not None and mode.name not in bench.modes:
                eprint(f"[bench] {bench.name} / {mode.name} ... SKIP")
                results.append(
                    {
                        "benchmark": bench.name,
                        "mode": mode.name,
                        "group": bench.group,
                        "status": STATUS_SKIP,
                        "detail": bench.modes_reason,
                        "wall_ms": None,
                        "samples_ms": [],
                        "peak_rss_bytes": None,
                        "checksum": None,
                        "tiered": None,
                        "tier_installs": [],
                        "gc": None,
                    }
                )
                continue
            eprint(f"[bench] {bench.name} / {mode.name} ... ", end="", flush=True)
            rec = run_one(mode, bench, binaries, ref, args, tests_dir)
            results.append(rec)
            med = (rec["wall_ms"] or {}).get("median_ms")
            eprint(
                f"{rec['status']}"
                + (f" {med:.1f}ms" if med else "")
                + (f" ({rec['detail'][:60]})" if rec["detail"] else "")
            )

    elapsed = time.time() - started
    load_end = load_snapshot()

    doc = {
        "schema_version": SCHEMA_VERSION,
        "generated_unix": int(started),
        "generated_iso": time.strftime("%Y-%m-%dT%H:%M:%S%z", time.localtime(started)),
        "sweep_seconds": round(elapsed, 2),
        "git": git_info(repo_root),
        "system": sysinfo,
        "load": {
            "at_start": load_start,
            "at_end": load_end,
            "threshold": args.load_threshold,
            "warning_raised": load_warning,
        },
        "config": {
            "manifest": str(manifest_path.relative_to(repo_root))
            if manifest_path.is_relative_to(repo_root)
            else str(manifest_path),
            "iterations": args.iterations,
            "warmups": args.warmups,
            "timeout_scale": args.timeout_scale,
            "include_slow": args.include_slow,
            "include_windowed": args.include_windowed,
            "gc_stats": args.gc_stats,
            "tier_log": args.tier_log,
            "gc_heap_mb": args.gc_heap_mb,
            "oracle": args.oracle,
            "modes": [m.name for m in modes],
            "benchmarks": [b.name for b in benches],
            "sequential": True,
        },
        "binaries": {k: str(v) for k, v in binaries.items()},
        "results": results,
    }

    print_table(results)
    print_summary(results)
    print()
    print(f"sweep completed in {elapsed:.1f}s")

    if args.out_json:
        out = pathlib.Path(args.out_json).resolve()
        out.parent.mkdir(parents=True, exist_ok=True)
        out.write_text(json.dumps(doc, indent=2, sort_keys=True) + "\n", encoding="utf-8")
        print(f"results written: {out}")

    exit_code = 0
    regressions: list[str] = []

    if args.baseline:
        bpath = pathlib.Path(args.baseline).resolve()
        if not bpath.exists():
            raise SystemExit(f"--baseline {bpath}: no such file")
        baseline = json.loads(bpath.read_text(encoding="utf-8"))
        if baseline.get("schema_version") != SCHEMA_VERSION:
            raise SystemExit(
                f"{bpath}: schema_version {baseline.get('schema_version')} != "
                f"{SCHEMA_VERSION}"
            )
        bsys = baseline.get("system", {})
        if bsys.get("cpu_model") != sysinfo["cpu_model"]:
            print()
            print(
                f"NOTE: baseline was taken on {bsys.get('cpu_model', '?')!r}, this "
                f"run on {sysinfo['cpu_model']!r}. Deltas across different CPUs "
                f"are not meaningful."
            )
        regressions, notes = compare_baseline(
            baseline,
            doc,
            args.regress_threshold,
            args.improve_threshold,
            args.regress_min_ms,
        )
        if notes:
            print()
            for n in notes:
                print(f"note: {n}")

    # Correctness is always a gate: an INVALID run means a configuration
    # produced the wrong answer, which no benchmark result can excuse.
    invalid = [r for r in results if r["status"] == STATUS_INVALID]
    for r in invalid:
        regressions.append(f"{r['benchmark']}/{r['mode']}: INVALID — {r['detail']}")

    if args.min_speedup > 0:
        by_bench: dict[str, dict[str, dict]] = {}
        for r in results:
            by_bench.setdefault(r["benchmark"], {})[r["mode"]] = r
        for bench_name, rows in sorted(by_bench.items()):
            base = rows.get("interp")
            if not base or base["status"] not in COMPARABLE_STATUSES:
                continue
            jit_rows = [
                r
                for name, r in rows.items()
                if name != "interp" and r["status"] in COMPARABLE_STATUSES and r["wall_ms"]
            ]
            if not jit_rows:
                continue
            best = min(jit_rows, key=lambda r: r["wall_ms"]["median_ms"])
            speedup = base["wall_ms"]["median_ms"] / best["wall_ms"]["median_ms"]
            if speedup < args.min_speedup:
                regressions.append(
                    f"{bench_name}: best speedup {speedup:.3f}x ({best['mode']}) "
                    f"< --min-speedup {args.min_speedup:.3f}"
                )

    if regressions:
        print()
        print("GATE FAILURES:")
        for r in regressions:
            print(f"  {r}")
        exit_code = 0 if args.no_gate else 1
    elif args.baseline or args.min_speedup > 0 or invalid:
        print()
        print("gate: pass")

    return exit_code


if __name__ == "__main__":
    sys.exit(main())
