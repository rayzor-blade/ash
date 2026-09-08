#!/usr/bin/env python3
"""Reproduce the retention bug, and say what the run actually showed.

The bug is **false retention**: collections run, stop the world in under a
millisecond, mark for hundreds of milliseconds, and free nothing, while the
live set is a few megabytes. The heap climbs to its cap and a thread raises
out-of-memory. Everything else follows from that.

What this script measures is therefore *what a collection reclaimed*, not
whether the program hung:

    freed nothing at a full heap   the bug
    out of memory                  the bug, caught one step later
    abandoned world stops          downstream, and often a stale mutator
                                   belonging to a worker that already exited
    finished                       clean

An earlier version of this script called "out of memory with no abandoned
stop" an honest full heap and told you not to report it. That was wrong, and
wrong in the expensive direction: allocation throughput is not live memory,
and a collection that frees zero blocks with a 2 MB live set is the defect
itself. Judge a run by `freed=`, not by how it died.

Usage:

    scripts/gc_straggler_repro.py                 # 6 runs, report
    scripts/gc_straggler_repro.py --runs 20
    scripts/gc_straggler_repro.py --sample        # macOS: stacks when it hangs
    scripts/gc_straggler_repro.py --host node     # the browser host instead

Exit status is 1 if the bug appeared, which makes it usable as a bisect
predicate -- inverted, since here "failed" is what you are looking for.
"""

import argparse
import os
import pathlib
import re
import shutil
import subprocess
import sys
import time

ROOT = pathlib.Path(__file__).resolve().parent.parent
MODULE = ROOT / "examples/browser/entities.wasm"
SOURCE = ROOT / "examples/browser/demo"
RUNNER = ROOT / "target/release/ash-wasm-run"

# Four threads for twenty seconds with the frame cap off. The cap is what a
# page runs with and what makes the demo well behaved; taking it off is what
# puts four threads on one heap at full allocation rate. Twenty seconds
# matters: five-second runs pass every time, which is how this was first
# mistaken for fixed.
THREADS = "4"
SECONDS = "20"
# The rate the drawing threads hold themselves to. A page runs at 60, which
# is well behaved; the point here is to allocate hard enough to stop the world
# constantly. Flat out (100000) also outruns the collector, so a run can die
# of an honestly full heap having reproduced nothing -- and the cap cannot be
# raised past ~512 MB, because a 32-bit address space will not reserve it.
# A few hundred frames a second collects often and still fits.
DEFAULT_FPS = "400"


def build_hint() -> str:
    return (
        f"Build it first:\n"
        f"  ./scripts/build_wasm_runtime.py --target wasm32-wasip1-threads\n"
        f"  cd {SOURCE} && haxe -cp . -main Entities -hl entities.hl\n"
        f"  ASH_WASM_FIBERS=1 ash --build ../entities.wasm \\\n"
        f"      --target wasm32-wasip1-threads entities.hl\n"
        f"  cargo build --release -p ash_wasm_runtime"
    )


# `[gc] #12 origin=... pause=..ms freed=N blocks live=N blocks (N MB) ...`
COLLECTION = re.compile(
    r"\[gc\] #(\d+) origin=(\S+) pause=(\S+)ms freed=(\d+) blocks live=(\d+) blocks"
)


def classify(output: str, timed_out: bool) -> tuple[str, str]:
    """(verdict, detail) for one run, judged on what was reclaimed."""
    collections = COLLECTION.findall(output)
    abandoned = len(re.findall(r"gave up stopping the world", output))
    oom = "out of memory" in output
    finished = "band-frames in" in output

    # A collection that frees nothing while holding a large heap is the
    # defect, whether or not the run went on to die of it.
    barren = [c for c in collections if int(c[3]) == 0]
    peak_live = max((int(c[4]) for c in collections), default=0)
    worst = max((int(c[4]) for c in barren), default=0)

    where = ""
    if abandoned:
        # Ordering matters for attribution: an abandoned stop after a worker
        # has exited is a stale registration, not a live thread refusing to
        # stop. Compare first occurrences.
        first_abandon = output.find("gave up stopping the world")
        first_death = min(
            (i for i in (output.find("out of memory"), output.find("proc_exit")) if i >= 0),
            default=-1,
        )
        stale = 0 <= first_death < first_abandon
        where = f", {abandoned} stop(s) abandoned{' after a worker exited' if stale else ''}"

    if barren:
        return (
            "BUG",
            f"{len(barren)} of {len(collections)} collections freed nothing, "
            f"worst at {worst * 32 // 1024} MB live{where}",
        )
    if oom:
        return (
            "BUG",
            f"out of memory after {len(collections)} collections, "
            f"peak {peak_live * 32 // 1024} MB live{where}",
        )
    if timed_out:
        return ("hung", f"{len(collections)} collections, none barren{where}")
    if finished:
        return (
            "clean",
            f"{len(collections)} collections, peak {peak_live * 32 // 1024} MB live",
        )
    return ("odd", "no recognised outcome; read the log")


def sample_pid(pid: int, seconds: int, out: pathlib.Path) -> None:
    """macOS `sample`, for the state of every thread while it is stuck.

    The pid is the child this script started. Looking one up by name instead
    matches the shell wrapper as readily as the runner, and sampling the
    wrong process reports one thread sitting in `__sigsuspend`.

    It will not name guest frames: wasmtime symbolicates its own trap
    backtraces from the module's name section, but an external sampler walks
    JIT-compiled anonymous memory and sees `???`. What it does show is which
    threads are blocked (a futex or `__psynch_cvwait` leaf), which are
    running, and -- the reason it earned its place here -- which have gone.
    """
    if sys.platform != "darwin" or not shutil.which("sample"):
        return
    subprocess.run(
        ["sample", str(pid), str(seconds), "-file", str(out)],
        capture_output=True,
    )
    if not out.exists():
        print(f"    (sample of pid {pid} produced nothing)", file=sys.stderr)


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--runs", type=int, default=6)
    ap.add_argument(
        "--host",
        choices=["wasmtime", "node"],
        default="wasmtime",
        help="wasmtime symbolicates and reproduces about 2 runs in 3; node is "
        "the browser host, reproduces about 3 in 4, and gives ??? frames",
    )
    ap.add_argument(
        "--sample",
        action="store_true",
        help="macOS only: capture every thread's stack during the run. The "
        "straggler is the point of this -- under wasmtime its frames have "
        "names.",
    )
    ap.add_argument(
        "--threads",
        default=THREADS,
        help=f"Haxe threads, one drawing band each (default {THREADS})",
    )
    ap.add_argument(
        "--fps",
        default=DEFAULT_FPS,
        help="rate the drawing threads hold themselves to; higher allocates "
        f"harder (default {DEFAULT_FPS}, 100000 is flat out)",
    )
    ap.add_argument("--logs", default=None, help="directory for per-run logs")
    args = ap.parse_args()

    if not MODULE.exists():
        print(f"missing {MODULE}\n\n{build_hint()}", file=sys.stderr)
        return 2

    logs = pathlib.Path(args.logs) if args.logs else ROOT / "target/gc-straggler"
    logs.mkdir(parents=True, exist_ok=True)

    if args.host == "wasmtime":
        if not RUNNER.exists():
            print(f"missing {RUNNER}\n\n{build_hint()}", file=sys.stderr)
            return 2
        command = [str(RUNNER), "--threads", str(MODULE), args.threads, SECONDS, args.fps]
        process_name = "ash-wasm-run"
    else:
        host = os.environ.get("ASH_BROWSER_HOST_DIR", "/tmp/ashnode")
        runner = pathlib.Path(host) / "run-node.js"
        if not runner.exists():
            print(
                f"missing {runner}. The browser host under node is built with:\n"
                f"  cargo build --release -p ash_browser "
                f"--target wasm32-unknown-unknown\n"
                f"  wasm-bindgen --target nodejs --out-dir {host} \\\n"
                f"      target/wasm32-unknown-unknown/release/ash_browser.wasm\n"
                f"  cp examples/browser/run-node*.js {host}/",
                file=sys.stderr,
            )
            return 2
        command = [
            "node",
            "--experimental-wasm-exnref",
            str(runner),
            "--agents",
            "7",
            str(MODULE),
            args.threads,
            SECONDS,
            args.fps,
        ]
        process_name = "node"

    # The counters and the mutator table are only printed with stats on, and
    # they are the whole diagnosis.
    env = dict(os.environ, ASH_GC_STATS="1")
    # Generous: the program asks for twenty seconds and a stuck one never
    # ends. Long enough for several abandoned stops to be recorded first.
    limit = int(SECONDS) * 3 + 20

    bugs = 0
    print(f"{args.runs} runs, {args.host}, {args.threads} threads x {SECONDS}s at {args.fps}fps\n")
    for run in range(1, args.runs + 1):
        log = logs / f"run{run}.log"
        with log.open("w") as sink:
            proc = subprocess.Popen(command, stdout=sink, stderr=subprocess.STDOUT, env=env)
            if args.sample:
                sample_pid(proc.pid, limit - 5, logs / f"run{run}.sample.txt")
            try:
                proc.wait(timeout=limit)
                timed_out = False
            except subprocess.TimeoutExpired:
                proc.kill()
                proc.wait()
                timed_out = True

        verdict, detail = classify(log.read_text(errors="replace"), timed_out)
        if verdict == "BUG":
            bugs += 1
        print(f"  run {run}: {verdict:12s} {detail}")

    print(f"\n{bugs} of {args.runs} reproduced the bug. Logs in {logs}")
    if bugs:
        print(
            "\nWhat to read first, in a log that says BUG:\n"
            "  grep 'origin=' -- the collections. `freed=0 blocks` beside a\n"
            "  large `live=` is the defect: the mark phase is keeping objects\n"
            "  nothing references.\n"
            "  grep 'mutators:' -- only if stops were abandoned, and check\n"
            "  whether a worker had already exited first. A mutator whose\n"
            "  thread is gone reads exactly like one that refuses to stop:\n"
            "  polls=0, at=running."
        )
    return 1 if bugs else 0


if __name__ == "__main__":
    sys.exit(main())
