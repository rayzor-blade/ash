#!/usr/bin/env python3
"""Regenerate crates/ash_cli/startup-order.txt, the order lld lays out the
functions ash runs first.

ash links LLVM statically, and LLVM's static constructors and the code they
call are spread across the executable. The kernel maps a 64 KB cluster around
every page a process faults in, so running them at startup makes tens of
megabytes of code resident. Listing the functions startup and the first
compiles execute, in the order they first run, puts them on a few pages.

Needs Linux x86_64 with `perf` and Intel PT (`intel_pt` in
/sys/bus/event_source/devices). Build ash first; the trace reads the symbols
of that binary. Each workload runs twice because the trace buffer can drop
data, and a function seen in either run is kept.

    scripts/startup_order.py [--ash target/release/ash]
"""

import argparse
import os
import subprocess
import sys
import tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
TESTS = os.path.join(ROOT, "crates/ash/test/tests")
OUT = os.path.join(ROOT, "crates/ash_cli/startup-order.txt")

# Startup first, so the interpreter's path is densest; then the compilers.
# Short programs: a trace grows by about a gigabyte per second of CPU.
WORKLOADS = [
    (["--mode", "interp"], "test_dynamic.hl"),
    (["--mode", "jit"], "test_dynamic.hl"),
    (["--mode", "hybrid"], "test_dynamic.hl"),
    (["--mode", "hybrid", "--jit-threshold", "1"], "test_dynamic.hl"),
    (["--mode", "hybrid"], "bench_fib.hl"),
]


def functions_run(ash, flags, program, tmp):
    """The functions of `ash` a run enters, in the order it first enters them."""
    data = os.path.join(tmp, "pt.data")
    subprocess.run(
        ["perf", "record", "-q", "-e", "intel_pt//u", "-o", data, "--",
         ash, *flags, os.path.join(TESTS, program)],
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, check=False,
    )
    script = subprocess.Popen(
        ["perf", "script", "-i", data, "--itrace=b", "-F", "ip,sym,dso", "--no-demangle"],
        stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, text=True,
    )
    suffix = f"({ash})"
    seen = {}
    for line in script.stdout:
        fields = line.split()
        if len(fields) == 3 and fields[2] == suffix and "@" not in fields[1]:
            seen.setdefault(fields[1], None)
    script.wait()
    os.unlink(data)
    return list(seen)


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--ash", default=os.path.join(ROOT, "target/release/ash"))
    args = parser.parse_args()
    ash = os.path.realpath(args.ash)
    if not os.path.exists("/sys/bus/event_source/devices/intel_pt"):
        sys.exit("needs Intel PT: run this on Linux x86_64 with an Intel CPU")

    order = {}
    # Not /tmp, which is often a small tmpfs.
    scratch = os.path.join(ROOT, "target")
    os.makedirs(scratch, exist_ok=True)
    with tempfile.TemporaryDirectory(dir=scratch) as tmp:
        for flags, program in WORKLOADS:
            for _ in range(2):
                found = functions_run(ash, flags, program, tmp)
                print(f"{' '.join(flags)} {program}: {len(found)} functions", file=sys.stderr)
                for name in found:
                    order.setdefault(name, None)
    if not order:
        sys.exit("the traces found no functions; is perf allowed to trace (perf_event_paranoid)?")
    with open(OUT, "w") as f:
        f.writelines(name + "\n" for name in order)
    print(f"wrote {len(order)} functions to {OUT}", file=sys.stderr)


if __name__ == "__main__":
    main()
