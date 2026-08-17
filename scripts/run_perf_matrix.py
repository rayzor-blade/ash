#!/usr/bin/env python3
"""DEPRECATED — superseded by scripts/ash_bench.py.

This script used to be the perf matrix. It measured two modes (`interp` and an
eagerly-promoting `hybrid`), reported medians, and gated on `--min-speedup`. It
did not check that a run produced the right answer, did not measure memory, and
did not read the JIT or GC counters ASH already prints — so a configuration
that got faster by getting something wrong looked like a win.

`scripts/ash_bench.py` is the canonical tool now. It covers everything this
script did and adds the correctness gate, peak RSS, the `[tiered]` / `[gc]`
counters, per-run timeouts, and baseline regression gating.

This file remains only so existing callers — notably
.github/workflows/perf_smoke.yml — keep working. It translates the old flags
and execs the new tool. Do not add features here; add them to ash_bench.py.

Old flag -> new behaviour:

  --repo-root      passed through
  --out-json       passed through
  --iterations     passed through
  --warmups        passed through
  --include-slow   passed through
  --min-speedup    passed through
  (implicit)       --modes interp,hybrid-eager, and the two-case corpus this
                   script selected, so its historical numbers stay comparable.
                   `hybrid-eager` is --jit-threshold 1, the promotion policy
                   this script hardcoded — not ASH's shipping default.
"""

from __future__ import annotations

import os
import pathlib
import sys

HERE = pathlib.Path(__file__).resolve().parent
NEW = HERE / "ash_bench.py"

# The corpus this script hardcoded: TestTieredHotLoop and MandelbrotSmall,
# plus Mandelbrot under --include-slow.
LEGACY_BENCHMARKS = "tiered_hotloop,mandelbrot_small"
LEGACY_BENCHMARKS_SLOW = "tiered_hotloop,mandelbrot_small,mandelbrot"
LEGACY_MODES = "interp,hybrid-eager"


VALUE_FLAGS = ("--repo-root", "--out-json", "--iterations", "--warmups",
               "--min-speedup")


def main(argv: list[str]) -> int:
    if "-h" in argv or "--help" in argv:
        print(__doc__)
        return 0

    print(
        "run_perf_matrix.py is deprecated; forwarding to ash_bench.py. "
        "See bench/README.md.",
        file=sys.stderr,
    )

    passthrough: list[str] = []
    include_slow = False

    i = 0
    while i < len(argv):
        a = argv[i]
        if a == "--include-slow":
            include_slow = True
            i += 1
        elif a in VALUE_FLAGS:
            if i + 1 >= len(argv):
                raise SystemExit(f"{a} requires a value")
            passthrough.extend([a, argv[i + 1]])
            i += 2
        elif a.startswith("--") and a.split("=", 1)[0] in VALUE_FLAGS:
            passthrough.append(a)
            i += 1
        else:
            raise SystemExit(
                f"run_perf_matrix.py: unknown option {a!r}. This is a "
                f"compatibility shim — call scripts/ash_bench.py directly for "
                f"anything new."
            )

    cmd = [
        sys.executable,
        str(NEW),
        "--modes", LEGACY_MODES,
        "--benchmarks",
        LEGACY_BENCHMARKS_SLOW if include_slow else LEGACY_BENCHMARKS,
    ]
    if include_slow:
        cmd.append("--include-slow")
    cmd.extend(passthrough)

    print(f"exec: {' '.join(cmd)}", file=sys.stderr)
    os.execv(sys.executable, cmd)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
