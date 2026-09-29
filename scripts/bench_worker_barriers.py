#!/usr/bin/env python3
"""Build and measure the AOT FixedThreadPool frame-barrier control."""

import argparse
import os
import re
import resource
import shutil
import statistics
import subprocess
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
SOURCE = ROOT / "crates/ash/test/tests"


def run(command: list[str], **kwargs: object) -> subprocess.CompletedProcess[str]:
    return subprocess.run(command, check=True, text=True, **kwargs)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("ash", type=Path, help="ash binary used for the AOT build")
    parser.add_argument("--repeats", type=int, default=5)
    parser.add_argument("--frames", type=int, default=20)
    args = parser.parse_args()
    if args.repeats < 1 or args.frames < 1:
        parser.error("repeats and frames must be positive")
    ash = args.ash.resolve()

    with tempfile.TemporaryDirectory(prefix="ash-worker-barrier-") as scratch:
        fixture = Path(scratch) / "barriers.hl"
        binary = Path(scratch) / "barriers"
        if shutil.which("haxe"):
            run(["haxe", "-cp", str(SOURCE), "-main", "BenchWorkerBarriers", "-hl", str(fixture)])
        else:
            shutil.copyfile(SOURCE / "bench_worker_barriers.hl", fixture)
        run([str(ash), "--build", str(binary), str(fixture)], capture_output=True)

        for jobs in (1, 16):
            for workers in (0, 1, 4):
                wall_ms = []
                cpu_ms = []
                collections = []
                for _ in range(args.repeats):
                    env = os.environ.copy()
                    env.update(ASH_WORKERS=str(workers), ASH_GC_STATS="1")
                    before = resource.getrusage(resource.RUSAGE_CHILDREN)
                    result = run(
                        [str(binary), str(jobs), str(args.frames), "recreate"],
                        capture_output=True,
                        env=env,
                        timeout=60,
                    )
                    after = resource.getrusage(resource.RUSAGE_CHILDREN)
                    expected = f"done={jobs * args.frames}"
                    if expected not in result.stdout or "recreated=true" not in result.stdout:
                        raise RuntimeError(result.stdout + result.stderr)
                    elapsed = re.search(r"\bms=([0-9.]+)", result.stdout)
                    gc_count = re.search(r"\[gc\] collections:\s+(\d+)", result.stderr)
                    if elapsed is None or gc_count is None:
                        raise RuntimeError(result.stdout + result.stderr)
                    wall_ms.append(float(elapsed.group(1)))
                    cpu_ms.append(
                        1000
                        * (
                            after.ru_utime
                            + after.ru_stime
                            - before.ru_utime
                            - before.ru_stime
                        )
                    )
                    collections.append(int(gc_count.group(1)))
                print(
                    f"jobs={jobs:2} workers={workers} frames={args.frames} "
                    f"wall_ms={statistics.median(wall_ms):.3f} "
                    f"process_cpu_ms={statistics.median(cpu_ms):.3f} "
                    f"collections={statistics.median(collections):g}"
                )


if __name__ == "__main__":
    main()
