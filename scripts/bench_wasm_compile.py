#!/usr/bin/env python3
"""Compare two compilers on one WASM input with warm caches and reversed run order."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import subprocess
import time

try:
    import resource
except ImportError:
    resource = None


def digest(path):
    result = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1 << 20), b""):
            result.update(chunk)
    return result.hexdigest()


def swap_pages():
    path = Path("/proc/vmstat")
    if not path.is_file():
        return {}
    return {key: int(value) for key, value in map(str.split, path.read_text().splitlines())
            if key in ("pswpin", "pswpout")}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("bytecode", type=Path)
    parser.add_argument("--baseline", type=Path, required=True)
    parser.add_argument("--candidate", type=Path, required=True)
    parser.add_argument("--runtime", type=Path, required=True)
    parser.add_argument("--out-dir", type=Path, required=True,
                        help="Output directory with any native side modules already staged")
    parser.add_argument("--report-dir", type=Path, required=True)
    parser.add_argument("--target", default="wasm32-wasip1-threads")
    parser.add_argument("--shards", type=int, default=5)
    parser.add_argument("--repeat", type=int, default=2)
    parser.add_argument("--codegen-level", type=int, choices=range(4), action="append")
    parser.add_argument("--fibers", action="store_true")
    args = parser.parse_args()
    if args.shards < 1 or args.repeat < 1:
        parser.error("--shards and --repeat must be positive")
    binaries = {name: getattr(args, name).resolve() for name in ("baseline", "candidate")}
    args.out_dir.mkdir(parents=True, exist_ok=True)
    args.report_dir.mkdir(parents=True, exist_ok=True)
    levels = args.codegen_level or [3, 0]
    report = {
        "host": platform.platform(),
        "cores": os.cpu_count(),
        "input_sha256": digest(args.bytecode),
        "compiler_sha256": {name: digest(path) for name, path in binaries.items()},
        "runtime_sha256": digest(args.runtime),
        "target": args.target,
        "shards": args.shards,
        "fibers": args.fibers,
        "air_level": 3,
        "llvm_ir_level": 3,
        "runs": [],
    }
    schedule = [(True, level, name) for level in levels for name in binaries]
    for level in levels:
        for repetition in range(args.repeat):
            names = list(binaries) if repetition % 2 == 0 else list(reversed(binaries))
            schedule.extend((False, level, name) for name in names)
    for index, (warmup, level, name) in enumerate(schedule):
        env = dict(os.environ, ASH_RUNTIME=str(args.runtime.resolve()),
                   ASH_AOT_SHARDS=str(args.shards))
        for key in ("ASH_AOT_DUMP_IR", "ASH_AOT_NO_OPT", "ASH_LLVM_PASSES",
                    "ASH_AOT_CODEGEN_LEVEL", "ASH_WASM_FIBERS", "ASH_AIR_DISK_CACHE"):
            env.pop(key, None)
        if args.fibers:
            env["ASH_WASM_FIBERS"] = "1"
        output = args.out_dir / f"compile-{name}-cg{level}.wasm"
        log = args.report_dir / f"{index}-{name}-cg{level}.log"
        command = [str(binaries[name]), "-O3", "--air-opt-level", "3",
                   "--codegen-opt-level", str(level), "--air-cache-dir",
                   str(args.report_dir / f"cache-{name}"), "--air-cache-stats",
                   "--build", str(output.resolve()), "--target", args.target,
                   str(args.bytecode.resolve())]
        load = list(os.getloadavg()) if hasattr(os, "getloadavg") else None
        swap_before = swap_pages()
        before = resource.getrusage(resource.RUSAGE_CHILDREN) if resource else None
        started = time.perf_counter()
        with log.open("wb") as stream:
            result = subprocess.run(command, env=env, stdout=stream,
                                    stderr=subprocess.STDOUT, timeout=600)
        elapsed = time.perf_counter() - started
        after = resource.getrusage(resource.RUSAGE_CHILDREN) if resource else None
        cpu_s = (after.ru_utime + after.ru_stime - before.ru_utime - before.ru_stime
                 if resource else None)
        text = log.read_text()
        if result.returncode:
            raise RuntimeError(f"Compiler exited {result.returncode}: {log}\n{text[-8000:]}")
        row = dict(run=index, warmup=warmup, variant=name, codegen=level,
                   wall_s=elapsed, cpu_s=cpu_s, bytes=output.stat().st_size, load_before=load,
                   log=log.name)
        row["swap_pages"] = {key: value - swap_before[key]
                             for key, value in swap_pages().items()}
        row.update({key: float(value) for key, value in
                    re.findall(r"\[aot-time\]\s+(\w+)=([\d.]+)", text)})
        row["shards"] = [dict(zip(("id", "kept", "prepare_ms", "opt_ms", "codegen_ms"),
                                  map(int, values))) for values in re.findall(
            r"\[aot\] shard (\d+): kept (\d+).*?prepare (\d+)ms, middle end (\d+)ms, codegen (\d+)ms", text)]
        report["runs"].append(row)
        (args.report_dir / "results.json").write_text(json.dumps(report, indent=2) + "\n")
        print(json.dumps(row), flush=True)


if __name__ == "__main__":
    main()
