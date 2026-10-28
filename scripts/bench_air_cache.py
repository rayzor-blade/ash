#!/usr/bin/env python3
"""Measure persistent AIR reuse, with compiler stages and logs kept for inspection."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import time


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("original", type=Path)
    parser.add_argument("edited", type=Path, help="same program after a function-body edit")
    parser.add_argument("--ash", type=Path, required=True)
    parser.add_argument("--out-dir", type=Path, required=True)
    parser.add_argument("--target")
    parser.add_argument("--runtime", type=Path, help="runtime selected through ASH_RUNTIME")
    parser.add_argument("--wasm-fibers", action="store_true")
    parser.add_argument("--shards", type=int)
    parser.add_argument("--keep-outputs", action="store_true")
    parser.add_argument("--llvm-passes")
    parser.add_argument("--timeout", type=int, default=900)
    args = parser.parse_args()
    args.out_dir.mkdir(parents=True, exist_ok=True)
    cache = Path(tempfile.mkdtemp(prefix="cache-", dir=args.out_dir))
    env = dict(os.environ)
    env["ASH_AIR_DISK_CACHE"] = "1"
    env["ASH_WASM_FIBERS"] = "1" if args.wasm_fibers else "0"
    if args.runtime:
        env["ASH_RUNTIME"] = str(args.runtime.resolve())
    if args.shards:
        env["ASH_AOT_SHARDS"] = str(args.shards)
    if args.llvm_passes:
        env["ASH_LLVM_PASSES"] = args.llvm_passes
    output = args.out_dir / ("application.wasm" if args.target and args.target.startswith("wasm") else "application")
    base = [str(args.ash.resolve()), "--air-cache-dir", str(cache.resolve()), "--air-cache-stats", "--air-cache-mb", "512", "--build", str(output.resolve())]
    if args.target:
        base += ["--target", args.target]
    rows = []
    for name, source, flags in [("disabled", args.original, ["--no-air-cache"]),
                                ("cold", args.original, []), ("warm", args.original, []),
                                ("edit", args.edited, [])]:
        log_path = args.out_dir / f"{name}.log"
        began = time.perf_counter()
        with log_path.open("wb") as log:
            result = subprocess.run(base + flags + [str(source.resolve())], env=env,
                                    stdout=log, stderr=subprocess.STDOUT, timeout=args.timeout)
        wall = time.perf_counter() - began
        log = log_path.read_text(errors="replace")
        if result.returncode:
            raise SystemExit(f"{name} failed ({result.returncode}); see {log_path}")
        row = {"run": name, "wall_s": round(wall, 6), "log": str(log_path),
               "sha256": hashlib.sha256(output.read_bytes()).hexdigest()}
        for key, value in re.findall(r"\[aot-time\]\s+(\w+)=([\d.]+)", log):
            row[key] = float(value) if key.endswith("_ms") else int(value)
        stats = re.search(r"\[air-disk-cache\]([^\n]+)", log)
        if not stats:
            raise SystemExit(f"{name}: no AIR cache counters; see {log_path}")
        row["cache"] = {key: (float(value) if key.endswith("_ms") else int(value)
                              if value.isdigit() else value)
                        for key, value in re.findall(r"(\w+)=([\w.]+)", stats[1])}
        if args.keep_outputs:
            shutil.copy2(output, args.out_dir / (name + output.suffix))
        rows.append(row)
        print(json.dumps(row), flush=True)
        (args.out_dir / "results.json").write_text(json.dumps({
            "command": base, "cache": str(cache), "runs": rows,
            "llvm_passes": env.get("ASH_LLVM_PASSES", "default"),
            "fibers": args.wasm_fibers,
        }, indent=2) + "\n")
    if len({row["sha256"] for row in rows[:3]}) != 1:
        raise SystemExit("disabled/cold/warm output hashes disagree")


if __name__ == "__main__":
    main()
