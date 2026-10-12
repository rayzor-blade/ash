#!/usr/bin/env python3
"""Same-binary, same-host fusion off/on experiment; never publishes site rows."""
from __future__ import annotations
import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
import platform
from pathlib import Path
import random
import re
import statistics
import sys

import ash_bench as bench

ROOT = Path(__file__).resolve().parents[1]
INPUTS = {
    "source": ("BenchBinaryTrees.hx", "d705a3b8c01dd2dcae7a89e27cf814317df8ba50ccbf3ec6d1ea8bfdcfd669b5"),
    "bytecode": ("bench_binary_trees.hl", "9698cecaf8852fa219acfd3361ea8659543cb7dc5a4033168aa3ff96d01a926d"),
}
HELPER = re.compile(r"^\[tree-fusion\] helper=(\d+) producer=(\d+) consumer=(\d+) ops=(\d+) regs=(\d+) node_allocations=(\d+)$", re.M)


def digest(path):
    h = hashlib.sha256()
    with Path(path).open("rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def inputs(root):
    out = {}
    for name, (file, expected) in INPUTS.items():
        path = root / "crates/ash/test/tests" / file
        actual = digest(path)
        if actual != expected:
            raise ValueError(f"{file}: benchmark artifact changed ({actual})")
        out[name] = {"path": str(path), "sha256": actual}
    return out


def environment(diagnostic=False):
    # An inherited scheduler, allocator or optimization experiment must not
    # silently become part of this controlled comparison.
    env = {k: v for k, v in os.environ.items() if not k.startswith("ASH_")}
    if diagnostic:
        env.update(ASH_GC_STATS="1", ASH_GC_TLAB="0", ASH_PROFILE="phases")
    return env


def order(pair):
    return ["off", "on"] if pair % 2 == 0 else ["on", "off"]


def validate_result(result):
    if not result.ok or result.stdout.strip() != "Checksum: -104864":
        raise ValueError(f"incorrect/incomplete execution: {bench.failure_detail(result)}; stdout={result.stdout!r}")


def helpers(stderr):
    return [dict(zip(("findex", "producer", "consumer", "ops", "regs", "node_allocations"), map(int, m)))
            for m in HELPER.findall(stderr)]


def validate_diagnostic(variant, stderr):
    gc = bench.parse_gc(stderr)
    if not gc or not {"bytes_allocated_mb", "external_bytes_mb", "collections", "pause_total_ns"} <= gc.keys():
        raise ValueError("missing allocation/GC counters")
    # Printed MB counters are actually MiB, rounded to one decimal. Include
    # the separately reported external domain, and preserve that uncertainty.
    total = gc["bytes_allocated_mb"] + gc["external_bytes_mb"]
    if variant == "on":
        hs = helpers(stderr)
        if not hs or any(h["node_allocations"] != 0 for h in hs):
            raise ValueError("missing/allocation-bearing fused helper")
        if "transformed_sites=3 helpers=1" not in stderr or not 3.9 <= total <= 8:
            raise ValueError(f"fusion/retained-tree allocation gate failed: {total} MiB")
    elif not total >= 800:
        raise ValueError(f"baseline allocation unexpectedly small: {total} MiB")
    return gc


def phases(stderr):
    # Phase totals from concurrent JIT workers overlap; never sum them into
    # process wall time or treat the instrumented run as a clean sample.
    pattern = r"^\s*(\S.*?)\s+([\d.]+)\s+([\d.]+)\s+(\d+)\s+[\d.]+%$"
    return [{"phase": name, "total_ms": float(total), "self_ms": float(own), "calls": int(n)}
            for name, total, own, n in re.findall(pattern, stderr, re.M)]


def distribution(xs):
    return {"samples": xs, "median": statistics.median(xs), "min": min(xs), "max": max(xs),
            "mean": statistics.mean(xs), "stddev": statistics.stdev(xs) if len(xs) > 1 else 0.0}


def summarize(runs):
    samples = {v: [r["wall_ms"] for r in runs if r["phase"] == "timing" and r["variant"] == v] for v in ("off", "on")}
    ratios = [on / off for off, on in zip(samples["off"], samples["on"], strict=True)]
    rng = random.Random(0)
    boot = sorted(statistics.median(rng.choices(ratios, k=len(ratios))) for _ in range(5000))
    gc = {v: [r["gc"]["bytes_allocated_mb"] + r["gc"]["external_bytes_mb"] for r in runs
              if r["phase"] == "accounting" and r["variant"] == v] for v in ("off", "on")}
    return {"process_wall_ms": {v: distribution(xs) for v, xs in samples.items()},
            "paired_on_over_off": distribution(ratios),
            "paired_ratio_bootstrap_95pct": [boot[125], boot[4874]],
            "interval_note": "Descriptive paired bootstrap; shared-host noise and correlated runs remain possible.",
            "cumulative_allocation_mib": gc,
            "allocation_reduction_pct": 100 * (1 - statistics.mean(gc["on"]) / statistics.mean(gc["off"]))}


def run_experiment(args, record):
    root, out = args.root.resolve(), args.out.resolve()
    binary = args.ash.resolve()
    record.update(inputs=inputs(root), binary={"path": str(binary), "sha256": digest(binary)},
                  removed_environment=sorted(k for k in os.environ if k.startswith("ASH_")))
    original_hash = record["binary"]["sha256"]
    hl = record["inputs"]["bytecode"]["path"]
    runs = record["runs"]

    def execute(phase, pair, variant, extra_env=None):
        diagnostic = phase in ("accounting", "ir")
        env = environment(diagnostic)
        env.update(extra_env or {})
        cmd = [str(binary), "--mode", "hybrid", "--quiet"]
        if variant == "off": cmd += ["--no-tree-fusion"]
        if diagnostic: cmd += ["--jit-log"]
        cmd.append(hl)
        if diagnostic and (wrapper := bench.time_wrapper()): cmd = wrapper + cmd
        before = os.getloadavg() if hasattr(os, "getloadavg") else None
        result = bench.exec_run(cmd, root, env, args.timeout)
        stem = f"{phase}-{pair:02d}-{variant}"
        for channel in ("stdout", "stderr"):
            (out / f"{stem}.{channel}").write_text(getattr(result, channel))
        r = {"phase": phase, "pair": pair, "variant": variant, "command": cmd,
             "environment": {k: v for k, v in env.items() if k.startswith("ASH_")},
             "load_before": before, "wall_ms": result.wall_ms, "returncode": result.returncode,
             "signal": result.signal, "timed_out": result.timed_out,
             "stdout": f"{stem}.stdout", "stderr": f"{stem}.stderr", "valid": False}
        runs.append(r)
        if phase == "ir":
            if not result.ok:
                raise ValueError(f"IR inspection failed: {bench.failure_detail(result)}")
            r.update(valid=True, kind="IR inspection only; guest does not execute")
            return result, r
        validate_result(result)
        r["checksum_valid"] = True
        if diagnostic:
            r.update(gc=validate_diagnostic(variant, result.stderr), peak_rss_bytes=bench.parse_rss_bytes(result.stderr),
                     tiered=bench.parse_tiered(result.stderr), tier_installs=bench.parse_tier_installs(result.stderr),
                     helpers=helpers(result.stderr), phases=phases(result.stderr))
            if not r["phases"] or r["peak_rss_bytes"] is None:
                raise ValueError("missing phase/RSS diagnostics")
        r["valid"] = True
        print(f"{phase} {pair} {variant}: {result.wall_ms:.2f} ms checksum=-104864", flush=True)
        return result, r

    for phase, count in [("warmup", args.warmups), ("timing", args.pairs), ("accounting", args.diagnostics)]:
        for pair in range(count):
            for variant in order(pair): execute(phase, pair, variant)
    h = next(r["helpers"][0] for r in runs if r["phase"] == "accounting" and r["variant"] == "on")
    result, _ = execute("ir", 0, "on", {"ASH_VERIFY_AIR": f"dump:{h['findex']}"})
    lines = [l for l in result.stderr.splitlines() if l.startswith("[air]   in ")]
    if not lines or len(lines) % h["ops"] != 0 or any(re.search(r"\b(New|Field|SetField|GetThis|SetThis)\s*\{", l) for l in lines):
        raise ValueError("missing helper IR or residual node/field operations")
    if not any(f"RefFun({h['findex']})" in l for l in lines):
        raise ValueError("helper IR lacks recursive traversal")
    if inputs(root) != record["inputs"] or digest(binary) != original_hash:
        raise ValueError("input/binary changed during measurement")
    record["summary"] = summarize(runs)


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--ash", type=Path, required=True)
    p.add_argument("--root", type=Path, default=ROOT)
    p.add_argument("--out", type=Path, required=True)
    p.add_argument("--source-revision", required=True, help="Revision actually compiled into --ash; not inferred from an unrelated checkout")
    p.add_argument("--source", choices=["ci", "nuc", "local"], default="local")
    p.add_argument("--pairs", type=int, default=7)
    p.add_argument("--warmups", type=int, default=1)
    p.add_argument("--diagnostics", type=int, default=2)
    p.add_argument("--timeout", type=float, default=120)
    args = p.parse_args(argv)
    if args.pairs < 2 or args.warmups < 0 or args.diagnostics < 1 or args.timeout <= 0:
        p.error("need at least two pairs, nonnegative warmups, positive diagnostics/timeout")
    args.out.mkdir(parents=True, exist_ok=False)
    record = {"schema_version": 1, "generated_iso": datetime.now(timezone.utc).isoformat(),
              "source": args.source, "source_revision": args.source_revision, "system": bench.system_info(),
              "kernel": platform.platform(), "affinity": sorted(os.sched_getaffinity(0)) if hasattr(os, "sched_getaffinity") else None,
              "harness_sha256": digest(Path(__file__)),
              "comparison": "same binary, --no-tree-fusion versus default; alternating AB/BA pairs, fresh processes",
              "timing_scope": "whole process including startup, compilation; no GC/JIT/profile instrumentation",
              "accounting_note": "TLAB disabled on both diagnostic variants; managed plus external counters, each rounded to 0.1 MiB (sum uncertainty +/-0.1 MiB). RSS is separate, not cumulative allocation.",
              "limitations": ["Helper IR/compilation diagnostics are separate from whole-program timing; no isolated helper throughput claim.",
                              "Local/NUC runs are not CI evidence. Fused AOT/wasm remain unsupported."],
              "runs": [], "status": "RUNNING"}
    try:
        run_experiment(args, record)
        record["status"] = "PASS"
    except (ValueError, OSError) as e:
        record.update(status="FAIL", error=str(e))
    finally:
        (args.out / "result.json").write_text(json.dumps(record, indent=2) + "\n")
    if record["status"] != "PASS":
        print(record["error"], file=sys.stderr)
        return 1
    print(json.dumps(record["summary"], indent=2))
    return 0


if __name__ == "__main__":
    sys.exit(main())
