#!/usr/bin/env python3
import argparse
import json
import os
import pathlib
import platform
import statistics
import subprocess
import time

try:
    import tomllib  # Python 3.11+
except ModuleNotFoundError as exc:
    raise SystemExit(f"tomllib unavailable: {exc}")


def run(cmd, timeout=None, cwd=None):
    p = subprocess.run(
        cmd,
        cwd=cwd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        timeout=timeout,
        check=False,
        text=True,
    )
    return p.returncode, p.stdout, p.stderr


def timed_run(cmd, timeout=None):
    start = time.perf_counter()
    p = subprocess.run(
        cmd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        timeout=timeout,
        check=False,
        text=True,
    )
    elapsed_ms = (time.perf_counter() - start) * 1000.0
    return p.returncode, p.stdout, p.stderr, elapsed_ms


def must_ok(code, out, err, context):
    if code != 0:
        raise SystemExit(f"{context} failed (exit={code})\nstdout:\n{out}\nstderr:\n{err}")


def load_cases(repo_root, include_slow):
    tests_dir = repo_root / "crates" / "ash" / "test" / "tests"
    cases_file = tests_dir / "parity_cases.toml"
    data = tomllib.loads(cases_file.read_text(encoding="utf-8"))
    by_name = {c["name"]: c for c in data.get("case", [])}

    selected = ["TestTieredHotLoop", "MandelbrotSmall"]
    if include_slow:
        selected.append("Mandelbrot")

    out = []
    for name in selected:
        c = by_name.get(name)
        if not c:
            continue
        if c.get("slow", False) and not include_slow:
            continue
        out.append(c)
    return out, tests_dir


def build_ash_cli(repo_root):
    code, out, err = run(["cargo", "build", "-q", "-p", "ash_cli"], cwd=repo_root)
    must_ok(code, out, err, "cargo build -p ash_cli")


def ash_cli_path(repo_root):
    target_root = repo_root / "target"
    candidates = [target_root / "debug" / "ash_cli"]

    host = platform.machine().lower()
    if host:
        if host in ("arm64", "aarch64"):
            candidates.append(target_root / "aarch64-apple-darwin" / "debug" / "ash_cli")
        elif host in ("x86_64", "amd64"):
            candidates.append(target_root / "x86_64-apple-darwin" / "debug" / "ash_cli")

    candidates.extend(sorted(target_root.glob("*/debug/ash_cli")))

    for path in candidates:
        if path.exists() and os.access(path, os.X_OK):
            return path

    searched = "\n".join(f"  - {p}" for p in candidates)
    raise SystemExit(f"ash_cli binary not found. Searched:\n{searched}")


def compile_case(tests_dir, case):
    if not case.get("compile", True):
        return
    code, out, err = run(
        [
            "haxe",
            "--cwd",
            str(tests_dir),
            "-main",
            case["main"],
            "-hl",
            case["hl"],
        ]
    )
    must_ok(code, out, err, f"compile case {case['name']}")


def run_case(binary, hl_path, mode, timeout_secs):
    cmd = [str(binary), "--mode", mode, "--quiet"]
    if mode == "hybrid":
        cmd.extend(["--jit-threshold", "1", "--jit-min-ops", "0", "--jit-max-args", "8"])
    cmd.append(str(hl_path))
    return timed_run(cmd, timeout=timeout_secs)


def summarize(samples):
    samples = list(samples)
    if not samples:
        return None
    return {
        "min_ms": min(samples),
        "median_ms": statistics.median(samples),
        "max_ms": max(samples),
        "mean_ms": statistics.mean(samples),
        "runs": len(samples),
    }


def main():
    parser = argparse.ArgumentParser(description="Run ASH perf smoke matrix")
    parser.add_argument("--repo-root", default=".")
    parser.add_argument("--out-json", required=True)
    parser.add_argument("--iterations", type=int, default=5)
    parser.add_argument("--warmups", type=int, default=1)
    parser.add_argument("--include-slow", action="store_true")
    parser.add_argument("--min-speedup", type=float, default=0.0)
    args = parser.parse_args()

    repo_root = pathlib.Path(args.repo_root).resolve()
    out_json = pathlib.Path(args.out_json).resolve()

    cases, tests_dir = load_cases(repo_root, args.include_slow)
    if not cases:
        raise SystemExit("no performance cases selected")

    build_ash_cli(repo_root)
    binary = ash_cli_path(repo_root)

    results = {
        "schema_version": 1,
        "generated_unix": int(time.time()),
        "iterations": args.iterations,
        "warmups": args.warmups,
        "include_slow": args.include_slow,
        "cases": [],
    }

    threshold_failures = []

    for case in cases:
        compile_case(tests_dir, case)
        hl_path = tests_dir / case["hl"]
        timeout_secs = int(case.get("timeout_secs", 60))

        # Warmups
        for _ in range(max(0, args.warmups)):
            for mode in ("interp", "hybrid"):
                code, out, err, _ = run_case(binary, hl_path, mode, timeout_secs)
                must_ok(code, out, err, f"warmup {case['name']} mode={mode}")

        interp_samples = []
        hybrid_samples = []

        for _ in range(max(1, args.iterations)):
            code, out, err, t_ms = run_case(binary, hl_path, "interp", timeout_secs)
            must_ok(code, out, err, f"perf run {case['name']} mode=interp")
            interp_samples.append(t_ms)

            code, out, err, t_ms = run_case(binary, hl_path, "hybrid", timeout_secs)
            must_ok(code, out, err, f"perf run {case['name']} mode=hybrid")
            hybrid_samples.append(t_ms)

        interp = summarize(interp_samples)
        hybrid = summarize(hybrid_samples)
        speedup = None
        if interp and hybrid and hybrid["median_ms"] > 0:
            speedup = interp["median_ms"] / hybrid["median_ms"]

        case_result = {
            "name": case["name"],
            "hl": case["hl"],
            "interp": interp,
            "hybrid": hybrid,
            "speedup_interp_over_hybrid": speedup,
        }
        results["cases"].append(case_result)

        if args.min_speedup > 0 and speedup is not None and speedup < args.min_speedup:
            threshold_failures.append(
                f"{case['name']}: speedup {speedup:.3f} < min {args.min_speedup:.3f}"
            )

    out_json.parent.mkdir(parents=True, exist_ok=True)
    out_json.write_text(json.dumps(results, indent=2, sort_keys=True) + "\n", encoding="utf-8")

    print(f"perf results written: {out_json}")
    for c in results["cases"]:
        speedup = c["speedup_interp_over_hybrid"]
        speedup_s = "n/a" if speedup is None else f"{speedup:.3f}x"
        print(
            f"{c['name']}: interp_med={c['interp']['median_ms']:.2f}ms "
            f"hybrid_med={c['hybrid']['median_ms']:.2f}ms speedup={speedup_s}"
        )

    if threshold_failures:
        raise SystemExit("\n".join(["performance threshold failures:"] + threshold_failures))


if __name__ == "__main__":
    main()
