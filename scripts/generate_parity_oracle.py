#!/usr/bin/env python3
import argparse
import json
import os
import pathlib
import subprocess
import sys
import time

try:
    import tomllib  # Python 3.11+
except ModuleNotFoundError as exc:  # pragma: no cover
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


def checksum_from_stdout(stdout):
    for line in stdout.splitlines():
        if "Checksum:" in line:
            return line.split("Checksum:", 1)[1].strip()
    return None


def main():
    parser = argparse.ArgumentParser(description="Generate HashLink parity oracle bundle")
    parser.add_argument("--repo-root", default=".")
    parser.add_argument("--out-dir", required=True)
    parser.add_argument("--include-slow", action="store_true")
    args = parser.parse_args()

    repo_root = pathlib.Path(args.repo_root).resolve()
    tests_dir = repo_root / "crates" / "ash" / "test" / "tests"
    cases_file = tests_dir / "parity_cases.toml"
    out_dir = pathlib.Path(args.out_dir).resolve()
    out_cases = out_dir / "cases"

    if not cases_file.exists():
        raise SystemExit(f"missing parity cases file: {cases_file}")

    data = tomllib.loads(cases_file.read_text(encoding="utf-8"))
    cases = data.get("case", [])

    out_cases.mkdir(parents=True, exist_ok=True)

    manifest_cases = []

    for case in cases:
        name = case["name"]
        hl_name = case["hl"]
        main_name = case.get("main", "")
        slow = bool(case.get("slow", False))
        timeout_secs = int(case.get("timeout_secs", 60))
        compile_enabled = bool(case.get("compile", True))

        if slow and not args.include_slow:
            continue

        hl_path = tests_dir / hl_name

        if compile_enabled:
            code, out, err = run(
                [
                    "haxe",
                    "--cwd",
                    str(tests_dir),
                    "-main",
                    main_name,
                    "-hl",
                    hl_name,
                ],
                timeout=max(60, timeout_secs),
            )
            if code != 0:
                raise SystemExit(
                    f"compile failed for {name} ({hl_name})\nstdout:\n{out}\nstderr:\n{err}"
                )

        if not hl_path.exists():
            raise SystemExit(f"missing .hl file for {name}: {hl_path}")

        code, out, err = run(["hl", str(hl_path)], timeout=timeout_secs)

        case_dir = out_cases / name
        case_dir.mkdir(parents=True, exist_ok=True)
        (case_dir / "hashlink.stdout").write_text(out, encoding="utf-8")
        (case_dir / "hashlink.stderr").write_text(err, encoding="utf-8")
        (case_dir / "exit_code.txt").write_text(str(code), encoding="utf-8")

        manifest_cases.append(
            {
                "name": name,
                "hl": hl_name,
                "slow": slow,
                "timeout_secs": timeout_secs,
                "expectation": case.get("expectation", "exact"),
                "normalize": case.get("normalize", "default"),
                "hashlink_exit_code": code,
                "checksum": checksum_from_stdout(out),
            }
        )

    manifest = {
        "schema_version": 1,
        "generated_unix": int(time.time()),
        "repo": str(repo_root),
        "tool_versions": {
            "haxe": run(["haxe", "--version"])[1].strip(),
            "hashlink": run(["hl", "--version"])[1].strip(),
            "python": sys.version.split()[0],
        },
        "normalization_version": 1,
        "case_count": len(manifest_cases),
        "cases": manifest_cases,
    }

    out_dir.mkdir(parents=True, exist_ok=True)
    (out_dir / "parity_manifest.json").write_text(
        json.dumps(manifest, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )

    print(f"wrote parity oracle bundle to {out_dir}")
    print(f"cases: {len(manifest_cases)}")


if __name__ == "__main__":
    main()
