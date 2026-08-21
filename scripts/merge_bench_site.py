#!/usr/bin/env python3
"""Merge benchmark partials into the JSON the website's benchmark section renders.

Inputs are a directory of per-benchmark partials, the same shape zyntax's
aggregate job consumes: `ash-<bench>.json` files are full `ash_bench.py
--out-json` documents (each holding one benchmark's rows), `hl-<bench>.json`
files come from `hl_bench.py` and carry both HashLink lanes (JIT and HL/C).
Output is one compact `results.json`:

  { schema_version, generated_iso, source, commit, branch, hl_version,
    system: {os, arch, cpu_model, cpu_count},
    benchmarks: [ { name, title, group, rows: [
        { engine: "hashlink-jit", label, status, median_ms, min_ms, max_ms, runs },
        { engine: "ash", label, mode, status, median_ms, compile_ms,
          min_ms, max_ms, runs, tiers: {cranelift, llvm}, checksum } ] } ] }

The page treats the HashLink JIT row as the 1.00 baseline when present and
falls back to absolute milliseconds when it is not — so a runner without a
working `hl` still publishes, just without ratios. `compile_ms` is the sum of
broker-side tier installs; it overlaps the run on background threads, which
the page's footnote says in as many words.

Usage: scripts/merge_bench_site.py --parts DIR --out website/bench/results.json [--source ci]
"""

from __future__ import annotations

import argparse
import json
import sys
import time
from pathlib import Path

# Display order and titles for the published set. Anything else found in the
# partials is appended alphabetically — the page renders whatever is present.
KNOWN = [
    ("fib", "fib(40)"),
    ("mandelbrot", "mandelbrot"),
    ("nbody", "n-body"),
    ("inlined_call", "inlined function"),
    ("method_call", "method call"),
    ("free_call", "free function"),
    ("closure_call", "closure call"),
]
TITLES = dict(KNOWN)
ORDER = {name: i for i, (name, _) in enumerate(KNOWN)}

# The mode published as "Ash". Others in the partial are fallbacks only.
PREFERRED_MODE = "hybrid-auto"


def wall_fields(wall: dict | None) -> dict:
    if not wall:
        return {}
    return {
        "median_ms": round(wall["median_ms"], 2),
        "min_ms": round(wall["min_ms"], 2),
        "max_ms": round(wall["max_ms"], 2),
        "runs": wall.get("runs"),
    }


def ash_row(docs: list[dict], bench: str) -> dict | None:
    """Best ash row for `bench` across every partial: a stale or failed
    partial must never shadow an OK row in another one."""
    rows = [
        r
        for doc in docs
        for r in doc.get("results", [])
        if r.get("benchmark") == bench
    ]
    if not rows:
        return None
    pick = next(
        (r for r in rows if r.get("mode") == PREFERRED_MODE and r.get("status") == "OK"),
        None,
    )
    if pick is None:
        pick = next((r for r in rows if r.get("status") == "OK"), rows[0])
    compile_ms = round(
        sum(t.get("compile_ms", 0.0) for t in pick.get("tier_installs") or []), 2
    )
    tiered = pick.get("tiered") or {}
    checksum = (pick.get("checksum") or {}).get("value")
    row = {
        "engine": "ash",
        "label": "Ash",
        "mode": pick.get("mode"),
        "status": pick.get("status"),
        "detail": pick.get("detail", ""),
        "compile_ms": compile_ms,
        "tiers": {
            "cranelift": tiered.get("cranelift", 0),
            "llvm": tiered.get("llvm", 0),
        },
    }
    if checksum:
        row["checksum"] = checksum
    row.update(wall_fields(pick.get("wall_ms")))
    return row


HL_LABELS = {"hashlink-jit": "HashLink JIT", "hashlink-c": "HashLink/C"}


def hl_row(doc: dict, bench: str, engine: str) -> dict | None:
    rec = next(
        (
            r
            for r in doc.get("results", [])
            # Early partials had no per-record engine; those were JIT-only.
            if r.get("benchmark") == bench
            and r.get("engine", "hashlink-jit") == engine
        ),
        None,
    )
    if rec is None or rec.get("status") in ("UNAVAILABLE", "SKIP"):
        return None
    row = {
        "engine": engine,
        "label": HL_LABELS[engine],
        "status": rec.get("status"),
        "detail": rec.get("detail", ""),
    }
    if rec.get("aot_build_ms") is not None:
        row["aot_build_ms"] = rec["aot_build_ms"]
    row.update(wall_fields(rec.get("wall_ms")))
    return row


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--parts", type=Path, required=True)
    ap.add_argument("--out", type=Path, required=True)
    ap.add_argument("--source", default="ci")
    args = ap.parse_args()

    ash_docs: list[dict] = []
    hl_docs: list[dict] = []
    for p in sorted(args.parts.glob("*.json")):
        try:
            doc = json.loads(p.read_text())
        except json.JSONDecodeError as e:
            print(f"::warning::skipping unparseable partial {p}: {e}")
            continue
        if doc.get("tool") == "hl_bench" or doc.get("engine") == "hashlink-jit":
            hl_docs.append(doc)
        elif "results" in doc:
            ash_docs.append(doc)
        else:
            print(f"::warning::skipping unrecognized partial {p}")

    if not ash_docs:
        print("::error::no ash partials found; nothing to publish")
        return 1

    names: list[str] = []
    for doc in ash_docs:
        for r in doc.get("results", []):
            b = r.get("benchmark")
            if b and b not in names:
                names.append(b)
    names.sort(key=lambda n: (ORDER.get(n, len(ORDER)), n))

    benchmarks = []
    for name in names:
        # Page row order matches the mock: baseline, Ash, then the AOT floor.
        rows = []
        for doc in hl_docs:
            row = hl_row(doc, name, "hashlink-jit")
            if row:
                rows.append(row)
                break
        row = ash_row(ash_docs, name)
        if row:
            rows.append(row)
        for doc in hl_docs:
            row = hl_row(doc, name, "hashlink-c")
            if row:
                rows.append(row)
                break
        group = next(
            (
                r.get("group")
                for doc in ash_docs
                for r in doc.get("results", [])
                if r.get("benchmark") == name and r.get("group")
            ),
            "misc",
        )
        benchmarks.append(
            {"name": name, "title": TITLES.get(name, name), "group": group, "rows": rows}
        )

    first = ash_docs[0]
    git = first.get("git") or {}
    system = first.get("system") or {}
    hl_version = next((d.get("hl_version") for d in hl_docs if d.get("hl_version")), None)

    out = {
        "schema_version": 1,
        "generated_iso": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "source": args.source,
        "commit": (git.get("commit") or "")[:12],
        "branch": git.get("branch"),
        "hl_version": hl_version,
        "system": {
            "os": system.get("os"),
            "arch": system.get("arch"),
            "cpu_model": system.get("cpu_model"),
            "cpu_count": system.get("cpu_count"),
        },
        "benchmarks": benchmarks,
    }
    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(json.dumps(out, indent=2) + "\n")
    ok_ash = sum(
        1
        for b in benchmarks
        for r in b["rows"]
        if r.get("engine") == "ash" and r.get("status") == "OK"
    )
    ok = sum(1 for b in benchmarks for r in b["rows"] if r.get("status") == "OK")
    print(f"[merge] wrote {args.out}: {len(benchmarks)} benchmarks, {ok} OK rows")
    if ok_ash == 0:
        # Publishing a run where every ash row failed would wipe the previous
        # good page for nothing; fail the aggregate and keep what is deployed.
        print("::error::no OK ash rows; refusing to publish an empty table")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
