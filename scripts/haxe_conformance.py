#!/usr/bin/env python3
"""Run the official Haxe test suite against ash.

ash's own corpus is 43 programs written to exercise things ash was known to
get wrong. That is a fine regression net and a poor conformance signal: it
only ever asks questions someone already thought to ask. The Haxe project
ships a suite that asks thousands, and it targets HashLink bytecode
directly -- exactly what ash executes -- so it can be pointed at ash with no
adaptation at all.

This fetches the suite at the tag matching the installed compiler, builds it
for the `hl` target, and runs each program under ash. With `--reference` it
also runs the same bytecode under a stock HashLink VM, which turns "ash
fails this" into the far more useful "ash fails this and HashLink does not".

    scripts/haxe_conformance.py                       # all suites, interp
    scripts/haxe_conformance.py --modes interp,hybrid
    scripts/haxe_conformance.py --suites sys --reference ~/hashlink/hl
    scripts/haxe_conformance.py --json out.json

Exit status is 1 if any suite regressed against `--baseline`, else 0. Without
a baseline it always exits 0: a first run is a measurement, not a verdict.
"""

import argparse
import json
import os
import pathlib
import re
import shutil
import subprocess
import sys
import time

# Suites that compile to HashLink bytecode and are meaningful for a VM.
# `misc`, `server`, `sourcemaps` and `nullsafety` test the *compiler*, not the
# runtime, so they are not here -- ash cannot pass or fail them.
SUITES = {
    "unit": {
        "dir": "tests/unit",
        "hxml": "compile-hl.hxml",
        # -D UTEST_PRINT_TESTS makes utest name each case and test as it
        # starts, via an unbuffered Sys.print. Without it a crashed run
        # yields nothing at all to count: utest's own tally is printed once,
        # at the end, so a VM that dies partway reports exactly as much as a
        # VM that dies immediately.
        "args": ["-D", "UTEST_PRINT_TESTS"],
        "programs": ["bin/unit.hl"],
        "needs": ["utest"],
        "about": "The main language suite: every type, operator, generic, "
                 "closure, enum, abstract and reflection path Haxe defines.",
    },
    "sys": {
        "dir": "tests/sys",
        "hxml": "compile-hl.hxml",
        "args": ["-D", "UTEST_PRINT_TESTS"],
        # Only the main driver is run directly; the other three are helpers it
        # spawns, and running them standalone tests nothing.
        "programs": ["bin/hl/sys.hl"],
        "needs": ["utest"],
        "about": "Filesystem, process, stdio and environment. The half of the "
                 "stdlib a game exercises before it draws anything.",
    },
    "threads": {
        "dir": "tests/threads",
        # This one carries no target in its hxml -- upstream CI supplies it on
        # the command line -- so the target is passed here instead.
        "hxml": "build.hxml",
        "args": ["-hl", "bin/threads.hl", "-D", "UTEST_PRINT_TESTS"],
        "programs": ["bin/threads.hl"],
        "needs": ["utest"],
        "about": "Threads, locks, mutexes, deques.",
    },
}

# utest's own verdict lines. A suite that neither fails nor announces success
# is treated as a pass only if it also exited zero -- see classify().
RE_FAIL = re.compile(r"\b(FAILED|ERROR|Error:)\b")
RE_OK = re.compile(r"\bOK\b|\bSUCCESS\b|\ball tests? (?:passed|ok)\b", re.I)


def run(cmd, cwd=None, timeout=900, env=None):
    return subprocess.run(
        cmd, cwd=cwd, capture_output=True, text=True, timeout=timeout,
        env=env or os.environ.copy(),
    )


def haxe_version(haxe: str) -> str | None:
    try:
        r = run([haxe, "--version"], timeout=60)
    except (OSError, subprocess.TimeoutExpired):
        return None
    return (r.stdout or r.stderr).strip().split()[0] if r.returncode == 0 else None


def ensure_checkout(root: pathlib.Path, tag: str) -> pathlib.Path:
    """A sparse, blobless checkout of just tests/ at the compiler's own tag.

    The tag matters more than it looks: the development branch tracks the next
    Haxe release and uses syntax the installed compiler rejects outright, so a
    mismatch fails at compile time with errors that look like ash's fault and
    are not.
    """
    src = root / f"haxe-tests-{tag}"
    if (src / "tests").is_dir():
        return src
    src.parent.mkdir(parents=True, exist_ok=True)
    shutil.rmtree(src, ignore_errors=True)
    print(f"fetching the Haxe {tag} suite into {src} ...", flush=True)
    r = run([
        "git", "clone", "--depth", "1", "--branch", tag,
        "--filter=blob:none", "--sparse",
        "https://github.com/HaxeFoundation/haxe.git", str(src),
    ], timeout=900)
    if r.returncode != 0:
        sys.exit(f"could not fetch the Haxe suite at tag {tag}:\n{r.stderr[:800]}")
    # A --sparse clone checks out only the root, so tests/ arrives here. This
    # is checked rather than assumed: when it silently did not happen, every
    # suite reported "not present at tag X" and the run published 0/0, which
    # reads like a result and is really a missing checkout.
    sc = run(["git", "sparse-checkout", "set", "tests"], cwd=str(src), timeout=300)
    if not (src / "tests").is_dir():
        sys.exit(
            f"the suite checkout at {src} has no tests/ directory after "
            f"`git sparse-checkout set tests`"
            + (f" (exit {sc.returncode}: {sc.stderr.strip()[:400]})"
               if sc.returncode else "")
        )
    return src


def ensure_libs(libs, haxelib: str) -> list[str]:
    missing = []
    for lib in libs:
        try:
            if run([haxelib, "path", lib], timeout=120).returncode != 0:
                if run([haxelib, "install", lib, "--always"], timeout=900).returncode != 0:
                    missing.append(lib)
        except OSError:
            # No haxelib on this machine at all. Only matters if we intend to
            # compile; --skip-build never reaches here.
            missing.append(lib)
    return missing


def hdll_sources(repo_root: pathlib.Path, explicit: str | None) -> list[pathlib.Path]:
    """Where to find HDLLs for this platform.

    The copies committed under examples/heaps_base2d/bin are Mach-O. Staging
    those on Linux would not merely fail to help -- ash would report a load
    error that reads like an ash defect and is really an architecture
    mismatch. So they are used only on macOS, and elsewhere the caller must
    say where a native set lives (a HashLink build tree, typically).
    """
    if explicit:
        return sorted(pathlib.Path(explicit).glob("*.hdll"))
    if sys.platform == "darwin":
        return sorted((repo_root / "examples/heaps_base2d/bin").glob("*.hdll"))
    return []


def stage_hdlls(dest: pathlib.Path, srcs: list[pathlib.Path]) -> int:
    """Put the HDLLs beside the bytecode.

    Several suites declare natives from ssl/fmt/sqlite even in tests that
    never call them, and a missing library is a hard load error rather than a
    lazy one -- so without these the suite does not start at all and reports
    nothing about ash.
    """
    dest.mkdir(parents=True, exist_ok=True)
    for s in srcs:
        shutil.copy2(s, dest / s.name)
    return len(srcs)


def classify(res, elapsed_ms, timed_out) -> tuple[str, str]:
    out = (res.stdout or "") + (res.stderr or "")
    if timed_out:
        return "TIMEOUT", f"no exit within the limit ({elapsed_ms:.0f}ms elapsed)"
    if "=== CRASH:" in out:
        line = next((l for l in out.splitlines() if "=== CRASH:" in l), "")
        return "CRASH", line.strip()[:160]
    if "panicked at" in out:
        line = next((l for l in out.splitlines() if "panicked at" in l), "")
        return "PANIC", line.strip()[:160]
    if res.returncode != 0:
        tail = [l for l in out.splitlines() if l.strip()][-1:] or [""]
        return "FAIL", f"exit {res.returncode}: {tail[0][:140]}"
    if RE_FAIL.search(out) and not RE_OK.search(out):
        line = next((l for l in out.splitlines() if RE_FAIL.search(l)), "")
        return "FAIL", line.strip()[:160]
    return "PASS", ""


# utest's PlainTextReport ends every run with this block. "assertations" is
# its own long-standing spelling, not a typo here.
RE_UTEST = re.compile(
    r"^\s*(assertations|successes|errors|failures|warnings)\s*:\s*(\d+)\s*$",
    re.I | re.M,
)


def parse_utest(out: str) -> dict | None:
    """utest's tally, when the suite got far enough to print one.

    This is what makes a conformance *percentage* meaningful rather than a
    suite-level pass/fail: a run that crashes two thirds of the way through
    still tells you how many assertions it got right first, and "3400 of 5000"
    is a number that moves as things get fixed, where "0 of 3 suites" is not.
    """
    found = {m.group(1).lower(): int(m.group(2)) for m in RE_UTEST.finditer(out)}
    if "assertations" not in found:
        return None
    bad = found.get("errors", 0) + found.get("failures", 0)
    total = found.get("assertations", 0)
    return {
        "assertions": total,
        "successes": found.get("successes", 0),
        "errors": found.get("errors", 0),
        "failures": found.get("failures", 0),
        "warnings": found.get("warnings", 0),
        # Assertions that did not go wrong. A crashed run reports what it
        # reached, which is the point.
        "passed": max(0, total - bad),
        "all_ok": "ALL TESTS OK" in out,
    }


# utest/utils/Print.hx under -D UTEST_PRINT_TESTS: "Running <Case>..." per
# case and a four-space-indented name per test, each printed as it starts.
RE_CASE = re.compile(r"^Running\s+(\S+?)\.\.\.\s*$", re.M)
RE_TEST = re.compile(r"^    ([A-Za-z_][A-Za-z0-9_]*)\s*$", re.M)


def parse_progress(out: str) -> dict:
    """How far the run got before it stopped, however it stopped.

    This is the only measure that survives a crash. It is deliberately a count
    of tests *entered*, not passed: a test that started and then took the VM
    down with it is progress in the sense that matters here — the VM reached
    it — and calling it a pass would be a lie the next fix would expose.
    """
    return {
        "cases_reached": len(RE_CASE.findall(out)),
        "tests_reached": len(RE_TEST.findall(out)),
    }


def missing_natives(out: str) -> list[str]:
    """ash narrates unresolved natives at startup; that line is a finding."""
    for line in out.splitlines():
        if "natives resolved," in line and "missing:" in line:
            return [n.strip() for n in line.split("missing:", 1)[1].split(",")]
    return []


def main(argv=None) -> int:
    repo_root = pathlib.Path(__file__).resolve().parent.parent
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--repo-root", type=pathlib.Path, default=repo_root)
    ap.add_argument("--work", type=pathlib.Path, default=None,
                    help="where to keep the Haxe checkout "
                         "(default: ~/.cache/ash-haxe-conformance)")
    ap.add_argument("--ash", default=None, help="ash binary (default: newest under target/)")
    ap.add_argument("--haxe", default=shutil.which("haxe") or "haxe")
    ap.add_argument("--haxelib", default=shutil.which("haxelib") or "haxelib")
    ap.add_argument("--tag", default=None,
                    help="Haxe tag to test against (default: the installed compiler's version)")
    ap.add_argument("--suites", default=",".join(SUITES),
                    help=f"comma-separated subset of: {', '.join(SUITES)}")
    ap.add_argument("--modes", default="interp",
                    help="ash modes to run, comma-separated (interp, hybrid, jit)")
    ap.add_argument("--reference", default=None,
                    help="a stock HashLink `hl` binary, to separate ash bugs from suite bugs")
    ap.add_argument("--timeout", type=int, default=900)
    ap.add_argument("--json", type=pathlib.Path, default=None)
    ap.add_argument("--baseline", type=pathlib.Path, default=None,
                    help="a previous --json; exit 1 if any suite got worse")
    ap.add_argument("--hdll-dir", default=None,
                    help="directory of .hdll files for this platform; the "
                         "committed macOS set is used automatically on darwin")
    ap.add_argument("--skip-build", action="store_true",
                    help="reuse bytecode already built under the checkout")
    args = ap.parse_args(argv)

    root = args.repo_root.resolve()
    # Deliberately not under target/: rust-cache owns that tree in CI and
    # prunes what it does not recognise, which silently emptied the checkout
    # between runs.
    work = (args.work
            or pathlib.Path.home() / ".cache" / "ash-haxe-conformance").resolve()

    # Release first, and never "whichever is newest".
    #
    # Picking by mtime meant an unrelated debug build could silently become
    # the thing under test, and the two are not interchangeable: on the
    # threads suite the release binary dies with SIGSEGV at 0x9 while the
    # debug binary aborts on `misaligned pointer dereference ... is 0x9`
    # first. Same defect, different report — so a baseline that does not
    # record which one ran cannot be compared against anything.
    ash, profile = args.ash, "explicit"
    if ash is None:
        def find(kind):
            c = [p for p in (root / "target").glob(f"{kind}/ash") if p.is_file()]
            c += [p for p in (root / "target").glob(f"*/{kind}/ash") if p.is_file()]
            c = [p for p in c if os.access(p, os.X_OK)]
            return max(c, key=lambda p: p.stat().st_mtime) if c else None
        rel, dbg = find("release"), find("debug")
        if rel is not None:
            ash, profile = str(rel), "release"
        elif dbg is not None:
            ash, profile = str(dbg), "debug"
            print("NOTE: no release binary; measuring the debug build. Its "
                  "assertions fire before the faults a release build shows, "
                  "so results are not comparable to a release baseline.")
        else:
            sys.exit("no ash binary under target/; build one or pass --ash")
    elif "/release/" in ash:
        profile = "release"
    elif "/debug/" in ash:
        profile = "debug"

    ver = haxe_version(args.haxe)
    if ver is None:
        if not args.skip_build:
            sys.exit(f"no working Haxe compiler at {args.haxe!r}")
        if not args.tag:
            sys.exit("no Haxe compiler here, so the suite tag cannot be "
                     "inferred — pass --tag to say which one the bytecode "
                     "was built from")
        print(f"no Haxe compiler; running pre-built bytecode for tag {args.tag}")
    tag = args.tag or ver
    print(f"ash:   {ash}  [{profile}]")
    print(f"haxe:  {ver or '(none, --skip-build)'}  ->  suite tag {tag}")
    if args.reference:
        print(f"ref:   {args.reference}")

    hdlls = hdll_sources(root, args.hdll_dir)
    if hdlls:
        print(f"hdll:  {len(hdlls)} from {hdlls[0].parent}")
    elif sys.platform != "darwin":
        print("hdll:  none (pass --hdll-dir); suites needing ssl/fmt will not load")

    src = ensure_checkout(work, tag)
    wanted = [s.strip() for s in args.suites.split(",") if s.strip()]
    unknown = [s for s in wanted if s not in SUITES]
    if unknown:
        sys.exit(f"unknown suite(s): {', '.join(unknown)}")
    modes = [m.strip() for m in args.modes.split(",") if m.strip()]

    report = {
        "generated_iso": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "haxe_version": ver,
        "suite_tag": tag,
        "ash": ash,
        "ash_profile": profile,
        "results": [],
    }

    for name in wanted:
        spec = SUITES[name]
        sdir = src / spec["dir"]
        if not sdir.is_dir():
            print(f"\n== {name}: SKIP (not present at tag {tag})")
            continue
        print(f"\n== {name} — {spec['about']}")

        gone = [] if args.skip_build else ensure_libs(spec.get("needs", []), args.haxelib)
        if gone:
            print(f"   SKIP: haxelib(s) unavailable: {', '.join(gone)}")
            report["results"].append({"suite": name, "mode": "-", "status": "SKIP",
                                      "detail": f"missing haxelib {', '.join(gone)}"})
            continue

        if not args.skip_build:
            print(f"   building {spec['hxml']} ...", flush=True)
            r = run([args.haxe, spec["hxml"], *spec.get("args", [])],
                    cwd=str(sdir), timeout=args.timeout)
            if r.returncode != 0:
                err = (r.stdout + r.stderr).strip().splitlines()
                detail = next((l for l in err if "ERROR" in l or "Error" in l), err[-1] if err else "")
                print(f"   SKIP: the suite did not compile — {detail[:150]}")
                report["results"].append({"suite": name, "mode": "-", "status": "SKIP",
                                          "detail": f"compile failed: {detail[:300]}"})
                continue

        for prog in spec["programs"]:
            p = sdir / prog
            if not p.is_file():
                report["results"].append({"suite": name, "mode": "-", "status": "SKIP",
                                          "detail": f"{prog} not produced"})
                continue
            stage_hdlls(p.parent, hdlls)

            engines = [(f"ash:{m}", [ash, "--mode", m]) for m in modes]
            if args.reference:
                engines.append(("hashlink", [args.reference]))

            for label, argv0 in engines:
                t0 = time.perf_counter()
                timed_out = False
                try:
                    res = run(argv0 + [str(p)], cwd=str(p.parent), timeout=args.timeout)
                except subprocess.TimeoutExpired as e:
                    timed_out = True
                    res = subprocess.CompletedProcess(
                        argv0, 1, (e.stdout or b"").decode("utf8", "replace"),
                        (e.stderr or b"").decode("utf8", "replace"))
                ms = (time.perf_counter() - t0) * 1000
                status, detail = classify(res, ms, timed_out)
                natives = missing_natives((res.stdout or "") + (res.stderr or ""))
                rec = {"suite": name, "program": prog, "engine": label,
                       "status": status, "detail": detail, "ms": round(ms, 1)}
                whole = (res.stdout or "") + (res.stderr or "")
                tally = parse_utest(whole)
                if tally:
                    rec["utest"] = tally
                rec["progress"] = parse_progress(whole)
                if natives:
                    rec["missing_natives"] = natives
                report["results"].append(rec)
                mark = "ok  " if status == "PASS" else status.ljust(4)
                print(f"   {mark} {label:<14} {prog:<22} {ms:7.0f}ms  {detail[:80]}")
                if natives:
                    print(f"        unresolved natives ({len(natives)}): {', '.join(natives[:6])}"
                          + (" ..." if len(natives) > 6 else ""))

    ash_rows = [r for r in report["results"]
                if r["engine"].startswith("ash:") and r["status"] != "SKIP"]
    passes = sum(1 for r in ash_rows if r["status"] == "PASS")
    total = len(ash_rows)
    a_pass = sum(r["utest"]["passed"] for r in ash_rows if r.get("utest"))
    a_total = sum(r["utest"]["assertions"] for r in ash_rows if r.get("utest"))
    # How far ash got, and how far the reference VM gets on the same
    # bytecode. The reference is the only honest denominator available: the
    # suite does not publish its own test count, and a total taken from a run
    # that crashed would shrink as ash got worse.
    t_reached = sum((r.get("progress") or {}).get("tests_reached", 0) for r in ash_rows)
    ref_rows = [r for r in report["results"] if r["engine"] == "hashlink"]
    t_total = sum((r.get("progress") or {}).get("tests_reached", 0) for r in ref_rows)
    report["summary"] = {
        "suites_total": total,
        "suites_passed": passes,
        "tests_reached": t_reached,
        "tests_total": t_total or None,
        # The headline. Unlike the assertion tally, this moves the moment ash
        # gets one test further, because utest prints each test as it starts
        # and that output survives a crash.
        "test_pct": round(100.0 * t_reached / t_total, 1) if t_total else None,
        "assertions_total": a_total,
        "assertions_passed": a_pass,
        # None rather than 0 throughout: "we do not know yet" and "we got
        # everything wrong" are different claims, and a site that renders the
        # second when it means the first is lying.
        "assertion_pct": round(100.0 * a_pass / a_total, 1) if a_total else None,
        "suite_pct": round(100.0 * passes / total, 1) if total else None,
    }
    print(f"\n{passes}/{total} suites passed")
    if t_total:
        print(f"{t_reached}/{t_total} tests reached "
              f"({report['summary']['test_pct']}%, denominator from the reference VM)")
    else:
        print(f"{t_reached} tests reached "
              "(no reference VM, so there is no total to divide by)")
    if a_total:
        print(f"{a_pass}/{a_total} assertions passed "
              f"({report['summary']['assertion_pct']}%)")
    else:
        print("no suite ran to completion, so utest printed no assertion tally")

    # Every distinct unresolved native across the whole run, which is the most
    # directly actionable output this harness produces.
    allnat = sorted({n for r in report["results"] for n in r.get("missing_natives", [])})
    if allnat:
        print(f"\nunresolved natives across all suites ({len(allnat)}):")
        for n in allnat:
            print(f"  {n}")
        report["missing_natives"] = allnat

    if args.json:
        args.json.parent.mkdir(parents=True, exist_ok=True)
        args.json.write_text(json.dumps(report, indent=2) + "\n")
        print(f"\nwrote {args.json}")

    if args.baseline and args.baseline.is_file():
        base = json.loads(args.baseline.read_text())
        if base.get("ash_profile") not in (None, profile):
            print(f"\nrefusing to compare a {profile} run against a "
                  f"{base['ash_profile']} baseline — the two report different "
                  "failures for the same defect")
            return 0
        was = {(r["suite"], r.get("program", ""), r["engine"]): r["status"]
               for r in base.get("results", [])}
        regressed = [
            r for r in report["results"]
            if was.get((r["suite"], r.get("program", ""), r["engine"])) == "PASS"
            and r["status"] != "PASS"
        ]
        if regressed:
            print("\nREGRESSED against the baseline:")
            for r in regressed:
                print(f"  {r['suite']}/{r['engine']}: PASS -> {r['status']}  {r['detail'][:100]}")
            return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
