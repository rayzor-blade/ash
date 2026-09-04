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

Not every upstream tree is VM material, and the report says so instead of
omitting them. `misc` is mixed: its expected-compile-error projects run under
the host haxe and land in a compiler bucket kept apart from the VM tally,
while the one piece upstream's Hl CI genuinely executes on a VM (eventLoop)
is built to .hl and run under ash. `optimization`, `nullsafety`, `server` and
`sourcemaps` test the compiler alone and appear as explicit NOT_APPLICABLE
rows, each with its reason.

Exit status is 1 if any suite regressed against `--baseline`, else 0. Without
a baseline it always exits 0: a first run is a measurement, not a verdict.
"""

import argparse
import collections
import concurrent.futures as cf
import functools
import filecmp
import json
import os
import pathlib
import re
import shutil
import subprocess
import sys
import tempfile
import time

# Suites that compile to HashLink bytecode and are meaningful for a VM.
# The remaining upstream trees are covered below: `misc` is mixed (a compiler
# tree plus the piece upstream really does execute on hl), and the trees in
# COMPILER_ONLY_SUITES test the *compiler*, not the runtime -- ash cannot
# pass or fail them, so they get explicit NOT_APPLICABLE rows instead.
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

# Trees that test the compiler alone, surveyed against how upstream CI really
# runs each one (tests/runci/targets/*.hx). None of them ever produces a
# runnable .hl, so ash can neither pass nor fail them -- but a suite absent
# from the report is indistinguishable from a suite someone forgot, so every
# one appears in every run as an explicit NOT_APPLICABLE row with its reason.
COMPILER_ONLY_SUITES = {
    "optimization": "analyzer/codegen assertions over the typed AST, per "
                    "target (upstream Js.hx runs run.hxml under --interp and "
                    "-js); recompiling with -hl fails TestBaseMacro's "
                    "eval-specific const-fold expectations -- no runtime "
                    "behaviour is under test",
    "nullsafety": "compile-time null-safety diagnostics checked by a macro "
                  "hook (upstream Macro.hx runs test.hxml, which has no "
                  "target and emits no artifact at all)",
    "server": "compilation-server/IDE protocol tests, compiled to JS and "
              "driven by node against `haxe --wait` (upstream Js.hx); the "
              "process under test is the compiler itself",
    "sourcemaps": "source-map emission checks run under eval (upstream "
                  "Macro.hx runs run.hxml with --interp); the artifact under "
                  "test is the map, not a program",
}

# `misc` is mixed, and the split matters. Its projects/ tree is a compiler
# suite -- 590 .hxml cases, 384 of them *-fail.hxml expected-COMPILE-error
# tests with .stderr expectations, zero targeting hl -- so those run under
# the host `haxe` exactly as upstream's Macro CI does, and land in a compiler
# bucket deliberately kept apart from the VM tally: ash cannot move that
# number, the rows exist so the tree is visibly covered. The one piece
# upstream's Hl CI target (runci/targets/Hl.hx) both compiles to bytecode AND
# executes on a HashLink VM is eventLoop; that is real VM conformance and
# lands in the ash tally. misc/hl's projects are compile-only even upstream
# (the single .hl they emit, Issue11196, is never executed by CI), so they
# stay on the compiler side rather than being promoted to a VM claim
# upstream never makes.
MISC_ABOUT = ("Mixed tree: an expected-compile-error suite (compiler bucket, "
              "host haxe) plus what upstream's Hl CI actually executes on a "
              "VM (ash's tally).")

# VM side: built to .hl, executed under ash, classified OK/FAIL/CRASH/EMPTY.
MISC_VM_CASES = [
    {
        "name": "eventLoop",
        "dir": "tests/misc/eventLoop",
        "hxml": "build-hl.hxml",
        "program": "eventLoop.hl",
        "about": "haxe.MainLoop/haxe.Timer ordering -- the machinery behind "
                 "every game loop. Upstream disables its stdout expectation "
                 "(build-hl.hxml.stdout.disabled), so exit status is the "
                 "upstream-faithful criterion; a silent exit-0 still lands "
                 "as EMPTY, never as a pass.",
    },
]

# Compiler side: each entry reproduces one upstream invocation, with its own
# cwd and hxml. Entries with "runner" use upstream's own eval harness
# (misc/src/Main.hx): it walks projects/, executes every *.hxml as a fresh
# `haxe`, enforces the expected-failure and .stderr/.stdout contracts, prints
# "Running haxe <path>" per case and "Done running N tests with M failures"
# at the end, and exits with the failure count. Entries without "runner" are
# a single plain compile whose whole contract is exit 0.
MISC_COMPILER_RUNS = [
    {
        "name": "projects",
        "cwd": "tests/misc",
        "hxml": "compile.hxml",
        "runner": True,
        # One invocation covering ~590 fresh `haxe` processes; give it more
        # room than a single suite build gets.
        "timeout_scale": 4,
        "about": "the 590-case expected-error/diagnostic tree (384 "
                 "*-fail.hxml); upstream: Macro.hx",
    },
    {
        "name": "resolution",
        "cwd": "tests/misc/resolution",
        "hxml": "run.hxml",
        "runner": True,
        "about": "import-resolution diagnostics, same runner; upstream: Macro.hx",
    },
    {
        "name": "hl",
        "cwd": "tests/misc/hl",
        "hxml": "run.hxml",
        "runner": True,
        "about": "hl-target compiler checks (hlc.json defines, HLC source "
                 "headers, hl.I64 typing); compile-only even upstream -- the "
                 "one .hl emitted is never executed by CI; upstream: Hl.hx",
    },
    {
        "name": "hl/reserved-keywords",
        "cwd": "tests/misc/hl/reserved-keywords",
        "hxml": "compile.hxml",
        "about": "hl/c emission of C-reserved identifiers; upstream compiles "
                 "this everywhere and only C-compiles+runs it on Linux CI; "
                 "upstream: Hl.hx",
    },
    {
        "name": "compiler_loops",
        "cwd": "tests/misc/compiler_loops",
        "hxml": "run.hxml",
        "runner": True,
        "platforms": ("linux",),
        "about": "compile-time loop/recursion limits; upstream runs this on "
                 "Linux only (its runner shells through `timeout`); "
                 "upstream: Macro.hx",
    },
]

# misc trees that belong to OTHER targets entirely. Nothing in them compiles
# to hl -- forcing them through the hl target would measure preprocessor
# gating, not conformance -- so they are excluded as one visible row rather
# than ninety empty ones.
MISC_OTHER_TARGET_TREES = ("cpp, cppObjc, cs, es6, flash, java, js, lua, "
                           "luaDeadCode, neko, php, python, weakmap")

# Everything a default run covers: the VM suites, the mixed misc tree, and
# the compiler-only trees (whose rows exist to be visibly not-applicable).
ALL_SUITES = [*SUITES, "misc", *COMPILER_ONLY_SUITES]

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
    # A historical cache format kept only built bytecode under tests/. That
    # is enough for unit and threads, but sys reads fixtures and compiles its
    # helper programs at runtime. Treat such a cache as incomplete instead of
    # publishing the resulting missing-file errors as Ash failures.
    checkout_markers = (
        src / ".git",
        src / "tests" / "sys" / "gen_test_res.py",
        src / "tests" / "sys" / "src" / "ExitCode.c",
    )
    if all(marker.exists() for marker in checkout_markers):
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


# The suite's own runner takes no filter: TestMain builds a fixed array of
# case instances and adds every one of them. That is fine until the VM under
# test crashes — utest's tally is printed once, at the end, so ONE bad case
# takes the whole measurement down with it and the run reports as though
# nothing worked. Isolation mode needs two things the suite does not offer:
# enumerate the cases, and run exactly one. This adds both, guarded behind
# argv flags so an unpatched invocation behaves identically.
ISOLATION_HOOK = """\
	var runner = new Runner();
	var __ashArgs = Sys.args();
	var __ashOnly = null;
	for (__i in 0...__ashArgs.length) {
		if (__ashArgs[__i] == "--ash-list") {
			for (__c in classes) Sys.println("ASHCASE " + Type.getClassName(Type.getClass(__c)));
			return;
		}
		if (__ashArgs[__i] == "--ash-only" && __i + 1 < __ashArgs.length) __ashOnly = __ashArgs[__i + 1];
	}
	for (c in classes) {
		if (__ashOnly != null && Type.getClassName(Type.getClass(c)) != __ashOnly) continue;
		runner.addCase(c);
	}
"""

ISOLATION_ORIGINAL = """\
	var runner = new Runner();
	for (c in classes) {
		runner.addCase(c);
	}
"""


def patch_for_isolation(src: pathlib.Path) -> bool:
    """Teach the unit suite's TestMain to list and to filter its cases.

    Returns True when the suite is patched (already or newly). Idempotent, and
    a no-op returning False if upstream's runner setup ever stops matching —
    isolation is then simply unavailable rather than silently mismeasuring.
    """
    main_hx = src / "tests" / "unit" / "src" / "unit" / "TestMain.hx"
    if not main_hx.is_file():
        return False
    text = main_hx.read_text()
    if "--ash-only" in text:
        return True
    if ISOLATION_ORIGINAL not in text:
        return False
    main_hx.write_text(text.replace(ISOLATION_ORIGINAL, ISOLATION_HOOK))
    return True


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


def stage_macos_libhl(ash: str) -> None:
    """Give Mach-O HDLLs the current ash runtime under their import name."""
    if sys.platform != "darwin":
        return
    exe_dir = pathlib.Path(ash).resolve().parent
    runtime = exe_dir / "libash_std.dylib"
    compat = exe_dir / "libhl.dylib"
    if not runtime.is_file():
        if not compat.is_file():
            print("WARNING: no libhl.dylib beside ash; Mach-O HDLLs may bind a "
                  "stale system HashLink runtime")
        return
    if compat.is_file() and filecmp.cmp(runtime, compat, shallow=False):
        return
    try:
        shutil.copy2(runtime, compat)
        print(f"libhl: current ash runtime staged at {compat}")
    except OSError as exc:
        sys.exit(f"could not stage the macOS libhl compatibility image: {exc}")


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
# utest's per-test verdict, printed for tests that did not pass:
#   "  testNanIfs: FAILURE FFFFFFFFFFFFFF."
# Successes print nothing (the report is configured NeverShowSuccessResults),
# so passed = accepted - these.
RE_TEST_BAD = re.compile(r"^  ([A-Za-z_][A-Za-z0-9_]*):\s+(FAILURE|ERROR)\b", re.M)


def parse_progress(out: str) -> dict:
    """How far the run got before it stopped, however it stopped.

    This is the only measure that survives a crash. It is deliberately a count
    of tests *entered*, not passed: a test that started and then took the VM
    down with it is progress in the sense that matters here — the VM reached
    it — and calling it a pass would be a lie the next fix would expose.
    """
    accepted = len(RE_TEST.findall(out))
    bad = len(set(m.group(1) for m in RE_TEST_BAD.finditer(out)))
    return {
        "cases_reached": len(RE_CASE.findall(out)),
        "tests_reached": accepted,
        # "accepted" is tests ash actually took on: utest names each one as it
        # starts, so this survives a crash mid-case. "passed" subtracts the
        # ones it then reported as FAILURE or ERROR.
        "tests_accepted": accepted,
        "tests_passed": max(0, accepted - bad),
        "tests_bad": bad,
    }


# ---------------------------------------------------------------------------
# Isolation mode
#
# One process per test case. The whole reason this exists: the suite's runner
# holds every case in one process and prints its tally once, at the end, so a
# VM that crashes in case 72 of 1195 reports exactly what a VM that crashes in
# case 1 reports — nothing. That makes the headline number a crash detector
# rather than a conformance measure, and it cannot improve until the LAST
# crash is fixed. Run each case on its own and a crash costs one case: every
# other case still votes, the percentage is real from the first run, and it
# rises with every fix instead of staying at zero.
# ---------------------------------------------------------------------------

RE_ASHCASE = re.compile(r"^ASHCASE\s+(\S+)\s*$", re.M)

# ---------------------------------------------------------------------------
# The wasm engine
#
# wasm is ahead-of-time only: there is no interpreter inside the module and no
# ash process around it, so "wasm" is not a mode ash has. It is a build and
# then a host. The build is per PROGRAM and the run is per CASE, which is the
# shape isolation already wants -- one link, then a run for each of 1195 cases
# -- so a module is built once and remembered, and a build that fails is
# remembered too rather than re-attempted 1195 times.
#
# Everything else about a case is unchanged: the same patched suite and the
# same `--ash-only` argument, which reaches `Sys.args()` in a compiled program
# exactly as it does under the interpreter.
# ---------------------------------------------------------------------------

WASM_TRIPLE = "wasm32-wasip1"
_WASM_MODULES: dict[str, str] = {}
_WASM_FAILURES: dict[str, str] = {}


def wasm_runner(ash: str) -> str:
    """The host that runs a module: beside ash, then on PATH."""
    beside = pathlib.Path(ash).parent / "ash-wasm-run"
    if beside.is_file():
        return str(beside)
    found = shutil.which("ash-wasm-run")
    if found:
        return found
    raise RuntimeError("no ash-wasm-run found beside ash or on PATH")


def wasm_module_for(ash: str, program: pathlib.Path, timeout: int) -> str:
    """Build `program` to wasm, once."""
    key = str(program)
    if key in _WASM_FAILURES:
        raise RuntimeError(_WASM_FAILURES[key])
    if key in _WASM_MODULES:
        return _WASM_MODULES[key]
    out = program.with_suffix(".wasm")
    # A module newer than its bytecode, the compiler that made it and the
    # runtime object linked into it is the module this build would produce.
    # The build is minutes, so a second measurement of the same ash against
    # the same suite should not pay it. The runtime object counts because it
    # is the piece that changes without the compiler changing: a std fix is
    # invisible to a comparison against `ash` alone.
    runtime = pathlib.Path(ash).parent / WASM_TRIPLE / "ash_runtime.o"
    try:
        built = out.stat().st_mtime
        inputs = [program.stat().st_mtime, os.stat(ash).st_mtime]
        if runtime.is_file():
            inputs.append(runtime.stat().st_mtime)
        if all(built > t for t in inputs):
            _WASM_MODULES[key] = str(out)
            return _WASM_MODULES[key]
    except FileNotFoundError:
        pass
    # Its own budget, not the caller's. A per-case timeout is seconds and a
    # whole-program AOT build is minutes -- the suite's main program is over a
    # megabyte of bytecode, and a cross build compiles it in one piece because
    # sharding is joined by the host's linker.
    try:
        r = run([ash, "--build", str(out), "--target", WASM_TRIPLE, str(program)],
                cwd=str(program.parent), timeout=max(timeout, 1800))
    except subprocess.TimeoutExpired:
        # One error type out of here, so a caller has one thing to catch.
        _WASM_FAILURES[key] = (
            f"building {program.name} for {WASM_TRIPLE} did not finish in "
            f"{max(timeout, 1800)}s")
        raise RuntimeError(_WASM_FAILURES[key]) from None
    if r.returncode != 0 or not out.is_file():
        why = ((r.stderr or "") + (r.stdout or "")).strip().splitlines()
        _WASM_FAILURES[key] = (
            f"building {program.name} for {WASM_TRIPLE} failed: "
            + (why[-1] if why else "no output"))
        raise RuntimeError(_WASM_FAILURES[key])
    _WASM_MODULES[key] = str(out)
    return str(out)


def engine_argv(ash: str, program: pathlib.Path, mode: str, timeout: int) -> list[str]:
    """The whole command that runs `program` under `mode`, program included.

    Program-included rather than a prefix, because the wasm engine does not
    run the `.hl` at all: it runs the module built from it.
    """
    if mode == "wasm":
        return [wasm_runner(ash), wasm_module_for(ash, program, timeout)]
    return [ash, "--mode", mode, str(program)]




def list_cases(ash: str, program: pathlib.Path, mode: str, timeout: int) -> list[str]:
    """Ask the patched suite to name its cases."""
    r = run(engine_argv(ash, program, mode, timeout) + ["--ash-list"],
            cwd=str(program.parent.parent), timeout=timeout)
    return RE_ASHCASE.findall(r.stdout or "")


def is_empty_on_target(out: str, tally: dict) -> bool:
    """Did utest fail solely because the class defines no runnable tests?

    Deliberately narrow: exactly one failed assertion, none passed, and the
    reported reason is utest's own "No tests executed". A class that has
    tests and fails them looks nothing like this.
    """
    if tally.get("successes") or tally.get("errors"):
        return False
    if tally.get("assertions") != 1 or tally.get("failures") != 1:
        return False
    return "No tests executed" in out


def run_one_case(ash: str, program: pathlib.Path, mode: str, case: str,
                 timeout: int) -> dict:
    """Run a single case in its own process and classify the outcome.

    `crash` is kept distinct from `fail` on purpose. A failing case is ash
    disagreeing with Haxe about a result — a conformance gap. A crashing case
    is ash losing the VM — a defect of a different severity, and the thing
    isolation exists to stop from swallowing everything after it.
    """
    started = time.time()
    timed_out = False
    try:
        r = run(engine_argv(ash, program, mode, timeout) + ["--ash-only", case],
                cwd=str(program.parent.parent), timeout=timeout)
        out = (r.stdout or "") + (r.stderr or "")
        rc = r.returncode
    except subprocess.TimeoutExpired as e:
        out = (e.stdout or b"").decode("utf-8", "replace") if isinstance(e.stdout, bytes) else (e.stdout or "")
        rc, timed_out = -1, True
    elapsed = (time.time() - started) * 1000.0

    tally = parse_utest(out)
    progress = parse_progress(out)
    if timed_out:
        status = "TIMEOUT"
    elif tally is None:
        # No tally printed at all: the VM did not survive to the end of the
        # case, whatever the exit code claims.
        status = "CRASH"
    elif tally["all_ok"]:
        status = "OK"
    elif is_empty_on_target(out, tally):
        # The class has no test methods on the hl target -- every one of them
        # sits behind `#if js`, `#if jvm`, `#if !hl` or similar, or they are
        # statics utest cannot discover. utest reports that as one failed
        # assertion ("No tests executed"), which is right for a run and wrong
        # for a CASE: isolation puts exactly one class in each run, so a class
        # that is empty on this target lands as a failed case.
        #
        # Measured on the 4.3.6 suite: 126 of 197 "failures" were this, so the
        # headline read 83.5% where the suite ash can actually attempt is
        # 93.4%. The reference VM agrees -- it runs the whole program in one
        # process, where empty classes contribute nothing and the aggregate
        # passes, which is why "tests reached" already read 93.7%.
        status = "EMPTY"
    else:
        status = "FAIL"

    detail = ""
    if status in ("CRASH", "TIMEOUT"):
        for line in out.splitlines():
            if "CRASH:" in line or "panicked at" in line or "uncaught exception" in line:
                detail = line.strip()[:300]
                break
        if not detail and rc:
            detail = f"exit {rc}"
    return {
        "case": case,
        "status": status,
        "ms": round(elapsed, 1),
        "tests_reached": progress["tests_reached"],
        "tests_accepted": progress["tests_accepted"],
        "tests_passed": progress["tests_passed"],
        "assertions": (tally or {}).get("assertions", 0),
        "assertions_passed": (tally or {}).get("passed", 0),
        "errors": (tally or {}).get("errors", 0),
        "failures": (tally or {}).get("failures", 0),
        "detail": detail,
    }


def run_isolated(ash: str, program: pathlib.Path, mode: str, timeout: int,
                 jobs: int, limit: int | None = None) -> dict:
    """Every case, each in its own process, aggregated into one verdict."""
    try:
        cases = list_cases(ash, program, mode, timeout)
    except RuntimeError as e:
        # A wasm build that failed: report it once, as this program's verdict,
        # rather than letting it end the sweep.
        return {"error": str(e)}
    if not cases:
        return {"error": "no cases enumerated (is the suite patched and rebuilt?)"}
    if limit:
        cases = cases[:limit]
    print(f"  isolation: {len(cases)} cases, {jobs} at a time", flush=True)

    results: list[dict] = []
    with cf.ThreadPoolExecutor(max_workers=jobs) as pool:
        futs = {pool.submit(run_one_case, ash, program, mode, c, timeout): c
                for c in cases}
        done = 0
        for fut in cf.as_completed(futs):
            results.append(fut.result())
            done += 1
            if done % 100 == 0 or done == len(cases):
                print(f"    {done}/{len(cases)} cases", flush=True)

    results.sort(key=lambda r: r["case"])
    counts = collections.Counter(r["status"] for r in results)
    # EMPTY is utest's synthetic one-assertion failure for a class with no
    # runnable methods on this target. It is not a Haxe assertion and the
    # reference whole-suite run does not count it, so exclude it from both
    # sides of the assertion score just as we do from the case denominator.
    scored_results = [r for r in results if r["status"] != "EMPTY"]
    assertions = sum(r["assertions"] for r in scored_results)
    passed = sum(r["assertions_passed"] for r in scored_results)
    # Classes with no tests on this target are not a score ash can move, so
    # they are named and excluded from the denominator rather than counted as
    # failures. They stay in `cases_total` so nothing is hidden.
    empty = counts["EMPTY"]
    attemptable = len(results) - empty
    return {
        # The headline. Its denominator is every case that HAS a test to run
        # on this target, so it cannot be gamed by a crash -- a case that
        # takes the VM down did not pass and still counts against us -- nor
        # depressed by classes the target compiles away to nothing.
        "cases_total": len(results),
        "cases_empty": empty,
        "cases_attemptable": attemptable,
        "cases_ok": counts["OK"],
        "cases_failed": counts["FAIL"],
        "cases_crashed": counts["CRASH"],
        "cases_timeout": counts["TIMEOUT"],
        "case_pct": round(100.0 * counts["OK"] / attemptable, 2) if attemptable else None,
        # Kept so the change in denominator is auditable rather than implied.
        "case_pct_of_all": round(100.0 * counts["OK"] / len(results), 2) if results else None,
        # Finer grained, and deliberately NOT the headline: a crashed case
        # prints no tally, so its assertions are missing from BOTH sides of
        # this ratio. That flatters us — the more cases crash, the higher it
        # reads. Quote it only alongside cases_crashed, or against a
        # reference denominator (see assertions_reference below).
        "assertions_of_completed": assertions,
        "assertions_passed": passed,
        "assertion_pct_of_completed":
            round(100.0 * passed / assertions, 2) if assertions else None,
        "tests_reached": sum(r["tests_reached"] for r in results),
        "tests_accepted": sum(r["tests_accepted"] for r in results),
        "tests_passed": sum(r["tests_passed"] for r in results),
        "results": results,
    }


def missing_natives(out: str) -> list[str]:
    """ash narrates unresolved natives at startup; that line is a finding."""
    for line in out.splitlines():
        if "natives resolved," in line and "missing:" in line:
            return [n.strip() for n in line.split("missing:", 1)[1].split(",")]
    return []


# ---------------------------------------------------------------------------
# The misc tree
#
# Two buckets from one suite, and the report never blends them. Compiler rows
# carry engine "haxe" and bucket "compiler": the VM summary filters on
# engine "ash:*", so nothing in that bucket can leak into ash's score in
# either direction. VM rows carry engine "ash:<mode>" like every other suite
# and use the per-case OK/FAIL/CRASH/EMPTY vocabulary.
# ---------------------------------------------------------------------------

# The upstream misc runner's own narration; both are stable output of
# misc/src/Main.hx.
RE_MISC_CASE = re.compile(r"^Running haxe (\S+)", re.M)
RE_MISC_DONE = re.compile(r"^Done running (\d+) tests? with (\d+) failures?", re.M)
# Failing case paths as the SUMMARY block names them. The runner prints that
# block only for runs of more than 20 cases; smaller runs inline their
# diagnostics instead, so the output tail is the fallback.
RE_MISC_FAILPATH = re.compile(r"^(projects[/\\]\S+\.hxml)\s*$", re.M)


# Backends and runners an upstream misc case can shell out to, keyed by the
# hxml flag that selects them. A case naming one of these is testing THAT
# generator, not the compiler's HL path, so on a host without the tool the
# case cannot run at all — and reporting it as a failure would make ash's
# report depend on which SDKs happen to be installed.
CASE_TOOLCHAIN = [
    (re.compile(r"(?<![-\w])--?cmd\s+node\b"), "node"),
    (re.compile(r"(?<![-\w])--?java\s"), "javac"),
    (re.compile(r"(?<![-\w])--?cs\s"), "mcs"),
    (re.compile(r"(?<![-\w])--?cpp\s"), "g++"),
    (re.compile(r"(?<![-\w])--?php\s"), "php"),
    (re.compile(r"(?<![-\w])--?python\s"), "python3"),
    (re.compile(r"(?<![-\w])--?lua\s"), "lua"),
    (re.compile(r"(?<![-\w])--?neko\s"), "neko"),
]


@functools.lru_cache(maxsize=None)
def host_has_tool(tool: str) -> bool:
    """Is `tool` actually usable here, not merely present?

    macOS ships a /usr/bin/javac shim that exists, resolves, and then tells
    you to install a JDK — so presence on PATH is not the question, and
    `shutil.which` alone would call this host Java-capable and file the
    resulting failure against ash.
    """
    exe = shutil.which(tool)
    if exe is None:
        return False
    if tool in ("javac", "java"):
        try:
            return run([exe, "-version"], timeout=60).returncode == 0
        except (OSError, subprocess.TimeoutExpired):
            return False
    return True


def case_missing_tool(case_dir: pathlib.Path, rel: str) -> str | None:
    """The external tool this case needs and this host lacks, if any.

    Read from the case's own hxml rather than guessed from its name. Returns
    None when the case is either self-contained or fully equipped here, in
    which case a failure is a real verdict and stays one.
    """
    hxml = case_dir / rel
    try:
        text = hxml.read_text(errors="replace")
    except OSError:
        return None
    for pattern, tool in CASE_TOOLCHAIN:
        if pattern.search(text) and not host_has_tool(tool):
            return tool
    return None


def haxe_env(haxe: str) -> dict:
    """PATH with the compiler under test first.

    The misc runners invoke the bare name `haxe` for every case, and several
    hl projects re-enter it via `--cmd haxe --run ...` from inside an hxml,
    so whichever haxe leads PATH is the one actually measured. Pin it to the
    one this script was pointed at.
    """
    env = os.environ.copy()
    resolved = shutil.which(haxe) or haxe
    env["PATH"] = (str(pathlib.Path(resolved).resolve().parent)
                   + os.pathsep + env.get("PATH", ""))
    return env


def run_misc_compiler(spec: dict, src: pathlib.Path, haxe: str,
                      timeout: int, misc_filter: str | None) -> dict:
    """One upstream compiler-runner invocation, as a compiler-bucket row."""
    rec = {"suite": "misc", "program": f"{spec['name']}/{spec['hxml']}",
           "engine": "haxe", "bucket": "compiler", "detail": ""}
    cdir = src / spec["cwd"]
    if not cdir.is_dir():
        rec.update(status="SKIP", detail=f"{spec['cwd']} not present at this tag")
        return rec
    if spec.get("platforms") and sys.platform not in spec["platforms"]:
        rec.update(status="SKIP",
                   detail=f"upstream runs this on {'/'.join(spec['platforms'])} only")
        return rec
    cmd = [haxe, spec["hxml"]]
    if misc_filter and spec.get("runner"):
        # Before the hxml, not after: several of these hxmls end in `--run
        # Main`, and anything appended after that point becomes a program
        # argument -- the define would silently never reach the compiler.
        cmd = [haxe, "-D", f"MISC_TEST_FILTER={misc_filter}", spec["hxml"]]
    limit = timeout * spec.get("timeout_scale", 1)
    t0 = time.perf_counter()
    try:
        r = run(cmd, cwd=str(cdir), timeout=limit, env=haxe_env(haxe))
    except subprocess.TimeoutExpired:
        rec.update(status="TIMEOUT", detail=f"no exit within {limit}s")
        return rec
    except OSError as exc:
        # No working haxe (e.g. --skip-build on a compilerless machine). The
        # VM suites can still run pre-built bytecode; this bucket cannot.
        rec.update(status="SKIP", detail=f"cannot invoke {haxe}: {exc}")
        return rec
    rec["ms"] = round((time.perf_counter() - t0) * 1000, 1)
    out = (r.stdout or "") + (r.stderr or "")
    tail = " / ".join(l.strip() for l in out.splitlines() if l.strip())[-400:]
    if not spec.get("runner"):
        # A plain compile: exit status is the whole contract.
        ok = r.returncode == 0
        rec.update(status="OK" if ok else "FAIL",
                   cases_total=1, cases_ok=int(ok), cases_failed=int(not ok),
                   detail="" if ok else f"exit {r.returncode}: {tail[-200:]}")
        return rec
    done = RE_MISC_DONE.search(out)
    if done is None:
        # The runner died before printing its tally -- a harness failure, not
        # a counted verdict. How many cases it started is still worth keeping.
        rec.update(status="CRASH", cases_total=len(RE_MISC_CASE.findall(out)),
                   detail=f"runner died before its tally (exit {r.returncode}): "
                          f"{tail[-200:]}")
        return rec
    # The Done line, not the exit code, is the authority: the runner exits
    # with its failure count, which wraps at 256.
    total, failed = int(done.group(1)), int(done.group(2))
    rec.update(cases_total=total, cases_ok=total - failed, cases_failed=failed,
               status="OK" if failed == 0 else "FAIL", detail=done.group(0))
    if failed:
        paths = (RE_MISC_FAILPATH.findall(out.split("SUMMARY:", 1)[1])
                 if "SUMMARY:" in out else [])
        rec["failed_cases"] = paths[:40]
        if not paths:
            rec["detail"] += " | " + tail[-300:]
        # Separate "this host cannot run the case" from "the case disagreed".
        # Both arrive as a failure in the runner's tally, and only the second
        # is a verdict: a case that shells into node or a JDK is testing that
        # backend, so on a machine without one it is unrun, not failing.
        # Without this split the report's headline moves with the SDKs
        # installed on whoever's laptop, which is the one property a
        # conformance gate must not have.
        blocked = {}
        for rel in rec["failed_cases"]:
            tool = case_missing_tool(cdir, rel)
            if tool:
                blocked[rel] = tool
        if not rec["failed_cases"]:
            # Runs of 20 cases or fewer inline their diagnostics instead of
            # printing a SUMMARY block, so the failures are unnamed. Every
            # case the runner STARTED is named though, and a case needing an
            # absent tool cannot have passed — so scan those for missing
            # prerequisites and attribute up to the failure count. Capped at
            # `failed` so this can only ever explain failures, never invent
            # or hide them.
            candidates = {}
            for rel in RE_MISC_CASE.findall(out):
                tool = case_missing_tool(cdir, rel)
                if tool:
                    candidates[rel] = tool
            for rel, tool in sorted(candidates.items())[:failed]:
                blocked[rel] = tool
            if blocked:
                rec["blocked_attribution"] = (
                    "by prerequisite scan; this runner does not name its "
                    "failures")
        if blocked:
            rec["blocked_cases"] = blocked
            rec["cases_blocked"] = len(blocked)
            rec["cases_failed"] = failed - len(blocked)
            missing = sorted(set(blocked.values()))
            rec["detail"] += (f" | {len(blocked)} of them unrunnable here "
                              f"(missing: {', '.join(missing)})")
            if rec["cases_failed"] == 0:
                # Nothing actually disagreed; the row is as green as this
                # host can make it.
                rec["status"] = "OK"
    return rec


def classify_vm_program(out: str, rc: int, elapsed_ms: float,
                        timed_out: bool) -> tuple[str, str]:
    """OK/FAIL/CRASH/TIMEOUT/EMPTY for a plain (non-utest) .hl program.

    Exit status is the criterion, as it is for upstream's Hl CI target -- so
    no verdict-line grepping here: these programs trace freely and their text
    is not a contract. EMPTY follows the is_empty_on_target precedent: they
    narrate what they do via trace(), so a run that exits 0 having printed
    nothing means the target conditionals compiled the interesting code away
    (or the VM ran none of it) -- report that, never count it as a pass.
    """
    if timed_out:
        return "TIMEOUT", f"no exit within the limit ({elapsed_ms:.0f}ms elapsed)"
    for needle in ("=== CRASH:", "panicked at"):
        if needle in out:
            line = next((l for l in out.splitlines() if needle in l), "")
            return "CRASH", line.strip()[:200]
    if rc != 0:
        last = [l for l in out.splitlines() if l.strip()][-1:] or [""]
        return "FAIL", f"exit {rc}: {last[0][:140]}"
    if not out.strip():
        return "EMPTY", "exited 0 without printing anything; nothing ran on this target"
    return "OK", ""


def run_misc_suite(src: pathlib.Path, haxe: str, ash: str, modes: list[str],
                   reference: str | None, timeout: int, skip_build: bool,
                   misc_filter: str | None) -> list[dict]:
    """The whole misc tree: compiler bucket, exclusion row, then VM cases."""
    rows = []
    print(f"\n== misc — {MISC_ABOUT}")

    for spec in MISC_COMPILER_RUNS:
        rec = run_misc_compiler(spec, src, haxe, timeout, misc_filter)
        rows.append(rec)
        mark = "ok  " if rec["status"] == "OK" else rec["status"].ljust(4)
        cases = (f"{rec.get('cases_ok', 0)}/{rec.get('cases_total', 0)} cases"
                 if "cases_total" in rec else "")
        print(f"   {mark} {'haxe':<14} {rec['program']:<34} "
              f"{rec.get('ms', 0):7.0f}ms  {cases:<14} {rec['detail'][:60]}")

    # The other targets' project trees, excluded as one visible row.
    excl = {"suite": "misc", "program": "per-target trees", "engine": "-",
            "bucket": "compiler", "status": "NOT_APPLICABLE",
            "detail": f"other targets' project dirs ({MISC_OTHER_TARGET_TREES}): "
                      "nothing in them compiles to hl"}
    rows.append(excl)
    print(f"   n/a  {'-':<14} {excl['program']:<34} {excl['detail'][:76]}")

    for case in MISC_VM_CASES:
        sdir = src / case["dir"]
        prog_id = f"{case['name']}/{case['program']}"
        if not sdir.is_dir():
            rows.append({"suite": "misc", "program": prog_id, "engine": "-",
                         "bucket": "vm", "status": "SKIP",
                         "detail": f"{case['dir']} not present at this tag"})
            continue
        if not skip_build:
            print(f"   building {case['hxml']} ...", flush=True)
            r = run([haxe, case["hxml"]], cwd=str(sdir), timeout=timeout,
                    env=haxe_env(haxe))
            if r.returncode != 0:
                err = (r.stdout + r.stderr).strip().splitlines()
                detail = next((l for l in err if "Error" in l),
                              err[-1] if err else "")
                print(f"   SKIP: {case['name']} did not compile — {detail[:150]}")
                rows.append({"suite": "misc", "program": prog_id, "engine": "-",
                             "bucket": "vm", "status": "SKIP",
                             "detail": f"compile failed: {detail[:300]}"})
                continue
        p = sdir / case["program"]
        if not p.is_file():
            rows.append({"suite": "misc", "program": prog_id, "engine": "-",
                         "bucket": "vm", "status": "SKIP",
                         "detail": f"{case['program']} not produced"})
            continue

        try:
            engines = [(f"ash:{m}", engine_argv(ash, p, m, timeout)) for m in modes]
        except RuntimeError as e:
            rows.append({"suite": "misc", "program": prog_id, "engine": "-",
                         "bucket": "vm", "status": "SKIP", "detail": str(e)})
            continue
        if reference:
            engines.append(("hashlink", [reference, str(p)]))
        for label, argv0 in engines:
            t0 = time.perf_counter()
            timed_out = False
            try:
                # eventLoop finishes in about a second; a hang here is a
                # finding in its own right, not a reason to stall the run
                # for the full suite budget.
                res = run(argv0, cwd=str(sdir), timeout=min(timeout, 120))
                out = (res.stdout or "") + (res.stderr or "")
                rc = res.returncode
            except subprocess.TimeoutExpired as e:
                out = ((e.stdout or b"").decode("utf-8", "replace")
                       if isinstance(e.stdout, bytes) else (e.stdout or ""))
                rc, timed_out = -1, True
            ms = (time.perf_counter() - t0) * 1000
            status, detail = classify_vm_program(out, rc, ms, timed_out)
            rec = {"suite": "misc", "program": prog_id, "engine": label,
                   "bucket": "vm", "status": status, "detail": detail,
                   "ms": round(ms, 1)}
            natives = missing_natives(out)
            if natives:
                rec["missing_natives"] = natives
            rows.append(rec)
            mark = "ok  " if status == "OK" else status.ljust(4)
            print(f"   {mark} {label:<14} {prog_id:<34} {ms:7.0f}ms  {detail[:70]}")
    return rows


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
    ap.add_argument("--suites", default=",".join(ALL_SUITES),
                    help=f"comma-separated subset of: {', '.join(ALL_SUITES)}")
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
    ap.add_argument("--isolate", action="store_true",
                    help="run each test case in its own process, so one crash "
                         "costs one case instead of the whole measurement "
                         "(unit suite only; patches the suite's TestMain)")
    ap.add_argument("--isolate-jobs", type=int, default=8,
                    help="cases to run concurrently in --isolate (default 8)")
    ap.add_argument("--isolate-timeout", type=int, default=60,
                    help="per-case timeout in seconds for --isolate (default 60)")
    ap.add_argument("--isolate-limit", type=int, default=None,
                    help="run only the first N cases (for a quick check)")
    ap.add_argument("--misc-filter", default=None,
                    help="regex handed to the misc projects runners as "
                         "MISC_TEST_FILTER, limiting which .hxml cases they "
                         "execute (for a quick check)")
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

    # Suite programs intentionally run from their upstream suite roots, so
    # command paths supplied relative to the repository must be made stable
    # before any cwd switch.
    ash = str(pathlib.Path(ash).resolve())
    if args.reference:
        args.reference = str(pathlib.Path(args.reference).expanduser().resolve())

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
        stage_macos_libhl(ash)
        print(f"hdll:  {len(hdlls)} from {hdlls[0].parent}")
    elif sys.platform != "darwin":
        print("hdll:  none (pass --hdll-dir); suites needing ssl/fmt will not load")

    src = ensure_checkout(work, tag)
    wanted = [s.strip() for s in args.suites.split(",") if s.strip()]
    unknown = [s for s in wanted if s not in ALL_SUITES]
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

    # The sys suite launches helper .hl programs through the conventional
    # `hl` command. Point that name at the engine currently being measured;
    # otherwise a missing system HashLink fails valid Ash runs, while an
    # installed one quietly measures two different VMs in the same row.
    vm_shim_temp = tempfile.TemporaryDirectory(prefix="ash-haxe-vm-")
    vm_shims = {}

    def env_for_vm(base_env, vm):
        shim = vm_shims.get(vm)
        if shim is None:
            shim = pathlib.Path(vm_shim_temp.name) / str(len(vm_shims))
            shim.mkdir()
            launcher = shim / ("hl.exe" if os.name == "nt" else "hl")
            try:
                launcher.symlink_to(vm)
            except OSError:
                shutil.copy2(vm, launcher)
            vm_shims[vm] = shim
        env = base_env.copy()
        env["PATH"] = str(shim) + os.pathsep + env.get("PATH", "")
        return env

    for name in wanted:
        if name in COMPILER_ONLY_SUITES:
            reason = COMPILER_ONLY_SUITES[name]
            print(f"\n== {name}: NOT APPLICABLE — {reason}")
            report["results"].append({"suite": name, "program": "",
                                      "engine": "-", "bucket": "compiler",
                                      "status": "NOT_APPLICABLE",
                                      "detail": reason})
            continue
        if name == "misc":
            report["results"].extend(run_misc_suite(
                src, args.haxe, ash, modes, args.reference, args.timeout,
                args.skip_build, args.misc_filter))
            continue
        spec = SUITES[name]
        sdir = src / spec["dir"]
        if not sdir.is_dir():
            print(f"\n== {name}: SKIP (not present at tag {tag})")
            continue
        print(f"\n== {name} — {spec['about']}")

        isolating = bool(args.isolate) and name == "unit"
        if isolating and not patch_for_isolation(src):
            print("   NOTE: could not patch TestMain for isolation; "
                  "falling back to whole-suite runs")
            isolating = False

        gone = [] if args.skip_build else ensure_libs(spec.get("needs", []), args.haxelib)
        if gone:
            print(f"   SKIP: haxelib(s) unavailable: {', '.join(gone)}")
            report["results"].append({"suite": name, "mode": "-", "status": "SKIP",
                                      "detail": f"missing haxelib {', '.join(gone)}"})
            continue

        if not args.skip_build:
            print(f"   building {spec['hxml']} ...", flush=True)
            # Upstream deliberately ships tests/sys/compile-fs.hxml with its
            # invalid-Unicode filesystem probe enabled and tells APFS users to
            # comment that define out. Do exactly that for the duration of a
            # Darwin build, then restore the checkout byte-for-byte. This is
            # upstream's platform switch, not a locally altered test case.
            fs_hxml = sdir / "compile-fs.hxml"
            fs_hxml_original = None
            if name == "sys" and sys.platform == "darwin" and fs_hxml.is_file():
                fs_hxml_original = fs_hxml.read_text()
                fs_hxml.write_text(fs_hxml_original.replace(
                    "-D TEST_INVALID_UNICODE_FS",
                    "# -D TEST_INVALID_UNICODE_FS (disabled on APFS)",
                ))
            try:
                r = run([args.haxe, spec["hxml"], *spec.get("args", [])],
                        cwd=str(sdir), timeout=args.timeout)
            finally:
                if fs_hxml_original is not None:
                    fs_hxml.write_text(fs_hxml_original)
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

            try:
                engines = [(f"ash:{m}", engine_argv(ash, p, m, args.timeout))
                           for m in modes]
            except RuntimeError as e:
                report["results"].append({"suite": name, "program": prog,
                                          "engine": "-", "status": "SKIP",
                                          "detail": str(e)})
                continue
            if args.reference:
                engines.append(("hashlink", [args.reference, str(p)]))

            if isolating:
                for m in modes:
                    label = f"ash:{m}"
                    print(f"   {label}: per-case isolation", flush=True)
                    iso = run_isolated(ash, p, m, args.isolate_timeout,
                                       args.isolate_jobs, args.isolate_limit)
                    if "error" in iso:
                        print(f"   SKIP: {iso['error']}")
                        report["results"].append({
                            "suite": name, "program": prog, "engine": label,
                            "status": "SKIP", "detail": iso["error"]})
                        continue
                    status = ("OK" if iso["cases_ok"] == iso["cases_attemptable"]
                              else "PARTIAL")
                    rec = {
                        "suite": name, "program": prog, "engine": label,
                        "status": status, "isolated": True,
                        "cases_total": iso["cases_total"],
                        "cases_empty": iso["cases_empty"],
                        "cases_attemptable": iso["cases_attemptable"],
                        "cases_ok": iso["cases_ok"],
                        "cases_failed": iso["cases_failed"],
                        "cases_crashed": iso["cases_crashed"],
                        "cases_timeout": iso["cases_timeout"],
                        "case_pct": iso["case_pct"],
                        "assertions_of_completed": iso["assertions_of_completed"],
                        "assertions_passed": iso["assertions_passed"],
                        "assertion_pct_of_completed": iso["assertion_pct_of_completed"],
                        "progress": {"cases_reached": iso["cases_total"],
                                     "tests_reached": iso["tests_reached"],
                                     "tests_accepted": iso["tests_accepted"],
                                     "tests_passed": iso["tests_passed"]},
                        # Only the cases that did not pass: the full 1195-row
                        # table is noise in a report whose job is to name what
                        # to fix next.
                        "cases": [r for r in iso["results"] if r["status"] != "OK"],
                    }
                    report["results"].append(rec)
                    print(f"     cases {iso['cases_ok']}/{iso['cases_attemptable']} ok "
                          f"({iso['case_pct']}%)  "
                          f"[{iso['cases_failed']} failed, "
                          f"{iso['cases_crashed']} crashed, "
                          f"{iso['cases_timeout']} timed out]")
                    print(f"     assertions {iso['assertions_passed']}/"
                          f"{iso['assertions_of_completed']} "
                          f"({iso['assertion_pct_of_completed']}%) "
                          f"among non-empty cases that ran to completion")
                if args.reference:
                    engines = [e for e in engines if e[0] == "hashlink"]
                else:
                    engines = []

            for label, argv0 in engines:
                t0 = time.perf_counter()
                timed_out = False
                suite_env = os.environ.copy()
                suite_env = env_for_vm(
                    suite_env,
                    ash if label.startswith("ash:") else args.reference,
                )
                if name == "sys":
                    # Upstream's TestSys explicitly requires the runner to
                    # provide this fixture. It is inherited by subprocesses,
                    # which is part of what that suite verifies.
                    suite_env.setdefault("EXISTS", "1")
                try:
                    # Programs are invoked by upstream from their suite root.
                    # In particular, sys addresses gen_test_res.py,
                    # src/ExitCode.c, compile-each.hxml and test-res/ relative
                    # to tests/sys. Running from bin/hl manufactured failures
                    # in both Ash and the reference VM.
                    res = run(
                        argv0,
                        cwd=str(sdir),
                        timeout=args.timeout,
                        env=suite_env,
                    )
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

    # Compiler-bucket and NOT_APPLICABLE rows carry engine "haxe" or "-", so
    # the ash: filter keeps the VM tally pure by construction. EMPTY rows are
    # named and excluded from the denominator, following the isolation
    # precedent: a program with nothing to run on this target is not a score
    # ash can move.
    ash_rows = [r for r in report["results"]
                if r.get("engine", "").startswith("ash:")
                and r["status"] not in ("SKIP", "EMPTY")]
    ash_empty = [r for r in report["results"]
                 if r.get("engine", "").startswith("ash:")
                 and r["status"] == "EMPTY"]
    successful = {"PASS", "OK"}
    passes = sum(1 for r in ash_rows if r["status"] in successful)
    total = len(ash_rows)
    # Unit isolation has one utest process per case, so its aggregate tally
    # lives directly on the result row. Whole-suite sys/threads rows carry the
    # ordinary terminal utest block. Fold both shapes into one site-wide
    # assertion measure instead of silently reporting only one category.
    a_pass = sum(
        r.get("assertions_passed", 0)
        if r.get("isolated")
        else (r.get("utest") or {}).get("passed", 0)
        for r in ash_rows
    )
    a_total = sum(
        r.get("assertions_of_completed", 0)
        if r.get("isolated")
        else (r.get("utest") or {}).get("assertions", 0)
        for r in ash_rows
    )
    # How far ash got, and how far the reference VM gets on the same
    # bytecode. The reference is the only honest denominator available: the
    # suite does not publish its own test count, and a total taken from a run
    # that crashed would shrink as ash got worse.
    t_reached = sum((r.get("progress") or {}).get("tests_reached", 0) for r in ash_rows)
    tests_accepted = sum(
        (r.get("progress") or {}).get("tests_accepted", 0) for r in ash_rows
    )
    tests_passed = sum(
        (r.get("progress") or {}).get("tests_passed", 0) for r in ash_rows
    )
    ref_rows = [r for r in report["results"] if r["engine"] == "hashlink"]
    t_total = sum((r.get("progress") or {}).get("tests_reached", 0) for r in ref_rows)
    report["summary"] = {
        "suites_total": total,
        "suites_passed": passes,
        "tests_reached": t_reached,
        "tests_total": t_total or None,
        # Site-wide totals. Isolation is unit-only, but sys and threads still
        # publish per-test progress and must move the public score.
        "tests_accepted": tests_accepted,
        "tests_passed": tests_passed,
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

    # Per-case isolation, when it ran. This is the only conformance figure the
    # site can honestly show a percentage for: its denominator is the case
    # list, so a crash counts against it and cannot be hidden. The
    # whole-suite metrics above can only ever report zero while any case
    # crashes, which is why every published run so far has said 0%.
    iso = [r for r in ash_rows if r.get("isolated")]
    if iso:
        c_total = sum(r.get("cases_total", 0) for r in iso)
        c_ok = sum(r.get("cases_ok", 0) for r in iso)
        c_empty = sum(r.get("cases_empty", 0) for r in iso)
        c_attempt = sum(r.get("cases_attemptable", r.get("cases_total", 0)) for r in iso)
        report["summary"].update({
            "isolated": True,
            "cases_total": c_total,
            "cases_empty": c_empty,
            "cases_attemptable": c_attempt,
            "cases_ok": c_ok,
            "cases_failed": sum(r.get("cases_failed", 0) for r in iso),
            "cases_crashed": sum(r.get("cases_crashed", 0) for r in iso),
            "cases_timeout": sum(r.get("cases_timeout", 0) for r in iso),
            "case_pct": round(100.0 * c_ok / c_attempt, 1) if c_attempt else None,
            "case_pct_of_all": round(100.0 * c_ok / c_total, 1) if c_total else None,
            # These are unit-only diagnostics. Do not overwrite the global
            # all-suite totals above.
            "isolated_tests_accepted": sum(
                r.get("progress", {}).get("tests_accepted", 0) for r in iso
            ),
            "isolated_tests_passed": sum(
                r.get("progress", {}).get("tests_passed", 0) for r in iso
            ),
        })
    if report["summary"].get("isolated"):
        sm = report["summary"]
        print(f"\n{sm['isolated_tests_passed']}/{sm['isolated_tests_accepted']} "
              "unit tests passed under isolation")
        print(f"{sm['cases_ok']}/{sm['cases_attemptable']} unit cases passed "
              f"({sm['case_pct']}%)  [{sm['cases_failed']} failed, "
              f"{sm['cases_crashed']} crashed, "
              f"{sm['cases_empty']} empty on this target]")
    print(f"\n{tests_passed}/{tests_accepted} tests passed across all suites")
    print(f"{passes}/{total} suites passed")
    if ash_empty:
        names = ", ".join(f"{r['suite']}/{r.get('program', '')}" for r in ash_empty)
        print(f"{len(ash_empty)} program(s) EMPTY on this target, excluded "
              f"from the denominator: {names}")
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

    # The compiler bucket, summarised apart from everything above: these are
    # host-haxe verdicts on compiler behaviour. They share the report so the
    # trees are visibly covered; they are not ash conformance and no key of
    # the VM summary includes them.
    comp_rows = [r for r in report["results"]
                 if r.get("bucket") == "compiler"
                 and r["status"] not in ("SKIP", "NOT_APPLICABLE")]
    if comp_rows:
        cc_total = sum(r.get("cases_total", 0) for r in comp_rows)
        cc_ok = sum(r.get("cases_ok", 0) for r in comp_rows)
        cc_blocked = sum(r.get("cases_blocked", 0) for r in comp_rows)
        cc_failed = sum(r.get("cases_failed", 0) for r in comp_rows)
        # Blocked cases leave the denominator: the score is over what this
        # host could actually attempt, so the same tree reports the same
        # number on a machine that happens to have node and a JDK.
        attemptable = cc_total - cc_blocked
        report["summary"]["compiler_bucket"] = {
            "runs": len(comp_rows),
            "runs_ok": sum(1 for r in comp_rows if r["status"] == "OK"),
            "cases_total": cc_total,
            "cases_ok": cc_ok,
            "cases_failed": cc_failed,
            "cases_blocked": cc_blocked,
            "cases_attemptable": attemptable,
            "case_pct": (round(100.0 * cc_ok / attemptable, 1)
                         if attemptable else None),
        }
        blocked_note = ""
        if cc_blocked:
            tools = sorted({t for r in comp_rows
                            for t in r.get("blocked_cases", {}).values()})
            blocked_note = (f"; {cc_blocked} unrunnable on this host "
                            f"(missing: {', '.join(tools)})")
        print(f"\ncompiler bucket (host haxe, not ash — kept out of the VM "
              f"tally): {cc_ok}/{attemptable} cases ok across "
              f"{len(comp_rows)} invocation(s){blocked_note}")

    # Suites and subtrees that cannot apply to a VM, each with its reason.
    # Printed every run: absence would be indistinguishable from an oversight.
    na_rows = [r for r in report["results"] if r["status"] == "NOT_APPLICABLE"]
    if na_rows:
        report["summary"]["not_applicable"] = [
            {"suite": r["suite"], "program": r.get("program", ""),
             "reason": r["detail"]}
            for r in na_rows]
        print(f"\nnot applicable to a VM ({len(na_rows)}):")
        for r in na_rows:
            what = r["suite"] + (f" ({r['program']})" if r.get("program") else "")
            print(f"  {what}: {r['detail']}")

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
        # .get("engine"): rows recorded before the compiler bucket existed
        # (and SKIP rows to this day) may carry "mode" instead.
        was = {(r["suite"], r.get("program", ""), r.get("engine", "-")): r["status"]
               for r in base.get("results", [])}
        regressed = [
            r for r in report["results"]
            if was.get((r["suite"], r.get("program", ""), r.get("engine", "-"))) in successful
            and r["status"] not in successful
        ]
        if regressed:
            print("\nREGRESSED against the baseline:")
            for r in regressed:
                print(f"  {r['suite']}/{r['engine']}: PASS -> {r['status']}  {r['detail'][:100]}")
            return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
