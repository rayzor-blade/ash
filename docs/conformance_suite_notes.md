# Conformance suite notes: target applicability inventory

Scope: the suites newly integrated into `scripts/haxe_conformance.py` — `misc`,
`optimization`, `nullsafety`, `server`, `sourcemaps` — from the Haxe 4.3.6
checkout at `~/.cache/ash-haxe-conformance/haxe-tests-4.3.6/tests`.
Ground truth for "what runs on HashLink" is upstream CI itself
(`tests/runci/targets/Hl.hx`); ground truth for the others is
`runci/targets/Macro.hx` and `runci/targets/Js.hx`. Swept 2026-08-28.

## What upstream HL CI actually executes (`runci/targets/Hl.hx`)

| Step | Executed on the HL VM? |
|---|---|
| `unit` compile-hl + `hl bin/unit.hl` | yes (already in our suite) |
| `unit` compile-hlc + gcc + run | Linux-CI-only, HL/C native path — not bytecode |
| `threads` build + run | yes (already in our suite) |
| `sys` compile-hl + run | yes (already in our suite) |
| `misc/eventLoop` build-hl + `hl eventLoop.hl` | **yes — the only newly-integrated VM execution** |
| `misc/hl/reserved-keywords` compile | compile only (C-compiled+run on Linux CI only, HL/C path) |
| `misc/hl` `run.hxml` | compile-only checks; the one emitted `.hl` (Issue11196) is never executed |

`optimization`, `nullsafety`, `server`, `sourcemaps` do **not** appear in
`Hl.hx` at all — they run under `Macro.hx` (eval) and `Js.hx` (node).

## Ash-gap findings (upstream HL runs it, ash must too) — the part that is real work

| Item | Status |
|---|---|
| `misc/eventLoop` (`haxe.MainLoop`/`haxe.Timer` ordering, `MainLoop.addThread`) | **NO GAP.** Verified 2026-08-28: built with haxe 4.3.6, run under `target/release/ash` — interp mode exit 0, jit mode exit 0, stdout **byte-identical** to upstream's expectation file (`build-hl.hxml.stdout.disabled`, all 23 lines in order: BEFORE1/BEFORE2 before T0, A–E interleaved with 0–9, `false`, T1–T4). |
| Missing std natives surfaced by `misc/hl/projects/Issue11196` (`-dce no`) | **Advisory gap.** Running its `out.hl` under ash: exit 0 (main is `var a:hl.I64 = 5;`), but the loader reports 3 unresolved natives: `std@hlp_bytes_subtract`, `std@hlp_bytes_address`, `std@hlp_bytes_from_address`. Upstream CI never executes this file, so it is not a conformance failure today — but real HashLink resolves these, and any `-dce no` program that *calls* `haxe.io.Bytes` address ops will fail on ash. Worth implementing. |

No other ash-gaps exist in the newly integrated suites: everything else in them
is either compiler-side (host `haxe` does the work; ash is not involved by
construction) or belongs to another target entirely.

## Preprocessor-gate sweep (the "empty main on HL" pattern)

Swept every `.hx` under `misc/projects`, `misc/resolution`,
`misc/compiler_loops`, `misc/eventLoop`, `nullsafety/src`, `optimization/src`,
`sourcemaps/src`, `server/src` for target `#if` gates:

- `misc/eventLoop/Main.hx` — three `#if sys` blocks. HL **is** a sys target:
  all three are ACTIVE. Nothing is gated away.
- ~25 hits are `#if macro` (macro-time infrastructure), not target gates.
- `misc/projects/Issue7108/Main.hx` has `#if js` — but it is a
  `compile-fail` fixture testing the parser's rejection of a `#elsif` typo;
  the gate is part of the expected error, not target selection.
- **Zero cases in these suites have a main body gated away from HL.** In this
  corpus target-specificity is expressed via hxml target flags and whole
  per-target directories, never via `#if <target>` around `main`. The
  EMPTY classification in the pipeline is therefore a tripwire for future
  cases, not a bucket with current members.

## Suite: misc (1063 files) — mixed

### VM-applicable (ash tally)

| Case | Gate/mechanism | On HL |
|---|---|---|
| `misc/eventLoop/build-hl.hxml` → `eventLoop.hl` | `#if sys` only (active on HL) | Runs; PASSES under ash interp+jit (verified, output above). `build-cpp.hxml`/`all.hxml` in the same dir are the cpp flavor — ignored, correctly. |

### Compiler bucket (host haxe, upstream-faithful; ash cannot move these numbers)

| Sub-tree | Cases | Mechanism | Why not VM rows |
|---|---|---|---|
| `misc/projects` via `compile.hxml` runner | 592 hxml (384 `*-fail.hxml`) | expected-compile-error / `.stderr` contracts under eval runner (`misc/src/Main.hx`) | Compiler diagnostics; no target ⇒ nothing to execute. 531 declare no target at all. |
| `misc/resolution` via `run.hxml` | runner tree | import-resolution diagnostics | same |
| `misc/hl` via `run.hxml` | 6 hxml (4 projects) | HL-target compiler checks | Compile-only even upstream — see table below |
| `misc/hl/reserved-keywords` | 1 hxml | hl/c C-identifier emission | Emits C source; executed only via gcc on Linux CI (HL/C, out of a bytecode VM's scope) |
| `misc/compiler_loops` via `run.hxml` | runner tree | compile-time loop/recursion limits | Linux-only upstream (runner shells through `timeout`); SKIP row on darwin |

`misc/hl` detail (all compile-only; none executed by upstream CI):

| Case | What it checks | Artifact |
|---|---|---|
| `projects/Issue10184/hlc-json.hxml` | hlc.json defines | `--hl out/main.c -D no-compilation` |
| `projects/Issue10376/{custom,default,no-header-dash}-header.hxml` (3) | HLC source-header defines | same |
| `projects/Issue11196/compile.hxml` | `hl.I64` typing, `-dce no` | `out.hl` — never run by CI (see ash-gap advisory) |
| `projects/Issue11689/compile.hxml` | hl/c emission | `-hl out/main.c` |

### misc/projects cases declaring another target (61 hxml)

Gate = explicit target flag in the hxml; the runner compiles them with that
target under host `haxe`, exactly as upstream Macro CI does. They are never
compiled to HL by the pipeline; forcing `-hl` would change each case's
contract (target-specific externs/diagnostics), so they stay in the compiler
bucket. 8 of them also *execute* output via `--cmd` (6 node, 2 neko) — node
and neko are present on this machine's pipeline PATH, so they run.

| Target | n | Cases (`misc/projects/…`) |
|---|---|---|
| js | 42 | Issue10863, Issue10871/Compiler(compile2), Issue3542, Issue3621, Issue4404(compile1), Issue4540, Issue4803(x2), Issue5525, Issue5644, Issue5833, Issue5843, Issue5856, Issue5888, Issue6030, Issue6435, Issue6790(x2), Issue6992, Issue7453, Issue7655(x2), Issue7936, Issue8176*, Issue8231*, Issue8828(x3), Issue8892, Issue8972, Issue9064, Issue9296*, Issue9308*, Issue9312, Issue9501*, Issue9968*, es6(x4), inline-constructors(x2)  (*=`--cmd node`) |
| neko | 11 | Issue2538, Issue3102(x2, one `--cmd neko`), Issue3181, Issue3500, Issue4364, Issue4679(`--cmd neko`), Issue4742, Issue5126, Issue5559, Issue6065 |
| swf | 4 | Issue2232(x2), Issue7796, Issue8241 |
| java | 2 | Issue11737(x2) |
| cpp | 1 | Issue10871/Compiler(compile3) |
| php | 1 | Issue8219 |
| hl | **0** | (none — nothing in projects/ targets hl) |

### Per-target trees — excluded wholesale (one NOT_APPLICABLE row in the report)

Each is a self-contained runner tree (`run.hxml`: `-cp ../src --run Main`)
whose inner projects declare that target; nothing compiles to hl.

| Tree | hxml cases | Tree | hxml cases |
|---|---|---|---|
| cs | 28 | lua | 7 |
| java | 20 | neko | 3 |
| python | 9 | cpp | 3 |
| js | 7 | flash | 2 |
| php | 7 | weakmap | 2 |
| — | — | cppObjc / es6 / luaDeadCode | 1 each |

Total: 13 trees, 91 hxml cases.

## Suite: optimization (62 files) — NOT_APPLICABLE to a VM

Upstream: `Js.hx` only (`haxe run.hxml` in optDir). Four `--each` blocks:

| Block | Mechanism | Why not HL |
|---|---|---|
| `--main TestAnalyzer --interp` | analyzer output asserted at macro/eval time | tests the *compiler's* analyzer; no target artifact |
| `--main TestNullChecker -D analyzer-check-null --interp` | same | same |
| `--main TestTreBehavior --interp` | TRE behavior under eval | same |
| `-js testopt.js` + `Macro.register(...)` over Test/TestJs/TestLocalDce/TestNadako/TestTreGeneration/TestInlineConstructors/issues (43 files) | generated-JS pattern matching: expectations are literal **JS source strings** in `@:js` metadata (~130 in TestJs.hx alone) | the expected artifacts are JavaScript text; unportable to HL by construction |

## Suite: nullsafety — NOT_APPLICABLE to a VM

Upstream: `Macro.hx`. Two invocations:

| hxml | Mechanism | Why not HL |
|---|---|---|
| `test.hxml` | **no target at all** — 5 case classes (TestStrict, TestStrictThreaded, TestLoose, TestSafeFieldInUnsafeClass, TestAbstract) checked by `--macro nullSafety(...)` + `Validator.register()`; expected errors matched at compile time | emits no artifact; the diagnostic pass is the test subject |
| `test-js-es6.hxml` | same + `-js bin/test-es6.js -D js-es=6` | js codegen under null safety; output never executed |

## Suite: server — NOT_APPLICABLE to a VM

Upstream: `Js.hx` (`haxe build.hxml` then `node test.js`). `build.hxml`:
`-js test.js -lib hxnodejs -lib utest -lib haxeserver`. The tests drive a
`haxe --wait` **compilation server** over the IDE protocol (display requests,
retyper, diagnostics — `src/cases/`: ServerTests, RetyperTests,
ReplaceRanges, display/, issues/). The process under test is the compiler;
the harness is hard-wired to node (`hxnodejs`). No HL involvement is possible
or meaningful.

## Suite: sourcemaps — NOT_APPLICABLE to a VM

Upstream: `Macro.hx` (`haxe run.hxml`). `run.hxml`: `-main Test --interp
-D eval-stack`. Generates code, then validates the emitted **source map**
(src/validation/: Lines, Target, ValidationReport). The artifact under test
is the map file; eval is the runner. HL emits no source maps; nothing to
conform to.

## Method notes

- Counts and gates come from direct sweeps of the checkout (grep for target
  `#if`, target flags in hxml, `--cmd` lines) plus reading `runci/targets/`.
- eventLoop and Issue11196 verdicts come from actually building with
  haxe 4.3.6 and executing under a snapshot of `target/release/ash`
  (2026-08-28, binary snapshotted because another session relinks it).
