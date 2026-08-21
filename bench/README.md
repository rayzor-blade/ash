# ASH benchmark suite

`scripts/ash_bench.py` is the **canonical** performance tool for this repo. It
sweeps the `.hl` corpus in [`benchmarks.toml`](benchmarks.toml) across ASH's
execution modes, verifies each run produced the *right answer*, and emits both a
readable table and a JSON baseline that a later run can be gated against.

`scripts/run_perf_matrix.py` is now a thin compatibility shim that forwards to
this tool. Do not add features there.

---

## Quick start

```sh
# build first — ash_std is embedded into `ash` at build time
cargo build -p ash_std && cargo build -p ash_cli   # produces the unified `ash` binary

# full default sweep, table on stdout + JSON on disk
./scripts/ash_bench.py --out-json bench/baselines/local.json

# compare a later run against that baseline; exits non-zero on regression
./scripts/ash_bench.py --baseline bench/baselines/local.json

# one benchmark, all modes, with GC counters
./scripts/ash_bench.py --benchmarks tiered_hotloop --gc-stats

# what would run
./scripts/ash_bench.py --list
```

`--build` runs the three `cargo build` invocations for you, in the order
`ash_std` → `ash_cli` → `ash`. That order is not optional: `ash_std` is a cdylib
embedded into `ash` via `include_bytes!`, so building `ash` against a stale
`ash_std` measures last week's standard library.

---

## Execution modes

| Mode | Binary | Flags | What it is |
|---|---|---|---|
| `interp` | `ash_cli` | `--mode interp` | Pure bytecode interpreter. The parity-validated reference, and the speedup denominator. |
| `hybrid-auto` | `ash_cli` | `--mode hybrid --jit-tier auto` | Full ladder: interpreter → Cranelift → LLVM. |
| `hybrid-cranelift` | `ash_cli` | `--mode hybrid --jit-tier cranelift` | Ladder pinned to the Cranelift middle tier. |
| `hybrid-llvm` | `ash_cli` | `--mode hybrid --jit-tier llvm` | Ladder pinned to the LLVM top tier. |
| `full-jit` | `ash` | *(none)* | Standalone whole-module LLVM JIT, no interpreter. |
| `hybrid-off` | `ash_cli` | `--mode hybrid --jit-tier off` | Control: tiering machinery present, promotion disabled. Not in the default set. |
| `hybrid-eager` | `ash_cli` | `--jit-threshold 1 …` | The promotion policy the old `run_perf_matrix.py` used, kept so its historical numbers stay comparable. Not in the default set. |

Select with `--modes a,b,c`. Non-default modes only run when named.

---

## Metrics

**`MEDIAN` / `MIN` / `MAX` / `SD%` — wall time, milliseconds.**
Whole-process wall clock, measured by the runner around `subprocess.run`, over
`--iterations` runs (default 5) after `--warmups` warmup runs (default 1).
Median is the headline. Min and max are printed next to it deliberately: a
median on its own cannot distinguish a noisy machine from a real change, and
`SD%` (standard deviation as a percentage of the median) is the first thing to
check before believing a delta. Anything over ~5% means the machine was busy.

This is **whole-program** time, including VM startup, bytecode load, and — for
the JIT modes — compilation. That is the number a user experiences, and it is
the number the tiering work exists to reduce. The `basic` benchmark exists to
give you the startup floor: subtract it mentally from every other row.

**`RSS` — peak resident set size.**
From `/usr/bin/time -l` on macOS (`-v` on Linux), parsed out of the
`maximum resident set size` line. Taken from a **separate instrumented run**,
not from the timed runs. Two reasons: `/usr/bin/time` adds a process to the
measurement, and — the real one — BSD `time(1)` reports its child's status
through `WEXITSTATUS`, which is meaningless for a signal-terminated child, so
wrapping the timed runs would erase exactly the SIGSEGV we need to record for
the full-JIT `game.hl` case.

**`SPEEDUP`.**
Median of this row divided into the median of the first comparable mode for
this benchmark — normally `interp`. Rows that are not `OK` get no speedup: a
configuration that produced the wrong answer, crashed, or timed out has no
meaningful runtime.

**`TIER c/l` — Cranelift / LLVM promotions.**
Parsed, not re-derived, from the line `ash_cli` prints under `--jit-log`:

```
[tiered] attempted=2 succeeded=1 failed=0 compiled_calls=19884 fallbacks=0 cranelift=1 llvm=0
```

The full record (`attempted`, `succeeded`, `failed`, `compiled_calls`,
`fallbacks`, `cranelift`, `llvm`) is in the JSON. The table shows
`cranelift/llvm`.

A trailing `(+c/l)` means **the tier engaged, but too late to be counted.**
That counter is sampled when the entrypoint returns, while LLVM promotion runs
on a background thread — on a short benchmark the install routinely lands after
the sample, so the counter reads `llvm=0` while a `[tier] install … tier=llvm`
line sits further down stderr. The runner collects those install lines
separately, because "the top tier never engaged" and "it engaged after the
program finished" are different bugs and the counter alone cannot tell them
apart. `hybrid-auto` on `tiered_hotloop` shows `1/0(+0/1)` for exactly this
reason.

**`GC` — collections.**
Only populated with `--gc-stats` (sets `ASH_GC_STATS=1`). The JSON also carries
`blocks_reclaimed`, `live_blocks`, `bytes_allocated_mb`, `pause_total_ms`,
`pause_max_ms` and `pause_total_ns`, parsed from ASH's own end-of-run report.

**`NOTE`.**
For checksum benchmarks, which accepted checksum came back (see below). For
everything else, the correctness verdict or the failure reason.

---

## Correctness gate

**A fast-but-wrong configuration must never look like a win.** A wrong answer is
recorded as `INVALID`, excluded from all speedup and regression arithmetic, and
fails the gate.

**Every timed iteration is checked, not just the last one.** ASH promotes on a
background thread, so a JIT mode can legitimately return a different answer on
different runs of the same command — which is exactly the bug worth catching,
and judging only the final run lets it through whenever that run happened to be
the correct one.

The runner also fingerprints each run's output and reports whether the
iterations agreed *with each other*, as `output_stable` in the JSON. That is a
separate question from whether they agreed with the reference, and the
combination is diagnostic:

| Verdict | Reading |
|---|---|
| all correct, stable | `OK` |
| all wrong, stable | a deterministic miscompile |
| all wrong, unstable | *"wrong on all N runs, and differently each time"* — uninitialized memory, not a miscompile |
| some wrong | *"N/M runs wrong (nondeterministic)"* — a race, most likely against background promotion |
| all correct, unstable | `OK` plus `WARNING: output varied across runs` |

That distinction is what identified the full-JIT `Map` iteration defect below
as a pointer being read as an `i32` rather than a bad lowering.

Expectations reuse the vocabulary of
`crates/ash/test/tests/parity_cases.toml` — and where a benchmark names a
`parity_case`, they are literally read from it, so the benchmark corpus and the
correctness corpus cannot drift apart:

- **`exact`** — normalized stdout must equal a reference. Normalization matches
  `normalize_text` in `crates/ash_cli/tests/common/mod.rs` (`default` /
  `json_ws` / `none`), so a program the parity suite calls correct is not called
  incorrect here over trailing whitespace.
- **`checksum`** — the `Checksum: N` line must be in an accepted set declared in
  the manifest.
- **`exit_only`** — exit code 0, nothing asserted about stdout.

### Where the reference comes from

In order: a HashLink oracle bundle (`--oracle <dir>`, produced by
`scripts/generate_parity_oracle.py`), otherwise an untimed `interp` run. The
JSON records which, as `reference_source`.

The fallback is not a compromise for its own sake: **HashLink does not run on
Apple M-series**, so on this machine the oracle is a CI artifact or nothing, and
`interp` is the mode the parity suite validates against HashLink in CI. Only
`exact` benchmarks need a reference run at all — which matters, because that run
is untimed but not free and `mandelbrot` takes minutes under the interpreter.

### The Mandelbrot checksums are FP-sensitive, on purpose

Both Mandelbrot benchmarks accept more than one answer, and the runner **records
which one came back**, because that is a measurement in its own right — it tells
you whether the hot function ran compiled and under what fusion policy:

| Benchmark | Checksum | Label | Meaning |
|---|---|---|---|
| `mandelbrot_small` (298²) | `22816350` | `unfused` | No FMA contraction. Matches `clang -ffp-contract=off` and ASH's interpreter bit-for-bit. |
| `mandelbrot_small` | `22825041` | `fused` | FMA contraction. Matches `clang -ffp-contract=on` and ASH's LLVM tier. |
| `mandelbrot` (875×500) | `112790102` | `unfused` | |
| `mandelbrot` | `112798515` | `fused` | Also the hxcpp / hxjava value. |
| `mandelbrot` | `112798587` | `fused-llvm` | ASH's own LLVM tier. Same fusion *presence* as hxcpp, different fusion *pattern* — 72 units apart. |

Fusion is what every reference implementation does; the unfused number appears
nowhere but a strict interpreter. A checksum **outside** this set is `INVALID`,
and a run whose *label* changed against a baseline is called out in the
comparison even when the time is flat — the FP policy moving is a behaviour
change. Background: `BACKLOG.md`, "Cranelift middle tier", cross-tier FMA
policy.

---

## Statuses

| Status | Meaning |
|---|---|
| `OK` | Ran, exited 0, produced the expected answer. The only status that enters comparisons. |
| `INVALID` | Ran and exited 0, but the answer was wrong. Always fails the gate. |
| `FAIL` | Non-zero exit or fatal signal. The signal name is recorded. |
| `TIMEOUT` | Exceeded the per-run time box. |
| `SKIP` | Not applicable — missing `.hl`, or a mode outside the benchmark's allowlist. |

A failing cell **never aborts the sweep**. The remaining benchmarks and modes
still run, and the failure is recorded — that is the whole point of the
`heaps_game` entry below.

---

## Load sensitivity — read this before trusting a number

**Runs are sequential by default and there is no `--jobs` flag.** Concurrent
`cargo` builds or parallel JIT runs skew these timings badly, and not by a
constant factor you could correct for. Observed on this repo: a **6.7 s** JIT
pre-warm inflated to **18.5 s** — 2.8× — purely from a concurrent build on the
same machine. The LLVM tier compiles on a background thread, so a loaded machine
does not just add noise, it changes *which tier a short benchmark actually
finishes in*. That turns the result bimodal rather than merely noisy, which is
far harder to spot in a table.

The runner reads the 1-minute load average before starting and warns when it
exceeds `--load-threshold` × cores (default 0.4). The warning is advice, not a
block; `--ignore-load` silences it. Either way, the load at start and end is
recorded in the JSON, so a suspicious baseline can be audited after the fact.

Practical advice: quit other builds, wait for the load average to fall below ~1
on a 10-core machine, and re-run anything with an `SD%` above ~5.

Baselines also record `system.cpu_model`. Comparing across different CPUs is
meaningless, and the runner says so when the models differ.

---

## Baselines and regression gating

```sh
./scripts/ash_bench.py --out-json bench/baselines/m1pro-2026-08-17.json      # record
./scripts/ash_bench.py --baseline bench/baselines/m1pro-2026-08-17.json      # gate
```

The comparison is per `(benchmark, mode)` pair on the **median**. A pair is a
regression when:

- the new median exceeds the baseline median by more than
  `--regress-threshold` (default `0.10`, i.e. 10%) **and** by more than
  `--regress-min-ms` (default `5.0`), **or**
- the status got worse — `OK` → `INVALID` / `FAIL` / `TIMEOUT`. A mode that used
  to answer correctly and now crashes has not got faster, whatever its wall
  time says.

**Both thresholds, not either.** Most of this corpus finishes in 15–25 ms,
nearly all of it VM startup, so a relative-only gate is unusable: the first
comparison run on an idle machine flagged `basic/hybrid-cranelift` at +15.5%
(17.9 → 20.6 ms) and `jsonarr/interp` at +39.1% (16.5 → 23.0 ms), neither of
which is a change anyone could act on. A pair that clears the relative bar but
not the absolute one prints `noise (+N ms < 5ms floor)` rather than a bare
`ok`, so a real regression on a fast benchmark stays visible while it grows.
Raise `--regress-min-ms` on a noisy machine; lower it when gating something
genuinely small.

`INVALID` in the current run always fails the gate, baseline or no baseline.
`--min-speedup X` additionally requires every benchmark's best JIT mode to beat
`interp` by at least `X` (this is the gate the old `run_perf_matrix.py`
carried). `--no-gate` reports everything and still exits 0.

Baseline files are versioned by `schema_version`; a mismatch is a hard error
rather than a silent misread.

---

## Environment knobs

Surfaced as flags so a run's configuration is recorded in its JSON rather than
living in the invoking shell. The runner **strips** `ASH_GC_STATS`,
`ASH_TIER_LOG` and `ASH_TIER` from the inherited environment, so an exported
variable cannot silently change what is measured.

| Flag | Env | Effect |
|---|---|---|
| `--gc-stats` | `ASH_GC_STATS=1` | GC counters in the instrumented run. |
| `--tier-log` | `ASH_TIER_LOG=1` | Per-function tier install lines. |
| `--gc-heap-mb N` | `ASH_GC_HEAP_MB=N` | Heap reservation (default 512 MB). Applies to *all* runs, timed included, since it changes allocation behaviour. |

`--jit-log` is passed automatically to the instrumented run and never to a timed
run: it writes to stderr on promotion, and instrumentation cost has no business
inside a number being compared across baselines.

---

## The corpus

Defined in [`benchmarks.toml`](benchmarks.toml), in the order it runs. Anything
with a `parity_case` inherits its `.hl`, expectation, normalization, timeout and
`slow` flag from `parity_cases.toml`.

- **`basic`** — startup floor. Everything else should be read net of this.
- **`tiered_hotloop`** — the ladder's designated exercise; the case the
  promotion counters are meant to be read on.
- **`mandelbrot_small`**, **`mandelbrot`** — FP kernels. `mandelbrot` is `slow`
  and needs `--include-slow`.
- **`array_push`**, **`gc`** — allocation churn.
- **`stdlib`** — broad stdlib exercise.
- **`jsonparse`**, **`jsonmin`**, **`jsonmin2`**, **`jsonmin3`**, **`jsonarr`**.
- **`mapiter`**, **`map_simple`**.

### Deliberate exclusions

**`TestMapDebug` / `TestMapDebug2` / `TestMapDebug3`** are debugging scaffolds
from the map bring-up, not workloads. Each does a handful of operations, so a
run is entirely process startup and the rows would only add noise. They stay in
the parity corpus, where they earn their place.

**`mandelbrot` (875×500) is `slow` and opt-in.** Measured: it does not complete
within 400 s under `interp` *or* `hybrid-auto`. At the default 5 iterations that
is over half an hour for a single cell. Run it deliberately and with a reduced
iteration count:

```sh
./scripts/ash_bench.py --benchmarks mandelbrot --include-slow \
    --iterations 1 --warmups 0 --modes hybrid-llvm,full-jit
```

**`heaps_game` (`examples/heaps_base2d/bin/game.hl`) is not a benchmark and is
excluded by default.** It opens an SDL window and runs an event loop that never
returns, so under every interpreter-backed mode the wall time measures the time
box rather than the program — there is no headless driver to make it terminate,
and a frame-rate-bound, vsync-bound, window-manager-dependent number would not
be comparable run to run even if there were. It is carried under
`--include-windowed`, restricted to `full-jit`, for exactly one reason: to keep
a dated, machine-readable record that `target/debug/ash` still **SIGSEGVs** on
it. Measured here: the fault arrives after ~156 s of whole-module LLVM
compilation, which is why its time box is 200 s — a shorter one records
`TIMEOUT` and hides the crash.

---

## JSON schema (v1)

Top level: `schema_version`, `generated_unix`, `generated_iso`,
`sweep_seconds`, `git` (`commit` / `branch` / `dirty`), `system` (`cpu_model`,
`cpu_count`, `ram_mb`, `arch`, `hostname`, `python`), `load` (`at_start`,
`at_end`, `threshold`, `warning_raised`), `config` (every flag that shaped the
run), `binaries` (resolved paths), and `results`.

Each entry in `results`:

```json
{
  "benchmark": "tiered_hotloop",
  "mode": "hybrid-cranelift",
  "group": "jit",
  "binary": "ash_cli",
  "hl": "test_tiered_hotloop.hl",
  "command": "…/ash_cli --mode hybrid --jit-tier cranelift --quiet …",
  "expectation": "exact",
  "status": "OK",
  "detail": "stdout matches interp",
  "reference_source": "interp",
  "wall_ms": { "min_ms": …, "median_ms": …, "mean_ms": …, "max_ms": …,
               "stddev_ms": …, "runs": 5 },
  "samples_ms": [ … ],
  "peak_rss_bytes": 34603008,
  "exit_code": 0,
  "signal": null,
  "checksum": { "value": "22816350", "label": "unfused", "accepted": true },
  "output_stable": true,
  "tiered": { "attempted": 1, "succeeded": 1, "failed": 0,
              "compiled_calls": 19886, "fallbacks": 0,
              "cranelift": 1, "llvm": 0 },
  "tier_installs": [ { "findex": 255, "tier": "cranelift", "compile_ms": 5.84 } ],
  "gc": { "collections": 0, "blocks_reclaimed": 0, "live_blocks": 0, … }
}
```

Raw `samples_ms` are kept so a later analysis can compute a statistic this
runner did not think to report.

---

## What the first baseline showed

`baselines/m1pro-2026-08-17.json` — Apple M1 Pro, 10 cores, 16 GB, `debug`
builds, 65 runs in ~505 s. A snapshot, not a target; re-measure rather than
trusting the numbers below, and note these are **debug** binaries. Four things
in it are worth knowing before you read your own run:

**The JIT tiers only pay off when there is a hot loop to pay them back.**
`tiered_hotloop` goes 4586 ms → 67 ms under `hybrid-cranelift` (**68.6×**), and
`mandelbrot_small` 13.9 s → 1053 ms under `full-jit` (**13.2×**). Every
short benchmark goes the other way: at ~16 ms of interpreter time, all the
hybrid modes land at 0.85–0.95× because promotion never repays its setup.

**`full-jit` carries a ~900 ms fixed cost.** It compiles the whole module before
running anything, so it is 0.02× on every benchmark that finishes in tens of
milliseconds and only wins where the compiled code runs long enough to amortize
that. The `basic` row (881 ms for a program the interpreter finishes in 17 ms)
is that cost with nothing else in it.

**The LLVM pre-warm is visible and it is not small.** `stdlib` runs 46 ms under
`interp`, 27.8 ms under `hybrid-cranelift` (1.66×) — and **459 ms** under
`hybrid-auto` and `hybrid-llvm`. Same program, same interpreter, 10× slower,
entirely from the top tier's one-time module pre-warm. Cranelift's middle tier
is what makes short-program tier-up viable at all.

**`mandelbrot_small` returns the *unfused* checksum in every hybrid mode,
including `hybrid-llvm` with `llvm=6`.** Six functions were LLVM-promoted and
the answer still matched the strict interpreter bit-for-bit, so the hot inner
loop is either not among the promoted six or is not being contracted. Only
`full-jit` returns `fused`. That single column is the whole reason the
checksum's *label* is recorded rather than just checked — see BACKLOG.md,
cross-tier FMA policy.

And the gate did its job on its first full run: `mapiter/full-jit` and
`map_simple/full-jit` came back **INVALID**. Both complete in ~1 s — faster than
several `OK` rows — while printing a different wrong number every time. Filed in
BACKLOG.md under JIT & tiering.

---

## Relationship to the other tools

| Tool | Role |
|---|---|
| `scripts/ash_bench.py` | **Canonical.** Performance + peak RSS + JIT/GC counters + correctness gate + baseline regression gating. |
| `scripts/run_perf_matrix.py` | Deprecated shim. Translates its old flags and forwards here so `.github/workflows/perf_smoke.yml` keeps working. |
| `scripts/generate_parity_oracle.py` | Produces the HashLink oracle bundle consumed by `--oracle`. Runs in CI, since HashLink does not run on Apple silicon. |
| `crates/ash/test/tests/run_stdlib_matrix.sh` | Correctness only, interpreter only. Not a performance tool. |
| `crates/ash_cli/tests/parity_matrix.rs` | The parity gate proper. `ash_bench.py` borrows its case metadata and its normalization rules; it does not replace it. |
