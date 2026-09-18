# Benchmark suite

`scripts/ash_bench.py` sweeps the corpus in [`benchmarks.toml`](benchmarks.toml)
across ash's execution modes, checks that every run produced the right
answer, and writes a table plus a JSON baseline a later run can be gated
against. `scripts/run_perf_matrix.py` is a compatibility shim that forwards
here; do not add features to it.

```sh
cargo build --release -p ash_std && cargo build --release -p ash

./scripts/ash_bench.py --out-json bench/baselines/local.json     # sweep, record
./scripts/ash_bench.py --baseline bench/baselines/local.json     # gate; non-zero on regression
./scripts/ash_bench.py --benchmarks tiered_hotloop --gc-stats    # one benchmark, GC counters
./scripts/ash_bench.py --list
```

`--build` runs the cargo builds in the right order. The order matters:
`ash_std` is embedded into `ash`, so building `ash` against a stale
`ash_std` measures last week's runtime.

## Modes

| Mode | Flags | |
|---|---|---|
| `interp` | `--mode interp` | Interpreter only. The parity-validated reference and the speedup denominator. |
| `hybrid-auto` | `--mode hybrid --jit-tier auto` | The full ladder. |
| `hybrid-cranelift` | `--mode hybrid --jit-tier cranelift` | Ladder pinned to Cranelift. |
| `hybrid-llvm` | `--mode hybrid --jit-tier llvm` | Ladder pinned to LLVM. |
| `full-jit` | `--mode jit` | No interpreter: every function compiled at its first call, LLVM re-tiering the hot ones. |
| `hybrid-off` | `--mode hybrid --jit-tier off` | Control: tiering present, promotion disabled. Not in the default set. |
| `hybrid-eager` | `--jit-threshold 1 …` | Historical policy, kept so old numbers stay comparable. Not in the default set. |

`--modes a,b,c` selects; non-default modes run only when named. The manifest
also defines policy variants (`hybrid-auto-P-osr`, `interp-O1`, …) for
specific investigations; `--list` prints them.

## Metrics

**`MEDIAN` / `MIN` / `MAX` / `SD%`** — whole-process wall time in
milliseconds over `--iterations` runs (default 5) after `--warmups` (default
1). Read `SD%` before believing a delta; above ~5% the machine was busy.
This includes startup, bytecode load and, for JIT modes, compilation — the
number a user experiences. The `basic` row is the startup floor.

**`RSS`** — peak resident set size from a separate instrumented run under
`/usr/bin/time` (`-l` on macOS, `-v` on Linux). Separate because
`/usr/bin/time` adds a process, and because BSD `time(1)` reports a
signal-terminated child's status through `WEXITSTATUS`, which would erase a
SIGSEGV the runner needs to record.

**`SPEEDUP`** — this row's median against the first comparable mode's,
normally `interp`. Rows that are not `OK` get none.

**`TIER c/l`** — Cranelift / LLVM promotions, parsed from the `[tiered]`
summary line under `--jit-log`. A trailing `(+c/l)` means a tier engaged
after the counter was sampled: the counter is read when the entrypoint
returns, and LLVM installs on a background thread, so on a short benchmark
the install often lands after the sample. The runner collects the install
lines separately because "never engaged" and "engaged too late" are
different bugs.

**`GC`** — collections, with `--gc-stats`. The JSON also carries reclaimed
blocks, live blocks, allocated MB and pause times from ash's own report.

**`NOTE`** — which accepted checksum came back, or the failure reason.

## Correctness gate

A fast wrong answer is `INVALID`, excluded from all speedup and regression
arithmetic, and fails the gate. Every timed iteration is checked, not only
the last: promotion runs on a background thread, so a JIT mode can answer
differently on different runs of the same command, which is exactly the bug
to catch.

The runner also fingerprints each run's output and reports `output_stable`:

| Verdict | Reading |
|---|---|
| all correct, stable | `OK` |
| all wrong, stable | a deterministic miscompile |
| all wrong, unstable | uninitialized memory, not a miscompile |
| some wrong | a race, most likely against background promotion |
| all correct, unstable | `OK` with `WARNING: output varied across runs` |

Expectations use `parity_cases.toml`'s vocabulary, and a benchmark that
names a `parity_case` reads them from it:

- `exact` — normalized stdout equals a reference (`default` / `json_ws` /
  `none`, as in `crates/ash_cli/tests/common/mod.rs`).
- `checksum` — the `Checksum: N` line is in an accepted set declared in the
  manifest.
- `exit_only` — exit code 0.

The reference is a HashLink oracle bundle (`--oracle <dir>`, from
`scripts/generate_parity_oracle.py`), else an untimed `interp` run;
`reference_source` in the JSON says which. Stock HashLink does not run on
Apple Silicon, so locally the oracle is a CI artifact or nothing.

### Mandelbrot's checksums

Both Mandelbrot benchmarks accept more than one answer and the runner records
which one came back, because it says whether the hot function ran compiled
and under what fusion policy:

| Benchmark | Checksum | Label | |
|---|---|---|---|
| `mandelbrot_small` (298²) | `22816350` | `unfused` | no FMA contraction; matches `clang -ffp-contract=off` and the interpreter bit for bit |
| `mandelbrot_small` | `22825041` | `fused` | matches `clang -ffp-contract=on` |
| `mandelbrot` (875×500) | `112790102` | `unfused` | |
| `mandelbrot` | `112798515` | `fused` | also the hxcpp / hxjava value |
| `mandelbrot` | `112798587` | `fused-llvm` | ash's LLVM tier: same fusion presence, different pattern |

A checksum outside the set is `INVALID`, and a label change against a
baseline is reported even when the time is flat.

## Statuses

| | |
|---|---|
| `OK` | ran, exited 0, expected answer; the only status that enters comparisons |
| `INVALID` | exited 0 with the wrong answer; always fails the gate |
| `FAIL` | non-zero exit or fatal signal, named |
| `TIMEOUT` | exceeded the per-run time box |
| `SKIP` | missing `.hl`, or a mode outside the benchmark's allowlist |

A failing cell never aborts the sweep.

## Load sensitivity

Runs are sequential and there is no `--jobs`. A concurrent build on the same
machine inflated a 6.7 s JIT pre-warm to 18.5 s, and because LLVM compiles on
a background thread a loaded machine changes which tier a short benchmark
finishes in — bimodal, not merely noisy. The runner reads the 1-minute load
average before starting and warns above `--load-threshold` × cores (default
0.4); `--ignore-load` silences it; both loads are recorded in the JSON.
Baselines record `system.cpu_model`, and the runner refuses to read a
comparison across different models as meaningful.

## Baselines and gating

```sh
./scripts/ash_bench.py --out-json bench/baselines/m1pro-2026-08-17.json   # record
./scripts/ash_bench.py --baseline bench/baselines/m1pro-2026-08-17.json   # gate
```

Per `(benchmark, mode)` on the median. A regression is a median worse by
more than `--regress-threshold` (default 10%) **and** more than
`--regress-min-ms` (default 5.0), or a status that got worse. Both bars,
because most of the corpus finishes in 15–25 ms of mostly startup and a
relative-only gate flags noise; a pair over the relative bar but under the
absolute one prints `noise (+N ms < 5ms floor)`. `INVALID` always fails.
`--min-speedup X` additionally requires each benchmark's best JIT mode to
beat `interp` by `X`; `--no-gate` reports and exits 0. Baseline files carry
`schema_version`; a mismatch is an error.

A bimodal sample — a tiered run whose compile lands before the loop ends on
some runs and not others — is split at its widest gap and both modes are
reported (`bimodal 3x886/4x1187`; `wall_ms.modes` in the JSON); the gate
compares fast mode against fast mode.

## A/B against a second binary

```sh
./scripts/ash_bench.py --ash-base /path/to/other/ash --base-commit abc1234
```

Every timed run of head is followed by one of base, so both see the same
machine in the same minute. The table gets a `vs base ±N%` note, an `A/B
against base` section follows, and each row's JSON carries the base numbers
under `base`. This is what the CI sweep does on every leg against the commit
the previous sweep measured, and it is the only cross-commit number on the
published page that means anything: the milliseconds come from whichever
runner the leg drew.

## Environment

The runner strips `ASH_GC_STATS`, `ASH_TIER_LOG` and `ASH_TIER` from the
inherited environment and exposes them as flags, so a run's configuration is
in its JSON:

| Flag | Env | |
|---|---|---|
| `--gc-stats` | `ASH_GC_STATS=1` | GC counters in the instrumented run |
| `--tier-log` | `ASH_TIER_LOG=1` | per-function install lines |
| `--gc-heap-mb N` | `ASH_GC_HEAP_MB=N` | heap reservation (default 512 MB); applies to timed runs too |

`--jit-log` goes to the instrumented run only.

## The corpus

`benchmarks.toml`, in run order. A bench with a `parity_case` inherits its
`.hl`, expectation, normalization, timeout and `slow` flag.

- `basic` — startup floor.
- `tiered_hotloop` — the ladder's designated exercise.
- `mandelbrot_small`, `mandelbrot` — FP kernels; `mandelbrot` is `slow` and
  needs `--include-slow` and a reduced iteration count.
- `array_push`, `gc`, `binary_trees` — allocation.
- `stdlib`, `jsonparse`, `jsonmin*`, `jsonarr`, `mapiter`, `map_simple`.
- `nbody`, `deltablue`, `fib`, the call benches — dispatch and calls.
- `simd_dot`, `simd_particles` — ash-simd.

`heaps_game` is not a benchmark: it opens a window and never returns. It is
carried under `--include-windowed`, restricted to `full-jit`, to keep a
machine-readable record of whether that build still crashes on it.

## JSON (schema v1)

Top level: `schema_version`, `generated_unix`, `generated_iso`,
`sweep_seconds`, `git`, `system`, `load`, `config`, `binaries`, `results`.
Each result:

```json
{
  "benchmark": "tiered_hotloop",
  "mode": "hybrid-cranelift",
  "group": "jit",
  "hl": "test_tiered_hotloop.hl",
  "command": "…/ash --mode hybrid --jit-tier cranelift --quiet …",
  "expectation": "exact",
  "status": "OK",
  "detail": "stdout matches interp",
  "reference_source": "interp",
  "wall_ms": { "min_ms": …, "median_ms": …, "mean_ms": …, "max_ms": …, "stddev_ms": …, "runs": 5 },
  "samples_ms": [ … ],
  "peak_rss_bytes": 34603008,
  "exit_code": 0,
  "signal": null,
  "checksum": { "value": "22816350", "label": "unfused", "accepted": true },
  "output_stable": true,
  "tiered": { "attempted": 1, "succeeded": 1, "failed": 0, "compiled_calls": 19886, "fallbacks": 0, "cranelift": 1, "llvm": 0 },
  "tier_installs": [ { "findex": 255, "tier": "cranelift", "compile_ms": 5.84 } ],
  "gc": { "collections": 0, "blocks_reclaimed": 0, "live_blocks": 0, … }
}
```

Raw `samples_ms` are kept so a later analysis can compute what this runner
did not.

## Related tools

| | |
|---|---|
| `scripts/hl_bench.py` | the HashLink JIT, HashLink/C and JVM lanes on the same corpus, for the published page |
| `scripts/aot_bench.py` | the AOT lane |
| `scripts/generate_parity_oracle.py` | the HashLink oracle bundle `--oracle` consumes; runs in CI |
| `crates/ash_cli/tests/parity_matrix.rs` | the parity gate proper; `ash_bench.py` borrows its case metadata and normalization |
