"""Prevent incorrect or incomparable runs from becoming fusion evidence."""
import importlib.util
import os
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts"))
import bench_tree_fusion as fusion


class Evidence(unittest.TestCase):
    def result(self, output="Checksum: -104864\n", code=0, timeout=False):
        return fusion.bench.RunResult(code, None, output, "", 12, timeout)

    def test_incorrect_failed_and_timeout_runs_refuse(self):
        fusion.validate_result(self.result())
        for r in [self.result("Checksum: 0"), self.result(code=1), self.result(timeout=True),
                  self.result("Checksum: -104864\nChecksum: 0")]:
            with self.assertRaises(ValueError): fusion.validate_result(r)

    def test_changed_workload_refuses(self):
        with tempfile.TemporaryDirectory() as d:
            root = Path(d)
            base = root / "crates/ash/test/tests"
            base.mkdir(parents=True)
            (base / "BenchBinaryTrees.hx").write_text("smaller workload")
            with self.assertRaisesRegex(ValueError, "artifact changed"): fusion.inputs(root)

    def test_timing_has_no_inherited_experiment_or_instrumentation(self):
        with patch.dict(os.environ, {"ASH_GC_TLAB": "0", "ASH_AIR_NO_INLINE": "1", "ASH_PROFILE": "all"}):
            self.assertFalse(any(k.startswith("ASH_") for k in fusion.environment()))
            env = fusion.environment(True)
            self.assertEqual(env["ASH_GC_TLAB"], "0")
            self.assertEqual(env["ASH_GC_STATS"], "1")
            self.assertNotIn("ASH_AIR_NO_INLINE", env)

    def test_counter_gate_includes_external_allocations_and_requires_fusion(self):
        prefix = "[gc] collections: 2\n[gc] pause total: 1.0ms, max 1.0ms, total 1000000ns\n"
        counter = "[gc] bytes allocated: 4.0MB (+ external 0.0MB)\n"
        helper = ("[tree-fusion] helper=388 producer=24 consumer=23 ops=19 regs=19 node_allocations=0\n"
                  "[tree-fusion] transformed_sites=3 helpers=1\n")
        fusion.validate_diagnostic("on", prefix + counter + helper)
        for text in [prefix + counter, prefix + counter + helper.replace("allocations=0", "allocations=1"),
                     prefix + counter.replace("external 0.0", "external 900.0") + helper,
                     prefix + helper]:
            with self.assertRaises(ValueError): fusion.validate_diagnostic("on", text)
        with self.assertRaises(ValueError): fusion.validate_diagnostic("off", prefix + counter)

    def test_alternating_pairs_and_summary_exclude_warmup_diagnostics(self):
        self.assertEqual([fusion.order(i) for i in range(3)], [["off", "on"], ["on", "off"], ["off", "on"]])
        runs = []
        for pair in range(2):
            for variant in fusion.order(pair):
                runs.append(dict(phase="timing", variant=variant, wall_ms=100 if variant == "off" else 25))
        for phase in ["warmup", "accounting", "ir"]:
            for variant in ["off", "on"]:
                runs.append(dict(phase=phase, variant=variant, wall_ms=9999,
                                 gc=dict(bytes_allocated_mb=900 if variant == "off" else 4, external_bytes_mb=0)))
        summary = fusion.summarize(runs)
        self.assertEqual(summary["paired_on_over_off"]["median"], 0.25)
        self.assertEqual(summary["paired_ratio_bootstrap_95pct"], [0.25, 0.25])
        self.assertGreater(summary["allocation_reduction_pct"], 99)

    def test_failed_experiment_persists_failure_without_summary(self):
        with tempfile.TemporaryDirectory() as d:
            dest = Path(d) / "result"
            with patch.object(fusion, "run_experiment", side_effect=ValueError("bad checksum")):
                self.assertEqual(fusion.main(["--ash", "/missing", "--source-revision", "test", "--out", str(dest)]), 1)
            import json
            data = json.loads((dest / "result.json").read_text())
            self.assertEqual(data["status"], "FAIL")
            self.assertNotIn("summary", data)


if __name__ == "__main__": unittest.main()
