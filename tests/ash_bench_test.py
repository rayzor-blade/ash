"""Allocation evidence must come from a correct, identified benchmark run."""

import importlib.util
import contextlib
import io
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location("ash_bench", ROOT / "scripts/ash_bench.py")
BENCH = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(BENCH)


class InstrumentedTest(unittest.TestCase):
    def setUp(self):
        self.args = BENCH.parse_args(["--iterations", "1", "--warmups", "0", "--gc-stats"])
        self.bench = BENCH.Bench({
            "name": "trees", "hl": "trees.hl", "expectation": "checksum",
            "checksums": [{"value": "-104864", "label": "reference"}],
        }, {}, ROOT)
        self.mode = BENCH.Mode({"name": "hybrid", "args": ["--mode", "hybrid"]})
        self.binaries = {"ash": Path("/fake/ash")}

    def result(self, checksum="-104864", rc=0, timeout=False):
        return BENCH.RunResult(
            rc, None, f"Checksum: {checksum}\n",
            "[gc] bytes allocated: 900.1MB (+ external 0.0MB)\n"
            "123456 maximum resident set size\n", 10.0, timeout,
        )

    def run_one(self, results, base=False):
        binaries = self.binaries | ({"ash-base": Path("/fake/base")} if base else {})
        with patch.object(BENCH, "exec_run", side_effect=results) as execute, \
                patch.object(BENCH, "time_wrapper", return_value=["/usr/bin/time", "-l"]):
            record = BENCH.run_one(self.mode, self.bench, binaries, None, self.args, ROOT)
        return record, execute

    def test_wrong_instrumented_checksum_invalidates_correct_timing(self):
        record, _ = self.run_one([self.result(), self.result("0")])
        self.assertEqual(record["status"], BENCH.STATUS_INVALID)
        self.assertEqual(record["samples_ms"], [10.0])
        self.assertIsNone(record["gc"])
        self.assertIsNone(record["peak_rss_bytes"])
        self.assertFalse(record["instrumented"]["checksum"]["accepted"])

    def test_failed_or_incomplete_diagnostics_never_supply_counters(self):
        for failure, status in [
            (self.result(rc=1), BENCH.STATUS_FAIL),
            (self.result(rc=None, timeout=True), BENCH.STATUS_TIMEOUT),
        ]:
            with self.subTest(status=status):
                record, _ = self.run_one([self.result(), failure])
                self.assertEqual(record["status"], status)
                self.assertIsNone(record["gc"])

    def test_base_has_separately_judged_allocation_evidence(self):
        record, execute = self.run_one(
            [self.result(), self.result(), self.result(), self.result("0")], base=True,
        )
        self.assertEqual(record["status"], BENCH.STATUS_OK)
        self.assertEqual(record["gc"]["bytes_allocated_mb"], 900.1)
        self.assertEqual(record["base"]["status"], BENCH.STATUS_INVALID)
        self.assertIsNone(record["base"]["instrumented"]["gc"])
        self.assertIn("/fake/base", execute.call_args_list[-1].args[0])
        # Both instrumented processes run after the interleaved clean timings.
        self.assertEqual(record["samples_ms"], [10.0])
        self.assertEqual(record["base"]["samples_ms"], [10.0])

    def test_exact_output_diagnostics_use_the_same_reference(self):
        self.bench.expectation = "exact"
        ref = BENCH.Reference("oracle", "Checksum: -104864\n", 0)
        with patch.object(BENCH, "exec_run", return_value=self.result()), \
                patch.object(BENCH, "time_wrapper", return_value=None):
            result = BENCH.run_instrumented(
                self.mode, self.bench, self.binaries, self.args, ROOT, ref,
            )
        self.assertEqual(result["status"], BENCH.STATUS_OK)
        self.assertIsNone(result["peak_rss_bytes"])
        self.assertEqual(result["gc"]["bytes_allocated_mb"], 900.1)

    def test_logs_preserve_failed_output_and_keep_head_and_base_separate(self):
        with tempfile.TemporaryDirectory() as temporary:
            self.args.diagnostics_dir = temporary
            record, _ = self.run_one(
                [self.result(), self.result(), self.result(), self.result("0")], base=True,
            )
            head = record["instrumented"]["logs"]
            base = record["base"]["instrumented"]["logs"]
            self.assertNotEqual(head["stdout"], base["stdout"])
            self.assertEqual(Path(head["stdout"]).read_text(), "Checksum: -104864\n")
            self.assertEqual(Path(base["stdout"]).read_text(), "Checksum: 0\n")
            self.assertIn("900.1MB", Path(base["stderr"]).read_text())

    def test_content_identity_changes_when_size_does_not(self):
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "ash"
            path.write_bytes(b"abc")
            before = BENCH.file_identity(path)
            path.write_bytes(b"def")
            after = BENCH.file_identity(path)
            self.assertEqual(before["bytes"], after["bytes"])
            self.assertNotEqual(before["sha256"], after["sha256"])

    def test_install_time_is_separate_from_compile_duration(self):
        rows = BENCH.parse_tier_installs(
            "[tier] install findex=24 name=Fun_24 tier=llvm addr=0x100 in 38.42ms t=57.4ms\n"
            "[tier] install findex=22 tier=cranelift addr=0x200 ops=12 in 0.50ms\n"
        )
        self.assertEqual(rows[0]["compile_ms"], 38.42)
        self.assertEqual(rows[0]["installed_at_run_ms"], 57.4)
        self.assertIsNone(rows[1]["installed_at_run_ms"])

    def test_command_exit_gates_bad_evidence_even_without_a_baseline(self):
        for status in (BENCH.STATUS_OK, BENCH.STATUS_INVALID, BENCH.STATUS_FAIL, BENCH.STATUS_TIMEOUT):
            with self.subTest(status=status):
                record = {"benchmark":"binary_trees", "mode":"hybrid-auto", "status":status,
                          "detail":"test evidence", "wall_ms":None}
                with patch.object(BENCH, "run_one", return_value=record), \
                        patch.object(BENCH, "system_info", return_value={"cpu_count":1}), \
                        patch.object(BENCH, "print_table"), patch.object(BENCH, "print_ab"), \
                        patch.object(BENCH, "print_summary"), \
                        contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
                    code = BENCH.main(["--ash", sys.executable, "--benchmarks", "binary_trees",
                                       "--modes", "hybrid-auto", "--ignore-load"])
                self.assertEqual(code, 0 if status == BENCH.STATUS_OK else 1)


if __name__ == "__main__":
    unittest.main()
