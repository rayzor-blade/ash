"""Publication gates without registry writes or account credentials."""

import importlib.util
import json
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch
from zipfile import ZipFile

ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location("publish_haxelib", ROOT / "scripts/publish_haxelib.py")
PUBLISHER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(PUBLISHER)


def archive(directory, name, dependencies=None, contributors=None):
    path = directory / (name + ".zip")
    metadata = dict(name=name, version="0.1.0", contributors=["rayzor", "rayzor"] if contributors is None else contributors,
                    dependencies=dependencies or {})
    with ZipFile(path, "w") as output:
        output.writestr("haxelib.json", json.dumps(metadata))
        output.writestr("Example.hx", "class Example {}")
    return path, metadata


class PublishTest(unittest.TestCase):
    def test_all_archives_must_be_present_and_ordered(self):
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            archive(directory, "ashui-components", {"ashui": "0.1.0"})
            with self.assertRaisesRegex(ValueError, "Missing release archives"):
                PUBLISHER.packages_in(directory, ["ashui", "ashui-components"], "rayzor", "v0.1.0")
            archive(directory, "ashui")
            with self.assertRaisesRegex(ValueError, "before ashui-components"):
                PUBLISHER.packages_in(directory, ["ashui-components", "ashui"], "rayzor", "v0.1.0")
            self.assertEqual(len(PUBLISHER.packages_in(directory, ["ashui", "ashui-components"], "rayzor", "v0.1.0")), 2)
            with self.assertRaisesRegex(ValueError, "does not match release"):
                PUBLISHER.packages_in(directory, ["ashui"], "rayzor", "v0.2.0")

    def test_publishing_owner_update_preserves_sources(self):
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            path, _ = archive(directory, "ash-future", contributors=[])
            path = path.rename(directory / "ash-future-0.1.0.zip")
            PUBLISHER.prepare_contributors(directory, ["ash-future"], "rayzor")
            PUBLISHER.prepare_contributors(directory, ["ash-future"], "rayzor")
            with ZipFile(path) as output:
                self.assertEqual(json.loads(output.read("haxelib.json"))["contributors"], ["rayzor", "darmie"])
                self.assertEqual(output.read("Example.hx"), b"class Example {}")

    def test_missing_dependency_blocks_entire_batch(self):
        packages = [(Path("core.zip"), dict(name="core", version="0.1.0", contributors=["rayzor"])),
                    (Path("media.zip"), dict(name="media", version="0.1.0", contributors=["rayzor"], dependencies={"hlavi": ""}))]
        with patch.object(PUBLISHER, "registry_versions", return_value=set()), patch.object(PUBLISHER.subprocess, "run") as run:
            with self.assertRaisesRegex(ValueError, "Publish dependency hlavi"):
                PUBLISHER.submit(packages, "rayzor", "test-only")
            run.assert_not_called()

    def test_existing_version_is_never_overwritten(self):
        packages = [(Path("core.zip"), dict(name="core", version="0.1.0", contributors=["rayzor"]))]
        with patch.object(PUBLISHER, "registry_versions", return_value={"0.1.0"}), patch.object(PUBLISHER.subprocess, "run") as run:
            PUBLISHER.submit(packages, "rayzor", "test-only")
            run.assert_not_called()

    def test_submit_selects_the_new_account_and_refuses_overwrite(self):
        packages = [(Path("core.zip"), dict(name="core", version="0.1.0", contributors=["rayzor", "darmie"]))]
        with patch.object(PUBLISHER, "registry_versions", side_effect=[set(), {"0.1.0"}]), \
                patch.object(PUBLISHER.subprocess, "run", return_value=subprocess.CompletedProcess([], 0)) as run:
            PUBLISHER.submit(packages, "rayzor", "test-only")
            self.assertEqual(run.call_args.args[0], ["haxelib", "submit", "core.zip", "rayzor", "test-only", "--never"])

    def test_registry_connection_errors_are_not_absent_libraries(self):
        with patch.object(PUBLISHER.subprocess, "run", return_value=subprocess.CompletedProcess([], 1, "", "TLS failure")):
            with self.assertRaisesRegex(ValueError, "Could not query Haxelib"):
                PUBLISHER.registry_versions("ashui")
        with patch.object(PUBLISHER.subprocess, "run", return_value=subprocess.CompletedProcess([], 0, "Releases:\n   2026-10-08 0.1.0 : First\n   2026-10-09 0.2.0-rc.1 : Next\n", "")):
            self.assertEqual(PUBLISHER.registry_versions("ashui"), {"0.1.0", "0.2.0-rc.1"})

    def test_nightly_cannot_be_published(self):
        with self.assertRaisesRegex(ValueError, "not nightly"):
            PUBLISHER.tag_version("nightly")


if __name__ == "__main__":
    unittest.main()
