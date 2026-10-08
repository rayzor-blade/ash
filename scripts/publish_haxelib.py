#!/usr/bin/env python3
"""Validate and optionally submit existing release ZIPs, in dependency order."""

import argparse
import json
import os
from pathlib import Path
import re
import subprocess
from zipfile import ZipFile


SEMVER = re.compile(r"(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(-(alpha|beta|rc)(\.(0|[1-9]\d*))?)?")


def tag_version(tag):
    core, separator, preview = tag.removeprefix("v").partition("-")
    if not re.fullmatch(r"\d+\.\d+\.\d+", core):
        raise ValueError("Publication requires a versioned release tag, not nightly")
    version = ".".join(str(int(part)) for part in core.split("."))
    if separator:
        version += "-" + preview
    if not SEMVER.fullmatch(version):
        raise ValueError(f"Invalid release version: {tag}")
    return version


def prepare_contributors(directory, names, account):
    """Set the publishing owner on release copies, preserving code and binaries."""
    for path in directory.glob("*.zip"):
        with ZipFile(path) as archive:
            metadata = json.loads(archive.read("haxelib.json"))
            if metadata["name"] not in names:
                continue
            contributors = [account] + [name for name in (metadata.get("contributors") or ["darmie"]) if name != account]
            if contributors == metadata.get("contributors"):
                continue
            metadata["contributors"] = contributors
            temporary = path.with_suffix(".tmp")
            with ZipFile(temporary, "w") as patched:
                for entry in archive.infolist():
                    data = json.dumps(metadata, indent=2) + "\n" if entry.filename == "haxelib.json" else archive.read(entry)
                    patched.writestr(entry, data)
        temporary.replace(path)
        print(f"{metadata['name']}: prepared release metadata for {account}")


def packages_in(directory, names, account, tag, independent_versions=False):
    selected = {}
    expected_version = None if tag == "nightly" or independent_versions else tag_version(tag)
    for path in sorted(directory.glob("*.zip")):
        with ZipFile(path) as archive:
            metadata = json.loads(archive.read("haxelib.json"))
            name = metadata["name"]
            if name not in names:
                continue
            if name in selected:
                raise ValueError(f"Multiple archives for {name}")
            if account not in metadata.get("contributors", []):
                raise ValueError(f"{name}: contributors must include {account}")
            version = metadata["version"]
            if not SEMVER.fullmatch(version):
                raise ValueError(f"{name}: invalid Haxelib version {version}")
            if expected_version and version != expected_version:
                raise ValueError(f"{name}: {version} does not match release {tag}")
            manifest = json.loads(archive.read("native/hdlls.json")) if "native/hdlls.json" in archive.namelist() else None
            if manifest:
                if "platforms" in manifest:
                    if manifest["tag"] != tag:
                        raise ValueError(f"{name}: native downloads are pinned to a different release")
                    if not re.fullmatch(r"[0-9a-f]{40}", manifest.get("revision", "")):
                        raise ValueError(f"{name}: native download revision is missing")
                    for entry in manifest["platforms"].values():
                        if not re.fullmatch(r"[0-9a-f]{64}", entry.get("sha256", "")):
                            raise ValueError(f"{name}: native download checksum is missing")
                else:
                    for entry in manifest.values():
                        if not archive.read(entry["packagePath"]):
                            raise ValueError(f"{name}: empty native library")
                        if "importPath" in entry and not archive.read(entry["importPath"]):
                            raise ValueError(f"{name}: empty native import library")
            selected[name] = (path, metadata)
    missing = set(names) - selected.keys()
    if missing:
        raise ValueError("Missing release archives: " + ", ".join(sorted(missing)))
    preceding = set()
    for name in names:
        metadata = selected[name][1]
        for dependency, version in metadata.get("dependencies", {}).items():
            if dependency in selected:
                if dependency not in preceding:
                    raise ValueError(f"Publish {dependency} before {name}")
                if version and version != selected[dependency][1]["version"]:
                    raise ValueError(f"{name}: dependency {dependency} version does not match its archive")
        preceding.add(name)
    return [selected[name] for name in names]


def registry_versions(name):
    result = subprocess.run(["haxelib", "info", name], text=True, capture_output=True, timeout=120)
    if result.returncode:
        if "No such Project" in result.stdout + result.stderr:
            return set()
        raise ValueError(f"Could not query Haxelib for {name}: {result.stdout}{result.stderr}")
    return set(re.findall(r"(?m)^\s+\S+\s+(\d+\.\d+\.\d+(?:-\S+)?)\s*:", result.stdout))


def submit(packages, account, password):
    names = {metadata["name"] for _, metadata in packages}
    # Check the whole batch before submitting its first archive.
    versions = {}
    for _, metadata in packages:
        for dependency, version in metadata.get("dependencies", {}).items():
            if dependency in names:
                continue
            if dependency not in versions:
                versions[dependency] = registry_versions(dependency)
            if not versions[dependency] or version and version not in versions[dependency]:
                raise ValueError(f"Publish dependency {dependency}{' ' + version if version else ''} before {metadata['name']}")
    for path, metadata in packages:
        name, version = metadata["name"], metadata["version"]
        if version in registry_versions(name):
            print(f"{name} {version}: already published; skipping")
            continue
        arguments = ["haxelib", "submit", str(path)]
        if len(metadata["contributors"]) > 1:
            arguments.append(account)
        arguments += [password, "--never"]
        # Avoid exception messages containing the command's password argument.
        result = subprocess.run(arguments, stdin=subprocess.DEVNULL, timeout=600)
        if result.returncode:
            raise ValueError(f"Submission failed for {name} {version} (exit {result.returncode})")
        if version not in registry_versions(name):
            raise ValueError(f"Haxelib did not report the submitted {name} {version}")
        print(f"{name} {version}: published")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", type=Path)
    parser.add_argument("--packages", nargs="+", required=True, help="package names in dependency order")
    parser.add_argument("--tag", required=True)
    parser.add_argument("--account", default="rayzor")
    parser.add_argument("--independent-versions", action="store_true", help="Ash packages have versions independent of the runtime")
    parser.add_argument("--prepare-contributors", action="store_true", help="set the publishing owner in release archive metadata")
    parser.add_argument("--submit", action="store_true", help="submit using HAXELIB_PASSWORD; otherwise validate only")
    args = parser.parse_args()
    try:
        if args.submit:
            tag_version(args.tag)
            if not os.environ.get("HAXELIB_PASSWORD"):
                raise ValueError("Configure the HAXELIB_PASSWORD GitHub Actions secret for rayzor")
        if args.prepare_contributors:
            prepare_contributors(args.directory, args.packages, args.account)
        packages = packages_in(args.directory, args.packages, args.account, args.tag, args.independent_versions)
        for _, metadata in packages:
            print(f"Validated {metadata['name']} {metadata['version']} for {args.account}")
        if args.submit:
            submit(packages, args.account, os.environ["HAXELIB_PASSWORD"])
    except (ValueError, KeyError, OSError, subprocess.TimeoutExpired) as error:
        # TimeoutExpired also holds the submission command; never print it.
        message = "Haxelib command timed out" if isinstance(error, subprocess.TimeoutExpired) else str(error)
        parser.exit(1, f"Error: {message}\n")


if __name__ == "__main__":
    main()
