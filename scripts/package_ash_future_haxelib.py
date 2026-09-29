#!/usr/bin/env python3
"""Build a self-contained ash-future Haxelib ZIP from platform HDLLs."""

import argparse
import json
from pathlib import Path
from zipfile import ZIP_DEFLATED, ZipFile


ROOT = Path(__file__).resolve().parent.parent
LIB = ROOT / "haxelib" / "ash-future"
MANIFEST = LIB / "native" / "hdlls.json"


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("assets", type=Path, help="directory containing ash_future-<platform>.hdll assets")
    parser.add_argument("--check", action="store_true", help="only check that every asset exists")
    args = parser.parse_args()

    platforms = json.loads(MANIFEST.read_text())
    missing = [
        spec["releaseAsset"]
        for spec in platforms.values()
        if not (args.assets / spec["releaseAsset"]).is_file()
    ]
    missing.extend(
        spec["importAsset"]
        for spec in platforms.values()
        if "importAsset" in spec and not (args.assets / spec["importAsset"]).is_file()
    )
    if missing:
        parser.error("missing release HDLLs: " + ", ".join(missing))
    if args.check:
        return

    version = json.loads((LIB / "haxelib.json").read_text())["version"]
    output = args.assets / f"ash-future-{version}.zip"
    with ZipFile(output, "w", compression=ZIP_DEFLATED) as archive:
        for name in ("haxelib.json", "README.md", "extraParams.hxml", "native/hdlls.json"):
            archive.write(LIB / name, name)
        for source in sorted((LIB / "ash").rglob("*.hx")):
            archive.write(source, source.relative_to(LIB))
        for spec in platforms.values():
            archive.write(args.assets / spec["releaseAsset"], spec["packagePath"])
            if "importAsset" in spec:
                archive.write(args.assets / spec["importAsset"], spec["importPath"])
    print(f"wrote {output}")


if __name__ == "__main__":
    main()
