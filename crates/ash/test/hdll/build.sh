#!/bin/bash
set -e
cd "$(dirname "$0")"
REPO_ROOT="$(cd ../../../.. && pwd)"
cc -shared -o mytest.hdll mytest.c -I"$REPO_ROOT/std"
haxe -main TestHdll --hl test_hdll.hl
echo "Built mytest.hdll and test_hdll.hl"
