#!/usr/bin/env bash
# Push the working tree to the Linux box and (optionally) build there.
#
# One directory, always the same one. Cloning per experiment left three
# checkouts on a 6GB machine -- 15G of mostly duplicate target/ -- and made
# "which tree did that number come from?" a real question. rsync keeps the
# remote a mirror of what is in front of you, including uncommitted work,
# which a clone cannot do.
#
# target/ is excluded on purpose: it is the biggest thing in the tree and the
# remote's own is the one that matters for incremental builds.
#
#   scripts/sync_nuc.sh              # sync only
#   scripts/sync_nuc.sh --build      # sync, then build ash_std + ash
#   ASH_REMOTE=other:/path scripts/sync_nuc.sh
set -euo pipefail
remote="${ASH_REMOTE:-nuc:~/ash-linux}"
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

echo "syncing $root -> $remote"
rsync -az --delete \
  --exclude 'target/' --exclude '.git/' --exclude 'node_modules/' \
  --exclude '*.hdll' --exclude '*.o' --exclude 'crates/ash/test/hdll/build/' \
  "$root/" "$remote/"

[ "${1:-}" = "--build" ] || { echo "synced"; exit 0; }

host="${remote%%:*}"
dir="${remote#*:}"
# The environment this machine needs, recorded rather than rediscovered:
# llvm-22 has Polly, llvm-21 has the libclang bindgen understands, and the
# zstd symlink satisfies a static link Ubuntu leaves half-provided.
ssh "$host" "cd $dir && \
  export PATH=\$HOME/.cargo/bin:\$HOME/haxe:/snap/bin:\$PATH \
    LLVM_SYS_211_PREFIX=/usr/lib/llvm-22 \
    LIBCLANG_PATH=/usr/lib/llvm-21/lib \
    RUSTFLAGS=\"-L native=\$HOME/linkfix\" \
    CARGO_BUILD_JOBS=6 && \
  cargo build -p ash_std && cargo build -p ash"
