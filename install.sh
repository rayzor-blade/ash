#!/bin/sh
# Ash installer — https://github.com/rayzor-blade/ash
#
#   curl -fsSL https://raw.githubusercontent.com/rayzor-blade/ash/main/install.sh | sh
#
# Drops the `ash` binary (and, on macOS, its bundled dylibs) into ~/.ash/bin
# and adds that directory to your PATH. Ash requires a 64-bit target.
set -eu

REPO="rayzor-blade/ash"
DEST="${ASH_INSTALL_DIR:-$HOME/.ash/bin}"

os="$(uname -s)"
arch="$(uname -m)"
case "$os/$arch" in
  Darwin/arm64)  target="macos-aarch64" ;;
  Darwin/x86_64) target="macos-x86_64" ;;
  Linux/x86_64)  target="linux-x86_64" ;;
  Linux/aarch64) echo "error: no prebuilt Linux aarch64 binary yet — build from source (see README)" >&2; exit 1 ;;
  *) echo "error: unsupported platform $os/$arch (ash requires a 64-bit target)" >&2; exit 1 ;;
esac

asset="ash-${target}.tar.gz"
url="https://github.com/${REPO}/releases/latest/download/${asset}"
fallback="https://github.com/${REPO}/releases/download/nightly/${asset}"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

echo "downloading ${asset} ..."
if ! curl -fsSL -o "$tmp/$asset" "$url"; then
  echo "latest release has no ${asset}; trying the nightly build ..."
  curl -fsSL -o "$tmp/$asset" "$fallback" || {
    echo "error: no prebuilt binary available for ${target} yet" >&2
    exit 1
  }
fi

mkdir -p "$DEST"
tar xzf "$tmp/$asset" -C "$DEST"
chmod +x "$DEST/ash"

# A quick sanity run; a missing shared library shows up here, not later.
if ! "$DEST/ash" --help >/dev/null 2>&1; then
  echo "warning: $DEST/ash did not run cleanly." >&2
  if [ "$os" = "Linux" ]; then
    echo "  missing shared libraries? try:" >&2
    echo "  sudo apt-get install -y libzstd1 zlib1g libxml2 libtinfo6 libedit2 libffi8" >&2
  fi
fi

# A shell-agnostic env file, so there is always exactly one thing to
# source regardless of which rc the user's shell actually reads.
env_file="$(dirname "$DEST")/env"
cat > "$env_file" <<EOF
# Added by the ash installer. Source this from your shell config.
case ":\${PATH}:" in
  *":$DEST:"*) ;;
  *) export PATH="$DEST:\$PATH" ;;
esac
EOF

line=". \"$env_file\""

# Which rc this user's shell actually reads. Picking the first rc that
# happens to exist gets it wrong for anyone whose login shell is not the
# first match, so start from $SHELL.
case "$(basename "${SHELL:-sh}")" in
  zsh)  rcs="$HOME/.zshrc" ;;
  bash) rcs="$HOME/.bashrc" ;;
  fish) rcs="" ;;
  *)    rcs="$HOME/.profile" ;;
esac

added=""
for rc in $rcs; do
  [ -e "$rc" ] || : > "$rc"
  if ! grep -qs "$env_file" "$rc"; then
    printf '\n# Added by the ash installer\n%s\n' "$line" >> "$rc"
  fi
  added="$rc"
  break
done

echo
echo "installed: $DEST/ash"
"$DEST/ash" --help 2>/dev/null | head -3 || true

case ":${PATH}:" in
  *":$DEST:"*)
    echo
    echo "ash is on your PATH. Try:  ash --mode hybrid main.hl"
    exit 0
    ;;
esac

# This script runs in its own shell and cannot change the PATH of the
# shell that invoked it — which is exactly why the next line matters.
echo
echo "----------------------------------------------------------------"
if [ -n "$added" ]; then
  echo "PATH updated in $added, but NOT in this shell."
  echo "Run this now (or open a new terminal):"
else
  echo "Add ash to your PATH — run this now, and add it to your shell config:"
fi
echo
echo "    $line"
echo
echo "then:  ash --mode hybrid main.hl"
echo "----------------------------------------------------------------"
