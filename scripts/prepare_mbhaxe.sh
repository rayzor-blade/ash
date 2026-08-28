#!/usr/bin/env bash
# Build an isolated, provenance-checked MBHaxe fixture for Ash.
#
# The SDL boundary is deliberately strict: Haxe externs and the native HDLL
# both come from the same pinned RandomityGuy/hashlink checkout. Nothing from
# crates/ash_sdl or Cargo's target/*/deps directory is accepted or copied.
set -euo pipefail

ASH_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WORK_ROOT="${ASH_MBHAXE_WORK_ROOT:-${ASH_ROOT}/target/mbhaxe}"
SOURCE_ROOT="${WORK_ROOT}/source"
HAXELIB_ROOT="${WORK_ROOT}/haxelib"
RUN_ROOT="${WORK_ROOT}/run"
NATIVE_DIR="${ASH_MBHAXE_NATIVE_DIR:-}"

MBHAXE_URL="https://github.com/RandomityGuy/MBHaxe.git"
MBHAXE_REF="0a95534a30bb0e34615ef924780ebec1f6cc6356"
HEAPS_URL="https://github.com/RandomityGuy/heaps.git"
HEAPS_REF="9317e1a4ac6bc6936a7d9062555081afedadc0ae"
HASHLINK_URL="https://github.com/RandomityGuy/hashlink.git"
HASHLINK_REF="e3e864e3fc9633cf448f13e333ebd5dca0866d11"
DATACHANNEL_URL="https://github.com/RandomityGuy/hxDatachannel.git"
DATACHANNEL_REF="00a453cce65b59b399bfbe2316f12c8940b92e43"
COLYSEUS_URL="https://github.com/colyseus/colyseus-websocket-hx.git"
COLYSEUS_REF="11b54e47e6f66377245c7e21ecd336f18a34c715"

usage() {
    cat <<'EOF'
usage: scripts/prepare_mbhaxe.sh [--native-dir DIR]

Builds target/mbhaxe/run with pinned MBHaxe bytecode, its matching SDL2
sdl.hdll, a relocatable Ash compatibility runtime, and optional non-SDL HDLLs
from DIR. The source DIR's sdl.hdll is always ignored.

Environment:
  ASH_MBHAXE_WORK_ROOT   generated workspace (default: target/mbhaxe)
  ASH_MBHAXE_NATIVE_DIR  same as --native-dir
EOF
}

while (($#)); do
    case "$1" in
        --native-dir)
            [[ $# -ge 2 ]] || { usage >&2; exit 2; }
            NATIVE_DIR="$2"
            shift 2
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "error: unknown argument: $1" >&2
            usage >&2
            exit 2
            ;;
    esac
done

case "${WORK_ROOT}" in
    "${ASH_ROOT}"/target/*) ;;
    *)
        echo "error: ASH_MBHAXE_WORK_ROOT must be below ${ASH_ROOT}/target" >&2
        exit 1
        ;;
esac

if [[ "$(uname -s)" != "Darwin" ]]; then
    echo "error: the MBHaxe SDL2 bundle workflow currently targets macOS" >&2
    exit 1
fi

for tool in git haxe haxelib clang pkg-config otool install_name_tool nm strings shasum codesign; do
    command -v "${tool}" >/dev/null || {
        echo "error: required tool not found: ${tool}" >&2
        exit 1
    }
done
pkg-config --exists sdl2 || {
    echo "error: SDL2 development files not found (brew install sdl2)" >&2
    exit 1
}

ASH_BIN="${ASH_ROOT}/target/release/ash"
ASH_LIBHL="${ASH_ROOT}/target/release/libhl.dylib"
[[ -x "${ASH_BIN}" ]] || {
    echo "error: ${ASH_BIN} is missing; run cargo build --release --bin ash" >&2
    exit 1
}
[[ -f "${ASH_LIBHL}" ]] || {
    echo "error: ${ASH_LIBHL} is missing; build the release runtime first" >&2
    exit 1
}

mkdir -p "${SOURCE_ROOT}" "${HAXELIB_ROOT}"

checkout_pinned() {
    local name="$1"
    local url="$2"
    local commit="$3"
    local checkout_dir="${SOURCE_ROOT}/${name}"

    if [[ ! -d "${checkout_dir}/.git" ]]; then
        git clone --filter=blob:none --no-checkout "${url}" "${checkout_dir}"
    fi
    local origin
    origin="$(git -C "${checkout_dir}" remote get-url origin)"
    if [[ "${origin}" != "${url}" ]]; then
        echo "error: ${checkout_dir} has unexpected origin ${origin}" >&2
        exit 1
    fi
    if ! git -C "${checkout_dir}" cat-file -e "${commit}^{commit}" 2>/dev/null; then
        git -C "${checkout_dir}" fetch --depth 1 origin "${commit}"
    fi
    if [[ "$(git -C "${checkout_dir}" rev-parse HEAD 2>/dev/null || true)" != "${commit}" ]]; then
        if [[ -n "$(git -C "${checkout_dir}" status --porcelain)" ]]; then
            echo "error: refusing to replace dirty generated checkout ${checkout_dir}" >&2
            exit 1
        fi
        git -C "${checkout_dir}" checkout --detach "${commit}"
    fi
    if [[ "$(git -C "${checkout_dir}" rev-parse HEAD)" != "${commit}" ]]; then
        echo "error: failed to pin ${name} to ${commit}" >&2
        exit 1
    fi
}

checkout_pinned MBHaxe "${MBHAXE_URL}" "${MBHAXE_REF}"
checkout_pinned heaps-rg "${HEAPS_URL}" "${HEAPS_REF}"
checkout_pinned hashlink-rg "${HASHLINK_URL}" "${HASHLINK_REF}"
checkout_pinned hxDatachannel-rg "${DATACHANNEL_URL}" "${DATACHANNEL_REF}"
checkout_pinned colyseus-websocket "${COLYSEUS_URL}" "${COLYSEUS_REF}"

if [[ ! -f "${HAXELIB_ROOT}/.haxelib/.repo-version" ]]; then
    (cd "${HAXELIB_ROOT}" && haxelib newrepo)
fi

register_dev() {
    local library="$1"
    local source_dir="$2"
    (cd "${HAXELIB_ROOT}" && haxelib dev "${library}" "${source_dir}" >/dev/null)
}

# These six explicit registrations make global haxelib selection irrelevant.
register_dev heaps "${SOURCE_ROOT}/heaps-rg"
register_dev hlsdl "${SOURCE_ROOT}/hashlink-rg/libs/sdl"
register_dev datachannel "${SOURCE_ROOT}/hxDatachannel-rg"
register_dev colyseus-websocket "${SOURCE_ROOT}/colyseus-websocket"
register_dev format "$(haxelib --global libpath format:3.8.0)"
register_dev hlopenal "$(haxelib --global libpath hlopenal:1.5.0)"

# RUN_ROOT is generated exclusively by this script. Recreate it so a stale or
# hand-copied sdl.hdll can never survive into the next diagnostic run.
rm -rf -- "${RUN_ROOT}"
mkdir -p "${RUN_ROOT}"

(
    cd "${HAXELIB_ROOT}"
    haxe \
        -cp "${SOURCE_ROOT}/MBHaxe" \
        -cp "${SOURCE_ROOT}/MBHaxe/src" \
        -lib heaps \
        -lib hlsdl \
        -lib datachannel \
        -lib colyseus-websocket \
        -hl "${RUN_ROOT}/marblegame.hl" \
        -D windowSize=1280x720 \
        -D keep-inline-positions \
        -D highDPI \
        -D flow_border \
        --main Main \
        -debug
)

read -r -a sdl_cflags <<<"$(pkg-config --cflags sdl2)"
read -r -a sdl_libs <<<"$(pkg-config --libs sdl2)"

clang \
    -dynamiclib -O2 -fPIC -std=c11 \
    -DGL_SILENCE_DEPRECATION \
    -Wno-pointer-sign -Wno-incompatible-pointer-types \
    -I"${SOURCE_ROOT}/hashlink-rg/src" \
    -I"${SOURCE_ROOT}/hashlink-rg/include" \
    -I"${SOURCE_ROOT}/hashlink-rg/libs/sdl" \
    "${sdl_cflags[@]}" \
    "${SOURCE_ROOT}/hashlink-rg/libs/sdl/sdl.c" \
    "${SOURCE_ROOT}/hashlink-rg/libs/sdl/gl.c" \
    -L"${ASH_ROOT}/target/release" -lhl \
    "${sdl_libs[@]}" \
    -framework OpenGL \
    -Wl,-rpath,@loader_path \
    -Wl,-install_name,@rpath/sdl.hdll \
    -o "${RUN_ROOT}/sdl.hdll"

linked_ash_std="$(otool -L "${RUN_ROOT}/sdl.hdll" | awk '/libash_std.*dylib/ { print $1; exit }')"
if [[ -z "${linked_ash_std}" ]]; then
    echo "error: freshly built sdl.hdll did not link against Ash's compatibility runtime" >&2
    exit 1
fi
install_name_tool -change "${linked_ash_std}" @rpath/libhl.dylib "${RUN_ROOT}/sdl.hdll"
install_name_tool -id @rpath/sdl.hdll "${RUN_ROOT}/sdl.hdll"

cp "${ASH_BIN}" "${RUN_ROOT}/ash"
cp "${ASH_LIBHL}" "${RUN_ROOT}/libhl.dylib"
install_name_tool -id @rpath/libhl.dylib "${RUN_ROOT}/libhl.dylib"

if [[ -n "${NATIVE_DIR}" ]]; then
    [[ -d "${NATIVE_DIR}" ]] || {
        echo "error: native HDLL directory not found: ${NATIVE_DIR}" >&2
        exit 1
    }
    shopt -s nullglob
    for hdll in "${NATIVE_DIR}"/*.hdll; do
        [[ "$(basename "${hdll}")" == "sdl.hdll" ]] && continue
        cp "${hdll}" "${RUN_ROOT}/"
    done
    shopt -u nullglob
fi

ln -s "${SOURCE_ROOT}/MBHaxe/data" "${RUN_ROOT}/data"

codesign --force -s - "${RUN_ROOT}/sdl.hdll" >/dev/null
codesign --force -s - "${RUN_ROOT}/libhl.dylib" >/dev/null
codesign --force -s - "${RUN_ROOT}/ash" >/dev/null

if strings "${RUN_ROOT}/sdl.hdll" | grep -Eiq 'ash_sdl|crates/ash_sdl|target/(debug|release)/deps/libsdl'; then
    echo "error: rejected sdl.hdll contaminated by the decommissioned ash_sdl crate" >&2
    exit 1
fi
if ! otool -L "${RUN_ROOT}/sdl.hdll" | grep -q '@rpath/libhl.dylib'; then
    echo "error: sdl.hdll does not resolve the staged Ash libhl" >&2
    exit 1
fi
if ! otool -L "${RUN_ROOT}/sdl.hdll" | grep -q 'libSDL2'; then
    echo "error: sdl.hdll is not linked to SDL2" >&2
    exit 1
fi
if otool -L "${RUN_ROOT}/sdl.hdll" | grep -q 'libSDL3'; then
    echo "error: SDL3 library found in the SDL2 MBHaxe fixture" >&2
    exit 1
fi
for symbol in hlp_win_create hlp_win_get_pixel_size hlp_gl_create_shader hlp_gl_shader_source; do
    nm -gU "${RUN_ROOT}/sdl.hdll" | grep -q " _${symbol}$" || {
        echo "error: sdl.hdll is missing ${symbol}" >&2
        exit 1
    }
done

{
    echo "MBHaxe ${MBHAXE_REF}"
    echo "heaps ${HEAPS_REF}"
    echo "hashlink-hlsdl-sdl2 ${HASHLINK_REF}"
    echo "hxDatachannel ${DATACHANNEL_REF}"
    echo "colyseus-websocket ${COLYSEUS_REF}"
    echo "haxe $(haxe --version)"
    echo "sdl2 $(pkg-config --modversion sdl2)"
    shasum -a 256 "${RUN_ROOT}/marblegame.hl" "${RUN_ROOT}/sdl.hdll" "${RUN_ROOT}/libhl.dylib"
} >"${RUN_ROOT}/PROVENANCE.txt"

echo "prepared isolated MBHaxe fixture: ${RUN_ROOT}"
echo "validated SDL source: ${HASHLINK_URL}@${HASHLINK_REF}:libs/sdl"
if [[ -z "${NATIVE_DIR}" ]]; then
    echo "warning: no non-SDL HDLL directory supplied; launch may report missing libraries" >&2
fi
echo "run: cd '${RUN_ROOT}' && ./ash --mode interp marblegame.hl"
