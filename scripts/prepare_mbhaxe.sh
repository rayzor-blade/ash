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
HOST_ARCH="$(uname -m)"

# Pick the SDL2 whose library is built for THIS machine.
#
# A Mac can carry both Homebrew prefixes at once — /usr/local from the Intel
# era and /opt/homebrew for arm64 — each with its own pkg-config and its own
# SDL2. Whichever leads PATH wins, so an unqualified `pkg-config sdl2` on an
# arm64 host happily reports the x86_64 install, and the build then dies deep
# inside SDL_cpuinfo.h pulling x86 intrinsics into an arm64 translation unit.
# Choose by inspecting the library, not by trusting the search order.
sdl2_pkgconfig=""
for candidate in pkg-config /opt/homebrew/bin/pkg-config /usr/local/bin/pkg-config; do
    command -v "${candidate}" >/dev/null || continue
    "${candidate}" --exists sdl2 2>/dev/null || continue
    libdir="$("${candidate}" --variable=libdir sdl2 2>/dev/null)"
    [[ -n "${libdir}" && -e "${libdir}/libSDL2.dylib" ]] || continue
    if lipo -archs "${libdir}/libSDL2.dylib" 2>/dev/null | tr ' ' '\n' | grep -qx "${HOST_ARCH}"; then
        sdl2_pkgconfig="${candidate}"
        break
    fi
done
[[ -n "${sdl2_pkgconfig}" ]] || {
    echo "error: no SDL2 built for ${HOST_ARCH} found (brew install sdl2)" >&2
    for candidate in /opt/homebrew/bin/pkg-config /usr/local/bin/pkg-config; do
        command -v "${candidate}" >/dev/null || continue
        libdir="$("${candidate}" --variable=libdir sdl2 2>/dev/null)" || continue
        [[ -e "${libdir}/libSDL2.dylib" ]] && echo "  ${libdir}/libSDL2.dylib is $(lipo -archs "${libdir}/libSDL2.dylib" 2>/dev/null)" >&2
    done
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
    # `git clone --no-checkout` writes no working tree while HEAD already names
    # the default branch tip. When the pinned commit IS that tip, comparing
    # HEAD to it succeeds and the checkout below is skipped -- leaving a repo
    # with no files, which surfaces much later as a missing subdirectory.
    local populated=1
    if [[ -z "$(ls -A "${checkout_dir}" 2>/dev/null | grep -v '^\.git$' || true)" ]]; then
        populated=0
    fi
    if [[ "${populated}" -eq 0 ]] \
        || [[ "$(git -C "${checkout_dir}" rev-parse HEAD 2>/dev/null || true)" != "${commit}" ]]; then
        if [[ "${populated}" -eq 1 && -n "$(git -C "${checkout_dir}" status --porcelain)" ]]; then
            echo "error: refusing to replace dirty generated checkout ${checkout_dir}" >&2
            exit 1
        fi
        git -C "${checkout_dir}" checkout --detach --force "${commit}"
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

read -r -a sdl_cflags <<<"$("${sdl2_pkgconfig}" --cflags sdl2)"
read -r -a sdl_libs <<<"$("${sdl2_pkgconfig}" --libs sdl2)"

clang \
    -arch "${HOST_ARCH}" \
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

# Upstream HDLLs are linked against `@rpath/libhl.1.dylib`, the versioned
# name HashLink installs. The fixture must not patch them — running the
# shipped binaries unmodified is what makes this a real test of Ash — so Ash's
# runtime answers to that name as well. A copy rather than a symlink so the
# staged tree stays self-contained and hashable.
cp "${ASH_LIBHL}" "${RUN_ROOT}/libhl.1.dylib"
install_name_tool -id @rpath/libhl.1.dylib "${RUN_ROOT}/libhl.1.dylib"

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
    # The HDLLs are taken exactly as the game ships them, so their own
    # third-party dependencies (libopenal, libpng, libuv, the vorbis family,
    # ...) have to come along too. Skip any libhl the source directory
    # carries: supplying Ash's runtime instead of upstream's is the entire
    # point of the fixture.
    for dylib in "${NATIVE_DIR}"/*.dylib; do
        case "$(basename "${dylib}")" in
            libhl*.dylib) continue ;;
        esac
        cp "${dylib}" "${RUN_ROOT}/"
    done
    # A copied Mach-O keeps a signature that no longer validates at its new
    # path, and anything fetched with a browser also carries a quarantine
    # attribute. Either one makes dlopen fail with "library load disallowed by
    # system policy", which reads like a missing dependency rather than a
    # signing problem. Re-sign ad hoc, exactly as this script already does for
    # the artifacts it builds itself.
    xattr -cr "${RUN_ROOT}" 2>/dev/null || true
    for staged in "${RUN_ROOT}"/*.hdll "${RUN_ROOT}"/*.dylib; do
        case "$(basename "${staged}")" in
            libhl.dylib|libhl.1.dylib) continue ;;
        esac
        codesign --force -s - "${staged}" >/dev/null 2>&1 || {
            echo "error: could not sign staged native $(basename "${staged}")" >&2
            exit 1
        }
    done
    shopt -u nullglob
fi

# A real directory, not a symlink into the source checkout.
#
# heaps resolves resource paths through the link and then mangles the result:
# with `data` symlinked to <work>/source/MBHaxe/data it looked for
# <work>/run/ce/MBHaxe/data/font/DomCasualD.fnt — the four characters of
# "sour" eaten off "source". The missing font threw inside
# ResourceLoader.init's callback, so hxd.App never reached
# `hxd.System.setLoop(mainLoop)`; the window and audio came up, the event loop
# ticked, and nothing was ever drawn because the loop function stayed null.
#
# Hard links keep the 375MB from being copied twice while still presenting a
# real directory tree, and fall back to a copy on filesystems that refuse
# them. Either way the game sees ordinary paths.
rm -rf "${RUN_ROOT}/data"
if ! cp -al "${SOURCE_ROOT}/MBHaxe/data" "${RUN_ROOT}/data" 2>/dev/null; then
    cp -R "${SOURCE_ROOT}/MBHaxe/data" "${RUN_ROOT}/data"
fi

codesign --force -s - "${RUN_ROOT}/sdl.hdll" >/dev/null
codesign --force -s - "${RUN_ROOT}/libhl.dylib" >/dev/null
codesign --force -s - "${RUN_ROOT}/libhl.1.dylib" >/dev/null
# Only what this script BUILDS is re-signed. The game's own HDLLs and their
# third-party dylibs are staged byte-for-byte: re-signing rewrites the
# signature and changes the hash, and "the shipped binaries, unmodified" is
# the property the fixture exists to demonstrate.
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
# Every staged binary must be the host's architecture. A mismatch here does
# not fail at load with a clear message — dyld reports the library as simply
# "not found", which reads as a missing HDLL and sends you looking in the
# wrong place entirely.
shopt -s nullglob
for artifact in "${RUN_ROOT}/ash" "${RUN_ROOT}"/*.hdll "${RUN_ROOT}"/*.dylib; do
    archs="$(lipo -archs "${artifact}" 2>/dev/null)"
    if ! tr ' ' '\n' <<<"${archs}" | grep -qx "${HOST_ARCH}"; then
        echo "error: $(basename "${artifact}") is ${archs:-unreadable}, not ${HOST_ARCH}" >&2
        exit 1
    fi
done
shopt -u nullglob
if [[ -n "${NATIVE_DIR}" ]]; then
    shopt -s nullglob
    for staged in "${RUN_ROOT}"/*.hdll "${RUN_ROOT}"/*.dylib; do
        name="$(basename "${staged}")"
        case "${name}" in sdl.hdll|libhl.dylib|libhl.1.dylib) continue ;; esac
        source_copy="${NATIVE_DIR}/${name}"
        [[ -e "${source_copy}" ]] || continue
        if ! cmp -s "${source_copy}" "${staged}"; then
            echo "error: ${name} was altered while staging; the fixture must run" >&2
            echo "       the shipped binaries unmodified" >&2
            exit 1
        fi
    done
    shopt -u nullglob
fi

sdl2_lib="$("${sdl2_pkgconfig}" --variable=libdir sdl2)/libSDL2.dylib"
if ! lipo -archs "${sdl2_lib}" 2>/dev/null | tr ' ' '\n' | grep -qx "${HOST_ARCH}"; then
    echo "error: linked ${sdl2_lib} is not ${HOST_ARCH}" >&2
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
    echo "sdl2 $("${sdl2_pkgconfig}" --modversion sdl2) ${HOST_ARCH} ($("${sdl2_pkgconfig}" --variable=libdir sdl2))"
    echo "arch ${HOST_ARCH}"
    if [[ -n "${NATIVE_DIR}" ]]; then
        echo "native-dir ${NATIVE_DIR}"
        shopt -s nullglob
        for staged in "${RUN_ROOT}"/*.hdll "${RUN_ROOT}"/*.dylib; do
            name="$(basename "${staged}")"
            case "${name}" in sdl.hdll|libhl.dylib|libhl.1.dylib) continue ;; esac
            echo "vendored ${name} $(lipo -archs "${staged}" 2>/dev/null | tr ' ' '+')"
        done
        shopt -u nullglob
    fi
    shasum -a 256 "${RUN_ROOT}/marblegame.hl" "${RUN_ROOT}/sdl.hdll" "${RUN_ROOT}/libhl.dylib"
} >"${RUN_ROOT}/PROVENANCE.txt"

echo "prepared isolated MBHaxe fixture: ${RUN_ROOT}"
echo "validated SDL source: ${HASHLINK_URL}@${HASHLINK_REF}:libs/sdl"

# Name the HDLLs this bytecode will look for and this fixture does not carry,
# rather than leaving them to be discovered as a load failure at launch. The
# build registers hlsdl, datachannel and hlopenal, so those are the libraries
# whose natives the .hl can reference; sdl is built here, the rest must come
# from --native-dir. A name scan of the bytecode is heuristic, so this only
# ever sharpens a warning — it never fails the build.
# Read the bytecode's strings ONCE into a variable rather than piping into
# each grep: `grep -q` exits at the first match and closes the pipe, `strings`
# takes SIGPIPE, and under `set -o pipefail` the pipeline then reports failure
# — so every match was discarded and this warning could never fire.
hl_names="$(strings "${RUN_ROOT}/marblegame.hl" || true)"
missing_hdlls=()
for lib in openal datachannel; do
    [[ -e "${RUN_ROOT}/${lib}.hdll" ]] && continue
    if grep -qx "${lib}" <<<"${hl_names}"; then
        missing_hdlls+=("${lib}.hdll")
    fi
done
if ((${#missing_hdlls[@]})); then
    echo "warning: bytecode references ${missing_hdlls[*]}, not staged here;" >&2
    echo "         supply them with --native-dir DIR (its sdl.hdll is ignored)" >&2
elif [[ -z "${NATIVE_DIR}" ]]; then
    echo "note: no --native-dir given, and the bytecode names no HDLL beyond sdl" >&2
fi
echo "run: cd '${RUN_ROOT}' && ./ash --mode interp marblegame.hl"
