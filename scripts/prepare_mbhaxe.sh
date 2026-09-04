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

# Every HDLL is built from the pinned checkout, by HashLink's own CMake.
#
# Copying prebuilt HDLLs in is how this fixture acquired an openal.hdll
# exporting 93 primitives where its source declares 157: it had been compiled
# against Apple's OpenAL.framework, which carries no EFX headers, so the whole
# `#ifdef ALC_EXT_EFX` block vanished. The game references 33 of the missing
# ones, and they surfaced at launch as unresolved natives -- reading like an
# Ash resolution bug rather than a library that simply lacks them.
#
# Upstream's build already knows each library's sources, flags and
# dependencies, so there is nothing to hand-derive. OPENAL_* is pinned at
# openal-soft because CMake otherwise finds Apple's framework first and
# reproduces exactly the defect above.
openal_prefix=""
for candidate in /opt/homebrew/opt/openal-soft /usr/local/opt/openal-soft; do
    [[ -f "${candidate}/include/AL/efx.h" ]] || continue
    [[ "$(lipo -archs "${candidate}/lib/libopenal.dylib" 2>/dev/null)" == *"${HOST_ARCH}"* ]] || continue
    openal_prefix="${candidate}"
    break
done
if [[ -z "${openal_prefix}" ]]; then
    echo "error: no ${HOST_ARCH} openal-soft with EFX headers found." >&2
    echo "       Apple's OpenAL.framework has no efx.h, and building against it" >&2
    echo "       silently drops 64 primitives: brew install openal-soft" >&2
    exit 1
fi

HL_BUILD="${WORK_ROOT}/hdll-build"
HL_HDLLS=(fmt openal ssl ui uv)
# -DCMAKE_FIND_FRAMEWORK=LAST is MBHaxe's own documented macOS build flag
# (README-macOS.md), and it is load-bearing rather than cosmetic. Without it
# CMake searches /Library/Frameworks and /System/Library/Frameworks first, so
# FindPNG resolved png.h out of Mono.framework -- a libpng 1.2-era header with
# no simplified read API -- while still linking Homebrew's libpng16. fmt.c
# guards png_decode on #ifdef PNG_IMAGE_VERSION, so the whole decoder compiled
# down to hl_error("PNG support is missing"), and the game reached its render
# loop with no textures. Frameworks last, and each library is found whole.
cmake -S "${SOURCE_ROOT}/hashlink-rg" -B "${HL_BUILD}" \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_FIND_FRAMEWORK=LAST \
    -DCMAKE_OSX_ARCHITECTURES="${HOST_ARCH}" \
    -DOPENAL_INCLUDE_DIR="${openal_prefix}/include/AL" \
    -DOPENAL_LIBRARY="${openal_prefix}/lib/libopenal.dylib" \
    -DWITH_FMT=ON -DWITH_OPENAL=ON -DWITH_SDL=OFF \
    -DWITH_SSL=ON -DWITH_UI=ON -DWITH_UV=ON \
    -DWITH_SQLITE=OFF -DWITH_VIDEO=OFF >/dev/null
cmake --build "${HL_BUILD}" -j"$(sysctl -n hw.ncpu)" \
    --target "${HL_HDLLS[@]/%/.hdll}" >/dev/null

# datachannel comes from its own pinned repository rather than hashlink's, but
# it is built here for the same reason as the rest: a prebuilt .hdll records
# whatever its author's machine happened to have, and the differences only show
# up as missing primitives at run time.
#
# Its CMakeLists says target_link_libraries(... libhl ...) with no such CMake
# target in scope, so the linker is handed a bare -llibhl and looks for
# liblibhl.dylib. The alias below satisfies that; the install name written into
# the result still comes from the dylib itself, so this changes nothing but the
# spelling the linker searches for.
DC_BUILD="${WORK_ROOT}/dc-build"
ln -sf libhl.dylib "${HL_BUILD}/bin/liblibhl.dylib"
cmake -S "${SOURCE_ROOT}/hxDatachannel-rg/cpp" -B "${DC_BUILD}" \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_OSX_ARCHITECTURES="${HOST_ARCH}" \
    -DHASHLINK_INCLUDE_DIR="${SOURCE_ROOT}/hashlink-rg/src" \
    -DHASHLINK_LIBRARY_DIR="${HL_BUILD}/bin" \
    -DBUILD_SHARED_LIBS=OFF -DNO_EXAMPLES=ON -DNO_TESTS=ON >/dev/null
cmake --build "${DC_BUILD}" -j"$(sysctl -n hw.ncpu)" \
    --target hxdatachannel.hdll >/dev/null
cp "${DC_BUILD}/datachannel.hdll" "${HL_BUILD}/bin/datachannel.hdll"
HL_HDLLS+=(datachannel)

# SDL keeps this script's own recipe rather than CMake's.
#
# Both compile the same pinned sources. CMake's sdl.hdll was once rejected here
# for taking every GL entry point down with a null dereference, but that
# diagnosis was wrong: the crashes came from the absolute LC_RPATH CMake bakes
# in, which pulled a second HashLink runtime into the process, and they are
# fixed above for every CMake-built library. This recipe stays because it is
# the one measured to render, not because CMake's is known to be broken --
# switching is a live option, gated on a run that reaches the menu.

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

# Under either name the runtime answers to. std/build.rs now stamps
# `@rpath/libhl.dylib` as the install name when ash_std is built, so a
# freshly linked hdll imports that directly and never mentions libash_std --
# which this check used to require, and so rejected a correct link.
linked_ash_std="$(otool -L "${RUN_ROOT}/sdl.hdll" \
    | awk '/libash_std.*dylib|@rpath\/libhl\.dylib/ { print $1; exit }')"
if [[ -z "${linked_ash_std}" ]]; then
    echo "error: freshly built sdl.hdll did not link against Ash's compatibility runtime" >&2
    otool -L "${RUN_ROOT}/sdl.hdll" >&2
    exit 1
fi
# A no-op when the install name was already stamped; still required for a
# runtime built before that change.
if [[ "${linked_ash_std}" != "@rpath/libhl.dylib" ]]; then
    install_name_tool -change "${linked_ash_std}" @rpath/libhl.dylib "${RUN_ROOT}/sdl.hdll"
fi
install_name_tool -id @rpath/sdl.hdll "${RUN_ROOT}/sdl.hdll"

for lib in "${HL_HDLLS[@]}"; do
    built="${HL_BUILD}/bin/${lib}.hdll"
    [[ -f "${built}" ]] || {
        echo "error: ${lib}.hdll was not produced by the pinned build" >&2
        exit 1
    }
    cp "${built}" "${RUN_ROOT}/${lib}.hdll"
    install_name_tool -id "@rpath/${lib}.hdll" "${RUN_ROOT}/${lib}.hdll"
    # CMake bakes an absolute LC_RPATH pointing back into the build tree, and
    # these libraries import @rpath/libhl.1.dylib. Left alone dyld satisfies
    # that from the build directory, loading a SECOND runtime -- the stock
    # HashLink one -- alongside the ash_std copy staged here. The HDLLs then
    # call into a GC that was never initialised, which surfaces as null derefs
    # inside otherwise healthy primitives. Repoint them at the staged runtime,
    # the way the hand-rolled recipe above already does.
    while read -r stale_rpath; do
        [[ -n "${stale_rpath}" ]] || continue
        install_name_tool -delete_rpath "${stale_rpath}" "${RUN_ROOT}/${lib}.hdll"
    done < <(otool -l "${RUN_ROOT}/${lib}.hdll" \
        | awk '/LC_RPATH/{want=1} want && $1=="path"{print $2; want=0}')
    install_name_tool -add_rpath @loader_path "${RUN_ROOT}/${lib}.hdll"
    codesign --force -s - "${RUN_ROOT}/${lib}.hdll" >/dev/null 2>&1 || true
    # Informational, never a gate: ui.c declares 36 primitives of which 18 are
    # Windows-only, and openal has 4 behind absent extensions, so "fewer than
    # declared" is normal. What the GAME can resolve is checked at the end.
    case "${lib}" in
        datachannel) lib_src="${SOURCE_ROOT}/hxDatachannel-rg/cpp/src" ;;
        *)           lib_src="${SOURCE_ROOT}/hashlink-rg/libs/${lib}" ;;
    esac
    declared="$(grep -rhoE 'DEFINE_PRIM[A-Z_]*\(' "${lib_src}"/*.c 2>/dev/null | wc -l | tr -d ' ')"
    exported="$(nm -gU "${RUN_ROOT}/${lib}.hdll" 2>/dev/null | grep -c ' T _hlp_' || true)"
    echo "built ${lib}.hdll from source: ${exported} primitives (source declares ${declared})"
done

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
    # Only what this script cannot build from source. Everything HashLink
    # ships is built above from the pinned checkout; accepting a prebuilt copy
    # instead is what let a stale openal.hdll into the fixture.
    for hdll in "${NATIVE_DIR}"/*.hdll; do
        name="$(basename "${hdll}" .hdll)"
        skip=""
        # `sdl` is built above too, by its own clang line rather than through
        # CMake, so it is not in HL_HDLLS and was being COPIED OVER by the
        # prebuilt one -- which is how a binary from the decommissioned
        # ash_sdl crate reached the fixture and tripped the check below. The
        # header has always said the source directory's sdl.hdll is ignored;
        # this is what makes that true.
        for built in "${HL_HDLLS[@]}" sdl; do
            [[ "${name}" == "${built}" ]] && skip=1 && break
        done
        if [[ -n "${skip}" ]]; then
            echo "note: ignoring prebuilt ${name}.hdll; built from source above" >&2
            continue
        fi
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

# fmt guards its png, jpeg and vorbis decoders behind #ifdefs that depend on
# which headers were found. A library found only half-way still links and still
# starts; the decoder is simply gone. Check the symbol, not the build log.
if ! nm -u "${RUN_ROOT}/fmt.hdll" | grep -q 'png_image_begin_read_from_memory'; then
    echo "error: fmt.hdll has no libpng simplified-read API, so png_decode" >&2
    echo "       compiled out and nothing will render. A stray png.h -- Mono's," >&2
    echo "       typically -- outranked the real one during the CMake build." >&2
    exit 1
fi

# Every staged HDLL must reach ash's runtime and no other. A second libhl in
# the process is not a link error -- both resolve, the program starts, and the
# only symptom is a null deref the first time an HDLL touches the GC it does
# not share. Assert that each @rpath here can only land inside RUN_ROOT.
#
# What decides this is ORDER, not membership. dyld walks LC_RPATH entries in
# order and takes the first hit, so a prebuilt HDLL that lists
# @executable_path before the /usr/local/lib it was built against resolves
# here and never reaches outside -- libhl.dylib is staged beside ash, so the
# first entry always hits. mysql.hdll and sqlite.hdll ship exactly that way.
#
# Rejecting them for the trailing entry would leave only one repair: rewriting
# load commands in a binary the game shipped, which the check below rightly
# refuses. So require that the FIRST rpath is inside the fixture, and treat
# what follows as unreachable rather than as a hazard.
[[ -f "${RUN_ROOT}/libhl.dylib" ]] || {
    echo "error: no libhl.dylib in ${RUN_ROOT}; an @executable_path rpath" >&2
    echo "       would fall through to whatever is on the system" >&2
    exit 1
}
for hdll in "${RUN_ROOT}"/*.hdll; do
    first_rpath="$(otool -l "${hdll}" \
        | awk '/LC_RPATH/{want=1} want && $1=="path"{print $2; exit}')"
    case "${first_rpath}" in
        @loader_path*|@executable_path*) ;;
        *)
            echo "error: $(basename "${hdll}") searches ${first_rpath:-nothing} before the" >&2
            echo "       fixture, so it can load a second libhl runtime" >&2
            exit 1
            ;;
    esac
done

if strings "${RUN_ROOT}/sdl.hdll" | grep -Eiq 'ash_sdl|crates/ash_sdl|target/(debug|release)/deps/libsdl'; then
    echo "error: rejected sdl.hdll contaminated by the decommissioned ash_sdl crate" >&2
    exit 1
fi
# Either name is Ash's runtime: the fixture stages it as both libhl.dylib and
# libhl.1.dylib, because HashLink's own build links the versioned name and
# upstream HDLLs expect it. What matters is that it resolves to something this
# directory provides, not which of the two aliases it asked for.
if ! otool -L "${RUN_ROOT}/sdl.hdll" | grep -Eq '@rpath/libhl(\.1)?\.dylib'; then
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
    # Signing is not an alteration, but it does move bytes: on arm64 `codesign`
    # pads __LINKEDIT out to a 16K page before appending, and the enlarged
    # vmsize stays in the load command even after --remove-signature. Staging
    # signs every prebuilt (it has to -- see above), so a staged copy can never
    # be byte-identical to the shipped one, and comparing them directly rejects
    # every library this check exists to wave through.
    #
    # So compare like with like: sign a scratch copy of the shipped file
    # exactly as staging signed the real one, and diff those. A genuine edit --
    # a rewritten load command, a rebuild, a patched byte -- still shows up.
    # Only the signature stops counting as one.
    signed_ref="$(mktemp -d)"
    trap 'rm -rf "${signed_ref}"' EXIT
    for staged in "${RUN_ROOT}"/*.hdll "${RUN_ROOT}"/*.dylib; do
        name="$(basename "${staged}")"
        case "${name}" in libhl.dylib|libhl.1.dylib) continue ;; esac
        # Anything built from the pinned source is EXPECTED to differ from a
        # prebuilt copy of the same name -- that difference is the point. This
        # check exists for the libraries the fixture takes as shipped. `sdl` is
        # built here too, by its own clang line rather than through CMake, so
        # it is not in HL_HDLLS and has to be named separately.
        built_here=""
        for built in "${HL_HDLLS[@]}" sdl; do
            [[ "${name}" == "${built}.hdll" ]] && built_here=1 && break
        done
        [[ -n "${built_here}" ]] && continue
        source_copy="${NATIVE_DIR}/${name}"
        [[ -e "${source_copy}" ]] || continue
        cp "${source_copy}" "${signed_ref}/${name}"
        codesign --force -s - "${signed_ref}/${name}" >/dev/null 2>&1 || {
            echo "error: could not sign the reference copy of ${name}" >&2
            exit 1
        }
        if ! cmp -s "${signed_ref}/${name}" "${staged}"; then
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
