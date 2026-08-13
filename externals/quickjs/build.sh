#!/bin/bash
#
# Build quickjs-ng plus the Trndi ABI shim.
#
#   ./build.sh          host target, plus the win64 cross-build on Linux
#   ./build.sh linux    host linux only
#   ./build.sh mac      host macOS only
#   ./build.sh win      win64 cross only
#   ./build.sh haiku    host Haiku only
#   ./build.sh freebsd  host FreeBSD only
#   ./build.sh shim     the ABI shim only, against an engine that is already there
#
# See README.md for the required packages.

set -e

QJS_TAG=v0.15.1
HERE="$(cd "$(dirname "$0")" && pwd)"
WORK="${TRNDI_QJS_WORK:-$HERE/.build}"
SRC="$WORK/quickjs-ng"

host="$(uname -s)"
what="${1:-all}"

# 'all' means "everything this host can produce": macOS and Haiku build only for
# themselves, Linux also cross-builds win64 through mingw.
if [ "$what" = all ]; then
  case "$host" in
    Darwin)  what=mac ;;
    Haiku)   what=haiku ;;
    FreeBSD) what=freebsd ;;
    *)       what=all-linux ;;
  esac
fi

case "$what" in
  linux|all-linux)
    if [ "$host" != Linux ]; then
      echo "cannot build Linux libraries on $host"; exit 1
    fi ;;
  mac)
    if [ "$host" != Darwin ]; then
      echo "the macOS libraries must be built on macOS"; exit 1
    fi ;;
  haiku)
    if [ "$host" != Haiku ]; then
      echo "the Haiku libraries must be built on Haiku"; exit 1
    fi ;;
  freebsd)
    if [ "$host" != FreeBSD ]; then
      echo "the FreeBSD libraries must be built on FreeBSD"; exit 1
    fi ;;
esac

# FreeBSD's base compiler is clang and there may be no 'gcc' at all, so do not
# hardcode one; 'cc' is the portable spelling on every target here. $CC wins.
CC="${CC:-$(command -v cc >/dev/null 2>&1 && echo cc || echo gcc)}"

# A shim-only build compiles one C file against an installed engine: it needs
# neither the work tree nor the source clone, so do not leave an empty .build
# behind for it.
if [ "$what" != shim ]; then
  mkdir -p "$WORK"
fi

# Only the branches that compile the engine need the source tree; a shim-only
# build takes its header from whichever engine is already installed.
fetch_src() {
  if [ ! -d "$SRC" ]; then
    echo "--> fetching quickjs-ng $QJS_TAG"
    git clone --depth 1 --branch "$QJS_TAG" \
      https://github.com/quickjs-ng/quickjs.git "$SRC"
  fi
}

# Ninja is preferred but not universal (it is not part of the Xcode command line
# tools); fall back to whatever generator cmake defaults to.
if command -v ninja >/dev/null 2>&1; then GEN=(-G Ninja); else GEN=(); fi

build_engine() {
  local name="$1"; shift
  fetch_src
  echo "--> building engine ($name)"
  cmake -S "$SRC" -B "$WORK/b-$name" "${GEN[@]}" \
    -DCMAKE_BUILD_TYPE=Release \
    -DBUILD_SHARED_LIBS=ON \
    -DQJS_BUILD_EXAMPLES=OFF \
    -DQJS_BUILD_CLI=OFF \
    "$@"
  cmake --build "$WORK/b-$name"
}

# FPC names its targets <cpu>-<os>, and prebuilt/ follows that because the .lpi
# library path is $(TargetCPU)-$(TargetOS). Several hosts disagree with FPC
# about the name: FreeBSD says amd64 for x86_64; Haiku reports 32-bit x86 as
# BePC and 64-bit ARM as arm64 (uname.c maps B_CPU_x86 / B_CPU_ARM_64), and
# macOS also says arm64; Linux says i686 where FPC says i386.
host_arch() {
  case "$(uname -m)" in
    BePC|i?86) echo i386 ;;
    amd64)     echo x86_64 ;;
    arm64)     echo aarch64 ;;
    *)         uname -m ;;
  esac
}

# Install the engine cmake built in $1 into $2, then recreate its symlinks.
#
# The one real versioned file is named explicitly and copied on its own. The
# glob this replaces ('cp libqjs.so.*') handed cp a SONAME symlink together
# with the very file it points at, which is order-dependent and not portable:
# on Haiku it produced a 0-byte destination from a 1.2 MB source, on a first
# run into an empty directory. Nothing noticed, because the copy ended in
# '2>/dev/null || true' -- the failure surfaced only at startup, as the loader
# reporting libqjs.so.0 missing while an intact-looking symlink farm sat in
# front of an empty file. So: clear stale links, copy one named file, let cp
# report its own errors, and verify the result is non-empty.
install_engine() {
  local from="$1" out="$2" engine
  engine="$(ls "$from"/libqjs.so.[0-9]*.[0-9]*.[0-9]* 2>/dev/null | head -1)"
  if [ -z "$engine" ]; then
    echo "no engine built in $from"; exit 1
  fi
  mkdir -p "$out"
  rm -f "$out"/libqjs.so "$out"/libqjs.so.[0-9]*
  cp "$engine" "$out/"
  if [ ! -s "$out/$(basename "$engine")" ]; then
    echo "copying $engine into $out produced an empty file"; exit 1
  fi
  soname_links "$out"
}

# Recreate the SONAME symlinks in $1; only the real file is tracked in git.
soname_links() {
  ( cd "$1"
    real="$(ls libqjs.so.[0-9]*.[0-9]*.[0-9]* 2>/dev/null | head -1)"
    if [ -n "$real" ]; then
      ln -sf "$real" libqjs.so.0
      ln -sf libqjs.so.0 libqjs.so
    fi )
}

# Refuse to compile the shim against an engine that is not the tag it — and the
# Pascal binding in units/trndi/ext/trndi.ext.quickjs.pp — was written for.
# quickjs-ng is pre-1.0 and promises no ABI stability between minor versions,
# and nothing catches a mismatch at runtime: tq_abi_version covers the shim,
# not the engine. Any extra arguments are passed to the compiler (the -I of a
# source tree, when the engine was not taken from the system).
check_engine_version() {
  local ver
  ver=$(printf '#include <quickjs.h>\nQJS_VERSION_MAJOR.QJS_VERSION_MINOR.QJS_VERSION_PATCH\n' \
        | $CC -x c -E "$@" - 2>/dev/null \
        | sed '/^#/d;/^[[:space:]]*$/d' | tail -1 | tr -d ' \t')
  if [ -z "$ver" ]; then
    echo "quickjs.h not found."
    if [ "$host" = Haiku ]; then
      echo "  install the engine:  pkgman install quickjs_ng quickjs_ng_devel"
      echo "  or build it here:    $0 haiku"
    else
      echo "  build the engine first, e.g. $0 linux"
    fi
    exit 1
  fi
  if [ "$ver" != "${QJS_TAG#v}" ]; then
    echo "engine header is $ver, but this shim targets ${QJS_TAG#v}."
    echo "Set TRNDI_QJS_ALLOW_VERSION_SKEW=1 to build anyway."
    [ -n "$TRNDI_QJS_ALLOW_VERSION_SKEW" ] || exit 1
  fi
}

# Compile the ABI shim for the host into $1. It links against the engine in $2
# when that is given, otherwise the system one — the shim only needs quickjs.h,
# so a packaged engine (Haiku: quickjs_ng_devel) is enough to build it.
build_shim() {
  local out="$1" enginedir="${2:-}"
  local inc=() lib=()
  # if/then, not '&&': under 'set -e' a false AND-list ends the script.
  if [ -f "$SRC/quickjs.h" ]; then inc=(-I"$SRC"); fi
  if [ -n "$enginedir" ]; then lib=(-L"$enginedir"); fi
  check_engine_version "${inc[@]}"
  # After the version check, so a refused build leaves the tree as it found it.
  # -lqjs resolves through the unversioned symlink, which git does not carry.
  if [ -n "$enginedir" ]; then soname_links "$enginedir"; fi
  mkdir -p "$out"
  if [ "$host" = Darwin ]; then
    clang -dynamiclib -O2 -std=c11 "${inc[@]}" \
      -o "$out/libtqshim.dylib" "$HERE/tq_shim.c" "${lib[@]}" -lqjs \
      -install_name @rpath/libtqshim.dylib -Wl,-rpath,@loader_path
  else
    $CC -shared -fPIC -O2 -std=c11 "${inc[@]}" \
      -o "$out/libtqshim.so" "$HERE/tq_shim.c" "${lib[@]}" -lqjs \
      -Wl,-rpath,'$ORIGIN'
  fi
}

if [ "$what" = all-linux ] || [ "$what" = linux ]; then
  arch="$(host_arch)"
  out="$HERE/prebuilt/${arch}-linux"
  mkdir -p "$out"

  build_engine linux
  echo "--> building shim (linux)"
  $CC -shared -fPIC -O2 -std=c11 -I"$SRC" \
    -o "$WORK/libtqshim.so" "$HERE/tq_shim.c" \
    -L"$WORK/b-linux" -lqjs -Wl,-rpath,'$ORIGIN'

  install_engine "$WORK/b-linux" "$out"
  cp "$WORK/libtqshim.so" "$out/"
  echo "    -> $out"
fi

# Haiku packages quickjs-ng itself (HaikuPorts dev-lang/quickjs-ng, currently
# the same tag this binding targets), so 'shim' against pkgman's engine is a
# valid alternative to this branch. Building it here keeps the engine version
# under Trndi's control rather than the package repository's, and matches what
# every other platform ships in prebuilt/.
if [ "$what" = haiku ]; then
  out="$HERE/prebuilt/$(host_arch)-haiku"
  mkdir -p "$out"

  build_engine haiku
  install_engine "$WORK/b-haiku" "$out"

  echo "--> building shim (haiku)"
  build_shim "$out" "$WORK/b-haiku"
  echo "    -> $out"
fi

# FreeBSD ports carries lang/quickjs (Bellard's), not quickjs-ng, so there is
# no packaged engine this binding can use and both halves are built here. The
# layout is otherwise the Linux one: rtld honours $ORIGIN, so the libraries sit
# beside the executable rather than in a subdirectory as on Haiku.
if [ "$what" = freebsd ]; then
  out="$HERE/prebuilt/$(host_arch)-freebsd"
  mkdir -p "$out"

  build_engine freebsd
  install_engine "$WORK/b-freebsd" "$out"

  echo "--> building shim (freebsd)"
  build_shim "$out" "$WORK/b-freebsd"
  echo "    -> $out"
fi

# Shim only, for a host whose engine is already in place — a packaged one, or a
# prebuilt/ directory that does not need rebuilding. The shim is Trndi's own
# code, so it is the half that has to be compiled wherever no binary is shipped.
if [ "$what" = shim ]; then
  case "$host" in
    Darwin)  out="$HERE/prebuilt/$(host_arch)-darwin" ;;
    Haiku)   out="$HERE/prebuilt/$(host_arch)-haiku" ;;
    Linux)   out="$HERE/prebuilt/$(host_arch)-linux" ;;
    FreeBSD) out="$HERE/prebuilt/$(host_arch)-freebsd" ;;
    *)       echo "no shim recipe for $host"; exit 1 ;;
  esac

  # Link against a prebuilt engine when one is sitting there, so the shim
  # records the same SONAME the executable will load; otherwise the system one.
  enginedir=""
  if ls "$out"/libqjs.* >/dev/null 2>&1; then
    enginedir="$out"
  fi

  echo "--> building shim ($host${enginedir:+, engine from $out})"
  build_shim "$out" "$enginedir"
  echo "    -> $out"
fi

if [ "$what" = mac ]; then
  # FPC calls Apple Silicon aarch64; uname calls it arm64. The directory name has
  # to match FPC, because the .lpi library path is $(TargetCPU)-$(TargetOS).
  arch="$(uname -m)"
  if [ "$arch" = arm64 ]; then arch=aarch64; fi
  out="$HERE/prebuilt/${arch}-darwin"
  mkdir -p "$out"

  # Optional: raise the minimum OS, or build a universal library, e.g.
  #   TRNDI_QJS_MACOS_MIN=11.0 TRNDI_QJS_MAC_ARCHS='arm64;x86_64' ./build.sh mac
  mac_args=()
  shim_args=()
  if [ -n "$TRNDI_QJS_MACOS_MIN" ]; then
    mac_args+=(-DCMAKE_OSX_DEPLOYMENT_TARGET="$TRNDI_QJS_MACOS_MIN")
    shim_args+=(-mmacosx-version-min="$TRNDI_QJS_MACOS_MIN")
  fi
  if [ -n "$TRNDI_QJS_MAC_ARCHS" ]; then
    mac_args+=(-DCMAKE_OSX_ARCHITECTURES="$TRNDI_QJS_MAC_ARCHS")
    # cmake takes one ;-separated list; clang wants a -arch per slice.
    for a in ${TRNDI_QJS_MAC_ARCHS//;/ }; do shim_args+=(-arch "$a"); done
  fi

  build_engine mac "${mac_args[@]}"

  # Unlike the Linux build, flatten the versioned dylib into a single unversioned
  # file. FPC links these by name (-lqjs) and there is nothing here to version
  # against, so this avoids storing symlinks in git — they do not survive a
  # checkout onto a Windows filesystem.
  engine="$WORK/b-mac/libqjs.dylib"
  if [ ! -e "$engine" ]; then
    engine="$(ls "$WORK"/b-mac/libqjs.*.dylib 2>/dev/null | head -1)"
  fi
  cp -L "$engine" "$out/libqjs.dylib"
  # cmake stamps the install name as @rpath/libqjs.<soversion>.dylib; retarget it
  # at the flattened name so dependents record a path that exists.
  install_name_tool -id @rpath/libqjs.dylib "$out/libqjs.dylib"

  echo "--> building shim (macOS $arch)"
  # Linked against $out, not the build tree, so the shim records the retargeted
  # install name. @loader_path lets it find the engine beside itself no matter
  # which directory the executable was launched from.
  clang -dynamiclib -O2 -std=c11 -I"$SRC" "${shim_args[@]}" \
    -o "$out/libtqshim.dylib" "$HERE/tq_shim.c" \
    -L"$out" -lqjs \
    -install_name @rpath/libtqshim.dylib \
    -Wl,-rpath,@loader_path

  # Locally built dylibs carry no signature; an ad-hoc one keeps Gatekeeper from
  # refusing to load them on Apple Silicon.
  codesign -f -s - "$out/libqjs.dylib" "$out/libtqshim.dylib" 2>/dev/null || true
  echo "    -> $out"
fi

if [ "$what" = all-linux ] || [ "$what" = win ]; then
  out="$HERE/prebuilt/x86_64-win64"
  mkdir -p "$out"

  cat > "$WORK/mingw.cmake" <<'EOF'
set(CMAKE_SYSTEM_NAME Windows)
set(CMAKE_SYSTEM_PROCESSOR x86_64)
set(CMAKE_C_COMPILER   x86_64-w64-mingw32-gcc)
set(CMAKE_CXX_COMPILER x86_64-w64-mingw32-g++)
set(CMAKE_RC_COMPILER  x86_64-w64-mingw32-windres)
set(CMAKE_FIND_ROOT_PATH /usr/x86_64-w64-mingw32/sys-root/mingw)
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
EOF

  build_engine win -DCMAKE_TOOLCHAIN_FILE="$WORK/mingw.cmake"
  echo "--> building shim (win64)"
  x86_64-w64-mingw32-gcc -shared -O2 -std=c11 -I"$SRC" \
    -o "$WORK/tqshim.dll" "$HERE/tq_shim.c" \
    "$WORK/b-win/libqjs.dll.a"

  cp "$WORK/b-win/libqjs.dll" "$WORK/tqshim.dll" "$out/"
  echo "    -> $out"
fi

echo "done."
