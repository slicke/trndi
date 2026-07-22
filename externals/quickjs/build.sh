#!/bin/bash
#
# Build quickjs-ng plus the Trndi ABI shim.
#
#   ./build.sh          host target, plus the win64 cross-build on Linux
#   ./build.sh linux    host linux only
#   ./build.sh mac      host macOS only
#   ./build.sh win      win64 cross only
#
# See README.md for the required packages.

set -e

QJS_TAG=v0.15.1
HERE="$(cd "$(dirname "$0")" && pwd)"
WORK="${TRNDI_QJS_WORK:-$HERE/.build}"
SRC="$WORK/quickjs-ng"

host="$(uname -s)"
what="${1:-all}"

# 'all' means "everything this host can produce": macOS builds only for itself,
# Linux also cross-builds win64 through mingw.
if [ "$what" = all ]; then
  if [ "$host" = Darwin ]; then what=mac; else what=all-linux; fi
fi

case "$what" in
  linux|all-linux)
    if [ "$host" = Darwin ]; then
      echo "cannot build Linux libraries on macOS"; exit 1
    fi ;;
  mac)
    if [ "$host" != Darwin ]; then
      echo "the macOS libraries must be built on macOS"; exit 1
    fi ;;
esac

mkdir -p "$WORK"

if [ ! -d "$SRC" ]; then
  echo "--> fetching quickjs-ng $QJS_TAG"
  git clone --depth 1 --branch "$QJS_TAG" \
    https://github.com/quickjs-ng/quickjs.git "$SRC"
fi

# Ninja is preferred but not universal (it is not part of the Xcode command line
# tools); fall back to whatever generator cmake defaults to.
if command -v ninja >/dev/null 2>&1; then GEN=(-G Ninja); else GEN=(); fi

build_engine() {
  local name="$1"; shift
  echo "--> building engine ($name)"
  cmake -S "$SRC" -B "$WORK/b-$name" "${GEN[@]}" \
    -DCMAKE_BUILD_TYPE=Release \
    -DBUILD_SHARED_LIBS=ON \
    -DQJS_BUILD_EXAMPLES=OFF \
    -DQJS_BUILD_CLI=OFF \
    "$@"
  cmake --build "$WORK/b-$name"
}

if [ "$what" = all-linux ] || [ "$what" = linux ]; then
  arch="$(uname -m)"
  out="$HERE/prebuilt/${arch}-linux"
  mkdir -p "$out"

  build_engine linux
  echo "--> building shim (linux)"
  gcc -shared -fPIC -O2 -std=c11 -I"$SRC" \
    -o "$WORK/libtqshim.so" "$HERE/tq_shim.c" \
    -L"$WORK/b-linux" -lqjs -Wl,-rpath,'$ORIGIN'

  cp "$WORK"/b-linux/libqjs.so.* "$out/" 2>/dev/null || true
  cp "$WORK/libtqshim.so" "$out/"

  # Recreate the SONAME symlinks; only the real file is tracked in git.
  ( cd "$out"
    real="$(ls libqjs.so.[0-9]*.[0-9]*.[0-9]* 2>/dev/null | head -1)"
    if [ -n "$real" ]; then
      ln -sf "$real" libqjs.so.0
      ln -sf libqjs.so.0 libqjs.so
    fi )
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
