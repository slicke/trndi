#!/bin/bash
#
# Build quickjs-ng plus the Trndi ABI shim.
#
#   ./build.sh          both host-linux and win64
#   ./build.sh linux    host only
#   ./build.sh win      win64 cross only
#
# See README.md for the required packages.

set -e

QJS_TAG=v0.15.1
HERE="$(cd "$(dirname "$0")" && pwd)"
WORK="${TRNDI_QJS_WORK:-$HERE/.build}"
SRC="$WORK/quickjs-ng"

what="${1:-all}"

mkdir -p "$WORK"

if [ ! -d "$SRC" ]; then
  echo "--> fetching quickjs-ng $QJS_TAG"
  git clone --depth 1 --branch "$QJS_TAG" \
    https://github.com/quickjs-ng/quickjs.git "$SRC"
fi

build_engine() {
  local name="$1"; shift
  echo "--> building engine ($name)"
  cmake -S "$SRC" -B "$WORK/b-$name" -G Ninja \
    -DCMAKE_BUILD_TYPE=Release \
    -DBUILD_SHARED_LIBS=ON \
    -DQJS_BUILD_EXAMPLES=OFF \
    -DQJS_BUILD_CLI=OFF \
    "$@"
  cmake --build "$WORK/b-$name"
}

if [ "$what" = all ] || [ "$what" = linux ]; then
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

if [ "$what" = all ] || [ "$what" = win ]; then
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
