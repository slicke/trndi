#!/bin/bash
#
# Build quickjs-ng plus the Trndi ABI shim.
#
#   ./build.sh          host target, plus the win64 cross-build on Linux
#   ./build.sh linux    host linux only
#   ./build.sh mac      host macOS only
#   ./build.sh win      x86_64 Windows, cross from Linux through mingw-w64
#   ./build.sh winarm   aarch64 Windows, cross from Linux through llvm-mingw
#   ./build.sh winhost  natively on Windows, in an MSYS2 shell (either arch)
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
# themselves, Linux also cross-builds x86_64 win64 through mingw. 'winarm' is
# deliberately not in it -- it needs llvm-mingw, which no distro packages, so
# folding it in would make 'all' fail on every machine that has only the
# x86_64 cross-compiler. Under MSYS2 the whole thing is a native Windows build.
if [ "$what" = all ]; then
  case "$host" in
    Darwin)                       what=mac ;;
    Haiku)                        what=haiku ;;
    FreeBSD)                      what=freebsd ;;
    MINGW*|MSYS*|CLANGARM64*|CYGWIN*) what=winhost ;;
    *)                            what=all-linux ;;
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
  winhost)
    # An MSYS2 shell reports MINGW64_NT-… / CLANGARM64_NT-… / MSYS_NT-…; the
    # cross targets ('win', 'winarm') are the way in from anywhere else.
    case "$host" in
      MINGW*|MSYS*|CLANGARM64*|CYGWIN*) ;;
      *) echo "winhost builds on Windows itself; use 'win' or 'winarm' to cross from $host"; exit 1 ;;
    esac ;;
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
    *)         uname -m ;;   # MSYS2 already agrees: x86_64 / aarch64
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

# --- Windows -----------------------------------------------------------------
#
# Both Windows targets produce libqjs.dll and tqshim.dll. Those names are not a
# convention here, they are the interface: the Pascal binding declares
# 'external libqjs.dll' / 'external tqshim.dll' (trndi.ext.quickjs.pp), and FPC
# writes the import under exactly that name -- no import library is involved on
# Windows. A toolchain that spells the engine qjs.dll, as MSVC does, links
# cleanly and then leaves the executable asking the loader for a DLL that was
# never shipped. mingw's lib-prefixed output is what the binding expects, which
# is why every route below is a mingw one.

# The C driver for a mingw triple. mingw-w64 packages ship <triple>-gcc;
# llvm-mingw ships <triple>-clang, usually with a <triple>-gcc symlink beside
# it. Prefer -gcc so a distro toolchain wins where one is installed, and fall
# back to -clang, which is the only spelling some llvm-mingw builds carry.
win_cc() {
  if command -v "$1-gcc" >/dev/null 2>&1; then echo "$1-gcc"
  elif command -v "$1-clang" >/dev/null 2>&1; then echo "$1-clang"
  else return 1
  fi
}

# Cross-build both libraries for Windows. $1 is the FPC cpu name, which is also
# the prebuilt/ directory -- the .lpi library path is $(TargetCPU)-$(TargetOS),
# so ARM64 is aarch64-win64 and nothing else. $2 is the mingw triple, $3 names
# the cmake build tree.
build_win_cross() {
  local cpu="$1" triple="$2" name="$3"
  local out="$HERE/prebuilt/${cpu}-win64" cc cxx root

  if ! cc="$(win_cc "$triple")"; then
    echo "no $triple toolchain on PATH."
    if [ "$cpu" = aarch64 ]; then
      echo "  No distro packages an ARM64 mingw. Either put llvm-mingw on PATH,"
      echo "  or build in a container that already has it -- see 'Windows on ARM'"
      echo "  in README.md, which installs nothing on the host."
    else
      echo "  Fedora: dnf install mingw64-gcc"
      echo "  Debian: apt install gcc-mingw-w64-x86-64"
    fi
    exit 1
  fi
  case "$cc" in
    *-gcc)   cxx="${cc%-gcc}-g++" ;;
    *-clang) cxx="${cc%-clang}-clang++" ;;
  esac

  mkdir -p "$out"
  {
    echo "set(CMAKE_SYSTEM_NAME Windows)"
    echo "set(CMAKE_SYSTEM_PROCESSOR $cpu)"
    echo "set(CMAKE_C_COMPILER   $cc)"
    echo "set(CMAKE_CXX_COMPILER $cxx)"
    echo "set(CMAKE_RC_COMPILER  $triple-windres)"
    # Fedora's mingw packages keep their sysroot here; llvm-mingw carries its
    # own inside the toolchain and has no such directory. Pointing cmake at a
    # path that does not exist makes every find_* search quietly useless, so
    # only set it when it is really there.
    root="/usr/$triple/sys-root/mingw"
    if [ -d "$root" ]; then echo "set(CMAKE_FIND_ROOT_PATH $root)"; fi
    echo "set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)"
    echo "set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)"
    echo "set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)"
  } > "$WORK/$name.cmake"

  build_engine "$name" -DCMAKE_TOOLCHAIN_FILE="$WORK/$name.cmake"

  echo "--> building shim ($cpu-win64)"
  # Into the build tree rather than straight into $out, so a failed compile
  # cannot leave half a library behind in a directory that is committed.
  "$cc" -shared -O2 -std=c11 -I"$SRC" \
    -o "$WORK/b-$name/tqshim.dll" "$HERE/tq_shim.c" \
    "$WORK/b-$name/libqjs.dll.a"

  cp "$WORK/b-$name/libqjs.dll" "$WORK/b-$name/tqshim.dll" "$out/"
  echo "    -> $out"
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
  build_win_cross x86_64 x86_64-w64-mingw32 win
fi

if [ "$what" = winarm ]; then
  build_win_cross aarch64 aarch64-w64-mingw32 winarm
fi

# Natively on Windows, in an MSYS2 shell. This is the route for a machine that
# runs Windows anyway -- notably Windows on ARM, where an ARM64 VM builds its
# own libraries and no cross-toolchain is involved at all. The compilers carry
# no triple prefix here: it is a host build that happens to target Windows, so
# it produces the same libqjs.dll / tqshim.dll pair as the cross-builds and
# lands in the same directory.
if [ "$what" = winhost ]; then
  # MSYS2's CLANGARM64 environment has clang and no gcc (there is no ARM64
  # mingw gcc); MINGW64 has gcc. Neither packages 'cc', so the default $CC
  # picked above does not necessarily name anything that exists -- but an
  # explicit CC= from the caller does, and is left alone.
  if ! command -v "$CC" >/dev/null 2>&1; then
    for c in gcc clang; do
      if command -v "$c" >/dev/null 2>&1; then CC="$c"; break; fi
    done
  fi
  if ! command -v "$CC" >/dev/null 2>&1; then
    echo "no C compiler in this MSYS2 environment."
    echo "  CLANGARM64: pacman -S mingw-w64-clang-aarch64-{clang,cmake,ninja} git"
    echo "  MINGW64:    pacman -S mingw-w64-x86_64-{gcc,cmake,ninja} git"
    exit 1
  fi

  # Ask the compiler what it targets rather than asking the host what it is.
  # host_arch() is wrong here: MSYS2 ships no native ARM64 runtime, so on
  # Windows on ARM the shell -- and its uname -- is the emulated x86_64 one
  # even in the CLANGARM64 environment, where every compiler in $PATH emits
  # ARM64 code. Trusting uname would file an ARM64 build under x86_64-win64
  # and hand the loader an ARM64 DLL for an x64 executable.
  triple="$("$CC" -dumpmachine)"
  case "$triple" in
    aarch64-*|arm64-*) arch=aarch64 ;;
    x86_64-*)          arch=x86_64 ;;
    i?86-*)            arch=i386 ;;
    *) echo "cannot tell what $CC targets (-dumpmachine said '$triple')"; exit 1 ;;
  esac
  # The plain MSYS environment builds against msys-2.0.dll, a Cygwin fork --
  # not a Windows-native DLL, and not something Trndi.exe can load. Its
  # compiler reports *-pc-msys, which is the one spelling to refuse.
  case "$triple" in
    *-msys)
      echo "this is the MSYS environment, which builds Cygwin-style binaries."
      echo "  Open the CLANGARM64 shell (ARM64) or MINGW64 shell (x64) instead."
      exit 1 ;;
  esac

  out="$HERE/prebuilt/${arch}-win64"
  mkdir -p "$out"

  build_engine winhost
  echo "--> building shim (${arch}-win64, native)"
  "$CC" -shared -O2 -std=c11 -I"$SRC" \
    -o "$WORK/b-winhost/tqshim.dll" "$HERE/tq_shim.c" \
    "$WORK/b-winhost/libqjs.dll.a"

  cp "$WORK/b-winhost/libqjs.dll" "$WORK/b-winhost/tqshim.dll" "$out/"
  echo "    -> $out"
fi

echo "done."
