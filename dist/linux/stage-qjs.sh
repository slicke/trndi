#!/bin/sh
# Stage the QuickJS engine and its ABI shim next to a Trndi binary for packaging.
#
#   stage-qjs.sh <prebuilt-dir> <dest-dir>
#
# The prebuilt directory uses the usual ELF three-name layout: the real
# libqjs.so.<major>.<minor>.<patch>, plus libqjs.so.0 (the SONAME) and
# libqjs.so (link-time only, for -lqjs) as symlinks recreated at build time.
#
# Packages must not carry those symlinks. 7-Zip 21.07+, fpm's dir source and
# mksquashfs each dereference symlinks by default, which silently turns one
# 1.2 MB library into three identical copies inside the ZIP/DEB/RPM/AppImage.
# Only the SONAME is ever resolved at runtime, so install the real file under
# that name and ship nothing else.
#
# A missing prebuilt directory is not an error: No Ext builds never load these.
set -eu

src="${1:?usage: stage-qjs.sh <prebuilt-dir> <dest-dir>}"
dest="${2:?usage: stage-qjs.sh <prebuilt-dir> <dest-dir>}"

if [ ! -d "$src" ]; then
  echo "stage-qjs: $src missing; skipping QuickJS libraries"
  exit 0
fi

real=$(cd "$src" && ls libqjs.so.[0-9]*.[0-9]*.[0-9]* 2>/dev/null | head -1)
if [ -z "$real" ]; then
  echo "stage-qjs: no libqjs.so.<version> found in $src" >&2
  exit 1
fi

# Ask the library itself which name the loader will look for, so a future
# quickjs-ng major bump does not silently ship an unreachable library.
soname=$(readelf -d "$src/$real" 2>/dev/null |
  sed -n 's/.*SONAME.*\[\(.*\)\].*/\1/p' | head -1)
[ -n "$soname" ] || soname="libqjs.so.0"

mkdir -p "$dest"
# Plain cp, not cp -P: $real is the regular file, and it is installed under the
# SONAME so no symlink is needed in the package at all.
cp "$src/$real" "$dest/$soname"
cp "$src/libtqshim.so" "$dest/libtqshim.so"
chmod 0644 "$dest/$soname" "$dest/libtqshim.so"

echo "stage-qjs: staged $real as $soname + libtqshim.so in $dest"
