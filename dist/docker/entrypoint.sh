#!/bin/bash
# Clones (or updates) Trndi's develop branch, builds it the way CI's matching
# Linux job does (via the repo's own Makefile, which mirrors that job - see
# guides/BUILDING.md), then hands off to the container's command (a shell by default).
set -uo pipefail

: "${TRNDI_REPO:=https://github.com/slicke/trndi.git}"
: "${TRNDI_BRANCH:=develop}"
: "${TRNDI_DIR:=/root/trndi}"

echo "==> Trndi dev container: $(uname -m), lazbuild: $(command -v lazbuild || echo 'NOT FOUND')"
if ! command -v lazbuild >/dev/null 2>&1; then
  echo "==> lazbuild is missing from this image - it was built before the" >&2
  echo "    architecture fix, or from a cached layer. Rebuild with:" >&2
  echo "      podman build --no-cache -t trndi-dev -f dist/docker/Dockerfile ." >&2
  echo "    (or the same command with docker)" >&2
  exec "$@"
fi

# The container's WORKDIR is $TRNDI_DIR, so the shell may already be sitting
# inside it. Step out first - rm -rf'ing (or replacing) the current working
# directory out from under the shell breaks getcwd() for every command after.
cd /

if [ -d "$TRNDI_DIR/.git" ]; then
  echo "==> Updating existing checkout at $TRNDI_DIR ($TRNDI_BRANCH)"
  git -C "$TRNDI_DIR" fetch origin "$TRNDI_BRANCH" \
    && git -C "$TRNDI_DIR" checkout "$TRNDI_BRANCH" \
    && git -C "$TRNDI_DIR" reset --hard "origin/$TRNDI_BRANCH"
else
  echo "==> Cloning $TRNDI_REPO ($TRNDI_BRANCH) into $TRNDI_DIR"
  rm -rf "$TRNDI_DIR"
  git clone --branch "$TRNDI_BRANCH" --single-branch "$TRNDI_REPO" "$TRNDI_DIR"
fi

cd "$TRNDI_DIR" || exec "$@"

# Both Linux CI jobs build Extensions (.github/workflows/build.yml), and
# externals/quickjs/prebuilt ships libraries for x86_64-linux and aarch64-linux -
# the .lpi picks the right directory from $(TargetCPU)-$(TargetOS) on its own.
# Override with TRNDI_BUILD_TARGET=noext-release to skip the engine entirely.
: "${TRNDI_BUILD_TARGET:=release}"

echo "==> Building Trndi (make $TRNDI_BUILD_TARGET: Qt6 widgetset)"

if make "$TRNDI_BUILD_TARGET"; then
  echo "==> Build succeeded. Binary: $TRNDI_DIR/build/Trndi"
else
  echo "==> Build failed - dropping into a shell in $TRNDI_DIR for debugging." >&2
fi

exec "$@"
