#!/bin/bash
# Clones (or updates) Trndi's develop branch, builds it the same way CI's
# linux-amd64 job does (via the repo's own Makefile, which mirrors that job —
# see guides/BUILDING.md and Makefile's MORMOT2_COMMIT/MORMOT2_STATIC_URL),
# then hands off to the container's command (a shell by default).
set -uo pipefail

: "${TRNDI_REPO:=https://github.com/slicke/trndi.git}"
: "${TRNDI_BRANCH:=develop}"
: "${TRNDI_DIR:=/root/trndi}"

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

echo "==> Fetching mORMot2 (make bootstrap: skips the fetch if externals/mORMot2 already exists)"
make bootstrap

echo "==> Building Trndi (make release: Extensions (Release), Qt6 widgetset on Linux)"
if make release; then
  echo "==> Build succeeded. Binary: $TRNDI_DIR/build/Trndi"
else
  echo "==> Build failed - dropping into a shell in $TRNDI_DIR for debugging." >&2
fi

exec "$@"
