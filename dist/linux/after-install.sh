#!/usr/bin/env bash
set -euo pipefail

EXT_UUID='trndi-current@slicke.com'
SRC="/usr/local/share/trndi/gnome-shell-extension/${EXT_UUID}"
DST="/usr/share/gnome-shell/extensions/${EXT_UUID}"

# Only proceed when the extension payload is present.
if [[ ! -d "$SRC" ]]; then
  exit 0
fi

# Only install system-wide if GNOME Shell is present.
if [[ ! -d "/usr/share/gnome-shell" ]]; then
  exit 0
fi

mkdir -p "$(dirname "$DST")"
rm -rf "$DST"
cp -a "$SRC" "$DST"

# Note: enabling is per-user; users can enable via Extensions app.
exit 0
