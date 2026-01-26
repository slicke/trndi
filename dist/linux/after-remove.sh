#!/usr/bin/env bash
set -euo pipefail

GNOME_EXT_UUID='trndi-current@slicke.com'
GNOME_DST="/usr/share/gnome-shell/extensions/${GNOME_EXT_UUID}"

KDE_PLASMOID_ID='com.slicke.trndi.current'
KDE_DST="/usr/share/plasma/plasmoids/${KDE_PLASMOID_ID}"

# Debian postrm passes: remove|purge|upgrade|...
# RPM scriptlets often pass: 0 (uninstall) / 1 (upgrade)
mode="${1:-}"

should_remove=false
if [[ "$mode" == "remove" || "$mode" == "purge" || "$mode" == "" ]]; then
  should_remove=true
elif [[ "$mode" == "0" ]]; then
  should_remove=true
fi

if [[ "$should_remove" == true ]]; then
  rm -rf "$GNOME_DST" || true
  rm -rf "$KDE_DST" || true
fi

exit 0
