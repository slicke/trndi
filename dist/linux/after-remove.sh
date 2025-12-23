#!/usr/bin/env bash
set -euo pipefail

EXT_UUID='trndi-current@slicke.com'
DST="/usr/share/gnome-shell/extensions/${EXT_UUID}"

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
  rm -rf "$DST" || true
fi

exit 0
