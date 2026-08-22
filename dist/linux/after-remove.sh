#!/bin/sh
set -eu

GNOME_EXT_UUID='trndi-current@slicke.com'
GNOME_DST="/usr/share/gnome-shell/extensions/${GNOME_EXT_UUID}"

KDE_PLASMOID_ID='com.slicke.trndi.current'
KDE_DST="/usr/share/plasma/plasmoids/${KDE_PLASMOID_ID}"

# Debian postrm passes: remove|purge|upgrade|...
# RPM scriptlets often pass: 0 (uninstall) / 1 (upgrade)
mode="${1:-}"

should_remove=false
if [ "${mode}" = "remove" ] || [ "${mode}" = "purge" ] || [ -z "${mode}" ]; then
  should_remove=true
elif [ "${mode}" = "0" ]; then
  should_remove=true
fi

if [ "${should_remove}" = true ]; then
  rm -rf "$GNOME_DST" || true
  rm -rf "$KDE_DST" || true

  # Mirror after-install.sh: fpm packages have no distro triggers.
  if command -v gtk-update-icon-cache >/dev/null 2>&1; then
    gtk-update-icon-cache -q /usr/share/icons/hicolor || true
  fi
  if command -v update-desktop-database >/dev/null 2>&1; then
    update-desktop-database -q /usr/share/applications || true
  fi
fi

exit 0
