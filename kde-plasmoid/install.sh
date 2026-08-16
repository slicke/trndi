#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-only
#
# Part of Trndi - https://github.com/slicke/trndi
# Copyright (c) Björn Lindh
#
# This program is distributed under the terms of the GNU General Public
# License, Version 3, as published by the Free Software Foundation. See
# LICENSE.md in the Trndi repository, or <https://www.gnu.org/licenses/gpl-3.0>.
#
# MEDICAL DISCLAIMER: Trndi is NOT a medical device. Readings shown may be
# delayed, inaccurate, or unavailable — never make medical decisions based on
# them. See DISCLAIMER.md.
#
# Installs or removes the "Trndi Current" Plasma 6 applet. Prefers
# kpackagetool6 and falls back to a plain directory copy when it is missing.
# The DEB/RPM packages install the plasmoid system-wide on their own (see
# dist/linux/after-install.sh) — this script is for source and AppImage users.

set -eu

PLASMOID_ID='com.slicke.trndi.current'
SRC_NAME='trndi-current'

# Where the DEB/RPM stages its copy of the plasmoid. Its post-install script
# copies from here into the system destination below, so the installed files
# are not owned by dpkg/rpm — this directory is what proves the package is
# managing that destination.
PKG_STAGE="/usr/local/share/trndi/kde-plasmoid/$PLASMOID_ID"

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
SRC="$script_dir/$SRC_NAME"

user_dest="${XDG_DATA_HOME:-$HOME/.local/share}/plasma/plasmoids/$PLASMOID_ID"
system_dest="/usr/share/plasma/plasmoids/$PLASMOID_ID"

action='install'
scope='user'
restart='false'
force='false'

usage() {
  cat <<EOF
Usage: $0 [--system] [--uninstall] [--restart] [--force]

  (no options)  install or upgrade for the current user
  --system      install into $system_dest (needs root)
  --uninstall   remove instead of install
  --restart     restart plasmashell afterwards so the change takes effect
  --force       proceed with --system even when the Trndi package owns it

After installing, add the widget via:
  Right click panel -> Add Widgets... -> "Trndi Current"
EOF
}

while [ $# -gt 0 ]; do
  case "$1" in
    --system) scope='system' ;;
    --uninstall|--remove) action='uninstall' ;;
    --restart) restart='true' ;;
    --force) force='true' ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage >&2; exit 2 ;;
  esac
  shift
done

have() { command -v "$1" >/dev/null 2>&1; }

require_root() {
  if [ "$(id -u)" != '0' ]; then
    echo "--system needs root; re-run with sudo." >&2
    exit 1
  fi
}

# Name of the package owning a path, if any. Covers a future package that ships
# the system copy directly instead of copying it in from the staging directory.
owning_package() {
  if have dpkg-query; then
    pkg=$(dpkg-query -S "$1" 2>/dev/null | head -n1 | cut -d: -f1)
    if [ -n "${pkg:-}" ]; then echo "$pkg"; return 0; fi
  fi
  if have rpm; then
    pkg=$(rpm -qf "$1" 2>/dev/null) || pkg=''
    if [ -n "$pkg" ]; then echo "$pkg"; return 0; fi
  fi
  return 1
}

# --system writes exactly where the DEB/RPM post-install script writes, so
# refuse by default rather than overwrite its copy or orphan its install.
guard_system_dest() {
  [ "$force" = 'true' ] && return 0
  owner=''
  if [ -d "$PKG_STAGE" ]; then
    owner='the installed Trndi package'
  else
    pkg=$(owning_package "$system_dest") || pkg=''
    [ -n "$pkg" ] && owner="package '$pkg'"
  fi
  [ -n "$owner" ] || return 0

  echo "$system_dest is managed by $owner." >&2
  if [ "$action" = 'install' ]; then
    echo "Installing here would overwrite its copy, and a package upgrade would overwrite yours." >&2
  else
    echo "Removing it would leave the package thinking the plasmoid is still installed;" >&2
    echo "remove the trndi package instead." >&2
  fi
  echo "Use --force to proceed anyway, or drop --system for a per-user install." >&2
  exit 1
}

copy_tree() {
  dest="$1"
  mkdir -p "$(dirname "$dest")"
  rm -rf "$dest"
  cp -a "$SRC" "$dest"
  echo "Installed to $dest"
}

plasmoid_installed() {
  [ -d "$user_dest" ] && return 0
  kpackagetool6 --type Plasma/Applet -l 2>/dev/null | grep -qx "$PLASMOID_ID"
}

install_user() {
  if have kpackagetool6; then
    if plasmoid_installed; then
      kpackagetool6 --type Plasma/Applet -u "$SRC" && return 0
      kpackagetool6 --type Plasma/Applet -i "$SRC" && return 0
    else
      kpackagetool6 --type Plasma/Applet -i "$SRC" && return 0
      kpackagetool6 --type Plasma/Applet -u "$SRC" && return 0
    fi
    echo "kpackagetool6 failed — falling back to a plain copy." >&2
  fi
  copy_tree "$user_dest"
}

uninstall_user() {
  removed='false'
  if have kpackagetool6; then
    # Quiet: it reports "not installed" as an error when only the fallback
    # copy exists, which the directory check below handles anyway.
    kpackagetool6 --type Plasma/Applet -r "$PLASMOID_ID" >/dev/null 2>&1 && removed='true'
  fi
  if [ -d "$user_dest" ]; then
    rm -rf "$user_dest"
    removed='true'
  fi
  if [ "$removed" = 'true' ]; then
    echo "Removed $PLASMOID_ID for $(id -un)."
  else
    echo "$PLASMOID_ID was not installed for $(id -un)."
  fi
}

restart_plasmashell() {
  if [ "$scope" = 'system' ] || [ "$(id -u)" = '0' ]; then
    echo "Not restarting plasmashell as root — log out and back in, or restart it as your user."
    return 0
  fi
  if ! have plasmashell; then
    return 0
  fi
  if have systemctl && systemctl --user list-unit-files plasma-plasmashell.service >/dev/null 2>&1; then
    systemctl --user restart plasma-plasmashell.service && return 0
  fi
  if have kquitapp6 && have kstart; then
    kquitapp6 plasmashell >/dev/null 2>&1 || true
    kstart plasmashell >/dev/null 2>&1 &
    return 0
  fi
  echo "Could not restart plasmashell automatically — restart it manually to pick up the change." >&2
}

if [ "$action" = 'install' ] && [ ! -d "$SRC" ]; then
  echo "Plasmoid source not found: $SRC" >&2
  exit 1
fi

case "$scope:$action" in
  user:install)
    install_user
    ;;
  user:uninstall)
    uninstall_user
    ;;
  system:install)
    guard_system_dest
    require_root
    copy_tree "$system_dest"
    ;;
  system:uninstall)
    guard_system_dest
    require_root
    if [ -d "$system_dest" ]; then
      rm -rf "$system_dest"
      echo "Removed $system_dest"
    else
      echo "$system_dest is not installed."
    fi
    ;;
esac

if [ "$restart" = 'true' ]; then
  restart_plasmashell
elif [ "$action" = 'install' ]; then
  echo 'Add it via: Right click panel -> Add Widgets... -> "Trndi Current"'
  echo 'If you upgraded an already-added widget, restart plasmashell (--restart) to load the new QML.'
fi

exit 0
