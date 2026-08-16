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
# Installs or removes the "Trndi Current" GNOME Shell extension, adding the
# running Shell's major version to metadata.json when it is not already listed.
# The DEB/RPM packages install it system-wide on their own (see
# dist/linux/after-install.sh) — this script is for source and AppImage users.

set -eu

EXT_UUID='trndi-current@slicke.com'
SRC_NAME='trndi-current'

# Where the DEB/RPM stages its copy of the extension. Its post-install script
# copies from here into the system destination below, so the installed files
# are not owned by dpkg/rpm — this directory is what proves the package is
# managing that destination.
PKG_STAGE="/usr/local/share/trndi/gnome-shell-extension/$EXT_UUID"

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
SRC="$script_dir/$SRC_NAME"

user_dest="${XDG_DATA_HOME:-$HOME/.local/share}/gnome-shell/extensions/$EXT_UUID"
system_dest="/usr/share/gnome-shell/extensions/$EXT_UUID"

action='install'
scope='user'
force='false'

usage() {
  cat <<EOF
Usage: $0 [--system] [--uninstall] [--force]

  (no options)  install or upgrade for the current user, then enable it
  --system      install into $system_dest (needs root)
  --uninstall   remove instead of install
  --force       proceed with --system even when the Trndi package owns it

GNOME Shell must be reloaded before the extension appears:
  X11:     Alt+F2, type r, Enter
  Wayland: log out and back in
EOF
}

while [ $# -gt 0 ]; do
  case "$1" in
    --system) scope='system' ;;
    --uninstall|--remove) action='uninstall' ;;
    --force) force='true' ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage >&2; exit 2 ;;
  esac
  shift
done

have() { command -v "$1" >/dev/null 2>&1; }

# GNOME refuses to load an extension whose metadata.json does not list the
# running Shell's major version, so add it to the installed copy if missing.
add_shell_version() {
  meta="$1/metadata.json"
  have gnome-shell || return 0
  have python3 || return 0
  [ -f "$meta" ] || return 0
  ver=$(gnome-shell --version 2>/dev/null | grep -Eo '[0-9]+' | head -n1 || true)
  [ -n "$ver" ] || return 0
  python3 - "$meta" "$ver" <<'PY' || true
import json, sys
path, ver = sys.argv[1], sys.argv[2]
try:
  with open(path, 'r', encoding='utf-8') as f:
    data = json.load(f)
  sv = data.get('shell-version')
  if not isinstance(sv, list):
    sv = []
  if ver not in sv:
    sv.append(ver)
    def key(x):
      try:
        return int(x)
      except Exception:
        return 10**9
    data['shell-version'] = sorted(set(sv), key=key)
    with open(path, 'w', encoding='utf-8') as f:
      json.dump(data, f, indent=2)
      f.write('\n')
except Exception:
  pass
PY
}

copy_tree() {
  dest="$1"
  mkdir -p "$(dirname "$dest")"
  rm -rf "$dest"
  cp -a "$SRC" "$dest"
  add_shell_version "$dest"
  echo "Installed to $dest"
}

remove_tree() {
  dest="$1"
  if [ -d "$dest" ]; then
    rm -rf "$dest"
    echo "Removed $dest"
  else
    echo "$dest is not installed."
  fi
}

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
    echo "Removing it would leave the package thinking the extension is still installed;" >&2
    echo "remove the trndi package instead." >&2
  fi
  echo "Use --force to proceed anyway, or drop --system for a per-user install." >&2
  exit 1
}

if [ "$action" = 'install' ] && [ ! -d "$SRC" ]; then
  echo "Extension source not found: $SRC" >&2
  exit 1
fi

case "$scope:$action" in
  user:install)
    copy_tree "$user_dest"
    if have gnome-extensions; then
      # Fails harmlessly when the Shell has not rescanned its extension dir yet.
      gnome-extensions enable "$EXT_UUID" 2>/dev/null \
        || echo "Enable it after reloading GNOME Shell: gnome-extensions enable $EXT_UUID"
    fi
    ;;
  user:uninstall)
    if have gnome-extensions; then
      gnome-extensions disable "$EXT_UUID" 2>/dev/null || true
    fi
    remove_tree "$user_dest"
    ;;
  system:install)
    guard_system_dest
    require_root
    copy_tree "$system_dest"
    echo "Enabling is per-user: gnome-extensions enable $EXT_UUID"
    ;;
  system:uninstall)
    guard_system_dest
    require_root
    remove_tree "$system_dest"
    ;;
esac

if [ "$action" = 'install' ]; then
  echo 'Reload GNOME Shell to pick it up — X11: Alt+F2, r, Enter. Wayland: log out and back in.'
fi

exit 0
