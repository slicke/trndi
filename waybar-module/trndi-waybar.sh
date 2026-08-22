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
# delayed, inaccurate, or unavailable - never make medical decisions based on
# them. See DISCLAIMER.md.
#
# Waybar custom-module script showing Trndi's current reading, for
# Hyprland/sway/river/niri setups. Reads the same cache file as the GNOME
# extension and KDE plasmoid and mirrors their semantics: hide when the file
# is old (Trndi likely not running), "--" when the reading itself is stale.
#
# Usage: trndi-waybar.sh [--hide-after MINUTES] [--no-arrow] [--age]
#   --hide-after N  print nothing when the cache file is older than N minutes
#                   (default 11)
#   --no-arrow      never append the trend arrow
#   --age           append the reading's age, e.g. "5.6 ↘ · 3m"
#
# Output is Waybar JSON ("return-type": "json"): text, tooltip, and a
# "fresh"/"stale" class for styling. See README.md for the config snippet.

set -eu

f="${XDG_CACHE_HOME:-$HOME/.cache}/trndi/current.txt"
hide_after=11
show_arrow=true
show_age=false

while [ $# -gt 0 ]; do
  case "$1" in
    --hide-after) shift; hide_after="${1:-11}" ;;
    --no-arrow) show_arrow=false ;;
    --age) show_age=true ;;
    -h|--help)
      sed -n 's/^# \{0,1\}//p' "$0" | sed -n '/^Usage:/,/^$/p'
      exit 0
      ;;
    *) echo "Unknown option: $1" >&2; exit 2 ;;
  esac
  shift
done

# No cache file: Trndi has not run yet (or cleaned up on exit) - show nothing.
[ -f "$f" ] || exit 0

now=$(date +%s)
mt=$(stat -c %Y "$f" 2>/dev/null || stat -f %m "$f" 2>/dev/null || echo 0)

# Hide when the cache file itself is old: Trndi is likely not running.
if [ "$mt" -gt 0 ] && [ $((now - mt)) -gt $((hide_after * 60)) ]; then
  exit 0
fi

# Cache layout (see WriteTrndiCurrentValueCache in trndi.native.linux.pp):
# line 1: value, line 2: reading epoch seconds, line 3: freshness threshold
# minutes, line 4: trend arrow ('' while Trndi's badge-trend setting is off).
value=$(sed -n 1p "$f")
epoch=$(sed -n 2p "$f")
fresh=$(sed -n 3p "$f")
arrow=$(sed -n 4p "$f")

[ -n "$value" ] || exit 0

# Compact ranges for panel fit: "70 - 180" -> "70-180".
value=$(printf '%s' "$value" | sed 's/ *- */-/g')

case "$epoch" in *[!0-9]*|'') epoch=0 ;; esac
case "$fresh" in *[!0-9]*|'') fresh=0 ;; esac
# Accept both seconds and milliseconds epochs.
[ "$epoch" -gt 1000000000000 ] && epoch=$((epoch / 1000))

stale=false
if [ "$epoch" -gt 0 ] && [ "$fresh" -gt 0 ] && [ $((now - epoch)) -gt $((fresh * 60)) ]; then
  stale=true
fi

# Reading age from the epoch when usable, the file mtime otherwise.
basis=$epoch
[ "$basis" -gt 0 ] || basis=$mt
age_min=$(( (now - basis) / 60 ))
[ "$age_min" -ge 0 ] || age_min=0

if [ "$stale" = true ]; then
  text='--'
  class='stale'
else
  text="$value"
  if [ "$show_arrow" = true ] && [ -n "$arrow" ]; then
    text="$text $arrow"
  fi
  class='fresh'
fi
if [ "$show_age" = true ]; then
  text="$text · ${age_min}m"
fi

tooltip="Trndi: $value"
[ -n "$arrow" ] && tooltip="$tooltip $arrow"
tooltip="$tooltip (${age_min} min ago)"
[ "$stale" = true ] && tooltip="$tooltip — stale"

esc() { printf '%s' "$1" | sed 's/\\/\\\\/g; s/"/\\"/g'; }
printf '{"text": "%s", "tooltip": "%s", "class": "%s"}\n' \
  "$(esc "$text")" "$(esc "$tooltip")" "$(esc "$class")"
