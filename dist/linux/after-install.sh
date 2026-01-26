#!/usr/bin/env bash
set -euo pipefail

GNOME_EXT_UUID='trndi-current@slicke.com'
GNOME_SRC="/usr/local/share/trndi/gnome-shell-extension/${GNOME_EXT_UUID}"
GNOME_DST="/usr/share/gnome-shell/extensions/${GNOME_EXT_UUID}"

KDE_PLASMOID_ID='com.slicke.trndi.current'
KDE_SRC="/usr/local/share/trndi/kde-plasmoid/${KDE_PLASMOID_ID}"
KDE_DST="/usr/share/plasma/plasmoids/${KDE_PLASMOID_ID}"

# --- GNOME Shell extension (system-wide install) ---
if [[ -d "$GNOME_SRC" && -d "/usr/share/gnome-shell" ]]; then
  mkdir -p "$(dirname "$GNOME_DST")"
  rm -rf "$GNOME_DST"
  cp -a "$GNOME_SRC" "$GNOME_DST"

  # Ensure metadata.json declares support for the currently installed GNOME Shell.
  # GNOME requires an explicit match in "shell-version".
  if command -v gnome-shell >/dev/null 2>&1; then
    ver="$(gnome-shell --version 2>/dev/null | grep -Eo '[0-9]+' | head -n1 || true)"
    meta="$GNOME_DST/metadata.json"
    if [[ -n "$ver" && -f "$meta" ]]; then
      python3 - <<'PY' "$meta" "$ver" || true
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
    # keep it roughly sorted numerically
    def key(x):
      try:
        return int(x)
      except Exception:
        return 10**9
    sv = sorted(set(sv), key=key)
    data['shell-version'] = sv
    with open(path, 'w', encoding='utf-8') as f:
      json.dump(data, f, indent=2)
      f.write('\n')
except Exception:
  pass
PY
    fi
  fi
fi

# --- KDE Plasma plasmoid (system-wide install) ---
# Plasma discovers widgets placed under /usr/share/plasma/plasmoids/<id>
if [[ -d "$KDE_SRC" ]]; then
  # Check if KDE Plasma is installed by looking for common KDE components
  if command -v plasmashell >/dev/null 2>&1 || [[ -d "/usr/share/plasma" ]] || [[ -d "/usr/share/kservices5" ]]; then
    mkdir -p "$(dirname "$KDE_DST")"
    rm -rf "$KDE_DST"
    cp -a "$KDE_SRC" "$KDE_DST"
    echo "KDE Plasma plasmoid installed to $KDE_DST"
  fi
fi

# Note: enabling is per-user; users can enable via Extensions app.
exit 0
