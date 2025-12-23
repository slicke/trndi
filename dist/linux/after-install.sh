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

# Ensure metadata.json declares support for the currently installed GNOME Shell.
# GNOME requires an explicit match in "shell-version".
if command -v gnome-shell >/dev/null 2>&1; then
  ver="$(gnome-shell --version 2>/dev/null | grep -Eo '[0-9]+' | head -n1 || true)"
  meta="$DST/metadata.json"
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

# Note: enabling is per-user; users can enable via Extensions app.
exit 0
