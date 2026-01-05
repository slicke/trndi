# Trndi Current (KDE Plasma plasmoid)

A minimal Plasma widget that shows Trndi’s current reading in the panel.

It reads the same cache file as the GNOME top-bar extension:
- `${XDG_CACHE_HOME:-$HOME/.cache}/trndi/current.txt`

If the file is missing, nothing is shown.

Newer Trndi versions write extra metadata lines to the cache file:
- line 1: value
- line 2: reading timestamp (Unix epoch seconds)
- line 3: freshness threshold (minutes)

If the reading is older than the freshness threshold, the plasmoid keeps showing the last value but with strike-through (like the main Trndi UI).

If the cache file itself is older than the freshness threshold (default 11 minutes when unknown), the plasmoid shows nothing (assumes Trndi is not running).

## Install (Plasma 6)
```bash
kpackagetool6 --type Plasma/Applet -i ./kde-plasmoid/trndi-current
```

Then add it via: **Right click panel → Add Widgets… → “Trndi Current”**.

## Uninstall (Plasma 6)
```bash
kpackagetool6 --type Plasma/Applet -r com.slicke.trndi.current
```
