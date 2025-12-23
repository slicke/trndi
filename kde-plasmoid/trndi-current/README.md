# Trndi Current (KDE Plasma plasmoid)

A minimal Plasma widget that shows Trndi’s current reading in the panel.

It reads the same cache file as the GNOME top-bar extension:
- `${XDG_CACHE_HOME:-$HOME/.cache}/trndi/current.txt`

If the file is missing or older than 10 minutes, nothing is shown.

## Install (Plasma 6)
```bash
kpackagetool6 --type Plasma/Applet -i ./kde-plasmoid/trndi-current
```

Then add it via: **Right click panel → Add Widgets… → “Trndi Current”**.

## Uninstall (Plasma 6)
```bash
kpackagetool6 --type Plasma/Applet -r com.slicke.trndi.current
```
