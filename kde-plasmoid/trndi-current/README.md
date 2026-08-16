# Trndi Current (KDE Plasma plasmoid)

A minimal Plasma widget that shows Trndi’s current reading in the panel.

It reads the same cache file as the GNOME top-bar extension:
- `${XDG_CACHE_HOME:-$HOME/.cache}/trndi/current.txt`

If the file is missing, nothing is shown.

Newer Trndi versions write extra metadata lines to the cache file:
- line 1: value
- line 2: reading timestamp (Unix epoch seconds)
- line 3: freshness threshold (minutes)
- line 4: trend arrow, empty when Trndi's own trend-arrow setting is off

## Options

Right click the widget → **Configure…**:

- **Show trend arrow** (default on) — appends line 4 next to the reading, so
  the widget matches the tray icon. Trndi only publishes an arrow while its
  own *trend arrow on the badge* setting is on; this hides it for this widget
  alone.
- **Show “X ago” row** (default off) — a second line with the reading's age.

If the reading is older than the freshness threshold, the plasmoid keeps showing the last value but with strike-through (like the main Trndi UI).

If the cache file itself is older than the freshness threshold (default 11 minutes when unknown), the plasmoid shows nothing (assumes Trndi is not running).

## Install (Plasma 6)
```bash
./kde-plasmoid/install.sh              # current user; installs or upgrades
sudo ./kde-plasmoid/install.sh --system  # all users
```

The script uses `kpackagetool6` when available and falls back to copying into
`~/.local/share/plasma/plasmoids/` when it is not. Pass `--restart` to restart
`plasmashell` afterwards — needed for an upgrade to take effect on a widget
that is already on the panel.

Then add it via: **Right click panel → Add Widgets… → “Trndi Current”**.

The DEB and RPM packages install the plasmoid system-wide on their own; this
script is for running from source or from the AppImage. `--system` writes to
the same directory those packages manage, so it refuses to run when the Trndi
package is installed — use a per-user install, or `--force` if you really mean
to overwrite the package's copy.

Manual equivalent:
```bash
kpackagetool6 --type Plasma/Applet -i ./kde-plasmoid/trndi-current   # -u to upgrade
```

## Uninstall (Plasma 6)
```bash
./kde-plasmoid/install.sh --uninstall
sudo ./kde-plasmoid/install.sh --uninstall --system
```

## License

GPL-3.0-only, like the rest of Trndi — see `LICENSE.md` in the [Trndi repository](https://github.com/slicke/trndi).
Trndi is not a medical device; see `DISCLAIMER.md`.
