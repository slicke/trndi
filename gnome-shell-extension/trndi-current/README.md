# Trndi Current (GNOME Shell extension)

Shows Trndi’s current reading in the GNOME top bar by reading:

- `~/.cache/trndi/current.txt`

Trndi writes that file on Linux whenever it updates the tray/badge.

## Install (user)

```bash
./gnome-shell-extension/install.sh
```

It copies the extension into `~/.local/share/gnome-shell/extensions/`, adds
your running Shell's major version to the installed `metadata.json` if it is
not listed, and enables the extension.

Then reload GNOME Shell:

- X11: press `Alt+F2`, type `r`, press Enter
- Wayland: log out/in

If enabling failed because the Shell had not rescanned yet, enable it with the
Extensions app (or `gnome-extensions enable trndi-current@slicke.com`).

Manual equivalent:

```bash
mkdir -p ~/.local/share/gnome-shell/extensions
rm -rf ~/.local/share/gnome-shell/extensions/trndi-current@slicke.com
cp -r gnome-shell-extension/trndi-current ~/.local/share/gnome-shell/extensions/trndi-current@slicke.com
```

## Install (system-wide)

```bash
sudo ./gnome-shell-extension/install.sh --system
```

The DEB and RPM packages already do this during install; the script is for
running from source or from the AppImage. Because `--system` writes to the same
directory those packages manage, it refuses to run when the Trndi package is
installed — use a per-user install, or `--force` if you really mean to
overwrite the package's copy.

## Uninstall

```bash
./gnome-shell-extension/install.sh --uninstall
sudo ./gnome-shell-extension/install.sh --uninstall --system
```

## Notes

- The indicator updates every 5 seconds.
- If no value is available it shows `--`.
- The trend arrow is appended to the reading when Trndi publishes one on line 4
  of the cache file — that is, while Trndi's own *trend arrow on the badge*
  setting is on. The extension has no settings of its own; turn the arrow off
  in Trndi if you do not want it.
- GNOME requires the extension `metadata.json` to list your GNOME Shell major version in `shell-version`.
	The DEB/RPM installer auto-adds your current version during install.

## License

GPL-3.0-only, like the rest of Trndi — see `LICENSE.md` in the [Trndi repository](https://github.com/slicke/trndi).
Trndi is not a medical device; see `DISCLAIMER.md`.
