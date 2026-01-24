# Trndi Current (GNOME Shell extension)

Shows Trndi’s current reading in the GNOME top bar by reading:

- `~/.cache/trndi/current.txt`

Trndi writes that file on Linux whenever it updates the tray/badge.

## Install (user)

```bash
mkdir -p ~/.local/share/gnome-shell/extensions
rm -rf ~/.local/share/gnome-shell/extensions/trndi-current@slicke.com
cp -r gnome-shell-extension/trndi-current ~/.local/share/gnome-shell/extensions/trndi-current@slicke.com
```

Then reload GNOME Shell and enable the extension:

- X11: press `Alt+F2`, type `r`, press Enter
- Wayland: log out/in

Enable with the Extensions app (or `gnome-extensions enable trndi-current@slicke.com`).

## Install (system-wide)

```bash
sudo rm -rf /usr/share/gnome-shell/extensions/trndi-current@slicke.com
sudo cp -r gnome-shell-extension/trndi-current /usr/share/gnome-shell/extensions/trndi-current@slicke.com
```

## Notes

- The indicator updates every 5 seconds.
- If no value is available it shows `--`.
- GNOME requires the extension `metadata.json` to list your GNOME Shell major version in `shell-version`.
	The DEB/RPM installer auto-adds your current version during install.
