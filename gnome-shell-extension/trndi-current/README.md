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

## Notes

- The indicator updates every 5 seconds.
- If no value is available it shows `--`.
