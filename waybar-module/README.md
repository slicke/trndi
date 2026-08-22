# Trndi Current (Waybar module)

Shows Trndi's current reading in [Waybar](https://github.com/Alexays/Waybar) —
the usual bar on Hyprland, sway, river and niri, where the GNOME extension and
KDE plasmoid don't apply.

It reads the same cache file as the other two indicators:

- `${XDG_CACHE_HOME:-$HOME/.cache}/trndi/current.txt`

and mirrors their behavior: hides itself when the cache file goes stale
(Trndi is likely not running), shows `--` when the reading itself is too old.

## Install

Copy the script somewhere Waybar can run it and make it executable:

```bash
mkdir -p ~/.config/waybar/scripts
cp waybar-module/trndi-waybar.sh ~/.config/waybar/scripts/
chmod +x ~/.config/waybar/scripts/trndi-waybar.sh
```

(The DEB/RPM packages stage it at `/usr/local/share/trndi/waybar-module/`;
the AppImage bundles it at `squashfs-root/usr/share/trndi/waybar-module/`
after `--appimage-extract`.)

Then add the module to `~/.config/waybar/config.jsonc`:

```jsonc
"custom/trndi": {
    "exec": "~/.config/waybar/scripts/trndi-waybar.sh",
    "return-type": "json",
    "interval": 5,
    "tooltip": true
},
```

and list `"custom/trndi"` in `modules-right` (or wherever you want it).
Style it in `~/.config/waybar/style.css`:

```css
#custom-trndi.stale {
    color: #888888;
}
```

## Options

The other indicators' settings map to script arguments in `"exec"`
(and `"interval"` above is their update-interval knob):

- `--hide-after N` — hide when the cache file is older than N minutes
  (default 11)
- `--no-arrow` — never append the trend arrow (Trndi only publishes one
  while its own *trend arrow on the badge* setting is on)
- `--age` — append the reading's age, e.g. `5.6 ↘ · 3m`

## Hyprland extras

Trndi already detects Hyprland as a tiling compositor and adapts its window
chrome. Useful window rules for `~/.config/hypr/hyprland.conf`:

```ini
# Let Trndi's small floating display float and stay on every workspace
windowrule = float, class:^(Trndi)$, title:^(Floating Display)$
windowrule = pin, class:^(Trndi)$, title:^(Floating Display)$
```

(On Hyprland versions before 0.46 the directive is spelled `windowrulev2`.)

Notifications on Hyprland/sway need a notification daemon (mako, swaync or
dunst) — with one running, Trndi posts full-fidelity D-Bus notifications
(urgency, icon, click-to-raise), the same as on KDE/GNOME.

## License

GPL-3.0-only, like the rest of Trndi — see `LICENSE.md` in the [Trndi repository](https://github.com/slicke/trndi).
Trndi is not a medical device; see `DISCLAIMER.md`.
