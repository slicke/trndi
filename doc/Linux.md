# Linux notes

## Widgetset
Trndi is built and tested against **Qt6** on Linux. If a build won't start or looks wrong, check that your Qt6 packages are installed — see `doc/Widgetsets.md`.

## Window take-over
On X11, Trndi cannot color the native title bar (the desktop/window manager owns that) — the window's client area still changes color with your reading.

On **Wayland** sessions Trndi draws its own title bar instead: the compositor's forced decorations are dropped and replaced with a bar that follows the window color (so title-bar coloring works just like on Windows/macOS), with working drag-to-move, edge resize, double-click maximize and min/max/close buttons. This is automatic on Wayland; you can control it with the `ux.own_titlebar` setting in `trndi.ini` (`auto` (default), `on` — also use it on X11, `off` — never). The bar is skipped when "no borders" mode is enabled, and hidden in fullscreen/kiosk mode.

## Multiple users
Each user gets their own color and nickname (Settings > Multi User). With the native (X11) title bar there's nothing to badge, so the window title is prefixed with `[name] Trndi` and a small sidebar showing the account appears in the window itself. With the drawn title bar (automatic on Wayland, or on X11 with `ux.own_titlebar=on`), the bar itself takes the account color like on Windows.

## Notifications
Trndi auto-selects a notification backend, no configuration needed:
- KDE/GNOME-like desktops: `org.freedesktop.Notifications` over D-Bus (via `gdbus`)
- Other desktops: `notify-send` (usually provided by `libnotify`; install it from your distro if missing)

If you don't see toasts, check that a notification service is running and that Do Not Disturb / focus mode isn't suppressing them.

## Desktop indicators (GNOME/KDE)
Trndi ships optional GNOME Shell and KDE Plasma panel widgets that show the current reading. See the "Linux desktop indicators" section in the main [README](/README.md) for installation steps (automatic for .deb/.rpm, manual for AppImage).

## Settings storage
Settings are stored at `~/.config/Trndi/trndi.ini`.
