# Linux notes

## Widgetset
Trndi is built and tested against **Qt6** on Linux. If a build won't start or looks wrong, check that your Qt6 packages are installed — see `doc/Widgetsets.md`.

## Window take-over
Unlike Windows and macOS, Trndi cannot color the title bar on Linux (the desktop/window manager owns that). The window's client area still changes color with your reading.

## Multiple users
Each user gets their own color and nickname (Settings > Multi User). On Linux there's no title bar to badge, so instead the window title is prefixed with `[name] Trndi` and a small sidebar showing the account appears in the window itself.

## Notifications
Trndi auto-selects a notification backend, no configuration needed:
- KDE/GNOME-like desktops: `org.freedesktop.Notifications` over D-Bus (via `gdbus`)
- Other desktops: `notify-send` (usually provided by `libnotify`; install it from your distro if missing)

If you don't see toasts, check that a notification service is running and that Do Not Disturb / focus mode isn't suppressing them.

## Desktop indicators (GNOME/KDE)
Trndi ships optional GNOME Shell and KDE Plasma panel widgets that show the current reading. See the "Linux desktop indicators" section in the main [README](/README.md) for installation steps (automatic for .deb/.rpm, manual for AppImage).

## Settings storage
Settings are stored at `~/.config/Trndi/trndi.ini`.
