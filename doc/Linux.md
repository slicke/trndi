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
- KDE/GNOME-like desktops: `org.freedesktop.Notifications` over D-Bus, spoken directly through `libdbus-1` (already on any desktop system). If the library is somehow absent Trndi falls back to the `gdbus` command-line tool, from GLib's binary package.
- Other desktops: `notify-send` (usually provided by `libnotify`; install it from your distro if missing)

The same connection carries the panel badge, the dark-mode query to `xdg-desktop-portal`, and the resume-from-suspend signal from logind — so on a system without `libglib2.0-bin` (Debian doesn't pull it in for KDE) those all keep working, where before they needed the `gdbus` tool.

If you don't see toasts, check that a notification service is running and that Do Not Disturb / focus mode isn't suppressing them.

## Desktop indicators (GNOME/KDE)
Trndi ships optional GNOME Shell and KDE Plasma panel widgets that show the current reading. See the "Linux desktop indicators" section in the main [README](/README.md) for installation steps (automatic for .deb/.rpm, manual for AppImage).

## Kiosk mode and screen blanking
`trndi --kiosk` keeps the screen lit by holding an idle inhibition with logind (`systemd-inhibit`/`elogind-inhibit`), with the desktop session's own power manager (`gnome-session-inhibit`/`kde-inhibit` — GNOME and KDE ignore the logind one, so this is what a Wayland kiosk depends on), and by turning off X11 blanking with `xset`. Missing tools are skipped. sway/Hyprland and other wlroots compositors need their idle daemon configured directly — see the [Display guide](/guides/Display.md#keep-awake-on-linux).

## Settings storage
Settings are stored at `~/.config/Trndi/trndi.ini`.
