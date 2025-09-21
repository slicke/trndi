# Notifications
Trndi can show toasts (those small pop-ups near the clock) when you're high or low.

Enabling notifications is optional — you can use Trndi without them.

## How Trndi chooses a backend
- Windows: Uses the BurntToast module (PowerShell) when available.
- macOS: Uses the built‑in user notification center — no setup required.
- Linux: Auto‑selects between org.freedesktop.Notifications over D‑Bus (gdbus) on KDE/GNOME‑like desktops and notify-send elsewhere. Trndi detects this at runtime; no manual toggle is needed.

Tip: You can see which backend is active in logs or by observing which tool is invoked (gdbus vs notify-send) when a notification fires.

## Windows
Trndi uses the BurntToast PowerShell module to show notifications.

Install (once, system‑wide) from an elevated PowerShell:

	Install-Module -Name BurntToast

Note: This installs on your system PowerShell, not inside Trndi.

## macOS
No extra setup is needed. Trndi uses the system notification center. Make sure notifications for the app are allowed under System Settings if you don’t see toasts.

## Linux
Trndi supports two common Linux paths and chooses automatically:

- KDE/GNOME‑like desktops (under Qt6 builds): Uses D‑Bus via gdbus with org.freedesktop.Notifications.
- Other desktops or when D‑Bus isn’t suitable: Falls back to notify-send.

Notes
- notify-send is usually provided by libnotify; install it from your distro if missing.
- If you don’t see notifications, check that your desktop has a notification service running and that the app isn’t muted or suppressed by Do Not Disturb.